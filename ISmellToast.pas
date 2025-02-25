unit ISmellToast;

interface

uses
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Objects, FMX.Ani, FMX.Skia,
  System.UITypes, FMX.Forms, System.Types, System.Skia, System.Classes,
  System.SysUtils, System.Math, System.Generics.Collections, System.SyncObjs;

procedure ShowSkiaToast(const AMessage: string; AOwner: TForm;
  APosition: Single = 0.9; ADuration: Integer = 2000);

implementation

type
  TToastItem = record
    Message: string;
    Duration: Integer;
    Owner: TForm;
    Position: Single;
  end;

  TToastHelper = class
  private
    class var FToastQueue: TQueue<TToastItem>;
    class var FIsShowing: Boolean;
    class var FLock: TCriticalSection;

    class procedure ShowNextToast;
  public
    class procedure Initialize;
    class procedure Finalize;
    class procedure PaintToast(Sender: TObject; const Canvas: ISkCanvas; const Dest: TRectF; const Opacity: Single);
    class procedure HideToast(Sender: TObject);
    class procedure OnFadeOutFinish(Sender: TObject);
  end;

{ TToastHelper }

class procedure TToastHelper.Initialize;
begin
  FToastQueue := TQueue<TToastItem>.Create;
  FLock := TCriticalSection.Create;
  FIsShowing := False;
end;

class procedure TToastHelper.Finalize;
begin
  FLock.Free;
  FToastQueue.Free;
end;

class procedure TToastHelper.ShowNextToast;
const
  PADDING = 20;
var
  Layout: TLayout;
  ToastBox: TSkPaintBox;
  Animation: TFloatAnimation;
  HideTimer: TTimer;
  MsgHolder: TStringList;
  Paint: ISkPaint;
  Font: ISkFont;
  TextBounds: TRectF;
  TextWidth: Single;
  ToastItem: TToastItem;
begin
  FLock.Enter;
  try
    if FIsShowing or (FToastQueue.Count = 0) then
      Exit;

    FIsShowing := True;
    ToastItem := FToastQueue.Dequeue;
  finally
    FLock.Leave;
  end;

  // Ensure owner is still valid
  if not Assigned(ToastItem.Owner) then
  begin
    FLock.Enter;
    try
      FIsShowing := False;
    finally
      FLock.Leave;
    end;
    Exit;
  end;

  // Create a container layout
  Layout := TLayout.Create(ToastItem.Owner);
  Layout.Parent := ToastItem.Owner;
  Layout.Align := TAlignLayout.None;
  Layout.Height := 40; // Fixed height

  // Initialize Skia Paint and Font
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  Font := TSkFont.Create(nil, 22);

  // Measure text width dynamically
  Font.MeasureText(ToastItem.Message, TextBounds, Paint);
  TextWidth := TextBounds.Width + PADDING * 2;

  // Set width dynamically (min 100px, max 300px)
  Layout.Width := Min(300, Max(100, TextWidth));

  // 🔹 Position at the **Lower Left Corner**
  Layout.Position.X := 20; //Left side, 20px margin
  Layout.Position.Y := ToastItem.Owner.Height * ToastItem.Position - Layout.Height;

  Layout.BringToFront;

  // Create the toast box
  ToastBox := TSkPaintBox.Create(Layout);
  ToastBox.Parent := Layout;
  ToastBox.Align := TAlignLayout.Contents;
  ToastBox.Opacity := 0; // Initially hidden
  ToastBox.OnDraw := TToastHelper.PaintToast;

  // Store message in TagObject
  MsgHolder := TStringList.Create;
  MsgHolder.Text := Trim(ToastItem.Message);
  ToastBox.TagObject := MsgHolder;

  // Create fade-in animation
  Animation := TFloatAnimation.Create(ToastBox);
  Animation.Parent := ToastBox;
  Animation.PropertyName := 'Opacity';
  Animation.StartValue := 0;
  Animation.StopValue := 1;
  Animation.Duration := 0.5; // 0.5 seconds fade-in
  Animation.Start;

  // Timer to keep toast visible before fade-out
  HideTimer := TTimer.Create(ToastItem.Owner);
  HideTimer.Interval := ToastItem.Duration; // Custom duration
  HideTimer.OnTimer := TToastHelper.HideToast;
  HideTimer.TagObject := ToastBox; // Store reference to toast for cleanup
  HideTimer.Enabled := True;
end;

class procedure TToastHelper.PaintToast(Sender: TObject; const Canvas: ISkCanvas;
  const Dest: TRectF; const Opacity: Single);
const
  Padding = 12;       // Padding inside the rectangle
  BaseFontSize = 16;  // Starting font size before scaling
  CornerRadius = 16;  // Rounded corners
var
  Paint, ShadowPaint: ISkPaint;
  Font: ISkFont;
  AMessage: string;
  ToastBox: TSkPaintBox;
  TextBlob: ISkTextBlob;
  TextBounds: TRectF;
  ScaleFactor: Single;
begin
  if not (Sender is TSkPaintBox) then Exit;

  ToastBox := TSkPaintBox(Sender);

  // Retrieve Message
  if Assigned(ToastBox.TagObject) and (ToastBox.TagObject is TStringList) then
    AMessage := Trim(TStringList(ToastBox.TagObject).Text)
  else
    AMessage := 'Toast Message';

  // === 🌙 Shadow Effect ===
  ShadowPaint := TSkPaint.Create;
  ShadowPaint.Color := $77000000; // Dark shadow with transparency
  ShadowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 8);
  Canvas.DrawRoundRect(
    TRectF.Create(Dest.Left + 2, Dest.Top + 4, Dest.Right, Dest.Bottom),
    CornerRadius, CornerRadius, ShadowPaint
  );


  Paint := TSkPaint.Create;
  Paint.Color := TAlphaColors.Dodgerblue; // Solid background
  Paint.AntiAlias := True;

  Canvas.DrawRoundRect(
    TRectF.Create(Dest.Left, Dest.Top, Dest.Right, Dest.Bottom),
    CornerRadius, CornerRadius, Paint
  );

  // Text Configuration
  Font := TSkFont.Create(nil, BaseFontSize);
  Font.Edging := TSkFontEdging.Antialias;
  Font.Size := BaseFontSize;

  Paint := TSkPaint.Create;
  Paint.Color := TAlphaColors.White;
  Paint.AntiAlias := True;

  // Measure and scale text
  Font.MeasureText(AMessage, TextBounds, Paint);
  ScaleFactor := Min((Dest.Width - 2 * Padding) / TextBounds.Width,
                     (Dest.Height - 2 * Padding) / TextBounds.Height);
  Font.Size := BaseFontSize * ScaleFactor;
  Font.MeasureText(AMessage, TextBounds, Paint);

  // Draw text centered in the toast
  TextBlob := TSkTextBlob.MakeFromText(AMessage, Font);
  if Assigned(TextBlob) then
    Canvas.DrawTextBlob(TextBlob,
      Dest.CenterPoint.X - TextBounds.Width / 2,
      Dest.CenterPoint.Y + TextBounds.Height / 4, Paint);
end;


class procedure TToastHelper.HideToast(Sender: TObject);
var
  ToastBox: TSkPaintBox;
  FadeOut: TFloatAnimation;
begin
  if not (Sender is TTimer) then Exit;

  // Retrieve the TSkPaintBox stored in the Timer's TagObject
  ToastBox := TSkPaintBox(TTimer(Sender).TagObject);
  if not Assigned(ToastBox) then Exit;

  // Stop and free the timer
  TTimer(Sender).Free;

  // Create fade-out animation
  FadeOut := TFloatAnimation.Create(ToastBox);
  FadeOut.Parent := ToastBox;
  FadeOut.PropertyName := 'Opacity';
  FadeOut.StartValue := 1;
  FadeOut.StopValue := 0;
  FadeOut.Duration := 0.5;
  FadeOut.OnFinish := TToastHelper.OnFadeOutFinish;
  FadeOut.Start;
end;

class procedure TToastHelper.OnFadeOutFinish(Sender: TObject);
var
  ToastBox: TSkPaintBox;
  Layout: TLayout;
begin
  if not (Sender is TFloatAnimation) then Exit;

  ToastBox := TSkPaintBox(TFloatAnimation(Sender).Parent);
  if not Assigned(ToastBox) then Exit;

  Layout := TLayout(ToastBox.Parent);

  // Free stored message
  if Assigned(ToastBox.TagObject) then
  begin
    ToastBox.TagObject.Free;
    ToastBox.TagObject := nil;
  end;

  // Remove layout and toast
  Layout.Free;

  // Show next toast if available
  FLock.Enter;
  try
    FIsShowing := False;
    if FToastQueue.Count > 0 then
      ShowNextToast;
  finally
    FLock.Leave;
  end;
end;

procedure ShowSkiaToast(const AMessage: string; AOwner: TForm;
  APosition: Single = 0.9; ADuration: Integer = 2000);
var
  ToastItem: TToastItem;
begin
  if not Assigned(AOwner) then Exit;

  TToastHelper.FLock.Enter;
  try
    ToastItem.Message := AMessage;
    ToastItem.Duration := ADuration;
    ToastItem.Owner := AOwner;
    ToastItem.Position := APosition;

    TToastHelper.FToastQueue.Enqueue(ToastItem);

    if not TToastHelper.FIsShowing then
      TToastHelper.ShowNextToast;
  finally
    TToastHelper.FLock.Leave;
  end;
end;

initialization
  TToastHelper.Initialize;

finalization
  TToastHelper.Finalize;

end.

