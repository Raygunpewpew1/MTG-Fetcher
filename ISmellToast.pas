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
    class procedure HideToast(Sender: TObject);
    class procedure OnFadeOutFinish(Sender: TObject);
    class procedure PaintToast(Sender: TObject; const Canvas: ISkCanvas;
      const Dest: TRectF; const Opacity: Single);
  public
    class procedure Initialize;
    class procedure Finalize;
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
  FreeAndNil(FLock);
  FreeAndNil(FToastQueue);
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
  TToastHelper.FLock.Enter;
  try
    if FIsShowing or (FToastQueue.Count = 0) then Exit;
    FIsShowing := True;
    ToastItem := FToastQueue.Dequeue;
  finally
    TToastHelper.FLock.Leave;
  end;

  if not Assigned(ToastItem.Owner) then
  begin
    TToastHelper.FLock.Enter;
    try
      FIsShowing := False;
    finally
      TToastHelper.FLock.Leave;
    end;
    Exit;
  end;

  Layout := TLayout.Create(ToastItem.Owner);
  Layout.Parent := ToastItem.Owner;
  Layout.Align := TAlignLayout.None;
  Layout.Height := 40;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  Font := TSkFont.Create(nil, 22);
  Font.MeasureText(ToastItem.Message, TextBounds, Paint);
  TextWidth := TextBounds.Width + PADDING * 2;

  Layout.Width := Min(300, Max(100, TextWidth));
  Layout.Position.X := 20;
  Layout.Position.Y := ToastItem.Owner.Height * ToastItem.Position - Layout.Height;
  Layout.BringToFront;

  ToastBox := TSkPaintBox.Create(Layout);
  ToastBox.Parent := Layout;
  ToastBox.Align := TAlignLayout.Contents;
  ToastBox.Opacity := 0;
  ToastBox.OnDraw := PaintToast;

  // Create TStringList and store it
  MsgHolder := TStringList.Create;
  try
    MsgHolder.Text := Trim(ToastItem.Message);
    ToastBox.TagObject := MsgHolder;
  except
    MsgHolder.Free;
    raise;
  end;

  Animation := TFloatAnimation.Create(ToastBox);
  Animation.Parent := ToastBox;
  Animation.PropertyName := 'Opacity';
  Animation.StartValue := 0;
  Animation.StopValue := 1;
  Animation.Duration := 0.5;
  Animation.Start;

  HideTimer := TTimer.Create(ToastItem.Owner);
  HideTimer.Interval := ToastItem.Duration;
  HideTimer.OnTimer := HideToast;
  HideTimer.TagObject := ToastBox;
  HideTimer.Enabled := True;
end;


class procedure TToastHelper.PaintToast(Sender: TObject; const Canvas: ISkCanvas;
  const Dest: TRectF; const Opacity: Single);
const
  Padding = 12;
  BaseFontSize = 16;
  CornerRadius = 16;
var
  Paint, ShadowPaint: ISkPaint;
  Font: ISkFont;
  AMessage: string;
  ToastBox: TSkPaintBox;
  TextBlob: ISkTextBlob;
  TextBounds: TRectF;
  ScaleFactor: Single;
  ShadowRect: TRectF;
begin
  if not (Sender is TSkPaintBox) then Exit;
  ToastBox := TSkPaintBox(Sender);

  if Assigned(ToastBox.TagObject) and (ToastBox.TagObject is TStringList) then
    AMessage := Trim(TStringList(ToastBox.TagObject).Text)
  else
    AMessage := 'Toast Message';




  ShadowPaint := TSkPaint.Create;
  ShadowPaint.Color := $77000000;
  ShadowPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 8);
  ShadowRect := Dest;
  ShadowRect.Inflate(2, 4);
  Canvas.DrawRoundRect(ShadowRect, CornerRadius, CornerRadius, ShadowPaint);

  Paint := TSkPaint.Create;
  Paint.Color := TAlphaColors.Dodgerblue;
  Paint.AntiAlias := True;
  Canvas.DrawRoundRect(Dest, CornerRadius, CornerRadius, Paint);

  Font := TSkFont.Create(nil, BaseFontSize);
  Font.Edging := TSkFontEdging.Antialias;
  Font.Size := BaseFontSize;

  Paint := TSkPaint.Create;
  Paint.Color := TAlphaColors.White;
  Paint.AntiAlias := True;

  Font.MeasureText(AMessage, TextBounds, Paint);
  ScaleFactor := Min((Dest.Width - 2 * Padding) / TextBounds.Width,
                     (Dest.Height - 2 * Padding) / TextBounds.Height);
  Font.Size := BaseFontSize * ScaleFactor;
  Font.MeasureText(AMessage, TextBounds, Paint);

  TextBlob := TSkTextBlob.MakeFromText(AMessage, Font);
  if Assigned(TextBlob) then
    Canvas.DrawTextBlob(TextBlob, Dest.CenterPoint.X - TextBounds.Width / 2,
                        Dest.CenterPoint.Y + TextBounds.Height / 4, Paint);
end;

class procedure TToastHelper.HideToast(Sender: TObject);
var
  ToastBox: TSkPaintBox;
  FadeOut: TFloatAnimation;
begin
  if not (Sender is TTimer) then Exit;

  ToastBox := TSkPaintBox(TTimer(Sender).TagObject);
  FreeAndNil(TTimer(Sender));

  if not Assigned(ToastBox) or (ToastBox.Parent = nil) then Exit;

  // Check if fade-out animation is already running
  if ToastBox.FindComponent('FadeOutAnimation') <> nil then Exit;

  // Create fade-out animation
  FadeOut := TFloatAnimation.Create(ToastBox);
  FadeOut.Name := 'FadeOutAnimation';
  FadeOut.Parent := ToastBox;
  FadeOut.PropertyName := 'Opacity';
  FadeOut.StartValue := 1;
  FadeOut.StopValue := 0;
  FadeOut.Duration := 0.5;
  FadeOut.OnFinish := OnFadeOutFinish;
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
  if not Assigned(Layout) then Exit;

  // Free TStringList stored in TagObject
  if Assigned(ToastBox.TagObject) then
  begin
    ToastBox.TagObject.Free;
    ToastBox.TagObject := nil;
  end;

  // Detach animation before freeing
  TFloatAnimation(Sender).OnFinish := nil;
  TFloatAnimation(Sender).Parent := nil;
  FreeAndNil(TFloatAnimation(Sender));

  // Remove from parent before freeing
  Layout.RemoveObject(ToastBox);
  FreeAndNil(ToastBox);
  FreeAndNil(Layout);

  // Reset flag and check queue
  TToastHelper.FLock.Enter;
  try
    TToastHelper.FIsShowing := False;
    if TToastHelper.FToastQueue.Count > 0 then
      TToastHelper.ShowNextToast;
  finally
    TToastHelper.FLock.Leave;
  end;
end;



procedure ShowSkiaToast(const AMessage: string; AOwner: TForm;
  APosition: Single; ADuration: Integer);
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

