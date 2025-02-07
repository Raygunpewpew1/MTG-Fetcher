unit ISmellToast;

interface

uses
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Objects, FMX.Ani, FMX.Skia,
  System.UITypes, FMX.Forms, System.Types, System.Skia, System.Classes, System.SysUtils;

procedure ShowSkiaToast(const AMessage: string; AOwner: TForm; APosition: Single = 0.8);

implementation

type
  TToastHelper = class
    class procedure PaintToast(Sender: TObject; const Canvas: ISkCanvas; const Dest: TRectF; const Opacity: Single);
    class procedure HideToast(Sender: TObject);
    class procedure OnFadeOutFinish(Sender: TObject); // handle animation finish
  end;



{ Paint the toast message using Skia }
class procedure TToastHelper.PaintToast(Sender: TObject; const Canvas: ISkCanvas; const Dest: TRectF; const Opacity: Single);
var
  Paint: ISkPaint;
  Font: ISkFont;
  AMessage: string;
  ToastBox: TSkPaintBox;
  TextBlob: ISkTextBlob;
  TextBounds: TRectF;
  BackgroundColor, TextColor: TAlphaColor;
begin
  if not (Sender is TSkPaintBox) then Exit;

  ToastBox := TSkPaintBox(Sender);

  // Set background and text colors
  BackgroundColor := TAlphaColors.Black;
  TextColor := TAlphaColors.White;

  // Create paint for the background
  Paint := TSkPaint.Create;
  Paint.Color := BackgroundColor;
  Paint.AlphaF := 0.85; // Slight transparency
  Paint.AntiAlias := True;

  // Draw the rounded background
  Canvas.DrawRoundRect(Dest, 20, 20, Paint);

  // Create and configure the font
  Font := TSkFont.Create(nil, 22); // Set font size
  Font.Edging := TSkFontEdging.Antialias;

  // Set the text color
  Paint.Color := TextColor;

  // Retrieve message from TagObject
   if Assigned(ToastBox.TagObject) and (ToastBox.TagObject is TStringList) then
    AMessage := Trim(TStringList(ToastBox.TagObject).Text)
   else
    AMessage := 'Toast Message';


  // Measure text using ISkFont.MeasureText
  Font.MeasureText(AMessage, TextBounds, Paint);

  // Create text blob
  TextBlob := TSkTextBlob.MakeFromText(AMessage, Font);
  if Assigned(TextBlob) then
  begin
    // Draw the text centered in the toast
    Canvas.DrawTextBlob(
      TextBlob,
      Dest.CenterPoint.X - (TextBounds.Width / 2), // Center horizontally
      Dest.CenterPoint.Y + (TextBounds.Height / 2), // Center vertically
      Paint
    );
  end;
end;


{ Hides the toast with fade-out animation }
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
    ToastBox.TagObject.Free;

  // Remove layout and toast
  Layout.Free;
end;



{ Show the toast message at a custom position }
procedure ShowSkiaToast(const AMessage: string; AOwner: TForm; APosition: Single);
var
  Layout: TLayout;
  ToastBox: TSkPaintBox;
  Animation: TFloatAnimation;
  HideTimer: TTimer;
  MsgHolder: TStringList;
begin
  // Create a container layout
  Layout := TLayout.Create(AOwner);
  Layout.Parent := AOwner;
  Layout.Align := TAlignLayout.None;
  Layout.Width := 200;   // Set fixed width
  Layout.Height := 40;   // Set height
  Layout.Position.X := 20;
  Layout.Position.Y := AOwner.Height * APosition - Layout.Height / 2; // Adjust position
  Layout.BringToFront;

  // Create the toast box
  ToastBox := TSkPaintBox.Create(Layout);
  ToastBox.Parent := Layout;
  ToastBox.Align := TAlignLayout.Contents;
  ToastBox.Opacity := 0; // Initially hidden
  ToastBox.OnDraw := TToastHelper.PaintToast;

  // Store message in TagObject
  MsgHolder := TStringList.Create;
  MsgHolder.Text := Trim(AMessage);
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
  HideTimer := TTimer.Create(AOwner);
  HideTimer.Interval := 2000; // Show for 2 seconds
  HideTimer.OnTimer := TToastHelper.HideToast; // ✅ Correct method reference
  HideTimer.TagObject := ToastBox; // Store reference to toast for cleanup
  HideTimer.Enabled := True;

end;

end.

