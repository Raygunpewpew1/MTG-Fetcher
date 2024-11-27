unit HighResForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls, FMX.Forms, FMX.Objects,
  System.Net.HttpClient, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Types, FMX.Dialogs;

type
  THighResImageForm = class(TForm)
    CloseButton: TButton;
    HighResImageControl: TImage;

    procedure CloseButtonClick(Sender: TObject);

  private
    procedure LoadImage(const ImageURL: string);

  public
    procedure ShowImage(const ImageURL: string; const CardName: string);
  end;

var
  HighResImageForm: THighResImageForm;
  Form1: TForm; // Forward declaration for Form1

implementation

{$R *.fmx}

procedure THighResImageForm.CloseButtonClick(Sender: TObject);
begin
  // Show Form1 (forward-declared)
  if Assigned(Form1) then
    Form1.Show;

  // Free the current form
  Self.Hide;
end;

procedure THighResImageForm.LoadImage(const ImageURL: string);
var
  HttpClient: THTTPClient;
  MemoryStream: TMemoryStream;
begin
  HttpClient := THTTPClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
    try
      HttpClient.Get(ImageURL, MemoryStream);
      MemoryStream.Position := 0;
      HighResImageControl.Bitmap.LoadFromStream(MemoryStream);
    except
      on E: Exception do
        ShowMessage('Failed to load image: ' + E.Message);
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

procedure THighResImageForm.ShowImage(const ImageURL: string; const CardName: string);
begin
  Caption := CardName;  // Set form caption
  LoadImage(ImageURL);  // Load the image
  Self.Show;            // Use non-modal display on Android
end;

end.
