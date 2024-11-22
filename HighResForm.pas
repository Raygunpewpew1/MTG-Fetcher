unit HighResForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls, FMX.Forms, FMX.Objects,
  System.Net.HttpClient,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Types,fmx.Dialogs;

type
  THighResImageForm = class(TForm)
    CloseButton: TButton;
    HighResImageControl: TImage;
    StyleBook1: TStyleBook;

    procedure CloseButtonClick(Sender: TObject);
  private
    procedure LoadImage(const ImageURL: string);

  public
    constructor Create(AOwner: TComponent); override;

    procedure ShowImage(const ImageURL: string; const CardName: string);

  end;

var
  HighResImageForm: THighResImageForm;

implementation

{$R *.fmx}



constructor THighResImageForm.Create(AOwner: TComponent);
begin
  inherited;
//  HighResImageControl := TImage.Create(Self);
//  HighResImageControl.Align := TAlignLayout.Client;
//  AddObject(HighResImageControl);
//
//  CloseButton := TButton.Create(Self);
//  CloseButton.Text := 'Close';
//  CloseButton.Align := TAlignLayout.Bottom;
//  CloseButton.OnClick := CloseButtonClick;
//  AddObject(CloseButton);
end;

procedure THighResImageForm.CloseButtonClick(Sender: TObject);
begin
  Close;
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
      begin
        // Display error message
        ShowMessage('Failed to load image: ' + E.Message);
        // Optionally, you can display the ErrorLabel here
      end;
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

procedure THighResImageForm.ShowImage(const ImageURL: string; const CardName: string);
begin
  Caption := CardName; // Set the form's caption to the card's name
  LoadImage(ImageURL); // Load the image
  ShowModal; // Display the form modally
end;

end.
