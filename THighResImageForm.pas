unit Form2;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls, FMX.Forms, FMX.Objects, System.Net.HttpClient,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Types;

type

  TForm2 = class(TForm)
    ImageControl1: TImageControl;
    Button1: TButton;
    procedure CloseButtonClick(Sender: TObject);
  private
    procedure LoadImage(const ImageURL: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowImage(const ImageURL: string);
  end;

var
  HighResImageForm: THighResImageForm;

implementation

{$R *.fmx}

constructor THighResImageForm.Create(AOwner: TComponent);
begin
  inherited;
  HighResImageControl := TImageControl.Create(Self);
  HighResImageControl.Align := TAlignLayout.Client;
  AddObject(HighResImageControl);

  CloseButton := TButton.Create(Self);
  CloseButton.Text := 'Close';
  CloseButton.Align := TAlignLayout.Bottom;
  CloseButton.OnClick := CloseButtonClick;
  AddObject(CloseButton);
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
    HttpClient.Get(ImageURL, MemoryStream);
    MemoryStream.Position := 0;
    HighResImageControl.Bitmap.LoadFromStream(MemoryStream);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

procedure THighResImageForm.ShowImage(const ImageURL: string);
begin
  LoadImage(ImageURL);
  ShowModal;
end;

end.
