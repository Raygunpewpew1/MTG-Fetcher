unit ImgListUnit;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms, System.ImageList, FMX.ImgList;

type
  TFImageList = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FImageListForm: TFImageList;

implementation

{$R *.fmx}

procedure TFImageList.FormCreate(Sender: TObject);
begin
  if not Assigned(ImageList1) then
  begin
    ImageList1 := TImageList.Create(Self);
  end;
end;

end.
