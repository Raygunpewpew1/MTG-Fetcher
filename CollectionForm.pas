unit CollectionForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts,DataModuleUnit,Data.DB,FireDAC.Comp.Client,
  FMX.Objects, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    ScrollBox1: TScrollBox;
    ListBox1: TListBox;
    Edit1: TEdit;
    Image1: TImage;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadCardsFromDatabase;
    procedure AddCardToListBox(const CardName, CardType, ManaCost: string;
      CardImage: TBitmap);
    procedure FilterListBoxCards(const SearchText: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.AddCardToListBox(const CardName, CardType, ManaCost: string; CardImage: TBitmap);
var
  ListItem: TListBoxItem;
  Image: TImage;
  NameLabel, TypeLabel, ManaCostLabel: TLabel;
begin
  // Create a new ListBoxItem
  ListItem := TListBoxItem.Create(ListBox1);
  ListItem.Parent := ListBox1;  // Add it to the ListBox
  ListItem.Height := 100;  // Set the height of each item

  // Add Image to ListBoxItem
  Image := TImage.Create(ListItem);
  Image.Parent := ListItem;
  Image.Bitmap := CardImage;  // Assign the image for the card
  Image.Align := TAlignLayout.Left;  // Align it to the left side of the item
  Image.Width := 80;  // Set the image width (adjust as needed)

  // Add Card Name Label
  NameLabel := TLabel.Create(ListItem);
  NameLabel.Parent := ListItem;
  NameLabel.Align := TAlignLayout.Top;
  NameLabel.Text := CardName;
  NameLabel.TextSettings.Font.Style := [TFontStyle.fsBold];  // Make the card name bold

  // Add Type Label (card type)
  TypeLabel := TLabel.Create(ListItem);
  TypeLabel.Parent := ListItem;
  TypeLabel.Align := TAlignLayout.Top;
  TypeLabel.Text := 'Type: ' + CardType;

  // Add Mana Cost Label
  ManaCostLabel := TLabel.Create(ListItem);
  ManaCostLabel.Parent := ListItem;
  ManaCostLabel.Align := TAlignLayout.Top;
  ManaCostLabel.Text := 'Mana Cost: ' + ManaCost;
end;


procedure TForm2.LoadCardsFromDatabase;
var
  Query: TFDQuery;
  CardName, CardType, ManaCost: string;
  CardImage: TBitmap;
  ImageStream: TMemoryStream;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;  // Your FireDAC connection
    Query.SQL.Text := 'SELECT CardName, TypeLine, ManaCost, ImageBlob FROM CardDetails';
    Query.Open;

    while not Query.Eof do
    begin
      // Fetch card details from the database
      CardName := Query.FieldByName('CardName').AsString;
      CardType := Query.FieldByName('TypeLine').AsString;
      ManaCost := Query.FieldByName('ManaCost').AsString;

      // Load the image if available
      if not Query.FieldByName('ImageBlob').IsNull then
      begin
        ImageStream := TMemoryStream.Create;
        CardImage := TBitmap.Create;
        try
          TBlobField(Query.FieldByName('ImageBlob')).SaveToStream(ImageStream);
          ImageStream.Position := 0;
          CardImage.LoadFromStream(ImageStream);

          // Add the card to ListBox
          AddCardToListBox(CardName, CardType, ManaCost, CardImage);
        finally
          CardImage.Free;
          ImageStream.Free;
        end;
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;



procedure TForm2.Edit1Change(Sender: TObject);
begin
FilterListBoxCards(edit1.Text);
end;

procedure TForm2.FilterListBoxCards(const SearchText: string);
var
  i: Integer;
  ListItem: TListBoxItem;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    // Directly cast the item to TListBoxItem
    ListItem := TListBoxItem(ListBox1.Items[i]);

    if SearchText.IsEmpty then
      ListItem.Visible := True
    else
      ListItem.Visible := ListItem.Text.ToLower.Contains(SearchText.ToLower);
  end;
end;


procedure TForm2.FormCreate(Sender: TObject);
begin
LoadCardsFromDatabase;
end;

end.
