unit CardCollectionform;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  SGlobalsZ, DataModuleUnit,
  FMX.Controls.Presentation, FMX.StdCtrls, FireDAC.Comp.Client, data.DB,
  System.Math,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Gestures, FMX.WebBrowser;

type
  TCardData = class
    CardName: string;
    CardType: string;
    ManaCost: string;
    Image: TBitmap;
    constructor Create(const AName, AType, AMana: string; AImage: TBitmap);
    destructor Destroy; override;
  end;

  TCollectionsForm = class(TForm)
    ScrollBox1: TScrollBox;
    FlowLayout1: TFlowLayout;
    Layout1: TLayout;
    ImageControl1: TImageControl;
    Label1: TLabel;
    Label2: TLabel;
    StyleBook1: TStyleBook; // Ensure GestureManager is added to the form
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadCollectionFromDatabase;
    procedure AdjustFlowLayoutSize(FlowLayout: TFlowLayout);
//    procedure LoadCollectionToListView(ListView: TListView);
    procedure CardLayoutClick(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CollectionsForm: TCollectionsForm;

implementation

{$R *.fmx}

{ TCardData }

constructor TCardData.Create(const AName, AType, AMana: string; AImage: TBitmap);
begin
  CardName := AName;
  CardType := AType;
  ManaCost := AMana;
  Image := AImage;
end;

destructor TCardData.Destroy;
begin
  if Assigned(Image) then
    Image.Free;
  inherited;
end;

{ TCollectionsForm }

procedure TCollectionsForm.FormCreate(Sender: TObject);
begin
  LoadCollectionFromDatabase;
  AdjustFlowLayoutSize(FlowLayout1);
end;

procedure TCollectionsForm.LoadCollectionFromDatabase;
var
  Query: TFDQuery;
  CardLayout: TLayout;
  CardImage: TImageControl;
  CardNameLabel: TLabel;
  ImageStream: TMemoryStream;
  CardData: TCardData;
  const
    FIELD_IMAGE_BLOB = 'ImageBlob';
    FIELD_CARD_NAME = 'CardName';
    FIELD_MANA_COST = 'ManaCost';
    FIELD_TYPE_LINE = 'TypeLine';
begin
  FlowLayout1.BeginUpdate;
  try
    // Clear existing controls and free associated CardData
    for var i := FlowLayout1.Controls.Count - 1 downto 0 do
    begin
      var ctl := FlowLayout1.Controls[i];
      if Assigned(ctl.TagObject) then
      begin
        TCardData(ctl.TagObject).Free;
        ctl.TagObject := nil;
      end;
      ctl.Free;
    end;

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DataModule1.FDConnection1;
      Query.SQL.Text := 'SELECT * FROM CardDetails';
      Query.Open;

      while not Query.Eof do
      begin
        // Create a new layout for each card
        CardLayout := TLayout.Create(FlowLayout1);
        CardLayout.Parent := FlowLayout1;
        CardLayout.Width := 175;
        CardLayout.Height := 245;
        CardLayout.Margins.Bottom := 10;
        CardLayout.HitTest := True; // Enable hit testing for interactions

        // Initialize CardData with card details
        CardData := TCardData.Create(
          Query.FieldByName(FIELD_CARD_NAME).AsString,
          Query.FieldByName(FIELD_TYPE_LINE).AsString,
          Query.FieldByName(FIELD_MANA_COST).AsString,
          nil); // Image will be loaded next

        // Assign CardData to TagObject for later retrieval
        CardLayout.TagObject := CardData;

        // Card Image
        CardImage := TImageControl.Create(CardLayout);
        CardImage.Parent := CardLayout;
        CardImage.Align := TAlignLayout.Top;
        CardImage.Height := 175; // Adjust based on layout
        CardImage.Margins.Bottom := 5;
        CardImage.HitTest := False; // Prevent image from intercepting clicks

        // Load image from database
        if not Query.FieldByName(FIELD_IMAGE_BLOB).IsNull then
        begin
          ImageStream := TMemoryStream.Create;
          try
            TBlobField(Query.FieldByName(FIELD_IMAGE_BLOB)).SaveToStream(ImageStream);
            ImageStream.Position := 0;
            CardData.Image := TBitmap.Create;
            CardData.Image.LoadFromStream(ImageStream);
            CardImage.Bitmap.Assign(CardData.Image); // Assign the loaded image to the ImageControl
          finally
            ImageStream.Free;
          end;
        end;

        // Card Name Label
        CardNameLabel := TLabel.Create(CardLayout);
        CardNameLabel.Parent := CardLayout;
        CardNameLabel.Align := TAlignLayout.Top;
        CardNameLabel.Text := CardData.CardName;
        CardNameLabel.StyledSettings := []; // Allow custom styling
        CardNameLabel.TextSettings.Font.Size := 12;
        CardNameLabel.TextSettings.Font.Style := [TFontStyle.fsBold];
        CardNameLabel.TextSettings.FontColor := TAlphaColors.Red;
        CardNameLabel.TextSettings.HorzAlign := TTextAlign.Center;
        CardNameLabel.Margins.Bottom := 5;

        // Assign the OnClick event to handle card clicks
        CardLayout.OnClick := CardLayoutClick;

        // Move to the next record
        Query.Next;
      end;
    finally
      Query.Free;
    end;
  finally
    FlowLayout1.EndUpdate;
  end;
end;

procedure TCollectionsForm.CardLayoutClick(Sender: TObject);
var
  CardData: TCardData;
 // DetailForm: TCardDetailForm; // Optional: a separate form to show details
begin
  if Sender is TLayout then
  begin
    CardData := TCardData(TLayout(Sender).TagObject);
    if Assigned(CardData) then
    begin
      // Option 1: Display card info using a message box
      ShowMessage(Format(
        'Name: %s'#13#10 +
        'Type: %s'#13#10 +
        'Mana Cost: %s',
        [CardData.CardName, CardData.CardType, CardData.ManaCost]));

      // Option 2: Open a detailed view form
      // Uncomment and implement if you have a separate form for details
      // DetailForm := TCardDetailForm.Create(nil);
      // try
      //   DetailForm.LoadCardData(CardData); // Implement this method
      //   DetailForm.ShowModal;
      // finally
      //   DetailForm.Free;
      // end;
    end;
  end;
end;

procedure TCollectionsForm.AdjustFlowLayoutSize(FlowLayout: TFlowLayout);
var
  TotalWidth, TotalHeight, RowHeight, ItemWidth: Single;
  Control: TControl;
  MaxWidth: Single;
  HorizontalSpacing, VerticalSpacing: Single;
begin
  // maximum width for FlowLayout
  MaxWidth := TControl(FlowLayout.Parent).Width;

  // Set spacing
  HorizontalSpacing := 10;
  VerticalSpacing := 10;

  TotalWidth := 0;
  TotalHeight := VerticalSpacing; // Start with initial vertical spacing
  RowHeight := 0;

  for Control in FlowLayout.Controls do
  begin
    ItemWidth := Control.Width + HorizontalSpacing;

    if (TotalWidth + ItemWidth) > MaxWidth then
    begin
      // Move to next row
      TotalHeight := TotalHeight + RowHeight + VerticalSpacing;
      TotalWidth := 0;
      RowHeight := 0;
    end;

    // Update row and layout widths and heights
    RowHeight := Max(RowHeight, Control.Height);
    TotalWidth := TotalWidth + ItemWidth;
  end;

  // Add height for the last row
  TotalHeight := TotalHeight + RowHeight;

  // Set FlowLayout size
  FlowLayout.Width := MaxWidth;
  FlowLayout.Height := TotalHeight / 4;

  // Optional: Log the final size for debugging
  // ShowMessage(Format('FlowLayout Height: %f, Width: %f', [FlowLayout.Height, FlowLayout.Width]));
end;

end.
