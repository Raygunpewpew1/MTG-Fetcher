unit BulkData;
        // not using just keeping for referance
interface

uses
  System.SysUtils,
  System.Generics.Collections,
  SGlobalsZ,
  JsonDataObjects,
  System.Classes,
  Logger,

  WrapperHelper,
  APIConstants,
  System.IOUtils,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Threading;


  procedure DownloadBulkData(const CardDataList: TList<TCardDetails>);
  procedure LoadBulkData(const FilePath: string; CardDataList: TList<TCardDetails>);
  procedure OnDownloadProgress(const Sender: TObject;
    AContentLength, AReadCount: Int64; var Abort: Boolean);

implementation

procedure LoadBulkData(const FilePath: string; CardDataList: TList<TCardDetails>);
var
  BulkFileStream: TStringStream;
  BulkJsonArray: TJsonArray;
  CardObj: TJsonObject;
  CardDetails: TCardDetails;
  I: Integer;
begin
  if not FileExists(FilePath) then
    raise Exception.Create('Bulk data file not found: ' + FilePath);

  BulkFileStream := TStringStream.Create;
  try
    // Load the bulk JSON data from the file
    BulkFileStream.LoadFromFile(FilePath);

    // Parse the root JSON array
    BulkJsonArray := TJsonArray.Parse(BulkFileStream.DataString) as TJsonArray;
    try
      // Clear or prepare list
      CardDataList.Clear;
      CardDataList.Capacity := BulkJsonArray.Count;

      // Iterate over the array and populate CardDataList
      for I := 0 to BulkJsonArray.Count - 1 do
      begin
        if BulkJsonArray.Types[I] = jdtObject then
        begin
          CardObj := BulkJsonArray.O[I]; // Get card object
          try
            FillCardDetailsFromJson(CardObj, CardDetails);
            CardDataList.Add(CardDetails);
          except
            on E: Exception do
              LogStuff(Format('Error processing card at index %d: %s', [I, E.Message]));
          end;
        end
        else
          LogStuff(Format('Skipping non-object element at index %d', [I]));
      end;
    finally
      BulkJsonArray.Free;
    end;
  finally
    BulkFileStream.Free;
  end;
end;

procedure OnDownloadProgress(const Sender: TObject;
  AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  //
  // if AContentLength > 0 then
  //   TThread.Queue(nil,
  //     procedure
  //     begin
  //       ProgressBar1.Max := 100;
  //       ProgressBar1.Value := (AReadCount / AContentLength) * 100;
  //     end);
end;

procedure DownloadBulkData(const CardDataList: TList<TCardDetails>);
var
  AppDataPath, SavePathX: string;
begin
  AppDataPath := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  if not TDirectory.Exists(AppDataPath) then
    TDirectory.CreateDirectory(AppDataPath);

  SavePathX := TPath.Combine(AppDataPath, BulkDataPath);

  // If the file already exists, just load it and exit
  if FileExists(SavePathX) then
  begin
    LoadBulkData(SavePathX, CardDataList);
    Exit;
  end;

  // Example: In a real form, you might show a progress label or bar:
  //   ProgressBar1.Visible := True;
  //   LabelProgress.Visible := True;
  //   LabelProgress.Text := S_FETCHING_BULK_DATA_METADATA;

  // Launch a background thread to handle the download
  TTask.Run(
    procedure
    var
      HttpClient: THTTPClient;
      MetadataStream, DataStream: TStringStream;
      MetadataJson: TJsonObject;
      DownloadURI: string;
    begin
      HttpClient := THTTPClient.Create;
      try
        MetadataStream := TStringStream.Create;
        DataStream := TStringStream.Create;
        try
          try
            // Update UI in main thread, if needed
            TThread.Queue(nil,
              procedure
              begin
                // LabelProgress.Text := S_FETCHING_BULK_DATA_METADATA;
              end);

            // Set custom headers
            HttpClient.CustomHeaders['User-Agent'] := UserAgent;
            HttpClient.CustomHeaders['Accept'] := AcceptHeader;

            // Download metadata
            HttpClient.Get('https://api.scryfall.com/bulk-data', MetadataStream);

            // Parse metadata JSON
            MetadataJson := TJsonObject.Parse(MetadataStream.DataString) as TJsonObject;
            try
              if MetadataJson.Contains(S_DATA) then
              begin
                for var Obj in MetadataJson.A['data'] do
                begin
                  if (Obj.S[S_TYPE] = S_ORACLE_CARDS) then
                  begin
                    DownloadURI := Obj.S[S_DOWNLOAD_URI];
                    Break;
                  end;
                end;
              end;

              if DownloadURI.IsEmpty then
                raise Exception.Create(S_DOWNLOAD_URI_FOR_DEFAULT_CARDS_NOT_FOUND);

              TThread.Queue(nil,
                procedure
                begin
                  // LabelProgress.Text := S_DOWNLOADING_BULK_DATA;
                  LogStuff(S_DOWNLOADING_BULK_DATA);
                end);

              // Hook up download progress if desired
              // HttpClient.OnReceiveData := OnDownloadProgress;
              HttpClient.Get(DownloadURI, DataStream);

              // Save data to file
              DataStream.SaveToFile(SavePathX);

              // Then load it into CardDataList
              TThread.Queue(nil,
                procedure
                begin
                  // LabelProgress.Text := S_DOWNLOAD_COMPLETE_FILE_SAVED_TO + SavePathX;
                  LoadBulkData(SavePathX, CardDataList);
                  LogStuff(S_DOWNLOAD_COMPLETE_FILE_SAVED_TO + SavePathX);
                end);

            finally
              MetadataJson.Free;
            end;

          except
            on E: Exception do
            begin
              // Update UI / log in main thread
              TThread.Queue(nil,
                procedure
                begin

                  // LabelProgress.Text := S_ERROR_DOWNLOADING_BULK_DATA;
                  // ShowMessage(S_ERROR + E.Message);
                  LogStuff(S_ERROR_DOWNLOADING_BULK_DATA + ': ' + E.Message);
                end);
            end;
          end;

        finally
          MetadataStream.Free;
          DataStream.Free;

          // Hide or reset UI in main thread
          TThread.Queue(nil,
            procedure
            begin
              // ProgressBar1.Visible := False;
            end);
        end;
      finally
        HttpClient.Free;
      end;
    end);
end;


end.
