unit JSONHelper;

interface

uses
  System.JSON, System.SysUtils, System.Generics.Collections;

type
  TJSONParser = class
  public
    // Basic types
    class function GetString(const Obj: TJSONObject; const Key: string; const Default: string = ''): string; static;
    class function GetInteger(const Obj: TJSONObject; const Key: string; const Default: Integer = 0): Integer; static;
    class function GetBoolean(const Obj: TJSONObject; const Key: string; const Default: Boolean = False): Boolean; static;
    class function GetFloat(const Obj: TJSONObject; const Key: string; const Default: Double = 0.0): Double; static;

    // Complex types
    class function GetObject(const Obj: TJSONObject; const Key: string): TJSONObject; static;
    class function GetArray(const Obj: TJSONObject; const Key: string): TJSONArray; static;

    // Nested key handling (e.g., "data.card_faces[0].mana_cost")
    class function GetNestedValue(const Obj: TJSONObject; const KeyPath: string; const Default: string = ''): string; static;

    // Utility functions
    class function KeyExists(const Obj: TJSONObject; const Key: string): Boolean; static;

    // Convert JSON array to string array
    class function JSONArrayToStringArray(const JSONArray: TJSONArray): TArray<string>; static;
  end;

implementation

{ TJSONParser }

class function TJSONParser.GetString(const Obj: TJSONObject; const Key: string; const Default: string): string;
begin
  if Obj.TryGetValue<string>(Key, Result) then
    Exit(Result)
  else
    Result := Default;
end;

class function TJSONParser.GetInteger(const Obj: TJSONObject; const Key: string; const Default: Integer): Integer;
begin
  if Obj.TryGetValue<Integer>(Key, Result) then
    Exit(Result)
  else
    Result := Default;
end;

class function TJSONParser.GetBoolean(const Obj: TJSONObject; const Key: string; const Default: Boolean): Boolean;
begin
  if Obj.TryGetValue<Boolean>(Key, Result) then
    Exit(Result)
  else
    Result := Default;
end;

class function TJSONParser.GetFloat(const Obj: TJSONObject; const Key: string; const Default: Double): Double;
var
  FloatValue: string;
begin
  if Obj.TryGetValue<string>(Key, FloatValue) then
    Result := StrToFloatDef(FloatValue, Default)
  else
    Result := Default;
end;

class function TJSONParser.GetObject(const Obj: TJSONObject; const Key: string): TJSONObject;
begin
  if not Obj.TryGetValue<TJSONObject>(Key, Result) then
    Result := nil;
end;

class function TJSONParser.GetArray(const Obj: TJSONObject; const Key: string): TJSONArray;
begin
  if not Obj.TryGetValue<TJSONArray>(Key, Result) then
    Result := nil;
end;

class function TJSONParser.GetNestedValue(const Obj: TJSONObject; const KeyPath: string; const Default: string): string;
var
  Keys: TArray<string>;
  Key, ModifiedKey: string;
  CurrentObj: TJSONObject;
  CurrentArr: TJSONArray;
  Index: Integer;
begin
  Keys := KeyPath.Split(['.']);
  CurrentObj := Obj;

  for Key in Keys do
  begin
    ModifiedKey := Key;

    if ModifiedKey.Contains('[') then
    begin
      // Handle arrays like "card_faces[0]"
      Index := StrToIntDef(ModifiedKey.Substring(ModifiedKey.IndexOf('[') + 1, ModifiedKey.IndexOf(']') - ModifiedKey.IndexOf('[') - 1), -1);
      ModifiedKey := ModifiedKey.Substring(0, ModifiedKey.IndexOf('['));

      CurrentArr := GetArray(CurrentObj, ModifiedKey);
      if Assigned(CurrentArr) and (Index >= 0) and (Index < CurrentArr.Count) then
      begin
        if CurrentArr.Items[Index] is TJSONObject then
          CurrentObj := CurrentArr.Items[Index] as TJSONObject
        else
          Exit(Default);
      end
      else
        Exit(Default);
    end
    else
    begin
      CurrentObj := GetObject(CurrentObj, ModifiedKey);
      if not Assigned(CurrentObj) then
        Exit(Default);
    end;
  end;

  Result := CurrentObj.ToString;
end;

class function TJSONParser.KeyExists(const Obj: TJSONObject; const Key: string): Boolean;
begin
  Result := Obj.Values[Key] <> nil;
end;

class function TJSONParser.JSONArrayToStringArray(const JSONArray: TJSONArray): TArray<string>;
var
  I: Integer;
begin
  if not Assigned(JSONArray) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(Result, JSONArray.Count);
  for I := 0 to JSONArray.Count - 1 do
  begin
    if JSONArray.Items[I].Value <> '' then
      Result[I] := JSONArray.Items[I].Value
    else
      Result[I] := ''; // Handle empty or null elements
  end;
end;

end.
