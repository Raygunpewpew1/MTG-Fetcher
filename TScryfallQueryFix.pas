unit TScryfallQueryFix;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  ScryfallQueryBuilder;

procedure ApplyQueryFixes;

implementation

procedure FixCloneImplementation;
begin
  TScryfallQuery.Clone :=
    function(Self: TScryfallQuery): TScryfallQuery
    var
      NewQuery: TScryfallQuery;
      Filter: TScryfallFilter;
    begin
      NewQuery := TScryfallQuery.Create;
      try
        if Assigned(Self.FFilters) then
        begin
          NewQuery.FFilters := TList<TScryfallFilter>.Create;
          for Filter in Self.FFilters do
            NewQuery.FFilters.Add(Filter.Clone);
        end;

        NewQuery.FOptions := Self.FOptions; // Copy options

        LogStuff(Format(
          'Clone - Original: %p, New: %p, Options: IncludeExtras: %s, Sort: %s, Direction: %s',
          [Pointer(Self), Pointer(NewQuery),
          BoolToStr(NewQuery.FOptions.IncludeExtras, True),
          NewQuery.FOptions.Sort, NewQuery.FOptions.Direction]), DEBUG);

        Result := NewQuery;
      except
        NewQuery.Free;
        raise;
      end;
    end;
end;

procedure FixPaginationLogic;
begin
  TScryfallQuery.SetPage :=
    function(Self: TScryfallQuery; PageNum: Integer): TScryfallQuery
    begin
      if not Assigned(Self.FFilters) then
        Self.FFilters := TList<TScryfallFilter>.Create;

      Self.FOptions.Page := Max(1, PageNum);
      LogStuff(Format('SetPage - Page: %d, Options: %s',
        [Self.FOptions.Page, Self.FOptions.ToString]), DEBUG);

      Result := Self;
    end;
end;

procedure AddValidationGuards;
begin
  TScryfallQuery.ValidateFilters :=
    function(Self: TScryfallQuery): Boolean
    begin
      if not Assigned(Self.FFilters) then
      begin
        LogStuff('ValidateFilters: FFilters is nil.', ERROR);
        Exit(False);
      end;

      for var Filter in Self.FFilters do
        if (Filter.Values = nil) or (Length(Filter.Values) = 0) then
        begin
          LogStuff('ValidateFilters: Filter has empty or nil Values.', ERROR);
          Exit(False);
        end;

      Result := True;
    end;
end;

procedure ApplyQueryFixes;
begin
  FixCloneImplementation;
  FixPaginationLogic;
  AddValidationGuards;
end;

end.
