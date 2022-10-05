unit Data.DBXFirebirdMetaDataReader.RSP37065;

interface

implementation

uses
  Data.DBXCommon, Data.DBXFirebirdMetaDataReader, Data.DBXMetaDataReader,
  Data.DBXPlatform,
  DDetours,
  Data.DBXFirebirdMetaDataReader.RSP37064;

var TDBXFirebirdTypeFilterCursor_GetIBType: function(Self: TDBXFirebirdTypeFilterCursor; const FieldType: SmallInt; const FieldSubType: SmallInt; const FieldScale: SmallInt): Integer = nil;
var TDBXFirebirdCustomMetaDataReader_GetAllDataTypes: function(Self: TDBXFirebirdCustomMetaDataReader): TDBXDataTypeDescriptionArray = nil;

type
  TDBXFirebirdTypeFilterCursorHelper = class helper for TDBXFirebirdTypeFilterCursor
    class function GetIBType_Address: Pointer;
  end;

  TDBXFirebirdCustomMetaDataReaderHelper = class helper for TDBXFirebirdCustomMetaDataReader
    const BooleanType = 13;
    const WideCharType = 14;
    const WideVarcharType = 15;
    const TimestampOffsetType = 16;
    function Get_FAlltypes: Pointer;
    class function GetAllDataTypes_Address: Pointer;
    function GetAllDataTypes_RSP37065: TDBXDataTypeDescriptionArray;
  end;

class function TDBXFirebirdTypeFilterCursorHelper.GetIBType_Address: Pointer;
begin
  Result := @TDBXFirebirdTypeFilterCursor.GetIBType;
end;

function GetIBType_RSP37065(Self: TDBXFirebirdTypeFilterCursor; const
    FieldType: SmallInt; const FieldSubType: SmallInt; const FieldScale:
    SmallInt): Integer;
begin
  Result := TDBXFirebirdTypeFilterCursor_GetIBType(Self, FieldType, FieldSubType, FieldScale);

  (* patch: support wide char type *)
  if (Result = TDBXFirebirdCustomMetaDataReader.CharType) and ComputeUnicode_RSP37064(Self) then
    Result := TDBXFirebirdCustomMetaDataReader.WideCharType;

  (* patch: support wide varchar type *)
  if (Result = TDBXFirebirdCustomMetaDataReader.VarcharType) and ComputeUnicode_RSP37064(Self) then
    Result := TDBXFirebirdCustomMetaDataReader.WideVarcharType;

  (* patch: support boolean type *)
  const blr_bool = 23;
  if (Result = -1) and (FieldType = blr_bool) then
    Result := TDBXFirebirdCustomMetaDataReader.BooleanType;

  (* patch: support timestamp offset type *)
  const blr_timestamp_tz = 29;
  if (Result = -1) and (FieldType = blr_timestamp_tz) then
    Result := TDBXFirebirdCustomMetaDataReader.TimestampOffsetType;
end;

class function TDBXFirebirdCustomMetaDataReaderHelper.GetAllDataTypes_Address: Pointer;
begin
  Result := @TDBXFirebirdCustomMetaDataReader.GetAllDataTypes;
end;

function TDBXFirebirdCustomMetaDataReaderHelper.Get_FAlltypes: Pointer;
asm
  {$ifdef Win32}lea eax, Self.FAlltypes{$ifend}
  {$ifdef Win64}lea rax, Self.FAlltypes{$ifend}
  ;
end;

function TDBXFirebirdCustomMetaDataReaderHelper.GetAllDataTypes_RSP37065: TDBXDataTypeDescriptionArray;
var Newtypes: TDBXDataTypeDescriptionArray;
    (* patch: StringType: Integer; *)
    A: ^TDBXDataTypeDescriptionArray;
begin
  A := Get_FAlltypes;
  if A^ = nil then
  begin
    SetLength(Newtypes, 17(* patch *));
    (* patch
    if FDefaultCharSetIsUnicode then
      StringType := TDBXDataTypes.WideStringType;
    *)
    Newtypes[TDBXFirebirdCustomMetaDataReader.CharType] := TDBXDataTypeDescription.Create('CHAR', TDBXDataTypes.AnsiStringType(* patch *), 32768, 'CHAR({0})', 'Precision', -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.SearchableWithLike or TDBXTypeFlag.Unsigned or TDBXTypeFlag.UnicodeOption);
    Newtypes[TDBXFirebirdCustomMetaDataReader.VarcharType] := TDBXDataTypeDescription.Create('VARCHAR', TDBXDataTypes.AnsiStringType(* patch *), 32678, 'VARCHAR({0})', 'Precision', -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.SearchableWithLike or TDBXTypeFlag.Unsigned or TDBXTypeFlag.UnicodeOption);
    Newtypes[TDBXFirebirdCustomMetaDataReader.IntegerType] := TDBXDataTypeDescription.Create('INTEGER', TDBXDataTypes.Int32Type, 10, 'INTEGER', NullString, -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.FixedPrecisionScale or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.SmallintType] := TDBXDataTypeDescription.Create('SMALLINT', TDBXDataTypes.Int16Type, 5, 'SMALLINT', NullString, -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.FixedPrecisionScale or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.FloatType] := TDBXDataTypeDescription.Create('FLOAT', TDBXDataTypes.SingleType, 53, 'FLOAT({0})', 'Precision', -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.DoubleType] := TDBXDataTypeDescription.Create('DOUBLE PRECISION', TDBXDataTypes.DoubleType, 53, 'DOUBLE PRECISION', NullString, -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.NumericType] := TDBXDataTypeDescription.Create('NUMERIC', TDBXDataTypes.BcdType, 18, 'NUMERIC({0}, {1})', 'Precision, Scale', 18, 0, NullString, NullString, NullString, NullString, TDBXTypeFlag.FixedLength or TDBXTypeFlag.FixedPrecisionScale or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.DecimalType] := TDBXDataTypeDescription.Create('DECIMAL', TDBXDataTypes.BcdType, 18, 'DECIMAL({0}, {1})', 'Precision, Scale', 18, 0, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.FixedPrecisionScale or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.Unsigned);
    Newtypes[TDBXFirebirdCustomMetaDataReader.DateType] := TDBXDataTypeDescription.Create('DATE', TDBXDataTypes.DateType, 0, 'DATE', NullString, -1, -1, '''', '''', NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.TimeType] := TDBXDataTypeDescription.Create('TIME', TDBXDataTypes.TimeType, 0, 'TIME', NullString, -1, -1, '''', '''', NullString, NullString, TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.TimestampType] := TDBXDataTypeDescription.Create('TIMESTAMP', TDBXDataTypes.TimeStampType, 0, 'TIMESTAMP', NullString, -1, -1, '''', '''', NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    Newtypes[TDBXFirebirdCustomMetaDataReader.BlobType] := TDBXDataTypeDescription.Create('BLOB', TDBXDataTypes.BlobType, 2147483647, 'BLOB', 'Precision', -1, -1, '''', '''', NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.Long or TDBXTypeFlag.Nullable or TDBXTypeFlag.Unsigned or TDBXTypeFlag.StringOption or TDBXTypeFlag.UnicodeOption);
    Newtypes[TDBXFirebirdCustomMetaDataReader.BigintType] := TDBXDataTypeDescription.Create('BIGINT', TDBXDataTypes.Int64Type, 19, 'BIGINT', NullString, -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch + TDBXTypeFlag.FixedLength or TDBXTypeFlag.FixedPrecisionScale or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    (* patch *)
    Newtypes[TDBXFirebirdCustomMetaDataReader.BooleanType] := TDBXDataTypeDescription.Create('BOOLEAN', TDBXDataTypes.BooleanType, 1, 'BOOLEAN', NullString, -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.Unsigned);
    Newtypes[TDBXFirebirdCustomMetaDataReader.WideCharType] := TDBXDataTypeDescription.Create('CHAR', TDBXDataTypes.WideStringType, 32768, 'CHAR({0})', 'Precision', -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.SearchableWithLike or TDBXTypeFlag.Unsigned or TDBXTypeFlag.UnicodeOption);
    Newtypes[TDBXFirebirdCustomMetaDataReader.WideVarcharType] := TDBXDataTypeDescription.Create('VARCHAR', TDBXDataTypes.WideStringType, 32678, 'VARCHAR({0})', 'Precision', -1, -1, NullString, NullString, NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable or TDBXTypeFlag.SearchableWithLike or TDBXTypeFlag.Unsigned or TDBXTypeFlag.UnicodeOption);
    Newtypes[TDBXFirebirdCustomMetaDataReader.TimestampOffsetType] := TDBXDataTypeDescription.Create('TIMESTAMP WITH TIME ZONE', TDBXDataTypes.TimeStampOffsetType, 0, 'TIMESTAMP WITH TIME ZONE', NullString, -1, -1, '''', '''', NullString, NullString, TDBXTypeFlag.BestMatch or TDBXTypeFlag.FixedLength or TDBXTypeFlag.Nullable or TDBXTypeFlag.Searchable);
    (* patch *)
    A^ := Newtypes;
  end;
  Result := A^;
end;

initialization
  TDBXFirebirdTypeFilterCursor_GetIBType := InterceptCreate(TDBXFirebirdTypeFilterCursor.GetIBType_Address, @GetIBType_RSP37065);
  TDBXFirebirdCustomMetaDataReader_GetAllDataTypes := InterceptCreate(TDBXFirebirdCustomMetaDataReader.GetAllDataTypes_Address, @TDBXFirebirdCustomMetaDataReader.GetAllDataTypes_RSP37065);
finalization
  InterceptRemove(@TDBXFirebirdTypeFilterCursor_GetIBType);
  InterceptRemove(@TDBXFirebirdCustomMetaDataReader_GetAllDataTypes);
end.
