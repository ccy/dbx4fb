unit dbx.firebird.command30;

interface

uses IB_Header, DBXpress, firebird.client, firebird.dsql,
     dbx.common, dbx.firebird.cursor30;

type
  TMetaData_Firebird = class(TInterfacedObject, IMetaDataProvider)
  private
    FSQLDA: TXSQLDA;
  protected
    function GetColumnCount: integer;
    function GetColumnLength(const aColNo: Word): LongWord;
    function GetColumnName(const aColNo: Word): WideString;
    function GetColumnPrecision(const aColNo: Word): Smallint;
    function GetColumnScale(const aColNo: Word): Smallint;
    function GetColumnType(const aColNo: Word): Word;
    function GetColumnSubType(const aColNo: Word): Word;
    function IsNullable(const aColNo: Word): boolean;
  public
    constructor Create(const aSQLDA: TXSQLDA);
  end;

  TSQLCommand30_Firebird = class(TInterfacedObject, ISQLCommand30)
  strict private
    FDBXOptions: TDBXOptions;
    FClient: IFirebirdClient;
    FDBHandle: pisc_db_handle;
    FDSQL: IFirebird_DSQL;
  private
    FIsStoredProc: boolean;
    FStatusVector: IStatusVector;
    function StatusVector: IStatusVector;
  protected
    function SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer):
        SQLResult; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer;
        MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function setParameter(ulParameter: Word ; ulChildPos: Word ; eParamType:
        TSTMTParamType ; uLogType: Word; uSubType: Word; iPrecision: Integer;
        iScale: Integer; Length: LongWord ; pBuffer: Pointer; lInd: Integer):
        SQLResult; stdcall;
    function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
        Length: Integer; var IsBlank: Integer): SQLResult; stdcall;
    function prepare(SQL: PWideChar; ParamCount: Word): SQLResult; stdcall;
    function execute(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult;
        stdcall;
    function getNextCursor(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function getRowsAffected(var Rows: LongWord): SQLResult; stdcall;
    function close: SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
  public
    constructor Create(const aClientLibrary: IFirebirdClient; const aDBHandle:
        pisc_db_handle; const aDBXOptions: TDBXOptions);
  end;

implementation

uses Windows, SysUtils, FMTBcd;

constructor TSQLCommand30_Firebird.Create(const aClientLibrary:
    IFirebirdClient; const aDBHandle: pisc_db_handle; const aDBXOptions:
    TDBXOptions);
begin
  inherited Create;
  FClient := aClientLibrary;
  FDBHandle := aDBHandle;
  FDBXOptions := aDBXOptions;
end;

function TSQLCommand30_Firebird.close: SQLResult;
begin
  if Assigned(FDSQL) then begin
    FDSQL.Close(StatusVector);
    StatusVector.CheckResult(Result, DBXERR_SQLERROR);
  end else
    Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.execute(var Cursor: ISQLCursor30): SQLResult;
var M: IMetaDataProvider;
begin
  Assert(Assigned(FDSQL));

  FDSQL.Execute(StatusVector);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  M := TMetaData_Firebird.Create(FDSQL.o_SQLDA);
  Cursor := TSQLCursor30_Firebird.Create(FClient, M, FDSQL, FDBXOptions.TrimChar);
  Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.executeImmediate(SQL: PWideChar; var Cursor:
    ISQLCursor30): SQLResult;
var tr: isc_tr_handle;
    tpb: AnsiString;
    teb: isc_teb;
begin
  Cursor := nil;

  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  tr := 0;
  teb.db_ptr := FDBHandle;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);
  FClient.isc_start_multiple(StatusVector.pValue, @tr, 1, @teb);
  if not StatusVector.CheckResult(Result, DBXERR_CONNECTIONFAILED) then Exit;

  FClient.isc_dsql_execute_immediate(StatusVector.pValue, FDBHandle, @tr, 0, PAnsiChar(AnsiString(SQL)), FDBXOptions.SQLDialect, nil);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then begin
    FClient.isc_rollback_transaction(StatusVector.pValue, @tr);
    StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
  end else begin
    FClient.isc_commit_transaction(StatusVector.pValue, @tr);
    StatusVector.CheckResult(Result, DBXERR_NOTIMPLEMENT);
  end;
end;

function TSQLCommand30_Firebird.getErrorMessage(Error: PWideChar): SQLResult;
begin
  StatusVector.GetLastError.GetMessage(Error);
  Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.getErrorMessageLen(out ErrorLen: SmallInt):
    SQLResult;
begin
  ErrorLen := StatusVector.GetError(FClient).GetLength;
  Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.getNextCursor(var Cursor: ISQLCursor30):
    SQLResult;
begin
  Assert(False);
end;

function TSQLCommand30_Firebird.GetOption(eSqlCommandOption: TSQLCommandOption;
    PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin
  case eSqlCommandOption of
    eCommRowsetSize: Assert(False);
    eCommBlobSize: Assert(False);
    eCommBlockRead: Assert(False);
    eCommBlockWrite: Assert(False);
    eCommParamCount: Assert(False);
    eCommNativeHandle: Assert(False);
    eCommCursorName: Assert(False);
    eCommStoredProc: Assert(False);
    eCommSQLDialect: Assert(False);
    eCommTransactionID: Assert(False);
    eCommPackageName: Assert(False);
    eCommTrimChar: Assert(False);
    eCommQualifiedName: Assert(False);
    eCommCatalogName: Assert(False);
    eCommSchemaName: Assert(False);
    eCommObjectName: Assert(False);
    eCommQuotedObjectName: Assert(False);
    eCommPrepareSQL: Assert(False);
    eCommDecimalSeparator: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.getParameter(ParameterNumber: Word; ulChildPos:
    Word; Value: Pointer; Length: Integer; var IsBlank: Integer): SQLResult;
begin
  Assert(False);
end;

function TSQLCommand30_Firebird.getRowsAffected(var Rows: LongWord): SQLResult;
var info_request: char;
begin
  if Assigned(FDSQL) then begin
    FDSQL.GetRowsAffected(StatusVector, Rows);
    if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;
  end else begin
    Rows := 0;
    Result := DBXERR_NONE;
  end;
end;

function TSQLCommand30_Firebird.StatusVector: IStatusVector;
begin
  if FStatusVector = nil then
    FStatusVector := TStatusVector.Create;
  Result := FStatusVector;
end;

function TSQLCommand30_Firebird.prepare(SQL: PWideChar; ParamCount: Word):
    SQLResult;
begin
  FDSQL := TFirebird_DSQL.Create(FClient);

  FDSQL.Open(StatusVector, FDBHandle);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;

  FDSQL.Prepare(StatusVector, SQL, FDBXOptions.SQLDialect, ParamCount);
  if not StatusVector.CheckResult(Result, DBXERR_SQLERROR) then Exit;
end;

function TSQLCommand30_Firebird.SetOption(eSqlCommandOption: TSQLCommandOption;
    ulValue: Integer): SQLResult;
begin
  case eSqlCommandOption of
    eCommRowsetSize: ; {$Message 'Do not sure what to do here'}
    eCommBlobSize: Assert(False);
    eCommBlockRead: Assert(False);
    eCommBlockWrite: Assert(False);
    eCommParamCount: Assert(False);
    eCommNativeHandle: Assert(False);
    eCommCursorName: Assert(False);
    eCommStoredProc: FIsStoredProc := boolean(ulValue);
    eCommSQLDialect: Assert(False);
    eCommTransactionID: Assert(False);
    eCommPackageName: Assert(False);
    eCommTrimChar: Assert(False);
    eCommQualifiedName: Assert(False);
    eCommCatalogName: Assert(False);
    eCommSchemaName: Assert(False);
    eCommObjectName: Assert(False);
    eCommQuotedObjectName: Assert(False);
    eCommPrepareSQL: Assert(False);
    eCommDecimalSeparator: Assert(False);
  end;
  Result := DBXERR_NONE;
end;

function TSQLCommand30_Firebird.setParameter(ulParameter: Word ; ulChildPos: Word
    ; eParamType: TSTMTParamType ; uLogType: Word; uSubType: Word; iPrecision:
    Integer; iScale: Integer; Length: LongWord ; pBuffer: Pointer; lInd:
    Integer): SQLResult;
var bIsNull: boolean;
begin
  Result := DBXERR_NONE;

  bIsNull := lInd = 1;
  case uLogType of
    fldZSTRING:    FDSQL.i_SQLDA[ulParameter].SetString(pBuffer, iPrecision, bIsNull);
    fldDATE:       FDSQL.i_SQLDA[ulParameter].SetDate(pBuffer, bIsNull);
    fldBLOB:       Assert(False);
    fldBOOL:       Assert(False);
    fldINT16:      FDSQL.i_SQLDA[ulParameter].SetShort(pBuffer, bIsNull);
    fldINT32:      FDSQL.i_SQLDA[ulParameter].SetInteger(pBuffer, bIsNull);
    fldFLOAT:      FDSQL.i_SQLDA[ulParameter].SetDouble(pBuffer, Length, bIsNull);
    fldBCD:        FDSQL.i_SQLDA[ulParameter].SetBCD(pBuffer, iScale, bIsNull);
    fldBYTES:      Assert(False);
    fldTIME:       FDSQL.i_SQLDA[ulParameter].SetTime(pBuffer, bIsNull);
    fldTIMESTAMP:  Assert(False);
    fldUINT16:     Assert(False);
    fldUINT32:     Assert(False);
    fldFLOATIEEE:  Assert(False);
    fldVARBYTES:   Assert(False);
    fldLOCKINFO:   Assert(False);
    fldCURSOR:     Assert(False);
    fldINT64:      Assert(False);
    fldUINT64:     Assert(False);
    fldADT:        Assert(False);
    fldARRAY:      Assert(False);
    fldREF:        Assert(False);
    fldTABLE:      Assert(False);
    fldDATETIME:   Assert(False);
    fldFMTBCD:     Assert(False);
    fldWIDESTRING: Assert(False);
    else
      Result := DBXERR_INVALIDPARAM;
  end;
end;

constructor TMetaData_Firebird.Create(const aSQLDA: TXSQLDA);
begin
  inherited Create;
  FSQLDA := aSQLDA;
end;

function TMetaData_Firebird.GetColumnCount: integer;
begin
  Result := FSQLDA.Count;
end;

function TMetaData_Firebird.GetColumnLength(const aColNo: Word): LongWord;
var V: TXSQLVAR;
begin
  V := FSQLDA.Vars[aColNo];
  if V.CheckType(SQL_INT64) then
    Result := SizeOf(TBcd)
  else
    Result := V.Size;
end;

function TMetaData_Firebird.GetColumnName(const aColNo: Word): WideString;
var S: string;
    P: TIB_Identifier;
begin
  P := FSQLDA.Vars[aColNo].sqlname;
  SetString(S, P, FSQLDA.Vars[aColNo].sqlname_length);
  Result := S;
end;

function TMetaData_Firebird.GetColumnPrecision(
  const aColNo: Word): Smallint;
var V: TXSQLVAR;
begin
  V := FSQLDA.Vars[aColNo];
  if V.CheckType(SQL_INT64) then begin
    Result := 19;
  end else
    Result := v.sqllen;
end;

function TMetaData_Firebird.GetColumnScale(const aColNo: Word): Smallint;
begin
  Result := FSQLDA.Vars[aColNo].sqlscale;
end;

function TMetaData_Firebird.GetColumnSubType(const aColNo: Word): Word;
begin
  Result := FSQLDA.Vars[aColNo].sqlsubtype;
end;

function TMetaData_Firebird.GetColumnType(const aColNo: Word): Word;
var iType: Smallint;
    iScale: Smallint;
begin
  iType := FSQLDA.Vars[aColNo].sqltype and not 1;
  iScale := FSQLDA.Vars[aColNo].sqlscale;
  case iType of
    SQL_SHORT: begin
      Assert(iScale = 0);
      Result := fldINT16;
    end;
    SQL_TEXT: Result := fldZSTRING;
    SQL_VARYING: Result := fldZSTRING;
    SQL_LONG: begin
      Assert(iScale = 0);
      Result := fldINT32;
    end;
    SQL_BLOB: Result := fldBLOB;
    SQL_INT64: Result := fldFMTBCD;
    SQL_FLOAT: Result := fldFLOAT;
    SQL_DOUBLE: Result := fldFLOAT;
    SQL_TYPE_DATE: Result := fldDATE;
    SQL_TYPE_TIME: Result := fldTIME;
    else
      raise Exception.CreateFmt('Unsupported data type: %d', [iType]);
  end;
end;

function TMetaData_Firebird.IsNullable(const aColNo: Word): boolean;
begin
  Result := FSQLDA.Vars[aColNo].IsNullable;
end;

end.
