program firebird.prototype;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Classes,
  Variants,
  IB_Header in '..\..\core\source\rtl\IB_Header.pas',
  firebird.client in '..\..\core\source\rtl\firebird.client.pas',
  firebird.client.debug in '..\..\core\source\rtl\firebird.client.debug.pas',
  firebird.dsql in '..\..\core\source\rtl\firebird.dsql.pas';

procedure CheckStatus(aStatus: IStatusVector; const aLibrary: IFirebirdClient);
var r: word;
begin
  if not aStatus.CheckResult(r, 65535) then
    OutputDebugString(PAnsiChar(aStatus.GetError(aLibrary).GetMessage));
end;

procedure test_isc_dsql_execute;
var hFB: THandle;
    lFB: IFirebirdClient;
var DPB, sServerName, sUserName, sPassword: AnsiString;
    hDB: isc_db_handle;
    Status: IStatusVector;
    hStmt: isc_stmt_handle;
var TPB: AnsiString;
    teb: isc_teb;
    hTR: isc_tr_handle;
var SQL: string;
    i_xsqlda: XSQLDA;
    iFetch: word;
    iCount: integer;
    o: TXSQLDA;
    S: string;
    iLen: word;
begin
  {$region 'Load Library'}
  hFB := LoadLibrary('C:\Project\Factory\System\Resource\bin\fbclient.1.5.3.dll');
  lFB := TFirebirdClientFactory.New(hFB);
  {$endregion}

  Status := TStatusVector.Create;

  {$region 'Attach'}
  sServerName := 'localhost:c:\My DB\Pay-0001.fdb';
  sUserName := 'SYSDBA';
  sPassword := 'masterkey';

  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(sUserName)) + sUserName +
         char(isc_dpb_password) + char(Length(sPassword)) + sPassword;

  hDB := 0;
  lFB.isc_attach_database(Status.pValue, Length(sServerName), PAnsiChar(sServerName), @hDB, Length(DPB), PAnsiChar(DPB));
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Allocate Statement'}
  hStmt := 0;
  lFB.isc_dsql_allocate_statement(Status.pValue, @hDB, @hStmt);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Start Transaction'}
  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  hTR := 0;
  teb.db_ptr := @hDB;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);

  lFB.isc_start_multiple(Status.pValue, @hTR, 1, @teb);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Prepare'}
  o := TXSQLDA.Create(lFB);

  SQL := 'SELECT 0, '''', '''', A.RDB$RELATION_NAME, A.RDB$FIELD_NAME, A.RDB$FIELD_POSITION, 0, B.RDB$FIELD_TYPE, '''', ' +
                'B.RDB$FIELD_SUB_TYPE, B.RDB$FIELD_LENGTH, 0, B.RDB$FIELD_SCALE, A.RDB$NULL_FLAG, B.RDB$DEFAULT_VALUE ' +
           'FROM RDB$RELATION_FIELDS A, RDB$FIELDS B ' +
          'WHERE (A.RDB$FIELD_SOURCE = B.RDB$FIELD_NAME) AND (A.RDB$RELATION_NAME = ''RDB$RELATIONS'') ' +
       'ORDER BY A.RDB$FIELD_POSITION';
  lFB.isc_dsql_prepare(Status.pValue, @hTR, @hStmt, Length(SQL), pAnsiChar(SQL), SQL_DIALECT_CURRENT, o.XSQLDA);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'describe bind'}
//  i_xsqlda.version := SQLDA_VERSION1;
//  i_xsqlda.sqln := 1;
//  lFB.isc_dsql_describe_bind(Status.pValue, @hStmt, SQL_DIALECT_CURRENT, @i_xsqlda);
//  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'describe'}
  if o.sqld > o.sqln then begin
    o.Count := o.sqld;
    lFB.isc_dsql_describe(Status.pValue, @hStmt, SQLDA_VERSION1, o.XSQLDA);
    CheckStatus(Status, lFB);
  end;
//  lFB.isc_dsql_describe(Status.pValue, @hStmt, SQL_DIALECT_CURRENT, nil);
//  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Execute'}
  lFB.isc_dsql_execute(Status.pValue, @hTR, @hStmt, o.Version, nil);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Fetch'}
  o.Prepare;

  iCount := 0;
  repeat
    iFetch := lFB.isc_dsql_fetch(Status.pValue, @hStmt, o.Version, o.XSQLDA);
    if iFetch = 0 then begin
      OutputDebugString(PChar(o[5].sqldata));
      Inc(iCount);
    end;
  until iFetch <> 0;
  if iFetch <> 100 then begin
    //EOF 
  end;
  CheckStatus(Status, lFB);

  o.Free;
  {$endregion}
  {$region 'Free Statement'}
  lFB.isc_dsql_free_statement(Status.pValue, @hStmt, DSQL_DROP);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Commit Transaction'}
  lFB.isc_commit_transaction(Status.pValue, @hTR);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Detach'}
  lFB.isc_detach_database(Status.pValue, @hDB);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'FreeLibrary'}
  lFB := nil;
  FreeLibrary(hFB);
  {$endregion}
end;

procedure test_isc_dsql_execute2;
var hFB: THandle;
    lFB: IFirebirdClient;
var DPB, sServerName, sUserName, sPassword: AnsiString;
    hDB: isc_db_handle;
    Status: IStatusVector;
    hStmt: isc_stmt_handle;
var TPB: AnsiString;
    teb: isc_teb;
    hTR: isc_tr_handle;
var SQL: string;
    i_xsqlda: XSQLDA;
    iFetch: word;
    iCount: integer;
    oSQLDA, iSQLDA: TXSQLDA;
    S: string;
    iLen: word;
    Ptr: pointer;
begin
  {$region 'Load Library'}
  hFB := LoadLibrary('C:\Project\Factory\System\Resource\bin\fbclient.1.5.3.dll');
  lFB := TFirebirdClientFactory.New(hFB);
  {$endregion}

  Status := TStatusVector.Create;

  {$region 'Attach'}
  sServerName := 'localhost:c:\my DB\Pay-0001.fdb';
  sUserName := 'SYSDBA';
  sPassword := 'masterkey';

  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(sUserName)) + sUserName +
         char(isc_dpb_password) + char(Length(sPassword)) + sPassword;

  hDB := 0;
  lFB.isc_attach_database(Status.pValue, Length(sServerName), PAnsiChar(sServerName), @hDB, Length(DPB), PAnsiChar(DPB));
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Allocate Statement'}
  hStmt := 0;
  lFB.isc_dsql_allocate_statement(Status.pValue, @hDB, @hStmt);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Start Transaction'}
  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  hTR := 0;
  teb.db_ptr := @hDB;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);

  lFB.isc_start_multiple(Status.pValue, @hTR, 1, @teb);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Prepare'}
  oSQLDA := TXSQLDA.Create(lFB, 1);

  SQL := 'SELECT '''' FROM RDB$RELATIONS WHERE RDB$FLAGS=?';
  lFB.isc_dsql_prepare(Status.pValue, @hTR, @hStmt, Length(SQL), pAnsiChar(SQL), SQL_DIALECT_CURRENT, oSQLDA.XSQLDA);
  CheckStatus(Status, lFB);
  if oSQLDA.sqld > oSQLDA.sqln then begin
    oSQLDA.Count := oSQLDA.sqld;
    lFB.isc_dsql_describe(Status.pValue, @hStmt, oSQLDA.Version, oSQLDA.XSQLDA);
    CheckStatus(Status, lFB);
  end;
  {$endregion}
  {$region 'describe bind'}
  iSQLDA := TXSQLDA.Create(lFB, 1);
  lFB.isc_dsql_describe_bind(Status.pValue, @hStmt, oSQLDA.Version, iSQLDA.XSQLDA);
  CheckStatus(Status, lFB);
  Assert(isqlDA.sqln = iSQLDA.sqld);
  isqlDA.Prepare;
  psmallint(iSQLDA.Vars[0].sqldata)^ := 1;
  {$endregion}
  {$region 'Execute'}
  lFB.isc_dsql_execute2(Status.pValue, @hTR, @hStmt, oSQLDA.Version, isqlDA.XSQLDA, nil);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Fetch'}
  oSQLDA.Prepare;

  iCount := 0;
  repeat
    iFetch := lFB.isc_dsql_fetch(Status.pValue, @hStmt, oSQLDA.Version, oSQLDA.XSQLDA);
    if iFetch = 0 then begin
      SetString(S, PChar(oSQLDA[8].sqldata), oSQLDA[8].sqllen);
      S := StringReplace(S, ' ', '.', [rfReplaceAll]);
      OutputDebugString(PChar(S));
      Inc(iCount);
    end;
  until iFetch <> 0;
  if iFetch <> 100 then begin
    //EOF 
  end;
  CheckStatus(Status, lFB);

  iSQLDA.Free;
  oSQLDA.Free;
  {$endregion}
  {$region 'Free Statement'}
  lFB.isc_dsql_free_statement(Status.pValue, @hStmt, DSQL_DROP);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Commit Transaction'}
  lFB.isc_commit_transaction(Status.pValue, @hTR);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Detach'}
  lFB.isc_detach_database(Status.pValue, @hDB);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'FreeLibrary'}
  lFB := nil;
  FreeLibrary(hFB);
  {$endregion}
end;

procedure test_isc_dsql_execute_writeblob;
var hBlob: isc_blob_handle;
    lBlobID: ISC_QUAD;
    CurPos, SegLen: LongInt;
    F: TFileStream;
    lBuffer: PChar;
    lSQL: string;
    o1, o2: TXSQLDA;
    lFB: IFirebirdClient;
    Status: IStatusVector;
    hFB: THandle;
    hDB: isc_db_handle;
    hTR: isc_tr_handle;
    hStmt: isc_stmt_handle;
    DPB, TPB, sServerName, sUserName, sPassword: AnsiString;
    teb: isc_teb;
    lData: pointer;
begin
  Status := TStatusVector.Create;
  {$region 'Load Library'}
  hFB := LoadLibrary('C:\Project\Factory\System\Resource\bin\fbclient.1.5.3.dll');
  lFB := TFirebirdClientFactory.New(hFB);
  {$endregion}
  {$region 'Attach'}
  sServerName := 'localhost:c:\My DB\Pay-0001.fdb';
  sUserName := 'SYSDBA';
  sPassword := 'masterkey';

  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(sUserName)) + sUserName +
         char(isc_dpb_password) + char(Length(sPassword)) + sPassword;

  hDB := 0;
  lFB.isc_attach_database(Status.pValue, Length(sServerName), PAnsiChar(sServerName), @hDB, Length(DPB), PAnsiChar(DPB));
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Allocate Statement'}
  hStmt := 0;
  lFB.isc_dsql_allocate_statement(Status.pValue, @hDB, @hStmt);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Start Transaction'}
  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  hTR := 0;
  teb.db_ptr := @hDB;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);

  lFB.isc_start_multiple(Status.pValue, @hTR, 1, @teb);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Prepare'}
  o1 := TXSQLDA.Create(lFB, 1);
  lSQL := 'UPDATE HR_EMP SET PHOTO = ? WHERE CODE=''A001''';
  lFB.isc_dsql_prepare(Status.pValue, @hTR, @hStmt, Length(lSQL), pAnsiChar(lSQL), SQL_DIALECT_CURRENT, nil);
  {$endregion}
  {$region 'Describe'}
  o2 := TXSQLDA.Create(lFB, 1);
  lFB.isc_dsql_describe_bind(Status.pValue, @hStmt, o2.Version, o2.XSQLDA);
  CheckStatus(Status, lFB);
  Assert(o2.sqln = o2.sqld);
  o2.Prepare;
  lData := o2.Vars[1].sqldata;
  Move(lBlobID, lData, SizeOf(@lBlobID));
  {$endregion}
  {$region 'Create Blob'}
  hBlob := 0;
  lBlobID.isc_quad_high := 0;
  lBlobID.isc_quad_low := 0;

  lFB.isc_create_blob2(Status.pValue, @hDB, @hTR, @hBlob, @lBlobID, 0, nil);
  {$endregion}
  {$region 'Write Blob'}
  F := TFileStream.Create('C:\dora.jpg', fmOpenRead);
  GetMem(lBuffer, F.Size);
  try
    F.Seek(0, soFromBeginning);
    F.ReadBuffer(lBuffer^, F.Size);
    CurPos := 0;
    SegLen := 16 * 1024;
    while (CurPos < F.Size) do begin
      if (CurPos + SegLen > F.Size) then
        SegLen := F.Size - CurPos;
      if lFB.isc_put_segment(Status.pValue, @hBlob, SegLen,
           PChar(@lBuffer[CurPos])) > 0 then
        raise Exception.Create('error');
      Inc(CurPos, SegLen);
    end;
  finally
    F.Free;
    FreeMem(lBuffer, F.Size);
  end;
  {$endregion}
  {$region 'Close Blob'}
  lFB.isc_close_blob(Status.pValue, @hBlob);
  {$endregion}
  {$region 'Execute Immediate'}
  lFB.isc_dsql_execute_immediate(Status.pValue, @hdb, @hTR, length(lsql), PAnsiChar(lSQL), 1, o2.XSQLDA);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Free Statement'}
  lFB.isc_dsql_free_statement(Status.pValue, @hStmt, DSQL_DROP);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Commit Transaction'}
  lFB.isc_commit_transaction(Status.pValue, @hTR);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Detach'}
  lFB.isc_detach_database(Status.pValue, @hDB);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'FreeLibrary'}
  lFB := nil;
  FreeLibrary(hFB);
  {$endregion}
  o1.Free;
  o2.Free;
end;

procedure test_isc_dsql_execute_readblob;
var hFB: THandle;
    lFB: IFirebirdClient;
var DPB, sServerName, sUserName, sPassword: AnsiString;
    hDB: isc_db_handle;
    Status: IStatusVector;
    hStmt: isc_stmt_handle;
var TPB: AnsiString;
    teb: isc_teb;
    hTR: isc_tr_handle;
var SQL: string;
    iFetch: word;
    iCount: integer;
    oSQLDA: TXSQLDA;
    S: string;
    iLen: word;
    iSegs, iSize, iMaxSize: integer;
    Ptr: pointer;
    hBlob: isc_blob_handle;
    lBlobSegment: PChar;
    lActualLen: integer;
    lBlobID: ISC_QUAD;
    lBlobStat: integer;
var CurPos: LongInt;
    BytesRead, SegLen: word;
    lSQLCode: LongInt;
    lBuffer: PAnsiChar;
    iBlobType: short;
    F: TFileStream;
begin
  {$region 'Load Library'}
  hFB := LoadLibrary('C:\Project\Factory\System\Resource\bin\fbclient.1.5.3.dll');
  lFB := TFirebirdClientFactory.New(hFB);
  {$endregion}

  Status := TStatusVector.Create;

  {$region 'Attach'}
  sServerName := 'localhost:c:\my DB\Pay-0001.fdb';
  sUserName := 'SYSDBA';
  sPassword := 'masterkey';

  DPB := char(isc_dpb_version1) +
         char(isc_dpb_user_name) + char(Length(sUserName)) + sUserName +
         char(isc_dpb_password) + char(Length(sPassword)) + sPassword;

  hDB := 0;
  lFB.isc_attach_database(Status.pValue, Length(sServerName), PAnsiChar(sServerName), @hDB, Length(DPB), PAnsiChar(DPB));
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Allocate Statement'}
  hStmt := 0;
  lFB.isc_dsql_allocate_statement(Status.pValue, @hDB, @hStmt);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Start Transaction'}
  tpb := char(isc_tpb_version3) + char(isc_tpb_write) + char(isc_tpb_read_committed) +
         char(isc_tpb_no_rec_version) + char(isc_tpb_nowait);

  hTR := 0;
  teb.db_ptr := @hDB;
  teb.tpb_len := Length(tpb);
  teb.tpb_ptr := PAnsiChar(tpb);

  lFB.isc_start_multiple(Status.pValue, @hTR, 1, @teb);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Prepare'}
  oSQLDA := TXSQLDA.Create(lFB, 1);
  SQL := 'SELECT Photo FROM HR_EMP WHERE CODE=''A001''';
  lFB.isc_dsql_prepare(Status.pValue, @hTR, @hStmt, Length(SQL), pAnsiChar(SQL), SQL_DIALECT_CURRENT, oSQLDA.XSQLDA);
  CheckStatus(Status, lFB);
  if oSQLDA.sqld > oSQLDA.sqln then begin
    oSQLDA.Count := oSQLDA.sqld;
    lFB.isc_dsql_describe(Status.pValue, @hStmt, oSQLDA.Version, oSQLDA.XSQLDA);
    CheckStatus(Status, lFB);
  end;
  {$endregion}
  {$region 'Execute'}
  lFB.isc_dsql_execute(Status.pValue, @hTR, @hStmt, oSQLDA.Version, nil);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Fetch'}
  oSQLDA.Prepare;
  oSQLDA.XSQLDA.sqlvar[0].sqldata := @lBlobID;
  oSQLDA.XSQLDA.sqlvar[0].sqltype := SQL_Blob + 1;
  oSQLDA.XSQLDA.sqlvar[0].sqllen := SizeOf(ISC_QUAD);

  lBlobSegment := GetMemory(1024);
  lBuffer := GetMemory(1024);
  F := TFileStream.Create('C:\car1.jpg', fmCreate);
  try
    hBlob := 0;
    repeat
      iFetch := lFB.isc_dsql_fetch(Status.pValue, @hStmt, 1, oSQLDA.XSQLDA);

      lFB.isc_open_blob2(Status.pValue, @hDB, @hTR, @hBlob, @lBlobID, 0, nil);
      lBlobStat := lFB.isc_get_segment(Status.pValue, @hBlob, PWord(lActualLen), SizeOf(lBlobSegment), lBlobSegment);
      while (lBlobStat = 0) or (Status.pValue[1] = isc_segment) do begin
        lBlobStat := lFB.isc_get_segment(Status.pValue, @hBlob, PWord(lActualLen), SizeOf(lBlobSegment), lBlobSegment);
        if lBlobStat = 0 then
          F.WriteBuffer(oSQLDA.XSQLDA.sqlvar[0].sqldata, lActualLen);
      end;
    until iFetch <> 0;
    if iFetch <> 100 then begin
      //EOF
    end;
  finally
    FreeMemory(lBlobSegment);
    FreeMemory(lBuffer);
    F.Free;
  end;

  {$endregion}
  {$region 'Close Blob'}
  lFB.isc_close_blob(Status.pValue, @hBlob);
  {$endregion}
  {$region 'Free Statement'}
  lFB.isc_dsql_free_statement(Status.pValue, @hStmt, DSQL_DROP);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Commit Transaction'}
  lFB.isc_commit_transaction(Status.pValue, @hTR);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'Detach'}
  lFB.isc_detach_database(Status.pValue, @hDB);
  CheckStatus(Status, lFB);
  {$endregion}
  {$region 'FreeLibrary'}
  lFB := nil;
  FreeLibrary(hFB);
  {$endregion}
  oSQLDA.Free;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  test_isc_dsql_execute_readblob;
end.
