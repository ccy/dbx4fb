unit dbx4.base;

interface

uses SysUtils, FmtBcd, SqlTimSt, DBXCommon, DBXPlatform, DBXDynalink;

type
  IDBXBase = interface(IInterface)
  ['{671ED8A1-C1CC-46CC-AFD6-62DE69695235}']
    function Close: TDBXErrorCode;
    function GetErrorMessage(LastErrorCode: TDBXErrorCode; ErrorMessage:
        TDBXWideStringBuilder): TDBXErrorCode;
    function GetErrorMessageLength(LastErrorCode: TDBXErrorCode; out ErrorLen:
        TInt32): TDBXErrorCode;
  end;

  TDBXBase = class abstract(TInterfacedObject, IDBXBase)
  protected
    function Close: TDBXErrorCode; virtual; abstract;
    function GetErrorMessage(LastErrorCode: TDBXErrorCode; ErrorMessage:
        TDBXWideStringBuilder): TDBXErrorCode; virtual; abstract;
    function GetErrorMessageLength(LastErrorCode: TDBXErrorCode; out ErrorLen:
        TInt32): TDBXErrorCode; virtual; abstract;
  end;

  IDBXDriver = interface(IDBXBase)
  ['{9C225153-186B-4C1B-9820-A21263E13758}']
    function Loaded: boolean;
  end;

  IDBXConnection = interface(IDBXBase)
  ['{53ACFC78-E0E4-4C73-A259-1DBAE966C7AD}']
    function BeginTransaction(out TransactionHandle: TDBXTransactionHandle;
        IsolationLevel: TInt32): TDBXErrorCode;
    function Commit(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function Connect(Count: TInt32; Names, Values: TWideStringArray): TDBXErrorCode;
    function IsolationLevel: TInt32;
    function Rollback(TransactionHandle: TDBXTransactionHandle): TDBXErrorCode;
    function SetCallbackEvent(CallbackHandle: DBXCallbackHandle; CallbackEvent:
        DBXTraceCallback): TDBXErrorCode;
  end;

  IDBXRow = interface(IDBXBase)
  ['{567A6C55-EFBE-4D77-860C-AFA834174677}']
    function GetAnsiString(Ordinal: TInt32; Value: TDBXAnsiStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
    function GetBcd(Ordinal: TInt32; out Value: TBcd; out IsNull: LongBool):
        TDBXErrorCode;
    function GetBoolean(Ordinal: TInt32; out Value, IsNull: LongBool):
        TDBXErrorCode;
    function GetByteLength(Ordinal: TInt32; out Length: Int64; out IsNull:
        LongBool): TDBXErrorCode;
    function GetBytes(Ordinal: TInt32; Offset: Int64; Value: TBytes; const
        LastIndex: TInt32; ValueOffset, Length: Int64; out ReturnLength: Int64; out
        IsNull: LongBool): TDBXErrorCode;
    function GetDate(Ordinal: TInt32; out Value: TDBXDate; out IsNull: LongBool):
        TDBXErrorCode;
    function GetDouble(Ordinal: TInt32; out Value: Double; out IsNull: LongBool):
        TDBXErrorCode;
    function GetFixedBytes(Ordinal: TInt32; Value: TBytes; const LastIndex: TInt32;
        ValueOffset: TInt32; out IsNull: LongBool): TDBXErrorCode;
    function GetInt16(Ordinal: TInt32; out Value: SmallInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetInt32(Ordinal: TInt32; out Value: LongInt; out IsNull: LongBool):
        TDBXErrorCode;
    function GetInt64(Ordinal: TInt32; out Value: Int64; out IsNull: LongBool):
        TDBXErrorCode;
    function GetTime(Ordinal: TInt32; out Value: TDBXTime; out IsNull: LongBool):
        TDBXErrorCode;
    function GetTimeStamp(Ordinal: TInt32; out Value: TSQLTimeStamp; out IsNull:
        LongBool): TDBXErrorCode;
    function GetWideString(Ordinal: TInt32; Value: TDBXWideStringBuilder; out
        IsNull: LongBool): TDBXErrorCode;
  end;

  IDBXWritableRow = interface(IDBXBase)
  ['{1B637B12-12B0-42AC-97F0-906C26D72F7E}']
    function SetAnsiString(Ordinal: TInt32; const Value: TDBXAnsiString; Length:
        Int64): TDBXErrorCode;
    function SetBcd(Ordinal: TInt32; Value: TBcd): TDBXErrorCode;
    function SetBytes(Ordinal: TInt32; BlobOffset: Int64; Value: TBytes; LastIndex:
        TInt32; ValueOffset, Length: Int64): TDBXErrorCode;
    function SetDate(Ordinal: TInt32; Value: TDBXDate): TDBXErrorCode;
    function SetDouble(Ordinal: TInt32; Value: Double): TDBXErrorCode;
    function SetInt16(Ordinal: TInt32; Value: SmallInt): TDBXErrorCode;
    function SetInt32(Ordinal: TInt32; Value: LongInt): TDBXErrorCode;
    function SetInt64(Ordinal: TInt32; Value: Int64): TDBXErrorCode;
    function SetNull(Ordinal: TInt32): TDBXErrorCode;
    function SetParameterType(Ordinal: TInt32; const Name: TDBXWideString;
        ChildPosition: TInt32; ParamDirection: TDBXParameterDirection; DBXType,
        DBXSubType: TInt32; Size, Precision: Int64; Scale: TInt32): TDBXErrorCode;
    function SetTime(Ordinal: TInt32; Value: TDBXTime): TDBXErrorCode;
    function SetTimeStamp(Ordinal: TInt32; var Value: TSQLTimeStamp): TDBXErrorCode;
    function SetWideString(Ordinal: TInt32; const Value: TDBXWideString; Length:
        Int64): TDBXErrorCode;
  end;

  IDBXReader = interface(IDBXBase)
  ['{7507B8FB-353D-43AF-9D47-DD79E09B6A47}']
    function ColumnCount: TInt32;
    function GetColumnMetadata(Ordinal: TInt32; Name: TDBXWideStringBuilder; out
        ColumnType, ColumnSubType, Length, precision, scale, flags: TInt32):
        TDBXErrorCode;
    function Next: TDBXErrorCode;
  end;

  IDBXCommand = interface(IDBXBase)
  ['{D509CC08-86E0-459E-8C08-E5E1346C7590}']
    function CreateParameterRow(out aRow: TDBXRowHandle): TDBXErrorCode;
    function Execute(out Reader: IDBXReader): TDBXErrorCode;
    function ExecuteImmediate(const SQL: TDBXWideString; out aReader: IDBXReader):
        TDBXErrorCode;
    function GetRowsAffected(out Rows: Int64): TDBXErrorCode;
    function Prepare(const SQL: TDBXWideString; Count: TInt32): TDBXErrorCode;
  end;

  IMetaDataProvider = interface
    function GetColumnCount: TInt32;
    function GetColumnLength(const aColNo: TInt32): LongWord;
    function GetColumnName(const aColNo: TInt32): WideString;
    function GetColumnPrecision(const aColNo: TInt32): TInt32;
    function GetColumnScale(const aColNo: TInt32): TInt32;
    function GetColumnType(const aColNo: TInt32): TInt32;
    function GetColumnSubType(const aColNo: TInt32): TInt32;
    function GetIsNullable(const aColNo: TInt32): boolean;
  end;

implementation

end.
