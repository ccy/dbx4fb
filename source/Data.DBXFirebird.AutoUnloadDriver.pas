unit Data.DBXFirebird.AutoUnloadDriver;

interface

implementation

uses
  Data.DBXCommon, Data.DBXFirebird,
  DDetours;

var TDBXFirebirdProperties_Create: function(InstanceOrVMT: Pointer; Alloc: ShortInt; DBXContext: TDBXContext): Pointer = nil;

function TDBXFirebirdProperties_Create_Patch(InstanceOrVMT: Pointer; Alloc: ShortInt; DBXContext: TDBXContext): Pointer;
begin
  var o := TDBXFirebirdProperties_Create(InstanceOrVMT, Alloc, DBXContext);
  TDBXFirebirdProperties(o).Values[TDBXPropertyNames.AutoUnloadDriver] := 'true';
  Result := o;
end;

initialization
  TDBXFirebirdProperties_Create := InterceptCreate(@TDBXFirebirdProperties.Create, @TDBXFirebirdProperties_Create_Patch);
finalization
  InterceptRemove(@TDBXFirebirdProperties_Create);
end.
