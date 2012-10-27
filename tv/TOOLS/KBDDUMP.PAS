UNIT KBDDump;

INTERFACE

USES StrTools, StackDmp;

IMPLEMENTATION

VAR OldDumpMore: TDumpProc;

PROCEDURE NewDumpMore (VAR F: TEXT); FAR;
  VAR P: POINTER;
  BEGIN
    Writeln(F, 'KBDHead: ',ToHex(MemW[$40:$1A],4));
    Write(F, 'KBDTail: ',ToHex(MemW[$40:$1C],4));
    P := Ptr($40,$1E);
    Dump(F, P^, 32);
    Flush(F);
    OldDumpMore(F);
  END;

BEGIN
  OldDumpMore := DumpMore;
  DumpMore := NewDumpMore;
END.