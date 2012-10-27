UNIT StreamEr;

INTERFACE

USES NameReg, Objects, StackDmp;

IMPLEMENTATION

{**** Date, Time and Version stamp. (c) 1990 by EMS. For Pascal! ****}
CONST Version =       1;    { 13.11.1990  8:24 }

VAR Status, ErrorInfo: INTEGER;
    OldDumpMore: TDumpProc;

PROCEDURE MyDumpProc (VAR F: TEXT); FAR;
  BEGIN
    Writeln(F, 'Unregistered Vision Object: ', GetVmtName(ErrorInfo)^);
    Flush(F);
    OldDumpMore(F);
  END;

PROCEDURE CloseTheShop (VAR S: TStream); FAR;
  BEGIN
    CASE S.Status OF
      -5,-6: BEGIN
        OldDumpMore := DumpMore;
        DumpMore := MyDumpProc;
        ErrorInfo := S.ErrorInfo;
        Status := S.Status;
        Halt(99);
      END;
    END;
  END;

BEGIN
  StreamError := @CloseTheShop;
END.