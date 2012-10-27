UNIT CtlBreak;

INTERFACE

USES
  TSR,Dos,Crt;

IMPLEMENTATION

VAR
  OldInt09,
  OldExitProc: POINTER;

PROCEDURE NewInt09( Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP: WORD );
INTERRUPT;
  BEGIN
  IF CheckBreak AND ((Mem[$40:$17] AND 4) = 4) AND (Port[$60] = 70)
   THEN BEGIN
    ResetKbd;
    EndOfIntCmd;
    RunError(99);
  END;
  JmpRoutine( OldInt09 );
END;

PROCEDURE NewExitProc; FAR;
  BEGIN
    ExitProc := OldExitProc;
    DeInstall( $09, OldInt09 );
    ExitCode := 99;
  END;

BEGIN
  OldExitProc := ExitProc;
  ExitProc := @NewExitProc;
  Install( $09, @NewInt09, OldInt09 );
END.
