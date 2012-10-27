UNIT CtrlAltDel;

INTERFACE

USES
  Dos, TSR;

PROCEDURE CtrlAltDelOff;

PROCEDURE CtrlAltDelOn;

IMPLEMENTATION

VAR
  OrgInt09: POINTER;
  OldExit: POINTER;
  Kbd_Flag: BYTE ABSOLUTE $0040:$0017;
  CtrlAltDelActive: BOOLEAN;

PROCEDURE SpecialInt09; INTERRUPT;
  BEGIN
    IF (Kbd_Flag AND $0C = $0C) AND (ScanCode = $53)
     THEN BEGIN
      ResetKbd;
      EndOfIntCmd;
      RunError(98);
     END
     ELSE JmpRoutine(OrgInt09);
  END;

PROCEDURE CtrlAltDelOff;
  BEGIN
    IF CtrlAltDelActive THEN BEGIN
      Install( $09, @SpecialInt09, OrgInt09 );
      CtrlAltDelActive := False;
    END;
  END;

PROCEDURE CtrlAltDelOn;
  BEGIN
    IF NOT CtrlAltDelActive THEN BEGIN
      DeInstall( $09, OrgInt09 );
      CtrlAltDelActive := True;
    END;
  END;

PROCEDURE LocalExitProc; FAR;
  BEGIN
    CtrlAltDelOn;
    ExitProc := OldExit;
  END;

BEGIN
  OldExit  := ExitProc;
  ExitProc := @LocalExitProc;
  CtrlAltDelActive := True;
  CtrlAltDelOff;
END.
