UNIT EMScreen;
{$M 8192,0,0}
INTERFACE

PROCEDURE SaveScreenEMS;

PROCEDURE RestoreScreenEMS;

IMPLEMENTATION

USES DOS, crt, EMS;

TYPE
  TScreen= ARRAY [0..3999] OF BYTE;

VAR
  IsScreenSaved: BOOLEAN;
  Screen       : TScreen ABSOLUTE $B800:0;
  EMMHandle    : WORD;
  PageFrameAddr: WORD;

PROCEDURE AllocEMSPages (Pages: WORD);
BEGIN
  EMMHandle := AllocatePages (Pages);
  GetPageFrameAddr (PageFrameAddr);
END;

PROCEDURE SaveEMS (VAR Buf; Length: WORD);
VAR
  Count: WORD;
BEGIN
  FOR Count := 0 TO Length-1 DO
    Mem[PageFrameAddr:Count] := TScreen(Buf)[Count];
END;

PROCEDURE LoadEMS (VAR Buf; Length: WORD);
VAR
  Count: WORD;
BEGIN
  FOR Count := 0 TO Length-1 DO
    TScreen(Buf)[Count] := Mem[PageFrameAddr:Count];
END;

PROCEDURE FreeEMS (Handle: WORD);
BEGIN
  ReleasePage (EMMHandle);
END;

PROCEDURE SaveScreenEMS;
BEGIN
  AllocEMSPages (1);
  SaveEMS (Screen, 4000);
  IsScreenSaved := TRUE;
END;

PROCEDURE RestoreScreenEMS;
BEGIN
  IF IsScreenSaved THEN
  BEGIN
    LoadEMS (Screen, 4000);
    FreeEMS (EMMHandle);
    IsScreenSaved := FALSE;
  END;
END;

BEGIN
  IsScreenSaved := FALSE;
  IF (NOT TestEMMDriver) THEN RunError(0);
END.