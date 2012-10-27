PROGRAM ScreenXMS;

(* bei Verwendung des EMS *)
(* USES Crt, EMScreen;    *)
(* bei Verwendung des XMS *)
USES Crt, XMScreen;

VAR
  i : INTEGER;

BEGIN
  ClrScr;
  WriteLn ('FÅllen mit A''s');
  Delay (1000);
  FOR i := 0 TO 1999 DO
    Write('A');
  Delay (1000);
  SaveScreenXMS;

  WriteLn ('Und alles ist im XMS !');
  WriteLn (' und Åberschreiben...');
  Delay (1000);
  ClrScr;

  For i := 0 TO 1999 DO
    Write('.');
  Delay(1000);

  WriteLn ('und nun wieder zurÅck...');
  Delay (1000);
  ClrScr;
  RestoreScreenXMS;
  ReadLn;
  ClrScr;
END.