PROGRAM ScreenEMS;

(* bei Verwendung des EMS *)
USES Crt, EMScreen;
(* bei Verwendung des XMS *)
(* USES Crt, XMScreen;    *)

VAR
  i : INTEGER;

BEGIN
  ClrScr;
  WriteLn ('F�llen mit A''s');
  Delay (1000);
  FOR i := 0 TO 1999 DO
    Write('A');
  Delay (1000);
  SaveScreenEMS;                       (* bzw. SaveScreenXMS    *)

  WriteLn ('Und alles ist im EMS !');  (* oder ins XMS          *)
  WriteLn (' und �berschreiben...');
  Delay (1000);
  ClrScr;

  For i := 0 TO 1999 DO
    Write('.');
  Delay(1000);

  WriteLn ('und nun wieder zur�ck...');
  Delay (1000);
  ClrScr;
  RestoreScreenEMS;                    (* bzw. RestoreScreenXMS *)
  ReadLn;
  ClrScr;
END.