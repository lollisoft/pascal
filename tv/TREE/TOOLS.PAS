UNIT Tools;

INTERFACE

FUNCTION UpStr(S: String): String;

FUNCTION UpCase(C: Char): Char;

IMPLEMENTATION

FUNCTION UpStr(S: String): String;
VAR L: Integer;
BEGIN
  FOR L := 1 TO Length(S) DO S[L] := UpCase(S[L]);
  UpStr := S;
END;

FUNCTION UpCase(C: Char): Char;
BEGIN
  CASE C OF
    'Ñ': C := 'é';
    'î': C := 'ô';
    'Å': C := 'ö';
  ELSE C := System.UpCase(C);
  END;
  UpCase := C;
END;

END.