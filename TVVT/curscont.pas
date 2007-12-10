Unit CursCont;
{$O+,F+}


Interface
Uses Dos;

Procedure CursorEin;

Implementation

Procedure CursorEin;
Var R: Registers;
begin
  Inline($B4/$01/$B9/$07/$06/$CD/$10);
  R.AH := $01;
  R.CX := $0007;
  Intr($10, R)
end;
end.