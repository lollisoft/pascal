UNIT TVCURSOR;


Interface


Procedure SaveCursor;
Procedure OrgCursor;
Procedure CursorEin;
Procedure CursorAus;



Implementation
uses Dos;


Var Regs: Registers;
    CS  : Word;


Procedure SaveCursor;
Begin
  Regs.AH := $03;
  Intr($10, Regs);
  CS := Regs.CX;
End;

Procedure OrgCursor;
Begin
  Regs.AH := $01;
  Regs.CX := CS;
  Intr($10, Regs)
End;

Procedure CursorEin;
Var R: Registers;
begin
  Inline($B4/$01/$B9/$07/$06/$CD/$10);
  R.AH := $01;
  R.CX := $0007;
  Intr($10, R)
end;

Procedure CursorAus;
begin
  Inline($B4/$01/$B9/$00/$0F/$CD/$10);
end;
End.