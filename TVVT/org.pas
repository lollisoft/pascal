Program cursor;
Uses Crt, Dos;
Var Regs: Registers;
Begin
  Inline($B4/$01/$B9/$07/$06/$CD/$10);
  Regs.AH := $01;
  Regs.CX := $0607;
  Intr($10, Regs)
End.