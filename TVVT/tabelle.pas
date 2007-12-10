program tabelle;
uses crt;
Var x,y,m,n : Integer;
    tab     : Array [0..3,0..3] of Integer;

Begin
  x := 10;
  y := 5;
  clrscr;
  For n := 0 to 3 do
    For m := 0 to 3 do
    Begin
      Tab[n][m] := n + m;
      Gotoxy(x+m, y+n);
      Write(Tab[n][m])
    End;
    Readln
End.