Program DVSWITCH;

Uses Crt;

Var DvDatFile: Text;
    Suchtext : String;
    Zeile    : String;

Begin
  Assign(DvDatFile, 'DvDat.tmp');
  {$I-}
  Reset(Dvdatfile);
  {$I+}
  If IOResult = 0 then
  Begin
    If ParamCount < 1 then
    Begin
      Writeln('Programmaufruf mit: DvSwitch Suchtext');
      Halt
    End;
    Suchtext := ParamStr(1);
    Readln(DvDatFile, Zeile);
    If Pos(SuchText, Zeile) <> 0 then
    Begin

  End
  Else writeln('DvDat.Tmp ist nicht vorhanden!')
End.
