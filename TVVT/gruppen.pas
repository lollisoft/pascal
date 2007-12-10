Program Gruppen;
{$M 65520,0,655360}
Uses Crt, Dirs;

Const Z : Integer = 0;


Var    Name       : String;
       Size       : Integer;
Procedure Traverse(Root: DateiListPtrtyp);
Begin
  If Root <> Nil then
  Begin
    Traverse(Root^.Links);
    Inc(Size);
    Write(Root^.DateiData.DateiName);
    Gotoxy(Wherex + 6 - Length(Root^.DateiData.DateiName),
           Wherey);
    Inc(Z);
    If Z = 13 then
    Begin
      Z := 0;
      Writeln
    End;

    Traverse(Root^.Rechts)
  End
End;

Var Param1,
    Param2,
    Param3 : String;

Begin
  Size := 0;
  Writeln('Dateigruppen - Finder 1.0 (c) 1993 Lothar Behrens');

  Dif := False;
  GruppenTree := Nil;

  If ParamCount > 2 then
  Begin
    Writeln('Zuviele Parameter!');
    Writeln('/A /a = Alle Verzeichnisse durchsuchen.');
    Halt
  End;

  Param1 := ParamStr(1);
  Param2 := ParamStr(2);
  If Param2[1] = '/' then
  Begin
    Param3 := Param1;
    Param1 := Param2;
    Param2 := Param3;
  End;


  If (Param1 = '/A') or
     (Param1 = '/a') then
    GanzePlatte := True Else GanzePlatte := False;

  TestMaskenSyntax(Param2);

  Maske := Param2;

  If Maske = '' then Maske := '*.*';

  Writeln('Scann:');
  Dir(Maske);
  Writeln('Anzeige:');

  Traverse(GruppenTree);
End.

