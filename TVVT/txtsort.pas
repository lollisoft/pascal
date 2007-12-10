Program TXTSort;
{$M 65520,0,655360}
Uses Crt, Video1;


Var SortTree: PfadListPtrtyp;
    SourceF,
    DestF   : Text;

    Name,
    Zeile   : String;


function FileExists(FileName: string): Boolean;
var
  f: file;
begin
  {$I-}
  Assign(f, FileName);
  Reset(f);
  Close(f);
  {$I+}
  FileExists := (IOResult = 0) and (FileName <> '');
end;  { FileExists }




Var Dif: Boolean;
    A  : Integer;
    F  : File of Byte;
Procedure Traverse(Root: PfadListPtrtyp);
Begin
  If Root <> Nil then
  Begin
    Traverse(Root^.Links);

    If Root^.PfadData.Anzahl > 1 then
      Writeln('Doppelde Zeilen vorhanden!');

    While Root^.PfadData.PfadName^[Length(Root^.PfadData.PfadName^)] = ' ' do
      System.Delete(Root^.PfadData.PfadName^,
                    Length(Root^.PfadData.PfadName^), 1);

    If Root^.Pfaddata.pfadname^ <> '' then
(*    For A:= 1 to Root^.PfadData.Anzahl do *)
      Writeln(DestF, Root^.PfadData.PfadName^);

    Traverse(Root^.Rechts)
  End
End;


Begin
  Writeln('TXTSort 1.0 (c) 1993 Lothar Behrens');
  Dif := False;
  SortTree := Nil;

  If ParamStr(2) <> '' then
  Begin
    
    Name := ParamStr(1);
    if FileExists(Name) then
    Begin
      Writeln('Datei wird eingelesen.');
      Assign(SourceF, Name);
      Assign(F, Name);
      Reset(F);
      If FileSize(F) > MemAvail then
      Begin
        Writeln('Datei zu groá, paát nicht in freien Speicher!');
        Close(F);
        Halt
      End;
      Close(F);


      Reset(SourceF);
      Repeat
        Readln(SourceF, Zeile);
        PfadName := Zeile;
        If PfadName <> '' then
          SpeicherePfad(SortTree, Dif);

      Until Eof(SourceF);

      Writeln('Datei wird geschrieben.');
      Readln;
      Assign(DestF, ParamStr(2));
      Rewrite(DestF);

      Traverse(SortTree);

      Close(DestF);
      Close(SourceF)

    End
    Else
    Begin
      Writeln('Datei nicht gefunden.')
    End
  End
  Else Writeln('Parameter falsch: Sourcefile Destfile')
End.

