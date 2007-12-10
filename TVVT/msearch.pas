Program Text_search(Input,Output);
{***********************************************************

  File       : MSearch.PAS
  Vers.      : 1.0
  Last Edit  : 20.01.94
  Autor      : L. Behrens
  File I/O   : INPUT, OUTPUT, File
  Progr. Spr.: Turbo Pascal 4.0 / 5.0
  Betr. Sys. : DOS 2.1 - 3.3
  Funktion: Das Programm untersucht beliebige Dateien auf
            ASCII Zeichenvolgen und schreibt in Texts.Dat
            Pfad und Dateinamen bei vorhandensein.

*************************************************************}
USES Dos, Crt;
{$M 65520,8192,655360}

{#########################################################}
{#                     Hauptprogramm                     #}
{#########################################################}

Const
      MaxLen = 16384;
      lang  : Word = 0;               (* ZeilenlÑnge          *)
      OutTextDat: String = '\MSearch.dat';
      Muster: String  = '';

Type  Filetype = File;                (* Textfile             *)
      Fname    = string;
      TextFiletyp = System.Text;

Var   Next  : Array [0..SizeOf(String)] of Word;
      pfad,
      befehl,
      extender,
      lab              : String;
      DirString: String;
      attr             : Byte;
      A                : Integer;

      Filename : Fname;                     {* Dateiname          *}
      Device   : Filetype;                  {* Handle             *}
      Buffer   : array [0..MaxLen] of Byte; {* Lesepuffer         *}
      TextFile : TextFiletyp;

Function PasstinMaske(Maske, Name: String):Boolean;
var Extender,
    Extender1,
    Befehl,
    Befehl1,
    Pfad,
    Pfad1    : String;
    Befehlistgleich,
    Extenderistgleich : Boolean;

Procedure BehandleBefehl;
BEGIN
  (* Ist nur ein Stern vorhanden? *)

  If Befehl1[1] = '*' then Befehlistgleich := True

  else

  (* Ist ein Stern vorhanden? *)

  If (Pos('*', Befehl1) <> 0) and

     (* Wenn der gegebene Dateiname nicht kleiner ist, als  *)
     (* die Maske ohne Stern, dann kann die Maske passen:   *)

     (Length(Befehl) >= Length(Befehl1) - 1) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen *)
       (* und nach Stern abtrennen:                      *)

     BEGIN
       For A := 1 to Length(Befehl1) - 1 do
         If Befehl1[A] = '?' then Befehl[A] := '?';

       System.Delete(Befehl, Pos('*', Befehl1), Length(Befehl));
       System.Delete(Befehl1, Pos('*', Befehl1), 1)
     end
  else
    If (Not(Pos('*', Befehl1) <> 0)) and
       (Length(Befehl) = Length(Befehl1)) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen: *)

       For A := 1 to Length(Befehl1) - 1 do

         If Befehl1[A] = '?' then Befehl[A] := '?';

  If Befehl = Befehl1 then Befehlistgleich := True

END;

Procedure BehandleExtender;
BEGIN
  (* Ist nur ein Stern vorhanden? *)

  If Extender1[2] = '*' then Extenderistgleich := True

  else

  (* Ist ein Stern vorhanden? *)

  If (Pos('*', Extender1) <> 0) and

     (* Wenn der gegebene Dateiname nicht kleiner ist, als  *)
     (* die Maske ohne Stern, dann kann die Maske passen:   *)

     (Length(Extender) >= Length(Extender1) - 1) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen *)
       (* und nach Stern abtrennen:                      *)

       BEGIN
         For A := 1 to Length(Extender1) - 1 do
           If Extender1[A] = '?' then Extender[A] := '?';
         System.Delete(Extender, Pos('*', Extender1), Length(Extender));
         System.Delete(Extender1, Pos('*', Extender1), 1)
       end
  else
    If (Not(Pos('*', Extender1) <> 0)) and
       (Length(Extender) = Length(Extender1)) then

       (* Nichtrelevante Zeichen mit Fragezeichen fÅllen: *)

       For A := 1 to Length(Extender1) - 1 do

         If Extender1[A] = '?' then Extender[A] := '?';

  If Extender = Extender1 then Extenderistgleich := True

END;


BEGIN
  PasstinMaske := False;
  Befehlistgleich := False;
  Extenderistgleich := False;

  FSplit(Name, Pfad, Befehl, Extender);
  FSplit(Maske, Pfad1, Befehl1, Extender1);

  BehandleBefehl;
  BehandleExtender;

  If Befehlistgleich and
     Extenderistgleich   then PasstinMaske := True

END;



Function  UpDate(term: String):String;
var    i    : integer;
      ch    : char;
      UpTerm: string;

begin
  UpTerm := '';
  for i := 1 to Length(term) do
  begin
    ch := upcase(term[i]);
    if ch <> ' ' then UpTerm := UpTerm + ch
  end;
  UpDate := UpTerm
end;

function Open(var fp: Filetype; Name: Fname): boolean;
{---------------------------------------------------------}
{      ôffne Datei, falls vorhanden und gebe Status       }
{      zurÅck: open -> true, sonst -> false               }
{---------------------------------------------------------}
begin
 Assign(fp,Name);                     {* Zuweisung Dateiname   *}
 {$I-}
 Reset(fp,1);                         {* ôffne Datei           *}
 {$I+}
 Open := IOResult = 0;                {* Gebe Status zurÅck    *}
end { Open };

Procedure InitNext;
Var i,j: Word;
Begin
  i := 1;
  j := 0;
  Next[1] := 0;
  Repeat
    if (j = 0) or (Muster[i] = Muster[j]) then
    Begin
      Inc(i);
      Inc(j);
      Next[i] := j
    End
    Else
      j := Next[j]
  Until i >= Length(Muster)
End;



Function KmpSearch: Word;
Var i,j: Word;
Begin
  i := 1;
  j := 1;

  InitNext;

  Repeat
    If (j=0) or (UpCase(Char(Buffer[i])) = UpCase(Char(Muster[j]))) then
    Begin
      Inc(i);
      Inc(j)
    End
    Else j := Next[j]
  Until (j > Length(Muster)) or
        (i > SizeOf(Buffer));

  if j > Length(Muster) then
    KmpSearch := i - Length(Muster)
  else
    KmpSearch := i
end;


Function SucheWord(DateiName: String): Boolean;
Begin
  IF Not OPEN (Device, DateiName) THEN
  Begin
    writeln('Die Datei ', DateiName,' kann nicht geîffnet werden.');
    Writeln(TextFile, 'Die Datei ', DateiName,' kann nicht geîffnet werden.');
    Flush(TextFile)
  End
  Else
  Begin
    Writeln ('Die Datei: ', DateiName,' wird bearbeitet');
    For A := 0 to FileSize(Device) Div 16384 do Write('.');
    Writeln;
    WHILE NOT (EOF(device)) DO           {* Datei sequentiell lesen *}
     begin
      BlockRead (device, Buffer, MaxLen, lang); {* lese Zeile           *}
      Write('.');
      If KmpSearch < Lang + 1 then
      Begin
        Writeln;
        Writeln('Gefunden ', DateiName);
        Writeln(TextFile, DateiName);
        Flush(TextFile);
        Close(Device);
        Exit
      End
      Else
        If Not Eof(Device) then
          Seek(Device, FilePos(Device) - Length(Muster) - 1)
     end;
    CLOSE (device);                      {* Datei schlie·en      *}
    Writeln
  End
End;


Procedure List(pfad:string);
var sr          : searchrec;
begin
  If System.MemAvail < 1024 then
  Begin
    Writeln('Kein Speicherplatz mehr da!');
    Halt
  End;

  if (pfad[Length(pfad)] <> '\') then pfad := pfad + '\';

  FindFirst(pfad+'*.*',attr,sr);
  while DosError = 0 do
  begin
    if ((sr.attr and (Directory + VolumeID)) = 0) and
       (Pos('.', sr.Name) <> 0) then
    Begin
      If PasstInMaske(UpDate(FileName), Sr.Name) then
      Begin
        SucheWord(Pfad + Sr.Name)
      End;
    End;


    FindNext(sr)
  end;

  FindFirst(pfad+'*.*',attr,sr);
  while DosError = 0 do
  begin
    if (sr.attr and Directory) <> 0 then
      if sr.name <> '.' then
        if sr.name <> '..' then
          list(pfad + sr.name);
    FindNext(sr)
  end
end;

Procedure TestMaskenSyntax(Var Maske: String);
var Ext : String[4];

Procedure BearbeiteString(var Mask: String);

BEGIN
  (* Lîsche '?' direckt vor dem '*' *)

    repeat
      If Mask[Pos('*', Mask)-1] = '?' then
        System.delete(Mask, Pos('*', Mask)-1, 1)
    until Mask[Pos('*', Mask)-1] <> '?';

    (* Nach erstem '*' abschneiden, wenn noch Zeichen da sind: *)

    If Pos('*', Mask) <> 0 then
      If Length(Mask) > Pos('*', Mask) then
       System.delete(Mask, Pos('*', Mask)+1, Length(Mask)-Pos('*', Mask))
END;

BEGIN
  Pfad := '';
  Befehl := '';
  Extender := '';

  Fsplit(Maske, Pfad, Befehl, Extender);

  BearbeiteString(Befehl);
  BearbeiteString(Extender);
  Maske := Befehl+Extender
END;


begin


  Filename := '';

  begin
    ClrScr;
    Writeln ('M u l t i  S e a r c h       (c) Lothar Behrens Version 1.');
    Writeln ('                             (c)                     1994.');
    Writeln;
    Write('File  (Vorgabe = *.*): ');
    Readln (Filename);
    Writeln;
  end;


  IF Length(Filename) = 0 THEN         {* Leereingabe ?       *}
    FileName := '*.*';

  TestMaskenSyntax(FileName);
  If FileName = '*' then FileName := '*.*';
  If Pos('.', FileName) = 0 then
    FileName := FileName + '.*';

  (* Hauptaktion: *)

  Write('Muster eingeben: ');
  Readln(Muster);

  Attr := $3F;

  Assign(TextFile, OutTextDat);
  {$I-}
  Reset(TextFile);
  If IoResult = 0 then
  Begin
    Close(TextFile);
    System.Erase(TextFile)
  End;
  {$I+}
  Rewrite(TextFile);
  GetDir(0, DirString);
  List(DirString);
  Close(TextFile);

end.

