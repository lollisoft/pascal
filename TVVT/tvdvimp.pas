Unit TVDVImp;
{$O+,F+}
Interface

Uses App, Views, Drivers, Video, Dos, LabHist, StdDlg, MsgBox, BDF;

Const MemWasLo: Boolean = False;

Procedure Import(FileName: String);
Procedure ImportNachtrag;
Procedure ImportPruefen;

Implementation


Var Df,
    Pf,
    Lf,
    Sf,
    Hf,
    Zf           : String12;
    Counter      : Integer;
    Name         : String;

    TextFile     : Text;
    IOError      : ShortInt;


Procedure ImportNachtrag;
Begin
  Assign(StrucktDataFile, 'Puffer0.DVS');
  Assign(HilfDataFile,    'Puffer0.DVH');
  Assign(ZusatzDataFile,  'Puffer0.DVZ');
  Assign(DateiDataFile,   'Puffer0.DVD');
  Assign(PfadDataFile,    'Puffer0.DVP');
  Assign(LabelDataFile,   'Puffer0.DVL');

  Rename(StrucktDataFile, 'Puffer1.DVS');
  Rename(HilfDataFile,    'Puffer1.DVH');
  Rename(ZusatzDataFile,  'Puffer1.DVZ');
  Rename(DateiDataFile,   'Puffer1.DVD');
  Rename(PfadDataFile,    'Puffer1.DVP');
  Rename(LabelDataFile,   'Puffer1.DVL');

  Assign(StrucktDataFile, StrucktFileName);
  Assign(HilfDataFile,    HilfFileName);
  Assign(ZusatzDataFile,  ZusatzFileName);
  Assign(DateiDataFile,   DateiFileName);
  Assign(PfadDataFile,    PfadFileName);
  Assign(LabelDataFile,   LabelFileName);

  Rename(StrucktDataFile, 'Puffer0.DVS');
  Rename(HilfDataFile,    'Puffer0.DVH');
  Rename(ZusatzDataFile,  'Puffer0.DVZ');
  Rename(DateiDataFile,   'Puffer0.DVD');
  Rename(PfadDataFile,    'Puffer0.DVP');
  Rename(LabelDataFile,   'Puffer0.DVL');

  Counter := 1;

  BuildDataFiles(Counter);

  Rename(StrucktDataFile, StrucktFileName);
  Rename(HilfDataFile,    HilfFileName);
  Rename(ZusatzDataFile,  ZusatzFileName);
  Rename(DateiDataFile,   DateiFileName);
  Rename(PfadDataFile,    PfadFileName);
  Rename(LabelDataFile,   LabelFileName);

End;
Procedure ImportPruefen;
Begin
End;

Procedure ReadLine(Var SR: SearchRec);
Var Path    : PathStr;
    Dir     : DirStr;
    Name    : NameStr;
    Ext     : ExtStr;
    DName   : String12;
Begin
  DName := '';
  ImportData := '';
  While (ImportData = '') do
  Begin
    Read(ImportFile, ImportData);
    If Pos('Datentr�gernummer', ImportData) <> 0 then ImportData := ''
  End;

  If Pos('Byte', ImportData) <> 0 then
  Begin
    IOError := 1;
    Exit
  End;

  If Pos('Datentr�ger ', ImportData) <> 0 then
  Begin
    DName := Copy(ImportData, 32, Length(ImportData) - 31);
    SR.Attr := Directory;
    SR.Name := DName;
    Exit
  End;

  If Pos('Verzeichnis', ImportData) <> 0 then
  Begin
    DName := Copy(ImportData, 17, Length(ImportData) - 16);
    SR.Attr := Directory;
    SR.Name := DName;
    Exit
  End;

  If Pos('<DIR>', ImportData) <> 0 then
  Begin
    DName := ImportData;
    SR.Attr := Directory;
    While DName[Length(DName)] = ' ' do System.Delete(DName, Length(DName), 1);
    If Length(DName) > 8 then DName[9] := '.';
    While Pos(' ', DName) <> 0 do System.Delete(DName, Pos(' ', DName), 1);
    SR.Name := DName
  End
  Else
  Begin
    DName := ImportData;
    SR.Attr := 0;
    While DName[Length(DName)] = ' ' do System.Delete(DName, Length(DName), 1);
    If Length(DName) > 8 then DName[9] := '.';
    While Pos(' ', DName) <> 0 do System.Delete(DName, Pos(' ', DName), 1);
    SR.Name := DName
  End
End;

Function OpenImportFile(FileName: String): Boolean;
Begin
  {$I-}
  Assign(TextFile, FileName);
  Reset(TextFile);
  OpenImportFile := IOResult = 0
  {$I+}
End;



Procedure MakeImportDirTree(Var Tree: ImportDirTreePtrtyp);




Procedure InsertInTree(Var Lauf: ImportDirTreePtrtyp;
                        Ziel: Longint;
                     Var Dif: Boolean);
var p1, p2 : ImportDirTreePtrtyp;
begin
  if Lauf = Nil then
  begin
    new(lauf);
    dif := true;
    With Lauf^ do
    Begin
      Zeiger := Ziel;
      Links := Nil;
      Rechts := Nil;
      Bal := 0
    End
  end

  else

  if GetDirElement(Ziel) < GetDirElement(Lauf^.Zeiger) then
  begin
    InsertInTree(Lauf^.Links, Ziel, Dif);

    if dif then
    case lauf^.bal of

    1 : begin lauf^.bal := 0; dif := false end;

    0 : lauf^.bal := -1;

    -1: begin
          p1 := Lauf^.links;
          if p1^.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts := lauf;
            lauf^.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts;
            p1^.rechts := p2^.links;
            p2^.links := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts := lauf;

            if p2^.bal = -1 then lauf^.bal := 1
                            else lauf^.bal := 0;

            if p2^.bal = 1  then P1^.bal := -1
                            else P1^.bal := 0;
            lauf := p2;
          end;
          lauf^.bal := 0;
          Dif := false
        end
      end
  end

  else

  if GetDirElement(Ziel) > GetDirElement(Lauf^.Zeiger) then
  begin
    InsertInTree(lauf^.rechts, Ziel, Dif);

    if dif then
    case lauf^.bal of
    -1 : begin lauf^.bal := 0; dif := false end;

    0 : lauf^.bal := +1;

    1: begin
         p1 := Lauf^.rechts;
         if p1^.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links := lauf;
           lauf^.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links;
           p1^.links := p2^.rechts;
           p2^.rechts := p1;
           lauf^.rechts := p2^.links;
           p2^.links := lauf;

           if p2^.bal = +1 then lauf^.bal := -1
                           else lauf^.bal := 0;

           if p2^.bal = -1 then P1^.bal := 1
                           else P1^.bal := 0;
           lauf := p2;
         end;

         lauf^.bal := 0;
         Dif := false
       end
     end
  end
end;

Var ImportDif: Boolean;
    Position : Longint;
Begin
  ImportDif := False;
  Tree := Nil;
  While Not Eof(ImportFile) do
  Begin
    Read(ImportFile, ImportData);
    If Pos('Verzeichnis', ImportData) <> 0 then
    Begin
      Position := FilePos(ImportFile);
      InsertInTree(Tree, Position - 1, ImportDif);
      Seek(ImportFile, Position)
    End;
  End;
  Seek(ImportFile, 0)
End;



Procedure List(Pfad:String; Line: Longint);
var sr          : searchrec;
    h           : Integer;
    ReScan      : Longint;
begin
  ReScan := Line;
  Seek(ImportFile, Line);
  IOError := 0;
  If System.MemAvail < 80000 then
  Begin
    MemWasLo := True;

    Df := Dateifilename;
    Pf := Pfadfilename;
    Lf := Labelfilename;
    Sf := Strucktfilename;
    Hf := Hilffilename;
    Zf := Zusatzfilename;

    Dateifilename   := 'Puffer' + GetString(Counter) + '.DVD';
    Pfadfilename    := 'Puffer' + GetString(Counter) + '.DVP';
    Labelfilename   := 'Puffer' + GetString(Counter) + '.DVL';
    Strucktfilename := 'Puffer' + GetString(Counter) + '.DVS';
    Hilffilename    := 'Puffer' + GetString(Counter) + '.DVH';
    Zusatzfilename  := 'Puffer' + GetString(Counter) + '.DVZ';

    Inc(Counter);

    InitFiles;

    SpeichereDateien;

    Dateifilename   := DF;
    Pfadfilename    := PF;
    Labelfilename   := LF;
    Strucktfilename := SF;
    Hilffilename    := HF;
    Zusatzfilename  := ZF;

    InitFiles;

    SetAllNil;

  End;
  If Pfad[Length(Pfad)] = '\' then System.Delete(Pfad, Length(Pfad), 1);

  pfad := pfad + '\';

  SpeicherePfad(Pfad, PfadList, PfadDif, NoNew);

  (* Neues Element in Strucktliste erzeugen: *)

  GetStruckt(StrucktListend,AltStruckt);

  (* F�r Zusatzdaten: *)

  Pfadzeiger := StrucktListEnd;

  (* Zuweisung in das Element: *)

  StrucktListend^.StrucktData.art := 2;
  StrucktListend^.StrucktData.PfadList := pfadadress;

  (* Neues Element in Hilfliste erzeugen: *)

  GetHilf(HilfListend,AltHilf);

  (* Zuweisung in das Element: *)

  HilfListend^.HilfData.art := 2;
  HilfListend^.HilfData.PfadList := Pointer(StrucktListend);




  ReadLine(Sr);
  while IOError = 0 do
  begin
    if (sr.attr and (directory + VolumeID)) = 0 then
    begin
      SpeichereDatei(sr.name, DateiList, DateiDif, NoNew);

      (* Neues Element in Strucktliste erzeugen: *)

      GetStruckt(StrucktListend,AltStruckt);

      (* Zuweisung in das Element: *)

      StrucktListend^.StrucktData.art := 3;
      StrucktListend^.StrucktData.DateiList := Dateiadress;

      If MemWasLo then
        SpeichereSuch(Nil,Pfadzeiger,Dateiadress)
      Else
        SpeichereSuch(Labelzeiger,Pfadzeiger,Dateiadress)
    end;

    ReadLine(Sr)
  end;


  IOError := 0;

  (* In Unterverzeichnisse gehen: *)

  Seek(ImportFile, ReScan);
  ReadLine(Sr);
  While IOError = 0 do
  Begin
    if (sr.attr and Directory) <> 0 then
      if sr.name <> '.' then
        if sr.name <> '..' then
        Begin
          Line := FilePos(ImportFile);
          list(pfad+sr.name, SearchPfadLine(Pfad + Sr.Name));
          IOError := 0;
          Seek(ImportFile, Line)
        End;
    ReadLine(Sr)
  End

end;



Procedure Dir;
Label Exit1;
Var Info : ^String;
    F    : File;
    MsgRes       : Word;
begin
  MemWasLo := False;
  Counter := 0;
  Pfad := '';
  DirStatus := True;

  ReadLine(SR);

  lab := SR.name;

  ReadLine(SR);
  Seek(ImportFile, 0);

  Pfad := SR.Name;

  If Pos('DISK', Lab) = 0 then
  Begin
    (*Repeat
      New(Info);
      Info^ := Lab;
      Lab := 'DISK-CD-0';
      If ExecDialog(ReadString('Datentr�ger hat noch keine Disk Nr. (Disk...) ?'), Info) = cmCancel
        then Exit;
      Info^ := UpDate(Info^)

    Until (Info^ <> '') and
          (Pos('DISK', Info^) <> 0);

    Lab := Info^;

    Dispose(Info);

    Info := Nil;
    *)
    Lab := 'DISK-CD-0'
  End;

  If LabelInLabelHistList(Lab) then
  Begin
    Name := Lab;
    Goto Exit1
  end;

  (* Sicherstellen, da� nur Disketten mit korrektem Label *)
  (* eingelesen werden: *)

  if pos('DISK',Lab) <> 0 then
  begin
    IOError := 0;
    if IOError = 0 then
    begin
      SpeichereLabel(lab,LabelList,LabelDif, NoNew);
      Name := Lab;

      GetStruckt(StrucktListend,AltStruckt);

      StrucktListend^.StrucktData.art := 1;
      StrucktListend^.StrucktData.LabelList := Labeladress;


      GetHilf(HilfListend,AltHilf);

      HilfListend^.HilfData.art := 1;
      HilfListend^.HilfData.LabelList := Pointer(StrucktListend);

      Labelzeiger := StrucktListend;

      list(UpDate(Pfad), SearchPfadLine(UpDate(Pfad)))
    end

    else
    begin
      MsgRes := MessageBox('ImportDatei nicht in Ordnung!', nil, mfOkButton);
      DirStatus := False
    end
  end

  else

  begin
    MsgRes := MessageBox('Diskette hat einen ung�ltigen Label!',
        Nil, mfError + mfOkButton);
        Name := '';
    DirStatus := False
  end;

  Exit1:

  If MemWasLo then
  Begin
    Df := Dateifilename;
    Pf := Pfadfilename;
    Lf := Labelfilename;
    Sf := Strucktfilename;
    Hf := Hilffilename;
    Zf := Zusatzfilename;

    Dateifilename   := 'Puffer' + GetString(Counter) + '.DVD';
    Pfadfilename    := 'Puffer' + GetString(Counter) + '.DVP';
    Labelfilename   := 'Puffer' + GetString(Counter) + '.DVL';
    Strucktfilename := 'Puffer' + GetString(Counter) + '.DVS';
    Hilffilename    := 'Puffer' + GetString(Counter) + '.DVH';
    Zusatzfilename  := 'Puffer' + GetString(Counter) + '.DVZ';

    InitFiles;

    SpeichereDateien;

    Dateifilename   := DF;
    Pfadfilename    := PF;
    Labelfilename   := LF;
    Strucktfilename := SF;
    Hilffilename    := HF;
    Zusatzfilename  := ZF;

    InitFiles;

    SetAllNil;

    BuildDataFiles(Counter);
    Wait('Datenimport �berforderte Speicherkapazit�t:' + Chr(13) +
         'Bitte schauen Sie f�r Weiteres in der Hilfe f�r Datenimport nach')

  End
end;




Procedure Import;
Var Stunde,
    Minute,
    Sekunde,
    HSec    : Word;
    S,
    M,
    Se,
    Time1,
    Time2   : String;
Begin
  GetTime(Stunde, Minute, Sekunde, HSec);
  Str(Stunde, S);
  Str(Minute, M);
  Str(Sekunde, Se);

  Time1 := S + ':' + M + ',' + Se;



  If Not OpenImportFile(FileName) then
  Begin
    Wait('Import - Datei kann nicht ge�ffnet werden!');
    Exit
  End;

  Assign(ImportFile, 'IMP.TMP');
  {$I-}
  Reset(ImportFile);

  If IOResult = 0 then
  Begin
    Wait('Tempon�r - Datei �berschreibt fremde Datei!');
    Close(ImportFile);
    Exit
  End;

  ReWrite(ImportFile);

  If IOResult <> 0 then
  Begin
    Wait('Tempon�r - Datei kann nicht ge�ffnet werden!');
    Exit
  End;
  {$I+}


  While Not Eof(TextFile) and
        Not (Pos('Anzahl', ImportData) <> 0) do
  Begin
    Readln(TextFile, ImportData);
    If (ImportData <> '') and
       (Pos('Anzahl', ImportData) = 0) then
      Write(ImportFile, ImportData);
  End;

  Seek(ImportFile, 0);

  MakeImportDirTree(ImportDirTree);

  Write(Chr(7));

  Dir;
  Close(ImportFile);
  Close(TextFile);


  (* Importfile wird nicht gel�scht, damit eine �berpr�fung durchgef�hrt *)
  (* werden kann.                                                        *)

  (*Erase(ImportFile);*)


  For A := 1 to Counter do
  Begin
    Assign(StrucktDataFile, 'Puffer' + GetString(A) + '.DVS');
    Assign(HilfDataFile,    'Puffer' + GetString(A) + '.DVH');
    Assign(ZusatzDataFile,  'Puffer' + GetString(A) + '.DVZ');
    Assign(DateiDataFile,   'Puffer' + GetString(A) + '.DVD');
    Assign(PfadDataFile,    'Puffer' + GetString(A) + '.DVP');
    Assign(LabelDataFile,   'Puffer' + GetString(A) + '.DVL');

    Erase(StrucktDataFile);
    Erase(HilfDataFile);
    Erase(ZusatzDataFile);
    Erase(DateiDataFile);
    Erase(PfadDataFile);
    Erase(LabelDataFile)
  End;



  GetTime(Stunde, Minute, Sekunde, HSec);
  Str(Stunde, S);
  Str(Minute, M);
  Str(Sekunde, Se);

  Time2 := S + ':' + M + ',' + Se;

  Wait('Dauer des Importierens von:' + Chr(13) +
       Time1 +                         Chr(13) +
       'bis' +                         Chr(13) +
       Time2);

End;
End.