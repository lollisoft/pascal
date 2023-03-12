Unit TVDVImp;
{$O+,F+}
Interface

Uses App, Views, Drivers, TVDVDATA, Dos, LabHist, StdDlg, MsgBox, Strings;



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

    MsgRes       : Word;



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



  Assign(StrucktDataFile, PData^.GetStrucktFileName);

  Assign(HilfDataFile,    PData^.GetHilfFileName);

  Assign(ZusatzDataFile,  PData^.GetZusatzFileName);

  Assign(DateiDataFile,   PData^.GetDateiFileName);

  Assign(PfadDataFile,    PData^.GetPfadFileName);

  Assign(LabelDataFile,   PData^.GetLabelFileName);



  Rename(StrucktDataFile, 'Puffer0.DVS');

  Rename(HilfDataFile,    'Puffer0.DVH');

  Rename(ZusatzDataFile,  'Puffer0.DVZ');

  Rename(DateiDataFile,   'Puffer0.DVD');

  Rename(PfadDataFile,    'Puffer0.DVP');

  Rename(LabelDataFile,   'Puffer0.DVL');



  Counter := 1;



  BuildDataFiles(Counter);



  Rename(StrucktDataFile, PData^.GetStrucktFileName);

  Rename(HilfDataFile,    PData^.GetHilfFileName);

  Rename(ZusatzDataFile,  PData^.GetZusatzFileName);

  Rename(DateiDataFile,   PData^.GetDateiFileName);

  Rename(PfadDataFile,    PData^.GetPfadFileName);

  Rename(LabelDataFile,   PData^.GetLabelFileName);



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

    If Pos('DatentrÑgernummer', ImportData) <> 0 then ImportData := ''

  End;



  If Pos('Byte', ImportData) <> 0 then

  Begin

    IOError := 1;

    Exit

  End;



  If Pos('DatentrÑger ', ImportData) <> 0 then

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







Procedure MakeImportDirTree(Var Tree: ImportDirTreeXPtrtyp);









Procedure InsertInTree(Var Lauf: ImportDirTreeXPtrtyp;

                        Ziel: Longint;

                     Var Dif: Boolean);

var p1, p2 : ImportDirTreeXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin

    new(lauf.Ptr);

    dif := true;

    With Lauf.Ptr^ do

    Begin

      Zeiger := Ziel;

      Links.Ptr := Nil;

      Rechts.Ptr := Nil;

      Bal := 0

    End

  end



  else



  if GetDirElement(Ziel) < GetDirElement(Lauf.Ptr^.Zeiger) then

  begin

    InsertInTree(Lauf.Ptr^.Links, Ziel, Dif);



    if dif then

    case lauf.Ptr^.bal of



    1 : begin lauf.Ptr^.bal := 0; dif := false end;



    0 : lauf.Ptr^.bal := -1;



    -1: begin

          p1 := Lauf.Ptr^.links;

          if p1.Ptr^.bal = -1 then

          begin

            lauf.Ptr^.links := p1.Ptr^.rechts;

            p1.Ptr^.rechts := lauf;

            lauf.Ptr^.bal := 0;

            lauf := p1

          end



          else



          begin

            p2 := p1.Ptr^.rechts;

            p1.Ptr^.rechts := p2.Ptr^.links;

            p2.Ptr^.links := p1;

            lauf.Ptr^.links := p2.Ptr^.rechts;

            p2.Ptr^.rechts := lauf;



            if p2.Ptr^.bal = -1 then lauf.Ptr^.bal := 1

                            else lauf.Ptr^.bal := 0;



            if p2.Ptr^.bal = 1  then P1.Ptr^.bal := -1

                            else P1.Ptr^.bal := 0;

            lauf := p2;

          end;

          lauf.Ptr^.bal := 0;

          Dif := false

        end

      end

  end



  else



  if GetDirElement(Ziel) > GetDirElement(Lauf.Ptr^.Zeiger) then

  begin

    InsertInTree(lauf.Ptr^.rechts, Ziel, Dif);



    if dif then

    case lauf.Ptr^.bal of

    -1 : begin lauf.Ptr^.bal := 0; dif := false end;



    0 : lauf.Ptr^.bal := +1;



    1: begin

         p1 := Lauf.Ptr^.rechts;

         if p1.Ptr^.bal = + 1 then

         begin

           lauf.Ptr^.rechts := p1.Ptr^.links;

           p1.Ptr^.links := lauf;

           lauf.Ptr^.bal := 0;

           lauf := p1

         end



         else



         begin

           p2 := p1.Ptr^.links;

           p1.Ptr^.links := p2.Ptr^.rechts;

           p2.Ptr^.rechts := p1;

           lauf.Ptr^.rechts := p2.Ptr^.links;

           p2.Ptr^.links := lauf;



           if p2.Ptr^.bal = +1 then lauf.Ptr^.bal := -1

                           else lauf.Ptr^.bal := 0;



           if p2.Ptr^.bal = -1 then P1.Ptr^.bal := 1

                           else P1.Ptr^.bal := 0;

           lauf := p2;

         end;



         lauf.Ptr^.bal := 0;

         Dif := false

       end

     end

  end

end;



Var ImportDif: Boolean;

    Position : Longint;

Begin

  ImportDif := False;

  Tree.Ptr := Nil;

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



    Df := PData^.GetDateifilename;

    Pf := PData^.GetPfadfilename;

    Lf := PData^.GetLabelfilename;

    Sf := PData^.GetStrucktfilename;

    Hf := PData^.GetHilffilename;

    Zf := PData^.GetZusatzfilename;



    PData^.SetDateifilename('Puffer' + GetString(Counter) + '.DVD');

    PData^.SetPfadfilename('Puffer' + GetString(Counter) + '.DVP');

    PData^.SetLabelfilename('Puffer' + GetString(Counter) + '.DVL');

    PData^.SetStrucktfilename('Puffer' + GetString(Counter) + '.DVS');

    PData^.SetHilffilename('Puffer' + GetString(Counter) + '.DVH');

    PData^.SetZusatzfilename('Puffer' + GetString(Counter) + '.DVZ');



    Inc(Counter);



    PData^.InitFiles;



    PData^.SpeichereDateien;



    PData^.SetDateifilename(DF);

    PData^.SetPfadfilename(PF);

    PData^.SetLabelfilename(LF);

    PData^.SetStrucktfilename(SF);

    PData^.SetHilffilename(HF);

    PData^.SetZusatzfilename(ZF);



    PData^.InitFiles;



    PData^.SetAllNil;



  End;

  If Pfad[Length(Pfad)] = '\' then System.Delete(Pfad, Length(Pfad), 1);



  pfad := pfad + '\';



  PData^.SpeicherePfad(Pfad, PfadList, PfadDif, NoNew);



  (* Neues Element in Strucktliste erzeugen: *)



  PData^.GetStruckt(StrucktListend,AltStruckt);



  (* FÅr Zusatzdaten: *)



  Pfadzeiger := StrucktListEnd;



  (* Zuweisung in das Element: *)



  StrucktListend.Ptr^.StrucktData.art := 2;

  StrucktListend.Ptr^.StrucktData.PfadList := pfadadress;



  (* Neues Element in Hilfliste erzeugen: *)



  PData^.GetHilf(HilfListend,AltHilf);



  (* Zuweisung in das Element: *)



  HilfListend.Ptr^.HilfData.art := 2;

  HilfListend.Ptr^.HilfData.PfadList := StrucktListend;









  ReadLine(Sr);

  while IOError = 0 do

  begin

    if (sr.attr and (directory + VolumeID)) = 0 then

    begin

      PData^.SpeichereDatei(sr.name, DateiList, DateiDif, NoNew);



      (* Neues Element in Strucktliste erzeugen: *)



      PData^.GetStruckt(StrucktListend,AltStruckt);



      (* Zuweisung in das Element: *)



      StrucktListend.Ptr^.StrucktData.art := 3;

      StrucktListend.Ptr^.StrucktData.DateiList := Dateiadress;



      If MemWasLo then

      Begin

        LabelZeiger.Ptr := Nil;

        PData^.SpeichereSuch(LabelZeiger,Pfadzeiger,Dateiadress)

      End

      Else

        PData^.SpeichereSuch(Labelzeiger,Pfadzeiger,Dateiadress)

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

      If ExecDialog(ReadString('DatentrÑger hat noch keine Disk Nr. (Disk...) ?'), Info) = cmCancel

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



  (* Sicherstellen, da· nur Disketten mit korrektem Label *)

  (* eingelesen werden: *)



  if pos('DISK',Lab) <> 0 then

  begin

    IOError := 0;

    if IOError = 0 then

    begin

      PData^.SpeichereLabel(lab,LabelList,LabelDif, NoNew);

      Name := Lab;



      PData^.GetStruckt(StrucktListend,AltStruckt);



      StrucktListend.Ptr^.StrucktData.art := 1;

      StrucktListend.Ptr^.StrucktData.LabelList := Labeladress;





      PData^.GetHilf(HilfListend,AltHilf);



      HilfListend.Ptr^.HilfData.art := 1;

      HilfListend.Ptr^.HilfData.LabelList := StrucktListend;



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

    MsgRes := MessageBox('Diskette hat einen ungÅltigen Label!',

        Nil, mfError + mfOkButton);

        Name := '';

    DirStatus := False

  end;



  Exit1:



  If MemWasLo then

  Begin

    Df := PData^.GetDateifilename;

    Pf := PData^.GetPfadfilename;

    Lf := PData^.GetLabelfilename;

    Sf := PData^.GetStrucktfilename;

    Hf := PData^.GetHilffilename;

    Zf := PData^.GetZusatzfilename;



    PData^.SetDateifilename('Puffer' + GetString(Counter) + '.DVD');

    PData^.SetPfadfilename('Puffer' + GetString(Counter) + '.DVP');

    PData^.SetLabelfilename('Puffer' + GetString(Counter) + '.DVL');

    PData^.SetStrucktfilename('Puffer' + GetString(Counter) + '.DVS');

    PData^.SetHilffilename('Puffer' + GetString(Counter) + '.DVH');

    PData^.SetZusatzfilename('Puffer' + GetString(Counter) + '.DVZ');



    PData^.InitFiles;



    PData^.SpeichereDateien;



    PData^.SetDateifilename(DF);

    PData^.SetPfadfilename(PF);

    PData^.SetLabelfilename(LF);

    PData^.SetStrucktfilename(SF);

    PData^.SetHilffilename(HF);

    PData^.SetZusatzfilename(ZF);



    PData^.InitFiles;



    PData^.SetAllNil;



    BuildDataFiles(Counter);

    Wait('Datenimport Åberforderte SpeicherkapazitÑt:' + Chr(13) +

         'Bitte schauen Sie fÅr Weiteres in der Hilfe fÅr Datenimport nach')



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

    Wait('Import - Datei kann nicht geîffnet werden!');

    Exit

  End;



  Assign(ImportFile, 'IMP.TMP');

  {$I-}

  Reset(ImportFile);



  If IOResult = 0 then

  Begin

    Wait('TemponÑr - Datei Åberschreibt fremde Datei!');

    Close(ImportFile);

    Exit

  End;



  ReWrite(ImportFile);



  If IOResult <> 0 then

  Begin

    Wait('TemponÑr - Datei kann nicht geîffnet werden!');

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





  (* Importfile wird nicht gelîscht, damit eine öberprÅfung durchgefÅhrt *)

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