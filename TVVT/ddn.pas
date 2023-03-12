Unit DDN; (* Disk - Direckt - Nachtrag *)
{$X+,O+,F+}
Interface

Uses Video;

Type   DataDifDatatyp      = Record
                               Stelle: Longint;
                               Anzahl: Integer;
                               Dif   : ShortInt
                             End;

       DataDifTreePtrtyp   = ^DataDifListtyp;
       DataDifListtyp      = Record
                               Links,
                               Rechts           : DataDifTreePtrtyp;
                               Bal              : ShortInt;
                               DataDifData      : DataDifDatatyp
                             end;

Const NoDif   : ShortInt = 0;
      DifRaus : ShortInt = -1;
      DifRein : ShortInt = 1;

Var DateiDifTree,
    PfadDifTree,
    LabelDifTree,
    StrucktDifTree,
    HilfDifTree,
    ZusatzDifTree   : DataDifTreePtrtyp;

    DateiTmpDifTree,
    PfadTmpDifTree,
    LabelTmpDifTree,
    StrucktTmpDifTree,
    HilfTmpDifTree,
    ZusatzTmpDifTree   : DataDifTreePtrtyp;


    DDif,
    PDif,
    LDif,
    SDif,
    HDif,
    ZDif            : Boolean;

    DateiTmp        : DateiFiletyp;
    PfadTmp         : PfadFiletyp;
    LabelTmp        : LabelFiletyp;
    StrucktTmp      : StrucktFiletyp;
    HilfTmp         : HilfFiletyp;
    ZusatzTmp       : ZusatzFiletyp;



Procedure DDNachtrag(Quelle, Ziel: String12);

Implementation


Procedure DDNachtrag;

Procedure AVLInsert(InsertStelle: Longint;
                    InsertAnzahl: Integer;
                    InsertDif   : ShortInt;
                      var  Lauf : DataDifTreePtrtyp;
                      var  dif  : Boolean;
                           NoNew: Boolean);

var p1, p2 : DataDifTreePtrtyp;
begin
  if Lauf = Nil then
  begin

    (* Nur balangsieren? *)

    If NoNew then Lauf := TreePointer
    Else
      new(lauf);


    dif := true;
    with lauf^ do
    begin
      Stelle := InsertStelle;
      Anzahl := InsertAnzahl;
      Links := Nil;
      Rechts := Nil;
      Bal := 0;
    end
  end

  else

  if InsertStelle < Lauf^.DataDifData.Stelle then
  begin
    AVLInsert(InsertStelle, InsertAnzahl, InsertDif, lauf^.links,dif, NoNew);

    if dif then
    case lauf^.bal of

    1 : begin lauf^.bal := 0; dif := false end;

    0 : lauf^.bal := -1;

    -1: begin (* Ausgleichen *)
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

  if InsertStelle > Lauf^.DataDifData.Stelle then
  begin
    AVLInsert(InsertStelle, InsertAnzahl, InsertDif, lauf^.rechts,dif, NoNew);

    if dif then
    case lauf^.bal of
    -1 : begin lauf^.bal := 0; dif := false end;

    0 : lauf^.bal := +1;

    1: begin (* Ausgleichen *)
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
  else
  begin
    Wait('Element schon besetzt!')
    (*Inc(Lauf^.DateiData.Anzahl);
    Dif := False*)
  end
end;



Procedure OpenallTmpFiles;
BEGIN
  ZusatzDataDateiOeffnen(ZusatzTmp);
  DateiDataDateiOeffnen(DateiTmp);
  PfadDataDateiOeffnen(PfadTmp);
  LabelDataDateiOeffnen(LabelTmp);
  HilfDataDateiOeffnen(HilfTmp);
  StrucktDataDateiOeffnen(StrucktTmp)
END;

Procedure CloseallTmpFiles;
BEGIN
  close(StrucktTmp);
  close(HilfTmp);
  close(LabelTmp);
  close(PfadTmp);
  close(DateiTmp);
  close(ZusatzTmp)
END;


Procedure InitTmpFiles;
BEGIN
  assign(StrucktTmp,Strucktfilename);
  assign(HilfTmp,Hilffilename);
  assign(DateiTmp,Dateifilename);
  assign(LabelTmp,Labelfilename);
  assign(PfadTmp,Pfadfilename);
  assign(ZusatzTmp,Zusatzfilename);
END;



Procedure GetDateiDif;
Var DateiDataTmp,
    DateiData    : DateiDatatyp;
    DD           : Longint;
Begin
  DD := 0;

  (* Volgender Abschnitt stellt fest, welche Dateien eingefÅgt werden. *)
  (* Dies wird in einen AVL Baum geschrieben und spÑter verwendet      *)



  Read(DateiTmp, DateiDataTmp);
  Read(DateiDataFile, DateiData);

  While Not Eof(DateiTmp) do
  Begin

    While (Not Eof(DateiDataFile)) and
          (DateiData.DateiName < DateiDataTmp.DateiName) do
      Read(DateiDataFile, DateiData);

    (* Wenn DateiData.DateiName grî·er ist als DateiTmp.DateiName, *)
    (* ist die Datei nicht vorhanden. DateiData darf nun nicht     *)
    (* Åberschrieben werden, da dieser Datensatz mit dem nÑchsten  *)
    (* DateiTmp - Datensatz verglichen wird.                       *)
    (* Es ist dann volgendes mîglich:                              *)
    (* DateiTmp.DateiName = DateiData.DateiName                    *)



    If DateiData.DateiName = DateiDataTmp.DateiName then

    (* Datei in alten Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(DateiDatafile),
                DateiDataTmp.Anzahl,
                NoDif,
                DateiDifTree,
                DDif,
                False)
*)    End
    Else

    (* Datei in alten Daten nun grî·er -> Neue Datei wird vor FilePos *)
    (* eingefÅgt.                                                     *)

    Begin
      Inc(DD);
      AVLInsert(FilePos(DateiDatafile),
                DateiDataTmp.Anzahl,
                DifRein,
                DateiDifTree,
                DDif,
                False)
    End;


    Read(DateiTmp, DateiDataTmp)
  End;



  (* Der zweite Abschnitt stellt fest, welche Dateien Rausfallen... *)

  Reset(DateiTmp);
  Reset(DateiDataFile);


  Read(DateiTmp, DateiDataTmp);
  Read(DateiDataFile, DateiData);

  While Not Eof(DateiDataFile) do
  Begin

    While (Not Eof(DateiTmp)) and
          (DateiData.DateiName > DateiDataTmp.DateiName) do
      Read(DateiTmp, DateiDataTmp);

    If DateiData.DateiName = DateiDataTmp.DateiName then

    (* Datei in neuen Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(DateiDatafile),
                DateiDataTmp.Anzahl,
                NoDif,
                DateiDifTree,
                DDif,
                False)
*)    End
    Else

    (* Datei in neuen Daten nun grî·er -> Datei wird vor FilePos *)
    (* entfernt.                                                 *)

    Begin
      Inc(DD);
      AVLInsert(FilePos(DateiDatafile),
                DateiDataTmp.Anzahl,
                DifRaus,
                DateiDifTree,
                DDif,
                False)
    End;


    Read(DateiDataFile, DateiData)
  End;
  Wait('Dateien, die behandelt werden: ' + GetString(DD) + Chr(13) +
       'Dateien, die Nachgetragen werden: ' + GetString(FileSize(DateiTmp)))
End;

Procedure GetPfadDif;
Var PfadDataTmp,
    PfadData    : PfadDatatyp;
Begin

  (* Volgender Abschnitt stellt fest, welche Pfaden eingefÅgt werden. *)
  (* Dies wird in einen AVL Baum geschrieben und spÑter verwendet      *)



  Read(PfadTmp, PfadDataTmp);
  Read(PfadDataFile, PfadData);

  While Not Eof(PfadTmp) do
  Begin

    While (Not Eof(PfadDataFile)) and
          (PfadData.PfadName < PfadDataTmp.PfadName) do
      Read(PfadDataFile, PfadData);

    (* Wenn PfadData.PfadName grî·er ist als PfadTmp.PfadName,    *)
    (* ist die Pfad nicht vorhanden. PfadData darf nun nicht      *)
    (* Åberschrieben werden, da dieser Datensatz mit dem nÑchsten *)
    (* PfadTmp - Datensatz verglichen wird.                       *)
    (* Es ist dann volgendes mîglich:                             *)
    (* PfadTmp.PfadName = PfadData.PfadName                       *)



    If PfadData.PfadName = PfadDataTmp.PfadName then

    (* Pfad in alten Daten vorhanden -> Nur Anzahl Ñndern.   *)
    (* Am besten gleich éndern und zurÅckschreiben, da die   *)
    (* Dateipositionen nicht weit von den Stellen sind, die  *)
    (* fÅr den Zugriff verwendet werden.                     *)
    (*                                                       *)
    (* Ich kann nicht einfach die Anzahl von einem Datensatz *)
    (* in den anderen Åbernehmen, da eine Datei ja auch von  *)
    (* einem anderen DatentrÑger kommen kann.                *)


    Begin
(*      AVLInsert(FilePos(PfadDatafile),
                PfadDataTmp.Anzahl,
                NoDif,
                PfadDifTree,
                PDif,
                False)
*)    End
    Else

    (* Pfad in alten Daten nun grî·er -> Neue Pfad wird vor FilePos *)
    (* eingefÅgt.                                                     *)

    Begin
      AVLInsert(FilePos(PfadDatafile),
                PfadDataTmp.Anzahl,
                DifRein,
                PfadDifTree,
                PDif,
                False)
    End;


    Read(PfadTmp, PfadDataTmp)
  End;



  (* Der zweite Abschnitt stellt fest, welche Pfade Rausfallen... *)

  Reset(PfadTmp);
  Reset(PfadDataFile);


  Read(PfadTmp, PfadDataTmp);
  Read(PfadDataFile, PfadData);

  While Not Eof(PfadDataFile) do
  Begin

    While (Not Eof(PfadTmp)) and
          (PfadData.PfadName > PfadDataTmp.PfadName) do
      Read(PfadTmp, PfadDataTmp);

    If PfadData.PfadName = PfadDataTmp.PfadName then

    (* Pfad in neuen Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(PfadDatafile),
                PfadDataTmp.Anzahl,
                NoDif,
                PfadDifTree,
                PDif,
                False)
*)    End
    Else

    (* Pfad in neuen Daten nun grî·er -> Pfad wird vor FilePos *)
    (* entfernt.                                               *)

    Begin
      AVLInsert(FilePos(PfadDatafile),
                PfadDataTmp.Anzahl,
                DifRaus,
                PfadDifTree,
                PDif,
                False)
    End;


    Read(PfadDataFile, PfadData)
  End
End;

Procedure GetLabelDif;
Var LabelDataTmp,
    LabelData    : LabelDatatyp;
Begin

  (* Volgender Abschnitt stellt fest, welche Labelen eingefÅgt werden. *)
  (* Dies wird in einen AVL Baum geschrieben und spÑter verwendet      *)



  Read(LabelTmp, LabelDataTmp);
  Read(LabelDataFile, LabelData);

  While Not Eof(LabelTmp) do
  Begin

    While (Not Eof(LabelDataFile)) and
          (LabelData.LabelName < LabelDataTmp.LabelName) do
      Read(LabelDataFile, LabelData);

    (* Wenn LabelData.LabelName grî·er ist als LabelTmp.LabelName, *)
    (* ist die Label nicht vorhanden. LabelData darf nun nicht     *)
    (* Åberschrieben werden, da dieser Datensatz mit dem nÑchsten  *)
    (* LabelTmp - Datensatz verglichen wird.                       *)
    (* Es ist dann volgendes mîglich:                              *)
    (* LabelTmp.LabelName = LabelData.LabelName                    *)



    If LabelData.LabelName = LabelDataTmp.LabelName then

    (* Label in alten Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                NoDif,
                LabelDifTree,
                LDif,
                False)
*)    End
    Else

    (* Label in alten Daten nun grî·er -> Neue Label wird vor FilePos *)
    (* eingefÅgt.                                                     *)

    Begin
      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                DifRein,
                LabelDifTree,
                LDif,
                False)
    End;


    Read(LabelTmp, LabelDataTmp)
  End;



  (* Der zweite Abschnitt stellt fest, welche Labelen Rausfallen... *)

  Reset(LabelTmp);
  Reset(LabelDataFile);


  Read(LabelTmp, LabelDataTmp);
  Read(LabelDataFile, LabelData);

  While Not Eof(LabelDataFile) do
  Begin

    While (Not Eof(LabelTmp)) and
          (LabelData.LabelName > LabelDataTmp.LabelName) do
      Read(LabelTmp, LabelDataTmp);

    If LabelData.LabelName = LabelDataTmp.LabelName then

    (* Label in neuen Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                NoDif,
                LabelDifTree,
                LDif,
                False)
*)    End
    Else

    (* Label in neuen Daten nun grî·er -> Label wird vor FilePos *)
    (* entfernt.                                                 *)

    Begin
      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                DifRaus,
                LabelDifTree,
                LDif,
                False)
    End;


    Read(LabelDataFile, LabelData)
  End
End;

Procedure GetStrucktDif;
Var LabelDataTmp,
    LabelData    : LabelDatatyp;

    PfadDataTmp,
    PfadData     : PfadDatatyp;

    DateiDataTmp,
    DateiData    : DateiDatatyp;

    StrucktDataTmp,
    StrucktData  : StrucktDatatyp;


Begin

  (* Volgender Abschnitt stellt fest, welche Labelen eingefÅgt werden. *)
  (* Dies wird in einen AVL Baum geschrieben und spÑter verwendet      *)



  Read(LabelTmp, LabelDataTmp);
  Read(LabelDataFile, LabelData);

  While Not Eof(LabelTmp) do
  Begin

    While (Not Eof(LabelDataFile)) and
          (LabelData.LabelName < LabelDataTmp.LabelName) do
      Read(LabelDataFile, LabelData);

    (* Wenn LabelData.LabelName grî·er ist als LabelTmp.LabelName, *)
    (* ist die Label nicht vorhanden. LabelData darf nun nicht     *)
    (* Åberschrieben werden, da dieser Datensatz mit dem nÑchsten  *)
    (* LabelTmp - Datensatz verglichen wird.                       *)
    (* Es ist dann volgendes mîglich:                              *)
    (* LabelTmp.LabelName = LabelData.LabelName                    *)



    If LabelData.LabelName = LabelDataTmp.LabelName then

    (* Label in alten Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                NoDif,
                LabelDifTree,
                LDif,
                False)
*)    End
    Else

    (* Label in alten Daten nun grî·er -> Neue Label wird vor FilePos *)
    (* eingefÅgt.                                                     *)

    Begin
      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                DifRein,
                LabelDifTree,
                LDif,
                False)
    End;


    Read(LabelTmp, LabelDataTmp)
  End;



  (* Der zweite Abschnitt stellt fest, welche Labelen Rausfallen... *)

  Reset(LabelTmp);
  Reset(LabelDataFile);


  Read(LabelTmp, LabelDataTmp);
  Read(LabelDataFile, LabelData);

  While Not Eof(LabelDataFile) do
  Begin

    While (Not Eof(LabelTmp)) and
          (LabelData.LabelName > LabelDataTmp.LabelName) do
      Read(LabelTmp, LabelDataTmp);

    If LabelData.LabelName = LabelDataTmp.LabelName then

    (* Label in neuen Daten vorhanden -> Nur Anzahl Ñndern. *)

    Begin
(*      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                NoDif,
                LabelDifTree,
                LDif,
                False)
*)    End
    Else

    (* Label in neuen Daten nun grî·er -> Label wird vor FilePos *)
    (* entfernt.                                                 *)

    Begin
      AVLInsert(FilePos(LabelDatafile),
                LabelDataTmp.Anzahl,
                DifRaus,
                LabelDifTree,
                LDif,
                False)
    End;


    Read(LabelDataFile, LabelData)
  End
End;

Procedure GetHilfDif;
Begin
End;
Procedure GetZusatzDif;
Begin
End;



Procedure LoescheDiskBereich;
Begin
End;


Begin
  Wait('DDN noch nicht fertiggestellt.' + Chr(13) +
       'Diese Routine wird dann aufgerufen,' + Chr(13) +
       'wenn der Speicher in jedem Fall' + Chr(13) +
       'nicht mehr ausreicht um die Daten zu bearbeiten.' + Chr(13) + Chr(13) +
       'Die neuen Daten sind in den Dateien "*.TMP".');

  Exit;


  DDif := False;
  PDif := False;
  LDif := False;
  SDif := False;
  HDif := False;
  ZDif := False;


  DateiDifTree := Nil;
  PfadDifTree := Nil;
  LabelDifTree := Nil;
  StrucktDifTree := Nil;
  HilfDifTree := Nil;
  ZusatzDifTree := Nil;

  DateiTmpDifTree := Nil;
  PfadTmpDifTree := Nil;
  LabelTmpDifTree := Nil;
  StrucktTmpDifTree := Nil;
  HilfTmpDifTree := Nil;
  ZusatzTmpDifTree := Nil;


  InitFileNames('.TMP');
  InitTmpFiles;
  OpenAllTmpFiles;

  InitFileNames(FileName);
  InitFiles;
  OpenAllFiles;

  Wait('GetDateiDif:');
  GetDateiDif;
  Wait('GetPfadDif:');
  GetPfadDif;
  Wait('GetLabelDif:');
  GetLabelDif;

  CloseAllFiles;
  CloseAllTmpFiles


End;
End.
