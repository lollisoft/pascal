Unit bdf; (* Build Data Files *)
{$F+,O+,X+}

(*****************************************************)
(*                                                   *)
(* Wann wird diese Unit aufgerufen:                  *)
(* --------------------------------                  *)
(*                                                   *)
(* In der Prozedur VIDEO/DIR werden die Daten        *)
(* gespeichert wenn der Speicherplatz nicht mehr     *)
(* ausreicht um alle Daten des Verzeichnisses        *)
(* aufzunehmen.                                      *)
(*                                                   *)
(* Es Werden Dateigruppen mit dem Namen 'Puffer',    *)
(* eine fortlaufende Nummer und der Entsprechenden   *)
(* Erweiterung erzeugt.                              *)
(*                                                   *)
(* Es entsteht also eine getrennte Strucktliste und  *)
(* deren Daten. Es kînnen dadurch mehrere EintrÑge   *)
(* auftreten.                                        *)
(*                                                   *)
(* In der Variable MemWasLo wird festgehalten, ob    *)
(* zwischenzeitlich abgespeichert wurde.             *)
(*                                                   *)
(* Am Ende der Prozedur VIDEO/DIR wird dies geprÅft  *)
(* und die restlichen Daten im Falle auch            *)
(* gespeichert, danach wird BuildDataFiles aufger.   *)
(*                                                   *)
(* Was BuildDataFiles macht:                         *)
(* -------------------------                         *)
(*                                                   *)
(* Es transportiert alle Daten der Dateien ab der    *)
(* ZÑhlung 1 in die Datei ..0.*.                     *)
(*                                                   *)
(* Dabei mu· auf die Indizierung geachtet werden.    *)
(* Elemente, die in der Ziehldatei schon enthalten   *)
(* sind, werden nicht angefÅgt. Hier wird in dem     *)
(* Ziehlelement die Variable Anzahl erhîht.          *)
(*                                                   *)
(* Elemente, die nicht vorhanden sind, werden hinten *)
(* angehÑngt.                                        *)
(*                                                   *)
(* Es wird je ein Baum fÅr Label, Pfad und Datei -   *)
(* Datei erzeugt, in denen am Anfang je Baumelement  *)
(* ein Zeiger auf ein Dateielement steht.            *)
(*                                                   *)
(* Dadurch besteht eine Verbindung vom Baum zur      *)
(* Datei. Der Baum kann nun umsortiert werden, oder  *)
(* es kînnen Elemente am Ende der Datei angefÅgt     *)
(* werden. Ein Zeiger auf dieses Dateielement wird   *)
(* nun in dem Baum eingefÅgt, damit die Reihenvolge  *)
(* der EintrÑge der Datei im Baum Sortiert vorliegt. *)
(*                                                   *)
(* Auf diese Weise kann mit binÑrer Suche ein Elem.  *)
(* in der Datei Åber den Baum ausfindig gemacht      *)
(* werden.                                           *)
(* Ebenso kann ein Element einfach an die Datei      *)
(* angehÑngt werden.                                 *)
(*                                                   *)
(* Weiterhin kann der Baum hinterher herangezogen    *)
(* werden, um die datei richtig zu sortieren und     *)
(* danach die Indizierung wieder herzustellen.       *)
(*                                                   *)
(* Ist das Element in die Datei transportiert worden *)
(* kann die Indizierung des entsprechenden Struckt-  *)
(* element verÑndert werden.                         *)
(*                                                   *)
(* Bei einem Dateielement mu· gesondert vorgegangen  *)
(* werden, da hier noch auf ein Zusatzelement verw.  *)
(* wird!!                                            *)
(*                                                   *)
(* Ist das Dateielement in der Zieldatei schon vor-  *)
(* handen, so mu· das Zusatzelement gefunden werden. *)
(* Jetzt wird an der Stelle in der Zusatzdatei ein   *)
(* neues Element geschrieben. Dieses mu· aber schon  *)
(* den Zeiger in die Strucktdatei haben.             *)
(*                                                   *)
(* ( Wichtig: Es wird ein Differenzzeiger addiert! ) *)
(*                                                   *)
(* Wenn das Dateielement nicht enthalten ist, wird   *)
(* das Zusatzelement an das Ende der Zusatzzieldatei *)
(* angehÑngt. Es wird auch hier der Strucktzeiger    *)
(* verÑndert.                                        *)
(*                                                   *)
(* Dieser Ablauf widerholt sich mit allen anderen    *)
(* Dateigruppen.                                     *)
(*                                                   *)
(* Wenn alle Daten in der Zieldateigruppe unter-     *)
(* gebracht                                          *)
(*****************************************************)



Interface
Uses Overlay, Dialogs;

Const Counter : Integer = 0;
      MemWasLo: Boolean = False;


Type
    Importtyp           = String;

    ImportFiletyp       = File Of Importtyp;

    ImportDirTreePtrtyp = ^ImportDirTreetyp;
    ImportDirTreetyp    = Record
                            Rechts,
                            Links  : ImportDirTreePtrtyp;
                            Zeiger : Longint;
                            Bal    : ShortInt
                          End;



Type  TreePtrtyp = ^Treetyp;
      Treetyp    = Record
                     Rechts,
                     Links  : TreePtrtyp;
                     Zeiger,
                     Stelle : Longint;
                     Bal    : Shortint
                   End;


Var

      ImportFile   : ImportFiletyp;
      ImportData   : Importtyp;
      ImportDirTree: ImportDirTreePtrtyp;

      HelpTreesOnFile: Boolean;

      Df,
      Pf,
      Lf,
      Sf,
      Hf,
      Zf: String;

      LabelTree,
      PfadTree,
      DateiTree : TreePtrtyp;

      DateiDif,
      PfadDif,
      LabelDif  : Boolean;

      SArg: String;


Procedure BuildDataFiles(Counter: Integer);
Function Suche(Tree: ImportDirTreePtrtyp): ImportDirTreePtrtyp;
Function SearchPfadLine(Pfad: String): Longint;
Function GetDirElement(Stelle: Longint): String;

Implementation
Uses TvvtData, Crt, MsgBox;

Procedure Wait(txt : string);
begin
  MessageBox(txt, nil, mfOkButton)
end;

Function GetDirElement;
Var D: Importtyp;
BEGIN
  Seek(ImportFile, Stelle);
  Read(ImportFile, D);
  GetDirElement := D
END;


Function Suche;
Var Name: String;
Begin
  If Tree <> Nil then
  Begin
    Name := GetDirElement(Tree^.Zeiger);

    If Name = SArg then Suche := Tree
    Else
      If Name > SArg then
        Suche := Suche(Tree^.Links)
      Else
        If Name < SArg then
          Suche := Suche(Tree^.Rechts)
  End
  Else Suche := Nil
End;

Function SearchPfadLine;
Var P: Pointer;
Begin
  SArg := 'Verzeichnis von ' + Pfad;

  P := Suche(ImportDirTree);

  If P <> Nil then
    SearchPfadLine := ImportDirTreePtrtyp(P)^.Zeiger + 1
  Else
  Begin
    Wait('Suche in Importdatei ging in die Hose!');
    Halt
  End
End;



(***********************************)

Function LGet(Stelle: Longint): String;
Var L: LabelDatatyp;
BEGIN
  Seek(LabelDataPufferFile, Stelle);
  Read(LabelDataPufferFile, L);
  LGet := L.LabelName
END;

Function PGet(Stelle: Longint): String;
Var P: PfadDatatyp;
BEGIN
  Seek(PfadDataPufferFile, Stelle);
  Read(PfadDataPufferFile, P);
  PGet := P.PfadName
END;

Function DGet(Stelle: Longint): String;
Var D: DateiDatatyp;
BEGIN
  Seek(DateiDataPufferFile, Stelle);
  Read(DateiDataPufferFile, D);
  DGet := D.DateiName
END;

Type GetDateiElementtyp = Function(Stelle: Longint): String;

Var  GetDateiElement : GetDateiElementtyp;

Function GetElement(Stelle: Longint): String;
Begin
  GetElement := GetDateiElement(Stelle)
End;

(************************************)

Const DVZaehler: Integer = 0;
      DZaehler : Integer = 0;

Var ErrCount    : Integer;
    PfadNF      : Integer;

Procedure PruefeDateiDaten(LabelStelle, PfadStelle, DateiStelle: Longint);
Var StrucktPos,
    LabelPos,
    PfadPos,
    DateiPos     : Longint;
    SD           : StrucktDatatyp;
    DateiData    : DateiDatatyp;
    PfadData     : PfadDatatyp;
    LabelData    : LabelDatatyp;
    DName        : String12;
    DateiVorh    : Boolean;
    Ready        : Boolean;
    PfadFound    : Boolean;
    PfadPos1     : Longint;
Begin
  PfadFound := False;
  Ready := False;
  DateiVorh := False;
  Inc(DZaehler);
  (***************************************)
  (* Hole Daten fÅr LabelStelle - 1,     *)
  (* PfadStelle - 1 und DateiStelle - 1: *)
  (***************************************)

  StrucktPos := FilePos(StrucktDataPufferFile);
  Seek(StrucktDataPufferFile, LabelStelle - 1);
  Read(StrucktDataPufferFile, SD);

  LabelPos := FilePos(LabelDataPufferFile);
  Seek(LabelDataPufferFile, SD.DLabelList - 1);
  Read(LabelDataPufferFile, LabelData);

  Seek(StrucktDataPufferFile, PfadStelle - 1);
  Read(StrucktDataPufferFile, SD);

  PfadPos := FilePos(PfadDataPufferFile);
  Seek(PfadDataPufferFile, SD.DPfadList - 1);
  Read(PfadDataPufferFile, PfadData);

  DateiPos := FilePos(DateiDataPufferFile);
  Seek(DateiDataPufferFile, DateiStelle - 1);
  Read(DateiDataPufferFile, DateiData);

  (***************************************)
  (* Suche Pfad in IMP.TMP und dann die  *)
  (* Datei in dessen Bereich:            *)
  (***************************************)

  While (Not Eof(ImportFile)) and
        (Not Ready) do
  Begin
    If PfadData.PfadName[Length(PfadData.PfadName)] = '\' then
      System.Delete(PfadData.PfadName, Length(PfadData.PfadName), 1);

    Seek(ImportFile, SearchPfadLine(PfadData.PfadName) - 1);

    Read(ImportFile, ImportData);

    ImportData := Copy(ImportData, 17, Length(ImportData) - 16);

    If PfadData.PfadName = ImportData then
    Begin
      PfadFound := True;
      While Not Ready do
      Begin
        Read(ImportFile, ImportData);
        DName := '';

        If Pos('Verzeichnis', ImportData) <> 0 then
          Ready := True;


        If (Pos('Byte', ImportData) = 0) and
           (ImportData <> '') then
          If Pos('<DIR>', ImportData) = 0 then
          Begin
            DName := ImportData;

            While DName[Length(DName)] = ' ' do System.Delete(DName, Length(DName), 1);
            If Length(DName) > 8 then DName[9] := '.';
            While Pos(' ', DName) <> 0 do System.Delete(DName, Pos(' ', DName), 1);

            If Pos(DateiData.DateiName, DName) <> 0 then
            Begin
              DateiVorh := True;
              Ready := True;
              Inc(DVZaehler);
            End
          End
      End;
      Ready := True
    End;
    Ready := True
  End;


  If Not DateiVorh then
  Begin
    Inc(ErrCount);
    Writexy(1, 4, 'Fehler! (' + GetString(ErrCount) + ')')
  End;
  If Not PfadFound then
  Begin
    Inc(PfadNF);
    Writexy(1, 5, 'Pfad nicht gefunden! (' + GetString(PfadNF) + ')')
  End;

  Seek(StrucktDataPufferFile, StrucktPos);
  Seek(DateiDataPufferFile, DateiPos);
  Seek(PfadDataPufferFile, PfadPos);
  Seek(LabelDataPufferFile, LabelPos)
End;


Procedure BuildBaum(Var Lauf: TreePtrtyp;
                        Ziel: Longint;
                     Var Dif: Boolean);
var p1, p2 : TreePtrtyp;
begin
  if Lauf = Nil then
  begin
    new(lauf);
    dif := true;
    with lauf^ do
    begin
      Zeiger := Ziel;
      Links := Nil;
      Rechts := Nil;
      Bal := 0;
    end
  end

  else

  if GetElement(Ziel) < GetElement(Lauf^.Zeiger) then
  begin
    BuildBaum(Lauf^.Links, Ziel, Dif);

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

  if GetElement(Ziel) > GetElement(Lauf^.Zeiger) then
  begin
    BuildBaum(lauf^.rechts, Ziel, Dif);

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


Procedure OpenPufferFiles;
BEGIN
  ZusatzDataDateiOeffnen(ZusatzDataPufferfile);
  DateiDataDateiOeffnen(DateiDataPufferfile);
  PfadDataDateiOeffnen(PfadDataPufferfile);
  LabelDataDateiOeffnen(LabelDataPufferfile);
  HilfDataDateiOeffnen(HilfDataPufferfile);
  StrucktDataDateiOeffnen(StrucktDataPufferfile)
END;



Procedure BuildDataFiles;

Var LabelData : LabelDatatyp;
    PfadData  : PfadDatatyp;
    DateiData : DateiDatatyp;
    ZusatzData: ZusatzDatatyp;
    Result    : TreePtrtyp;
Const StrucktDif: Longint = 0;


Procedure InitPufferFiles;
BEGIN
  (* öber ...Filename und einer angefÅgten Nummerrierung. *)
  (* Wobei andere Dateivariablen verwendet werden.        *)

  assign(StrucktDataPufferfile,Strucktfilename);
  assign(HilfDataPufferfile,Hilffilename);
  assign(DateiDataPufferfile,Dateifilename);
  assign(LabelDataPufferfile,Labelfilename);
  assign(PfadDataPufferfile,Pfadfilename);
  assign(ZusatzDataPufferfile,Zusatzfilename);
END;

Var SArg: String;

Function Suche(Tree: TreePtrtyp): TreePtrtyp;
Var Name: String;
Begin
  If Tree <> Nil then
  Begin
    Name := GetElement(Tree^.Zeiger);

    If Name = SArg then Suche := Tree
    Else
      If Name > SArg then
        Suche := Suche(Tree^.Links)
      Else
        If Name < SArg then
          Suche := Suche(Tree^.Rechts)
  End
  Else Suche := Nil
End;

Var LogWrite    : Boolean;
    LogFile     : Text;



Function SucheMod(Tree: TreePtrtyp; Arg: Longint; Max: Longint): TreePtrtyp;
Begin
  If Tree <> Nil then
  Begin
    If LogWrite then
    Begin
      Writeln(LogFile, 'Arg: ' + GetString(Arg));
      Flush(LogFile);
      Writeln(LogFile, 'Ze : ' + GetString(Tree^.Zeiger));
      Flush(LogFile);
      Writeln(LogFile, 'St : ' + GetString(Tree^.Stelle));
      Flush(LogFile)
    End;

    If (Tree^.Zeiger >= Max) or
       (Tree^.Zeiger <0) then
    Begin
      Wait('Stelle auf Datei in Baum (Zeiger) zu gro·: ' + Chr(13) +
           GetString(Tree^.Zeiger));
      Halt
    End;
    If Tree^.Zeiger = Arg then SucheMod := Tree
    Else
      If Tree^.Zeiger > Arg then
      Begin
        If LogWrite then
        Begin
          Writeln(LogFile, 'G l: ');
          Flush(LogFile)
        End;

        SucheMod := SucheMod(Tree^.Links, Arg, Max)
      End
      Else
        If Tree^.Zeiger < Arg then
        Begin
          If LogWrite then
          Begin
            Writeln(LogFile, 'G r: ');
            Flush(LogFile)
          End;
          SucheMod := SucheMod(Tree^.Rechts, Arg, Max)
        End
  End
  Else SucheMod := Nil
End;

Procedure PruefeDaten(LabelName, PfadName, DateiName: String);
Var StrucktPos,
    LabelPos,
    PfadPos,
    DateiPos  : Longint;
    SD           : StrucktDatatyp;
    DateiData    : DateiDatatyp;
    PfadData     : PfadDatatyp;
    LabelData    : LabelDatatyp;
    DName        : String12;
    DateiVorh    : Boolean;
    Ready        : Boolean;
    PfadFound    : Boolean;
    PfadPos1      : Longint;
Begin
  PfadFound := False;
  DateiVorh := False;
  Ready := False;
  (***************************************)
  (* Suche Pfad in IMP.TMP und dann die  *)
  (* Datei in dessen Bereich:            *)
  (***************************************)

  If PfadName[Length(PfadName)] = '\' then System.Delete(PfadName, Length(PfadName), 1);

  While (Not Eof(ImportFile)) and
        (Not Ready) do
  Begin
    Seek(ImportFile, SearchPfadLine(PfadName) - 1);

    Read(ImportFile, ImportData);

    ImportData := Copy(ImportData, 17, Length(ImportData) - 16);

    If PfadName = ImportData then
    Begin
      PfadFound := True;
      While Not Ready do
      Begin

        Read(ImportFile, ImportData);
        DName := '';

        If Pos('Verzeichnis', ImportData) <> 0 then
          Ready := True;

        If (Pos('Byte', ImportData) = 0) and
           (ImportData <> '') then
          If Pos('<DIR>', ImportData) = 0 then
          Begin
            DName := ImportData;

            While DName[Length(DName)] = ' ' do System.Delete(DName, Length(DName), 1);
            If Length(DName) > 8 then DName[9] := '.';
            While Pos(' ', DName) <> 0 do System.Delete(DName, Pos(' ', DName), 1);

            If Pos(DateiName, DName) <> 0 then
            Begin
              DateiVorh := True;
              Ready := True
            End
          End
      End;
      Ready := True
    End;
    Ready := True
  End;
  If Not DateiVorh then
  Begin
    Inc(ErrCount);
    Writexy(1, 4, 'Fehler! (' + GetString(ErrCount) + ') ' + DateiName)
  End;
  If Not PfadFound then
  Begin
    Inc(PfadNF);
    Writexy(1, 5, 'Pfad nicht gefunden! (' + GetString(PfadNF) + ')')
  End
End;




Procedure MoveLabel;
Begin
  Seek(LabelDataFile, StrucktData.DLabelList - 1);
  Read(LabelDataFile, LabelData);

  GetDateiElement := LGet;
  SArg := LabelData.LabelName;
  Result := Suche(LabelTree);

  If Result <> Nil then
  Begin
    (* Strucktelement in Datei anpassen: *)

    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DLabelList := Result^.Zeiger + 1;
    Write(StrucktDataFile, StrucktData);

    (* Labelelement anpassen: *)

    Seek(LabelDataPufferFile, Result^.Zeiger);
    Read(LabelDataPufferFile, LabelData);
    Seek(LabelDataPufferFile, Result^.Zeiger);
    Inc(LabelData.Anzahl);
    Write(LabelDataPufferFile, LabelData)
  End
  Else
  Begin
    (* Strucktelement in Datei anpassen: *)

    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DLabelList := FileSize(LabelDataPufferFile) + 1;
    Write(StrucktDataFile, StrucktData);

    (* Labelelement anpassen: *)

    Seek(LabelDataPufferFile, FileSize(LabelDataPufferFile));
    LabelData.Anzahl := 1;
    Write(LabelDataPufferFile, LabelData);

    SArg := LabelData.LabelName;
    GetDateiElement := LGet;
    BuildBaum(LabelTree, FilePos(LabelDataPufferFile) - 1, LabelDif)
  End;
End;


Procedure MovePfad;
Begin
  If StrucktData.DPfadList - 1 >= FileSize(PfadDataFile) then
  Begin
    Wait('Pfadzeiger zu gro·!');
    Halt
  End;
  Seek(PfadDataFile, StrucktData.DPfadList - 1);
  Read(PfadDataFile, PfadData);

  GetDateiElement := PGet;
  SArg := PfadData.PfadName;
  Result := Suche(PfadTree);

  If Result <> Nil then
  Begin
    (* Strucktelement in Datei anpassen: *)
    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DPfadList := Result^.Zeiger + 1;
    Write(StrucktDataFile, StrucktData);

    Seek(PfadDataPufferFile, Result^.Zeiger(* - 1 *));
    Read(PfadDataPufferFile, PfadData);
    Seek(PfadDataPufferFile, Result^.Zeiger(* - 1 *));
    Inc(PfadData.Anzahl);
    Write(PfadDataPufferFile, PfadData)
  End
  Else
  Begin
    (* Strucktelement in Datei anpassen: *)
    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DPfadList := FileSize(PfadDataPufferFile) + 1;
    Write(StrucktDataFile, StrucktData);

    (* Labelelement anpassen: *)

    Seek(PfadDataPufferFile, FileSize(PfadDataPufferFile));
    PfadData.Anzahl := 1;
    Write(PfadDataPufferFile, PfadData);

    SArg := PfadData.PfadName;
    GetDateiElement := PGet;
    BuildBaum(PfadTree, FilePos(PfadDataPufferFile) - 1, PfadDif)
  End;
End;

Var ZusatzLoadet: Boolean;
    Lauf        : ZusatzListPtrtyp;
    Help        : ZusatzListPtrtyp;
    B           : Longint;
Procedure MoveDatei;
Var Z,
    Z1,
    Z2,
    Z3,
    Z4: String;
    FP: Longint;
Procedure PruefeDatei;
Begin
  Str(StrucktData.DDateiList, Z);
  Str(FileSize(DateiDataFile), Z1);
  Str(FilePos(StrucktDataFile), Z2);
  Str(FileSize(StrucktDataFile), Z3);

  If StrucktData.DDateiList - 1 >= FileSize(DateiDataFile) then
  Begin
    Wait('Dateizeiger zu gro·!' + Chr(13) +
         'DDateiList   : ' + Z + Chr(13) +
         'DateiDataFile: ' + Z1 + Chr(13) +
         'Strucktstelle: ' + Z2 + Chr(13) +
         'StrucktDatei : ' + Z3);
    Halt
  End;
End;

Begin
  PruefeDatei;
  Seek(DateiDataFile, StrucktData.DDateiList - 1);
  Read(DateiDataFile, DateiData);

  GetDateiElement := DGet;
  SArg := DateiData.DateiName;
  Result := Suche(DateiTree);

  If Result <> Nil then
  Begin
    (* Strucktelement in Datei anpassen: *)

    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DDateiList := Result^.Zeiger + 1;
    Write(StrucktDataFile, StrucktData);

    (* Dateielement anpassen: *)

    If Result^.Zeiger >= FileSize(DateiDataPufferFile) then
    Begin
      Wait('Move Datei: Zeiger auf Dateidatapufferdatei zu gro·!');
      Halt
    End;

    Seek(DateiDataPufferFile, Result^.Zeiger (* - 1 *) );
    Read(DateiDataPufferFile, DateiData);
    Seek(DateiDataPufferFile, Result^.Zeiger (* - 1 *) );
    Inc(DateiData.Anzahl);
    Write(DateiDataPufferFile, DateiData)
  End
  Else
  Begin
    (* Strucktelement in Datei anpassen: *)

    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DDateiList := FileSize(DateiDataPufferFile) + 1;
    Write(StrucktDataFile, StrucktData);

    (* Dateielement anpassen: *)

    Seek(DateiDataPufferFile, FileSize(DateiDataPufferFile));
    DateiData.Anzahl := 1;
    DateiData.Loc := Nil;
    Write(DateiDataPufferFile, DateiData);

    SArg := DateiData.DateiName;
    GetDateiElement := DGet;
    BuildBaum(DateiTree, FilePos(DateiDataPufferFile) - 1, DateiDif);
  End;

End; (* MoveDatei *)


Var Hilf : TreePtrtyp;
    Liste: TreePtrtyp;
{
Procedure TreeToList(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    TreeToList(Tree^.Links);

    If Liste = Nil then
    Begin
      Liste := Tree;
      Hilf := Tree;
      Liste^.Links := Nil
    End
    Else
    Begin
      Hilf^.Rechts := Tree;
      Hilf := Hilf^.Rechts
    End;

    TreeToList(Tree^.Rechts)
  End
End;
}
Procedure NumeriereBaum(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    NumeriereBaum(Tree^.Links);

    Inc(B);
    Tree^.Stelle := B;

    NumeriereBaum(Tree^.Rechts)
  End
End;

Var LDPF: LabelFiletyp;
    PDPF: PfadFiletyp;
    DDPF: DateiFiletyp;



Procedure OpenPFiles;
BEGIN
  DateiDataDateiOeffnen(DDPF);
  PfadDataDateiOeffnen(PDPF);
  LabelDataDateiOeffnen(LDPF);
END;



Procedure ReOrgLabel(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    ReOrgLabel(Tree^.Links);

    Seek(LabelDataPufferFile, Tree^.Zeiger);
    Read(LabelDataPufferFile, LabelData);
    Write(LDPF, LabelData);

    ReOrgLabel(Tree^.Rechts)
  End
End;

Procedure ReOrgPfad(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    ReOrgPfad(Tree^.Links);

    Seek(PfadDataPufferFile, Tree^.Zeiger);
    Read(PfadDataPufferFile, PfadData);
    Write(PDPF, PfadData);

    ReOrgPfad(Tree^.Rechts)
  End
End;

Procedure ReOrgDatei(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    ReOrgDatei(Tree^.Links);

    Seek(DateiDataPufferFile, Tree^.Zeiger);
    Read(DateiDataPufferFile, DateiData);
    DateiData.Loc := Nil;
    Write(DDPF, DateiData);

    ReOrgDatei(Tree^.Rechts)
  End
End;

Type TreeFiletyp = File of Treetyp;
Var
     LabelTreeFile,
     PfadTreeFile,
     DateiTreeFile : TreeFiletyp;

Procedure TreeSave(Tree:TreePtrtyp; Var TreeFile: TreeFiletyp);
Begin
  If Tree <> Nil then
  Begin
    TreeSave(Tree^.Links, TreeFile);
    Write(TreeFile, Tree^);
    TreeSave(Tree^.Rechts, TreeFile);
    Dispose(Tree)
  End;
End;

Procedure SaveHelpTrees;
Begin
  Assign(LabelTreeFile, 'Label.tre');
  {$I-}
  Reset(LabelTreeFile);
  If IOResult = 0 then Erase(LabelTreeFile);

  Rewrite(LabelTreeFile);
  If IOResult <> 0 then
  Begin
    Wait('Fehler beim auslagern der Hilfsdaten!');
    Halt
  End;
  {$I+}
  TreeSave(LabelTree, LabelTreeFile);
  LabelTree := Nil;

  Assign(PfadTreeFile, 'Pfad.tre');
  {$I-}
  Reset(PfadTreeFile);
  If IOResult = 0 then Erase(PfadTreeFile);

  Rewrite(PfadTreeFile);
  If IOResult <> 0 then
  Begin
    Wait('Fehler beim auslagern der Hilfsdaten!');
    Halt
  End;
  {$I+}
  TreeSave(PfadTree, PfadTreeFile);
  PfadTree := Nil;

  Assign(DateiTreeFile, 'Datei.tre');
  {$I-}
  Reset(DateiTreeFile);
  If IOResult = 0 then Erase(DateiTreeFile);

  Rewrite(DateiTreeFile);
  If IOResult <> 0 then
  Begin
    Wait('Fehler beim auslagern der Hilfsdaten!');
    Halt
  End;
  {$I+}
  TreeSave(DateiTree, DateiTreeFile);
  DateiTree := Nil;

  HelpTreesOnFile := True
End;

Procedure TreeTraverse(Tree: TreePtrtyp);
Begin
  If Tree <> Nil then
  Begin
    TreeTraverse(Tree^.Links);

    Writeln(LogFile, 'Zeiger: ' + GetString(Tree^.Zeiger));
    Flush(LogFile);
    Writeln(LogFile, 'Stelle: ' + GetString(Tree^.Stelle));
    Flush(LogFile);
    Writeln(LogFile);
    Flush(LogFile);

    TreeTraverse(Tree^.Rechts)
  End
End;


Var Dif    : Boolean;
    NewTree: TreePtrtyp;

Procedure TreeReSort(Tree: TreePtrtyp);

Procedure ReBuildBaum(Var Lauf: TreePtrtyp;
                          Data: TreePtrtyp;
                       Var Dif: Boolean);
var p1, p2 : TreePtrtyp;
begin
  if Lauf = Nil then
  begin
    Lauf := Data;
    dif := true;
    with lauf^ do
    begin
      Links := Nil;
      Rechts := Nil;
      Bal := 0;
    end
  end

  else

  if Data^.Zeiger < Lauf^.Zeiger then
  begin
    ReBuildBaum(Lauf^.Links, Data, Dif);

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

  if Data^.Zeiger > Lauf^.Zeiger then
  begin
    ReBuildBaum(lauf^.rechts, Data, Dif);

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


Begin
  If Tree <> Nil then
  Begin
    TreeReSort(Tree^.Links);

    New(Hilf);
    Hilf^ := Tree^;
    ReBuildBaum(NewTree, Hilf, Dif);

    TreeReSort(Tree^.Rechts);

    Dispose(Tree)
  End
End;

Type PDateiDatatyp = ^DateiDatatyp;

Procedure GetDElement(Zeiger: Longint; Var DD: DateiDatatyp);
Var P: PDateiDatatyp;
Begin
  Seek(DateiDataPufferFile, Zeiger - 1);
  Read(DateiDataPufferFile, DD);
End;

Procedure PutDElement(DPP: PDateiDatatyp; Zeiger: Longint);
Begin
  Seek(DateiDataPufferFile, Zeiger - 1);
  Write(DateiDataPufferFile, DPP^)
End;


Procedure SpeichereSuchDatei(LP, PP, DP: Longint);
var Help    : ZusatzListPtrtyp;
    DPP     : PDateiDatatyp;
    Position: Longint;
    SD      : StrucktDatatyp;
    Err     : Boolean;
    DD      : DateiDatatyp;
begin
  If (System.MemAvail < 1024) and
     Not HelpTreesOnFile then   SaveHelpTrees;
  Err := False;
  New(DPP);
  GetDElement(DP, DPP^);

  (*******************************)
  (* PrÅfe IntegritÑt der Daten: *)
  (*******************************)


  Position := FilePos(StrucktDataPufferFile);

  Seek(StrucktDataPufferFile, LP - 1);
  Read(StrucktDataPufferFile, SD);
  If SD.Art <> 4 then
  Begin
    Wait('LP - Zeiger von Zusatz auf Struckt nicht in ordnung!' + Chr(13) +
         GetString(SD.Art));
    Err := True
  End;

  Seek(StrucktDataPufferFile, PP - 1);
  Read(StrucktDataPufferFile, SD);
  If SD.Art <> 5 then
  Begin
    Wait('PP - Zeiger von Zusatz auf Struckt nicht in ordnung!' + Chr(13) +
         GetString(SD.Art));
    Err := True
  End;

  Read(StrucktDataPufferFile, SD);
  Seek(DateiDataPufferFile, SD.DDateiList - 1);
  Read(DateiDataPufferFile, DD);
  While (DD.DateiName <> DPP^.DateiName) and
        (SD.Art = 6) and
        Not Eof(StrucktDataPufferFile) do
  Begin
    Read(StrucktDataPufferFile, SD);
    Seek(DateiDataPufferFile, SD.DDateiList - 1);
    Read(DateiDataPufferFile, DD);
  End;

  If DD.DateiName <> DPP^.DateiName then Inc(ErrCount);

  If Err then Inc(ErrCount);

  Seek(StrucktDataPufferFile, Position);

  (***********************************)

  If DPP^.Loc = Nil then
  begin
    (* ôffnen und zuweisen: *)
    new(Help);
    Help^.ZusatzData.sl := Pointer(lp);
    Help^.ZusatzData.sp := Pointer(pp);

    (* Verbinden mit DateiElement: *)
    DPP^.Loc := Help;
    PutDElement(DPP, DP);

    (* An ZusatzListe vorne anhÑngen: *)

    Help^.Next := ZusatzList;
    Help^.Back := Nil;

    if ZusatzList <> Nil then ZusatzList^.Back := Help
                          else ZusatzListend := Help;

    ZusatzList := Help
  end
  else
  begin
    (* ôffnen und zuweisen: *)
    new(Help);
    Help^.ZusatzData.sl := Pointer(lp);
    Help^.ZusatzData.sp := Pointer(pp);

    (* Hinter anderem Element einfÅgen: *)


    Help^.Back := dpp^.Loc;
    Help^.Next := dpp^.Loc^.Next;

    (* Wenn es das letzte Element ist, dann: *)

    if dpp^.Loc^.Next = Nil then
    Begin
      dpp^.Loc^.Next := Help;
      ZusatzListend := dpp^.Loc;
    End
    else
    begin
      dpp^.Loc^.Next^.Back := Help;
      dpp^.Loc^.Next := Help
    end

  end;
  Dispose(DPP)
end;


Var DateiStelle,
    LabelStelle,
    PfadStelle,
    Find,
    Find1,
    Zaehler       : Longint;
    SD1,
    SD2           : StrucktDatatyp;

Procedure OpenLogFile;
begin
  (*$I-*)
  rewrite(LogFile);
  if IOResult <> 0 then
  begin
    Wait('Log - Datei kann nicht geîffnet werden !');
    Halt
  end
  (*$I+*)
end;

Begin
  HelpTreesOnFile := False;

  Find := 0;
  Find1 := 0;
  Df := Dateifilename;
  Pf := Pfadfilename;
  Lf := Labelfilename;
  Sf := Strucktfilename;
  Hf := Hilffilename;
  Zf := Zusatzfilename;

  Dateifilename   := 'Puffer' + '0' + '.DVD';
  Pfadfilename    := 'Puffer' + '0' + '.DVP';
  Labelfilename   := 'Puffer' + '0' + '.DVL';
  Strucktfilename := 'Puffer' + '0' + '.DVS';
  Hilffilename    := 'Puffer' + '0' + '.DVH';
  Zusatzfilename  := 'Puffer' + '0' + '.DVZ';

  InitPufferFiles;
  OpenPufferFiles;

  DateiDif := False;
  PfadDif := False;
  LabelDif := False;

  DateiTree := Nil;
  PfadTree := Nil;
  LabelTree := Nil;

  (*****************************************)
  (* Herstellen der HilfsbÑume:            *)
  (*****************************************)
  DateiStelle := 0;

  GetDateiElement := DGet;
  While Not Eof(DateiDataPufferFile) do
  Begin
    Read(DateiDataPufferFile, DateiData);
    BuildBaum(DateiTree, DateiStelle, DateiDif);
    Inc(DateiStelle);
    Seek(DateiDataPufferFile, DateiStelle)
  End;

  DateiStelle := 0;

  GetDateiElement := PGet;
  While Not Eof(PfadDataPufferFile) do
  Begin
    Read(PfadDataPufferFile, PfadData);
    BuildBaum(PfadTree, DateiStelle, PfadDif);
    Inc(DateiStelle);
    Seek(PfadDataPufferFile, DateiStelle)
  End;

  DateiStelle := 0;

  GetDateiElement := LGet;
  While Not Eof(LabelDataPufferFile) do
  Begin
    Read(LabelDataPufferFile, LabelData);
    BuildBaum(LabelTree, DateiStelle, LabelDif);
    Inc(DateiStelle);
    Seek(LabelDataPufferFile, DateiStelle)
  End;

  Seek(DateiDataPufferFile, 0);
  Seek(PfadDataPufferFile, 0);
  Seek(LabelDataPufferFile, 0);

  B := 0;
  NumeriereBaum(DateiTree);
  If B <> FileSize(DateiDataPufferFile) then
    Wait('Anzahl von Dateitree beim Erstaufbau stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(DateiDataPufferFile)));
  B := 0;
  NumeriereBaum(PfadTree);
  If B <> FileSize(PfadDataPufferFile) then
    Wait('Anzahl von Pfadtree beim Erstaufbau stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(PfadDataPufferFile)));

  B := 0;
  NumeriereBaum(LabelTree);
  If B <> FileSize(LabelDataPufferFile) then
    Wait('Anzahl von Labeltree beim Erstaufbau stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(LabelDataPufferFile)));




(*  ErrCount := 0;
  PfadNF := 0;
  Zaehler := 0;
  Seek(ImportFile, 0);
  While Not Eof(StrucktDataPufferFile) do
  Begin
    Read(StrucktDataPufferFile, StrucktData);
    Case StrucktData.Art of
     4 : Begin
           Seek(LabelDataPufferFile, StrucktData.DLabelList - 1);
           Read(LabelDataPufferFile, LabelData)
         End;
     5 : Begin
           Seek(PfadDataPufferFile, StrucktData.DPfadList - 1);
           Read(PfadDataPufferFile, PfadData)
         End;
     6 : Begin
           Inc(Zaehler);
           Seek(DateiDataPufferFile, StrucktData.DDateiList - 1);
           Read(DateiDataPufferFile, DateiData);
           Writexy(1, 2, 'PrÅfe  : ' + GetString(Zaehler));
           PruefeDaten(LabelData.LabelName,
                       PfadData.PfadName,
                       DateiData.DateiName);
           Writexy(1, 3, 'GeprÅft: ' + GetString(Zaehler));
         End
      Else Wait('Element in Struckt nicht 4 - 6!');
    End
  End;

  ErrCount := 0;

  Seek(StrucktDataPufferFile, 0);
  *)

  For A := 1 to Counter do
  Begin
    Dateifilename   := 'Puffer' + GetString(A) + '.DVD';
    Pfadfilename    := 'Puffer' + GetString(A) + '.DVP';
    Labelfilename   := 'Puffer' + GetString(A) + '.DVL';
    Strucktfilename := 'Puffer' + GetString(A) + '.DVS';
    Hilffilename    := 'Puffer' + GetString(A) + '.DVH';
    Zusatzfilename  := 'Puffer' + GetString(A) + '.DVZ';

    InitFiles;
    OpenAllFiles;

  (*  Wait('PrÅfe Quelldateien');

    ErrCount := 0;
    PfadNF := 0;
    Zaehler := 0;
    Seek(ImportFile, 0);
    While Not Eof(StrucktDataFile) do
    Begin
      Read(StrucktDataFile, StrucktData);
      Case StrucktData.Art of
       4 : Begin
             Seek(LabelDataFile, StrucktData.DLabelList - 1);
             Read(LabelDataFile, LabelData)
           End;
       5 : Begin
             Seek(PfadDataFile, StrucktData.DPfadList - 1);
             Read(PfadDataFile, PfadData)
           End;
       6 : Begin
             Inc(Zaehler);
             Seek(DateiDataFile, StrucktData.DDateiList - 1);
             Read(DateiDataFile, DateiData);
             Writexy(1, 2, 'PrÅfe  : ' + GetString(Zaehler));
             PruefeDaten(LabelData.LabelName,
                         PfadData.PfadName,
                         DateiData.DateiName);
             Writexy(1, 3, 'GeprÅft: ' + GetString(Zaehler));
           End
        Else Wait('Element in Struckt nicht 4 - 6!');
      End
    End;
    Wait('Quelldateien geprÅft, ErrCount: ' + GetString(ErrCount));
    ErrCount := 0;

    Seek(StrucktDataFile, 0);
    *)


    (* Daten, die in den 'InitFiles' Dateien enthalten sind, *)
    (* werden in die 'InitPufferFiles' Dateien geschrieben.  *)
    (* Dabei werden die Querverbindungen von Struckt nach .. *)
    (* verschoben. Ebenso wird die Zusatzdatei bei MoveDatei *)
    (* nachtrÑglich behandelt.                               *)
    (* Es bleibt viel Speicherplatz Åbrig, da nur wenige     *)
    (* Elemente im Speicher gehalten werden.                 *)



    While Not(Eof(StrucktDataFile)) do
    Begin
      Read(StrucktDataFile, StrucktData);
      Case StrucktData.Art of
        4 : MoveLabel;
        5 : MovePfad;
        6 : MoveDatei;
        Else Wait('ZusammenfÅgen der LPD-Daten: Element nicht 4 - 6!');
      End
    End;

    (* Wird benîtigt, um die Querverbindungen von Hilf nach Struckt *)
    (* wieder aufzubauen:                                           *)

    StrucktDif := StrucktDif + FileSize(StrucktDataPufferFile);

    (* Die mit InitFiles gesetzten Struckt- und Hilfdateien werden *)
    (* an die jeweiligen Pufferdateien angehÑngt.                  *)

    Seek(StrucktDataPufferFile, FileSize(StrucktDataPufferFile));
    Seek(HilfDataPufferFile, FileSize(HilfDataPufferFile));
    Seek(StrucktDataFile, 0);
    Seek(HilfDataFile, 0);

    While Not(Eof(StrucktDataFile)) do
    Begin
      Read(StrucktDataFile, StrucktData);
      Write(StrucktDataPufferFile, StrucktData)
    End;

    While Not(Eof(HilfDataFile)) do
    Begin
      Read(HilfDataFile, HilfData);

      (* HilfData.DLabelList gleich um StrucktDif erhîhen. *)

      Inc(HilfData.DLabelList, StrucktDif);

      (* Und an HilfDataPufferFile anhÑngen: *)

      Write(HilfDataPufferFile, HilfData)
    End;

    CloseAllFiles;
  End;

  B := 0;
  NumeriereBaum(DateiTree);
  If B <> FileSize(DateiDataPufferFile) then
    Wait('Anzahl von Dateitree stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(DateiDataPufferFile)));
  B := 0;
  NumeriereBaum(PfadTree);
  If B <> FileSize(PfadDataPufferFile) then
    Wait('Anzahl von Pfadtree stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(PfadDataPufferFile)));
  B := 0;
  NumeriereBaum(LabelTree);
  If B <> FileSize(LabelDataPufferFile) then
    Wait('Anzahl von Labeltree stimmt nicht mit Datei Åberein!' + Chr(13) +
         'Tree : ' + GetString(B) + Chr(13) +
         'Datei: ' + GetString(FileSize(LabelDataPufferFile)));


  Close(DateiDataPufferFile);
  Close(PfadDataPufferFile);
  Close(LabelDataPufferFile);

  DateiDataDateiOeffnen(DateiDataPufferfile);
  PfadDataDateiOeffnen(PfadDataPufferfile);
  LabelDataDateiOeffnen(LabelDataPufferfile);


  Assign(DDPF, 'D1.DVD');
  Assign(PDPF, 'D1.DVP');
  Assign(LDPF, 'D1.DVL');

  OpenPFiles;

  ErrCount := 0;
  PfadNF := 0;
(*  Seek(ImportFile, 0);

  Wait('PrÅfe Daten vor Reorg');

  Seek(StrucktDataPufferFile, 0);
  Zaehler := 0;
  While Not Eof(StrucktDataPufferFile) do
  Begin
    Read(StrucktDataPufferFile, StrucktData);
    Case StrucktData.Art of
     4 : Begin
           Seek(LabelDataPufferFile, StrucktData.DLabelList - 1);
           Read(LabelDataPufferFile, LabelData)
         End;
     5 : Begin
           Seek(PfadDataPufferFile, StrucktData.DPfadList - 1);
           Read(PfadDataPufferFile, PfadData)
         End;
     6 : Begin
           Inc(Zaehler);
           Seek(DateiDataPufferFile, StrucktData.DDateiList - 1);
           Read(DateiDataPufferFile, DateiData);
           Writexy(1, 2, 'PrÅfe  : ' + GetString(Zaehler));
           PruefeDaten(LabelData.LabelName,
                       PfadData.PfadName,
                       DateiData.DateiName);
           Writexy(1, 3, 'GeprÅft: ' + GetString(Zaehler));
         End
      Else Wait('Element in Struckt nicht 4 - 6!');
    End
  End;
  Wait('Daten vor Reorg geprÅft, ErrCount: ' + GetString(ErrCount));
  ErrCount := 0;
  *)


  ReOrgDatei(DateiTree);
  ReOrgPfad(PfadTree);
  ReOrgLabel(LabelTree);

  Close(DateiDataPufferfile);
  Close(PfadDataPufferfile);
  Close(LabelDataPufferfile);
  Close(StrucktDataPufferFile);
  Close(HilfDataPufferFile);
  Close(ZusatzDataPufferFile);

  Close(DDPF);
  Close(PDPF);
  Close(LDPF);

  Assign(DDPF, 'Puffer0.DVD');
  Assign(PDPF, 'Puffer0.DVP');
  Assign(LDPF, 'Puffer0.DVL');

  Erase(DDPF);
  Erase(PDPF);
  Erase(LDPF);

  Assign(DDPF, 'D1.DVD');
  Assign(PDPF, 'D1.DVP');
  Assign(LDPF, 'D1.DVL');

  Rename(DDPF, 'Puffer0.DVD');
  Rename(PDPF, 'Puffer0.DVP');
  Rename(LDPF, 'Puffer0.DVL');

  Assign(LogFile, 'D:\TestLogA.Log');
  OpenLogFile;
  TreeTraverse(DateiTree);
  Close(LogFile);

  B := System.MemAvail;
  NewTree := Nil;
  Dif := False;
  TreeReSort(DateiTree);
  DateiTree := NewTree;

  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +
                                     GetString(B) + Chr(13) +
                                     GetString(System.MemAvail));

  B := System.MemAvail;
  NewTree := Nil;
  Dif := False;
  TreeReSort(PfadTree);
  PfadTree := NewTree;

  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +
                                     GetString(B) + Chr(13) +
                                     GetString(System.MemAvail));

  B := System.MemAvail;
  NewTree := Nil;
  Dif := False;
  TreeReSort(LabelTree);
  LabelTree := NewTree;

  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +
                                     GetString(B) + Chr(13) +
                                     GetString(System.MemAvail));


  Dateifilename   := 'Puffer' + '0' + '.DVD';
  Pfadfilename    := 'Puffer' + '0' + '.DVP';
  Labelfilename   := 'Puffer' + '0' + '.DVL';
  Strucktfilename := 'Puffer' + '0' + '.DVS';
  Hilffilename    := 'Puffer' + '0' + '.DVH';
  Zusatzfilename  := 'Puffer' + '0' + '.DVZ';

  InitPufferFiles;
  Erase(ZusatzDataPufferFile);
  OpenPufferFiles;
  LogWrite := False;


  Assign(LogFile, 'D:\TestLogB.Log');
  OpenLogFile;
  TreeTraverse(DateiTree);
  Close(LogFile);


  Zaehler := 0;
  ErrCount := 0;
  PfadNF := 0;

(*  Seek(ImportFile, 0); *)

  Seek(StrucktDataPufferFile, 0);
  ZusatzList := Nil;
  While Not Eof(StrucktDataPufferFile) do
  Begin
    Inc(Zaehler);
    Read(StrucktDataPufferFile, StrucktData);
    Case StrucktData.Art of
     4 : Begin
           LabelStelle := 0;

           If HelpTreesOnFile then
           Begin
             Seek(LabelTreeFile, StrucktData.DLabelList - 1);
             New(Result);
             Read(LabelTreeFile, Result^)
           End
           Else
           Result := SucheMod(LabelTree, StrucktData.DLabelList - 1, FileSize(LabelDataPufferFile));

           If Result <> Nil then
           Begin
             StrucktData.DLabelList := Result^.Stelle;
             LabelStelle := Zaehler;
             Seek(StrucktDataPufferFile, FilePos(StrucktDataPufferFile) - 1);
             Write(StrucktDataPufferFile, StrucktData)
           End
           Else Wait('Label nicht gefunden!');
           If HelpTreesOnFile then Dispose(Result)
         End;
     5 : Begin
           PfadStelle := 0;

           If HelpTreesOnFile then
           Begin
             Seek(PfadTreeFile, StrucktData.DPfadList - 1);
             New(Result);
             Read(PfadTreeFile, Result^)
           End
           Else
            Result := SucheMod(PfadTree, StrucktData.DPfadList - 1, FileSize(PfadDataPufferFile));

           If Result <> Nil then
           Begin
             StrucktData.DPfadList := Result^.Stelle;
             PfadStelle := Zaehler;
             If PfadStelle <> FilePos(StrucktDataPufferFile) then
             Begin
               Wait('PfadStelle <> FilePos(StrucktDataPufferFile)');
               Halt
             End;
             Seek(StrucktDataPufferFile, FilePos(StrucktDataPufferFile) - 1);
             Write(StrucktDataPufferFile, StrucktData)
           End
           Else Wait('Pfad nicht gefunden!');
           If HelpTreesOnFile then Dispose(Result)
         End;
     6 : Begin
           If HelpTreesOnFile then
           Begin
             Seek(DateiTreeFile, StrucktData.DDateiList - 1);
             New(Result);
             Read(DateiTreeFile, Result^)
           End
           Else
             Result := SucheMod(DateiTree, StrucktData.DDateiList - 1, FileSize(DateiDataPufferFile));

           Inc(Find1);
           If Result <> Nil then
           Begin
             Inc(Find);
             StrucktData.DDateiList := Result^.Stelle;
             Seek(StrucktDataPufferFile, FilePos(StrucktDataPufferFile) - 1);
             Write(StrucktDataPufferFile, StrucktData);

             If (LabelStelle = 0) or
                (PfadStelle = 0) then
                Begin
                  Wait('Ein Label oder Pfad wurde nicht gefunden!');
                  Halt
                End;

             SpeichereSuchDatei(LabelStelle, PfadStelle, Result^.Stelle);
         (*  Writexy(1, 2, 'PrÅfe  : ' + GetString(Find));
             PruefeDateiDaten(LabelStelle, PfadStelle, Result^.Stelle);
             Writexy(1, 3, 'GeprÅft: ' + GetString(Find)); *)
           End;
           If HelpTreesOnFile then Dispose(Result)
         End
      Else Wait('Element in Struckt nicht 4 - 6!');
    End
    
  End;

(*  Wait(GetString(Find) + ' Dateien von ' + GetString(Find1) + ' gefunden.' + Chr(13) +
       GetString(DZaehler) + ' Dateien geprÅft.' + Chr(13) +
       GetString(DVZaehler) + ' Dateien richtig.'+ Chr(13) +
       'ErrCount: ' + GetString(ErrCount));
*)
  ErrCount := 0;

  Help := ZusatzList;
  B := 1;
  While Help <> Nil do
  Begin
    Help^.Back := Pointer(B);
    Write(ZusatzDataPufferFile, Help^.ZusatzData);
    Inc(B);
    Help := Help^.Next
  End;

  Seek(DateiDataPufferFile, 0);
  While Not Eof(DateiDataPufferFile) do
  Begin
    Read(DateiDataPufferFile, DateiData);
    DateiData.Loc := DateiData.Loc^.Back;
    Seek(DateiDataPufferFile, FilePos(DateiDataPufferFile) - 1);
    Write(DateiDataPufferFile, DateiData)
  End;

  Wait('Lîsche Zusatzliste');

  Help := ZusatzList;
  While Help <> Nil do
  Begin
    ZusatzList := Help;
    Help := Help^.Next;
    Dispose(ZusatzList)
  End;
  Dispose(ZusatzList);

  Wait('Zusatzliste gelîscht');

  Close(DateiDataPufferfile);
  Close(PfadDataPufferfile);
  Close(LabelDataPufferfile);
  Close(StrucktDataPufferFile);
  Close(HilfDataPufferFile);
  Close(ZusatzDataPufferFile);

  Dateifilename   := DF;
  Pfadfilename    := PF;
  Labelfilename   := LF;
  Strucktfilename := SF;
  Hilffilename    := HF;
  Zusatzfilename  := ZF;

  If HelpTreesOnFile then
  Begin
    HelpTreesOnFile := False;

    Close(LabelTreeFile);
    Close(PfadTreeFile);
    Close(DateiTreeFile);
    Erase(LabelTreeFile);
    Erase(PfadTreeFile);
    Erase(DateiTreeFile)
  End

End; (* BuildDataFiles *)

End.
