Unit TvvtData;

{$X+,O+,F+}
Interface

Uses Crt,
     App,
     Drivers,
     GadGets,
     Dos,
     LabHist,
     Memory,
     MsgBox,
     Objects,
     Dialogs,
     Views,
     StdDlg;

type

      string12             = string[12];


const Dateifilename: string   = 'Datei.dvd';
      Pfadfilename: string    = 'Pfad.dvd';
      Labelfilename: String   = 'Label.dvd';
      Strucktfilename: string = 'Struckt.dvd';
      Hilffilename: string    = 'Hilf.dvd';
      Zusatzfilename: string  = 'Zusatz.dvd';

      FileName           : String12 = '.DVD';

      ImportFileName     : String12 = '.DIR';




(*      LabelFileOpened: Boolean = False;
      PfadFileOpened: Boolean = False;
      DateiFileOpened: Boolean = False;
      StrucktFileOpened: Boolean = False;
      HilfFileOpened: Boolean = False;
      ZusatzFileOpened: Boolean = False;
*)
      NoNew: Boolean          = False;

Var   TreePointer: Pointer;  (* Wird verwendet, wenn NoNew True ist.    *)
                             (* Dh. wenn der Baum nur balangsiert wird. *)

type

     arttyp               = byte;   (* Label, Pfad, Datei *)


(*************************************************************)
(*                                                           *)
(* Globale Datentypen werden z. B. beim Sortieren verwendet. *)
(*                                                           *)
(*************************************************************)

     GlobalListPtrtyp = ^GlobalListtyp;

     GlobalListtyp    = Record
                            Back,
                            Next    : GlobalListPtrtyp;
                            Bal     : Shortint;
                            Anzahl  : Byte;
                          end;

     GlobalBaumPtrtyp = ^GlobalBaumtyp;

     GlobalBaumtyp    = Record
                            Links,
                            Rechts  : GlobalBaumPtrtyp;
                            Bal     : Shortint;
                            Anzahl  : Byte;
                          end;


     ListenPtrtyp     = ^Listentyp;

     Listentyp         = Record
                            Back,
                            Next  : ListenPtrtyp
                          end;

     LabelDatatyp     = Record
                            Bal       : Shortint;
                            Anzahl    : Byte;
                            LabelName : String12
                          end;


     LabelListPtrtyp = ^LabelListtyp;

     LabelListtyp     = record
                            Links,
                            Rechts    : LabelListPtrtyp;
                            LabelData : LabelDatatyp
                          end;

     PfadDatatyp      = Record
                            Bal       : Shortint;
                            Anzahl    : Byte;
                            PfadName  : String[70]
                          end;


     PfadListPtrtyp  = ^PfadListtyp;

     PfadListtyp      = record
                            Links,
                            Rechts    : PfadListPtrtyp;
                            PfadData  : PfadDatatyp
                          end;

     DateiListPtrtyp = ^DateiListtyp;


     StrucktDatatyp     = record
                            case art : arttyp of
                              1 : (LabelList   : LabelListPtrtyp);
                              2 : (PfadList    : PfadListPtrtyp);
                              3 : (DateiList   : DateiListPtrtyp);
                              4 : (DLabelList : Longint);
                              5 : (DPfadList  : Longint);
                              6 : (DDateiList : Longint);
                          end;


     StrucktListPtrtyp  = ^StrucktListtyp;

     StrucktListtyp      = record
                               Back,
                               Next        : StrucktListPtrtyp;
                               StrucktData : StrucktDatatyp
                             end;

     ZusatzListPtrtyp   = ^ZusatzListtyp;

     ZusatzDatatyp         = record
                               sl,
                               sp  : StrucktListPtrtyp
                             end;

     ZusatzListtyp       = record
                               Back,
                               Next: ZusatzListPtrtyp;
                               ZusatzData: ZusatzDatatyp
                             end;

     DateiDatatyp       = record
                            Bal      : Shortint;
                            Anzahl   : byte;
                            DateiName: String12;
                            Loc      : ZusatzListPtrtyp
                          end;




     HilfDatatyp           = record
                               case art : arttyp of
                                 1 : (LabelList : StrucktListPtrtyp);
                                 2 : (PfadList  : StrucktListPtrtyp);
                                 3 : (DLabelList : Longint);
                                 4 : (DPfadList  : Longint)
                               end;

     HilfListPtrtyp     = ^HilfListtyp;

     HilfListtyp         = record
                               Back,
                               Next     : HilfListPtrtyp;
                               HilfData : HilfDatatyp
                             end;



     DateiListtyp     = record
                            Links,
                            Rechts           : DateiListPtrtyp;
                            DateiData        : DateiDatatyp
                          end;


     sltyp             = record
                           vp1,
                           vp2   : HilfListPtrtyp;
                           vp3   : StrucktListPtrtyp;
                            s1,
                            s2,
                            s3   : String
                          end;

     StrucktFiletyp    = File of StrucktDatatyp;
     HilfFiletyp       = File of HilfDatatyp;
     LabelFiletyp      = File of LabelDatatyp;
     PfadFiletyp       = File of PfadDatatyp;
     DateiFiletyp      = File of DateiDatatyp;
     ZusatzFiletyp     = File of ZusatzDatatyp;


(* Noch nicht einwandfrei
   ( nicht ersichtlich, wieviele jeweils vorhanden sind ) *)

     Mediumtyp            = record
                               n         : String;
                               z, ez     : ZusatzListPtrtyp;
                               s, es, as : StrucktListPtrtyp;
                               h, eh, ah : HilfListPtrtyp;
                               l         : LabelListPtrtyp;
                               p         : PfadListPtrtyp;
                               d         : DateiListPtrtyp;
                               DateiDif,
                               PfadDif,
                               LabelDif,
                               na        : Boolean
                             end;
     MediumFeldtyp       = array ['A'..'J'] of Mediumtyp;

(* Dynamische Felder in denen jedes Element auf ein spezifischen   *)
(* Datensatz eines Listenelementes zeigt.                          *)

     SPointerArray         = Array [1..1] of StrucktListPtrtyp;
     LPointerArray         = Array [1..1] of LabelListPtrtyp;
     PPointerArray         = Array [1..1] of PfadListPtrtyp;
     DPointerArray         = Array [1..1] of DateiListPtrtyp;
     ZPointerArray         = Array [1..1] of ZusatzListPtrtyp;

(* Dynamisches Feld, das Zeigeroperationen auf alle Åbrigen Felder *)
(* zulÑsst.                                                        *)

     GPointerArray         = Array [1..1] of GlobalListPtrtyp;

(* Die Zeiger zu den dynamischen Feldern:                          *)

     StrucktFeldPtrtyp  = ^SPointerArray;
     LabelFeldPtrtyp    = ^LPointerArray;
     PfadFeldPtrtyp     = ^PPointerArray;
     DateiFeldPtrtyp    = ^DPointerArray;
     ZusatzFeldPtrtyp   = ^ZPointerArray;
     GlobalFeldPtrtyp   = ^GPointerArray;




var
     StrucktFeld : StrucktFeldPtrtyp;
     EMSSF       : Boolean;
     LabelFeld   : LabelFeldPtrtyp;
     EMSLF       : Boolean;
     PfadFeld    : PfadFeldPtrtyp;
     EMSPF       : Boolean;
     DateiFeld   : DateiFeldPtrtyp;
     EMSDF       : Boolean;
     ZusatzFeld  : ZusatzFeldPtrtyp;
     EMSZF       : Boolean;

     DelLabelFeld : LabelFeldPtrtyp;
     EMSDLF       : Boolean;
     DelPfadFeld  : PfadFeldPtrtyp;
     EMSDPF       : Boolean;
     DelDateiFeld : DateiFeldPtrtyp;
     EMSDDF       : Boolean;




     DirStatus   : Boolean;
     Maske       : String;
     zhilf       : integer;
     Stelle,
     Anzahl,
     Deleted,   (* Elemente, die nicht gespeichert werden *)
     SAnzahl,
     LAnzahl,
     PAnzahl,
     DAnzahl,
     ZAnzahl          : Longint;

     N                : String;
     A, B             : Longint;
     DateiDif,
     PfadDif,
     LabelDif         : Boolean;
     Raus,
     ch,
     cha              : Char;
     MediumFeld       : MediumFeldtyp;
     M                : Array [1..2] of Mediumtyp;
     Nachgetragen,
     w                : Boolean;
     ziffer,
     ende             : Char;
     mask,
     pfad			  : DirStr;
     befehl			  : NameStr;
     extender		  : ExtStr;
     lab              : String;
     DiskNr           : String12;
     srec,
     sr               : Searchrec;
     s                : Real;
     attr             : Byte;

     Labelzeiger,
     Pfadzeiger       : StrucktListPtrtyp;



     ZusatzList,
     ZusatzListend    : ZusatzListPtrtyp;

     LabelList,
     Labeladress      : LabelListPtrtyp;

     PfadList,
     Pfadadress       : PfadListPtrtyp;

     DateiList,
     Dateiadress      : DateiListPtrtyp;

     HilfList,
     HilfListend,
     HHilf,
     AltHilf          : HilfListPtrtyp;

     StrucktList,
     StrucktListend,
     AltStruckt       : StrucktListPtrtyp;

     StrucktData      : StrucktDatatyp;
     HilfData         : HilfDatatyp;

     StrucktDatafile  : StrucktFiletyp;
     HilfDatafile     : HilfFiletyp;
     LabelDatafile    : LabelFiletyp;
     PfadDatafile     : PfadFiletyp;
     DateiDatafile    : DateiFiletyp;
     ZusatzDatafile   : ZusatzFiletyp;

     StrucktDataPufferfile  : StrucktFiletyp;
     HilfDataPufferfile     : HilfFiletyp;
     LabelDataPufferfile    : LabelFiletyp;
     PfadDataPufferfile     : PfadFiletyp;
     DateiDataPufferfile    : DateiFiletyp;
     ZusatzDataPufferfile   : ZusatzFiletyp;


(* Feld, das benîtigt wird, um auf Zeiger aller Felder zugreifen zu kînnen. *)
(* Dieses Feld dient zur allgemeinen Umwandlungshilfe *)
(* Dies ist der Zeiger dazu:   *)

     GlobalFeldPtr  : GlobalFeldPtrtyp;

(* Globale Zeigerverkettung: *)

     GlobalListPtr  : GlobalListPtrtyp;

     Regs           : Registers;
     CursorSave     : Word;




Procedure Wait(txt : string);
Function  GetFName: String;
Procedure CursorAus;
Procedure NewArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);
Procedure DisposeArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);

Procedure Fehler(txt:string ; var ch:char);
Procedure GetMedium(ch: char);
Procedure SpeichereDatei(DateiName : string12;
                         var  Lauf : DateiListPtrtyp;
                         var  dif  : Boolean;
                              NoNew: Boolean);
Procedure GetStruckt (var Ende, cut: StrucktListPtrtyp);
Procedure GetHilf (var Ende, cut: HilfListPtrtyp);
Procedure SpeichereSuch(lp, pp: StrucktListPtrtyp;
                             dp: DateiListPtrtyp);
Procedure Writexy(x,y : byte; txt : string);
Procedure SpeichereLabel(LabelName : string12;
                         var  Lauf : LabelListPtrtyp;
                         var  dif  : Boolean;
                              NoNew: Boolean);
Procedure SpeicherePfad(PfadName : string;
                        var  Lauf : PfadListPtrtyp;
                        var  dif  : Boolean;
                             NoNew: Boolean);



Procedure InitFileNames(Name: String12);
Function  TestFiles:Boolean;
Procedure DateiDataDateiOeffnen  (var Datei : DateiFiletyp);
Procedure LabelDataDateiOeffnen  (var Datei : LabelFiletyp);
Procedure PfadDataDateiOeffnen   (var Datei : PfadFiletyp);
Procedure StrucktDataDateiOeffnen(var Datei : StrucktFiletyp);
Procedure HilfDataDateiOeffnen   (var Datei : HilfFiletyp);
Procedure ZusatzDataDateiOeffnen (var Datei : ZusatzFiletyp);
Procedure CloseAllFiles;
Procedure OpenAllFiles;


Function  UpDate(term: String):String;
Procedure List(pfad:string);
Procedure Dir(Var Name: String);
Procedure CopyMinWork(x:byte);
Procedure CopyWorkinM(x:byte);
Procedure TestMaskenSyntax(Var Maske: String);
Function  PasstInMaske(Maske, Name: PathStr):Boolean;
Procedure NewFileName(Name: String; Var FileName: String12);
Procedure Initfiles;
Procedure SetAllNil;
Procedure MachBinBaum(Var p: GlobalListPtrtyp;
                        l, r : Longint);
Procedure SpeichereDateien;
Procedure LadeDateien;
Procedure LPDFelderBauen;
Procedure Halt;
Procedure SaveCursor;
Procedure OrgCursor;
Procedure ZaehleElemente(Root: GlobalBaumPtrtyp);
Function ReadString(DialogTyp: String): PDialog;
Function AskYesNo(Ask: String): PDialog;
Function ExecDialog(P: PDialog; Data: Pointer): Word;
Procedure PruefeDaten(Wo: String);
Function GetString(Zahl: Longint): String;


Implementation
Uses DynAr, BDF;

var ms, mx, my : Word;

Function GetString;
Var Help : String;
Begin
  Str(Zahl, Help);
  GetString := Help
End;


Function GetFName: String;
Var Name: String;
Begin
  Name := ParamStr(0);
  While Pos('.', Name) <> 0 do
    System.Delete(Name, Length(Name), 1);
  GetFName := Name
End;

function ExecDialog;
var
  Result: Word;
BEGIN
  Result := cmCancel;
  P := PDialog(Application^.ValidView(P));
  if P <> nil then
  BEGIN
    if Data <> nil then P^.SetData(Data^);
    Result := DeskTop^.ExecView(P);
    if (Result <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    Dispose(P, Done);
  END;
  ExecDialog := Result;
END;

function ReadString;
var
  D: PDialog;
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(2, 0, 78, 8);
  D := New(PDialog, Init(R, DialogTyp));
  with D^ do
  BEGIN
    Options := Options or ofCentered;

    R.Assign(3, 3, 32, 4);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);

    R.Assign(32, 3, 35, 4);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(48, 3, 58, 5);
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

    SelectNext(False);
  END;
  ReadString := D;
END;

function AskYesNo;
var
  D: PDialog;
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(2, 0, 78, 8);
  D := New(PDialog, Init(R, ''));
  with D^ do
  BEGIN
    Options := Options or ofCentered;

    R.Assign(3, 3, Length(Ask) + 3, 4);
    Insert(New(PStaticText, Init(R, Ask)));

    R.Assign(48, 3, 58, 5);
    Insert(New(PButton, Init(R, '~Y~es', cmOk, bfDefault)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, '~N~o', cmNo, bfNormal)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, '~A~bbrechen', cmCancel, bfNormal)));

    SelectNext(False);
  END;
  AskYesNo := D;
END;


Procedure PruefeDaten;
Var SArt  : String;
    LA,
    HA    : Longint;
    SHA,
    SLA   : String;
    HLauf : HilfListPtrTyp;
    ZLauf : ZusatzListPtrtyp;
    SLP,
    SSP   : String;
    Lauf  : StrucktListPtrtyp;
    Anzahl: Integer;
    HAn,
    ZAn,
    PAn,
    LAn   : Integer;
    SDAn,
    SSDAn,
    SZAn  : String;

Procedure ZaehleAbs(Root: GlobalBaumPtrtyp);
Begin
  If Root <> Nil then
  Begin
    ZaehleAbs(Root^.Links);

    If Root^.Anzahl > 0 then
      Inc(Anzahl, Root^.Anzahl);

    ZaehleAbs(Root^.Rechts)
  End
End;

Procedure TesteZusatzaufStruckt(ZLauf: ZusatzListPtrtyp);
Var Gef : Boolean;
Begin
  Gef := False;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf = ZLauf^.ZusatzData.sl then
    Begin
      While (Lauf <> Nil) and
            (Lauf <> ZLauf^.ZusatzData.sp) do
        Lauf := Lauf^.Next;
      If Lauf <> Nil then Gef := True
    End;
    If Gef then Exit;
    Lauf := Lauf^.Next
  End;
  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!')
End;

Procedure TesteHilfaufStruckt(HLauf: HilfListPtrtyp);
Var Gef : Boolean;
Begin
  Gef := False;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf = HLauf^.HilfData.LabelList then
      Gef := True;
    If Gef then Exit;
    Lauf := Lauf^.Next
  End;
  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!')
End;

Begin
  Wait(Wo);

(* Teste 1, 2, 3 von Strucktliste: *)

  Lauf := StrucktList;

  If Lauf^.Back <> Nil then
    Wait('Strucktliste nicht in ordnung');

  While Lauf <> Nil do
  Begin
    If Not(Lauf^.StrucktData.art In [1, 2, 3]) then
      Wait('Strucktliste hat fehlerhafte Elemente!');
    Lauf := Lauf^.Next
  End;

(* Teste 1, 2 von Hilfliste: *)

  HLauf := HilfList;

  If HLauf^.Back <> Nil then
    Wait ('Hilfliste nicht in ordnung!');

  While HLauf <> Nil do
  Begin
    If Not(HLauf^.HilfData.art In [1, 2]) then
      Wait('Hilfliste hat fehlerhafte Elemente!');
    HLauf := HLauf^.Next
  End;



(* PrÅfe Zeiger von Zusatz auf Struckt: *)

  ZLauf := ZusatzList;

  While ZLauf <> Nil do
  Begin
    TesteZusatzaufStruckt(ZLauf);
    ZLauf := ZLauf^.Next
  End;

(* PrÅfe Zeiger von Hilf auf Struckt: *)

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    TesteHilfaufStruckt(HLauf);
    HLauf := HLauf^.Next
  End;


(* Testen, ob die Anzahl der Dateien mit der Anzahl der *)
(* Zusatzelemente Åbereinstimmt:                        *)

  Anzahl := 0;
  ZaehleAbs(GlobalBaumPtrtyp(DateiList));
  ZAn := Anzahl;
  Str(ZAn, SDAn);

  Anzahl := 0;

  ZLauf := ZusatzList;

  While ZLauf <> Nil do
  Begin
    Inc(Anzahl);
    ZLauf := ZLauf^.Next
  End;
  Str(Anzahl, SZAn);

  If Anzahl <> ZAn then Wait('Anzahl <> ZA ( Datei - Zusatz )!' + Chr(13) +
                             'Datei : ' + SDAn + Chr(13) +
                             'Zusatz: ' + SZAn );

(********************************************************)

(* Testen, ob die Anzahl der Dateien mit der Anzahl der *)
(* Dateielemente in der Strucktliste Åbereinstimmt:     *)

  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 3 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> ZAn then Wait('Anzahl <> SAn ( Datei - Struckt )!');

(********************************************************)

(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)
(* Pfadelemente in der Strucktliste Åbereinstimmt:      *)

  Anzahl := 0;

  ZaehleAbs(GlobalBaumPtrtyp(PfadList));
  PAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 2 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> PAn then Wait('Anzahl <> PAn ( Pfad - Struckt )!');

(********************************************************)

(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)
(* Labelelemente in der Strucktliste Åbereinstimmt:     *)

  Anzahl := 0;

  ZaehleAbs(GlobalBaumPtrtyp(LabelList));
  LAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 1 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> LAn then Wait('Anzahl <> LAn! ( Label - Struckt )');

(********************************************************)

(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)
(* Labelelemente in der Hilfliste Åbereinstimmt:        *)

  Anzahl := 0;

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    If HLauf^.HilfData.art = 1 then Inc(Anzahl);
    HLauf := HLauf^.Next
  End;

  HAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 1 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> HAn then Wait('Label: Anzahl <> HAn! ( Hilf - Struckt )');

(********************************************************)

(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)
(* Pfadelemente in der Hilfliste Åbereinstimmt:         *)

  Anzahl := 0;

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    If HLauf^.HilfData.art = 2 then Inc(Anzahl);
    HLauf := HLauf^.Next
  End;

  HAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 2 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> HAn then Wait('Pfad: Anzahl <> HAn! ( Hilf - Struckt )');

(********************************************************)


  LA := 0;
  HA := 0;

  HLauf := HilfList;
  Lauf := StrucktList;


  While (Lauf <> Nil) do
  Begin
    If Lauf^.StrucktData.art <> 3 then


    Begin
      If Not(Lauf^.StrucktData.Art in [1, 2]) then
        Wait('In Strucktliste ist ein Element,' + chr(13) +
             'dessen Art nicht 1 oder 2 ist!');

      Inc(LA);
      Inc(HA);

      If HLauf <> Nil then
        If Not((HLauf^.HilfData.art = 1) or (HLauf^.HilfData.art = 2)) then
        Begin
          SArt := '';
          Str(HLauf^.HilfData.art, SArt);
          Wait('Hilfelement <> 1, 2: ' + SArt +
               ' an Stelle: ' + SHA)
        End
        Else
      Else
        Wait('Element: ' + SHA + ' ist Nil');

      If HLauf^.HilfData.LabelList <> Lauf then
      Begin
        Wait('Hilfelement zeigt nicht auf zugeh. Strucktelement!');
      End;

      HLauf := HLauf^.Next
    End;



    If Lauf^.Next = Nil then
      If Lauf^.StrucktData.Art <> 3 then
        Wait('Fataler Fehler in Strucktliste, nach Pfad oder Label keine Datei mehr!');
    Lauf := Lauf^.Next
  End;



  Str(LA, SLA);
  Str(HA, SHA);



  If HA <> LA then
  Begin
    Wait('Anzahl der Strucktelemente' + chr(13) +
         '(au·er Art = 3) stimmt nicht' + chr(13) +
         'Åberein mit Hilfelementen!' + chr(13) + chr(13) +
         'Strucktelemente: ' + SLA + Chr(13) +
         'Hilfelemente   : ' + SHA);
    OpenAllFiles;
    HA := FileSize(HilfDataFile);
    CloseAllFiles;
    Str(HA, SHA);
    Wait('Anzahl der Hilfelemente in Datei: ' + SHA);
  End;
End;


Procedure BaumToList(Var Root: GlobalBaumPtrtyp);
Var MerkeLinks,
    MerkeRechts, 
    Tree         : GlobalBaumPtrtyp;
Begin

End;


Procedure Halt;
Begin
  OrgCursor;
  Clrscr;
  System.Halt
End;

Procedure SaveCursor;
Begin
  Regs.AH := $03;
  Intr($10, Regs);
  CursorSave := Regs.CX;
End;

Procedure OrgCursor;
Begin
  Regs.AH := $01;
  Regs.CX := CursorSave;
  Intr($10, Regs)
End;


Procedure LSpeichern(Root: Pointer; Stelle: Longint);
BEGIN
  If LabelListPtrtyp(Root)^.LabelData.Anzahl = 0 then
    DelLabelFeld^[Stelle] := LabelListPtrtyp(Root)
  Else
    LabelFeld^[Stelle] := LabelListPtrtyp(Root)
END;

Procedure PSpeichern(Root: Pointer; Stelle: Longint);
BEGIN
  If PfadListPtrtyp(Root)^.PfadData.Anzahl = 0 then
    DelPfadFeld^[Stelle] := PfadListPtrtyp(Root)
  Else
  PfadFeld^[Stelle] := PfadListPtrtyp(Root)
END;

Procedure DSpeichern(Root: Pointer; Stelle: Longint);
BEGIN
  If DateiListPtrtyp(Root)^.DateiData.Anzahl = 0 then
    DelDateiFeld^[Stelle] := DateiListPtrtyp(Root)
  Else
  DateiFeld^[Stelle] := DateiListPtrtyp(Root)
END;


Procedure MachBinBaum;

var mitte        : Longint;

BEGIN
  Mitte := (l + r) Div 2;

  P := GlobalFeldPtr^[Mitte];

  GlobalFeldPtr^[Mitte]^.Back := Nil;
  GlobalFeldPtr^[Mitte]^.Next := Nil;

  if Mitte > l then MachBinBaum(GlobalFeldPtr^[Mitte]^.Back,l,Mitte-1);
  if Mitte < r then MachBinBaum(GlobalFeldPtr^[Mitte]^.Next,Mitte+1,r)
END;

Type Speichern = Procedure(Root: Pointer; Stelle: Longint);

Var  Speichere : Speichern;

Procedure ZaehleElemente;
BEGIN
  if Root <> Nil then
  Begin
    ZaehleElemente(Root^.Rechts);
    if Root^.Anzahl > 0 then
      Inc(Anzahl)
    Else
      Inc(Deleted);
    ZaehleElemente(Root^.Links)
  end
END;


Procedure BaueFeld(Root: GlobalBaumPtrtyp);
var Hilf : GlobalListPtrtyp;
BEGIN
  if Root <> Nil then
  BEGIN
    BaueFeld(Root^.Links);

    if Root^.Anzahl <> 0 then

    BEGIN
      If Root^.Anzahl < 0 then
      Begin
        Wait('Fehler in Anzahl eines Elementes!');
        Halt
      End;
      Inc(Anzahl);
      Root^.Links := Pointer(Anzahl);
      Speichere(Root, Anzahl)
    END
    Else
    Begin
      Inc(Deleted);
      Speichere(Root, Deleted)
    End;

    BaueFeld(Root^.Rechts)
  end
END;




Procedure LPDFelderBauen;

BEGIN
{  Anzahl := 0;
  ZaehleElemente(GlobalListPtrtyp(LabelList));
  LAnzahl := Anzahl;
  GetArray(LabelFeld, 4, Anzahl);
  Speichere := LSpeichern;
  Anzahl := 0;
  BaueFeld(GlobalListPtrtyp(LabelList));

  Anzahl := 0;
  ZaehleElemente(GlobalListPtrtyp(PfadList));
  PAnzahl := Anzahl;
  GetArray(PfadFeld, 4, Anzahl);
  Speichere := PSpeichern;
  Anzahl := 0;
  BaueFeld(GlobalListPtrtyp(PfadList));

  Anzahl := 0;
  ZaehleElemente(GlobalListPtrtyp(DateiList));
  DAnzahl := Anzahl;
  GetArray(DateiFeld, 4, Anzahl);
  Speichere := DSpeichern;
  Anzahl := 0;
  BaueFeld(GlobalListPtrtyp(DateiList));
}

  Anzahl := 0;
  Deleted := 0;
  ZaehleElemente(GlobalBaumPtrtyp(LabelList));

  LAnzahl := Anzahl;
  If GetArray(Pointer(LabelFeld)   , 4, Anzahl, EMSLF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;


  If GetArray(Pointer(DelLabelFeld), 4, Deleted, EMSDLF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;

  Speichere := LSpeichern;
  Anzahl := 0;
  Deleted := 0;
  BaueFeld(GlobalBaumPtrtyp(LabelList));

  For A := 1 to Deleted do
    Dispose(DelLabelFeld^[A]);

  ReleaseArray(DelLabelFeld, 4, Deleted, EMSDLF);

  Anzahl := 0;
  Deleted := 0;
  ZaehleElemente(GlobalBaumPtrtyp(PfadList));

  PAnzahl := Anzahl;
  If GetArray(Pointer(PfadFeld)   , 4, Anzahl, EMSPF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;


  If GetArray(Pointer(DelPfadFeld), 4, Deleted, EMSDPF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;

  Speichere := PSpeichern;
  Anzahl := 0;
  Deleted := 0;
  BaueFeld(GlobalBaumPtrtyp(PfadList));

  For A := 1 to Deleted do
    Dispose(DelPfadFeld^[A]);

  ReleaseArray(DelPfadFeld, 4, Deleted, EMSDPF);


  Anzahl := 0;
  Deleted := 0;
  ZaehleElemente(GlobalBaumPtrtyp(DateiList));

  DAnzahl := Anzahl;
  If GetArray(Pointer(DateiFeld)   , 4, Anzahl, EMSDF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;


  If GetArray(Pointer(DelDateiFeld), 4, Deleted, EMSDDF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;

  Speichere := DSpeichern;
  Anzahl := 0;
  Deleted := 0;
  BaueFeld(GlobalBaumPtrtyp(DateiList));

  For A := 1 to Deleted do
    Dispose(DelDateiFeld^[A]);

  ReleaseArray(DelDateiFeld, 4, Deleted, EMSDDF);




END;



Procedure SpeichereDateien;
var Lauf         : HilfListPtrtyp;

Procedure StrucktfeldBauen;
var Hilf : Pointer;
BEGIN
  (* Wichtig: *)

  Anzahl := 0;
  Hilf := StrucktList;

  (* ZÑhlen der Elemente: *)

  while StrucktList <> Nil do
  BEGIN
    Inc(Anzahl);
    StrucktList := StrucktList^.Next
  END;

  (* GezÑhlte Elemente: *)

  SAnzahl := Anzahl;

  (* Strucktfeld reservieren: *)

  If GetArray(Pointer(StrucktFeld),4,SAnzahl, EMSSF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;

  (* Strucktfeld mit werten belegen: *)

  (* Wichtig: *)
  StrucktList := Hilf;

  for a := 1 to SAnzahl do
  BEGIN
    StrucktFeld^[A] := StrucktList;
    StrucktFeld^[A]^.Back := Pointer(A);
    StrucktList := StrucktList^.Next;
  END;
  StrucktList := Hilf
END;



Procedure ZusatzUmlenken;
var hilf : Pointer;
BEGIN
  Hilf := ZusatzList;
  While ZusatzList <> Nil do
  BEGIN
    If ZusatzList^.ZusatzData.SL <> Nil then
      ZusatzList^.ZusatzData.sl := ZusatzList^.ZusatzData.sl^.Back;



    ZusatzList^.ZusatzData.sp := ZusatzList^.ZusatzData.sp^.Back;
    ZusatzList := ZusatzList^.Next
  END;
  ZusatzList := Hilf
END;

Procedure HilfUmlenken;
var Hilf : Pointer;
BEGIN
  Hilf := HilfList;
  For A := 1 to SAnzahl do
    case StrucktFeld^[A]^.StrucktData.Art of
      4  : BEGIN
             HilfList^.HilfData.DLabelList := A;
             HilfList^.HilfData.art := 3;
             HilfList := HilfList^.Next
           END;
      5  : BEGIN
             HilfList^.HilfData.DPfadList := A;
             HilfList^.HilfData.art := 4;
             HilfList := HilfList^.Next
           end
    END;
  HilfList := Hilf
END;

Procedure ZusatzFeldBauen;
var
    Hilf   : Pointer;
BEGIN
  Anzahl := 0;
  Hilf := ZusatzList;
  while ZusatzList <> Nil do
  BEGIN
    Inc(Anzahl);
    ZusatzList^.Back := Pointer(Anzahl);
    ZusatzList := ZusatzList^.Next
  END;
  ZAnzahl := Anzahl;
  ZusatzList := Hilf;

  If GetArray(Pointer(ZusatzFeld),4,Anzahl, EMSZF) = 1 then
  Begin
    Wait('Kein Speicher fÅr diese Operation mehr da!');
    Halt
  End;

  (* Feld mit aktuellem Zeiger der Zusatzliste belegen: *)

  For a := 1 to Anzahl do
  BEGIN
    ZusatzFeld^[a] := ZusatzList;
    ZusatzList := ZusatzList^.Next
  END;
  ZusatzList := Hilf
END;

Procedure StrucktUmlenken;
BEGIN
  for A := 1 to SAnzahl do
  BEGIN
    case StrucktFeld^[A]^.StrucktData.art of

      1 : BEGIN
            StrucktFeld^[A]^.StrucktData.DLabelList :=
            Longint(StrucktFeld^[A]^.StrucktData.LabelList^.Links);
            StrucktFeld^[A]^.StrucktData.art := 4
          END;

      2 : BEGIN
            StrucktFeld^[A]^.StrucktData.DPfadList :=
            Longint(StrucktFeld^[A]^.StrucktData.PfadList^.Links);
            StrucktFeld^[A]^.StrucktData.art := 5
          END;

      3 : BEGIN
            StrucktFeld^[A]^.StrucktData.DDateiList :=
            Longint(StrucktFeld^[A]^.StrucktData.DateiList^.Links);
            StrucktFeld^[A]^.StrucktData.art := 6
          end
    end
  end
END;

Procedure DateiUmlenken;
BEGIN
  For A := 1 to DAnzahl do
    DateiFeld^[A]^.DateiData.Loc := DateiFeld^[A]^.DateiData.Loc^.Back
END;


Var Mem1       : Longint;
    GlobalList : GlobalListPtrtyp;
    HString    : String;
BEGIN

  (* Listen und BÑume in Felder umwandeln: *)

  If TestFiles then
  Begin
    Erase(DateiDataFile);
    Erase(PfadDataFile);
    Erase(LabelDataFile);
    Erase(ZusatzDataFile);
    Erase(HilfDataFile);
    Erase(StrucktDataFile)
  End;


  LPDFelderBauen;    (* Erzeugen der Felder fÅr Label, Pfad und Datei *)
  StrucktfeldBauen;  (* s. Befehl *)
  StrucktUmlenken;   (* Zeiger, die auf Labels, Pfade und Dateien zeigen *)
                      (* werden in Feldzeiger ( Longint ) umgewandelt     *)
  ZusatzUmlenken;    (* Wandelt die Zeiger sl und sp in Longint um.      *)
  HilfUmlenken;      (* Wandelt Zeiger aus der Hilfliste um in Longint.  *)
  ZusatzFeldBauen;
  DateiUmlenken;
{
  Wait('ZÑhle Elemente in BÑumen',0);
  Anzahl := 0;
  ZaehleElemente(GlobalBaumPtrtyp(LabelList));
  LAnzahl := Anzahl;

  Anzahl := 0;
  ZaehleElemente(GlobalBaumPtrtyp(PfadList));
  PAnzahl := Anzahl;

  Anzahl := 0;
  ZaehleElemente(GlobalBaumPtrtyp(DateiList));
  DAnzahl := Anzahl;

  Wait('Mache aus BÑumen Listen',0);
  BaumToList(GlobalBaumPtrtyp(LabelList));
  BaumToList(GlobalBaumPtrtyp(PfadList));
  BaumToList(GlobalBaumPtrtyp(DateiList));

  Wait('öberprÅfe die Anzahl der Elemente',0);
  Anzahl := 0;
  GlobalList := GlobalListPtrtyp(DateiList);
  While GlobalList <> Nil do
  Begin
    If GlobalList^.Anzahl > 0 then Inc(Anzahl);
    GlobalList := GlobalList^.Next
  End;
  If DAnzahl <> Anzahl then Wait('DAnzahl <> Anzahl nach BaumToList!',0);

  Anzahl := 0;
  GlobalList := GlobalListPtrtyp(PfadList);
  While GlobalList <> Nil do
  Begin
    If GlobalList^.Anzahl > 0 then Inc(Anzahl);
    GlobalList := GlobalList^.Next
  End;
  If PAnzahl <> Anzahl then Wait('PAnzahl <> Anzahl nach BaumToList!',0);

  Anzahl := 0;
  GlobalList := GlobalListPtrtyp(LabelList);
  While GlobalList <> Nil do
  Begin
    If GlobalList^.Anzahl > 0 then Inc(Anzahl);
    GlobalList := GlobalList^.Next
  End;
  If LAnzahl <> Anzahl then Wait('LAnzahl <> Anzahl nach BaumToList!',0);

  Halt;

  StrucktUmlenken;

}
  (* Felder abspeichern: *)

  OpenallFiles;


  for A := 1 to SAnzahl do
  BEGIN
    Write(StrucktDatafile,StrucktFeld^[A]^.StrucktData);
    Dispose(StrucktFeld^[A])
  END;

  While HilfList^.Next <> Nil do
  BEGIN
    write(HilfDatafile,HilfList^.HilfData);
    HilfList := Hilflist^.Next;

    Dispose(HilfList^.Back)
  END;
  Write(HilfDatafile, HilfList^.HilfData);
  Dispose(HilfList);


  for A := 1 to ZAnzahl do
  BEGIN
    write(ZusatzDatafile,ZusatzFeld^[A]^.ZusatzData);
    Dispose(ZusatzFeld^[A])
  END;

  for A := 1 to LAnzahl do
  BEGIN
    HString := LabelFeld^[A]^.LabelData.LabelName;
    FillChar(LabelFeld^[A]^.LabelData.LabelName,
             SizeOf(LabelFeld^[A]^.LabelData.LabelName),
             ' ');
    LabelFeld^[A]^.LabelData.LabelName := HString;

    write(LabelDatafile,LabelFeld^[A]^.LabelData);
    Dispose(LabelFeld^[A]);
  END;

  for A := 1 to PAnzahl do
  BEGIN
    HString := PfadFeld^[A]^.PfadData.PfadName;
    FillChar(PfadFeld^[A]^.PfadData.PfadName,
             SizeOf(PfadFeld^[A]^.PfadData.PfadName),
             ' ');
    PfadFeld^[A]^.PfadData.PfadName := HString;

    write(PfadDatafile,PfadFeld^[A]^.PfadData);
    Dispose(PfadFeld^[A])
  END;

  for A := 1 to DAnzahl do
  BEGIN
    HString := DateiFeld^[A]^.DateiData.DateiName;
    FillChar(DateiFeld^[A]^.DateiData.DateiName,
             SizeOf(DateiFeld^[A]^.DateiData.DateiName),
             ' ');
    DateiFeld^[A]^.DateiData.DateiName := HString;

    write(DateiDatafile,DateiFeld^[A]^.DateiData);
    Dispose(DateiFeld^[A])
  END;

  ReleaseArray(LabelFeld  , 4, LAnzahl, EMSLF);
  ReleaseArray(PfadFeld   , 4, PAnzahl, EMSPF);
  ReleaseArray(DateiFeld  , 4, DAnzahl, EMSDF);
  ReleaseArray(StrucktFeld, 4, SAnzahl, EMSSF);
  ReleaseArray(ZusatzFeld , 4, ZAnzahl, EMSZF);

  CloseallFiles;

END;


Procedure LadeDateien;
var
    StrucktFeld : StrucktFeldPtrtyp;
    LabelFeld   : LabelFeldPtrtyp;
    PfadFeld    : PfadFeldPtrtyp;
    DateiFeld   : DateiFeldPtrtyp;
    ZusatzFeld  : ZusatzFeldPtrtyp;

    Lauf         : HilfListPtrtyp;





Procedure LadeHilf;
BEGIN
  Anzahl := Filesize(HilfDatafile);
  if Anzahl <> 0 then
  BEGIN
    new(HilfList);
    HilfList^.Back := Nil;
    HilfListend := HilfList;
    AltHilf := Nil;
    For A := 1 to Anzahl do
    BEGIN
      Read(HilfDatafile,HilfListEnd^.HilfData);
      If Not (HilfListEnd^.HilfData.Art in [3,4]) then
        Wait('Beim Laden der Hilf - Datei wurde ein Fehler gefunden ( Art <> 3, 4 )!');
      New(HilfListend^.Next);
      HilfListend^.Next^.Back := HilfListEnd;
      HilfListend := HilfListend^.Next
    END;
    HilfListend := HilfListend^.Back;
    Dispose(HilfListend^.Next);
    HilfListend^.Next := Nil
  end
END;

Procedure LadeStruckt;
BEGIN
  SAnzahl := Filesize(StrucktDatafile);

  if SAnzahl <> 0 then
  BEGIN
    NewArray(StrucktFeld,4,SAnzahl);
    For A := 1 to SAnzahl do
    BEGIN
      new(StrucktFeld^[A]);
      read(StrucktDatafile,StrucktFeld^[A]^.StrucktData);
    end
  end
END;

Procedure WandleHilf;
BEGIN
  Lauf := HilfList;
  while Lauf <> Nil do
  BEGIN
    case Lauf^.HilfData.art of
     3  : BEGIN
            Stelle := Lauf^.HilfData.DLabelList;
            Lauf^.HilfData.art := 1;
            Lauf^.HilfData.LabelList := StrucktFeld^[Stelle]
          END;
     4  : BEGIN
            Stelle := Lauf^.HilfData.DPfadList;
            Lauf^.HilfData.art := 2;
            Lauf^.HilfData.PfadList := StrucktFeld^[Stelle]
          end
    Else Wait('Hilfelement <> 3, 4!')
    END;
    Lauf := Lauf^.Next
  end
END;

Procedure LadeZusatz;
BEGIN
  ZAnzahl := Filesize(ZusatzDatafile);
  if ZAnzahl <> 0 then
  BEGIN
    newarray(ZusatzFeld,4,ZAnzahl);
    For A := 1 to ZAnzahl do
    BEGIN
      new(ZusatzFeld^[A]);
      read(ZusatzDatafile,ZusatzFeld^[A]^.ZusatzData)
    end
  end
END;

Procedure WandleZusatz;
BEGIN
  For A := 1 to ZAnzahl do
  BEGIN
    ZusatzFeld^[A]^.ZusatzData.sl :=
    StrucktFeld^[Longint(ZusatzFeld^[A]^.ZusatzData.sl)];
    ZusatzFeld^[A]^.ZusatzData.sp :=
    StrucktFeld^[Longint(ZusatzFeld^[A]^.ZusatzData.sp)]
  end
END;

Procedure LadeLabel;
BEGIN
  LAnzahl := Filesize(LabelDatafile);
  if LAnzahl <> 0 then
  BEGIN
    newarray(LabelFeld,4,LAnzahl);
    For A := 1 to LAnzahl do
    BEGIN
      new(LabelFeld^[A]);
      read(LabelDatafile,LabelFeld^[A]^.LabelData)
    end
  end
END;

Procedure LadePfad;
BEGIN
  PAnzahl := Filesize(PfadDatafile);
  if PAnzahl <> 0 then
  BEGIN
    newarray(PfadFeld,4,PAnzahl);
    For A := 1 to PAnzahl do
    BEGIN
      new(PfadFeld^[A]);
      read(PfadDatafile,PfadFeld^[A]^.PfadData)
    end
  end
END;

Procedure LadeDatei;
BEGIN
  DAnzahl := Filesize(DateiDatafile);
  if DAnzahl <> 0 then
  BEGIN
    newarray(DateiFeld,4,DAnzahl);
    For A := 1 to DAnzahl do
    BEGIN
      new(DateiFeld^[A]);
      read(DateiDatafile,DateiFeld^[A]^.DateiData);
    end
  end
END;

Procedure WandleStruckt;
BEGIN
  For A := 1 to SAnzahl do
  BEGIN
    case StrucktFeld^[A]^.StrucktData.art of
     4  : BEGIN
            Stelle := StrucktFeld^[A]^.StrucktData.DLabelList;
            StrucktFeld^[A]^.StrucktData.art := 1;
            StrucktFeld^[A]^.StrucktData.LabelList := LabelFeld^[Stelle]
          END;
     5  : BEGIN
            Stelle := StrucktFeld^[A]^.StrucktData.DPfadList;
            StrucktFeld^[A]^.StrucktData.art := 2;
            StrucktFeld^[A]^.StrucktData.PfadList := PfadFeld^[Stelle]
          END;
     6  : BEGIN
            Stelle := StrucktFeld^[A]^.StrucktData.DDateiList;
            StrucktFeld^[A]^.StrucktData.art := 3;
            StrucktFeld^[A]^.StrucktData.DateiList := DateiFeld^[Stelle]
          end
    end
  end
END;

Procedure WandleDatei;
BEGIN
  For A := 1 to DAnzahl do
  BEGIN
    Stelle := Longint(DateiFeld^[A]^.DateiData.Loc);
    DateiFeld^[A]^.DateiData.Loc := ZusatzFeld^[Stelle]
  end
END;

Var Hilf: StrucktListPtrtyp;

BEGIN
  MessageBox('OpenAllFiles.', nil, mfOkButton);
  OpenAllFiles;

  MessageBox('LadeHilf.', nil, mfOkButton);
  LadeHilf;     (* Als Liste *)

  MessageBox('LadeStruckt.', nil, mfOkButton);
  LadeStruckt;  (* Als Feld  *)

  MessageBox('WandleHilf.', nil, mfOkButton);
  WandleHilf;

  MessageBox('LadeZusatz.', nil, mfOkButton);
  LadeZusatz;   (* Als Feld  *)

  MessageBox('WandleZusatz.', nil, mfOkButton);
  WandleZusatz;

  MessageBox('LadeLabel.', nil, mfOkButton);
  LadeLabel;  (* Als Feld  *)

  MessageBox('LadePfad.', nil, mfOkButton);
  LadePfad;   (* Als Feld  *)

  MessageBox('LadeDatei.', nil, mfOkButton);
  LadeDatei;  (* Als Feld  *)

  MessageBox('WandleStruckt.', nil, mfOkButton);
  WandleStruckt;

  MessageBox('WandleDatei.', nil, mfOkButton);
  WandleDatei;


(* --- Verketten der Strucktliste --- *)

  MessageBox('Verkette Strukturliste', nil, mfOkButton);

  For A := 1 to SAnzahl-1 do
  BEGIN
    StrucktFeld^[A]^.Next := StrucktFeld^[A+1];
    StrucktFeld^[A+1]^.Back := StrucktFeld^[A]
  END;
  StrucktFeld^[1]^.Back := Nil;
  StrucktFeld^[SAnzahl]^.Next := Nil;


  StrucktList := StrucktFeld^[1];
  StrucktListend := StrucktFeld^[SAnzahl];

  ReleaseArray(StrucktFeld,4,SAnzahl, EMSSF);
  StrucktFeld := Nil;



(* ---    Umwandeln des Restes    --- *)

  MessageBox('Verkette rest.', nil, mfOkButton);
  For A := 1 to ZAnzahl-1 do
  BEGIN
    ZusatzFeld^[A]^.Next := ZusatzFeld^[A+1];
    ZusatzFeld^[A+1]^.Back := ZusatzFeld^[A]
  END;
  ZusatzFeld^[1]^.Back := Nil;
  ZusatzFeld^[ZAnzahl]^.Next := Nil;

  ZusatzList := ZusatzFeld^[1];
  ZusatzListend := ZusatzFeld^[ZAnzahl];

  ReleaseArray(ZusatzFeld,4,ZAnzahl, EMSZF);
  ZusatzFeld := Nil;



  (* Mache die BÑume Label, Pfad und Datei: *)


  LabelList := Nil;
  PfadList := Nil;
  DateiList := Nil;
  LabelDif := False;
  PfadDif := False;
  DateiDif := False;
  NoNew := True;

  GlobalFeldPtr := Pointer(DateiFeld);

(*  MachBinBaum(GlobalListPtrtyp(DateiList),1,DAnzahl);*)

  Hilf := StrucktList;
  While StrucktList <> Nil do
  Begin
    Case StrucktList^.StrucktData.Art of
      1 : Begin
            TreePointer := StrucktList^.StrucktData.LabelList;
            SpeichereLabel(StrucktList^.
                           StrucktData.LabelList^.
                           LabelData.LabelName,
                           LabelList,
                           LabelDif,
                           NoNew);
            StrucktList^.StrucktData.LabelList := Labeladress
          End;
      2 : Begin
            TreePointer := StrucktList^.StrucktData.PfadList;
            SpeicherePfad (StrucktList^.
                           StrucktData.PfadList^.
                           PfadData.PfadName,
                           PfadList,
                           PfadDif,
                           NoNew);
            StrucktList^.StrucktData.PfadList := Pfadadress
          End;
      3 : Begin
            TreePointer := StrucktList^.StrucktData.DateiList;
            SpeichereDatei(StrucktList^.
                           StrucktData.DateiList^.
                           DateiData.DateiName,
                           DateiList,
                           DateiDif,
                           NoNew);
            StrucktList^.StrucktData.DateiList := Dateiadress
          End;
    End;
    StrucktList := StrucktList^.Next
  End;
  StrucktList := Hilf;

  ReleaseArray(DateiFeld,4,DAnzahl, EMSDF);
  DateiFeld := Nil;

  ReleaseArray(PfadFeld,4,PAnzahl, EMSPF);
  PfadFeld := Nil;

  ReleaseArray(LabelFeld,4,LAnzahl, EMSLF);
  LabelFeld := Nil;

  NoNew := False;


  CloseallFiles;
  MessageBox('Fertig.', nil, mfOkButton);

END;


Procedure InitFileNames;
Begin
  If Pos('.', Name) <> 0 then
  Begin
    LabelFileName   := 'LABEL'   + Name;
    PfadFileName    := 'PFAD'    + Name;
    DateiFileName   := 'DATEI'   + Name;
    StrucktFileName := 'STRUCKT' + Name;
    HilfFileName    := 'HILF'    + Name;
    ZusatzFileName  := 'ZUSATZ'  + Name
  End
  Else
  Begin
    LabelFileName   := Name + '.DVL';
    PfadFileName    := Name + '.DVP';
    DateiFileName   := Name + '.DVD';
    StrucktFileName := Name + '.DVS';
    HilfFileName    := Name + '.DVH';
    ZusatzFileName  := Name + '.DVZ'
  End
End;


Procedure Initfiles;
BEGIN
  assign(StrucktDatafile,Strucktfilename);
  assign(HilfDatafile,Hilffilename);
  assign(DateiDatafile,Dateifilename);
  assign(LabelDatafile,Labelfilename);
  assign(PfadDatafile,Pfadfilename);
  assign(ZusatzDatafile,Zusatzfilename);
END;

Procedure SetAllNil;
BEGIN
  StrucktList := Nil;
  StrucktListend := Nil;
  AltStruckt := Nil;

  HilfList := Nil;
  HilfListend := Nil;
  AltHilf := Nil;

  ZusatzList := Nil;
  ZusatzListend := Nil;

  LabelList := Nil;
  PfadList := Nil;
  DateiList := Nil;

  DateiDif := False;
  PfadDif := False;
  LabelDif := False;
  Nachgetragen := True
END;


Procedure CloseallFiles;
BEGIN
  close(StrucktDatafile);
  close(HilfDatafile);
  close(LabelDatafile);
  close(PfadDatafile);
  close(DateiDatafile);
  close(ZusatzDatafile)
END;

Procedure OpenallFiles;
BEGIN
  ZusatzDataDateiOeffnen(ZusatzDatafile);
  DateiDataDateiOeffnen(DateiDatafile);
  PfadDataDateiOeffnen(PfadDatafile);
  LabelDataDateiOeffnen(LabelDatafile);
  HilfDataDateiOeffnen(HilfDatafile);
  StrucktDataDateiOeffnen(StrucktDatafile)
END;



Procedure TestMaskenSyntax(Var Maske: String);
var Ext : String[4];

Procedure BearbeiteString(var Mask: NameStr);

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

Procedure BearbeiteExtender(var Mask: ExtStr);

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
  BearbeiteExtender(Extender);
  Maske := Befehl+Extender
END;

Function PasstinMaske(Maske, Name: PathStr):Boolean;
var Extender,
    Extender1: ExtStr;
    Befehl,
    Befehl1  : NameStr;
    Pfad,
    Pfad1    : DirStr;
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




Procedure writexy;
begin
  gotoxy(x,y);
  write(txt)
end;


Procedure Wait;
begin
  MessageBox(txt, nil, mfOkButton)
end;


Procedure CursorAus;
begin
  Inline($B4/$01/$B9/$00/$0F/$CD/$10);
end;

Procedure NewArray;
var AnyPointer : ^Integer Absolute Pointer;

begin
  getmem(AnyPointer,ElementSize * Anzahl);
end;

Procedure DisposeArray;
var AnyPointer : ^Integer Absolute Pointer;

begin
  freemem(AnyPointer,ElementSize * Anzahl);
end;

Procedure GetMedium;
var Help : Mediumtyp;
begin
    Help := MediumFeld[ch];
    With MediumFeld[ch] do
    begin
      s  := StrucktList;
      es := StrucktListend;
      as := AltStruckt;

      h  := HilfList;
      eh := HilfListend;
      ah := AltHilf;

      l  := LabelList;
      p  := PfadList;
      d  := DateiList;
      na := Nachgetragen
    end;
    MediumFeld[ch].n := N;
    With Help do
    begin
      StrucktList      := s;
      StrucktListend  := es;
      AltStruckt       := as;

      HilfList         := h;
      HilfListend     := eh;
      AltHilf          := ah;

      LabelList        := l;
      PfadList         := p;
      DateiList        := d;
      Nachgetragen      := na
    end;
    N := Help.n;
    Raus := ch
end;



Procedure SpeichereDatei;
var p1, p2 : DateiListPtrtyp;
begin
  if Lauf = Nil then
  begin

    (* Nur balangsieren? *)

    If NoNew then Lauf := TreePointer
    Else
      new(lauf);


    Dateiadress := Lauf;
    dif := true;
    with lauf^ do
    begin
      DateiData.DateiName := DateiName;
      Links := Nil;
      Rechts := Nil;
      If Not NoNew then
        DateiData.Loc := Nil;
      DateiData.Bal := 0;
      DateiData.Anzahl := 1
    end
  end

  else

  if DateiName < Lauf^.DateiData.DateiName then
  begin
    SpeichereDatei(DateiName,lauf^.links,dif, NoNew);

    if dif then
    case lauf^.DateiData.bal of

    1 : begin lauf^.DateiData.bal := 0; dif := false end;

    0 : lauf^.DateiData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links;
          if p1^.DateiData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts := lauf;
            lauf^.DateiData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts;
            p1^.rechts := p2^.links;
            p2^.links := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts := lauf;

            if p2^.DateiData.bal = -1 then lauf^.DateiData.bal := 1
                            else lauf^.DateiData.bal := 0;

            if p2^.DateiData.bal = 1  then P1^.DateiData.bal := -1
                            else P1^.DateiData.bal := 0;
            lauf := p2;
          end;
          lauf^.DateiData.bal := 0;
          Dif := false
        end
      end
  end

  else

  if DateiName > Lauf^.DateiData.DateiName then
  begin
    SpeichereDatei(DateiName,lauf^.rechts,dif, NoNew);

    if dif then
    case lauf^.DateiData.bal of
    -1 : begin lauf^.DateiData.bal := 0; dif := false end;

    0 : lauf^.DateiData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts;
         if p1^.DateiData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links := lauf;
           lauf^.DateiData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links;
           p1^.links := p2^.rechts;
           p2^.rechts := p1;
           lauf^.rechts := p2^.links;
           p2^.links := lauf;

           if p2^.DateiData.bal = +1 then lauf^.DateiData.bal := -1
                           else lauf^.DateiData.bal := 0;

           if p2^.DateiData.bal = -1 then P1^.DateiData.bal := 1
                           else P1^.DateiData.bal := 0;
           lauf := p2;
         end;

         lauf^.DateiData.bal := 0;
         Dif := false
       end
     end
  end
  else
  begin
    Dateiadress := Lauf;
    Inc(Lauf^.DateiData.Anzahl);
    Dif := False
  end
end;


Procedure SpeichereLabel;
var p1, p2 : LabelListPtrtyp;
begin
  if Lauf = Nil then
  begin

    (* Nur balangsieren? *)

    If NoNew then Lauf := TreePointer
    Else
      new(lauf);


    Labeladress := Lauf;
    dif := true;
    with lauf^ do begin
      LabelData.LabelName := LabelName;
      links := Nil;
      rechts := Nil;
      LabelData.bal := 0;
      LabelData.Anzahl := 1
    end
  end

  else

  if LabelName = Lauf^.LabelData.LabelName then
  begin
    Labeladress := Lauf;
    Inc(Lauf^.LabelData.Anzahl);
    Dif := False
  end

  else

  if LabelName < Lauf^.LabelData.LabelName then
  begin
    SpeichereLabel(LabelName,lauf^.links,dif, NoNew);

    if dif then
    case lauf^.LabelData.bal of

    1 : begin lauf^.LabelData.bal := 0; dif := false end;

    0 : lauf^.LabelData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links;
          if p1^.LabelData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts := lauf;
            lauf^.LabelData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts;
            p1^.rechts := p2^.links;
            p2^.links := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts := lauf;

            if p2^.LabelData.bal = -1 then lauf^.LabelData.bal := 1
                            else lauf^.LabelData.bal := 0;

            if p2^.LabelData.bal = 1  then P1^.LabelData.bal := -1
                            else P1^.LabelData.bal := 0;
            lauf := p2;
          end;
          lauf^.LabelData.bal := 0;
          Dif := false
        end
      end
  end

  else

  if LabelName > Lauf^.LabelData.LabelName then
  begin
    SpeichereLabel(LabelName, lauf^.rechts,dif, NoNew);

    if dif then
    case lauf^.LabelData.bal of
    -1 : begin lauf^.LabelData.bal := 0; dif := false end;

    0 : lauf^.LabelData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts;
         if p1^.LabelData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links := lauf;
           lauf^.LabelData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links;
           p1^.links := p2^.rechts;
           p2^.rechts := p1;
           lauf^.rechts := p2^.links;
           p2^.links := lauf;

           if p2^.LabelData.bal = +1 then lauf^.LabelData.bal := -1
                           else lauf^.LabelData.bal := 0;

           if p2^.LabelData.bal = -1 then P1^.LabelData.bal := 1
                           else P1^.LabelData.bal := 0;
           lauf := p2;
         end;

         lauf^.LabelData.bal := 0;
         Dif := false
       end
     end
  end
end;



Procedure SpeicherePfad;
var p1, p2 : PfadListPtrtyp;
begin
  if Lauf = Nil then
  begin

    (* Nur balangsieren? *)

    If NoNew then Lauf := TreePointer
    Else
      new(lauf);


    Pfadadress := Lauf;
    dif := true;
    with lauf^ do
    begin
      PfadData.PfadName := PfadName;
      Links := Nil;
      Rechts := Nil;
      PfadData.Bal := 0;
      PfadData.Anzahl := 1
    end
  end

  else

  if PfadName = Lauf^.PfadData.PfadName then
  begin
    Pfadadress := Lauf;
    Inc(Lauf^.PfadData.Anzahl);
    Dif := False
  end

  else

  if PfadName < Lauf^.PfadData.PfadName then
  begin
    SpeicherePfad(PfadName,lauf^.links,dif, NoNew);

    if dif then
    case lauf^.PfadData.bal of

    1 : begin lauf^.PfadData.bal := 0; dif := false end;

    0 : lauf^.PfadData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links;
          if p1^.PfadData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts := lauf;
            lauf^.PfadData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts;
            p1^.rechts := p2^.links;
            p2^.links := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts := lauf;

            if p2^.PfadData.bal = -1 then lauf^.PfadData.bal := 1
                            else lauf^.PfadData.bal := 0;

            if p2^.PfadData.bal = 1  then P1^.PfadData.bal := -1
                            else P1^.PfadData.bal := 0;
            lauf := p2;
          end;
          lauf^.PfadData.bal := 0;
          Dif := false
        end
      end
  end

  else

  if PfadName > Lauf^.PfadData.PfadName then
  begin
    SpeicherePfad(PfadName,lauf^.rechts,dif, NoNew);

    if dif then
    case lauf^.PfadData.bal of
    -1 : begin lauf^.PfadData.bal := 0; dif := false end;

    0 : lauf^.PfadData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts;
         if p1^.PfadData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links := lauf;
           lauf^.PfadData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links;
           p1^.links := p2^.rechts;
           p2^.rechts := p1;
           lauf^.rechts := p2^.links;
           p2^.links := lauf;

           if p2^.PfadData.bal = +1 then lauf^.PfadData.bal := -1
                           else lauf^.PfadData.bal := 0;

           if p2^.PfadData.bal = -1 then P1^.PfadData.bal := 1
                           else P1^.PfadData.bal := 0;
           lauf := p2;
         end;

         lauf^.PfadData.bal := 0;
         Dif := false
       end
     end
  end
end;


Procedure GetStruckt;
begin
  if Ende = nil then
  begin
    new(Ende);
    StrucktList := Ende;
    Ende^.Next := Nil;
    Ende^.Back := Nil
  end
  else
  begin
    if cut <> Nil then
    begin
      (* Liste getrennt: *)

      (* Ende mit Cut verbinden: *)
      Ende^.Next := Cut;
      Cut^.Back := Ende;

      (* Ende weiterrÅcken: *)
      Ende := Ende^.Next;
      (* Cut weiterrÅcken: *)
      Cut := Cut^.Next;

      (* Und vor Cut wieder trennen: *)
      Cut^.Back := Nil;
      Ende^.Next := Nil

    end
    else
    begin
      (* Liste nicht getrennt: *)
      (* Cut = Nil             *)

      New(Ende^.Next);
      Ende^.Next^.Back := Ende;
      Ende := Ende^.Next;
      Ende^.Next := Nil
    end
  end
end;


Procedure GetHilf;
begin
  if Ende = nil then
  begin
    new(Ende);
    HilfList := Ende;
    Ende^.Next := Nil;
    Ende^.Back := Nil
  end
  else
  begin
    if cut <> Nil then
    begin
      (* Liste getrennt: *)

      (* Ende mit Cut verbinden: *)
      Ende^.Next := Cut;
      Cut^.Back := Ende;

      (* Ende weiterrÅcken: *)
      Ende := Ende^.Next;
      (* Cut weiterrÅcken: *)
      Cut := Cut^.Next;

      (* Und vor Cut wieder trennen: *)
      Cut^.Back := Nil;
      Ende^.Next := Nil

    end
    else
    begin
      (* Liste nicht getrennt: *)
      (* Cut = Nil             *)

      New(Ende^.Next);
      Ende^.Next^.Back := Ende;
      Ende := Ende^.Next;
      Ende^.Next := Nil
    end
  end
end;

Procedure SpeichereSuch;
var Help : ZusatzListPtrtyp;
begin
  if dp^.DateiData.Loc = Nil then
  begin
    (* ôffnen und zuweisen: *)
    new(Help);
    Help^.ZusatzData.sl := lp;
    Help^.ZusatzData.sp := pp;

    (* Verbinden mit DateiElement: *)
    dp^.DateiData.Loc := Help;

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
    Help^.ZusatzData.sl := lp;
    Help^.ZusatzData.sp := pp;

    (* Hinter anderem Element einfÅgen: *)


    Help^.Back := dp^.DateiData.Loc;
    Help^.Next := dp^.DateiData.Loc^.Next;

    (* Wenn es das letzte Element ist, dann: *)

    if dp^.DateiData.Loc^.Next = Nil then
    Begin
      dp^.DateiData.Loc^.Next := Help;
      ZusatzListend := dp^.DateiData.Loc;
    End
    else
    begin
      dp^.DateiData.Loc^.Next^.Back := Help;
      dp^.DateiData.Loc^.Next := Help
    end

  end
end;

Function TestFiles;
begin
  TestFiles := True;
  {$I-}
  reset(StrucktDatafile);
  if IOResult <> 0 then TestFiles := False;
  reset(HilfDatafile);
  if IOResult <> 0 then TestFiles := False;
  reset(LabelDatafile);
  if IOResult <> 0 then TestFiles := False;
  reset(PfadDatafile);
  if IOResult <> 0 then TestFiles := False;
  reset(DateiDatafile);
  if IOResult <> 0 then TestFiles := False
  {$I+}
end;


Procedure NewFileName;
var Info: ^String;
BEGIN
  New(Info);
  Repeat
    info^ := FileName;
    If ExecDialog(ReadString(Name), Info)
        = cmCancel then Exit;
    If Info^ = '' then
    Begin
      Wait('Sie mÅssen mindestens einen Buchstaben eingeben!')
    End
    Else
    If Pos('.', Info^) <> 0 then
    Begin
      If Length(Info^) > 4 then
      Begin
        Wait('Ihr Extender hat mit Punkt mehr als 4 Zeichen!');
        Info^ := ''
      End
      Else
      Begin
        If Length(Info^) = 1 then
        Begin
          Wait('Sie sollten schon einen Extender angeben, ' +
               'da Sie die Dateien so leichter erkennen.');
          Info^ := ''
        End
      End
    End
    Else
    Begin
      If Length(Info^) > 8 then
      Begin
        Wait('Ihr Dateiname hat mehr als 8 Zeichen!');
        Info^ := ''
      End
      Else
      Begin
        If (UpDate(Info^) = 'DATEI')   or
           (UpDate(Info^) = 'PFAD')    or
           (UpDate(Info^) = 'LABEL')   or
           (UpDate(Info^) = 'STRUCKT') or
           (UpDate(Info^) = 'HILF')    or
           (UpDate(Info^) = 'ZUSATZ') then
        Begin
          Wait('Volgende Namen dÅrfen Sie nicht angeben:' + Chr(13) +
               'Datei,' + Chr(13) +
               'Pfad,' + Chr(13) +
               'Label,' + Chr(13) +
               'Struckt,' + Chr(13) +
               'Hilf und' + Chr(13) +
               'Zusatz.');
          Info^ := ''
        End
      End
    End;
  Until Info^ <> '';
  FileName := Info^;

  InitFileNames(FileName);
  InitFiles;

  Dispose(Info)
END;



Procedure DateiDataDateiOeffnen;
begin
  (*$I-*)
  reset(Datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Index-Datei kann nicht geîffnet werden !');
    delay(1000);
    Halt
  end
  (*$I+*)
end;

Procedure PfadDataDateiOeffnen;
begin
  (*$I-*)
  reset(Datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Pfad-Datei kann nicht geîffnet werden !');
    delay(1000);
    halt
  end
  (*$I+*)
end;

Procedure LabelDataDateiOeffnen;
Var IOR: Integer;
begin
  (*$I-*)
  reset(datei);
  if IOResult <> 0 then rewrite(Datei);
  IOR := IOResult;
  if IOR <> 0 then
  begin
    Wait('Label-Datei kann nicht geîffnet werden, Fehler: ' + GetString(IOR));
    halt
  end
  (*$I+*)
end;
Procedure ZusatzDataDateiOeffnen;
begin
  (*$I-*)
  reset(datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Zusatz-Datei kann nicht geîffnet werden !');
    delay(1000);
    halt
  end
  (*$I+*)
end;
Procedure HilfDataDateiOeffnen;
begin
  (*$I-*)
  reset(datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Hilf-Datei kann nicht geîffnet werden !');
    delay(1000);
    halt
  end
  (*$I+*)
end;

Procedure StrucktDataDateiOeffnen;
begin
  (*$I-*)
  reset(datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Struckt-Datei kann nicht geîffnet werden !');
    delay(1000);
    halt
  end
  (*$I+*)
end;

Procedure Fehler;
begin
  gotoxy(1,24);
  write(txt);
  ch := upcase(readkey);
  gotoxy(1,24);
  clreol
end;

Function UpDate;
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


Procedure List;
var sr          : searchrec;
    h           : Integer;
begin
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

  if (pfad[Length(pfad)] <> ':') or w then pfad := pfad + '\';

  SpeicherePfad(Pfad, PfadList, PfadDif, NoNew);

  (* Neues Element in Strucktliste erzeugen: *)

  GetStruckt(StrucktListend,AltStruckt);

  (* FÅr Zusatzdaten: *)
  Pfadzeiger := StrucktListEnd;

  (* Zuweisung in das Element: *)

  StrucktListend^.StrucktData.art := 2;
  StrucktListend^.StrucktData.PfadList := pfadadress;

  (* Neues Element in Hilfliste erzeugen: *)

  GetHilf(HilfListend,AltHilf);

  (* Zuweisung in das Element: *)

  HilfListend^.HilfData.art := 2;
  HilfListend^.HilfData.PfadList := Pointer(StrucktListend);



  FindFirst(pfad+mask,attr,sr);
  while DosError = 0 do
  begin
    if (sr.attr and (directory + VolumeID)) = 0 then
    begin
      SpeichereDatei(sr.name, DateiList, DateiDif, NoNew);

      (* Neues Element in Strucktliste erzeugen: *)

      GetStruckt(StrucktListend,AltStruckt);

      (* Zuweisung in das Element: *)

      StrucktListend^.StrucktData.art := 3;
      StrucktListend^.StrucktData.DateiList := Dateiadress;

      SpeichereSuch(Labelzeiger,Pfadzeiger,Dateiadress)
    end;
    FindNext(sr)
  end;

  FindFirst(pfad+'*.*',attr,sr);
  while DosError = 0 do
  begin
    if (sr.attr and Directory) <> 0 then
      if sr.name <> '.' then
        if sr.name <> '..' then list(pfad+sr.name);
    FindNext(sr)
  end
end;



Procedure Dir;
Label Exit1;
Var Info : ^String;
    F    : File;
begin
  MemWasLo := False;
  Counter := 0;

  Pfad := '';
  Befehl := '';
  Extender := '';
  DirStatus := True;
  Attr := $3F;

  Fsplit(Mask,Pfad,Befehl,Extender);

  if Length(Pfad) <> 0 then Delete(Mask,1,Length(Pfad));
  W := False;
  if (Pos(':\',Pfad) = 2) and
     (Length(Pfad) = 3) then W := True;

  if pfad[Length(pfad)] = '\' then delete(pfad,length(pfad),1);
  s := 0;

  FindFirst(Pfad + Befehl + Extender, $08, srec);
  lab := srec.name;

  If Pos('DISK', Lab) = 0 then
  Begin
    Repeat
      New(Info);
      Info^ := 'DISK';
      If ExecDialog(ReadString('DatentrÑger hat noch keine Disk Nr. (Disk...) ?'), Info) = cmCancel
        then Exit;
      Info^ := UpDate(Info^)
    Until (Info^ <> '') and
          (Pos('DISK', Info^) <> 0);
    Lab := Info^;
    Dispose(Info);
    Info := Nil;
    If ExecDialog(AskYesNo('Label Ñndern ?'), Info) = cmOk then
    Begin
      DoneSysError;
      DoneEvents;
      DoneVideo;
      DoneMemory;
      SetMemTop (HeapPtr);
      SwapVectors;

      Writeln('éndere Label mit Dos - Befehl:');
      Exec(GetEnv('COMSPEC'), '/C Label A:' + Lab);

      SwapVectors;
      SetMemTop (HeapEnd);
      InitMemory;
      InitVideo;
      InitEvents;
      InitSysError;
      Application^.Redraw;
    End
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

    FindFirst(Pfad + Befehl + Extender, AnyFile, srec);
    if Doserror = 0 then
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

      list(UpDate(pfad))
    end

    else
    begin
      MessageBox('Diskette leer!', nil, mfOkButton);
      DirStatus := False
    end
  end

  else

  begin
    MessageBox('Diskette hat einen ungÅltigen Label!',
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

    BuildDataFiles(Counter)

  End
end;

Procedure CopyMinWork;
begin
  With M[x] do
  begin
    StrucktList := s;
    StrucktListend := es;
    AltStruckt := as;

    ZusatzList := z;
    ZusatzListend := ez;

    HilfList := h;
    HilfListend := eh;
    AltHilf := ah;

    LabelList := l;
    PfadList := p;
    DateiList := d;
    Nachgetragen := na
  end;
  N := M[x].N;
  DateiDif := M[x].DateiDif;
  PfadDif := M[x].PfadDif;
  LabelDif := M[x].LabelDif
end;

Procedure CopyWorkinM;
begin
  With M[x] do
  begin
    s  := StrucktList;
    es := StrucktListend;
    as := AltStruckt;

    z := ZusatzList;
    ez := ZusatzListend;

    h  := HilfList;
    eh := HilfListend;
    ah := AltHilf;

    l  := LabelList;
    p  := PfadList;
    d  := DateiList;
    na := Nachgetragen
  end;
  M[x].N := N;
  M[x].DateiDif := DateiDif;
  M[x].PfadDif := PfadDif;
  M[x].LabelDif := LabelDif
end;


end.
