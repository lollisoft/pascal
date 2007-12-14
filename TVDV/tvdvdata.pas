Unit TVDVDATA;  (* Kapselung der Daten, die Dateien etc. betreffen. *)

Interface

Uses Objects,

     LabHist,
     MsgBox,
     Dialogs,
     TvdvCmds,
     HelpCmds,
     HelpFile,
     Crt,
     Dos,
     Memory,
     GadGets,
     Drivers,
     Views,
     Menus,
     Editors,
   (* FÅr Editor: *)
     Editor,
     Strings,
     Colors,
     App,
     CursCont,
     StdDlg,
     StrTools,
     EMS,
     XMS,
     XHeap;

(****************************)

(* Deklarationen aus Video: *)

(****************************)





Const Counter : Integer = 0;

      MemWasLo: Boolean = False;

Type
(*************************************************************)
(*                                                           *)
(* Globale Datentypen werden z. B. beim Sortieren verwendet. *)
(*                                                           *)
(*************************************************************)
    String12            = String[12];

    LabelDatatyp        = Record
                            Bal       : Shortint;
                            Anzahl    : Byte;
                            LabelName : String12
                          end;

     GlobalListPtrtyp = ^GlobalListtyp;
     GlobalListXPtrtyp = Record
                           Ptr    : GlobalListPtrtyp;
                           BlockNr: Word
                         End;
     GlobalListtyp    = Record
                            Back,
                            Next    : GlobalListXPtrtyp;
                            Bal     : Shortint;
                            Anzahl  : Byte;
                          end;
     GlobalBaumPtrtyp = ^GlobalBaumtyp;
     GlobalBaumXPtrtyp = Record
                           Ptr    : GlobalBaumPtrtyp;
                           BlockNr: Word
                         End;
     GlobalBaumtyp    = Record
                            Links,
                            Rechts  : GlobalBaumXPtrtyp;
                            Bal     : Shortint;
                            Anzahl  : Byte;
                          end;
     ListenPtrtyp     = ^Listentyp;
     ListenXPtrtyp    = Record
                          Ptr    : ListenPtrtyp;
                          BlockNr: Word
                        End;
     Listentyp         = Record
                            Back,
                            Next  : ListenXPtrtyp
                          end;
     LabelListPtrtyp = ^LabelListtyp;
     LabelListXPtrtyp = Record
                          Ptr    : LabelListPtrtyp;
                          BlockNr: Word
                        End;

     LabelListtyp     = record

                            Links,

                            Rechts    : LabelListXPtrtyp;

                            LabelData : LabelDatatyp

                          end;



     PfadDatatyp      = Record

                            Bal       : Shortint;

                            Anzahl    : Byte;

                            PfadName  : String[70]

                          end;





     PfadListPtrtyp  = ^PfadListtyp;



     PfadListXPtrtyp = Record

                         Ptr    : PfadListPtrtyp;

                         BlockNr: Word

                       End;



     PfadListtyp      = record

                            Links,

                            Rechts    : PfadListXPtrtyp;

                            PfadData  : PfadDatatyp

                          end;



     DateiListPtrtyp = ^DateiListtyp;



     DateiListXPtrtyp = Record

                          Ptr    : DateiListPtrtyp;

                          BlockNr: Word

                        End;



     arttyp               = byte;   (* Label, Pfad, Datei *)


     StrucktDatatyp     = record

                            case art : arttyp of

                              1 : (LabelList   : LabelListXPtrtyp);

                              2 : (PfadList    : PfadListXPtrtyp);

                              3 : (DateiList   : DateiListXPtrtyp);

                              4 : (DLabelList : Longint);

                              5 : (DPfadList  : Longint);

                              6 : (DDateiList : Longint);

                          end;





     StrucktListPtrtyp  = ^StrucktListtyp;



     StrucktListXPtrtyp = Record

                            Ptr    : StrucktListPtrtyp;

                            BlockNr: Word

                          End;



     StrucktListtyp      = record

                               Back,

                               Next        : StrucktListXPtrtyp;

                               StrucktData : StrucktDatatyp

                             end;



     ZusatzListPtrtyp   = ^ZusatzListtyp;



     ZusatzListXPtrtyp  = Record

                            Ptr    : ZusatzListPtrtyp;

                            BlockNr: Word

                          End;



     ZusatzDatatyp         = record

                               sl,

                               sp  : StrucktListXPtrtyp

                             end;



     ZusatzListtyp       = record

                               Back,

                               Next: ZusatzListXPtrtyp;

                               ZusatzData: ZusatzDatatyp

                             end;



     DateiDatatyp       = record

                            Bal      : Shortint;

                            Anzahl   : byte;

                            DateiName: String12;

                            Loc      : ZusatzListXPtrtyp

                          end;









     HilfDatatyp           = record

                               case art : arttyp of

                                 1 : (LabelList : StrucktListXPtrtyp);

                                 2 : (PfadList  : StrucktListXPtrtyp);

                                 3 : (DLabelList : Longint);

                                 4 : (DPfadList  : Longint)

                               end;



     HilfListPtrtyp     = ^HilfListtyp;



     HilfListXPtrtyp  = Record

                          Ptr    : HilfListPtrtyp;

                          BlockNr: Word

                        End;



     HilfListtyp         = record

                               Back,

                               Next     : HilfListXPtrtyp;

                               HilfData : HilfDatatyp

                             end;







     DateiListtyp     = record

                            Links,

                            Rechts           : DateiListXPtrtyp;

                            DateiData        : DateiDatatyp

                          end;

     sltyp             = record
                           vp1,
                           vp2   : HilfListXPtrtyp;
                           vp3   : StrucktListXPtrtyp;
                            s1,
                            s2,
                            s3   : String
                          end;


    Importtyp           = String;
    ImportFiletyp       = File Of Importtyp;
    ImportDirTreePtrtyp = ^ImportDirTreetyp;
    ImportDirTreeXPtrtyp = Record
                             Ptr    : ImportDirTreePtrtyp;
                             BlockNr: Word
                           End;
    ImportDirTreetyp    = Record
                            Rechts,
                            Links  : ImportDirTreeXPtrtyp;
                            Zeiger : Longint;
                            Bal    : ShortInt
                          End;
    XPointer             = Record
                             Ptr    : Pointer;
                             BlockNr: Word
                           End;

    StrucktFiletyp    = File of StrucktDatatyp;
    HilfFiletyp       = File of HilfDatatyp;
    LabelFiletyp      = File of LabelDatatyp;
    PfadFiletyp       = File of PfadDatatyp;
    DateiFiletyp      = File of DateiDatatyp;
    ZusatzFiletyp     = File of ZusatzDatatyp;


Type GetDateiElementtyp = Function(Stelle: Longint): String;
Var  GetDateiElement : GetDateiElementtyp;


Type  TreePtrtyp = ^Treetyp;



      TreeXPtrtyp = Record

                      Ptr    : TreePtrtyp;

                      BlockNr: Word

                    End;



      Treetyp    = Record

                     Rechts,

                     Links  : TreeXPtrtyp;

                     Zeiger,

                     Stelle : Longint;

                     Bal    : Shortint

                   End;





Var
      MsgRes       : Word;


      ImportFile   : ImportFiletyp;

      ImportData   : Importtyp;

      ImportDirTree: ImportDirTreeXPtrtyp;



      HelpTreesOnFile: Boolean;



      Df,

      Pf,

      Lf,

      Sf,

      Hf,

      Zf: String;



      LabelTree,

      PfadTree,

      DateiTree : TreeXPtrtyp;



      DateiDif,

      PfadDif,

      LabelDif  : Boolean;



      SArg: String;





Procedure BuildDataFiles(Counter: Integer);

Procedure Suche(Tree: ImportDirTreeXPtrtyp; Var Result: XPointer);

Function SearchPfadLine(Pfad: String): Longint;

Function GetDirElement(Stelle: Longint): String;


Const

      NoNew: Boolean          = False;

      Dateifilename: string   = 'Datei.dvd';

      Pfadfilename: string    = 'Pfad.dvd';

      Labelfilename: String   = 'Label.dvd';

      Strucktfilename: string = 'Struckt.dvd';

      Hilffilename: string    = 'Hilf.dvd';

      Zusatzfilename: string  = 'Zusatz.dvd';



      FileName           : String12 = '.DVD';

      ImportFileName     : String12 = '.DIR';









Type











(* Noch nicht einwandfrei

   ( nicht ersichtlich, wieviele jeweils vorhanden sind ) *)



     Mediumtyp            = record

                               n         : String;

                               z, ez     : ZusatzListXPtrtyp;

                               s, es, as : StrucktListXPtrtyp;

                               h, eh, ah : HilfListXPtrtyp;

                               l         : LabelListXPtrtyp;

                               p         : PfadListXPtrtyp;

                               d         : DateiListXPtrtyp;

                               DateiDif,

                               PfadDif,

                               LabelDif,

                               na        : Boolean

                             end;

     MediumFeldtyp       = array ['A'..'J'] of Mediumtyp;



(* Dynamische Felder in denen jedes Element auf ein spezifischen   *)

(* Datensatz eines Listenelementes zeigt.                          *)



     SPointerArray         = Array [1..1] of StrucktListXPtrtyp;

     LPointerArray         = Array [1..1] of LabelListXPtrtyp;

     PPointerArray         = Array [1..1] of PfadListXPtrtyp;

     DPointerArray         = Array [1..1] of DateiListXPtrtyp;

     ZPointerArray         = Array [1..1] of ZusatzListXPtrtyp;



(* Dynamisches Feld, das Zeigeroperationen auf alle Åbrigen Felder *)

(* zulÑsst.                                                        *)



     GPointerArray         = Array [1..1] of GlobalListXPtrtyp;



(* Die Zeiger zu den dynamischen Feldern:                          *)



     StrucktFeldXPtrtyp  = ^SPointerArray;

     LabelFeldXPtrtyp    = ^LPointerArray;

     PfadFeldXPtrtyp     = ^PPointerArray;

     DateiFeldXPtrtyp    = ^DPointerArray;

     ZusatzFeldXPtrtyp   = ^ZPointerArray;

     GlobalFeldXPtrtyp   = ^GPointerArray;







(* Hilfsvariabeln, die fÅr das Einblenden der verketteten *)

(* Struckturen verwendet wird. Bsp. x^.y^.z^.Data         *)





     ListenDataXtyp= Record

                       Element: Listentyp;

                       XPtr   : ListenXPtrtyp

                     End;



     DateiDataXtyp = Record

                       DateiData: DateiDatatyp;

                       XPtr     : DateiListXPtrtyp

                     End;



     PfadDataXtyp  = Record

                       PfadData : PfadDatatyp;

                       XPtr     : PfadListXPtrtyp

                     End;



     LabelDataXtyp = Record

                       LabelData: LabelDatatyp;

                       XPtr     : LabelListXPtrtyp

                     End;



     StrucktDataXtyp = Record

                         StrucktData: StrucktDatatyp;

                         XPtr       : StrucktListXPtrtyp

                       End;



     HilfDataXtyp = Record

                         HilfData: HilfDatatyp;

                         XPtr    : HilfListXPtrtyp

                       End;



     ZusatzDataXtyp = Record

                         ZusatzData: ZusatzDatatyp;

                         XPtr      : ZusatzListXPtrtyp

                       End;


var

     StrucktFeld : StrucktFeldXPtrtyp;

     EMSSF       : Boolean;

     LabelFeld   : LabelFeldXPtrtyp;

     EMSLF       : Boolean;

     PfadFeld    : PfadFeldXPtrtyp;

     EMSPF       : Boolean;

     DateiFeld   : DateiFeldXPtrtyp;

     EMSDF       : Boolean;

     ZusatzFeld  : ZusatzFeldXPtrtyp;

     EMSZF       : Boolean;



     DelLabelFeld : LabelFeldXPtrtyp;

     EMSDLF       : Boolean;

     DelPfadFeld  : PfadFeldXPtrtyp;

     EMSDPF       : Boolean;

     DelDateiFeld : DateiFeldXPtrtyp;

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

     Raus,

     ch,

     cha              : Char;

     MediumFeld       : MediumFeldtyp;

     M                : Array [1..2] of Mediumtyp;

     Nachgetragen,

     w                : Boolean;

     ziffer,

     ende             : Char;

     mask             : String;

var  pfad             : DirStr;

var  befehl           : NameStr;

var  extender         : ExtStr;

     lab              : String;

     DiskNr           : String12;

     srec,

     sr               : Searchrec;

     s                : Real;

     attr             : Byte;



     Labelzeiger,

     Pfadzeiger       : StrucktListXPtrtyp;







     ZusatzList,

     ZusatzListend    : ZusatzListXPtrtyp;



     LabelList,

     Labeladress      : LabelListXPtrtyp;



     PfadList,

     Pfadadress       : PfadListXPtrtyp;



     DateiList,

     Dateiadress      : DateiListXPtrtyp;



     HilfList,

     HilfListend,

     HHilf,

     AltHilf          : HilfListXPtrtyp;



     StrucktList,

     StrucktListend,

     AltStruckt       : StrucktListXPtrtyp;



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

     PfadDataPufferfile     : PfadFiletyp;

     LabelDataPufferfile    : LabelFiletyp;

     DateiDataPufferfile    : DateiFiletyp;

     ZusatzDataPufferfile   : ZusatzFiletyp;





(* Feld, das benîtigt wird, um auf Zeiger aller Felder zugreifen zu kînnen. *)

(* Dieses Feld dient zur allgemeinen Umwandlungshilfe *)

(* Dies ist der Zeiger dazu:   *)



     GlobalFeldXPtr  : GlobalFeldXPtrtyp;



(* Globale Zeigerverkettung: *)



     GlobalListPtr  : GlobalListXPtrtyp;



     Regs           : Registers;

     CursorSave     : Word;











(* FileImport: *)

Var

    Name         : String;
    TextFile     : Text;
    IOError      : ShortInt;







(* Deklarationen von Video - Ende! *)





Type TData = ^Data;

     Data  = Object(TObject)

               Laufwerk     : String[12];

               Procedure GetStruckt (var Ende, cut: StrucktListXPtrtyp);
               Procedure GetHilf (var Ende, cut: HilfListXPtrtyp);

               Function GetStrucktFileName:String12;

               Function GetHilfFileName:String12;

               Function GetZusatzFileName:String12;

               Function GetLabelFileName:String12;

               Function GetPfadFileName:String12;

               Function GetDateiFileName:String12;


               Procedure SetStrucktFileName(Name: String12);

               Procedure SetHilfFileName(Name: String12);

               Procedure SetZusatzFileName(Name: String12);

               Procedure SetLabelFileName(Name: String12);

               Procedure SetPfadFileName(Name: String12);

               Procedure SetDateiFileName(Name: String12);


               Constructor Init;

               Destructor  Done; Virtual;


               Procedure SetAllNil;

               Procedure InitFileNames(Name: String12);

               Procedure Initfiles;

               Procedure OpenAllFiles;

               Procedure CloseAllFiles;



               Function  PasstInMaske(Maske, Name: String12):Boolean;

               Function  TestFiles:Boolean;

               Procedure NewFileName(Name: String; Var FileName: String12);

               Procedure ZaehleElemente(Root: GlobalBaumXPtrtyp);

               Procedure SetLaufwerk(L: String);

               Function  GetLaufwerk: String;



               Procedure CopyMinWork(x:byte);

               Procedure CopyWorkinM(x:byte);







               Procedure ListFromFile(pfad:string; Line: Longint);

               Procedure List(pfad:string);

               Procedure Dir(Var Name: String12);





               Procedure ImportDir;





               Procedure Import;

               Procedure ImportNachtrag;

               Procedure ImportPruefen;





               Procedure Scanner;

               Procedure DiskAS;

               Procedure DatenAufgeben;

               Procedure Nachtrag;

               Procedure Aufreum;

               Procedure LadeDateien;

               Procedure SpeichereDateien;

               Procedure DiskAutoScann;



               

               Procedure SpeicherePfad(PfadName : string;

                                       var Lauf : PfadListXPtrtyp;

                                       var dif  : Boolean;

                                           NoNew: Boolean);



               Procedure SpeichereDatei(DateiName : string12;

                                        var  Lauf : DateiListXPtrtyp;

                                        var  dif  : Boolean;

                                             NoNew: Boolean);



               Procedure SpeichereSuch(lp, pp: StrucktListXPtrtyp;

                                           dp: DateiListXPtrtyp);







               Procedure SpeichereLabel(LabelName : string12;

                                        var  Lauf : LabelListXPtrtyp;

                                        var  dif  : Boolean;

                                             NoNew: Boolean);





             End;



Const PData: TData = Nil;



Procedure DateiDataDateiOeffnen  (var Datei : DateiFiletyp);

Procedure LabelDataDateiOeffnen  (var Datei : LabelFiletyp);

Procedure PfadDataDateiOeffnen   (var Datei : PfadFiletyp);

Procedure StrucktDataDateiOeffnen(var Datei : StrucktFiletyp);

Procedure HilfDataDateiOeffnen   (var Datei : HilfFiletyp);

Procedure ZusatzDataDateiOeffnen (var Datei : ZusatzFiletyp);



Function ExecDialog(P: PDialog; Data: Pointer): Word;

Function ReadString(DialogTyp: String): PDialog;

Function AskYesNo(Ask: String): PDialog;

Function  GetFName: String;









Implementation


Var   TreePointer: XPointer;  (* Wird verwendet, wenn NoNew True ist.    *)
                             (* Dh. wenn der Baum nur balangsiert wird. *)







Function GetFName: String;

Var Name: String;

Begin

  Name := ParamStr(0);

  While Pos('.', Name) <> 0 do

    System.Delete(Name, Length(Name), 1);

  GetFName := Name

End;





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





Procedure Data.NewFileName;

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







Function Data.TestFiles;

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





Function Data.PasstinMaske(Maske, Name: String12):Boolean;

var Extender,
    Extender1: ExtStr;
var
    Befehl   : NameStr;
    Befehl1  : NameStr;
var
    Pfad,
    Pfad1    : DirStr;
var
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
       End
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





Procedure Data.ZaehleElemente;

BEGIN

  if Root.Ptr <> Nil then

  Begin

    ZaehleElemente(Root.Ptr^.Rechts);

    if Root.Ptr^.Anzahl > 0 then

      Inc(Anzahl)

    Else

      Inc(Deleted);

    ZaehleElemente(Root.Ptr^.Links)

  end

END;







Procedure Data.OpenallFiles;

BEGIN

  ZusatzDataDateiOeffnen(ZusatzDatafile);

  DateiDataDateiOeffnen(DateiDatafile);

  PfadDataDateiOeffnen(PfadDatafile);

  LabelDataDateiOeffnen(LabelDatafile);

  HilfDataDateiOeffnen(HilfDatafile);

  StrucktDataDateiOeffnen(StrucktDatafile)

END;



Procedure Data.CloseallFiles;

BEGIN

  close(StrucktDatafile);

  close(HilfDatafile);

  close(LabelDatafile);

  close(PfadDatafile);

  close(DateiDatafile);

  close(ZusatzDatafile)

END;







Procedure Data.SetStrucktFileName(Name: String12);

Begin

  StrucktFileName := Name

End;



Procedure Data.SetHilfFileName(Name: String12);

Begin

  HilfFileName := Name

End;



Procedure Data.SetZusatzFileName(Name: String12);

Begin

  ZusatzFileName := Name

End;



Procedure Data.SetLabelFileName(Name: String12);

Begin

  LabelFileName := Name

End;



Procedure Data.SetPfadFileName(Name: String12);

Begin

  PfadFileName := Name

End;



Procedure Data.SetDateiFileName(Name: String12);

Begin

  DateiFileName := Name

End;







Function Data.GetStrucktFileName: String12;

Begin

  GetStrucktFileName := StrucktFileName

End;



Function Data.GetHilfFileName: String12;

Begin

  GetHilfFileName := HilfFileName

End;



Function Data.GetZusatzFileName: String12;

Begin

  GetZusatzFileName := ZusatzFileName

End;



Function Data.GetLabelFileName: String12;

Begin

  GetLabelFileName := LabelFileName

End;



Function Data.GetPfadFileName: String12;

Begin

  GetPfadFileName := PfadFileName

End;



Function Data.GetDateiFileName: String12;

Begin

  GetDateiFileName := DateiFileName

End;



Procedure DateiDataDateiOeffnen;

begin

  (*$I-*)

  reset(Datei);

  if IOResult <> 0 then rewrite(Datei);

  if IOResult <> 0 then

  begin

    Wait('Index-Datei kann nicht geîffnet werden !');

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

    Wait('Pfad-Datei kann nicht geîffnet werden !');

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

    Wait('Zusatz-Datei kann nicht geîffnet werden !');

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

    Wait('Hilf-Datei kann nicht geîffnet werden !');

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

    Wait('Struckt-Datei kann nicht geîffnet werden !');

    halt

  end

  (*$I+*)

end;











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





Procedure Data.GetHilf;

begin

  if Ende.Ptr = nil then

  begin

    new(Ende.Ptr);

    HilfList := Ende;

    Ende.Ptr^.Next.Ptr := Nil;

    Ende.Ptr^.Back.Ptr := Nil

  end

  else

  begin

    if cut.Ptr <> Nil then

    begin

      (* Liste getrennt: *)



      (* Ende mit Cut verbinden: *)

      Ende.Ptr^.Next := Cut;

      Cut.Ptr^.Back := Ende;



      (* Ende weiterrÅcken: *)

      Ende := Ende.Ptr^.Next;

      (* Cut weiterrÅcken: *)

      Cut := Cut.Ptr^.Next;



      (* Und vor Cut wieder trennen: *)

      Cut.Ptr^.Back.Ptr := Nil;

      Ende.Ptr^.Next.Ptr := Nil



    end

    else

    begin

      (* Liste nicht getrennt: *)

      (* Cut = Nil             *)



      New(Ende.Ptr^.Next.Ptr);

      Ende.Ptr^.Next.Ptr^.Back := Ende;

      Ende := Ende.Ptr^.Next;

      Ende.Ptr^.Next.Ptr := Nil

    end

  end

end;





Procedure Data.GetStruckt;

begin

  if Ende.Ptr = nil then

  begin

    new(Ende.Ptr);

    StrucktList := Ende;

    Ende.Ptr^.Next.Ptr := Nil;

    Ende.Ptr^.Back.Ptr := Nil

  end

  else

  begin

    if cut.Ptr <> Nil then

    begin

      (* Liste getrennt: *)



      (* Ende mit Cut verbinden: *)

      Ende.Ptr^.Next := Cut;

      Cut.Ptr^.Back := Ende;



      (* Ende weiterrÅcken: *)

      Ende := Ende.Ptr^.Next;

      (* Cut weiterrÅcken: *)

      Cut := Cut.Ptr^.Next;



      (* Und vor Cut wieder trennen: *)

      Cut.Ptr^.Back.Ptr := Nil;

      Ende.Ptr^.Next.Ptr := Nil



    end

    else

    begin

      (* Liste nicht getrennt: *)

      (* Cut = Nil             *)



      New(Ende.Ptr^.Next.Ptr);

      Ende.Ptr^.Next.Ptr^.Back := Ende;

      Ende := Ende.Ptr^.Next;

      Ende.Ptr^.Next.Ptr := Nil

    end

  end

end;









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









Procedure Data.ListFromFile(Pfad:String; Line: Longint);

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



  (* FÅr Zusatzdaten: *)



  Pfadzeiger := StrucktListEnd;



  (* Zuweisung in das Element: *)



  StrucktListend.Ptr^.StrucktData.art := 2;

  StrucktListend.Ptr^.StrucktData.PfadList := pfadadress;



  (* Neues Element in Hilfliste erzeugen: *)



  GetHilf(HilfListend,AltHilf);



  (* Zuweisung in das Element: *)



  HilfListend.Ptr^.HilfData.art := 2;

  HilfListend.Ptr^.HilfData.PfadList := StrucktListend;









  ReadLine(Sr);

  while IOError = 0 do

  begin

    if (sr.attr and (directory + VolumeID)) = 0 then

    begin

      SpeichereDatei(sr.name, DateiList, DateiDif, NoNew);



      (* Neues Element in Strucktliste erzeugen: *)



      GetStruckt(StrucktListend,AltStruckt);



      (* Zuweisung in das Element: *)



      StrucktListend.Ptr^.StrucktData.art := 3;

      StrucktListend.Ptr^.StrucktData.DateiList := Dateiadress;



      If MemWasLo then

      Begin

        LabelZeiger.Ptr := Nil;

        SpeichereSuch(LabelZeiger,Pfadzeiger,Dateiadress)

      End

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

          ListFromFile(pfad+sr.name, SearchPfadLine(Pfad + Sr.Name));

          IOError := 0;

          Seek(ImportFile, Line)

        End;

    ReadLine(Sr)

  End



end;







Procedure Data.ImportDir;

Label Exit1;

Var Info : ^String;

    F    : File;

    W    : Word;
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

      SpeichereLabel(lab,LabelList,LabelDif, NoNew);

      Name := Lab;



      GetStruckt(StrucktListend,AltStruckt);



      StrucktListend.Ptr^.StrucktData.art := 1;

      StrucktListend.Ptr^.StrucktData.LabelList := Labeladress;





      GetHilf(HilfListend,AltHilf);



      HilfListend.Ptr^.HilfData.art := 1;

      HilfListend.Ptr^.HilfData.LabelList := StrucktListend;



      Labelzeiger := StrucktListend;



      ListFromFile(UpDate(Pfad), SearchPfadLine(UpDate(Pfad)))

    end



    else

    begin

      W := MessageBox('ImportDatei nicht in Ordnung!', Nil, mfOkButton);

      DirStatus := False

    end

  end



  else



  begin

    W := MessageBox('Diskette hat einen ungÅltigen Label!',

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

    Wait('Datenimport Åberforderte SpeicherkapazitÑt:' + Chr(13) +

         'Bitte schauen Sie fÅr Weiteres in der Hilfe fÅr Datenimport nach')



  End

end;





Procedure Data.List;

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



  StrucktListend.Ptr^.StrucktData.art := 2;

  StrucktListend.Ptr^.StrucktData.PfadList := pfadadress;



  (* Neues Element in Hilfliste erzeugen: *)



  GetHilf(HilfListend,AltHilf);



  (* Zuweisung in das Element: *)



  HilfListend.Ptr^.HilfData.art := 2;

  HilfListend.Ptr^.HilfData.PfadList.Ptr := Pointer(StrucktListend.Ptr);







  FindFirst(pfad+mask,attr,sr);

  while DosError = 0 do

  begin

    if (sr.attr and (directory + VolumeID)) = 0 then

    begin

      SpeichereDatei(sr.name, DateiList, DateiDif, NoNew);



      (* Neues Element in Strucktliste erzeugen: *)



      GetStruckt(StrucktListend,AltStruckt);



      (* Zuweisung in das Element: *)



      StrucktListend.Ptr^.StrucktData.art := 3;

      StrucktListend.Ptr^.StrucktData.DateiList := Dateiadress;



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



{

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



    FindFirst(Pfad + Befehl + Extender, AnyFile And Not VolumeId, srec);

    if Doserror = 0 then

    begin

      SpeichereLabel(lab,LabelList,LabelDif, NoNew);

      Name := Lab;



      GetStruckt(StrucktListend,AltStruckt);



      StrucktListend.Ptr^.StrucktData.art := 1;

      StrucktListend.Ptr^.StrucktData.LabelList := Labeladress;





      GetHilf(HilfListend,AltHilf);



      HilfListend.Ptr^.HilfData.art := 1;

      HilfListend.Ptr^.HilfData.LabelList.Ptr := Pointer(StrucktListend.Ptr);



      Labelzeiger := StrucktListend;



      list(UpDate(pfad))

    end



    else

    begin

      Wait('Diskette leer!');

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

}



Procedure Data.CopyMinWork;

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



Procedure Data.CopyWorkinM;

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







Procedure Data.Import;

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



  ImportDir;

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















Constructor Data.Init;

Begin

End;

Destructor  Data.Done;

Begin

End;











Procedure Data.Scanner;

Var ch    : char;

    LName : String12;

    Lauf  : StrucktListPtrtyp;

Begin

  LabelHistList := Nil;

  LName := '';

  if Nachgetragen = True then

  Begin

    Dir(LName);

    if DirStatus then

    Nachgetragen := False

  End

  else

    Wait('Es sind noch Daten im Arbeitsbereich!')

End;



Procedure Data.DiskAS;



Var  Ch            : char;

     LName         : String12;
     W             : Word;


Begin

  LabelHistList := Nil;



  LName := '';



  repeat

    Mask := Laufwerk + ':\*.*';

    N := Mask;

    (*$I-*)

    if System.MemAvail > 1024 then

    Begin

      Dir(LName);

      If LName <> '' then

        If LabelInLabelHistList(LName) then

          W := MessageBox('Sie haben entweder eine Festplatte gewÑhlt, oder eine Disk ' +

                     'nochmal eingelegt!', nil, mfOkButton)

        else

          PutLabelInHistList(LName);



      If MessageBox('Noch eine Disk einlesen?',

              nil, mfInformation + mfYesButton + mfNoButton) =

         cmYes then

           Ch := 'J'

         else

           Ch := 'N'

    End

    else

    Begin

      W := MessageBox('Speicher ist voll !', Nil, mfOkButton);

      ch := 'N'

    End

    (*$I+*)

  until ch = 'N';



  Nachgetragen := False;



  DelLabelHistList

End;







Procedure Data.DatenAufgeben;

Begin

End;

Procedure Data.ImportNachtrag;

Begin

End;

Procedure Data.ImportPruefen;

Begin

End;

Procedure Data.Nachtrag;

Begin

End;

Procedure Data.Aufreum;

Begin

End;

Procedure Data.LadeDateien;

Begin

End;

Procedure Data.SpeichereDateien;

Begin

End;

Procedure Data.DiskAutoScann;

Begin

End;





Procedure Data.SpeichereDatei;

var p1, p2 : DateiListXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin



    (* Nur balangsieren? *)



    If NoNew then

    Begin

      Lauf.Ptr := TreePointer.Ptr;

      Lauf.BlockNr := TreePointer.BlockNr

    End

    Else

      new(lauf.Ptr);





    Dateiadress := Lauf;

    dif := true;

    with lauf.Ptr^ do

    begin

      DateiData.DateiName := DateiName;

      Links.Ptr := Nil;

      Rechts.Ptr := Nil;

      If Not NoNew then

        DateiData.Loc.Ptr := Nil;

      DateiData.Bal := 0;

      DateiData.Anzahl := 1

    end

  end



  else



  if DateiName < Lauf.Ptr^.DateiData.DateiName then

  begin

    SpeichereDatei(DateiName,lauf.Ptr^.links,dif, NoNew);



    if dif then

    case lauf.Ptr^.DateiData.bal of



    1 : begin lauf.Ptr^.DateiData.bal := 0; dif := false end;



    0 : lauf.Ptr^.DateiData.bal := -1;



    -1: begin (* Ausgleichen *)

          p1 := Lauf.Ptr^.links;

          if p1.Ptr^.DateiData.bal = -1 then

          begin

            lauf.Ptr^.links := p1.Ptr^.rechts;

            p1.Ptr^.rechts := lauf;

            lauf.Ptr^.DateiData.bal := 0;

            lauf := p1

          end



          else



          begin

            p2 := p1.Ptr^.rechts;

            p1.Ptr^.rechts := p2.Ptr^.links;

            p2.Ptr^.links := p1;

            lauf.Ptr^.links := p2.Ptr^.rechts;

            p2.Ptr^.rechts := lauf;



            if p2.Ptr^.DateiData.bal = -1 then lauf.Ptr^.DateiData.bal := 1

                            else lauf.Ptr^.DateiData.bal := 0;



            if p2.Ptr^.DateiData.bal = 1  then P1.Ptr^.DateiData.bal := -1

                            else P1.Ptr^.DateiData.bal := 0;

            lauf := p2;

          end;

          lauf.Ptr^.DateiData.bal := 0;

          Dif := false

        end

      end

  end



  else



  if DateiName > Lauf.Ptr^.DateiData.DateiName then

  begin

    SpeichereDatei(DateiName,lauf.Ptr^.rechts,dif, NoNew);



    if dif then

    case lauf.Ptr^.DateiData.bal of

    -1 : begin lauf.Ptr^.DateiData.bal := 0; dif := false end;



    0 : lauf.Ptr^.DateiData.bal := +1;



    1: begin (* Ausgleichen *)

         p1 := Lauf.Ptr^.rechts;

         if p1.Ptr^.DateiData.bal = + 1 then

         begin

           lauf.Ptr^.rechts := p1.Ptr^.links;

           p1.Ptr^.links := lauf;

           lauf.Ptr^.DateiData.bal := 0;

           lauf := p1

         end



         else



         begin

           p2 := p1.Ptr^.links;

           p1.Ptr^.links := p2.Ptr^.rechts;

           p2.Ptr^.rechts := p1;

           lauf.Ptr^.rechts := p2.Ptr^.links;

           p2.Ptr^.links := lauf;



           if p2.Ptr^.DateiData.bal = +1 then lauf.Ptr^.DateiData.bal := -1

                           else lauf.Ptr^.DateiData.bal := 0;



           if p2.Ptr^.DateiData.bal = -1 then P1.Ptr^.DateiData.bal := 1

                           else P1.Ptr^.DateiData.bal := 0;

           lauf := p2;

         end;



         lauf.Ptr^.DateiData.bal := 0;

         Dif := false

       end

     end

  end

  else

  begin

    Dateiadress := Lauf;

    Inc(Lauf.Ptr^.DateiData.Anzahl);

    Dif := False

  end

end;





Procedure Data.SpeicherePfad;

var p1, p2 : PfadListXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin



    (* Nur balangsieren? *)



    If NoNew then

    Begin

      Lauf.Ptr := TreePointer.Ptr;

      Lauf.BlockNr := TreePointer.BlockNr

    End

    Else

      new(lauf.Ptr);





    Pfadadress := Lauf;

    dif := true;

    with lauf.Ptr^ do

    begin

      PfadData.PfadName := PfadName;

      Links.Ptr := Nil;

      Rechts.Ptr := Nil;

      PfadData.Bal := 0;

      PfadData.Anzahl := 1

    end

  end



  else



  if PfadName = Lauf.Ptr^.PfadData.PfadName then

  begin

    Pfadadress := Lauf;

    Inc(Lauf.Ptr^.PfadData.Anzahl);

    Dif := False

  end



  else



  if PfadName < Lauf.Ptr^.PfadData.PfadName then

  begin

    SpeicherePfad(PfadName,lauf.Ptr^.links,dif, NoNew);



    if dif then

    case lauf.Ptr^.PfadData.bal of



    1 : begin lauf.Ptr^.PfadData.bal := 0; dif := false end;



    0 : lauf.Ptr^.PfadData.bal := -1;



    -1: begin (* Ausgleichen *)

          p1 := Lauf.Ptr^.links;

          if p1.Ptr^.PfadData.bal = -1 then

          begin

            lauf.Ptr^.links := p1.Ptr^.rechts;

            p1.Ptr^.rechts := lauf;

            lauf.Ptr^.PfadData.bal := 0;

            lauf := p1

          end



          else



          begin

            p2 := p1.Ptr^.rechts;

            p1.Ptr^.rechts := p2.Ptr^.links;

            p2.Ptr^.links := p1;

            lauf.Ptr^.links := p2.Ptr^.rechts;

            p2.Ptr^.rechts := lauf;



            if p2.Ptr^.PfadData.bal = -1 then lauf.Ptr^.PfadData.bal := 1

                            else lauf.Ptr^.PfadData.bal := 0;



            if p2.Ptr^.PfadData.bal = 1  then P1.Ptr^.PfadData.bal := -1

                            else P1.Ptr^.PfadData.bal := 0;

            lauf := p2;

          end;

          lauf.Ptr^.PfadData.bal := 0;

          Dif := false

        end

      end

  end



  else



  if PfadName > Lauf.Ptr^.PfadData.PfadName then

  begin

    SpeicherePfad(PfadName,lauf.Ptr^.rechts,dif, NoNew);



    if dif then

    case lauf.Ptr^.PfadData.bal of

    -1 : begin lauf.Ptr^.PfadData.bal := 0; dif := false end;



    0 : lauf.Ptr^.PfadData.bal := +1;



    1: begin (* Ausgleichen *)

         p1 := Lauf.Ptr^.rechts;

         if p1.Ptr^.PfadData.bal = + 1 then

         begin

           lauf.Ptr^.rechts := p1.Ptr^.links;

           p1.Ptr^.links := lauf;

           lauf.Ptr^.PfadData.bal := 0;

           lauf := p1

         end



         else



         begin

           p2 := p1.Ptr^.links;

           p1.Ptr^.links := p2.Ptr^.rechts;

           p2.Ptr^.rechts := p1;

           lauf.Ptr^.rechts := p2.Ptr^.links;

           p2.Ptr^.links := lauf;



           if p2.Ptr^.PfadData.bal = +1 then lauf.Ptr^.PfadData.bal := -1

                           else lauf.Ptr^.PfadData.bal := 0;



           if p2.Ptr^.PfadData.bal = -1 then P1.Ptr^.PfadData.bal := 1

                           else P1.Ptr^.PfadData.bal := 0;

           lauf := p2;

         end;



         lauf.Ptr^.PfadData.bal := 0;

         Dif := false

       end

     end

  end

end;





Procedure Data.SpeichereSuch;

var Help : ZusatzListXPtrtyp;

begin

  if dp.Ptr^.DateiData.Loc.Ptr = Nil then

  begin

    (* ôffnen und zuweisen: *)

    new(Help.Ptr);

    Help.Ptr^.ZusatzData.sl := lp;

    Help.Ptr^.ZusatzData.sp := pp;



    (* Verbinden mit DateiElement: *)

    dp.Ptr^.DateiData.Loc := Help;



    (* An ZusatzListe vorne anhÑngen: *)



    Help.Ptr^.Next := ZusatzList;

    Help.Ptr^.Back.Ptr := Nil;



    if ZusatzList.Ptr <> Nil then ZusatzList.Ptr^.Back := Help

                          else ZusatzListend := Help;



    ZusatzList := Help

  end

  else

  begin

    (* ôffnen und zuweisen: *)

    new(Help.Ptr);

    Help.Ptr^.ZusatzData.sl := lp;

    Help.Ptr^.ZusatzData.sp := pp;



    (* Hinter anderem Element einfÅgen: *)





    Help.Ptr^.Back := dp.Ptr^.DateiData.Loc;

    Help.Ptr^.Next := dp.Ptr^.DateiData.Loc.Ptr^.Next;



    (* Wenn es das letzte Element ist, dann: *)



    if dp.Ptr^.DateiData.Loc.Ptr^.Next.Ptr = Nil then

    Begin

      dp.Ptr^.DateiData.Loc.Ptr^.Next := Help;

      ZusatzListend := dp.Ptr^.DateiData.Loc;

    End

    else

    begin

      dp.Ptr^.DateiData.Loc.Ptr^.Next.Ptr^.Back := Help;

      dp.Ptr^.DateiData.Loc.Ptr^.Next := Help

    end



  end

end;





Procedure Data.SpeichereLabel;

var p1, p2 : LabelListXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin



    (* Nur balangsieren? *)



    If NoNew then

    Begin

      Lauf.Ptr := TreePointer.Ptr;

      Lauf.BlockNr := TreePointer.BlockNr

    End

    Else

      new(lauf.Ptr);





    Labeladress := Lauf;

    dif := true;

    with lauf.Ptr^ do begin

      LabelData.LabelName := LabelName;

      links.Ptr := Nil;

      rechts.Ptr := Nil;

      LabelData.bal := 0;

      LabelData.Anzahl := 1

    end

  end



  else



  if LabelName = Lauf.Ptr^.LabelData.LabelName then

  begin

    Labeladress := Lauf;

    Inc(Lauf.Ptr^.LabelData.Anzahl);

    Dif := False

  end



  else



  if LabelName < Lauf.Ptr^.LabelData.LabelName then

  begin

    SpeichereLabel(LabelName,lauf.Ptr^.links,dif, NoNew);



    if dif then

    case lauf.Ptr^.LabelData.bal of



    1 : begin lauf.Ptr^.LabelData.bal := 0; dif := false end;



    0 : lauf.Ptr^.LabelData.bal := -1;



    -1: begin (* Ausgleichen *)

          p1 := Lauf.Ptr^.links;

          if p1.Ptr^.LabelData.bal = -1 then

          begin

            lauf.Ptr^.links := p1.Ptr^.rechts;

            p1.Ptr^.rechts := lauf;

            lauf.Ptr^.LabelData.bal := 0;

            lauf := p1

          end



          else



          begin

            p2 := p1.Ptr^.rechts;

            p1.Ptr^.rechts := p2.Ptr^.links;

            p2.Ptr^.links := p1;

            lauf.Ptr^.links := p2.Ptr^.rechts;

            p2.Ptr^.rechts := lauf;



            if p2.Ptr^.LabelData.bal = -1 then lauf.Ptr^.LabelData.bal := 1

                            else lauf.Ptr^.LabelData.bal := 0;



            if p2.Ptr^.LabelData.bal = 1  then P1.Ptr^.LabelData.bal := -1

                            else P1.Ptr^.LabelData.bal := 0;

            lauf := p2;

          end;

          lauf.Ptr^.LabelData.bal := 0;

          Dif := false

        end

      end

  end



  else



  if LabelName > Lauf.Ptr^.LabelData.LabelName then

  begin

    SpeichereLabel(LabelName, lauf.Ptr^.rechts,dif, NoNew);



    if dif then

    case lauf.Ptr^.LabelData.bal of

    -1 : begin lauf.Ptr^.LabelData.bal := 0; dif := false end;



    0 : lauf.Ptr^.LabelData.bal := +1;



    1: begin (* Ausgleichen *)

         p1 := Lauf.Ptr^.rechts;

         if p1.Ptr^.LabelData.bal = + 1 then

         begin

           lauf.Ptr^.rechts := p1.Ptr^.links;

           p1.Ptr^.links := lauf;

           lauf.Ptr^.LabelData.bal := 0;

           lauf := p1

         end



         else



         begin

           p2 := p1.Ptr^.links;

           p1.Ptr^.links := p2.Ptr^.rechts;

           p2.Ptr^.rechts := p1;

           lauf.Ptr^.rechts := p2.Ptr^.links;

           p2.Ptr^.links := lauf;



           if p2.Ptr^.LabelData.bal = +1 then lauf.Ptr^.LabelData.bal := -1

                           else lauf.Ptr^.LabelData.bal := 0;



           if p2.Ptr^.LabelData.bal = -1 then P1.Ptr^.LabelData.bal := 1

                           else P1.Ptr^.LabelData.bal := 0;

           lauf := p2;

         end;



         lauf.Ptr^.LabelData.bal := 0;

         Dif := false

       end

     end

  end

end;





Procedure Data.SetAllNil;

BEGIN
  StrucktList.Ptr := Nil;
  StrucktListend.Ptr := Nil;
  AltStruckt.Ptr := Nil;

  HilfList.Ptr := Nil;
  HilfListend.Ptr := Nil;
  AltHilf.Ptr := Nil;

  ZusatzList.Ptr := Nil;
  ZusatzListend.Ptr := Nil;

  LabelList.Ptr := Nil;
  PfadList.Ptr := Nil;
  DateiList.Ptr := Nil;

  DateiDif := False;
  PfadDif := False;
  LabelDif := False;
  Nachgetragen := True

END;





Procedure Data.SetLaufwerk;

Begin

End;



Function Data.GetLaufwerk;

Begin

End;



Procedure Data.Dir;

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



    FindFirst(Pfad + Befehl + Extender, AnyFile And Not VolumeId, srec);

    if Doserror = 0 then

    begin

      SpeichereLabel(lab,LabelList,LabelDif, NoNew);

      Name := Lab;



      GetStruckt(StrucktListend,AltStruckt);



      StrucktListend.Ptr^.StrucktData.art := 1;

      StrucktListend.Ptr^.StrucktData.LabelList := Labeladress;





      GetHilf(HilfListend,AltHilf);



      HilfListend.Ptr^.HilfData.art := 1;

      HilfListend.Ptr^.HilfData.LabelList.Ptr := Pointer(StrucktListend.Ptr);



      Labelzeiger := StrucktListend;



      list(UpDate(pfad))

    end



    else

    begin

      Wait('Diskette leer!');

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





Procedure Data.Initfiles;

BEGIN

  assign(StrucktDatafile,Strucktfilename);

  assign(HilfDatafile,Hilffilename);

  assign(DateiDatafile,Dateifilename);

  assign(LabelDatafile,Labelfilename);

  assign(PfadDatafile,Pfadfilename);

  assign(ZusatzDatafile,Zusatzfilename);

END;







Procedure Data.InitFileNames;

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





Function GetDirElement;

Var D: Importtyp;

BEGIN

  Seek(ImportFile, Stelle);

  Read(ImportFile, D);

  GetDirElement := D

END;





Procedure Suche;

Var Name: String;

Begin

  If Tree.Ptr <> Nil then

  Begin

    Name := GetDirElement(Tree.Ptr^.Zeiger);



    If Name = SArg then

    Begin

      Result.Ptr := Tree.Ptr;

      Result.BlockNr := Tree.BlockNr

    End

    Else

      If Name > SArg then

        Suche(Tree.Ptr^.Links, Result)

      Else

        If Name < SArg then

          Suche(Tree.Ptr^.Rechts, Result)

  End

  Else Result.Ptr := Nil

End;



Function SearchPfadLine;

Var P: XPointer;

Begin

  SArg := 'Verzeichnis von ' + Pfad;



  Suche(ImportDirTree, P);



  If P.Ptr <> Nil then

    SearchPfadLine := ImportDirTreeXPtrtyp(P).Ptr^.Zeiger + 1

  Else

  Begin

    Wait('Suche in Importdatei ging in die Hose!');

    Halt

  End

End;


(***********************************)

{$F+}

Function LGet(Stelle: Longint): String;

Var L: LabelDatatyp;

Var f: File of LabelDatatyp;

BEGIN
  Seek(LabelDataFile, Stelle);
  Read(LabelDataFile, L);
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

{$F-}

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
    f            : File Of LabelDatatyp;
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



  LabelPos := FilePos(f);

  Seek(f, SD.DLabelList - 1);

  Read(f, LabelData);



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

    (*Writexy(1, 4, 'Fehler! (' + GetString(ErrCount) + ')')*)

  End;

  If Not PfadFound then

  Begin

    Inc(PfadNF);

    (*Writexy(1, 5, 'Pfad nicht gefunden! (' + GetString(PfadNF) + ')')*)

  End;



  Seek(StrucktDataPufferFile, StrucktPos);

  Seek(DateiDataPufferFile, DateiPos);

  Seek(PfadDataPufferFile, PfadPos);

  Seek(f, LabelPos)

End;





Procedure BuildBaum(Var Lauf: TreeXPtrtyp;

                        Ziel: Longint;

                     Var Dif: Boolean);

var p1, p2 : TreeXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin

    new(lauf.Ptr);

    dif := true;

    with lauf.Ptr^ do

    begin

      Zeiger := Ziel;

      Links.Ptr := Nil;

      Rechts.Ptr := Nil;

      Bal := 0;

    end

  end



  else



  if GetElement(Ziel) < GetElement(Lauf.Ptr^.Zeiger) then

  begin

    BuildBaum(Lauf.Ptr^.Links, Ziel, Dif);



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



  if GetElement(Ziel) > GetElement(Lauf.Ptr^.Zeiger) then

  begin

    BuildBaum(lauf.Ptr^.rechts, Ziel, Dif);



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

    Result    : TreeXPtrtyp;

Const StrucktDif: Longint = 0;





Procedure InitPufferFiles;

BEGIN

  (* öber ...Filename und einer angefÅgten Nummerrierung. *)

  (* Wobei andere Dateivariablen verwendet werden.        *)



  assign(StrucktDataPufferfile,PData^.GetStrucktfilename);

  assign(HilfDataPufferfile,PData^.GetHilffilename);

  assign(DateiDataPufferfile,PData^.GetDateifilename);

  assign(LabelDataPufferfile,PData^.GetLabelfilename);

  assign(PfadDataPufferfile,PData^.GetPfadfilename);

  assign(ZusatzDataPufferfile,PData^.GetZusatzfilename);

END;



Var SArg: String;



Procedure Suche(Tree: TreeXPtrtyp; Var Result: TreeXPtrtyp);

Var Name: String;

Begin

  If Tree.Ptr <> Nil then

  Begin

    Name := GetElement(Tree.Ptr^.Zeiger);



    If Name = SArg then Result := Tree

    Else

      If Name > SArg then

        Suche(Tree.Ptr^.Links, Result)

      Else

        If Name < SArg then

          Suche(Tree.Ptr^.Rechts, Result)

  End

  Else Result.Ptr := Nil

End;



Var LogWrite    : Boolean;

    LogFile     : Text;







Procedure SucheMod(Tree: TreeXPtrtyp;

                  Arg: Longint;

                  Max: Longint;

                  Var Result: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    If LogWrite then

    Begin

      Writeln(LogFile, 'Arg: ' + GetString(Arg));

      Flush(LogFile);

      Writeln(LogFile, 'Ze : ' + GetString(Tree.Ptr^.Zeiger));

      Flush(LogFile);

      Writeln(LogFile, 'St : ' + GetString(Tree.Ptr^.Stelle));

      Flush(LogFile)

    End;



    If (Tree.Ptr^.Zeiger >= Max) or

       (Tree.Ptr^.Zeiger <0) then

    Begin

      Wait('Stelle auf Datei in Baum (Zeiger) zu gro·: ' + Chr(13) +

           GetString(Tree.Ptr^.Zeiger));

      Halt

    End;

    If Tree.Ptr^.Zeiger = Arg then Result := Tree

    Else

      If Tree.Ptr^.Zeiger > Arg then

      Begin

        If LogWrite then

        Begin

          Writeln(LogFile, 'G l: ');

          Flush(LogFile)

        End;



        SucheMod(Tree.Ptr^.Links, Arg, Max, Result)

      End

      Else

        If Tree.Ptr^.Zeiger < Arg then

        Begin

          If LogWrite then

          Begin

            Writeln(LogFile, 'G r: ');

            Flush(LogFile)

          End;

          SucheMod(Tree.Ptr^.Rechts, Arg, Max, Result)

        End

  End

  Else Result.Ptr := Nil

End;



Procedure PruefeDaten(LabelName, PfadName, DateiName: String);

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

    (*Writexy(1, 4, 'Fehler! (' + GetString(ErrCount) + ') ' + DateiName)*)

  End;

  If Not PfadFound then

  Begin

    Inc(PfadNF);

    (*Writexy(1, 5, 'Pfad nicht gefunden! (' + GetString(PfadNF) + ')')*)

  End

End;

Procedure MoveLabel;

Begin
  Seek(LabelDataFile, StrucktData.DLabelList - 1);
  Read(LabelDataFile, LabelData);
  GetDateiElement := LGet;
  SArg := LabelData.LabelName;
  Suche(LabelTree, Result);

  If Result.Ptr <> Nil then
  Begin
    (* Strucktelement in Datei anpassen: *)
    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);
    StrucktData.DLabelList := Result.Ptr^.Zeiger + 1;
    Write(StrucktDataFile, StrucktData);

    (* Labelelement anpassen: *)
    Seek(LabelDataPufferFile, Result.Ptr^.Zeiger);
    Read(LabelDataPufferFile, LabelData);
    Seek(LabelDataPufferFile, Result.Ptr^.Zeiger);
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

  Suche(PfadTree, Result);



  If Result.Ptr <> Nil then

  Begin

    (* Strucktelement in Datei anpassen: *)

    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);

    StrucktData.DPfadList := Result.Ptr^.Zeiger + 1;

    Write(StrucktDataFile, StrucktData);



    Seek(PfadDataPufferFile, Result.Ptr^.Zeiger(* - 1 *));

    Read(PfadDataPufferFile, PfadData);

    Seek(PfadDataPufferFile, Result.Ptr^.Zeiger(* - 1 *));

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

    Lauf        : ZusatzListXPtrtyp;

    Help        : ZusatzListXPtrtyp;

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

  Suche(DateiTree, Result);



  If Result.Ptr <> Nil then

  Begin

    (* Strucktelement in Datei anpassen: *)



    Seek(StrucktDataFile, FilePos(StrucktDataFile) - 1);

    StrucktData.DDateiList := Result.Ptr^.Zeiger + 1;

    Write(StrucktDataFile, StrucktData);



    (* Dateielement anpassen: *)



    If Result.Ptr^.Zeiger >= FileSize(DateiDataPufferFile) then

    Begin

      Wait('Move Datei: Zeiger auf Dateidatapufferdatei zu gro·!');

      Halt

    End;



    Seek(DateiDataPufferFile, Result.Ptr^.Zeiger (* - 1 *) );

    Read(DateiDataPufferFile, DateiData);

    Seek(DateiDataPufferFile, Result.Ptr^.Zeiger (* - 1 *) );

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

    DateiData.Loc.Ptr := Nil;

    Write(DateiDataPufferFile, DateiData);



    SArg := DateiData.DateiName;

    GetDateiElement := DGet;

    BuildBaum(DateiTree, FilePos(DateiDataPufferFile) - 1, DateiDif);

  End;



End; (* MoveDatei *)





Var Hilf : TreeXPtrtyp;

    Liste: TreeXPtrtyp;

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

Procedure NumeriereBaum(Tree: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    NumeriereBaum(Tree.Ptr^.Links);



    Inc(B);

    Tree.Ptr^.Stelle := B;



    NumeriereBaum(Tree.Ptr^.Rechts)

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







Procedure ReOrgLabel(Tree: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    ReOrgLabel(Tree.Ptr^.Links);



    Seek(LabelDataPufferFile, Tree.Ptr^.Zeiger);

    Read(LabelDataPufferFile, LabelData);

    Write(LDPF, LabelData);



    ReOrgLabel(Tree.Ptr^.Rechts)

  End

End;



Procedure ReOrgPfad(Tree: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    ReOrgPfad(Tree.Ptr^.Links);



    Seek(PfadDataPufferFile, Tree.Ptr^.Zeiger);

    Read(PfadDataPufferFile, PfadData);

    Write(PDPF, PfadData);



    ReOrgPfad(Tree.Ptr^.Rechts)

  End

End;



Procedure ReOrgDatei(Tree: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    ReOrgDatei(Tree.Ptr^.Links);



    Seek(DateiDataPufferFile, Tree.Ptr^.Zeiger);

    Read(DateiDataPufferFile, DateiData);

    DateiData.Loc.Ptr := Nil;

    Write(DDPF, DateiData);



    ReOrgDatei(Tree.Ptr^.Rechts)

  End

End;



Type TreeFiletyp = File of Treetyp;

Var

     LabelTreeFile,

     PfadTreeFile,

     DateiTreeFile : TreeFiletyp;



Procedure TreeSave(Tree:TreeXPtrtyp; Var TreeFile: TreeFiletyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    TreeSave(Tree.Ptr^.Links, TreeFile);

    Write(TreeFile, Tree.Ptr^);

    TreeSave(Tree.Ptr^.Rechts, TreeFile);

    Dispose(Tree.Ptr)

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

  LabelTree.Ptr := Nil;



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

  PfadTree.Ptr := Nil;



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

  DateiTree.Ptr := Nil;



  HelpTreesOnFile := True

End;



Procedure TreeTraverse(Tree: TreeXPtrtyp);

Begin

  If Tree.Ptr <> Nil then

  Begin

    TreeTraverse(Tree.Ptr^.Links);



    Writeln(LogFile, 'Zeiger: ' + GetString(Tree.Ptr^.Zeiger));

    Flush(LogFile);

    Writeln(LogFile, 'Stelle: ' + GetString(Tree.Ptr^.Stelle));

    Flush(LogFile);

    Writeln(LogFile);

    Flush(LogFile);



    TreeTraverse(Tree.Ptr^.Rechts)

  End

End;





Var Dif    : Boolean;

    NewTree: TreeXPtrtyp;



Procedure TreeReSort(Tree: TreeXPtrtyp);



Procedure ReBuildBaum(Var Lauf: TreeXPtrtyp;

                          Data: TreeXPtrtyp;

                       Var Dif: Boolean);

var p1, p2 : TreeXPtrtyp;

begin

  if Lauf.Ptr = Nil then

  begin

    Lauf := Data;

    dif := true;

    with lauf.Ptr^ do

    begin

      Links.Ptr := Nil;

      Rechts.Ptr := Nil;

      Bal := 0;

    end

  end



  else



  if Data.Ptr^.Zeiger < Lauf.Ptr^.Zeiger then

  begin

    ReBuildBaum(Lauf.Ptr^.Links, Data, Dif);



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



  if Data.Ptr^.Zeiger > Lauf.Ptr^.Zeiger then

  begin

    ReBuildBaum(lauf.Ptr^.rechts, Data, Dif);



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





Begin

  If Tree.Ptr <> Nil then

  Begin

    TreeReSort(Tree.Ptr^.Links);



    New(Hilf.Ptr);

    Hilf.Ptr^ := Tree.Ptr^;

    ReBuildBaum(NewTree, Hilf, Dif);



    TreeReSort(Tree.Ptr^.Rechts);



    Dispose(Tree.Ptr)

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

var Help    : ZusatzListXPtrtyp;

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



  If DPP^.Loc.Ptr = Nil then

  begin

    (* ôffnen und zuweisen: *)

    new(Help.Ptr);

    Help.Ptr^.ZusatzData.sl.Ptr := Pointer(lp);

    Help.Ptr^.ZusatzData.sp.Ptr := Pointer(pp);



    (* Verbinden mit DateiElement: *)

    DPP^.Loc := Help;

    PutDElement(DPP, DP);



    (* An ZusatzListe vorne anhÑngen: *)



    Help.Ptr^.Next := ZusatzList;

    Help.Ptr^.Back.Ptr := Nil;



    if ZusatzList.Ptr <> Nil then ZusatzList.Ptr^.Back := Help

                          else ZusatzListend := Help;



    ZusatzList := Help

  end

  else

  begin

    (* ôffnen und zuweisen: *)

    new(Help.Ptr);

    Help.Ptr^.ZusatzData.sl.Ptr := Pointer(lp);

    Help.Ptr^.ZusatzData.sp.Ptr := Pointer(pp);



    (* Hinter anderem Element einfÅgen: *)





    Help.Ptr^.Back := dpp^.Loc;

    Help.Ptr^.Next := dpp^.Loc.Ptr^.Next;



    (* Wenn es das letzte Element ist, dann: *)



    if dpp^.Loc.Ptr^.Next.Ptr = Nil then

    Begin

      dpp^.Loc.Ptr^.Next := Help;

      ZusatzListend := dpp^.Loc;

    End

    else

    begin

      dpp^.Loc.Ptr^.Next.Ptr^.Back := Help;

      dpp^.Loc.Ptr^.Next := Help

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

  Df := PData^.GetDateifilename;

  Pf := PData^.GetPfadfilename;

  Lf := PData^.GetLabelfilename;

  Sf := PData^.GetStrucktfilename;

  Hf := PData^.GetHilffilename;

  Zf := PData^.GetZusatzfilename;



  PData^.SetDateifilename('Puffer' + '0' + '.DVD');

  PData^.SetPfadfilename('Puffer' + '0' + '.DVP');

  PData^.SetLabelfilename('Puffer' + '0' + '.DVL');

  PData^.SetStrucktfilename('Puffer' + '0' + '.DVS');

  PData^.SetHilffilename('Puffer' + '0' + '.DVH');

  PData^.SetZusatzfilename('Puffer' + '0' + '.DVZ');



  InitPufferFiles;

  OpenPufferFiles;



  DateiDif := False;

  PfadDif := False;

  LabelDif := False;



  DateiTree.Ptr := Nil;

  PfadTree.Ptr := Nil;

  LabelTree.Ptr := Nil;



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

    PData^.SetDateifilename('Puffer' + GetString(A) + '.DVD');

    PData^.SetPfadfilename('Puffer' + GetString(A) + '.DVP');

    PData^.SetLabelfilename('Puffer' + GetString(A) + '.DVL');

    PData^.SetStrucktfilename('Puffer' + GetString(A) + '.DVS');

    PData^.SetHilffilename('Puffer' + GetString(A) + '.DVH');

    PData^.SetZusatzfilename('Puffer' + GetString(A) + '.DVZ');



    PData^.InitFiles;

    PData^.OpenAllFiles;



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



    PData^.CloseAllFiles;

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

  NewTree.Ptr := Nil;

  Dif := False;

  TreeReSort(DateiTree);

  DateiTree := NewTree;



  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +

                                     GetString(B) + Chr(13) +

                                     GetString(System.MemAvail));



  B := System.MemAvail;

  NewTree.Ptr := Nil;

  Dif := False;

  TreeReSort(PfadTree);

  PfadTree := NewTree;



  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +

                                     GetString(B) + Chr(13) +

                                     GetString(System.MemAvail));



  B := System.MemAvail;

  NewTree.Ptr := Nil;

  Dif := False;

  TreeReSort(LabelTree);

  LabelTree := NewTree;



  If B <> System.MemAvail then Wait('Freier Speicherplatz verÑndert!' + Chr(13) +

                                     GetString(B) + Chr(13) +

                                     GetString(System.MemAvail));





  PData^.SetDateifilename('Puffer' + '0' + '.DVD');

  PData^.SetPfadfilename('Puffer' + '0' + '.DVP');

  PData^.SetLabelfilename('Puffer' + '0' + '.DVL');

  PData^.SetStrucktfilename('Puffer' + '0' + '.DVS');

  PData^.SetHilffilename('Puffer' + '0' + '.DVH');

  PData^.SetZusatzfilename('Puffer' + '0' + '.DVZ');



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

  ZusatzList.Ptr := Nil;

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

             New(Result.Ptr);

             Read(LabelTreeFile, Result.Ptr^)

           End

           Else

           SucheMod(LabelTree, StrucktData.DLabelList - 1, FileSize(LabelDataPufferFile), Result);



           If Result.Ptr <> Nil then

           Begin

             StrucktData.DLabelList := Result.Ptr^.Stelle;

             LabelStelle := Zaehler;

             Seek(StrucktDataPufferFile, FilePos(StrucktDataPufferFile) - 1);

             Write(StrucktDataPufferFile, StrucktData)

           End

           Else Wait('Label nicht gefunden!');

           If HelpTreesOnFile then Dispose(Result.Ptr)

         End;

     5 : Begin

           PfadStelle := 0;



           If HelpTreesOnFile then

           Begin

             Seek(PfadTreeFile, StrucktData.DPfadList - 1);

             New(Result.Ptr);

             Read(PfadTreeFile, Result.Ptr^)

           End

           Else

            SucheMod(PfadTree, StrucktData.DPfadList - 1, FileSize(PfadDataPufferFile), Result);



           If Result.Ptr <> Nil then

           Begin

             StrucktData.DPfadList := Result.Ptr^.Stelle;

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

           If HelpTreesOnFile then Dispose(Result.Ptr)

         End;

     6 : Begin

           If HelpTreesOnFile then

           Begin

             Seek(DateiTreeFile, StrucktData.DDateiList - 1);

             New(Result.Ptr);

             Read(DateiTreeFile, Result.Ptr^)

           End

           Else

             SucheMod(DateiTree, StrucktData.DDateiList - 1, FileSize(DateiDataPufferFile), Result);



           Inc(Find1);

           If Result.Ptr <> Nil then

           Begin

             Inc(Find);

             StrucktData.DDateiList := Result.Ptr^.Stelle;

             Seek(StrucktDataPufferFile, FilePos(StrucktDataPufferFile) - 1);

             Write(StrucktDataPufferFile, StrucktData);



             If (LabelStelle = 0) or

                (PfadStelle = 0) then

                Begin

                  Wait('Ein Label oder Pfad wurde nicht gefunden!');

                  Halt

                End;



             SpeichereSuchDatei(LabelStelle, PfadStelle, Result.Ptr^.Stelle);

         (*  Writexy(1, 2, 'PrÅfe  : ' + GetString(Find));

             PruefeDateiDaten(LabelStelle, PfadStelle, Result^.Stelle);

             Writexy(1, 3, 'GeprÅft: ' + GetString(Find)); *)

           End;

           If HelpTreesOnFile then Dispose(Result.Ptr)

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

  While Help.Ptr <> Nil do

  Begin

    Help.Ptr^.Back.Ptr := Pointer(B);

    Write(ZusatzDataPufferFile, Help.Ptr^.ZusatzData);

    Inc(B);

    Help := Help.Ptr^.Next

  End;



  Seek(DateiDataPufferFile, 0);

  While Not Eof(DateiDataPufferFile) do

  Begin

    Read(DateiDataPufferFile, DateiData);

    DateiData.Loc := DateiData.Loc.Ptr^.Back;

    Seek(DateiDataPufferFile, FilePos(DateiDataPufferFile) - 1);

    Write(DateiDataPufferFile, DateiData)

  End;



  Help := ZusatzList;

  While Help.Ptr <> Nil do

  Begin

    ZusatzList := Help;

    Help := Help.Ptr^.Next;

    Dispose(ZusatzList.Ptr)

  End;



  Close(DateiDataPufferfile);

  Close(PfadDataPufferfile);

  Close(LabelDataPufferfile);

  Close(StrucktDataPufferFile);

  Close(HilfDataPufferFile);

  Close(ZusatzDataPufferFile);



  PData^.SetDateifilename(DF);

  PData^.SetPfadfilename(PF);

  PData^.SetLabelfilename(LF);

  PData^.SetStrucktfilename(SF);

  PData^.SetHilffilename(HF);

  PData^.SetZusatzfilename(ZF);



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