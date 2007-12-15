Unit Video;



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

     StdDlg,

     XHeap,
     TVDVData;



type



      string12             = string[12];









type



     arttyp               = byte;   (* Label, Pfad, Datei *)







(*************************************************************)

(*                                                           *)

(* Globale Datentypen werden z. B. beim Sortieren verwendet. *)

(*                                                           *)

(*************************************************************)



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



     LabelDatatyp     = Record

                            Bal       : Shortint;

                            Anzahl    : Byte;

                            LabelName : String12

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

     pfad,

     befehl,

     extender,

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

     LabelDataPufferfile    : LabelFiletyp;

     PfadDataPufferfile     : PfadFiletyp;

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


const
     NoNew: Boolean          = False;

Var  TreePointer: XPointer;  (* Wird verwendet, wenn NoNew True ist.    *)
                             (* Dh. wenn der Baum nur balangsiert wird. *)




Procedure Wait(txt : string);

Procedure NewArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);

Procedure DisposeArray(Var Pointer; ElementSize: Integer; Anzahl: Longint);



Procedure Fehler(txt:string ; var ch:char);

Procedure GetMedium(ch: char);

Procedure Writexy(x,y : byte; txt : string);

Procedure OpenAllFiles;

Procedure DateiDataDateiOeffnen  (var Datei : DateiFiletyp);
Procedure LabelDataDateiOeffnen  (var Datei : LabelFiletyp);
Procedure PfadDataDateiOeffnen   (var Datei : PfadFiletyp);
Procedure StrucktDataDateiOeffnen(var Datei : StrucktFiletyp);
Procedure HilfDataDateiOeffnen   (var Datei : HilfFiletyp);
Procedure ZusatzDataDateiOeffnen (var Datei : ZusatzFiletyp);






Function  TestFiles:Boolean;

Procedure CloseAllFiles;


Function  UpDate(term: String):String;

Procedure List(pfad:string);

Procedure Dir(Var Name: String);

Procedure CopyMinWork(x:byte);

Procedure CopyWorkinM(x:byte);

Procedure TestMaskenSyntax(Var Maske: String);

Function  PasstInMaske(Maske, Name: String12):Boolean;

Procedure Initfiles;

Procedure MachBinBaum(Var p: GlobalListXPtrtyp;

                        l, r : Longint);

Procedure SpeichereDateien;

Procedure LadeDateien;

Procedure LPDFelderBauen;

Procedure Halt;



Procedure CursorAus;

Procedure SaveCursor;

Procedure OrgCursor;

Procedure PruefeDaten(Wo: String);

Function GetString(Zahl: Longint): String;

Procedure SpeichereDatei(DateiName : string12;
                         var  Lauf : DateiListPtrtyp;
                         var  dif  : Boolean;
                              NoNew: Boolean);

Procedure SpeichereLabel(LabelName : string12;
                         var  Lauf : LabelListPtrtyp;
                         var  dif  : Boolean;
                              NoNew: Boolean);

Procedure SpeicherePfad(PfadName : string;
                        var  Lauf : PfadListPtrtyp;
                        var  dif  : Boolean;
                             NoNew: Boolean);

Procedure SpeichereSuch(lp, pp: StrucktListPtrtyp;
                             dp: DateiListPtrtyp);

Procedure GetStruckt (var Ende, cut: StrucktListPtrtyp);

Procedure GetHilf (var Ende, cut: HilfListPtrtyp);


Procedure SetAllNil;

Implementation

Uses DynAr;



var ms, mx, my : Word;



Type

      XPointer             = Record

                               Ptr    : Pointer;

                               BlockNr: Word

                             End;

Procedure CloseallFiles;
BEGIN
  close(StrucktDatafile);
  close(HilfDatafile);
  close(LabelDatafile);
  close(PfadDatafile);
  close(DateiDatafile);
  close(ZusatzDatafile)
END;


Procedure Wait;
begin
  MessageBox(txt, nil, mfOkButton)
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

Procedure SpeichereSuch;
var Help : ZusatzListPtrtyp;
begin
  if dp^.DateiData.Loc.Ptr = Nil then
  begin
    (* ôffnen und zuweisen: *)
    new(Help);
    Help^.ZusatzData.sl.Ptr := lp;
    Help^.ZusatzData.sp.Ptr := pp;

    (* Verbinden mit DateiElement: *)
    dp^.DateiData.Loc.Ptr := Help;

    (* An ZusatzListe vorne anhÑngen: *)

    Help^.Next := ZusatzList;
    Help^.Back.Ptr := Nil;

    if ZusatzList.Ptr <> Nil then ZusatzList.Ptr^.Back.Ptr := Help
                          else ZusatzListend.Ptr := Help;

    ZusatzList.Ptr := Help
  end
  else
  begin
    (* ôffnen und zuweisen: *)
    new(Help);
    Help^.ZusatzData.sl.Ptr := lp;
    Help^.ZusatzData.sp.Ptr := pp;

    (* Hinter anderem Element einfÅgen: *)


    Help^.Back := dp^.DateiData.Loc;
    Help^.Next := dp^.DateiData.Loc.Ptr^.Next;

    (* Wenn es das letzte Element ist, dann: *)

    if dp^.DateiData.Loc.Ptr^.Next.Ptr = Nil then
    Begin
      dp^.DateiData.Loc.Ptr^.Next.Ptr := Help;
      ZusatzListend := dp^.DateiData.Loc;
    End
    else
    begin
      dp^.DateiData.Loc.Ptr^.Next.Ptr^.Back.Ptr := Help;
      dp^.DateiData.Loc.Ptr^.Next.Ptr := Help
    end

  end
end;


Procedure SetAllNil;
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


Procedure SpeichereDatei;
var p1, p2 : DateiListPtrtyp;
begin
  if Lauf = Nil then
  begin

    (* Nur balangsieren? *)

    If NoNew then
      Lauf := TreePointer.Ptr
    Else
      new(lauf);


    Dateiadress.Ptr := Lauf;
    dif := true;
    with lauf^ do
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

  if DateiName < Lauf^.DateiData.DateiName then
  begin
    SpeichereDatei(DateiName,lauf^.links.Ptr,dif, NoNew);

    if dif then
    case lauf^.DateiData.bal of

    1 : begin lauf^.DateiData.bal := 0; dif := false end;

    0 : lauf^.DateiData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links.Ptr;
          if p1^.DateiData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts.Ptr := lauf;
            lauf^.DateiData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts.Ptr;
            p1^.rechts := p2^.links;
            p2^.links.Ptr := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts.Ptr := lauf;

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
    SpeichereDatei(DateiName,lauf^.rechts.Ptr,dif, NoNew);

    if dif then
    case lauf^.DateiData.bal of
    -1 : begin lauf^.DateiData.bal := 0; dif := false end;

    0 : lauf^.DateiData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts.Ptr;
         if p1^.DateiData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links.Ptr := lauf;
           lauf^.DateiData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links.Ptr;
           p1^.links := p2^.rechts;
           p2^.rechts.Ptr := p1;
           lauf^.rechts := p2^.links;
           p2^.links.Ptr := lauf;

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
    Dateiadress.Ptr := Lauf;
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

    If NoNew then Lauf := TreePointer.Ptr
    Else
      new(lauf);


    Labeladress.Ptr := Lauf;
    dif := true;
    with lauf^ do begin
      LabelData.LabelName := LabelName;
      links.Ptr := Nil;
      rechts.Ptr := Nil;
      LabelData.bal := 0;
      LabelData.Anzahl := 1
    end
  end

  else

  if LabelName = Lauf^.LabelData.LabelName then
  begin
    Labeladress.Ptr := Lauf;
    Inc(Lauf^.LabelData.Anzahl);
    Dif := False
  end

  else

  if LabelName < Lauf^.LabelData.LabelName then
  begin
    SpeichereLabel(LabelName,lauf^.links.Ptr,dif, NoNew);

    if dif then
    case lauf^.LabelData.bal of

    1 : begin lauf^.LabelData.bal := 0; dif := false end;

    0 : lauf^.LabelData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links.Ptr;
          if p1^.LabelData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts.Ptr := lauf;
            lauf^.LabelData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts.Ptr;
            p1^.rechts := p2^.links;
            p2^.links.Ptr := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts.Ptr := lauf;

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
    SpeichereLabel(LabelName, lauf^.rechts.Ptr,dif, NoNew);

    if dif then
    case lauf^.LabelData.bal of
    -1 : begin lauf^.LabelData.bal := 0; dif := false end;

    0 : lauf^.LabelData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts.Ptr;
         if p1^.LabelData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links.Ptr := lauf;
           lauf^.LabelData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links.Ptr;
           p1^.links := p2^.rechts;
           p2^.rechts.Ptr := p1;
           lauf^.rechts := p2^.links;
           p2^.links.Ptr := lauf;

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

    If NoNew then Lauf := TreePointer.Ptr
    Else
      new(lauf);


    Pfadadress.Ptr := Lauf;
    dif := true;
    with lauf^ do
    begin
      PfadData.PfadName := PfadName;
      Links.Ptr := Nil;
      Rechts.Ptr := Nil;
      PfadData.Bal := 0;
      PfadData.Anzahl := 1
    end
  end

  else

  if PfadName = Lauf^.PfadData.PfadName then
  begin
    Pfadadress.Ptr := Lauf;
    Inc(Lauf^.PfadData.Anzahl);
    Dif := False
  end

  else

  if PfadName < Lauf^.PfadData.PfadName then
  begin
    SpeicherePfad(PfadName,lauf^.links.Ptr,dif, NoNew);

    if dif then
    case lauf^.PfadData.bal of

    1 : begin lauf^.PfadData.bal := 0; dif := false end;

    0 : lauf^.PfadData.bal := -1;

    -1: begin (* Ausgleichen *)
          p1 := Lauf^.links.Ptr;
          if p1^.PfadData.bal = -1 then
          begin
            lauf^.links := p1^.rechts;
            p1^.rechts.Ptr := lauf;
            lauf^.PfadData.bal := 0;
            lauf := p1
          end

          else

          begin
            p2 := p1^.rechts.Ptr;
            p1^.rechts := p2^.links;
            p2^.links.Ptr := p1;
            lauf^.links := p2^.rechts;
            p2^.rechts.Ptr := lauf;

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
    SpeicherePfad(PfadName,lauf^.rechts.Ptr,dif, NoNew);

    if dif then
    case lauf^.PfadData.bal of
    -1 : begin lauf^.PfadData.bal := 0; dif := false end;

    0 : lauf^.PfadData.bal := +1;

    1: begin (* Ausgleichen *)
         p1 := Lauf^.rechts.Ptr;
         if p1^.PfadData.bal = + 1 then
         begin
           lauf^.rechts := p1^.links;
           p1^.links.Ptr := lauf;
           lauf^.PfadData.bal := 0;
           lauf := p1
         end

         else

         begin
           p2 := p1^.links.Ptr;
           p1^.links := p2^.rechts;
           p2^.rechts.Ptr := p1;
           lauf^.rechts := p2^.links;
           p2^.links.Ptr := lauf;

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


Function GetString;
Var Help : String;
Begin
  Str(Zahl, Help);
  GetString := Help
End;



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


Procedure OpenallFiles;
BEGIN
  ZusatzDataDateiOeffnen(ZusatzDatafile);
  DateiDataDateiOeffnen(DateiDatafile);
  PfadDataDateiOeffnen(PfadDatafile);
  LabelDataDateiOeffnen(LabelDatafile);
  HilfDataDateiOeffnen(HilfDatafile);
  StrucktDataDateiOeffnen(StrucktDatafile)
END;





Procedure PruefeDaten;

Var SArt  : String;

    LA,

    HA    : Longint;

    SHA,

    SLA   : String;

    HLauf : HilfListXPtrTyp;

    ZLauf : ZusatzListXPtrtyp;

    SLP,

    SSP   : String;

    Lauf  : StrucktListXPtrtyp;

    Anzahl: Integer;

    HAn,

    ZAn,

    PAn,

    LAn   : Integer;

    SDAn,

    SSDAn,

    SZAn  : String;



Procedure ZaehleAbs(Root: GlobalBaumXPtrtyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    ZaehleAbs(Root.Ptr^.Links);



    If Root.Ptr^.Anzahl > 0 then

      Inc(Anzahl, Root.Ptr^.Anzahl);



    ZaehleAbs(Root.Ptr^.Rechts)

  End

End;



Procedure TesteZusatzaufStruckt(ZLauf: ZusatzListXPtrtyp);

Var Gef : Boolean;

Begin

  Gef := False;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If (Lauf.Ptr = ZLauf.Ptr^.ZusatzData.sl.Ptr) and

       (Lauf.BlockNr = ZLauf.Ptr^.ZusatzData.sl.BlockNr) then

    Begin

      While (Lauf.Ptr <> Nil) and

            ((Lauf.Ptr <> ZLauf.Ptr^.ZusatzData.sp.Ptr) or

             (Lauf.BlockNr <> ZLauf.Ptr^.ZusatzData.sp.BlockNr)) do

        Lauf := Lauf.Ptr^.Next;

      If Lauf.Ptr <> Nil then Gef := True

    End;

    If Gef then Exit;

    Lauf := Lauf.Ptr^.Next

  End;

  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!')

End;



Procedure TesteHilfaufStruckt(HLauf: HilfListXPtrtyp);

Var Gef : Boolean;

Begin

  Gef := False;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If (Lauf.Ptr = HLauf.Ptr^.HilfData.LabelList.Ptr) and

       (Lauf.BlockNr = HLauf.Ptr^.HilfData.LabelList.BlockNr) then

      Gef := True;

    If Gef then Exit;

    Lauf := Lauf.Ptr^.Next

  End;

  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!')

End;



Begin

  Wait(Wo);



(* Teste 1, 2, 3 von Strucktliste: *)



  Lauf := StrucktList;



  If Lauf.Ptr^.Back.Ptr <> Nil then

    Wait('Strucktliste nicht in ordnung');



  While Lauf.Ptr <> Nil do

  Begin

    If Not(Lauf.Ptr^.StrucktData.art In [1, 2, 3]) then

      Wait('Strucktliste hat fehlerhafte Elemente!');

    Lauf := Lauf.Ptr^.Next

  End;



(* Teste 1, 2 von Hilfliste: *)



  HLauf := HilfList;



  If HLauf.Ptr^.Back.Ptr <> Nil then

    Wait ('Hilfliste nicht in ordnung!');



  While HLauf.Ptr <> Nil do

  Begin

    If Not(HLauf.Ptr^.HilfData.art In [1, 2]) then

      Wait('Hilfliste hat fehlerhafte Elemente!');

    HLauf := HLauf.Ptr^.Next

  End;







(* PrÅfe Zeiger von Zusatz auf Struckt: *)



  ZLauf := ZusatzList;



  While ZLauf.Ptr <> Nil do

  Begin

    TesteZusatzaufStruckt(ZLauf);

    ZLauf := ZLauf.Ptr^.Next

  End;



(* PrÅfe Zeiger von Hilf auf Struckt: *)



  HLauf := HilfList;



  While HLauf.Ptr <> Nil do

  Begin

    TesteHilfaufStruckt(HLauf);

    HLauf := HLauf.Ptr^.Next

  End;





(* Testen, ob die Anzahl der Dateien mit der Anzahl der *)

(* Zusatzelemente Åbereinstimmt:                        *)



  Anzahl := 0;

  ZaehleAbs(GlobalBaumXPtrtyp(DateiList));

  ZAn := Anzahl;

  Str(ZAn, SDAn);



  Anzahl := 0;



  ZLauf := ZusatzList;



  While ZLauf.Ptr <> Nil do

  Begin

    Inc(Anzahl);

    ZLauf := ZLauf.Ptr^.Next

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



  While Lauf.Ptr <> Nil do

  Begin

    If Lauf.Ptr^.StrucktData.Art = 3 then Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  If Anzahl <> ZAn then Wait('Anzahl <> SAn ( Datei - Struckt )!');



(********************************************************)



(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)

(* Pfadelemente in der Strucktliste Åbereinstimmt:      *)



  Anzahl := 0;



  ZaehleAbs(GlobalBaumXPtrtyp(PfadList));

  PAn := Anzahl;

  Anzahl := 0;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If Lauf.Ptr^.StrucktData.Art = 2 then Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  If Anzahl <> PAn then Wait('Anzahl <> PAn ( Pfad - Struckt )!');



(********************************************************)



(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)

(* Labelelemente in der Strucktliste Åbereinstimmt:     *)



  Anzahl := 0;



  ZaehleAbs(GlobalBaumXPtrtyp(LabelList));

  LAn := Anzahl;

  Anzahl := 0;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If Lauf.Ptr^.StrucktData.Art = 1 then Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  If Anzahl <> LAn then Wait('Anzahl <> LAn! ( Label - Struckt )');



(********************************************************)



(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)

(* Labelelemente in der Hilfliste Åbereinstimmt:        *)



  Anzahl := 0;



  HLauf := HilfList;



  While HLauf.Ptr <> Nil do

  Begin

    If HLauf.Ptr^.HilfData.art = 1 then Inc(Anzahl);

    HLauf := HLauf.Ptr^.Next

  End;



  HAn := Anzahl;

  Anzahl := 0;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If Lauf.Ptr^.StrucktData.Art = 1 then Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  If Anzahl <> HAn then Wait('Label: Anzahl <> HAn! ( Hilf - Struckt )');



(********************************************************)



(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)

(* Pfadelemente in der Hilfliste Åbereinstimmt:         *)



  Anzahl := 0;



  HLauf := HilfList;



  While HLauf.Ptr <> Nil do

  Begin

    If HLauf.Ptr^.HilfData.art = 2 then Inc(Anzahl);

    HLauf := HLauf.Ptr^.Next

  End;



  HAn := Anzahl;

  Anzahl := 0;

  Lauf := StrucktList;



  While Lauf.Ptr <> Nil do

  Begin

    If Lauf.Ptr^.StrucktData.Art = 2 then Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  If Anzahl <> HAn then Wait('Pfad: Anzahl <> HAn! ( Hilf - Struckt )');



(********************************************************)





  LA := 0;

  HA := 0;



  HLauf := HilfList;

  Lauf := StrucktList;





  While (Lauf.Ptr <> Nil) do

  Begin

    If Lauf.Ptr^.StrucktData.art <> 3 then





    Begin

      If Not(Lauf.Ptr^.StrucktData.Art in [1, 2]) then

        Wait('In Strucktliste ist ein Element,' + chr(13) +

             'dessen Art nicht 1 oder 2 ist!');



      Inc(LA);

      Inc(HA);



      If HLauf.Ptr <> Nil then

        If Not((HLauf.Ptr^.HilfData.art = 1) or (HLauf.Ptr^.HilfData.art = 2)) then

        Begin

          SArt := '';

          Str(HLauf.Ptr^.HilfData.art, SArt);

          Wait('Hilfelement <> 1, 2: ' + SArt +

               ' an Stelle: ' + SHA)

        End

        Else

      Else

        Wait('Element: ' + SHA + ' ist Nil');



      If (HLauf.Ptr^.HilfData.LabelList.Ptr <> Lauf.Ptr) or

         (HLauf.Ptr^.HilfData.LabelList.BlockNr <> Lauf.BlockNr) then

      Begin

        Wait('Hilfelement zeigt nicht auf zugeh. Strucktelement!');

      End;



      HLauf := HLauf.Ptr^.Next

    End;







    If Lauf.Ptr^.Next.Ptr = Nil then

      If Lauf.Ptr^.StrucktData.Art <> 3 then

        Wait('Fataler Fehler in Strucktliste, nach Pfad oder Label keine Datei mehr!');

    Lauf := Lauf.Ptr^.Next

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





Procedure LSpeichern(Root: GlobalBaumXPtrtyp; Stelle: Longint);

BEGIN

  If LabelListXPtrtyp(Root).Ptr^.LabelData.Anzahl = 0 then

    DelLabelFeld^[Stelle] := LabelListXPtrtyp(Root)

  Else

    LabelFeld^[Stelle] := LabelListXPtrtyp(Root)

END;



Procedure PSpeichern(Root: GlobalBaumXPtrtyp; Stelle: Longint);

BEGIN

  If PfadListXPtrtyp(Root).Ptr^.PfadData.Anzahl = 0 then

    DelPfadFeld^[Stelle] := PfadListXPtrtyp(Root)

  Else

  PfadFeld^[Stelle] := PfadListXPtrtyp(Root)

END;



Procedure DSpeichern(Root: GlobalBaumXPtrtyp; Stelle: Longint);

BEGIN

  If DateiListXPtrtyp(Root).Ptr^.DateiData.Anzahl = 0 then

    DelDateiFeld^[Stelle] := DateiListXPtrtyp(Root)

  Else

  DateiFeld^[Stelle] := DateiListXPtrtyp(Root)

END;





Procedure MachBinBaum;



var mitte        : Longint;



BEGIN

  Mitte := (l + r) Div 2;



  P := GlobalFeldXPtr^[Mitte];



  GlobalFeldXPtr^[Mitte].Ptr^.Back.Ptr := Nil;

  GlobalFeldXPtr^[Mitte].Ptr^.Next.Ptr := Nil;



  if Mitte > l then MachBinBaum(GlobalFeldXPtr^[Mitte].Ptr^.Back,l,Mitte-1);

  if Mitte < r then MachBinBaum(GlobalFeldXPtr^[Mitte].Ptr^.Next,Mitte+1,r)

END;



Type Speichern = Procedure(Root: GlobalBaumXPtrtyp; Stelle: Longint);



Var  Speichere : Speichern;





Procedure ZaehleElemente(Root: GlobalBaumXPtrtyp);

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





Procedure BaueFeld(Root: GlobalBaumXPtrtyp);

var Hilf : GlobalListXPtrtyp;

BEGIN

  if Root.Ptr <> Nil then

  BEGIN

    BaueFeld(Root.Ptr^.Links);



    if Root.Ptr^.Anzahl <> 0 then



    BEGIN

      If Root.Ptr^.Anzahl < 0 then

      Begin

        Wait('Fehler in Anzahl eines Elementes!');

        Halt

      End;

      Inc(Anzahl);

      Root.Ptr^.Links.Ptr := Pointer(Anzahl);

      Speichere(Root, Anzahl)

    END

    Else

    Begin

      Inc(Deleted);

      Speichere(Root, Deleted)

    End;



    BaueFeld(Root.Ptr^.Rechts)

  end

END;









Procedure LPDFelderBauen;



BEGIN

{  Anzahl := 0;

  ZaehleElemente(GlobalListXPtrtyp(LabelList));

  LAnzahl := Anzahl;

  GetArray(LabelFeld, SizeOf(LabelList), Anzahl);

  Speichere := LSpeichern;

  Anzahl := 0;

  BaueFeld(GlobalListXPtrtyp(LabelList));



  Anzahl := 0;

  ZaehleElemente(GlobalListXPtrtyp(PfadList));

  PAnzahl := Anzahl;

  GetArray(PfadFeld, SizeOf(PfadList), Anzahl);

  Speichere := PSpeichern;

  Anzahl := 0;

  BaueFeld(GlobalListXPtrtyp(PfadList));



  Anzahl := 0;

  ZaehleElemente(GlobalListXPtrtyp(DateiList));

  DAnzahl := Anzahl;

  GetArray(DateiFeld, SizeOf(DateiList), Anzahl);

  Speichere := DSpeichern;

  Anzahl := 0;

  BaueFeld(GlobalListXPtrtyp(DateiList));

}



  Anzahl := 0;

  Deleted := 0;

  ZaehleElemente(GlobalBaumXPtrtyp(LabelList));



  LAnzahl := Anzahl;

  If GetArray(Pointer(LabelFeld)   , SizeOf(LabelList), Anzahl, EMSLF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;





  If GetArray(Pointer(DelLabelFeld), SizeOf(LabelList), Deleted, EMSDLF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;



  Speichere := LSpeichern;

  Anzahl := 0;

  Deleted := 0;

  BaueFeld(GlobalBaumXPtrtyp(LabelList));



  For A := 1 to Deleted do

    Dispose(DelLabelFeld^[A].Ptr);



  ReleaseArray(DelLabelFeld, SizeOf(LabelList), Deleted, EMSDLF);



  Anzahl := 0;

  Deleted := 0;

  ZaehleElemente(GlobalBaumXPtrtyp(PfadList));



  PAnzahl := Anzahl;

  If GetArray(Pointer(PfadFeld)   , SizeOf(PfadList), Anzahl, EMSPF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;





  If GetArray(Pointer(DelPfadFeld), SizeOf(PfadList), Deleted, EMSDPF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;



  Speichere := PSpeichern;

  Anzahl := 0;

  Deleted := 0;

  BaueFeld(GlobalBaumXPtrtyp(PfadList));



  For A := 1 to Deleted do

    Dispose(DelPfadFeld^[A].Ptr);



  ReleaseArray(DelPfadFeld, SizeOf(PfadList), Deleted, EMSDPF);





  Anzahl := 0;

  Deleted := 0;

  ZaehleElemente(GlobalBaumXPtrtyp(DateiList));



  DAnzahl := Anzahl;

  If GetArray(Pointer(DateiFeld)   , SizeOf(DateiList), Anzahl, EMSDF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;





  If GetArray(Pointer(DelDateiFeld), SizeOf(DateiList), Deleted, EMSDDF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;



  Speichere := DSpeichern;

  Anzahl := 0;

  Deleted := 0;

  BaueFeld(GlobalBaumXPtrtyp(DateiList));



  For A := 1 to Deleted do

    Dispose(DelDateiFeld^[A].Ptr);



  ReleaseArray(DelDateiFeld, SizeOf(DateiList), Deleted, EMSDDF);









END;







Procedure SpeichereDateien;

var Lauf         : HilfListPtrtyp;



Procedure StrucktfeldBauen;

var Hilf : StrucktListXPtrtyp;

BEGIN

  (* Wichtig: *)



  Anzahl := 0;

  Hilf := StrucktList;



  (* ZÑhlen der Elemente: *)



  while StrucktList.Ptr <> Nil do

  BEGIN

    Inc(Anzahl);

    StrucktList := StrucktList.Ptr^.Next

  END;



  (* GezÑhlte Elemente: *)



  SAnzahl := Anzahl;



  (* Strucktfeld reservieren: *)



  If GetArray(Pointer(StrucktFeld), SizeOf(StrucktList), SAnzahl, EMSSF) = 1 then

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

    StrucktFeld^[A].Ptr^.Back.Ptr := Pointer(A);

    StrucktList := StrucktList.Ptr^.Next;

  END;

  StrucktList := Hilf

END;







Procedure ZusatzUmlenken;

var hilf : ZusatzListXPtrtyp;

BEGIN

  Hilf := ZusatzList;

  While ZusatzList.Ptr <> Nil do

  BEGIN

    If ZusatzList.Ptr^.ZusatzData.SL.Ptr <> Nil then

      ZusatzList.Ptr^.ZusatzData.sl := ZusatzList.Ptr^.ZusatzData.sl.Ptr^.Back;







    ZusatzList.Ptr^.ZusatzData.sp := ZusatzList.Ptr^.ZusatzData.sp.Ptr^.Back;

    ZusatzList := ZusatzList.Ptr^.Next

  END;

  ZusatzList := Hilf

END;



Procedure HilfUmlenken;

var Hilf : HilfListXPtrtyp;

BEGIN

  Hilf := HilfList;

  For A := 1 to SAnzahl do

    case StrucktFeld^[A].Ptr^.StrucktData.Art of

      4  : BEGIN

             HilfList.Ptr^.HilfData.DLabelList := A;

             HilfList.Ptr^.HilfData.art := 3;

             HilfList := HilfList.Ptr^.Next

           END;

      5  : BEGIN

             HilfList.Ptr^.HilfData.DPfadList := A;

             HilfList.Ptr^.HilfData.art := 4;

             HilfList := HilfList.Ptr^.Next

           end

    END;

  HilfList := Hilf

END;



Procedure ZusatzFeldBauen;

var

    Hilf   : ZusatzListXPtrtyp;

BEGIN

  Anzahl := 0;

  Hilf := ZusatzList;

  while ZusatzList.Ptr <> Nil do

  BEGIN

    Inc(Anzahl);

    ZusatzList.Ptr^.Back.Ptr := Pointer(Anzahl);

    ZusatzList := ZusatzList.Ptr^.Next

  END;

  ZAnzahl := Anzahl;

  ZusatzList := Hilf;



  If GetArray(Pointer(ZusatzFeld), SizeOf(ZusatzList), Anzahl, EMSZF) = 1 then

  Begin

    Wait('Kein Speicher fÅr diese Operation mehr da!');

    Halt

  End;



  (* Feld mit aktuellem Zeiger der Zusatzliste belegen: *)



  For a := 1 to Anzahl do

  BEGIN

    ZusatzFeld^[a] := ZusatzList;

    ZusatzList := ZusatzList.Ptr^.Next

  END;

  ZusatzList := Hilf

END;



Procedure StrucktUmlenken;

BEGIN

  for A := 1 to SAnzahl do

  BEGIN

    case StrucktFeld^[A].Ptr^.StrucktData.art of



      1 : BEGIN

            StrucktFeld^[A].Ptr^.StrucktData.DLabelList :=

            Longint(StrucktFeld^[A].Ptr^.StrucktData.LabelList.Ptr^.Links.Ptr);

            StrucktFeld^[A].Ptr^.StrucktData.art := 4

          END;



      2 : BEGIN

            StrucktFeld^[A].Ptr^.StrucktData.DPfadList :=

            Longint(StrucktFeld^[A].Ptr^.StrucktData.PfadList.Ptr^.Links.Ptr);

            StrucktFeld^[A].Ptr^.StrucktData.art := 5

          END;



      3 : BEGIN

            StrucktFeld^[A].Ptr^.StrucktData.DDateiList :=

            Longint(StrucktFeld^[A].Ptr^.StrucktData.DateiList.Ptr^.Links.Ptr);

            StrucktFeld^[A].Ptr^.StrucktData.art := 6

          end

    end

  end

END;



Procedure DateiUmlenken;

BEGIN

  For A := 1 to DAnzahl do

    DateiFeld^[A].Ptr^.DateiData.Loc := DateiFeld^[A].Ptr^.DateiData.Loc.Ptr^.Back

END;





Var Mem1       : Longint;

    GlobalList : GlobalListXPtrtyp;

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

  GlobalList := GlobalListXPtrtyp(DateiList);

  While GlobalList <> Nil do

  Begin

    If GlobalList^.Anzahl > 0 then Inc(Anzahl);

    GlobalList := GlobalList^.Next

  End;

  If DAnzahl <> Anzahl then Wait('DAnzahl <> Anzahl nach BaumToList!',0);



  Anzahl := 0;

  GlobalList := GlobalListXPtrtyp(PfadList);

  While GlobalList <> Nil do

  Begin

    If GlobalList^.Anzahl > 0 then Inc(Anzahl);

    GlobalList := GlobalList^.Next

  End;

  If PAnzahl <> Anzahl then Wait('PAnzahl <> Anzahl nach BaumToList!',0);



  Anzahl := 0;

  GlobalList := GlobalListXPtrtyp(LabelList);

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

    Write(StrucktDatafile,StrucktFeld^[A].Ptr^.StrucktData);

    Dispose(StrucktFeld^[A].Ptr)

  END;



  While HilfList.Ptr^.Next.Ptr <> Nil do

  BEGIN

    write(HilfDatafile,HilfList.Ptr^.HilfData);

    HilfList := Hilflist.Ptr^.Next;



    Dispose(HilfList.Ptr^.Back.Ptr)

  END;

  Write(HilfDatafile, HilfList.Ptr^.HilfData);

  Dispose(HilfList.Ptr);





  for A := 1 to ZAnzahl do

  BEGIN

    write(ZusatzDatafile,ZusatzFeld^[A].Ptr^.ZusatzData);

    Dispose(ZusatzFeld^[A].Ptr)

  END;



  for A := 1 to LAnzahl do

  BEGIN

    HString := LabelFeld^[A].Ptr^.LabelData.LabelName;

    FillChar(LabelFeld^[A].Ptr^.LabelData.LabelName,

             SizeOf(LabelFeld^[A].Ptr^.LabelData.LabelName),

             ' ');

    LabelFeld^[A].Ptr^.LabelData.LabelName := HString;



    write(LabelDatafile,LabelFeld^[A].Ptr^.LabelData);

    Dispose(LabelFeld^[A].Ptr);

  END;



  for A := 1 to PAnzahl do

  BEGIN

    HString := PfadFeld^[A].Ptr^.PfadData.PfadName;

    FillChar(PfadFeld^[A].Ptr^.PfadData.PfadName,

             SizeOf(PfadFeld^[A].Ptr^.PfadData.PfadName),

             ' ');

    PfadFeld^[A].Ptr^.PfadData.PfadName := HString;



    write(PfadDatafile,PfadFeld^[A].Ptr^.PfadData);

    Dispose(PfadFeld^[A].Ptr)

  END;



  for A := 1 to DAnzahl do

  BEGIN

    HString := DateiFeld^[A].Ptr^.DateiData.DateiName;

    FillChar(DateiFeld^[A].Ptr^.DateiData.DateiName,

             SizeOf(DateiFeld^[A].Ptr^.DateiData.DateiName),

             ' ');

    DateiFeld^[A].Ptr^.DateiData.DateiName := HString;



    write(DateiDatafile,DateiFeld^[A].Ptr^.DateiData);

    Dispose(DateiFeld^[A].Ptr)

  END;



  ReleaseArray(LabelFeld  , SizeOf(LabelList), LAnzahl, EMSLF);

  ReleaseArray(PfadFeld   , SizeOf(PfadList), PAnzahl, EMSPF);

  ReleaseArray(DateiFeld  , SizeOf(DateiList), DAnzahl, EMSDF);

  ReleaseArray(StrucktFeld, SizeOf(StrucktList), SAnzahl, EMSSF);

  ReleaseArray(ZusatzFeld , SizeOf(ZusatzList), ZAnzahl, EMSZF);



  CloseallFiles;



END;





Procedure LadeDateien;

var

    StrucktFeld : StrucktFeldXPtrtyp;

    LabelFeld   : LabelFeldXPtrtyp;

    PfadFeld    : PfadFeldXPtrtyp;

    DateiFeld   : DateiFeldXPtrtyp;

    ZusatzFeld  : ZusatzFeldXPtrtyp;



    Lauf        : HilfListXPtrtyp;











Procedure LadeHilf;

BEGIN

  Anzahl := Filesize(HilfDatafile);

  if Anzahl <> 0 then

  BEGIN

    new(HilfList.Ptr);

    HilfList.Ptr^.Back.Ptr := Nil;

    HilfListend := HilfList;

    AltHilf.Ptr := Nil;

    For A := 1 to Anzahl do

    BEGIN

      Read(HilfDatafile,HilfListEnd.Ptr^.HilfData);

      If Not (HilfListEnd.Ptr^.HilfData.Art in [3,4]) then

        Wait('Beim Laden der Hilf - Datei wurde ein Fehler gefunden ( Art <> 3, 4 )!');

      New(HilfListend.Ptr^.Next.Ptr);

      HilfListend.Ptr^.Next.Ptr^.Back := HilfListEnd;

      HilfListend := HilfListend.Ptr^.Next

    END;

    HilfListend := HilfListend.Ptr^.Back;

    Dispose(HilfListend.Ptr^.Next.Ptr);

    HilfListend.Ptr^.Next.Ptr := Nil

  end

END;



Procedure LadeStruckt;

BEGIN

  SAnzahl := Filesize(StrucktDatafile);



  if SAnzahl <> 0 then

  BEGIN

    NewArray(StrucktFeld, SizeOf(StrucktList), SAnzahl);

    For A := 1 to SAnzahl do

    BEGIN

      new(StrucktFeld^[A].Ptr);

      read(StrucktDatafile,StrucktFeld^[A].Ptr^.StrucktData);

    end

  end

END;



Procedure WandleHilf;

BEGIN

  Lauf := HilfList;

  while Lauf.Ptr <> Nil do

  BEGIN

    case Lauf.Ptr^.HilfData.art of

     3  : BEGIN

            Stelle := Lauf.Ptr^.HilfData.DLabelList;

            Lauf.Ptr^.HilfData.art := 1;

            Lauf.Ptr^.HilfData.LabelList := StrucktFeld^[Stelle]

          END;

     4  : BEGIN

            Stelle := Lauf.Ptr^.HilfData.DPfadList;

            Lauf.Ptr^.HilfData.art := 2;

            Lauf.Ptr^.HilfData.PfadList := StrucktFeld^[Stelle]

          end

    Else Wait('Hilfelement <> 3, 4!')

    END;

    Lauf := Lauf.Ptr^.Next

  end

END;



Procedure LadeZusatz;

BEGIN

  ZAnzahl := Filesize(ZusatzDatafile);

  if ZAnzahl <> 0 then

  BEGIN

    NewArray(ZusatzFeld, SizeOf(ZusatzList), ZAnzahl);

    For A := 1 to ZAnzahl do

    BEGIN

      new(ZusatzFeld^[A].Ptr);

      read(ZusatzDatafile,ZusatzFeld^[A].Ptr^.ZusatzData)

    end

  end

END;



Procedure WandleZusatz;

BEGIN

  For A := 1 to ZAnzahl do

  BEGIN

    ZusatzFeld^[A].Ptr^.ZusatzData.sl :=

    StrucktFeld^[Longint(ZusatzFeld^[A].Ptr^.ZusatzData.sl.Ptr)];

    ZusatzFeld^[A].Ptr^.ZusatzData.sp :=

    StrucktFeld^[Longint(ZusatzFeld^[A].Ptr^.ZusatzData.sp.Ptr)]

  end

END;



Procedure LadeLabel;

BEGIN

  LAnzahl := Filesize(LabelDatafile);

  if LAnzahl <> 0 then

  BEGIN

    NewArray(LabelFeld, SizeOf(LabelList), LAnzahl);

    For A := 1 to LAnzahl do

    BEGIN

      new(LabelFeld^[A].Ptr);

      read(LabelDatafile,LabelFeld^[A].Ptr^.LabelData)

    end

  end

END;



Procedure LadePfad;

BEGIN

  PAnzahl := Filesize(PfadDatafile);

  if PAnzahl <> 0 then

  BEGIN

    NewArray(PfadFeld, SizeOf(PfadList), PAnzahl);

    For A := 1 to PAnzahl do

    BEGIN

      new(PfadFeld^[A].Ptr);

      read(PfadDatafile,PfadFeld^[A].Ptr^.PfadData)

    end

  end

END;



Procedure LadeDatei;

BEGIN

  DAnzahl := Filesize(DateiDatafile);

  if DAnzahl <> 0 then

  BEGIN

    NewArray(DateiFeld, SizeOf(DateiList), DAnzahl);

    For A := 1 to DAnzahl do

    BEGIN

      new(DateiFeld^[A].Ptr);

      read(DateiDatafile,DateiFeld^[A].Ptr^.DateiData);

    end

  end

END;



Procedure WandleStruckt;

BEGIN

  For A := 1 to SAnzahl do

  BEGIN

    case StrucktFeld^[A].Ptr^.StrucktData.art of

     4  : BEGIN

            Stelle := StrucktFeld^[A].Ptr^.StrucktData.DLabelList;

            StrucktFeld^[A].Ptr^.StrucktData.art := 1;

            StrucktFeld^[A].Ptr^.StrucktData.LabelList := LabelFeld^[Stelle]

          END;

     5  : BEGIN

            Stelle := StrucktFeld^[A].Ptr^.StrucktData.DPfadList;

            StrucktFeld^[A].Ptr^.StrucktData.art := 2;

            StrucktFeld^[A].Ptr^.StrucktData.PfadList := PfadFeld^[Stelle]

          END;

     6  : BEGIN

            Stelle := StrucktFeld^[A].Ptr^.StrucktData.DDateiList;

            StrucktFeld^[A].Ptr^.StrucktData.art := 3;

            StrucktFeld^[A].Ptr^.StrucktData.DateiList := DateiFeld^[Stelle]

          end

    end

  end

END;



Procedure WandleDatei;

BEGIN

  For A := 1 to DAnzahl do

  BEGIN

    Stelle := Longint(DateiFeld^[A].Ptr^.DateiData.Loc.Ptr);

    DateiFeld^[A].Ptr^.DateiData.Loc := ZusatzFeld^[Stelle]

  end

END;



Var Hilf: StrucktListXPtrtyp;



BEGIN

  OpenAllFiles;

  



  LadeHilf;     (* Als Liste *)

  LadeStruckt;  (* Als Feld  *)

  WandleHilf;

  LadeZusatz;   (* Als Feld  *)

  WandleZusatz;





  LadeLabel;  (* Als Feld  *)

  LadePfad;   (* Als Feld  *)

  LadeDatei;  (* Als Feld  *)

  WandleStruckt;

  WandleDatei;





(* --- Verketten der Strucktliste --- *)



  For A := 1 to SAnzahl-1 do

  BEGIN

    StrucktFeld^[A].Ptr^.Next := StrucktFeld^[A+1];

    StrucktFeld^[A+1].Ptr^.Back := StrucktFeld^[A]

  END;

  StrucktFeld^[1].Ptr^.Back.Ptr := Nil;

  StrucktFeld^[SAnzahl].Ptr^.Next.Ptr := Nil;





  StrucktList := StrucktFeld^[1];

  StrucktListend := StrucktFeld^[SAnzahl];



  ReleaseArray(StrucktFeld, SizeOf(StrucktList), SAnzahl, EMSSF);

  StrucktFeld := Nil;







(* ---    Umwandeln des Restes    --- *)



  For A := 1 to ZAnzahl-1 do

  BEGIN

    ZusatzFeld^[A].Ptr^.Next := ZusatzFeld^[A+1];

    ZusatzFeld^[A+1].Ptr^.Back := ZusatzFeld^[A]

  END;

  ZusatzFeld^[1].Ptr^.Back.Ptr := Nil;

  ZusatzFeld^[ZAnzahl].Ptr^.Next.Ptr := Nil;



  ZusatzList := ZusatzFeld^[1];

  ZusatzListend := ZusatzFeld^[ZAnzahl];



  ReleaseArray(ZusatzFeld, SizeOf(ZusatzList), ZAnzahl, EMSZF);

  ZusatzFeld := Nil;







  (* Mache die BÑume Label, Pfad und Datei: *)





  LabelList.Ptr := Nil;

  PfadList.Ptr := Nil;

  DateiList.Ptr := Nil;

  LabelDif := False;

  PfadDif := False;

  DateiDif := False;

  NoNew := True;



  GlobalFeldXPtr := Pointer(DateiFeld);



(*  MachBinBaum(GlobalListXPtrtyp(DateiList),1,DAnzahl);*)



  Hilf := StrucktList;

  While StrucktList.Ptr <> Nil do

  Begin

    Case StrucktList.Ptr^.StrucktData.Art of

      1 : Begin

            TreePointer.Ptr := StrucktList.Ptr^.StrucktData.LabelList.Ptr;

            TreePointer.BlockNr := StrucktList.Ptr^.StrucktData.LabelList.BlockNr;

            SpeichereLabel(StrucktList.Ptr^.

                           StrucktData.LabelList.Ptr^.

                           LabelData.LabelName,

                           LabelList.Ptr,

                           LabelDif,

                           NoNew);

            StrucktList.Ptr^.StrucktData.LabelList := Labeladress

          End;

      2 : Begin

            TreePointer.Ptr := StrucktList.Ptr^.StrucktData.PfadList.Ptr;

            TreePointer.BlockNr := StrucktList.Ptr^.StrucktData.PfadList.BlockNr;

            SpeicherePfad (StrucktList.Ptr^.

                           StrucktData.PfadList.Ptr^.

                           PfadData.PfadName,

                           PfadList.Ptr,

                           PfadDif,

                           NoNew);

            StrucktList.Ptr^.StrucktData.PfadList := Pfadadress

          End;

      3 : Begin

            TreePointer.Ptr := StrucktList.Ptr^.StrucktData.DateiList.Ptr;

            TreePointer.BlockNr := StrucktList.Ptr^.StrucktData.DateiList.BlockNr;

            SpeichereDatei(StrucktList.Ptr^.

                           StrucktData.DateiList.Ptr^.

                           DateiData.DateiName,

                           DateiList.Ptr,

                           DateiDif,

                           NoNew);

            StrucktList.Ptr^.StrucktData.DateiList := Dateiadress

          End;

    End;

    StrucktList := StrucktList.Ptr^.Next

  End;

  StrucktList := Hilf;



  ReleaseArray(DateiFeld, SizeOf(DateiList), DAnzahl, EMSDF);

  DateiFeld := Nil;



  ReleaseArray(PfadFeld, SizeOf(PfadList), PAnzahl, EMSPF);

  PfadFeld := Nil;



  ReleaseArray(LabelFeld, SizeOf(LabelList), LAnzahl, EMSLF);

  LabelFeld := Nil;



  NoNew := False;





  CloseallFiles





END;



















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



Function PasstinMaske(Maske, Name: String12):Boolean;

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









Procedure writexy;

begin

  gotoxy(x,y);

  write(txt)

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

Procedure GetHilf;
begin
  if Ende = nil then
  begin
    new(Ende);
    HilfList.Ptr := Ende;
    Ende^.Next.Ptr := Nil;
    Ende^.Back.Ptr := Nil
  end
  else
  begin
    if cut <> Nil then
    begin
      (* Liste getrennt: *)

      (* Ende mit Cut verbinden: *)
      Ende^.Next.Ptr := Cut;
      Cut^.Back.Ptr := Ende;

      (* Ende weiterrÅcken: *)
      Ende := Ende^.Next.Ptr;
      (* Cut weiterrÅcken: *)
      Cut := Cut^.Next.Ptr;

      (* Und vor Cut wieder trennen: *)
      Cut^.Back.Ptr := Nil;
      Ende^.Next.Ptr := Nil

    end
    else
    begin
      (* Liste nicht getrennt: *)
      (* Cut = Nil             *)

      New(Ende^.Next.Ptr);
      Ende^.Next.Ptr^.Back.Ptr := Ende;
      Ende := Ende^.Next.Ptr;
      Ende^.Next.Ptr := Nil
    end
  end
end;


Procedure GetStruckt;
begin
  if Ende = nil then
  begin
    new(Ende);
    StrucktList.Ptr := Ende;
    Ende^.Next.Ptr := Nil;
    Ende^.Back.Ptr := Nil
  end
  else
  begin
    if cut <> Nil then
    begin
      (* Liste getrennt: *)

      (* Ende mit Cut verbinden: *)
      Ende^.Next.Ptr := Cut;
      Cut^.Back.Ptr := Ende;

      (* Ende weiterrÅcken: *)
      Ende := Ende^.Next.Ptr;
      (* Cut weiterrÅcken: *)
      Cut := Cut^.Next.Ptr;

      (* Und vor Cut wieder trennen: *)
      Cut^.Back.Ptr := Nil;
      Ende^.Next.Ptr := Nil

    end
    else
    begin
      (* Liste nicht getrennt: *)
      (* Cut = Nil             *)

      New(Ende^.Next.Ptr);
      Ende^.Next.Ptr^.Back.Ptr := Ende;
      Ende := Ende^.Next.Ptr;
      Ende^.Next.Ptr := Nil
    end
  end
end;





Procedure Fehler;

begin

  gotoxy(1,24);

  write(txt);

  ch := upcase(readkey);

  gotoxy(1,24);

  clreol

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



  SpeicherePfad(Pfad, PfadList.Ptr, PfadDif, NoNew);



  (* Neues Element in Strucktliste erzeugen: *)



  GetStruckt(StrucktListend.Ptr,AltStruckt.Ptr);



  (* FÅr Zusatzdaten: *)

  Pfadzeiger := StrucktListEnd;



  (* Zuweisung in das Element: *)



  StrucktListend.Ptr^.StrucktData.art := 2;

  StrucktListend.Ptr^.StrucktData.PfadList := pfadadress;



  (* Neues Element in Hilfliste erzeugen: *)



  GetHilf(HilfListend.Ptr,AltHilf.Ptr);



  (* Zuweisung in das Element: *)



  HilfListend.Ptr^.HilfData.art := 2;

  HilfListend.Ptr^.HilfData.PfadList.Ptr := Pointer(StrucktListend.Ptr);







  FindFirst(pfad+mask,attr,sr);

  while DosError = 0 do

  begin

    if (sr.attr and (directory + VolumeID)) = 0 then

    begin

      SpeichereDatei(sr.name, DateiList.Ptr, DateiDif, NoNew);



      (* Neues Element in Strucktliste erzeugen: *)



      GetStruckt(StrucktListend.Ptr,AltStruckt.Ptr);



      (* Zuweisung in das Element: *)



      StrucktListend.Ptr^.StrucktData.art := 3;

      StrucktListend.Ptr^.StrucktData.DateiList := Dateiadress;



      SpeichereSuch(Labelzeiger.Ptr,Pfadzeiger.Ptr,Dateiadress.Ptr)

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



    FindFirst(Pfad + Befehl + Extender, AnyFile And Not VolumeId, srec);

    if Doserror = 0 then

    begin

      SpeichereLabel(lab,LabelList.Ptr,LabelDif, NoNew);

      Name := Lab;



      GetStruckt(StrucktListend.Ptr,AltStruckt.Ptr);



      StrucktListend.Ptr^.StrucktData.art := 1;

      StrucktListend.Ptr^.StrucktData.LabelList := Labeladress;





      GetHilf(HilfListend.Ptr,AltHilf.Ptr);



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


Procedure Initfiles;
BEGIN
  assign(StrucktDatafile,Strucktfilename);
  assign(HilfDatafile,Hilffilename);
  assign(DateiDatafile,Dateifilename);
  assign(LabelDatafile,Labelfilename);
  assign(PfadDatafile,Pfadfilename);
  assign(ZusatzDatafile,Zusatzfilename);
END;

Procedure CursorAus;
begin
  Inline($B4/$01/$B9/$00/$0F/$CD/$10);
end;


end.

