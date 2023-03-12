Unit VT; (* Vokabeltrainer *)
{$X+ $O+ $F+}

Interface

Uses Crt,
     Dos,
     Printer,
     App,
     Objects,
     TVVTCmds,
     Drivers,
     Memory,
     Views,
     Dialogs,
     DEdit,
     MsgBox,
     Video,
     Filefind,
     Strings,
     StrTools;

Type
  String80 = String[80];

  PTrans                   = ^TTrans;
  TTrans                   = Object(TObject)
    Translate              : Integer;

    Constructor Init(Tr: Integer);
    Destructor Done; Virtual;
  End;

  PVokabeltyp              = ^Vokabeltyp;
  Vokabeltyp               = Record
    Wort                   : String80;
    Datum                  : String[12];
    ED                     : Boolean;
    Anzahl                 : Integer;
    Tested                 : Boolean;
    ZL                     : Integer;
  End;

  PVokabel                 = ^TVokabel;
  TVokabel                 = Object(TObject)
    Vokabel                : PVokabeltyp;

    Constructor Init(VokabelData: Vokabeltyp);
    Destructor Done; Virtual;
    Procedure SetData(Var Rec); Virtual;
    Function  GetData:PVokabeltyp; Virtual;
    Procedure SetDatum; Virtual;
  End;

  PVokabelCollection       = ^TVokabelCollection;
  TVokabelCollection       = Object(TSortedCollection)
    Constructor Init(ALimit, ADelta: Integer);
    Destructor Done; Virtual;
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function KeyOf(Item: Pointer): Pointer; Virtual;
    Procedure Print(TransVColl: PVokabelCollection;
                    TransTColl: PCollection); Virtual;
  End;

  PVokabelListBox          = ^TVokabelListBox;
  TVokabelListBox          = Object(TListBox)
    Constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
    Destructor Done; Virtual;
    Function GetText(Item: Integer; MaxLen: Integer): String; Virtual;
  End;

  VFtyp = File of Vokabeltyp;


  TFtyp = File of Integer;


Var

    Save                   : Boolean;

    AEDTransCollection     : PCollection;
    ADETransCollection     : PCollection;
    AEnglischCollection    : PVokabelCollection;
    ADeutschCollection     : PVokabelCollection;

    Vokabel                : Vokabeltyp;
    AVokabel               : PVokabel;
    ATrans                 : PTrans;


    EVF,
    DVF     : VFtyp;

    EDTF,
    DETF    : TFtyp;

    NoFiles : Boolean;

Var ATranslateCollection   : PVokabelCollection;



Type


  pVokabelneditierenDialog = ^tVokabelneditierenDialog;
  tVokabelneditierenDialog = OBJECT (tDialog)

    ADERadioButton         : PRadioButtons;

    EditEItem,
    EditDItem              : Integer;

    AEnglischListBox       : PVokabelListBox;
    ADeutschListBox        : PVokabelListBox;
    ATranslateListBox      : PVokabelListBox;


    AEnglischString        : PStr;
    ADeutschString         : PStr;
  
    AEnglischScrollBar     : PScrollBar;
    ADeutschScrollBar      : PScrollBar;
    ATranslateScrollBar    : PScrollBar;

    AEnglischEingabe       : PInputLine;
    ADeutschEingabe        : PInputLine;

    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Destructor  Done; Virtual;

    Procedure HandleEvent(Var Event: TEvent); Virtual;
  END;

Var     Trans,
        Item        : Integer;
        DoDecStelle : Integer;



Procedure VokabelnEditierenDialog;

Procedure SaveEnglischCollection(AColl: PVokabelCollection);
Procedure SaveDeutschCollection(AColl: PVokabelCollection);
Procedure SaveEDTransCollection(AColl: PCollection);
Procedure SaveDETransCollection(AColl: PCollection);
Procedure VokabelDateiOeffnen(Var Datei: VFtyp);
Procedure TranslateDateiOeffnen(Var Datei: TFtyp);
Function TranslateDateiTest(Var Datei: TFtyp): Boolean;
Function VokabelDateiTest(Var Datei: VFtyp): Boolean;

Implementation


Procedure VokabelDateiOeffnen;
begin
  (*$I-*)
  reset(Datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Vokabel-Datei kann nicht geîffnet werden !');
    delay(1000);
    Halt
  end
  (*$I+*)
end;

Procedure TranslateDateiOeffnen;
begin
  (*$I-*)
  reset(Datei);
  if IOResult <> 0 then rewrite(Datei);
  if IOResult <> 0 then
  begin
    writeln;
    write('Translate-Datei kann nicht geîffnet werden !');
    delay(1000);
    Halt
  end
  (*$I+*)
end;

Function TranslateDateiTest;
Var Test: Boolean;
begin
  (*$I-*)
  reset(Datei);
  Test := IOResult = 0;
  If Test then Close(Datei);
  TranslateDateiTest := Test
  (*$I+*)
end;

Function VokabelDateiTest;
Var Test: Boolean;
begin
  (*$I-*)
  reset(Datei);
  Test := IOResult = 0;
  If Test then Close(Datei);
  VokabelDateiTest := Test
  (*$I+*)
end;




Procedure SaveEnglischCollection;

Procedure SaveEVokabel(Vokabel: PVokabel); Far;
Begin
  Write(EVF, Vokabel^.Vokabel^)
End;

Begin
  AColl^.ForEach(@SaveEVokabel)
End;

Procedure SaveDeutschCollection;

Procedure SaveDVokabel(Vokabel: PVokabel); Far;
Begin
  Write(DVF, Vokabel^.Vokabel^)
End;

Begin
  AColl^.ForEach(@SaveDVokabel)
End;

Procedure SaveEDTransCollection;

Procedure SaveEDTranslate(Trans: PTrans); Far;
Begin
  Write(EDTF, Trans^.Translate)
End;

Begin
  AColl^.ForEach(@SaveEDTranslate);
End;

Procedure SaveDETransCollection;

Procedure SaveDETranslate(Trans: PTrans); Far;
Begin
  Write(DETF, Trans^.Translate)
End;

Begin
  AColl^.ForEach(@SaveDETranslate);
End;


Constructor TTrans.Init(Tr: Integer);
Begin
  TObject.Init;
  Translate := Tr
End;

Destructor TTrans.Done;
Begin
  TObject.Done
End;

  
Constructor TVokabel.Init(VokabelData: Vokabeltyp);
Begin
  TObject.Init;
  New(Vokabel);
  Vokabel^ := VokabelData;
End;

Destructor TVokabel.Done;
Begin
  Dispose(Vokabel);
  TObject.Done
End;

Procedure TVokabel.SetDatum;
Var DayofWeek, Tag, Monat, Jahr: Word;

Begin
  GetDate(Jahr, Monat, Tag, DayofWeek);
  Vokabel^.Datum := LONGINTtoSTRING(Tag, 10, 10) +   '.' +
                    LONGINTtoSTRING(Monat, 10, 10) + '.' +
                    LONGINTtoSTRING(Jahr, 10, 10);
  Vokabel^.Tested := False;
End;

Procedure TVokabel.SetData(Var Rec);
Var V: PVokabeltyp Absolute Rec;
Begin
  If Vokabel = Nil then New(Vokabel);
  Vokabel := V;
End;

Function  TVokabel.GetData:PVokabeltyp;
Begin
  GetData := Vokabel
End;

Constructor TVokabelCollection.Init(ALimit, ADelta: Integer);
Begin
  TSortedCollection.Init(ALimit, ADelta)
End;

Destructor TVokabelCollection.Done;
Begin
  FreeAll;
  TSortedCollection.Done;
End;

Procedure TVokabelCollection.Print(TransVColl: PVokabelCollection;
                                   TransTColl: PCollection);

Var DZeile: Integer;
    DSeite: Integer;

Procedure DruckZeile(Vokabel: PVokabel); Far;
Var Zeile: String;
Var A    : Integer;
Begin

  If DZeile = 0 then
  Begin
    Zeile := 'Seite: ' + LONGINTtoSTRING(DSeite, 10, 10);
    While Length(Zeile) < 75 do
    Zeile := ' ' + Zeile;

    Writeln(Lst, Zeile);
    Writeln(Lst);
    Inc(DZeile, 2);
    Inc(DSeite)
  End;

  If DZeile + Vokabel^.Vokabel^.Anzahl - 1 > 58 then
  Begin
    DZeile := 0;
    Write(Lst, #12);
  End;

  For A:= 0 to Vokabel^.Vokabel^.Anzahl - 1 do
  Begin
    Inc(DZeile);


    Zeile := '';

    If A = 0 then
    Begin
      Zeile := '     ' + Vokabel^.Vokabel^.Wort;

      Zeile := Zeile + ' ';

      While Length(Zeile) < 39 do Zeile := Zeile + '.'
    End
    Else
      While Length(Zeile) < 39 do Zeile := Zeile + ' ';


    Zeile := Zeile + ' ' +
             PVokabel(
             TransVColl^.At(
             PTrans(
             TransTColl^.At(
             Vokabel^.
             Vokabel^.ZL + A))^.
             Translate))^.Vokabel^.Wort;



    Writeln(Lst, Zeile)
  End;
  If Vokabel^.Vokabel^.Anzahl > 1 then
  Begin
    Inc(DZeile);
    Writeln(Lst)
  End
End;

Begin
  DZeile := 0;
  DSeite := 1;
  If Count <> 0 then
  Begin
    ForEach(@DruckZeile);
    Writeln(Lst, #12)
  End
End;

Function TVokabelCollection.Compare(Key1, Key2: Pointer): Integer;
Begin
  (* Diese Routine ist nicht korreckt geschrieben. In Verbindung *)
  (* mit der direckten Anwendung von Search ist alles wieder Ok. *)

  If DownStr(String(Key1^)) < DownStr(PVokabel(Key2)^.Vokabel^.Wort) then
    Compare := - 1;
  If DownStr(String(Key1^)) = DownStr(PVokabel(Key2)^.Vokabel^.Wort) then
    Compare := 0;
  If DownStr(String(Key1^)) > DownStr(PVokabel(Key2)^.Vokabel^.Wort) then
    Compare := 1;
End;

Function TVokabelCollection.KeyOf(Item: Pointer): Pointer;
Begin
  KeyOf := @PVokabel(Item)^.Vokabel^.Wort
End;

Constructor TVokabelListBox.Init(var Bounds: TRect;
                                   ANumCols: Word;
                                 AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Destructor TVokabelListBox.Done;
Begin
  TListBox.Done
End;

Function TVokabelListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := PVokabel(List^.At(Item))^.Vokabel^.Wort
End;

Destructor tVokabelneditierenDialog.Done;
Var F: File;
Begin
  If Save = True then
  Begin
    Assign(F, 'PRDatum.Dat');
    {$I-}
    Reset(F);
    {$I+}

    If IOResult = 0 then
    Begin
      System.Close(F);
      Erase(F)
    End
  End;


  Dispose(AEnglischEingabe, Done);
  Dispose(AEnglischScrollBar, Done);
  Dispose(AEnglischListBox, Done);

  Dispose(ADeutschEingabe, Done);
  Dispose(ADeutschScrollBar, Done);
  Dispose(ADeutschListBox, Done);

  ATranslateCollection^.DeleteAll;
  Dispose(ATranslateCollection, Done);
  Dispose(ATranslateScrollBar, Done);
  Dispose(ATranslateListBox, Done);


  (*Dispose(ADERadioButton, Done);*)

  tDialog.Done;
End;

CONSTRUCTOR tVokabelneditierenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
BEGIN
  tDialog.Init (Bounds, aTitle);

  (*********************)
  (* Translate Listbox *)
  (*********************)

  R.Assign (38, 3+11, 39, 8+11);
  ATranslateScrollBar := New (pScrollBar, Init  (R));
  ATranslateScrollBar^.SetRange (0, 0);
  Insert(ATranslateScrollBar);

  R.Assign (3, 3+11, 38, 8+11);
  ATranslateListBox := New (pVokabelListBox, Init (R, 1, pScrollBar (ATranslateScrollBar)));
  Insert (ATranslateListBox);
  R.Assign (3, 2+11, 15, 3+11);
  Insert (New (pLabel, Init (R, 'öber~s~etzung', ATranslateListBox)));

  (***********************)
  (* TranslateCollection *)
  (***********************)

  ATranslateCollection := New(PVokabelCollection, Init(1, 1));

  ATranslateListBox^.Options := ATranslateListBox^.Options And Not ofSelectable;

  ATranslateListBox^.NewList(ATranslateCollection);

  ATranslateListBox^.SetRange(0);

  ATranslateListBox^.EventMask := FocusedEvents + PositionalEvents;


  (*********************)
  (* Englische Listbox *)
  (*********************)

  R.Assign (38, 3, 39, 8);
  AEnglischScrollBar := New (pScrollBar, Init  (R));
  AEnglischScrollBar^.SetRange (0, 0);
  Insert(AEnglischScrollBar);

  R.Assign (3, 3, 38, 8);
  AEnglischListBox := New (pVokabelListBox, Init (R, 1, pScrollBar (AEnglischScrollBar)));
  Insert (AEnglischListBox);
  R.Assign (3, 2, 14, 3);
  Insert (New (pLabel, Init (R, '~e~nglisch', AEnglischListBox)));


  (************************)
  (* Englische Collection *)
  (************************)

  If AEnglischCollection^.Count > 0 then
    AEnglischListBox^.Options := AEnglischListBox^.Options or ofSelectable
  Else
    AEnglischListBox^.Options := AEnglischListBox^.Options And Not ofSelectable;

  AEnglischListBox^.NewList(AEnglischCollection);

  AEnglischListBox^.SetRange(AEnglischCollection^.Count);

  AEnglischListBox^.EventMask := FocusedEvents + PositionalEvents;

  (********************)
  (* Deutsche Listbox *)
  (********************)

  R.Assign (75, 3, 76, 8);
  ADeutschScrollBar := New (pScrollBar, Init  (R));
  ADeutschScrollBar^.SetRange (0, 0);
  Insert(ADeutschScrollBar);

  R.Assign (40, 3, 75, 8);
  ADeutschListBox := New (pVokabelListBox, Init (R, 1, pScrollBar (ADeutschScrollBar)));
  Insert (ADeutschListBox);
  R.Assign (40, 2, 50, 3);
  Insert (New (pLabel, Init (R, '~d~eutsch', ADeutschListBox)));

  (***********************)
  (* Deutsche Collection *)
  (***********************)

  
  
  If ADeutschCollection^.Count > 0 then
    ADeutschListBox^.Options := ADeutschListBox^.Options or ofSelectable
  Else
    ADeutschListBox^.Options := ADeutschListBox^.Options And Not ofSelectable;

  ADeutschListBox^.NewList(ADeutschCollection);

  ADeutschListBox^.SetRange(ADeutschCollection^.Count);

  ADeutschListBox^.EventMask := FocusedEvents + PositionalEvents;



  R.Assign (3, 11, 39, 12);
  AEnglischEingabe := New (pInputLine, Init (R, 80));
  Insert (AEnglischEingabe);
  R.Assign (3, 10, 26, 11);
  Insert (New (pLabel, Init (R, '~W~orteingabe englisch', AEnglischEingabe)));

  R.Assign (40, 11, 76, 12);
  ADeutschEingabe := New (pInputLine, Init (R, 80));
  Insert (ADeutschEingabe);
  R.Assign (40, 10, 62, 11);
  Insert (New (pLabel, Init (R, 'W~o~rteingabe deutsch', ADeutschEingabe)));


  R.Assign (40, 14, 55, 16);
  Insert (New (pButton, Init (R, 'Ent~f~ernen', cmVVEEntfernen, 0)));

  R.Assign (40, 17, 55, 19);
  Insert (New (pButton, Init (R, '~V~orhanden ?', cmVVV, 0)));

  R.Assign (42+20, 14, 57+20, 16);
  Insert (New (pButton, Init (R, 'öber~n~ahme', cmVVESpeichern, 0)));
 
  R.Assign (42+20, 17, 57+20, 19);
  Insert (New (pButton, Init (R, ' ~B~eenden ', cmOk, 0)));
 
  SelectNext (FALSE);
END;


Procedure tVokabelneditierenDialog.HandleEvent(Var Event: TEvent);
Var EVokabel,
    DVokabel    : PVokabel;
    EDTrans,
    DETrans     : PTrans;
    Vokabel1,
    Vokabel2    : Vokabeltyp;
    Wort        : String;
    EItem,
    DItem,
    EDTranslate,
    DETranslate : Integer;
    ETrue,
    DTrue       : Boolean;
    

(*************************)
(* Vokabelpaar einfugen: *)
(*************************)


Procedure AktVInd(Vokabel: PVokabel); Far;
Begin
  If Vokabel^.Vokabel^.ZL > Item then Inc(Vokabel^.Vokabel^.ZL)
End;

Procedure AktTInd(Translate: PTrans); Far;
Begin
  If Translate^.Translate >= Trans then Inc(Translate^.Translate)
End;



Procedure ECB;
Begin
  If EDTranslate < AEDTransCollection^.Count then
  Begin
    Item := EDTranslate;
    AEnglischCollection^.ForEach(@AktVInd)
  End;
End;

Procedure DETB;
Begin
  If EItem < AEnglischCollection^.Count then
  Begin
    Trans := EItem;
    ADETransCollection^.ForEach(@AktTInd)
  End;
End;

Procedure EDTB;
Begin
  If DItem < ADeutschCollection^.Count then
  Begin
    Trans := DItem;
    AEDTransCollection^.ForEach(@AktTInd)
  End
End;

Procedure DCB;
Begin
  If DETranslate < ADETransCollection^.Count then
  Begin
    Item := DETranslate;
    ADeutschCollection^.ForEach(@AktVInd)
  End
End;

Procedure ENE;
Begin
  Dispose(EVokabel, Done);

  EDTrans := New(PTrans, Init(DItem));
  AEDTransCollection^.AtInsert(EDTranslate, EDTrans);

  Inc(PVokabel(AEnglischCollection^.At(EItem))^.Vokabel^.Anzahl);
End;

Procedure EE;
Begin
  EDTrans := New(PTrans, Init(DItem));
  EDTranslate := AEDTransCollection^.Count;
  AEDTransCollection^.Insert(EDTrans);

  EVokabel^.Vokabel^.ZL := EDTranslate;

  AEnglischCollection^.AtInsert(EItem, EVokabel);

  AEnglischListBox^.SetRange(AEnglischListBox^.List^.Count);


  AEnglischListBox^.Options := AEnglischListBox^.Options or
                               ofSelectable;
End;


Procedure DNE;
Begin
  Dispose(DVokabel, Done);

  DETrans := New(PTrans, Init(EItem));
  ADETransCollection^.AtInsert(DETranslate, DETrans);

  Inc(PVokabel(ADeutschCollection^.At(DItem))^.Vokabel^.Anzahl);
End;


Procedure DE;
Begin
  DETrans := New(PTrans, Init(EItem));
  DETranslate := ADETransCollection^.Count;
  ADETransCollection^.Insert(DETrans);

  DVokabel^.Vokabel^.ZL := DETranslate;

  ADeutschCollection^.AtInsert(DItem, DVokabel);

  ADeutschListBox^.SetRange(ADeutschListBox^.List^.Count);
  ADeutschListBox^.Options := ADeutschListBox^.Options or
                              ofSelectable;
End;

(**************************)
(* Vokabelpaar entfernen: *)
(**************************)


Procedure DoVDec(Vokabel: PVokabel); Far;
Begin
  If Vokabel^.Vokabel^.ZL > DoDecStelle then
    Dec(Vokabel^.Vokabel^.ZL)
End;

Procedure DoTDec(Translate: PTrans); Far;
Begin
  If Translate^.Translate > DoDecStelle then
    Dec(Translate^.Translate)
End;


Procedure DelETrans;
Begin
  (* Lîschen des Translateelementes: *)

  AEDTransCollection^.FreeItem(AEDTransCollection^.At(EDTranslate));
  AEDTransCollection^.AtDelete(EDTranslate);

  (* Zeiger in VokabelColl, die grî·er als EDTranslate sind, *)
  (* um eins Decrementieren:                                 *)

  DoDecStelle := EDTranslate;

  AEnglischCollection^.ForEach(@DoVDec);
End;

Procedure DelDTrans;
Begin
  (* Lîschen des Translateelementes: *)

  ADETransCollection^.FreeItem(ADETransCollection^.At(DETranslate));
  ADETransCollection^.AtDelete(DETranslate);

  (* Zeiger in VokabelColl, die grî·er als DETranslate sind, *)
  (* um eins Decrementieren:                                 *)

  DoDecStelle := DETranslate;

  ADeutschCollection^.ForEach(@DoVDec);
End;

Procedure DelEWort;
Begin
  If PVokabel(AEnglischCollection^.At(EItem))^.Vokabel^.Anzahl = 1 then
  Begin

    (* Lîschen des Vokabelelementes: *)

    AEnglischCollection^.FreeItem(AEnglischCollection^.At(EItem));
    AEnglischCollection^.AtDelete(EItem);

    DoDecStelle := EItem;

    ADETransCollection^.ForEach(@DoTDec)

  End
  Else Dec(PVokabel(AEnglischCollection^.At(EItem))^.Vokabel^.Anzahl)
End;

Procedure DelDWort;
Begin
  If PVokabel(ADeutschCollection^.At(DItem))^.Vokabel^.Anzahl = 1 then
  Begin

    (* Lîschen des Vokabelelementes: *)

    ADeutschCollection^.FreeItem(ADeutschCollection^.At(DItem));
    ADeutschCollection^.AtDelete(DItem);
    
    DoDecStelle := DItem;

    AEDTransCollection^.ForEach(@DoTDec)

  End
  Else Dec(PVokabel(ADeutschCollection^.At(DItem))^.Vokabel^.Anzahl)
End;


Procedure SearchDETranslate;
Var A    : Integer;
Begin
  DETranslate := - 1;

  For A := 0 to PVokabel(ADeutschCollection^.At(DItem))^.
                         Vokabel^.Anzahl - 1 do
  Begin
    If PTrans(PCollection(ADETransCollection^.At(PVokabel(
                          ADeutschCollection^.At(DItem))^.
                          Vokabel^.ZL + A)))^.Translate = EItem then
    Begin
      DETranslate := PVokabel(ADeutschCollection^.At(DItem))^.
                     Vokabel^.ZL + A
    End
  End
End;

Procedure SearchEDTranslate;
Var A    : Integer;
Begin
  EDTranslate := - 1;

  For A := 0 to PVokabel(AEnglischCollection^.At(EItem))^.
                         Vokabel^.Anzahl - 1 do
  Begin
    If PTrans(PCollection(AEDTransCollection^.At(PVokabel(
                          AEnglischCollection^.At(EItem))^.
                          Vokabel^.ZL + A)))^.Translate = DItem then
    Begin
      EDTranslate := PVokabel(AEnglischCollection^.At(EItem))^.
                     Vokabel^.ZL + A
    End
  End
End;

(*******************************)

Begin
  TDialog.HandleEvent(Event);

  ADeutschListBox^.HandleEvent(Event);
  AEnglischListBox^.HandleEvent(Event);
  ATranslateListBox^.HandleEvent(Event);

  If (Event.What = evCommand) and
     (Event.Command = cmVVV) then
  Begin
    AEnglischEingabe^.GetData(Vokabel1.Wort);
    ADeutschEingabe^.GetData(Vokabel2.Wort);

    EVokabel := New(PVokabel, Init(Vokabel1));
    DVokabel := New(PVokabel, Init(Vokabel2));

    If AEnglischCollection^.Search(EVokabel, EItem) then
    Begin
      AEnglischListBox^.FocusItem(EItem)
    End;
    If ADeutschCollection^.Search(DVokabel, DItem) then
    Begin
      ADeutschListBox^.FocusItem(DItem)
    End;
    ClearEvent(Event);

    Dispose(EVokabel, Done);
    Dispose(DVokabel, Done);

    ReDraw
  End;


  If (Event.What = evCommand) and
     (Event.Command = cmVVEEntfernen) then
  Begin

    EditEItem := AEnglischListBox^.Focused;
    EditDItem := ADeutschListBox^.Focused;

    AEnglischEingabe^.
      SetData(PVokabel(
              AEnglischCollection^.At(
              EditEItem))^.
              Vokabel^.Wort);

    ADeutschEingabe^.
      SetData(PVokabel(
              ADeutschCollection^.At(
              EditDItem))^.
              Vokabel^.Wort);

    Redraw;

    AEnglischEingabe^.GetData(Vokabel1.Wort);
    ADeutschEingabe^.GetData(Vokabel2.Wort);

    If (Vokabel1.Wort <> '') and
       (Vokabel2.Wort <> '') then
    Begin

      EVokabel := New(PVokabel, Init(Vokabel1));
      EVokabel^.Vokabel^.ED := True;
      EVokabel^.Vokabel^.Anzahl := 1;
      EVokabel^.SetDatum;

      DVokabel := New(PVokabel, Init(Vokabel2));
      DVokabel^.Vokabel^.ED := False;
      DVokabel^.Vokabel^.Anzahl := 1;
      DVokabel^.SetDatum;



      ETrue := AEnglischCollection^.Search(EVokabel, EItem);
      DTrue := ADeutschCollection^.Search(DVokabel, DItem);

      If ETrue and DTrue then
      Begin

        (* Folgende beiden Routinen suchen die Elemente in den Translate-  *)
        (* listen, die jeweils auf das andere Wort zeigen. Damit ein       *)
        (* angegebenes Wortpaar gelîscht werden kann, mu· eine öbersetzung *)
        (* in beide Richtungen vorhanden sein!                             *)
        (*                                                                 *)
        (* Liefern EDTranslate und DETranslate zurÅck.                     *)


        SearchDETranslate;
        SearchEDTranslate;


        If (EDTranslate > - 1) and
           (DETranslate > - 1) then
        Begin
          Save := True;

          (* Lîsche Vokabelpaar: *)

          DelETrans;
          DelDTrans;

          DelEWort;

          DelDWort;


          ADeutschListBox^.SetRange(ADeutschListBox^.List^.Count);
          AEnglischListBox^.SetRange(AEnglischListBox^.List^.Count);

          ATranslateCollection^.DeleteAll;
          ATranslateCollection^.Count := 0;

          ATranslateListBox^.SetRange(0);

          ReDraw
        End
        Else
	  MessageBox('FÅr dieses Wortpaar besteht keine öbersetzung!' + Chr(13) +
               'WÑhlen Sie in den Listboxen ein Vokabelpaar aus.' + Chr(13) +
               'Versuchen Sie es dann nochmal.', nil, mfOkButton)
      End
      Else 	  MessageBox('Fehler in der Datenstrucktur!', nil, mfOkButton);

      Dispose(EVokabel, Done);
      Dispose(DVokabel, Done)


    End
    Else
      Wait('Sie haben kein Wortpaar angegeben!');

    ClearEvent(Event)
  End;


  If (Event.What = evCommand) and
     (Event.Command = cmVVESpeichern) then
  Begin
    Save := True;
    AEnglischEingabe^.GetData(Vokabel1.Wort);
    ADeutschEingabe^.GetData(Vokabel2.Wort);

    If (Vokabel1.Wort <> '') and
       (Vokabel2.Wort <> '') then
    Begin
      EVokabel := New(PVokabel, Init(Vokabel1));
      EVokabel^.Vokabel^.ED := True;
      EVokabel^.Vokabel^.Anzahl := 1;
      EVokabel^.SetDatum;

      DVokabel := New(PVokabel, Init(Vokabel2));
      DVokabel^.Vokabel^.ED := False;
      DVokabel^.Vokabel^.Anzahl := 1;
      DVokabel^.SetDatum;

      ETrue := AEnglischCollection^.Search(EVokabel, EItem);
      DTrue := ADeutschCollection^.Search(DVokabel, DItem);

      EDTranslate := - 1;
      DETranslate := - 1;

      If DTrue then SearchDETranslate;
      If ETrue then SearchEDTranslate;


      If (EDTranslate > - 1) and
         (DETranslate > - 1) then
        Wait('öbersetzung ist schon vorhanden!')
      Else
      Begin
        If DTrue then DETranslate :=
          PVokabel(ADeutschCollection^.At(DItem))^.Vokabel^.ZL;

        If ETrue then EDTranslate :=
          PVokabel(AEnglischCollection^.At(EItem))^.Vokabel^.ZL;


        If DTrue = False then EDTB Else DCB;

        If ETrue = False then DETB Else ECB;

        If ETrue then ENE Else EE;

        If DTrue then DNE Else DE;

        ReDraw
      End
    End;

    ClearEvent(Event);
    AEnglischEingabe^.Select
  End;

  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) and
     (Event.InfoPtr = AEnglischListBox) then

  Begin
    ATranslateCollection^.DeleteAll;
    ATranslateCollection^.Count := 0;

    For A:= 0 to PVokabel(AEnglischCollection^.At(AEnglischListBox^.
                                                  Focused))^.
                                                  Vokabel^.Anzahl - 1
    do
    Begin
      ATranslateCollection^.Insert(ADeutschCollection^.At(
                                   PTrans(
                                   AEDTransCollection^.At(
                                   PVokabel(
                                   AEnglischCollection^.At(
                                   AEnglischListBox^.Focused))^.
                                   Vokabel^.ZL + A))^.Translate));

      If PVokabel(ADeutschCollection^.At(
                  PTrans(
                  AEDTransCollection^.At(
                  PVokabel(
                  AEnglischCollection^.At(
                  AEnglischListBox^.Focused))^.
                  Vokabel^.ZL + A))^.Translate))^.Vokabel^.ED = True then
        Wait('Fehler in der Datenstrucktur!')
    End;



    ATranslateListBox^.SetRange(ATranslateCollection^.Count);
    ATranslateListBox^.Options := ATranslateListBox^.Options or ofSelectable;
    ReDraw;
    ClearEvent(Event);
  End;



  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) and
     (Event.InfoPtr = ADeutschListBox) then
  Begin
    ATranslateCollection^.DeleteAll;
    ATranslateCollection^.Count := 0;

    For A:= 0 to PVokabel(ADeutschCollection^.At(ADeutschListBox^.
                                                  Focused))^.
                                                  Vokabel^.Anzahl - 1
    do
    Begin
      ATranslateCollection^.Insert(AEnglischCollection^.At(
                                   PTrans(
                                   ADETransCollection^.At(
                                   PVokabel(
                                   ADeutschCollection^.At(
                                   ADeutschListBox^.Focused))^.
                                   Vokabel^.ZL + A))^.Translate));

      If PVokabel(AEnglischCollection^.At(
                  PTrans(
                  ADETransCollection^.At(
                  PVokabel(
                  ADeutschCollection^.At(
                  ADeutschListBox^.Focused))^.
                  Vokabel^.ZL + A))^.Translate))^.Vokabel^.ED = False then
        Wait('Fehler in der Datenstrucktur!')
    End;



    ATranslateListBox^.SetRange(ATranslateCollection^.Count);
    ATranslateListBox^.Options := ATranslateListBox^.Options or ofSelectable;
    ReDraw;
    ClearEvent(Event);
  End;

  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) and
     (Event.InfoPtr = ATranslateListBox) then
  Begin
    Case PVokabel(ATranslateCollection^.At(
                  ATranslateListBox^.Focused))^.Vokabel^.ED of

      True  : Begin
                ETrue := AEnglischCollection^.Search(
                         PVokabel(ATranslateCollection^.At(
                                  ATranslateListBox^.Focused)),
                                  EItem);

                If ETrue then
                Begin
                  AEnglischListBox^.FocusItem(EItem);
                  AEnglischEingabe^.SetData(PVokabel(
                                            AEnglischCollection^.At(EItem))^.
                                            Vokabel^.Wort);
                End
                Else
                  Wait('Fehler')
              End;
      False : Begin
                DTrue := ADeutschCollection^.Search(
                         PVokabel(ATranslateCollection^.At(
                                  ATranslateListBox^.Focused)),
                                  DItem);

                If DTrue then
                Begin
                  ADeutschListBox^.FocusItem(DItem);
                  ADeutschEingabe^.SetData(PVokabel(
                                           ADeutschCollection^.At(DItem))^.
                                           Vokabel^.Wort);
                End
                Else
                  Wait('Fehler')
              End;
    End
  End



End;


PROCEDURE VokabelneditierenDialog;
  VAR
    R: tRect;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pVokabelneditierenDialog, Init (R, 'Vokabeln editieren'));
 
    { Datenrecord initialisieren ! }
  Desktop^.ExecView (Application^.ValidView (Dialog));

  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;
Begin
  Save := False
End.