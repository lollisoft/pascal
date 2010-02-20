Unit DEDIT;
{$O+ $F+}
Interface

Uses Crt,
     App,
     Objects,
     TvdvCmds,
     Drivers,
     Memory,
     Views,
     Dialogs,
{     Editors,  }
     Video,
     Filefind,
     StrTools;

TYPE

  pRadioButton          = ^tRadioButton;
  tRadioButton          = Object(tRadioButtons)
    Procedure Press(Item:Integer); Virtual;
  End;

  PWindowListBox        = ^TWindowListBox;
  TWindowListBox        = Object(TListBox)
    Anfang              : Longint;
    StrucktSeek         : Longint;
    Constructor Init(var Bounds: TRect;
                      ANumCols: Word;
                    AScrollBar: PScrollBar);

    Procedure FocusItem(Item: Integer); Virtual;
    Procedure HandleEvent(Var Event: TEvent); Virtual;
  End;

  pDateneditorDialog    = ^tDateneditorDialog;
  tDateneditorDialog    = OBJECT(tDialog)

    (* FÅr Dateien oder Labels anzeigen: *)

    LabelString                : String12;
    Showtyp,
    PlatteSpeichertyp          : Integer;

    ADLRadioButton,
    ASucheRadioButton,
    APlatteSpeicherRadioButton : PRadioButtons;

    ALabelDateiCollection      : PLabelCollection;
    APfadCollection            : PPfadCollection;

    ALabelDateiScrollBar       : PScrollBar;
    APfadScrollBar             : PScrollBar;

    ALabelDateiListBox         : PWindowListBox;
    APfadListBox               : PWindowListBox;

    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Destructor Done; Virtual;

    Procedure InitLabelData;
    Procedure InitPfadData;
    Procedure InitDateiData;

    Procedure InitPlattenLabelData;
    Procedure InitPlattenPfadData;
    Procedure InitPlattenDateiData(SeekPos: Longint);

    Procedure InitDataAgain;
    Procedure InitData;

    Function GetPalette: PPalette; Virtual;
    Procedure LoescheDiskbereich(Var DiskName: String12);
    Procedure HandleEvent (Var Event: TEvent); Virtual;
  END;

Var ADateneditorDialog : pDateneditorDialog;
    EditAllPlatte,
    EditAllLabelDatei,
    EditAllPfad        : Boolean;
    LabelDateiSeek     : Longint;
    PlatteSpeicherData : Word;

    IsLabelSelected,
    IsPfadSelected,
    IsDateiSelected    : Boolean;



    PfadFocus,
    LabelDateiFocus    : Integer;

    DateiCount,
    PfadCount,
    LabelCount         : Integer;

    DateiSeek,
    PfadSeek,
    LabelSeek: Longint;


Procedure InitDEdit;



PROCEDURE DateneditorDialog;
Procedure Ton(hz, Time: Integer);

Implementation

Procedure Ton(Hz, Time: Integer);
Begin
  Sound(Hz);
  Delay(Time);
  NoSound
End;


Constructor TWindowListBox.Init(var Bounds: TRect;
                                  ANumCols: Word;
                                AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar);
  Anfang := 0
End;

Procedure TWindowListBox.HandleEvent(Var Event: TEvent);
Begin
  If (Event.What = evBroadCast) and
     (Event.Command = cmScrollBarChanged) then
  Begin
    FocusItem(VScrollBar^.Value)
  End;

  TListBox.HandleEvent(Event)
End;

Procedure TWindowListBox.FocusItem(Item: Integer);
Begin
  If Focused = Item then
  Begin
    TListBox.FocusItem(Item);
    Message(TopView, evBroadCast, cmWindowListItemSelected, @Self);
  End
  Else
    TListBox.FocusItem(Item)
End;

CONSTRUCTOR tDateneditorDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  EditAllPlatte := True;
  EditAllPfad   := True;
  EditAllLabelDatei := True;


  tDialog.Init (Bounds, aTitle);


  (************************************)
  (*                                  *)
  (*   Labellistbox initialisieren:   *)
  (*                                  *)
  (************************************)

  Showtyp := 0;

  LabelDateiFocus := 0;



  (* LabelDatei Rollbalken initialisieren: *)

  R.Assign (18, 3, 19, 8);
  ALabelDateiScrollBar := New (pScrollBar, Init  (R));
  ALabelDateiScrollBar^.SetRange (0, 0);

  (* LabelDateiListBox initialisieren: *)

  R.Assign (4, 3, 18, 8);
  ALabelDateiListBox := New (pWindowListBox, Init (R, 1, ALabelDateiScrollBar));

  (* und an pLabel ( Daten ) HÑngen: *)

  R.Assign (3, 2, 11, 3);
  Insert (New (pLabel, Init (R, '~D~aten', ALabelDateiListBox)));

  (***********************************)
  (*                                 *)
  (*   Pfadlistbox initialisieren:   *)
  (*                                 *)
  (***********************************)

  PfadFocus := 0;


  (* Pfad Rollbalken initialisieren: *)

  R.Assign (75, 14, 76, 21);
  APfadScrollBar := New (pScrollBar, Init  (R));
  APfadScrollBar^.SetRange (0, 0);

  (* und an PfadListBox hÑngen: *)

  R.Assign (4, 14, 75, 21);
  APfadListBox := New (pWindowListBox, Init (R, 1, APfadScrollBar));

  R.Assign (3, 13, 10, 14);
  Insert (New (pLabel, Init (R, '~P~fad', APfadListBox)));


  (********************************************)
  (* Eingabezeile und History initialisieren: *)
  (********************************************)

  R.Assign (21, 6, 46, 7);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (20, 5, 33, 6);
  Insert (New (pLabel, Init (R, '~Z~u suchen:', View)));
  R.Assign (45, 6, 48, 7);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));

  (********************************************************)
  (* RadioButtons fÅr Platte/Speicher initialisieren      *)
  (********************************************************)

  R.Assign (51, 10, 70, 12);
  APlatteSpeicherRadioButton := New (pRadioButtons,
               Init (R,
                     NewSItem ('Speicherdaten',
                     NewSItem ('Plattendaten',
                     NIL))));
  Insert (APlatteSpeicherRadioButton);
  R.Assign (51, 9, 60, 10);
  Insert (New (pLabel, Init (R, '~E~ditiere', APlatteSpeicherRadioButton)));

  (********************************************************)
  (* RadioButtons fÅr Suchargument initialisieren         *)
  (********************************************************)

  R.Assign (51, 3, 76, 6);
  ASucheRadioButton := New (pRadioButtons,
               Init (R,
                     NewSItem ('nach Dateien',
                     NewSItem ('nach Disk''s',
                     NewSItem ('nach Verzeichnissen',
                     NIL)))));
  Insert (ASucheRadioButton);
  R.Assign (51, 2, 59, 3);
  Insert (New (pLabel, Init (R, 'S~u~che', ASucheRadioButton)));

  (**************************************************)
  (* RadioButtons fÅr Anzeigemodus initialisieren:  *)
  (**************************************************)
  R.Assign (4, 10, 15, 12);
  ADLRadioButton := New (pRadioButtons,
               Init (R,
                     NewSItem ('Label',
                     NewSItem ('Datei',
                     NIL))));

  R.Assign (4, 9, 14, 10);
  Insert (New (pLabel, Init (R, 'A~n~zeige', ADLRadioButton)));

  Insert (ADLRadioButton);

  (********************************)
  (* Schaltknîpfe initialisieren: *)
  (********************************)


  R.Assign (20, 9, 35, 11);
  Insert (New (pButton, Init (R, '~V~erÑndern', cmDEDITBemEdit, 0)));

  R.Assign (36, 9, 49, 11);
  Insert (New (pButton, Init (R, '~A~bbruch', cmCancel, 0)));

  R.Assign (20, 11, 35, 13);
  Insert (New (pButton, Init (R, '~S~peichern', cmDEditSpeichern, 0)));

  R.Assign (20, 3, 49, 5);
  Insert (New (pButton, Init (R, '  Diskbereich ~l~îschen  ', cmDEDITDiskBereichLoeschen, 0)));

  R.Assign (36, 11, 48, 13);
  Insert (New (pButton, Init (R, 'Su~c~hen', cmDEditSuchen, 0)));



  (********************************************)
  (*                                          *)
  (*   Initialisieren der Collectionen und    *)
  (*       zuwei·en in die Listboxen.         *)
  (*                                          *)
  (********************************************)



  ALabelDateiCollection := New(PLabelCollection, Init(1, 1));

  ALabelDateiListBox^.Options := ALabelDateiListBox^.Options And Not ofSelectable;

  ALabelDateiListBox^.NewList(ALabelDateiCollection);

  ALabelDateiListBox^.SetRange(0);

  ALabelDateiListBox^.EventMask := FocusedEvents + PositionalEvents;


  APfadCollection := New(PPfadCollection, Init(1, 1));

  APfadListBox^.Options := APfadListBox^.Options And Not ofSelectable;

  APfadListBox^.NewList(APfadCollection);

  APfadListBox^.SetRange(0);

  APfadListBox^.EventMask := FocusedEvents + PositionalEvents;


  Insert(ALabelDateiScrollBar);
  Insert(APfadScrollBar);
  Insert(ALabelDateiListBox);
  Insert(APfadListBox);

  SelectNext (FALSE);

  InitData;
END;


Destructor tDateneditorDialog.Done;
Begin
  If APlatteSpeicherRadioButton^.Value = 1 then
  Begin
    ALabelDateiCollection^.FreeAll;
    APfadCollection^.FreeAll;
    ALabelDateiCollection^.Count := 0;
    APfadCollection^.Count := 0
  End
  Else
  Begin
    ALabelDateiCollection^.DeleteAll;
    APfadCollection^.DeleteAll
  End;
  Dispose(ALabelDateiCollection, Done);
  Dispose(APfadCollection, Done);
  Dispose(ALabelDateiListBox, Done);
  Dispose(APfadListBox, Done);
  Dispose(ADLRadioButton, Done);
  Dispose(ASucheRadioButton, Done);
  Dispose(APlatteSpeicherRadioButton, Done);
  Dispose(ALabelDateiScrollBar, Done);
  Dispose(APfadScrollBar, Done);

  TDialog.Done
End;

Procedure tDateneditorDialog.InitDateiData;
Var Lauf: StrucktListPtrtyp;
Begin
  Lauf := StrucktList;
  While Lauf <> Nil do
  Begin
    Case Lauf^.StrucktData.Art of
      3 : Begin
            PLabelCollection(ALabelDateiListBox^.List)^.
              Insert(@Lauf^.StrucktData.DateiList^.
              DateiData.DateiName);
            ALabelDateiListBox^.SetRange(ALabelDateiListBox^.List^.Count)
          End;
    End;
    Lauf := Lauf^.Next
  End;
  ALabelDateiListBox^.Options := ALabelDateiListBox^.Options or ofSelectable;
  ALabelDateiListBox^.FocusItem(0);
  ALabelDateiListBox^.TopItem := 0;
End;

Procedure tDateneditorDialog.InitPfadData;
Var Lauf: StrucktListPtrtyp;
Begin
  Lauf := StrucktList;
  While Lauf <> Nil do
  Begin
    Case Lauf^.StrucktData.Art of
      2 : Begin
            PPfadCollection(APfadListBox^.List)^.
              Insert(@Lauf^.StrucktData.PfadList^.
              PfadData.PfadName);
            APfadListBox^.SetRange(APfadListBox^.List^.Count)
          End;
    End;
    Lauf := Lauf^.Next
  End;
  APfadListBox^.Options := APfadListBox^.Options or ofSelectable;
  APfadListBox^.FocusItem(0);
  APfadListBox^.TopItem := 0;
End;


Procedure tDateneditorDialog.InitLabelData;
Var Lauf: StrucktListPtrtyp;
Begin
  Lauf := StrucktList;
  While Lauf <> Nil do
  Begin
    Case Lauf^.StrucktData.Art of
      1 : Begin
            PLabelCollection(ALabelDateiListBox^.List)^.
              Insert(@Lauf^.StrucktData.LabelList^.
              LabelData.LabelName);
            ALabelDateiListBox^.SetRange(ALabelDateiListBox^.List^.Count)
          End;
    End;
    Lauf := Lauf^.Next
  End;

  ALabelDateiListBox^.Options := ALabelDateiListBox^.Options or ofSelectable;
  ALabelDateiListBox^.FocusItem(0);
  ALabelDateiListBox^.TopItem := 0;
End;



(**********************************************************)
(* Daten von Platte laden:                                *)
(**********************************************************)


(**********)
(* Datei: *)
(**********)

Procedure tDateneditorDialog.InitPlattenDateiData(SeekPos: Longint);
Var PfadData: PfadDatatyp;
    DateiData: DateiDatatyp;
    P        : Pointer;

Begin
  Seek(DateiDataFile, SeekPos);

  While Not Eof(DateiDataFile) do
  Begin
    Read(DateiDataFile, DateiData);

    P := MemAlloc(SizeOf(DateiData.DateiName));

    If P = Nil then
    Begin
      EditAllPlatte := False;
      EditAllLabelDatei := False;

      With ALabelDateiListBox^ do
      Begin
        Options := Options or ofSelectable;
        Focused := 0;
        TopItem := 0;
        If Range > DateiCount then
          DateiCount := Range;
        Draw
      End;

      Exit
    End;

    String(P^) := DateiData.DateiName;

    PLabelCollection(ALabelDateiListBox^.List)^.
      Insert(P);

    ALabelDateiListBox^.SetRange(ALabelDateiListBox^.List^.Count)
  End;

  With ALabelDateiListBox^ do
  Begin
    Options := Options or ofSelectable;
    Focused := 0;
    TopItem := 0;
    If Range > DateiCount then
      DateiCount := Range;
    Draw
  End;
End;

(**********)
(* Pfad:  *)
(**********)

Procedure tDateneditorDialog.InitPlattenPfadData;
Var LabelData: LabelDatatyp;
    PfadData : PfadDatatyp;
    P        : Pointer;
Begin
  While Not Eof(StrucktDataFile) do
  Begin
    Read(StrucktDataFile, StrucktData);
    Case StrucktData.Art of
      5 : Begin
            Seek(PfadDataFile, StrucktData.DPfadList - 1);
            Read(PfadDataFile, PfadData);
            P := MemAlloc(SizeOf(PfadData.PfadName));

            If P = Nil then
            Begin
              EditAllPlatte := False;
              EditAllPfad := False;

              With APfadListBox^ do
              Begin
                Options := APfadListBox^.Options or ofSelectable;
                FocusItem(0);
                TopItem := 0;
                If Range > PfadCount then
                  PfadCount := Range;
                StrucktSeek := FilePos(StrucktDataFile)
              End;

              Exit
            End;

            String(P^) := PfadData.PfadName;

            PPfadCollection(APfadListBox^.List)^.
              Insert(P);

            APfadListBox^.SetRange(APfadListBox^.List^.Count)
          End;
    End
  End;

  With APfadListBox^ do
  Begin
    Options := APfadListBox^.Options or ofSelectable;
    FocusItem(0);
    TopItem := 0;
    If Range > PfadCount then
      PfadCount := Range;
    StrucktSeek := FilePos(StrucktDataFile)
  End;
End;

(**********)
(* Label: *)
(**********)

Procedure tDateneditorDialog.InitPlattenLabelData;
Var LabelData: LabelDatatyp;
    PfadData : PfadDatatyp;
    P        : Pointer;
Begin
  While Not Eof(StrucktDataFile) do
  Begin
    Read(StrucktDataFile, StrucktData);
    Case StrucktData.Art of
      4 : Begin
            Seek(LabelDataFile, StrucktData.DLabelList - 1);
            Read(LabelDataFile, LabelData);
            P := MemAlloc(SizeOf(LabelData.LabelName));

            If P = Nil then
            Begin
              EditAllPlatte := False;
              EditAllLabelDatei := False;

              With ALabelDateiListBox^ do
              Begin
                Options := ALabelDateiListBox^.Options or ofSelectable;
                FocusItem(0);
                TopItem := 0;
                If Range > LabelCount then
                  LabelCount := Range;
                StrucktSeek := FilePos(StrucktDataFile)
              End;

              Exit
            End;

            String(P^) := LabelData.LabelName;

            PLabelCollection(ALabelDateiListBox^.List)^.
              Insert(P);

            ALabelDateiListBox^.SetRange(ALabelDateiListBox^.List^.Count)
          End;
    End
  End;

  With ALabelDateiListBox^ do
  Begin
    Options := ALabelDateiListBox^.Options or ofSelectable;
    FocusItem(0);
    TopItem := 0;
    If Range > LabelCount then
      LabelCount := Range;
    StrucktSeek := FilePos(StrucktDataFile)
  End
End;

(*****************)
(* Init-Routinen *)
(*****************)


Procedure tDateneditorDialog.InitData;

Begin
  If StrucktList = Nil then
  Begin
    PlatteSpeicherData := 1;
    APlatteSpeicherRadioButton^.SetData(PlatteSpeicherData);
    APlatteSpeicherRadioButton^.GetData(PlatteSpeichertyp)
  End;
  If APlatteSpeicherRadioButton^.Value = 0 then
    Case ADLRadioButton^.Value of
      0 : Begin
            InitLabelData;
            InitPfadData
          End;
      1 : Begin
            InitDateiData;
            InitPfadData
          End
    End
  Else
  Begin
    Case ADLRadioButton^.Value of
      0 : Begin
            InitPlattenLabelData;
            Seek(StrucktDataFile, 0);
            InitPlattenPfadData
          End;
      1 : Begin
            InitPlattenDateiData(0);
            Seek(StrucktDataFile, 0);
            InitPlattenPfadData
          End
    End;
  End;
  Draw
End;



Procedure tDateneditorDialog.InitDataAgain;

Begin
  If StrucktList = Nil then
  Begin
    PlatteSpeicherData := 1;
    APlatteSpeicherRadioButton^.SetData(PlatteSpeicherData);
    APlatteSpeicherRadioButton^.GetData(PlatteSpeichertyp)
  End;
  If APlatteSpeicherRadioButton^.Value = 0 then
    Case ADLRadioButton^.Value of
      0 : InitLabelData;
      1 : InitDateiData
    End
  Else
  Begin
    Case ADLRadioButton^.Value of
      0 : InitPlattenLabelData;
      1 : InitPlattenDateiData(0)
    End;
  End;
  Draw
End;



Procedure tRadioButton.Press(Item: Integer);
Begin
  tRadioButtons.Press(Item);
  Message(TopView, evBroadCast, cmPressRadioButton, @Item)
End;




Function tDateneditorDialog.GetPalette: PPalette;
Const TempPal: String[Length(CDialog)] = CDialog;
Begin
  GetPalette := @TempPal
End;



Procedure tDateneditorDialog.LoescheDiskbereich(Var DiskName: String12);
Var Lauf    : StrucktListPtrtyp;
    SZahl   : String;
    HilfLauf: HilfListPtrtyp;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    VorDatei: String;

Procedure LoescheElement(Var Anfang,
                             Ende,
                             Element          : ListenPtrtyp);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure LoescheStrucktElement(var Anfang,
                                    Ende,
                                    Element          : StrucktListPtrtyp);
var Hilf : StrucktListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Struckt - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheHilfElement(var Anfang,
                                 Ende,
                                 Element          : HilfListPtrtyp);
var Hilf : HilfListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Hilf - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheZusatzElement(var Anfang,
                                   Ende,
                                   Element    : ZusatzListPtrtyp);
var Hilf: ZusatzListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Zusatz - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheLabel(var l: String12);
begin
  l := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
  Dec(Lauf^.StrucktData.LabelList^.LabelData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoeschePfad(var p: String);
begin
  p := Lauf^.StrucktData.PfadList^.PfadData.PfadName;
  Dec(Lauf^.StrucktData.PfadList^.PfadData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoescheDatei(zl, zp : String);
var Zaehler : Byte;
    Help,
    LocHelp : ZusatzListPtrtyp;
    Anz     : String;
begin

  Zaehler := 1;
  If Lauf = Nil then
  Begin
    Wait('Lauf ist Nil, Dateielement kann nicht gelîscht werden!');
    Halt
  End;

  If Lauf^.StrucktData.DateiList = Nil then
  Begin
    Wait('Lauf^.StrucktData.DateiList = Nil!');
    Halt
  End;

  Help := Lauf^.StrucktData.DateiList^.DateiData.Loc;

  LocHelp := Help;

  If Help = Nil then
  Begin
    Wait('Help = Nil!' + Chr(13) +
         'DateiData.Anzahl: ' +
          GetString(Lauf^.StrucktData.DateiList^.DateiData.Anzahl) + Chr(13) +
         'Datei: ' +
          Lauf^.StrucktData.DateiList^.DateiData.DateiName + Chr(13) +
         'Datei, die vorher gelîscht wurde: ' + VorDatei);
    Halt
  End;


  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          LabelList^.LabelData.LabelName) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          PfadList^.PfadData.PfadName)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler);
    If Zaehler > Lauf^.StrucktData.DateiList^.DateiData.Anzahl then
    Begin
      Wait('Zusatz - Element nicht gefunden!');
      Halt
    End
  end;

  If Lauf^.StrucktData.DateiList^.DateiData.Anzahl = 1 then
  begin
    If Help <> LocHelp Then
    Begin
      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )');
      Halt
    End;

    Lauf^.StrucktData.DateiList^.DateiData.Loc := Nil;

  end
  else
    If Help = LocHelp then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)
      If LocHelp^.Next = Nil then
      Begin
        Clrscr;
        Writeln('DateiData.Loc ist letztes Element, weiterrÅcken nicht mîglich!');
        Writeln('Anzahl: ', Lauf^.StrucktData.DateiList^.DateiData.Anzahl);
        Writeln('Datei : ', Lauf^.StrucktData.DateiList^.DateiData.DateiName);
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp^.Next;
    end
    Else
    Begin
      If Help = Nil then
      Begin
        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )');
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp;
    End;


  LoescheZusatzElement(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.DateiList^.DateiData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

end;



begin
  CopyWorkinM(2);
  HilfLauf := HilfList;

  While (DiskName <> HilfLauf^.HilfData.LabelList^.
                     StrucktData.LabelList^.LabelData.LabelName) and
        (HilfLauf <> Nil) do
    HilfLauf := HilfLauf^.Next;

  If HilfLauf = Nil then
  Begin
    Wait('Label nicht in Daten!!!');
    Exit
  End;

  Lauf := HilfLauf^.HilfData.LabelList;

  A := 0;
(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)



  (* Laufe die Strucktliste durch und lîsche die Daten: *)


  repeat
    Inc(A);

    Str(A, SZahl);
    case Lauf^.StrucktData.art of
      1 : LoescheLabel(l);
      2 : LoeschePfad(p);
      3 : Begin
            VorDatei := Lauf^.StrucktData.DateiList^.DateiData.DateiName;
            LoescheDatei(l, p)
          End
    Else
      Wait('Falsches Element in Strucktliste!');
    end


  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
  CopyMinWork(2);
  DiskName := ''
end;




Procedure tDateneditorDialog.HandleEvent(Var Event: TEvent);

Procedure AdjustLabelDateiWindow(Richtung: Shortint);

Procedure AdjustLabel;
Begin
End;

Procedure AdjustDatei;
Begin
End;

Begin
  Case Richtung of
    1  : Case ADLRadioButton^.Value of
           0 : AdjustLabel;
           1 : AdjustDatei
         End;
    -1 : Case ADLRadioButton^.Value of
           0 : AdjustLabel;
           1 : AdjustDatei
         End
  End
End;


Procedure ResetLabelDateiListBox;
Begin
  If APlatteSpeicherRadioButton^.Value = 0 then
    ALabelDateiCollection^.DeleteAll
  Else
    ALabelDateiCollection^.FreeAll;

  ALabelDateiCollection^.Done;
  ALabelDateiCollection^.Init(1, 1);

  ALabelDateiListBox^.Options := ALabelDateiListBox^.Options
                                 And Not ofSelectable;
  ALabelDateiListBox^.TopItem := 0;
  ALabelDateiListBox^.Focused := - 1;
  ALabelDateiListBox^.SetRange(0);
  ALabelDateiListBox^.Draw;
  ALabelDateiCollection^.Count := 0;
End;

Procedure ResetPfadListBox;
Begin
  If APlatteSpeicherRadioButton^.Value = 0 then
    APfadCollection^.DeleteAll
  Else
    APfadCollection^.FreeAll;

  APfadCollection^.Done;
  APfadCollection^.Init(1, 1);



  APfadListBox^.Options := APfadListBox^.Options
                           And Not ofSelectable;
  APfadListBox^.TopItem := 0;
  APfadListBox^.SetRange(0);
  APfadCollection^.Count := 0;
End;


Procedure LabelSelected;
Begin
  If IsLabelSelected then
  (* PfadListBox entleeren und Uhrsprungszustand wieder herstellen. *)
  Begin
    IsLabelSelected := False
  End
  Else
  (* PfadListBox entleeren und Pfade eintragen, die zu diesem Label geh. *)
  Begin
    Ton(2000, 500);
    IsLabelSelected := True
  End
End;

Procedure DateiSelected;
Begin
  If IsDateiSelected then
  (* LabelDateiListBox wieder mit Dateinamen fÅllen. *)
  Begin
    IsDateiSelected := False
  End
  Else
  (* Pfad auf den richtigen Eintrag setzen und in LabelDateiListBox den *)
  (* passenden Label anzeigen.                                          *)
  Begin
    Ton(1500, 500);
    IsDateiSelected := True
  End
End;

Procedure PfadSelected;
Begin
  If IsPfadSelected then
  (* LabelDateiListBox entleeren und wieder mit alten EintrÑgen fÅllen. *)
  Begin
    IsPfadSelected := False
  End
  Else
  (* LabelDateiListBox mit Dateinamen fÅllen, die zu dem Pfad gehîren. *)
  Begin
    Ton(1000, 500);
    IsPfadSelected := True
  End
End;




Var Mem: Word;
    SeekDif: Longint;

Begin

  (* Soll Datenfenster andere Daten anzeigen? *)

  If Not(ADLRadioButton^.Mark(Showtyp)) then
    Begin
      If APlatteSpeicherRadioButton^.Value = 0 then
        ALabelDateiCollection^.DeleteAll
      Else
        ALabelDateiCollection^.FreeAll;

      ALabelDateiCollection^.Done;
      ALabelDateiCollection^.Init(1, 1);

      ADLRadioButton^.GetData(Showtyp);

      ALabelDateiListBox^.Options := ALabelDateiListBox^.Options
                                     And Not ofSelectable;
      ALabelDateiListBox^.TopItem := 0;
      ALabelDateiListBox^.SetRange(0);
      ALabelDateiCollection^.Count := 0;


      EditAllLabelDatei := True;

      EditAllPlatte := EditAllLabelDatei = EditAllPfad;

      LabelDateiFocus := - 1;
      Seek(StrucktDataFile, 0);
      InitDataAgain;
      ALabelDateiListBox^.Select;
      Redraw;
      LabelDateiFocus := 0
    End;


  If Not(APlatteSpeicherRadioButton^.Mark(PlatteSpeichertyp)) then
    If (PlatteSpeichertyp = 1) and
       (StrucktList = Nil) then
    Begin
      APlatteSpeicherRadioButton^.SetData(PlatteSpeichertyp);
      APlatteSpeicherRadioButton^.Draw;
    End

    Else
    Begin

      If PlatteSpeichertyp = 0 then
      Begin
        APfadCollection^.DeleteAll;
        ALabelDateiCollection^.DeleteAll
      End
      Else
      Begin
        APfadCollection^.FreeAll;
        ALabelDateiCollection^.FreeAll
      End;

      ALabelDateiCollection^.Done;
      ALabelDateiCollection^.Init(1, 1);

      APfadCollection^.Done;
      APfadCollection^.Init(1, 1);


      APlatteSpeicherRadioButton^.GetData(PlatteSpeichertyp);

      ALabelDateiListBox^.Options := ALabelDateiListBox^.Options
                                     And Not ofSelectable;
      ALabelDateiListBox^.TopItem := 0;
      ALabelDateiListBox^.SetRange(0);
      ALabelDateiCollection^.Count := 0;

      APfadListBox^.Options := APfadListBox^.Options
                                     And Not ofSelectable;
      APfadListBox^.TopItem := 0;
      APfadListBox^.SetRange(0);
      APfadCollection^.Count := 0;

      EditAllPlatte := True;
      EditAllPfad   := True;
      EditAllLabelDatei := True;


      PfadFocus := - 1;
      LabelDateiFocus := - 1;
      Seek(StrucktDataFile, 0);
      InitData;
      Redraw;
      PfadFocus := 0;
      LabelDateiFocus := 0;
    End;

  tDialog.HandleEvent(Event);


  If Event.What = evCommand then
  Case Event.Command of
    cmDEDITBemEdit             : Begin
                                 End;
    cmDEDITSpeichern           : Begin
                                   Wait('Speichern der Editierten Daten');
                                   Video.SpeichereDateien;
                                   SetAllNil;
                                   Event.What := evCommand;
                                   Event.Command := cmOk;
                                   PutEvent(Event)
                                 End;
    cmDEDITLabeledit           : Begin
                                 End;
    cmDEDITDiskBereichLoeschen : Begin
                                   Wait('Lîsche Diskbereich');

                                   If Showtyp = 0 then
                                   Begin
                                     LabelString := ALabelDateiListBox^.
                                                    GetText(ALabelDateiListBox^.Focused,0);
                                     Wait('Lîsche Diskbereich: ' + Chr(13) +
                                           LabelString);

                                     LoescheDiskbereich(LabelString);
                                     If LabelString = '' then
                                     Begin
                                       ALabelDateiCollection^.AtDelete(ALabelDateiListBox^.Focused);
                                       ALabelDateiListBox^.SetRange(ALabelDateiListBox^.Range - 1);
                                       ALabelDateiListBox^.Draw
                                     End
                                   End
                                   Else
                                     Wait('Es werden keine Labels angezeigt!')
                                 End;
  End;




  (**************************)
  (* Pfad wird angezeigt:   *)
  (**************************)



  If Not EditAllPlatte then
    If (Event.What = evBroadCast) and
       (Event.Command = cmWindowListItemSelected) then
    Begin
      If (Event.InfoPtr = APfadListBox) and
          Not EditAllPfad               then
      Begin
        If PfadFocus <> APfadListBox^.Focused then
          PfadFocus := APfadListBox^.Focused
        Else

          If PfadFocus in [0, APfadListBox^.List^.Count - 1] then
          Begin
            Ton(100, 100);
            If PfadFocus = 0 then
              If APfadListBox^.Anfang > 0 then
              Begin
                ResetPfadListBox;
                PfadFocus := - 1;
                InitPlattenPfadData;
                Redraw;
                PfadFocus := 0
              End
              Else
            Else
              If PfadFocus = APfadListBox^.List^.Count - 1 then
                If APfadListBox^.Anfang +
                   PfadFocus < Filesize(PfadDataFile) then
                Begin
                  ResetPfadListBox;
                  PfadFocus := - 1;
                  InitPlattenPfadData;
                  Redraw;
                  PfadFocus := 0
                End
          End
      End;


      (**************************)
      (* Label wird angezeigt:  *)
      (**************************)




      If (Event.InfoPtr = ALabelDateiListBox) and
         (Not EditAllLabelDatei) and
         (ADLRadioButton^.Value = 0) then
      Begin
        If LabelDateiFocus <> ALabelDateiListBox^.Focused then
          LabelDateiFocus := ALabelDateiListBox^.Focused
        Else

          If LabelDateiFocus in [0, ALabelDateiListBox^.List^.Count - 1] then
          Begin
            Ton(100, 100);
            If LabelDateiFocus = 0 then
              If ALabelDateiListBox^.Anfang > 0 then
              Begin
                ResetLabelDateiListBox;
                LabelDateiFocus := - 1;
                InitPlattenLabelData;
                Redraw;
                LabelDateiFocus := 0
              End
              Else
            Else
              If LabelDateiFocus = ALabelDateiListBox^.List^.Count - 1 then
                If ALabelDateiListBox^.Anfang +
                   LabelDateiFocus < Filesize(LabelDataFile) then
                Begin
                  ResetLabelDateiListBox;
                  LabelDateiFocus := - 1;
                  InitPlattenLabelData;
                  Redraw;
                  LabelDateiFocus := 0
                End
          End

      End;


      (**************************)
      (* Datei wird angezeigt:  *)
      (**************************)



      If (Event.InfoPtr = ALabelDateiListBox) and
         (Not EditAllLabelDatei) and
         (ADLRadioButton^.Value = 1) then
      Begin
        If LabelDateiFocus <> ALabelDateiListBox^.Focused then
          LabelDateiFocus := ALabelDateiListBox^.Focused
        Else


          If LabelDateiFocus in [0, ALabelDateiListBox^.List^.Count - 1] then
          Begin
            Ton(100, 100);
            If LabelDateiFocus = 0 then
              If DateiSeek > 0 then

(***********)
(* ZurÅck: *)
(***********)

              Begin
                DateiSeek := DateiSeek - DateiCount;
                If DateiSeek < 0 then DateiSeek := 0;

                ResetLabelDateiListBox;
                LabelDateiFocus := - 1;
                InitPlattenDateiData(DateiSeek);
                LabelDateiFocus := 0;
              End
              Else
            Else
              If LabelDateiFocus = ALabelDateiListBox^.List^.Count - 1 then
                If Not Eof(DateiDataFile) then

(***********)
(* Weiter: *)
(***********)

                Begin
                  DateiSeek := DateiSeek + DateiCount;

                  ResetLabelDateiListBox;
                  LabelDateiFocus := - 1;
                  InitPlattenDateiData(DateiSeek);
                  LabelDateiFocus := 0;
                End
          End
      End;
      ClearEvent(Event)
    End;

  ALabelDateiListBox^.HandleEvent(Event);
  APfadListBox^.HandleEvent(Event);

  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) then
  Begin
    If (Event.InfoPtr = ALabelDateiListBox) and
       Not IsPfadSelected then
    Begin
      If Showtyp = 0 then
        LabelSelected
      Else
        DateiSelected
    End;
    If (Event.InfoPtr = APfadListBox) and
       Not (IsLabelSelected or IsDateiSelected) then
      PfadSelected
  End;


End;


PROCEDURE DateneditorDialog;
  VAR
    R: tRect;
    Code: INTEGER;
    Dialog: pDateneditorDialog;
BEGIN
  OpenAllFiles;
  R.Assign (0, 0, 80, 23);
  Dialog := New (pDateneditorDialog, Init (R, 'Dateneditor'));

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));

  IF Code <> cmCancel THEN
  BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
    { Data muss ausgewertet werden ! }
  END;
  IF Dialog <> NIL THEN
    Dispose (Dialog, Done);
  CloseAllFiles
END;

Procedure InitDEdit;

Begin
  DateiCount := 0;
  PfadCount := 0;
  LabelCount := 0;
  DateiSeek := 0;
  PfadSeek := 0;
  LabelSeek := 0;
  IsLabelSelected := False;
  IsPfadSelected := False;
  IsDateiSelected := False
End;

End.
