Unit VTest;

Interface

Uses Crt,
     Dos,
     App,
     TVVTCmds,
     Drivers,
     Memory,
     Views,
     Dialogs,
     Vid,
     VT,
     Objects,
     StrTools;

Type

  pAuswahl              = ^tAuswahl;
  tAuswahl              = Object(tObject)
    Item                : Integer;

    Constructor Init(I: Integer);
    Destructor Done; Virtual;
    Procedure SetData(Var Rec); Virtual;
    Procedure GetData(Var Rec); Virtual;
  End;

  PDatum                = ^TDatum;
  TDatum                = OBject(TObject)
    Datum               : PString;
    Constructor Init(Date: String);
    Destructor Done; Virtual;
  End;

  PDatumCollection       = ^TDatumCollection;
  TDatumCollection       = Object(TSortedCollection)
    Constructor Init(ALimit, ADelta: Integer);
    Destructor Done; Virtual;
    Function Compare(Key1, Key2: Pointer): Integer; Virtual;
    Function KeyOf(Item: Pointer): Pointer; Virtual;
    Procedure Insert(Item: Pointer); Virtual;
  End;

  PDatumListBox         = ^TDatumListBox;
  TDatumListBox         = Object(TVokabelListBox)
    Constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
    Destructor Done; Virtual;
    Function GetText(Item: Integer; MaxLen: Integer): String; Virtual;
  End;

  pVokabelntestenDialog = ^tVokabelntestenDialog;
  tVokabelntestenDialog = OBJECT (tDialog)
    DatumStelle         : Integer;
    AInputLine          : PInputLine;
    ATest,
    AErgebnis           : PStaticText;

    ATranslateScrollBar : PScrollBar;
    ATranslateCollection: PVokabelCollection;
    ATranslateListBox   : PVokabelListBox;

    ADatumScrollBar     : PScrollBar;
    ADatumCollection    : PDatumCollection;
    ADatumListBox       : PDatumListBox;

    ADeutschVAuswahl    : PCollection;
    AEnglischVAuswahl   : PCollection;

    AVokabel            : PVokabel;

    VTESTCheck          : PCheckBoxes;
    V                   : Word;

    DCountDown,
    ECountDown          : PStaticText;

    Geprueft,
    Richtig             : Integer;

    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Destructor  Done; Virtual;
    Function GetWort(Var Vokabel: PVokabel; Tested: Boolean):String; Virtual;
    Function GetCount(P: PVokabelCollection): String; Virtual;
    Procedure HandleEvent(Var Event: TEvent); Virtual;
    Procedure InitVA(Var Menge: Longint); Virtual;
  END;

PROCEDURE VokabelntestenDialog;

Implementation

Var LastES,
    LastDS,
    EDRandom,
    EStelle,
    DStelle  : Integer;
    TestMenge,
    M1,
    M2       : Longint;
    FE, FD,
    DatumFile: File of Integer;


Constructor TDatum.Init(Date: String);
Begin
  TObject.Init;
  Datum := NewStr(Date);
End;
Destructor TDatum.Done;
Begin
  If Datum <> Nil then DisposeStr(Datum);
  TObject.Done
End;

Constructor TDatumCollection.Init(ALimit, ADelta: Integer);
Begin
  TSortedCollection.Init(ALimit, ADelta)
End;

Destructor TDatumCollection.Done;
Begin
  FreeAll;
  TSortedCollection.Done;
End;

Procedure TDatumCollection.Insert(Item: Pointer);
Var C: Integer;
Begin
  C := Count;
  TSortedCollection.Insert(Item);
  If C = Count then Dispose(PDatum(Item), Done)
End;

Function TDatumCollection.Compare(Key1, Key2: Pointer): Integer;
Var S1, S2: String;
Begin
  S1 := PString(Key1)^;
  S2 := PString(Key2)^;

  If S1[2] = '.' then S1 := '0' + S1;
  If S2[2] = '.' then S2 := '0' + S2;

  If S1[5] = '.' then System.Insert('0', S1, 4);
  If S2[5] = '.' then System.Insert('0', S2, 4);

  S1 := System.Copy(S1, 7, 4) +
        System.Copy(S1, 4, 3) +
        System.Copy(S1, 1, 3);

  S2 := System.Copy(S2, 7, 4) +
        System.Copy(S2, 4, 3) +
        System.Copy(S2, 1, 3);

  If S1 < S2 then
    Compare := - 1
  Else
    If S1 = S2 then
      Compare := 0
    Else
      Compare := 1;
End;
Function TDatumCollection.KeyOf(Item: Pointer): Pointer;
Begin
  KeyOf := PDatum(Item)^.Datum
End;

Constructor TDatumListBox.Init(var Bounds: TRect;
                                   ANumCols: Word;
                                 AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Destructor TDatumListBox.Done;
Begin
  TListBox.Done
End;

Function TDatumListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := PDatum(List^.At(Item))^.Datum^
End;

Constructor tAuswahl.Init(I: Integer);
Begin
  tObject.Init;
  Item := I
End;

Destructor tAuswahl.Done;
Begin
  tObject.Done
End;

Procedure tAuswahl.SetData(Var Rec);
Var I: Integer Absolute Rec;
Begin
  Item := I
End;

Procedure tAuswahl.GetData(Var Rec);
Var I: Integer Absolute Rec;
Begin
  I := Item
End;

Procedure SetTested(Vokabel: PVokabel);
Begin
  Vokabel^.Vokabel^.Tested := True
End;

Const EVA : Integer = 0;
      DVA : Integer = 0;

Procedure tVokabelntestenDialog.InitVA(Var Menge: Longint);
Var P   :Pointer;
    ER,
    DR  : Integer;

Function Vorhanden(Item: Integer; PVA: PCollection): Boolean;
Var Lauf: Integer;
Begin
  Lauf := 0;
  While (Lauf < PVA^.Count) and
        (PAuswahl(PVA^.At(Lauf))^.Item <> Item) do Inc(Lauf);

  Vorhanden := Lauf < PVA^.Count
End;

Procedure LadeAuswahl;
Var Stelle: Integer;

Procedure TestCheck;
Begin
  Stelle := 0;
  A := 0;
  Case VTESTCheck^.Value of
    1 : While (Stelle < ADeutschCollection^.Count) and
              (A <= Menge) do
        Begin
          If Not PVokabel(ADeutschCollection^.At(
                          Stelle))^.Vokabel^.Tested
             and (PVokabel(ADeutschCollection^.At(Stelle))^.
                  Vokabel^.Datum =
                  PString(
                  PDatum(
                  ADatumCollection^.At(ADatumListBox^.
                  Focused))^.Datum)^)

             then Inc(A);
          Inc(Stelle)
        End;
    0 : While (Stelle < ADeutschCollection^.Count) and
              (A <= Menge) do
        Begin
          If Not PVokabel(ADeutschCollection^.At(
                          Stelle))^.Vokabel^.Tested
             then Inc(A);

          Inc(Stelle)
        End;
  End;

  Stelle := 0;
  B := 0;

  Case VTESTCheck^.Value of
    1 : While (Stelle < AEnglischCollection^.Count) and
              (B <= Menge) do
        Begin
          If Not PVokabel(AEnglischCollection^.At(
                          Stelle))^.Vokabel^.Tested
             and (PVokabel(AEnglischCollection^.At(Stelle))^.
                  Vokabel^.Datum =
                  PString(
                  PDatum(
                  ADatumCollection^.At(ADatumListBox^.
                  Focused))^.Datum)^)

             then Inc(B);
          Inc(Stelle)
        End;
    0 : While (Stelle < AEnglischCollection^.Count) and
              (B <= Menge) do
        Begin
          If Not PVokabel(AEnglischCollection^.At(
                          Stelle))^.Vokabel^.Tested
             then Inc(B);

          Inc(Stelle)
        End;
  End;
End;

Procedure Test0;
Var    Lauf  : Integer;
Begin
  If (A = 0) and (B = 0) then
  Begin
    If A = 0 then
    Begin
      Case VTESTCheck^.Value of
        1 : Begin
              For Lauf := 0 to ADeutschCollection^.Count - 1 do
                If  PVokabel(ADeutschCollection^.At(Lauf))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^ then
                     PVokabel(ADeutschCollection^.At(Lauf))^.Vokabel^.
                     Tested := False
            End;
        0 : Begin
              For Lauf := 0 to ADeutschCollection^.Count - 1 do
                PVokabel(ADeutschCollection^.At(Lauf))^.Vokabel^.
                Tested := False
            End;
      End;
    End;

    If B = 0 then
    Begin
      Case VTESTCheck^.Value of
        1 : Begin
              For Lauf := 0 to AEnglischCollection^.Count - 1 do
                If  PVokabel(AEnglischCollection^.At(Lauf))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^ then
                     PVokabel(AEnglischCollection^.At(Lauf))^.Vokabel^.
                     Tested := False
            End;
        0 : Begin
              For Lauf := 0 to AEnglischCollection^.Count - 1 do
                PVokabel(AEnglischCollection^.At(Lauf))^.Vokabel^.
                Tested := False
            End;
      End;
    End;
  End;
  (* Volgende Schleife endet immer, da zuvor Bereiche wieder auf False *)
  (* gesetzt wurden.                                                   *)


  While (A = 0) and
        (B = 0) and
        (VTESTCheck^.Value = 1) do
  Begin
    If ADatumListBox^.Focused < ADatumCollection^.Count - 1 then
      ADatumListBox^.FocusItem(ADatumListBox^.Focused + 1)
    Else
      ADatumListBox^.FocusItem(0);
    ReDraw;
    
    TestCheck
  End
End;


Begin
  TestCheck;
  Test0;

  If B < Menge then
  Begin
    Stelle := 0;
    While Stelle < AEnglischCollection^.Count do
    Begin
      Case VTESTCheck^.Value of
        1 : If Not PVokabel(AEnglischCollection^.At(
                            Stelle))^.Vokabel^.Tested and
                   (PVokabel(AEnglischCollection^.At(Stelle))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^)
            then
            Begin
              Write(FE, Stelle);
              P := New(PAuswahl, Init(Stelle));
              AEnglischVAuswahl^.Insert(P);
            End;
        0 : If Not PVokabel(AEnglischCollection^.At(
                            Stelle))^.Vokabel^.Tested
            then
            Begin
              Write(FE, Stelle);
              P := New(PAuswahl, Init(Stelle));
              AEnglischVAuswahl^.Insert(P);
            End;
      End;
      Inc(Stelle)
    End
  End
  Else
  For B := 0 to Menge - 1 do
  Begin
    Case VTESTCheck^.Value of
      1 : Repeat
            ER := Random(AEnglischCollection^.Count);
          Until Not PVokabel(AEnglischCollection^.At(ER))^.
                    Vokabel^.Tested and
                   (PVokabel(AEnglischCollection^.At(ER))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^) and
                Not Vorhanden(ER, AEnglischVAuswahl);
      0 : Repeat
            ER := Random(AEnglischCollection^.Count);
          Until Not PVokabel(AEnglischCollection^.At(ER))^.
                    Vokabel^.Tested and
                Not Vorhanden(ER, AEnglischVAuswahl);
    End;

    Write(FE, ER);
    P := New(PAuswahl, Init(ER));
    AEnglischVAuswahl^.Insert(P);
  End;

  If A < Menge then
  Begin
    Stelle := 0;
    While Stelle < ADeutschCollection^.Count do
    Begin
      Case VTESTCheck^.Value of
        1 : If (Not PVokabel(ADeutschCollection^.At(
                            Stelle))^.Vokabel^.Tested) and
                   (PVokabel(ADeutschCollection^.At(Stelle))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^) then
            Begin
              Write(FD, Stelle);
              P := New(PAuswahl, Init(Stelle));
              ADeutschVAuswahl^.Insert(P);
            End;
        0 : If Not PVokabel(ADeutschCollection^.At(
                            Stelle))^.Vokabel^.Tested
            then
            Begin
              Write(FD, Stelle);
              P := New(PAuswahl, Init(Stelle));
              ADeutschVAuswahl^.Insert(P);
            End;
      End;
      Inc(Stelle)
    End
  End
  Else
  For A := 0 to Menge - 1 do
  Begin
    Case VTESTCheck^.Value of
      1 : Repeat
            DR := Random(ADeutschCollection^.Count);
          Until Not PVokabel(ADeutschCollection^.At(DR))^.
                    Vokabel^.Tested and
                   (PVokabel(ADeutschCollection^.At(DR))^.
                    Vokabel^.Datum =
                    PString(
                    PDatum(
                    ADatumCollection^.At(ADatumListBox^.
                    Focused))^.Datum)^) and
                Not Vorhanden(DR, ADeutschVAuswahl);
      0 : Repeat
            DR := Random(ADeutschCollection^.Count)
          Until Not PVokabel(ADeutschCollection^.At(DR))^.
                    Vokabel^.Tested and
                Not Vorhanden(DR, ADeutschVAuswahl);
    End;

    Write(FD, DR);
    P := New(PAuswahl, Init(DR));
    ADeutschVAuswahl^.Insert(P);
  End;

  System.Close(FE);
  System.Close(FD)
End;

Begin
  AEnglischVAuswahl := New(PCollection, Init(1, 1));
  ADeutschVAuswahl  := New(PCollection, Init(1, 1));


  {$I-}
  Reset(FE);
  Reset(FD);
  {$I+}

  If IOResult <> 0 then
  Begin
    Rewrite(FE);
    Rewrite(FD);

    LadeAuswahl;
  End
  Else
  Begin
    While Not Eof(FD) do
    Begin
      Read(FD, DR);
      P := New(PAuswahl, Init(DR));
      ADeutschVAuswahl^.Insert(P);
    End;

    While Not Eof(FE) do
    Begin
      Read(FE, ER);
      P := New(PAuswahl, Init(ER));
      AEnglischVAuswahl^.Insert(P)
    End;

    M1 := FileSize(FE);
    M2 := FileSize(FD);

    If M1 <= M2 then Menge := M1;
    If M2 < M1 then Menge := M2;

    System.Close(FE);
    System.Close(FD)
  End
End;

Var    Leer, WortNeu    : String;
       TestRichtig: Boolean;


Function tVokabelntestenDialog.GetWort(Var Vokabel: PVokabel; Tested: Boolean): String;

Function AlReady(PVA: PCollection;
                 PV: PVokabelCollection): Boolean;
Begin
  AlReady := False;

  For A := 0 to PVA^.Count - 1 do
    If Not PVokabel(PV^.At(PAuswahl(PVA^.At(A))^.Item))^.Vokabel^.Tested
      then Exit;

  Already := True
End;


Begin
  Leer := '';

  If (AEnglischCollection^.Count <> 0) or
     (ADeutschCollection^.Count <> 0) then
  Begin
    If Tested then Save := True;

    If Tested and (EDRandom > 50) then
       SetTested(AEnglischCollection^.At(EStelle));

    If Tested and (EDRandom <= 50) then
       SetTested(ADeutschCollection^.At(DStelle));

    If AlReady(AEnglischVAuswahl, AEnglischCollection) and
       AlReady(ADeutschVAuswahl,  ADeutschCollection) then
    Begin
      Wait('Already');
      Dispose(AEnglischVAuswahl, Done);
      Dispose(ADeutschVAuswahl, Done);
      Erase(FE);
      Erase(FD);
      TestMenge := 15;
      InitVA(TestMenge);

      ATranslateCollection^.DeleteAll;
      ATranslateListBox^.SetRange(ATranslateCollection^.Count);
      AInputLine^.SetData(Leer);

      WortNeu := GetWort(AVokabel, False);
      DisposeStr(ATest^.Text);
      ATest^.Text := NewStr(WortNeu);


      DisposeStr(ECountDown^.Text);
      Leer := 'Engl. Count: ' + GetCount(AEnglischCollection);
      ECountDown^.Text := NewStr(Leer);

      DisposeStr(DCountDown^.Text);
      Leer := 'Deut. Count: ' + GetCount(ADeutschCollection);
      DCountDown^.Text := NewStr(Leer);
      Leer := '';
      ReDraw;
    End;

    EDRandom := Random(100);

    If EDRandom > 50 then
      If AlReady(AEnglischVAuswahl, AEnglischCollection) then
      Begin
        EDRandom := 40;
        (*Wait('EDRandom := 40;')*)
      End;

    If EDRandom < 50 then
      If AlReady(ADeutschVAuswahl, ADeutschCollection) then
      Begin
        EDRandom := 60;
        (*Wait('EDRandom := 60;')*)
      End;


    If AlReady(AEnglischVAuswahl, AEnglischCollection) and
       AlReady(ADeutschVAuswahl,  ADeutschCollection) then
      Wait('Beide fertig!');

    If EDRandom > 50 then
    Begin
      While (EVA < AEnglischVAuswahl^.Count) and
             PVokabel(AEnglischCollection^.At
             (PAuswahl(AEnglischVAuswahl^.At(EVA))^.
             Item))^.Vokabel^.Tested
             do
               Inc(EVA);

      If EVA >= AEnglischVAuswahl^.Count then
        Wait('EVA zu gro·' + Chr(13) +
             'EVA  : ' + GetString(EVA) + Chr(13) +
             'Count: ' + GetString(AEnglischVAuswahl^.Count));

      EStelle := PAuswahl(AEnglischVAuswahl^.At(EVA))^.Item;


      Vokabel := New(PVokabel,
                     Init(PVokabel(
                          AEnglischCollection^.At
                          (PAuswahl(AEnglischVAuswahl^.At(EVA))^.
                          Item))^.Vokabel^));

      Vokabel^.Vokabel^ := PVokabel(
                           AEnglischCollection^.At
                           (PAuswahl(AEnglischVAuswahl^.At(EVA))^.
                           Item))^.Vokabel^;
      If EVA + 1 = AEnglischVAuswahl^.Count then EVA := 0
    End
    Else
    Begin
      While (DVA < ADeutschVAuswahl^.Count) and
             PVokabel(ADeutschCollection^.At
             (PAuswahl(ADeutschVAuswahl^.At(DVA))^.
             Item))^.Vokabel^.Tested
             do
      Inc(DVA);

      If DVA >= ADeutschVAuswahl^.Count then
        Wait('DVA zu gro·' + Chr(13) +
             'DVA  : ' + GetString(DVA) + Chr(13) +
             'Count: ' + Chr(13) + GetString(ADeutschVAuswahl^.Count));

      DStelle := PAuswahl(ADeutschVAuswahl^.At(DVA))^.Item;

      Vokabel := New(PVokabel,
                     Init(PVokabel(
                          ADeutschCollection^.At(
                          PAuswahl(ADeutschVAuswahl^.At(DVA))^.
                          Item))^.Vokabel^));

      Vokabel^.Vokabel^ := PVokabel(
                           ADeutschCollection^.At(
                           PAuswahl(ADeutschVAuswahl^.At(DVA))^.
                           Item))^.Vokabel^;
      If DVA +  1 = ADeutschVAuswahl^.Count then DVA := 0;
    End;

    If EDRandom > 50 then
      GetWort := 'Englisch: ' + Vokabel^.Vokabel^.Wort
    Else
      GetWort := 'Deutsch: ' + Vokabel^.Vokabel^.Wort
  End
  Else
  Begin
    Wait('Vokabeltest ohne geladene Daten noch nicht mîglich!')
  End;

End;

Destructor tVokabelntestenDialog.Done;
Begin
  {$I-}
  ReWrite(DatumFile);
  {$I+}
  If IOResult = 0 then
  Begin
    Write(DatumFile, ADatumListBox^.Focused);
    System.Close(DatumFile)
  End
  Else Wait('Momentan eingestelltes Datum kann nicht gespeichert werden, ' +
            'da sich die Datei nicht îffnen lÑsst!');

  If Not NoFiles then
  Begin
    System.Close(EVF);
    System.Close(DVF);
    System.Close(EDTF);
    System.Close(DETF)
  End;
  Dispose(AInputLine, Done);
  Dispose(VTESTCheck, Done);
  Dispose(ATest, Done);
  Dispose(AErgebnis, Done);

  ATranslateCollection^.DeleteAll;
  Dispose(ATranslateCollection, Done);

  Dispose(ATranslateListBox, Done);
  Dispose(ATranslateScrollBar, Done);
  Dispose(AVokabel, Done);
  Dispose(ADatumListBox, Done);
  Dispose(ADatumCollection, Done);
  Dispose(ADatumScrollBar, Done);

  Dispose(DCountDown, Done);
  Dispose(ECountDown, Done);

  Dispose(AEnglischVAuswahl, Done);
  Dispose(ADeutschVAuswahl, Done);

  TDialog.Done
End;

Function tVokabelntestenDialog.GetCount(P: PVokabelCollection): String;
Begin
  B := 0;
  Case VTESTCheck^.Value of
    0 : For A := 0 to P^.Count - 1 do
          If PVokabel(P^.At(A))^.Vokabel^.Tested = False then Inc(B);
    1 : For A := 0 to P^.Count - 1 do
          If (PVokabel(P^.At(A))^.Vokabel^.Tested = False) and
             (PVokabel(P^.At(A))^.Vokabel^.Datum =
              PString(
              PDatum(
              ADatumCollection^.At(ADatumListBox^.
              Focused))^.Datum)^) then Inc(B);
  End;
  GetCount := GetString(B)
End;

CONSTRUCTOR tVokabelntestenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
    Wort: String;
       P: PDatum;
       S: String;
       Z: Integer;
BEGIN
  AVokabel := Nil;
  Richtig := 0;
  Geprueft := 0;

  tDialog.Init (Bounds, aTitle);
  R.Assign (5, 2, 26, 3);
  Insert (New (pStaticText, Init (R, 'Bitte Åbersetzen Sie:')));

  R.Assign (4, 7, 40, 8);
  AInputLine := New (pInputLine, Init (R, 80));
  Insert (AInputLine);

  R.Assign (4, 6, 16, 7);
  Insert (New (pLabel, Init (R, '~B~edeutung', AInputLine)));


  R.Assign (59, 3+13, 60, 8+13);
  ADatumScrollBar := New (pScrollbar, Init (R));
  ADatumScrollBar^.SetRange (0, 0);
  Insert(ADatumScrollBar);

  R.Assign (45+5-5, 3+13, 59+5-5, 8+13);
  ADatumListBox := New (pDatumListBox, Init (R, 1, pScrollBar (ADatumScrollBar)));
  Insert (ADatumListBox);
  R.Assign (45+5-5, 2+13, 59+5-5, 3+13);
  Insert (New (pLabel, Init (R, '~D~atum', ADatumListBox)));

  ADatumCollection := New(PDatumCollection, Init(1,1));
  ADatumListBox^.Options := ADatumListBox^.Options And Not ofSelectable;
  ADatumListBox^.NewList(ADatumCollection);
  ADatumListBox^.SetRange(0);
  ADatumListBox^.EventMask := FocusedEvents + PositionalEvents;

  For A := 0 to ADeutschCollection^.Count - 1 do
    ADatumCollection^.Insert(New(
                             PDatum,
                             Init(
                             PVokabel(
                             ADeutschCollection^.At(A))^.
                             Vokabel^.Datum)));

  For A := 0 to AEnglischCollection^.Count - 1 do
    ADatumCollection^.Insert(New(
                             PDatum,
                             Init(
                             PVokabel(
                             AEnglischCollection^.At(A))^.
                             Vokabel^.Datum)));


  ADatumListBox^.SetRange(ADatumCollection^.Count);
  ADatumListBox^.Options := ADatumListBox^.Options or ofSelectable;

  If ADatumListBox^.Range > 0 then
  Begin
    Assign(DatumFile, 'PRDatum.Dat');
    {$I-}
      Reset(DatumFile);
    {$I+}

    If IOResult = 0 then
    Begin
      Read(DatumFile, DatumStelle);
      ADatumListBox^.Focused := DatumStelle
    End
  End;

  R.Assign (50-5, 7, 70-5, 8);
  VTESTCheck := New (pCheckBoxes,
               Init (R, 
                     NewSItem ('nur akt. Datum',
                     NewSItem ('M. nicht verÑ.',
                     NIL))));
  Insert (VTESTCheck);
  V := VTESTCheck^.Value;

  R.Assign (50-5, 6, 69-5, 7);
  Insert (New (pLabel, Init (R, 'Sonder~f~unktionen', VTESTCheck)));


  TestMenge := 15;
  InitVA(TestMenge);


  R.Assign (5, 4, 40, 5);
  Wort := GetWort(AVokabel, False);
  ATest := New (pStaticText, Init (R, Wort));
  Insert(ATest);

  (*********************)
  (* Translate Listbox *)
  (*********************)

  R.Assign (39, 3+13, 40, 8+13);
  ATranslateScrollBar := New (pScrollBar, Init  (R));
  ATranslateScrollBar^.SetRange (0, 0);
  Insert(ATranslateScrollBar);

  R.Assign (4, 3+13, 39, 8+13);
  ATranslateListBox := New (pVokabelListBox, Init (R, 1, pScrollBar (ATranslateScrollBar)));
  Insert (ATranslateListBox);
  R.Assign (4, 2+13, 16, 3+13);
  Insert (New (pLabel, Init (R, 'öber~s~etzung', ATranslateListBox)));

  (***********************)
  (* TranslateCollection *)
  (***********************)

  ATranslateCollection := New(PVokabelCollection, Init(1, 1));
  ATranslateListBox^.Options := ATranslateListBox^.Options And Not ofSelectable;
  ATranslateListBox^.NewList(ATranslateCollection);
  ATranslateListBox^.SetRange(0);
  ATranslateListBox^.EventMask := FocusedEvents + PositionalEvents;


  R.Assign (65, 18, 75, 20);
  Insert (New (pButton, Init (R, '~E~nde', cmOk, 0)));

  R.Assign (4, 10, 40, 11);
  AErgebnis := New(PStaticText, Init(R, ''));
  Insert(AErgebnis);

  R.Assign (50-5, 10, 70-5, 11);
  DCountDown := New(PStaticText, Init(R, 'Deut. Count: ' + GetCount(ADeutschCollection)));

  R.Assign (50-5, 11, 70-5, 12);
  ECountDown := New(PStaticText, Init(R, 'Engl. Count: ' + GetCount(AEnglischCollection)));

  Insert(DCountDown);
  Insert(ECountDown);

  SelectNext (FALSE);
END;

Procedure tVokabelntestenDialog.HandleEvent(Var Event: TEvent);
Var
    R          : TRect;
    W1,
    W2,
    W3         : String;
    

Begin
  Leer := '';

  If (VTESTCheck^.Value <> V) then
  Begin
    V := VTESTCheck^.Value;
    Dispose(AEnglischVAuswahl, Done);
    Dispose(ADeutschVAuswahl, Done);
    Erase(FE);
    Erase(FD);
    TestMenge := 15;
    InitVA(TestMenge);

    ATranslateCollection^.DeleteAll;
    ATranslateListBox^.SetRange(ATranslateCollection^.Count);
    AInputLine^.SetData(Leer);
    Dispose(AVokabel, Done);

    WortNeu := GetWort(AVokabel, False);
    DisposeStr(ATest^.Text);
    ATest^.Text := NewStr(WortNeu);

    DisposeStr(ECountDown^.Text);
    Leer := 'Engl. Count: ' + GetCount(AEnglischCollection);
    ECountDown^.Text := NewStr(Leer);

    DisposeStr(DCountDown^.Text);
    Leer := 'Deut. Count: ' + GetCount(ADeutschCollection);
    DCountDown^.Text := NewStr(Leer);
    Leer := '';

    ReDraw;
    AInputLine^.Select
  End;


  If (Event.What = evKeyDown) and
     (Event.KeyCode = kbEnter) and
     AInputLine^.GetState(sfSelected) then
  Begin
    Inc(Geprueft);
    TestRichtig := False;

    If AEnglischCollection^.Count <> 0 then
    Begin
      For A := 0 to AVokabel^.Vokabel^.Anzahl - 1 do
      Begin
        Case AVokabel^.Vokabel^.ED of
          True  : Begin
                    W1 := AInputLine^.Data^;

                    W2 := PVokabel(
                                 ADeutschCollection^.At(
                                 PTrans(
                                 AEDTransCollection^.At(
                                 AVokabel^.
                                 Vokabel^.ZL + A))^.
                                 Translate))^.
                                 Vokabel^.Wort;

                    If (Pos('=', W2) > 0) or
                       (Pos('(', W2) > 0) or
                       (Pos(',', W2) > 0) then
                      If Pos(W1, W2) > 0 then TestRichtig := True;
                    If W1 = W2 then TestRichtig := True
                  End;
          False : Begin
                    W1 := AInputLine^.Data^;

                    W2 := PVokabel(
                                 AEnglischCollection^.At(
                                 PTrans(
                                 ADETransCollection^.At(
                                 AVokabel^.
                                 Vokabel^.ZL + A))^.
                                 Translate))^.
                                 Vokabel^.Wort;

                    If (Pos('=', W2) > 0) or
                       (Pos('(', W2) > 0) or
                       (Pos(',', W2) > 0) then
                      If Pos(W1, W2) > 0 then TestRichtig := True;
                    If W1 = W2 then TestRichtig := True
                  End;
        End
      End;
      If TestRichtig then Inc(Richtig)
    End
    Else
    Begin
    End;

    ATranslateCollection^.DeleteAll;
    ATranslateListBox^.SetRange(ATranslateCollection^.Count);


    For A:= 0 to AVokabel^.Vokabel^.Anzahl - 1
    do
    Begin
      Case AVokabel^.Vokabel^.ED of
        True  : ATranslateCollection^.Insert(ADeutschCollection^.At(
                                     PTrans(
                                     AEDTransCollection^.At(
                                     AVokabel^.
                                     Vokabel^.ZL + A))^.
                                     Translate));

        False : ATranslateCollection^.Insert(AEnglischCollection^.At(
                                     PTrans(
                                     ADETransCollection^.At(
                                     AVokabel^.
                                     Vokabel^.ZL + A))^.
                                     Translate));
      End
    End;


    ATranslateListBox^.SetRange(ATranslateCollection^.Count);
    ATranslateListBox^.Options := ATranslateListBox^.Options or ofSelectable;


    AInputLine^.SetData(Leer);
    Dispose(AVokabel, Done);

    WortNeu := GetWort(AVokabel, TestRichtig);

    DisposeStr(ATest^.Text);
    ATest^.Text := NewStr(WortNeu);

    DisposeStr(AErgebnis^.Text);
    AErgebnis^.Text := NewStr('GeprÅft: ' +
                               GetString(Geprueft) +
                               ', ' +
                               'richtig: ' +
                               GetString(Richtig));

    DisposeStr(ECountDown^.Text);
    Leer := 'Engl. Count: ' + GetCount(AEnglischCollection);
    ECountDown^.Text := NewStr(Leer);

    DisposeStr(DCountDown^.Text);
    Leer := 'Deut. Count: ' + GetCount(ADeutschCollection);
    DCountDown^.Text := NewStr(Leer);
    Leer := '';


    ReDraw;
  End;


  TDialog.HandleEvent(Event);

  ADatumListBox^.HandleEvent(Event);
  ATranslateListBox^.HandleEvent(Event);


End;


PROCEDURE VokabelntestenDialog;
  VAR
    R: tRect;
    Code: INTEGER;
    Dialog: pDialog;
    F     : File;
BEGIN
  If VokabelDateiTest(EVF) then
  Begin
    VokabelDateiOeffnen(EVF);
    VokabelDateiOeffnen(DVF);
    TranslateDateiOeffnen(EDTF);
    TranslateDateiOeffnen(DETF);
    NoFiles := False;
  End
  Else
    NoFiles := True;

  If AEnglischCollection^.Count = 0 then
  Begin
    If NoFiles then
    Begin
      Wait('Vokabeltest ist nicht mîglich, ' +
           'da keine Dateien vorhanden sind!');
      Exit
    End
    Else
    Begin
      Wait('Vokabeltest ist nicht mîglich, ' +
           'da keine Daten geladen sind!');
      Exit
    End
  End;

  Assign(F, 'PRDatum.Dat');

  {$I-}
  Reset(FE);
  Reset(FD);

  If IOResult = 0 then
    If (FileSize(FE) = 0) or (FileSize(FD) = 0) then
    Begin
      System.Close(FE);
      System.Close(FD);
      Erase(FE);
      Erase(FD)
    End;

  Reset(F);

  If IOResult <> 0 then
  Begin
    Reset(FE);
    If IOResult = 0 then
    Begin
      System.Close(FE);
      Erase(FE)
    End;

    Reset(FD);
    If IOResult = 0 then
    Begin
      System.Close(FD);
      Erase(FD)
    End
  End;

  {$I+}

  R.Assign (0, 0, 80, 23);
  Dialog := New (pVokabelntestenDialog, Init (R, 'Vokabeln testen'));
 
    { Datenrecord initialisieren ! }

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN

  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;

Begin
  Assign(FE, 'TEST1.DAT');
  Assign(FD, 'TEST2.DAT');
  Randomize;
End.