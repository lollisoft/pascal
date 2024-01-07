Unit ConInPro; (* Variablen im Projekt (Editor) *)

Interface
Uses TVBUCMDS, msgBox, App, Objects, Drivers, Dialogs, Views, MyViews, AB_Obj;



  TYPE
    ConstanteninProjektData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      ListPtr1: pCollection;
      Focused1: INTEGER;
      String0: STRING [80];
      String1: STRING [80];
      String2: STRING [80];
    END;





  PConstListBox = ^TConstListBox;
  TConstListBox = Object(TMyListBox)
    constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
    Function GetText(Item: Integer; Len: Integer): String; Virtual;
  End;

  PConstValueListBox = ^TConstValueListBox;
  TConstValueListBox = Object(TMyListBox)
    constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
    Function GetText(Item: Integer; Len: Integer): String; Virtual;
  End;
  
  pConstanteninProjektDialog = ^tConstanteninProjektDialog;
  tConstanteninProjektDialog = OBJECT (tMyDialog)
    B: Boolean;
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);

    Procedure HandleEvent(Var Event: TEvent); Virtual;
  END;

PROCEDURE ConstanteninProjektDialog(P: PConstList);

Implementation


CONSTRUCTOR tConstanteninProjektDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  B := False;

  tMyDialog.Init (Bounds, aTitle);
  R.Assign (29, 3, 30, 9);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0);
  Insert(View);
  R.Assign (5, 3, 29, 9);
  View := New (pConstListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (5, 2, 18, 3);
  Insert (New (pMyLabel, Init (R, '~C~onstanten', View)));
 
  R.Assign (69, 3, 70, 9);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0);
  Insert(View);
  R.Assign (45, 3, 69, 9);
  View := New (pConstValueListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (45, 2, 65, 3);
  Insert (New (pMyLabel, Init (R, 'Constanten - ~W~ert', View)));
 
  R.Assign (5, 12, 29, 13);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (5, 11, 25, 12);
  Insert (New (pMyLabel, Init (R, 'Constanten - ~N~ame', View)));
  R.Assign (29, 12, 32, 13);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (45, 12, 69, 13);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (45, 11, 51, 12);
  Insert (New (pMyLabel, Init (R, '~T~yp', View)));
  R.Assign (69, 12, 72, 13);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (5, 15, 29, 16);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (5, 14, 14, 15);
  Insert (New (pMyLabel, Init (R, '~I~nhalt', View)));
  R.Assign (29, 15, 32, 16);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (4, 19, 13, 21);
  Insert (New (pButton, Init (R, '~N~eu', cmVarNeu, 0)));

  R.Assign (16, 19, 30, 21);
  Insert (New (pButton, Init (R, '~L~”schen', cmVarDelete, 0)));

  R.Assign (44, 19, 52, 21);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (57, 19, 70, 21);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);

  B := True;
END;



Procedure tConstanteninProjektDialog.HandleEvent(Var Event: TEvent);
Var S, S1, S2: String;
    U: PMyList;
    E, E1, E2: PInputLine;
    R: PRadioButtons;
    L: PListBox;
    I: Integer;

Procedure NewConst(L, L1: PListBox; T: String);
Var ConstObj : PConstObj;
Begin
    If L <> Nil then
    Begin
      U := PMyList(L^.List);

      If U = Nil then Exit;

      E := GetViewObject('Constanten - Name');

      If (E <> Nil) and (TypeOf(E^) = TypeOf(TInputLine)) then
      Begin
        E^.GetData(S);
        S1 := PInputLine(GetViewObject('Inhalt'))^.Data^;

        If (S = '') or (S1 = '') then Exit;

        ConstObj := New(PConstObj, Init);
        ConstObj^.SetName(S);

        ConstObj^.SetValue(S1);


        If T <> '' then
          ConstObj^.SetTyp(T)
        Else
          ConstObj^.SetTyp('');

        If U = Nil then Halt;

        L^.List^.AtInsert(U^.Focused, ConstObj);

        L1^.SetRange(U^.Count);
        L^.SetRange(U^.Count);
      End;
    End;
End;

Procedure DeleteConst(L, L1: PListBox);
Begin
  If L <> Nil then
  Begin
    Pointer(U) := L^.List;

    If U = Nil Then Exit;

    If U^.Count > 0 then
    Begin
      E := GetViewObject('Constanten - Name');
      E1 := GetViewObject('Typ');
      E2 := GetViewObject('Inhalt');



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TInputLine)) then
      Begin
        S := PConstObj(U^.At(U^.Focused))^.GetName;
        S1 := PConstObj(U^.At(U^.Focused))^.GetTyp;
        S2 := PConstObj(U^.At(U^.Focused))^.GetValue;

        L^.List^.AtFree(U^.Focused);

        If U^.Focused > U^.Count - 1 then U^.Focused := U^.Count - 1;

        B := False;
        L^.SetRange(U^.Count);
        L1^.SetRange(U^.Count);
        B := True;

        If U^.Count = 0 then
        Begin
          E^.SetData(S);
          E1^.SetData(S1);
          E2^.SetData(S2);
          U^.Focused := 0;
        End
        Else
        Begin
          E^.SetData(S);
          E1^.SetData(S1);
          E2^.SetData(S2);
        End;
      End;
    End
  End
End;


Begin
  TMyDialog.HandleEvent(Event);

  If (Event.What = evCommand) and
     (Event.Command = cmVarNeu) then
  Begin
    R := GetViewObject('Typ');

    If (R <> Nil) and (TypeOf(R^) = TypeOf(TInputLine)) then
    Begin
      S := PInputLine(R)^.Data^;

      If S = '' then
        NewConst(GetViewObject('Constanten'),
                 GetViewObject('Constanten - Wert'), '')
      Else
        NewConst(GetViewObject('Constanten'),
                 GetViewObject('Constanten - Wert'), S)
    End;
    ClearEvent(Event);
    ReDraw;
  End;

  If (Event.What = evCommand) and
     (Event.Command = cmVarDelete) then
  Begin
    DeleteConst(GetViewObject('Constanten'),
                GetViewObject('Constanten - Wert'));

    ClearEvent(Event);
    ReDraw;
  End;

  If (Event.What = evBroadCast) and
     (Event.Command = cmItemFocused) and B then
  Begin
    S := GetViewName(Event.InfoPtr);

    B := False;

    If S = 'Constanten' then
      PListBox(GetViewObject('Constanten - Wert'))^.FocusItem(
      PListBox(Event.InfoPtr)^.Focused)
    Else
      If S = 'Constanten - Wert' then
        PListBox(GetViewObject('Constanten'))^.FocusItem(
        PListBox(Event.InfoPtr)^.Focused);



    B := True;

    ClearEvent(Event);
    ReDraw
  End;

End;

PROCEDURE ConstanteninProjektDialog(P: PConstList);
  VAR
    R: tRect;
    Data: ConstanteninProjektData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pConstanteninProjektDialog, Init (R, 'Variablen in Projekt'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (ConstanteninProjektData), 0);

  Data.ListPtr0 := P;
  Data.Focused0  := P^.Focused;

  Data.ListPtr1 := P;
  Data.Focused1  := P^.Focused;


  Dialog^.SetData (Data); 
  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
    Dialog^.GetData (Data);
    { Data muss ausgewertet werden ! }
  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;


constructor TConstListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
Begin
  TMyListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Function TConstListBox.GetText(Item: Integer; Len: Integer): String;
Begin
  GetText := PConstObj(List^.At(Item))^.GetName
End;

constructor TConstValueListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
Begin
  TMyListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Function TConstValueListBox.GetText(Item: Integer; Len: Integer): String;
Begin
  GetText := PConstObj(List^.At(Item))^.GetValue
End;

End.