Unit VarInPro; (* Variablen im Projekt (Editor) *)



Interface

Uses TVBUCMDS, msgBox, App, Objects, Drivers, Dialogs, Views, MyViews, Var_Obj;





TYPE

  VariableninProjektData = RECORD

    ListPtr0: pCollection;

    Focused0: INTEGER;

    ListPtr1: pCollection;

    Focused1: INTEGER;

    Cluster0: WORD;

    Cluster1: WORD;

    String0: STRING[80];

  END;





  PVarListBox = ^TVarListBox;

  TVarListBox = Object(TMyListBox)

    constructor Init(var Bounds: TRect;

                       ANumCols: Word;

                     AScrollBar: PScrollBar);

    Function GetText(Item: Integer; Len: Integer): String; Virtual;

  End;



  PVarTypListBox = ^TVarTypListBox;

  TVarTypListBox = Object(TMyListBox)

    constructor Init(var Bounds: TRect;

                       ANumCols: Word;

                     AScrollBar: PScrollBar);

    Function GetText(Item: Integer; Len: Integer): String; Virtual;

  End;

  

  pVariableninProjektDialog = ^tVariableninProjektDialog;

  tVariableninProjektDialog = OBJECT (tMyDialog)

    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);



    Procedure HandleEvent(Var Event: TEvent); Virtual;

  END;



PROCEDURE VariableninProjektDialog(P: PVarList);



Implementation









CONSTRUCTOR tVariableninProjektDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);

  VAR

    R: tRect;

    View: pView;

BEGIN

  tMyDialog.Init (Bounds, aTitle);

  R.Assign (29, 3, 30, 9);

  View := New (pScrollBar, Init  (R)); 

  pScrollBar (View)^.SetRange (0, 0);

  Insert(View);

  R.Assign (5, 3, 29, 9);

  View := New (pVarListBox, Init (R, 1, pScrollBar (View)));

  Insert (View);

  R.Assign (5, 2, 17, 3);

  Insert (New (pMyLabel, Init (R, '~V~ariablen', View)));

 

  R.Assign (69, 3, 70, 9);

  View := New (pScrollBar, Init  (R)); 

  pScrollBar (View)^.SetRange (0, 0); 

  Insert(View);

  R.Assign (45, 3, 69, 9);

  View := New (pVarTypListBox, Init (R, 1, pScrollBar (View)));

  Insert (View);

  R.Assign (45, 2, 63, 3);

  Insert (New (pMyLabel, Init (R, 'Variablen - ~T~yp', View)));

 

  R.Assign (5, 12, 21, 14);

  View := New (pRadioButtons,

               Init (R, 

                     NewSItem ('Variablen',

                     NewSItem ('Var. - Typ',

                     NIL))));

  Insert (View);

  R.Assign (5, 11, 18, 12);



  View := New (pMyLabel, Init (R, '~B~earbeiten', View));



  If TypeOf(View^) <> TypeOf(TMyLabel) then Halt;



  Insert (View);

 

  R.Assign (45, 12, 58, 13);

  View := New (pCheckBoxes,

               Init (R, 

                     NewSItem ('Ordinal',

                     NIL)));

  Insert (View);

  R.Assign (45, 11, 51, 12);

  Insert (New (pMyLabel, Init (R, 'A~r~t', View)));



  R.Assign (26, 16, 50, 17);

  View := New (pMyInputLine, Init (R, 80));

  Insert (View);

  R.Assign (26, 15, 36, 16);

  Insert (New (pMyLabel, Init (R, '~E~ingabe', View)));

  R.Assign (50, 16, 53, 17);

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

END;



Procedure tVariableninProjektDialog.HandleEvent(Var Event: TEvent);

Var S: String;

    U: PMyList;

    E: PInputLine;

    R: PRadioButtons;

    L: PListBox;

    I: Integer;









Procedure NewVariableTyp(L: PListBox);

Var VarTypList: PVarTypList;

Begin

  If L <> Nil then

  Begin

    U := PMyList(L^.List);



    E := GetViewObject('Eingabe');



    If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

    Begin

      E^.GetData(S);



      If S = '' then Exit;



      VarTypList := New(PVarTypList, Init);

      VarTypList^.SetName(S);



      L^.List^.AtInsert(L^.Focused, VarTypList);



      L^.SetRange(U^.Count)

    End;

    Message(@Self, evBroadCast, cmItemFocused, L)

  End;

End;



Procedure NewVariable(L: PListBox);

Var VarObj : PVarObj;

Begin

    If L <> Nil then

    Begin

      U := PMyList(L^.List);



      If U = Nil then Exit;



      E := GetViewObject('Eingabe');



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

      Begin

        E^.GetData(S);



        If S = '' then Exit;



        VarObj := New(PVarObj, Init);

        VarObj^.SetName(S);



        If U = Nil then Halt;



        L^.List^.AtInsert(U^.Focused, VarObj);



        L^.SetRange(U^.Count)

      End;

    End;

End;













Procedure DeleteVariableTyp(L: PListBox);

Begin

  If L <> Nil then

  Begin

    Pointer(U) := L^.List;



    If U^.Count > 0 then

    Begin

      E := GetViewObject('Eingabe');



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

      Begin

        S := PMyList(U^.At(U^.Focused))^.GetName;



        L^.List^.AtFree(U^.Focused);



        If U^.Focused > U^.Count - 1 then U^.Focused := U^.Count - 1;



        L^.SetRange(U^.Count);



        If U^.Count = 0 then

        Begin

          E^.SetData(S);

          U^.Focused := 0;

        End

        Else

        Begin

          If L^.Focused <> U^.Focused then

          Begin

            L^.Focused := L^.Range - 1;

            U^.Focused := L^.Focused;

          End;



          E^.SetData(S);

        End;

      End;

      Message(@Self, evBroadCast, cmItemFocused, L)

    End

  End

End;



Procedure DeleteVariable(L: PListBox);

Begin

  If L <> Nil then

  Begin

    Pointer(U) := L^.List;



    If U = Nil Then Exit;



    If U^.Count > 0 then

    Begin

      E := GetViewObject('Eingabe');



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

      Begin

        S := PVarObj(U^.At(U^.Focused))^.GetName;



        L^.List^.AtFree(U^.Focused);



        If U^.Focused > U^.Count - 1 then U^.Focused := U^.Count - 1;



        L^.SetRange(U^.Count);



        If U^.Count = 0 then

        Begin

          E^.SetData(S);

          U^.Focused := 0;

        End

        Else

        Begin

          If L^.Focused <> U^.Focused then

          Begin

            L^.Focused := L^.Range - 1;

            U^.Focused := L^.Focused;

          End;



          E^.SetData(S);

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

    R := GetViewObject('Bearbeiten');



    If (R <> Nil) and (TypeOf(R^) = TypeOf(TRadioButtons)) then

    Begin

      Case R^.Value of

        0 : NewVariable(GetViewObject('Variablen'));

        1 : NewVariableTyp(GetViewObject('Variablen - Typ'));

      End

    End;

    ClearEvent(Event);

    ReDraw;

  End;



  If (Event.What = evCommand) and

     (Event.Command = cmVarDelete) then

  Begin

    R := GetViewObject('Bearbeiten');



    If (R <> Nil) and (TypeOf(R^) = TypeOf(TRadioButtons)) then

    Begin

      Case R^.Value of

        0 : DeleteVariable(GetViewObject('Variablen'));

        1 : DeleteVariableTyp(GetViewObject('Variablen - Typ'));

      End

    End;

    ClearEvent(Event);

    ReDraw;

  End;



  If (Event.What = evBroadCast) and

     (Event.Command = cmItemFocused) then

  Begin

    S := GetViewName(Event.InfoPtr);

    If S = 'Variablen - Typ' then

    Begin

      L := PListBox(GetViewObject('Variablen'));



      L^.List := Nil;



      I := PListBox(GetViewObject('Variablen - Typ'))^.Focused;



      U := Nil;



      If PListBox(GetViewObject('Variablen - Typ'))^.Range > 0 then



        U := PListBox(GetViewObject('Variablen - Typ'))^.List^.At(I);



      L^.NewList(U);

    End;

    ClearEvent(Event);

    ReDraw

  End;



End;



PROCEDURE VariableninProjektDialog(P: PVarList);

  VAR

    R: tRect;

    Data: VariableninProjektData;

    Code: INTEGER;

    Dialog: pDialog;

BEGIN

  R.Assign (0, 0, 80, 23);

  Dialog := New (pVariableninProjektDialog, Init (R, 'Variablen in Projekt'));

 

    { Datenrecord initialisieren ! }

  FillChar (Data, SizeOf (VariableninProjektData), 0);



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





constructor TVarListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);

Begin

  TMyListBox.Init(Bounds, ANumCols, AScrollBar)

End;



Function TVarListBox.GetText(Item: Integer; Len: Integer): String;

Begin

  GetText := PVarObj(List^.At(Item))^.GetName

End;



constructor TVarTypListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);

Begin

  TMyListBox.Init(Bounds, ANumCols, AScrollBar)

End;



Function TVarTypListBox.GetText(Item: Integer; Len: Integer): String;

Begin

  GetText := PVarTypList(List^.At(Item))^.GetName

End;



End.