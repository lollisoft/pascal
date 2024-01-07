Unit UInPro;  (* Unit's im Projekt (Editor) *)





Interface

Uses TVBUCMDS, App, Objects, Drivers, Dialogs, Views, MyViews, Var_Obj;













Type



  PUnitListBox = ^TUnitListBox;

  TUnitListBox = Object(TMyListBox)

    Function GetText(Item: Integer; Len: Integer): String; Virtual;

  End;





  UnitsinProjektData = RECORD

    ListPtr0: pCollection;

    Focused0: INTEGER;

    Cluster0: WORD;

    String0: STRING [80];

  END;



  pUnitsinProjektDialog = ^tUnitsinProjektDialog;

  tUnitsinProjektDialog = OBJECT (tDialog)

    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);



    Procedure HandleEvent(Var Event: TEvent); Virtual;

  END;





PROCEDURE UnitsinProjektDialog(P: PUnitList);



Implementation







CONSTRUCTOR tUnitsinProjektDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);

  VAR

    R: tRect;

    View: pView;

BEGIN

  tDialog.Init (Bounds, aTitle);

  R.Assign (29, 3, 30, 9);

  View := New (pScrollBar, Init  (R)); 

  pScrollBar (View)^.SetRange (0, 0); 

  Insert(View);

  R.Assign (5, 3, 29, 9);

  View := New (pUnitListBox, Init (R, 1, pScrollBar (View)));

  Insert (View);

  R.Assign (5, 2, 13, 3);

  Insert (New (pLabel, Init (R, '~U~nits', View)));

 

  R.Assign (45, 3, 67, 5);

  View := New (pCheckBoxes,

               Init (R, 

                     NewSItem ('~T~urbo - intern',

                     NewSItem ('~S~tandart',

                     NIL))));

  Insert (View);

  R.Assign (45, 2, 61, 3);

  Insert (New (pLabel, Init (R, '~E~igenschaften', View)));

 

  R.Assign (45, 8, 67, 9);

  View := New (pMyInputLine, Init (R, 80));

  Insert (View);

  R.Assign (45, 7, 52, 8);

  Insert (New (pLabel, Init (R, 'Un~i~t', View)));

  R.Assign (67, 8, 70, 9);

  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));

 

  R.Assign (4, 13, 13, 15);

  Insert (New (pButton, Init (R, '~N~eu', cmUnitNeu, 0)));



  R.Assign (15, 13, 30, 15);

  Insert (New (pButton, Init (R, ' ~L~îschen ', cmUnitDelete, 0)));

 

  R.Assign (44, 13, 52, 15);

  Insert (New (pButton, Init (R, '~O~K', 10, 1)));

 

  R.Assign (57, 13, 70, 15);

  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));

 

  SelectNext (FALSE);

END;





Procedure tUnitsinProjektDialog.HandleEvent(Var Event: TEvent);

Var L: PUnitListBox;

    U: PMyList;

    E: PMyInputLine;

    S: String;

    UO: PUnitObj;

Begin

  TDialog.HandleEvent(Event);



  If (Event.What = evCommand) and

     (Event.Command = cmUnitNeu) then

  Begin

    L := Message(@Self, evBroadCast, cmWhoIsListBox, Nil);



    If (L <> Nil) and (TypeOf(L^) = TypeOf(TUnitListBox)) then

    Begin

      Pointer(U) := L^.List;



      E := Message(@Self, evBroadCast, cmWhoIsInput, Nil);



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

      Begin

        E^.GetData(S);



        If S <> '' then

        Begin

          UO := New(PUnitObj, Init);

          UO^.SetName(S);



          L^.List^.AtInsert(U^.Focused, UO);



          L^.SetRange(U^.Count)

        End

      End;

      ClearEvent(Event);

      ReDraw

    End;



    ClearEvent(Event)

  End;



  If (Event.What = evCommand) and

     (Event.Command = cmUnitDelete) then

  Begin

    L := Message(@Self, evBroadCast, cmWhoIsListBox, Nil);



    If (L <> Nil) and (TypeOf(L^) = TypeOf(TUnitListBox)) then

    Begin

      Pointer(U) := L^.List;



      If U^.Count > 0 then

      Begin

        E := Message(@Self, evBroadCast, cmWhoIsInput, Nil);



        If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

        Begin

          S := PUnitObj(U^.At(U^.Focused))^.GetName;



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

        ClearEvent(Event);

        ReDraw

      End

    End;

    ClearEvent(Event)

  End;





  If (Event.What = evBroadCast) and

     (Event.Command = cmItemFocused) then

  Begin

    L := Event.InfoPtr;



    If TypeOf(L^) = TypeOf(TUnitListBox) then

    Begin

      Pointer(U) := L^.List;



      E := Message(@Self, evBroadCast, cmWhoIsInput, Nil);



      If (E <> Nil) and (TypeOf(E^) = TypeOf(TMyInputLine)) then

      Begin



        (* Sind mehrere Daten zu transportieren, dann sollten *)

        (* statt S zwei TUnitObjekte verwendet werden.        *)





        (* ZurÅckschreiben in alte Position: *)



        E^.GetData(S);

        If (S <> '') and

           (U^.Focused > - 1) then

          PUnitObj(U^.At(U^.Focused))^.SetName(S);



        U^.Focused := L^.Focused;



        S := PUnitObj(U^.At(U^.Focused))^.GetName;

        E^.SetData(S);

      End;

      ClearEvent(Event);

      ReDraw

    End;

  End;

End;



 

PROCEDURE UnitsinProjektDialog(P: PUnitList);

  VAR

    R: tRect;

    Data: UnitsinProjektData;

    Code: INTEGER;

    Dialog: pDialog;

BEGIN

  R.Assign (1, 4, 77, 21);

  Dialog := New (pUnitsinProjektDialog, Init (R, 'Units in Projekt'));

 

    { Datenrecord initialisieren ! }

  FillChar (Data, SizeOf (UnitsinProjektData), 0);



  Data.ListPtr0 := P;

  Data.Focused0  := P^.Focused;





  Dialog^.SetData (Data); 

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));

  IF Code <> cmCancel THEN BEGIN

    { cmCancel muss ev ersetzt werden }

    { Code auswerten }

    Dialog^.GetData (Data);





    P^.Focused := Data.Focused0;





    { Data muss ausgewertet werden ! }

  END;

  IF Dialog <> NIL THEN 

    Dispose (Dialog, Done);

END;





Function TUnitListBox.GetText(Item: Integer; Len: Integer): String;

Begin

  GetText := PUnitObj(List^.At(Item))^.GetName

End;

End.