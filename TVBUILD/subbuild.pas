Unit SubBuild; (* Subroutinen Editor *)


Interface
Uses App,
     Dialogs,
     Views,
     Drivers,
     strings,
     Editor,
     Editors,
     TVBUCMDS,
     MsgBox,
     MyViews,

     Ab_Obj,

     Objects;

  TYPE
    EditiereSubroutinenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      Cluster0: WORD;
      String0: STRING [80];
      String1: STRING [80];
      String2: STRING [80];
    END;
 
  pProcedureListBox = ^TProcedureListBox;
  TProcedureListBox = Object(TMyListBox)
    constructor Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
    Function GetText(Item: Integer; Len: Integer): String; Virtual;
  End;


  pEditiereSubroutinenDialog = ^tEditiereSubroutinenDialog;
  tEditiereSubroutinenDialog = OBJECT (tMyDialog)
    IfFunc: Boolean;


    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Procedure HandleEvent(Var Event: TEvent); Virtual;
  END;


PROCEDURE EditiereSubroutinenDialog(P: PProcedureList);

Implementation

CONSTRUCTOR tEditiereSubroutinenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tMyDialog.Init (Bounds, aTitle);
  IfFunc := False;
  R.Assign (54, 3, 55, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  Insert (View);
  R.Assign (3, 3, 54, 10);
  View := New (pProcedureListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (3, 2, 14, 3);
  Insert (New (pMyLabel, Init (R, '~R~outinen', View)));
 
  R.Assign (58, 3, 74, 5);
  View := New (pMyRadioButtons,
               Init (R, 
                     NewSItem ('~P~rozedur',
                     NewSItem ('~F~unktion',
                     NIL))));
  Insert (View);
  R.Assign (58, 2, 64, 3);
  Insert (New (pMyLabel, Init (R, '~A~rt', View)));
 
  R.Assign (57, 7, 77, 9);
  Insert (New (pButton, Init (R, '~Z~urÅck aus Sub', cmZurueckAusSub, 0)));

  R.Assign (57, 9, 77, 11);
  Insert (New (pButton, Init (R, '~E~dit Block', cmEditBlock, 0)));
 
  R.Assign (3, 12, 54, 13);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (3, 11, 13, 12);
  Insert (New (pMyLabel, Init (R, 'Ro~u~tine', View)));
  R.Assign (54, 12, 57, 13);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (30, 14, 57, 16);
  Insert (New (pButton, Init (R, '~P~arameter der Routine', 0, 0)));
 
  R.Assign (64, 14, 77, 16);
  Insert (New (pButton, Init (R, '~H~ilfe', 9, 0)));
 
  R.Assign (3, 17, 21, 18);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (3, 16, 21, 17);
  Insert (New (pMyLabel, Init (R, '~F~unktions - Typ', View)));
  R.Assign (21, 17, 24, 18);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (33, 17, 57, 19);
  Insert (New (pButton, Init (R, 'T~y~pen der Funktion', 0, 0)));
 
  R.Assign (2, 20, 11, 22);
  Insert (New (pButton, Init (R, '~N~eu', cmNeu, 0)));
 
  R.Assign (12, 20, 25, 22);
  Insert (New (pButton, Init (R, '~L~îschen', cmDelete, 0)));
 
  R.Assign (42, 20, 57, 22);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (64, 20, 77, 22);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);
END;


PROCEDURE EditiereSubroutinenDialog(P: PProcedureList);
  VAR
    R: tRect;
    Data: EditiereSubroutinenData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pEditiereSubroutinenDialog, Init (R, 'Editiere Subroutinen'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (EditiereSubroutinenData), 0);

  Data.ListPtr0 := P;


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


Procedure tEditiereSubroutinenDialog.HandleEvent(Var Event: TEvent);
Var L: PListBox;
    P: PProcedureObj;
    S: String;
    I: PInputLine;
    A: Integer;
    R: PMyRadioButtons;
    EdMessage: MyEditMessageRecord;

Begin
  tMyDialog.HandleEvent(Event);


(*  If (Event.What = evBroadCast) and
     (Event.Command = cmMovedTo) then
  Begin
    If PMyRadioButtons(Event.InfoPtr)^.Value = 0 then
      IfFunc := False
    Else
      IfFunc := True
  End;
*)

  If (Event.What = evBroadCast) and
     (Event.Command = cmEdTextDoubleClicked) then
  Begin
    EdMessage := MyEditMessageRecord(Event.InfoPtr^);

    Pointer(L) := EdMessage.View;

    S := '';

    For A := 1 to EdMessage.Spalte do S := S + ' ';

    S := S + '^';


    If TypeOf(L^) = TypeOf(TMyMemoEdit) then
    Begin
      ClearEvent(Event);
      Wait('Text doppeld Selektiert' + Chr(13) +
           PStr(EdMessage.Coll^.At(EdMessage.Zeile))^.GetStr + Chr(13) +
           S);
    End
  End;



  (* Procedure/Function - List Selected *)

  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) then
  Begin  (* Editiere Selektierte Routine *)
    L := Event.InfoPtr;
    PProcedureObj(L^.List^.At(L^.Focused))^.AMainCode^.Edit;
    ClearEvent(Event)
  End;

  If (Event.What = evBroadCast) and
     (Event.Command = cmItemFocused) then
  Begin
    L := Event.InfoPtr;

    P := PProcedureObj(L^.List^.At(L^.Focused));

    S := P^.GetProcedureName;
    PInputLine(GetViewObject('Routine'))^.SetData(S);

    S := P^.GetProcedureTyp;
    PInputLine(GetViewObject('Funktions - Typ'))^.SetData(S);


    ClearEvent(Event);
  End;


  If (Event.What = evCommand) and
     (Event.Command = cmNeu) then
  Begin
    R := GetViewObject('Art');

    Case R^.Value of

    1     : Begin
              P := New(PProcedureObj, Init);

              I := PInputLine(GetViewObject('Routine'));
              I^.GetData(S);
              P^.SetProcedureName(S);

              I := PInputLine(GetViewObject('Funktions - Typ'));
              I^.GetData(S);

              If S = '' then
              Begin
                ClearEvent(Event);
                Dispose(P, Done);
                Exit
              End;

              P^.SetProcedureTyp(S);

              L := GetViewObject('Routinen');
            End;
    0     : Begin
              P := New(PProcedureObj, Init);

              I := PInputLine(GetViewObject('Routine'));
              I^.GetData(S);
              P^.SetProcedureName(S);

              L := GetViewObject('Routinen');
            End;
    End;
    L^.List^.Insert(P);

    L^.SetRange(L^.List^.Count);
    ClearEvent(Event);
    ReDraw
  End;

  If (Event.What = evCommand) and
     (Event.Command = cmDelete) then
  Begin
    L := GetViewObject('Routinen');
    Pointer(P) := L^.List^.At(L^.Focused);

    L^.List^.AtDelete(L^.Focused);
    L^.SetRange(L^.List^.Count);

    I := GetViewObject('Routine');
    S := P^.GetProcedureName;
    I^.SetData(S);

    I := GetViewObject('Funktions - Typ');
    S := P^.GetProcedureTyp;
    I^.SetData(S);

    Dispose(P, Done);

    ClearEvent(Event);
    ReDraw
  End;
End;


constructor TProcedureListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
Begin
  TMyListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Function TProcedureListBox.GetText(Item: Integer; Len: Integer): String;
Begin
  GetText := PProcedureObj(List^.At(Item))^.GetProcedureName;
End;

End.