Unit MyViews; (* Memo mit Antwort auf BroadCast, cmWhoIsMemo. *)



Interface



Uses Objects, Drivers, Views, Editors, Dialogs;



Const cmWhoIsMemo    = 201;

      cmWhoIsInput   = 202;

      cmWhoIsListBox = 203;

      cmWhoIsLabel   = 204;

      cmWhoIsView    = 205;



      cmItemFocused  = 210;

      cmInputEnter   = 211;

      cmMovedTo      = 212;

Type



   PMyLabel = ^TMyLabel;

   TMyLabel = Object(TLabel)

     Constructor Init(var Bounds: TRect; AText: String; ALink: PView);

     Procedure HandleEvent(Var Event: TEvent); Virtual;

   End;



   PMyMemo = ^TMyMemo;

   TMyMemo = Object (TMemo)

     Constructor Init (R: TRect; P1: Pointer;

                       AScrollBar: pScrollBar; P2: Pointer; I: Integer);

     Procedure HandleEvent(Var Event: TEvent); Virtual;

   End;





   PMyInputLine = ^TMyInputLine;

   TMyInputLine = Object (TInputLine)

     constructor Init(var Bounds: TRect; AMaxLen: Integer);

     Procedure HandleEvent(Var Event: TEvent); Virtual;

   End;



   PMyListBox = ^TMyListBox;

   TMyListBox = Object (TListBox)

     OldFocus: Integer;

     constructor Init(var Bounds: TRect;

                        ANumCols: Word;

                      AScrollBar: PScrollBar);



     Procedure HandleEvent(Var Event: TEvent); Virtual;



     Procedure FocusItem(Item: Integer); Virtual;

   End;



   PMyDialog = ^TMyDialog;

   TMyDialog = Object(TDialog)

     CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);





     Function GetViewObject(Name:String): Pointer;

     Function GetViewName(V: PView): String;

   End;



   PMyRadioButtons = ^TMyRadioButtons;

   TMyRadioButtons = Object(TRadioButtons)

     constructor Init(var Bounds: TRect; AStrings: PSItem);



     Procedure MovedTo(Item: Integer); Virtual;

   End;



Implementation



constructor TMyRadioButtons.Init(var Bounds: TRect; AStrings: PSItem);

Begin

  TRadioButtons.Init(Bounds, AStrings);

End;



Procedure TMyRadioButtons.MovedTo(Item: Integer);

Begin

  TRadioButtons.MovedTo(Item);

  Message(Owner, evBroadCast, cmMovedTo, @Self)

End;





Constructor TMyLabel.Init(var Bounds: TRect; AText: String; ALink: PView);

Begin

  TLabel.Init(Bounds, AText, ALink);



  EventMask := EventMask Or evBroadCast

End;



Procedure TMyLabel.HandleEvent(Var Event: TEvent);

Var S: String;

Begin

  TLabel.HandleEvent(Event);

  If (Event.What = evBroadCast)     and

     (Event.Command = cmWhoIsLabel) then

  Begin

    If TypeOf(Self) <> TypeOf(TMyLabel) then Halt;



    S := String(Text^);

    While Pos('~', S) <> 0 do System.Delete(S, Pos('~', S), 1);



    If S = String(Event.InfoPtr^) then

      ClearEvent(Event);

  End;



  If (Event.What = evBroadCast)     and

     (Event.Command = cmWhoIsView) then

  Begin

    If Link = Event.InfoPtr then

      ClearEvent(Event);

  End;

End;



Constructor TMyMemo.Init(R: TRect; P1: Pointer;

                        AScrollBar: pScrollBar; P2: Pointer; I: Integer);

Begin

  TMemo.Init(R, P1, AScrollBar, P2, I)

End;



Procedure TMyMemo.HandleEvent(Var Event: TEvent);

Begin

  TMemo.HandleEvent(Event);

  If (Event.What = evBroadCast) and

     (Event.Command = cmWhoIsMemo) then ClearEvent(Event);

End;



Constructor TMyInputLine.Init(var Bounds: TRect; AMaxLen: Integer);

Begin

  TInputLine.Init(Bounds, AMaxLen);

  EventMask := EventMask or evBroadCast;

End;



Procedure TMyInputLine.HandleEvent(Var Event: TEvent);

Begin

  If (Event.What = evBroadCast) and

     (Event.Command = cmWhoIsInput) then ClearEvent(Event);



  If (Event.What = evKeyBoard) and

     (Event.KeyCode = kbEnter) then

  Begin

    Message(Owner, evBroadCast, cmInputEnter, @Self);

    ClearEvent(Event)

  End;



  TInputLine.HandleEvent(Event);

End;



Constructor TMyListBox.Init(var Bounds: TRect;

                              ANumCols: Word;

                            AScrollBar: PScrollBar);

Begin

  TListBox.Init(Bounds, ANumCols, AScrollBar);

End;



Procedure TMyListBox.FocusItem(Item: Integer);

Var F: Integer;

Begin

  F := Focused;

  OldFocus := F;

  TListBox.FocusItem(Item);



  If F <> Item then

    Message(Owner, evBroadcast, cmItemFocused, @Self);

End;





Procedure TMyListBox.HandleEvent(Var Event: TEvent);

Begin

  TListBox.HandleEvent(Event);

  If (Event.What = evBroadCast) and

     (Event.Command = cmWhoIsListBox) then ClearEvent(Event);

End;





CONSTRUCTOR TMyDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);

Begin

  TDialog.Init(Bounds, aTitle)

End;



Function TMyDialog.GetViewObject(Name: String): Pointer;

Var P: Pointer;

    RR: PMyLabel;

    S: String;

Begin

  S := Name;

  P := @S;



  RR := Message(@Self, evBroadCast, cmWhoIsLabel, P);



  If (RR <> Nil) and (TypeOf(RR^) <> TypeOf(TMyLabel)) then Halt;



  If RR <> Nil Then

    GetViewObject := RR^.Link

  Else

    GetViewObject := Nil

End;



Function TMyDialog.GetViewName(V: PView): String;

Var RR: PMyLabel;

    T : String;

Begin

  RR := Message(@Self, evBroadCast, cmWhoIsView, V);



  If (RR <> Nil) and (TypeOf(RR^) <> TypeOf(TMyLabel)) then Halt;

  T := RR^.Text^;





  While Pos('~', T) <> 0 do System.Delete(T, Pos('~', T), 1);





  GetViewName := T

End;









End.
