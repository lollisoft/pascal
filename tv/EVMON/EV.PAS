{********************************************}
{                                            }
{      Event-Monitor zur šberwachung         }
{     von Ereignissen in Turbo Vision        }
{                                            }
{********************************************}

{$X+}

UNIT Ev;

INTERFACE

USES App, Drivers, Objects, Views, Dialogs, EvCol, ConstCol;

CONST
  cmNewEventString = 15001;

TYPE
  PEvMonApplication = ^TEvMonApplication;
  TEvMonApplication = OBJECT(TApplication)
    PROCEDURE GetEvent(VAR Event: TEvent); VIRTUAL;
    CONSTRUCTOR Init;
  END;

  PEventListBox = ^TEventListBox;
  TEventListBox = OBJECT(TListBox)
    FUNCTION GetText(Item: Integer; MaxLen: Integer): String; VIRTUAL;
    PROCEDURE FocusItem(Item: Integer); VIRTUAL;
  END;

  {
   dieser Typ wird nur definiert, um die CheckBox anders
   aussehen zu lassen - siehe Draw-Methode
  }
  PMyCheckBoxes = ^TMyCheckBoxes;
  TMyCheckBoxes = OBJECT(TCheckBoxes)
    PROCEDURE Draw; VIRTUAL;
  END;

  PEvMonitorWindow = ^TEvMonitorWindow;
  TEvMonitorWindow = OBJECT(TWindow)
    EvListBox: PEventListBox;
    Mask: PMyCheckBoxes;
    CONSTRUCTOR Init;
    FUNCTION GetPalette: PPalette; VIRTUAL;
    PROCEDURE InsertEvent(Event: TEvent);
    FUNCTION EventEnabled(V: Word; What: Word): Boolean;
  END;

  PEventDesc = ^TEventDesc;
  TEventDesc = OBJECT(TView)
    Desc: ARRAY[0..2] OF String[30];
    CONSTRUCTOR Init(VAR Bounds: TRect);
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

FUNCTION Message(Receiver: PView; What, Command: Word; InfoPtr: Pointer): Pointer;

CONST
  EvWindow: PEvMonitorWindow = NIL;

IMPLEMENTATION

FUNCTION Message(Receiver: PView; What, Command: Word; InfoPtr: Pointer): Pointer;
VAR
  E: TEvent;
BEGIN
  E.What := What;
  E.Command := Command;
  E.Infoptr := Infoptr;
  IF (EvWindow <> NIL) AND (EvWindow^.State AND sfSelected = 0) THEN
    EvWindow^.InsertEvent(E);
  Message:= Views.Message( Receiver, What, Command, InfoPtr);
END;

PROCEDURE TMyCheckBoxes.Draw;
CONST
  Button = ' [ ]';
BEGIN
  DrawBox(Button, #4);
END;

CONSTRUCTOR TEvMonitorWindow.Init;
VAR
  R, S: TRect;
  AListBox: PEventListBox;
  AScrollBar: PScrollBar;
  ACollection: PEventCollection;
  AEventMaskBox: PMyCheckBoxes;
  AEvDesc: PEventDesc;
BEGIN
  R.Assign(47,1,79,12);
  TWindow.Init(R,'Event-Monitor', wnNoNumber);
  Flags := Flags AND NOT (wfGrow OR wfZoom OR wfClose);
  State := State AND NOT sfShadow;
  EvWindow := @Self;
  ACollection := New(PEventCollection, Init(500));
  R.Assign(2,1,13,7);
  S.Assign(13,1,14,7);
  AScrollBar := New(PScrollBar, Init(S));
  AListBox := New(PEventListBox, Init(R, 1, AScrollBar));
  EvListBox := AListBox;
  Insert(AListBox);
  AListBox^.NewList(ACollection);
  Insert(AScrollBar);
  R.Assign(15, 1, 30, 7);
  AEventMaskBox :=
    New(PMyCheckBoxes, Init(R,
      NewSItem('MouseDown',
      NewSItem('MouseUp',
      NewSItem('MouseAuto',
      NewSItem('KeyDown',
      NewSItem('Command',
      NewSItem('Broadcast', NIL ))))))));
  Mask := AEventMaskBox;
  Insert(AEventMaskBox);
  R.Assign(3, 7, 30, 10);
  AEvDesc := New(PEventDesc, Init(R));
  Insert(AEvDesc);
END;

FUNCTION TEvMonitorWindow.EventEnabled(V: Word; What: Word): Boolean;
BEGIN
  EventEnabled := False;
  CASE What OF
    evMouseDown:
      IF V AND 1 <> 0 THEN EventEnabled := True;
    evMouseUp:
      IF V AND 2 <> 0 THEN EventEnabled := True;
    evMouseAuto:
      IF V AND 4 <> 0 THEN EventEnabled := True;
    evKeyDown:
      IF V AND 8 <> 0 THEN EventEnabled := True;
    evCommand:
      IF V AND 16 <> 0 THEN EventEnabled := True;
    evBroadcast:
      IF V AND 32 <> 0 THEN EventEnabled := True;
  END;
END;

PROCEDURE TEvMonitorWindow.InsertEvent(Event: TEvent);
VAR
  MyBounds: TRect;
BEGIN
  IF (State AND sfFocused <> 0) OR
     NOT EventEnabled(Mask^.Value, Event.What) THEN Exit;
  GetBounds(MyBounds);
  DeskTop^.MakeGlobal(MyBounds.A, MyBounds.A);
  DeskTop^.MakeGlobal(MyBounds.B, MyBounds.B);
  IF Event.What AND evMouse <> 0 THEN
    IF MyBounds.Contains(Event.Where) THEN Exit;
  PEventCollection(EvListBox^.List)^.InsertEvent(Event);
  EvListBox^.SetRange(EvListBox^.List^.Count);
  IF EvListBox^.List^.Count > EvListBox^.Size.Y THEN
    EvListBox^.VScrollBar^.SetValue(EvListBox^.List^.Count);
  IF EvListBox^.List^.Count > 0 THEN
    EvListBox^.FocusItem(EvListBox^.List^.Count-1);
  EvListBox^.DrawView;
END;

FUNCTION TEvMonitorWindow.GetPalette: PPalette;
CONST
  CEvWindow = CDialog;
  P: String[Length(CEvWindow)] = CEvWindow;
BEGIN
  GetPalette := @P;
END;

CONSTRUCTOR TEvMonApplication.Init;
BEGIN
  TApplication.Init;
  DeskTop^.Insert(New(PEvMonitorWindow, Init));
END;

PROCEDURE TEvMonApplication.GetEvent(VAR Event: TEvent);
BEGIN
  TApplication.GetEvent(Event);
  IF (Event.What <> evNothing) AND (Event.What <> evMouseMove) THEN
    IF EvWindow <> NIL THEN
      EvWindow^.InsertEvent(Event);
END;

PROCEDURE TEventListBox.FocusItem(Item: Integer);
TYPE
  PWord = ^Word;
VAR
  S, Sym: String;
  L: ARRAY[0..2] OF LongInt;
  StrArray: ARRAY[0..2] OF String[30];
  E: TEvent;
  ConstantToSearchFor: Word;
  W: PConstRec;
  VmtLink: String[6];

FUNCTION IsThisNumber(P: PConstRec): Boolean; FAR;
BEGIN
  IsThisNumber := ConstantToSearchFor = P^.Value;
END;

BEGIN
  TListBox.FocusItem(Item);
  StrArray[0] := '';
  StrArray[1] := '';
  E := PEvent(List^.At(Item))^;
  CASE E.What OF
    evMouseUp,
    evMouseDown,
    evMouseAuto:
      BEGIN
        L[0] := LongInt(E.Where.X);
        L[1] := LongInt(E.Where.Y);
        FormatStr(S, 'X:%d  Y:%d',L);
        StrArray[0] := '';
        StrArray[1] := S;
        StrArray[2] := '';
      END;
    evKeyDown:
      BEGIN
        ConstantToSearchFor := E.KeyCode;
        W := ConstCollection^.FirstThat(@IsThisNumber);
        IF W <> NIL THEN Sym := W^.Key
        ELSE Sym := ''''+E.CharCode+'''';
        L[0] := LongInt(E.KeyCode);
        L[1] := LongInt(E.KeyCode);
        FormatStr(S, 'Key: $%x(%d)',L);
        StrArray[0] := S;
        L[0] := LongInt(@Sym);
        FormatStr(S, 'Char: %s',L);
        StrArray[1] := S;
        StrArray[2] := '';
      END;
    evCommand,
    evBroadcast:
      BEGIN
        ConstantToSearchFor := E.KeyCode;
        W := ConstCollection^.FirstThat(@IsThisNumber);
        IF W <> NIL THEN Sym := W^.Key
        ELSE Sym := '';
        L[0] := LongInt(E.Command);
        L[1] := LongInt(E.Command);
        FormatStr(S, 'Event.Command: $%x(%d)',L);
        StrArray[0] := S;
        IF E.InfoPtr <> NIL THEN BEGIN
          L[0] := LongInt(PWord(E.InfoPtr)^);
          FormatStr(S, 'VmtLink: %d',L);
        END ELSE S := '';
        StrArray[1] := Sym;
        StrArray[2] := S;
      END;
  END;
  Views.Message(Owner, evBroadcast, cmNewEventString, @StrArray);
END;

FUNCTION TEventListBox.GetText(Item: Integer; MaxLen: Integer): String;

FUNCTION EvDescription(I: Integer): String;
CONST
  EvDescStr: ARRAY[1..10] OF String[10] =
        ('MouseDown',
         'MouseUp',
         'MouseMove',
         'MouseAuto',
         'KeyDown',
         '','', '',
         'Command',
         'Broadcast');
VAR
  K: Integer;
  W: Word;
BEGIN
  K := 1;
  W :=PEvent(List^.At(I))^.What;
  WHILE NOT Odd(W) DO BEGIN
    W := W SHR 1;
    Inc(K);
  END;
  EvDescription := EvDescStr[K];
END;

VAR
  RealIndex: Integer;
BEGIN
  GetText := EvDescription(Item)
END;

CONSTRUCTOR TEventDesc.Init(VAR Bounds: TRect);
BEGIN
  TView.Init(Bounds);
  EventMask := EventMask OR evBroadcast;
END;

PROCEDURE TEventDesc.HandleEvent(VAR Event: TEvent);
TYPE
  PStrArrayType = ^TStrArrayType;
  TStrArrayType = ARRAY[0..2] OF String[30];
BEGIN
  TView.HandleEvent(Event);
  IF (Event.What = evBroadcast) AND (Event.Command = cmNewEventString) THEN
  BEGIN
    Desc[0] := PStrArrayType(Event.InfoPtr)^[0];
    Desc[1] := PStrArrayType(Event.InfoPtr)^[1];
    Desc[2] := PStrArrayType(Event.InfoPtr)^[2];
    DrawView;
  END;
END;

PROCEDURE TEventDesc.Draw;
VAR
  B: TDrawBuffer;
  C: Byte;
BEGIN
  C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, Desc[0], C);
  WriteLine(0, 0, Size.X, 1, B);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, Desc[1], C);
  WriteLine(0, 1, Size.X, 1, B);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, Desc[2], C);
  WriteLine(0, 2, Size.X, 1, B);
END;

END.