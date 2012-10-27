PROGRAM DateInputLine;

USES (* Turbo Pascal Standardlib *)
     DOS,
     (* Turbo VISION Stuff *)
     App, Dialogs, Drivers, Memory, MsgBox, Menus, Objects, StdDlg, Views,
     (* anything else *)
     MyDate, XDate;

CONST
  cmBase          = 2900;
  cmNothing       = cmBase + 1;
  cmAbout         = cmBase + 2;
  cmOpenCalWindow = cmBase + 3;
  brValidDate     = cmBase + 4;

  LineCount       = 7;
  LineLength      = 20;
  InitValue       : TPoint = (x:6; y:6);

TYPE
  PPoint      = ^TPoint;
  TCalString  = String[LineLength];

  PMonth      = ^TMonth;
  TMonth      = OBJECT (TScroller)
     Datum: TXDate;
     CONSTRUCTOR Init (VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
                       AStartDate: TXDate);
     PROCEDURE Draw; VIRTUAL;
     FUNCTION DataSize: WORD; VIRTUAL;
     PROCEDURE GetData (VAR Rec); VIRTUAL;
     PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
     FUNCTION EvaluateDate (Position: TPoint; VAR ResDay: WORD): BOOLEAN;
  END;

  PLittleCal  = ^TLittleCal;
  TLittleCal  = OBJECT (TWindow)
     CONSTRUCTOR Init (VAR Bounds: TRect; AStartDate: TXDate);
     PROCEDURE SizeLimits (VAR Min, Max : TPoint); VIRTUAL;
     PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
  END;

  PDateInputLine = ^TDateInputLine;
  TDateInputLine = OBJECT (TInputLine)
    Datum: TXDate;
    CONSTRUCTOR Init (VAR Bounds: TRect; ADate: TXDate);
    PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
  END;

  PTestDialog = ^TTestDialog;
  TTestDialog = OBJECT (TDialog)
    CONSTRUCTOR Init (VAR Bounds: TRect; ATitle: String);
  END;

  TCalApp     = OBJECT (TApplication)
     CONSTRUCTOR Init;
     PROCEDURE InitStatusLine; VIRTUAL;
     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
  END;

VAR
  MyApp       : TCalApp;
  Lines       : ARRAY [0..LineCount] OF TCalString;

(* ========================== anything else ============================== *)

FUNCTION ConvertDate (t : TXDate): String;
CONST
  Delim : String = '.';
VAR
  d,m,y : String;
BEGIN
  Str (t.GetDay:2, d);
  Str (t.GetMonth:2, m);
  Str (t.GetYear:4, y);
  ConvertDate := d + DeLim + m + DeLim + y;
END;

(* ============================== TMonth ================================= *)

CONSTRUCTOR TMonth.Init (VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
                         AStartDate: TXDate);
BEGIN
  TScroller.Init (Bounds, AHScrollBar, AVScrollBar);
  ScrollDraw;
  Datum.Init (AStartDate.GetDay, AStartDate.GetMonth, AStartDate.GetYear);
END;

PROCEDURE TMonth.Draw;
VAR
  Offset, Loop, Counter : INTEGER;
  AWeek, ADay, Equals   : String;
  Color : BYTE;
  B : TDrawBuffer;
  t : TXDate;
BEGIN
  HideCursor;
  t.Init (01, Datum.GetMonth, Datum.GetYear);
  Str (t.GetYear, AWeek);
  Offset := (14 - Length(CDMonthNames[t.GetMonth])) DIV 2;
  Equals := '';
  FOR Loop := 1 TO Offset DO
    Equals := Equals + #205;

  FOR Loop := 0 TO LineCount DO
    Lines[Loop] := '                    ';

  Counter := 0;
  Lines[Counter] := Equals + ' ' + CDMonthNames[t.GetMonth] + ' ' + AWeek + ' ' + Equals;
  INC(Counter);

  Lines[Counter] := 'Mo Di Mi Do Fr Sa So';
  INC(Counter);

  Offset := INTEGER (t.GetWeekDay) * 3;

  (* Generate a month *)
  REPEAT
    (* Arbeitsstring leeren *)
    AWeek := '';
    (* entsprechend dem Wochentag des 1. einrÅcken *)
    FOR Loop := 1 TO Offset DO
      AWeek := AWeek + ' ';

    REPEAT
      Str (t.GetDay:2,ADay);
      AWeek := AWeek + ADay + ' ';
      t.INCDay;
    UNTIL ((t.GetWeekDay = 0) OR (t.GetDay = 1));
    Lines[Counter] := AWeek;
    INC(Counter);
    Offset := 0;
  UNTIL (t.GetDay = 1);

  Color := GetColor ($01);
  FOR Counter := 0 TO LineCount DO
  BEGIN
    MoveChar (B, ' ', BYTE(Color), LineLength);
    MoveStr (B, Lines[Counter], BYTE(Color));
    WriteLine (0, Counter, LineLength, 1, B);
  END;
END (* TMonth.Draw *);

FUNCTION TMonth.EvaluateDate (Position : TPoint; VAR ResDay : WORD): BOOLEAN;
VAR
  FirstWeek, Row, WeekDay : INTEGER;
  Result : BOOLEAN;
  MyDatum: TXDate;
BEGIN
  Result := FALSE;
  MyDatum.Init (01, Datum.GetMonth, Datum.GetYear);
  Row := Position.Y - 2;
  WeekDay := Position.X DIV 3;
  IF (((Position.X MOD 3) = 2) OR ((Row = 0) AND (WeekDay < MyDatum.GetWeekday)))
  THEN Result := FALSE
  ELSE BEGIN
    ResDay := (Row - 1) * 7 + (7 - MyDatum.GetWeekDay) + WeekDay + 1;
    Result := TRUE;
    IF (ResDay > MyDatum.GetMonthLength) THEN Result := FALSE;
  END;
  IF (NOT Result) THEN ResDay := 1;
  EvaluateDate := Result;
END (* TMonth.EvaluateDate *);

FUNCTION TMonth.DataSize: WORD;
BEGIN
  DataSize := SizeOf (TXDate);
END;

PROCEDURE TMonth.GetData (VAR Rec);
BEGIN
  TXDate(Rec) := Datum;
END;

PROCEDURE TMonth.HandleEvent (VAR Event : TEvent);
VAR
  P        : TPoint;
  ClickDate: WORD;
  DatePtr  : PXDate;
  NewEvent : TEvent;
  NewValue, Change : INTEGER;
  ValidDate : BOOLEAN;
BEGIN
  TScroller.HandleEvent (Event);
  IF ((Event.What = evBroadcast) AND (Event.Command = cmScrollBarChanged)) THEN
  BEGIN
    IF (Event.InfoPtr = HScrollBar) THEN
    BEGIN
      NewValue := PScrollBar(Event.InfoPtr)^.Value;
      Change := NewValue - InitValue.X ;
      Datum.Init (Datum.GetDay, Datum.GetMonth, Datum.GetYear + Change);
      PScrollBar(Event.InfoPtr)^.SetValue (InitValue.X);
      ClearEvent(Event);
    END;
    IF (Event.InfoPtr = VScrollBar) THEN
    BEGIN
      NewValue := PScrollBar(Event.InfoPtr)^.Value;
      Change := NewValue - InitValue.Y;
      Datum.ChangeMonth (Change);
      PScrollBar(Event.InfoPtr)^.SetValue (InitValue.Y);
      ClearEvent (Event);
    END;
  END;

  IF ((Event.What = evMouseDown) AND Event.Double) THEN
  BEGIN
    MakeLocal (Event.Where, P);
    IF (P.Y > 1) THEN ValidDate := EvaluateDate (P, ClickDate);
    IF ValidDate THEN BEGIN
      NewEvent.What := evBroadcast;
      NewEvent.Command := brValidDate;
      Datum.Init (ClickDate, Datum.GetMonth, Datum.GetYear);
      PutEvent (NewEvent);
    END ELSE Write(#7);
  END;
END (* TMonth.HandleEvent *);

(* ============================ TLittleCal =============================== *)

CONSTRUCTOR TLittleCal.Init (VAR Bounds: TRect; AStartDate: TXDate);
VAR
  Scroller : PMonth;
  VScrollBar, HScrollBar : PScrollBar;
  R : TRect;
BEGIN
  R.Assign (20,6,44,18);               (* assigning size of window to open   *)
  TWindow.Init (R, 'TDateInputLine',0);

  GetExtent (R);
  R.Assign (R.B.X-1, R.A.Y+1, R.B.X, R.B.Y-1);
  New (HScrollBar, Init(R));
  HScrollBar^.Options := HScrollBar^.Options OR ofPostProcess;
  HScrollBar^.SetParams (InitValue.X, 0, 2*InitValue.X, 1, 1);
  Insert (HScrollBar);

  GetExtent (R);
  R.Assign (R.A.X+3, R.B.Y-1, R.B.X-2, R.B.Y);
  New (VScrollBar, Init(R));
  VScrollBar^.Options := VScrollBar^.Options OR ofPostProcess;
  VScrollBar^.SetParams (InitValue.Y, 0, 2*InitValue.Y, 1, 1);
  Insert (VScrollBar);

  GetExtent (R);
  R.Assign (R.A.X + 2, R.A.Y + 2, R.B.X - 2, R.B.Y - 2);
  Scroller := New (PMonth, Init(R, VScrollBar, HScrollBar, AStartDate));
  Scroller^.Options := Scroller^.Options OR ofFramed;
  Insert (Scroller);
END (* TLittleCal.Init *);

PROCEDURE TLittleCal.SizeLimits (VAR Min, Max : TPoint);
CONST
  CalMin : TPoint = (X: 24; Y: 12);
  CalMax : TPoint = (X: 24; Y: 12);
BEGIN
  Min := CalMin;
  Max := CalMax;
END (* TLittleCal.SizeLimits *);

PROCEDURE TLittleCal.HandleEvent (VAR Event: TEvent);
BEGIN
  TWindow.HandleEvent (Event);
  IF ((Event.What = evCommand) AND (Event.Command = cmCancel)) THEN
  BEGIN
    EndModal (Event.Command);
    ClearEvent (Event);
  END;
  IF ((Event.What = evBroadcast) AND (Event.Command = brValidDate)) THEN
  BEGIN
    EndModal (Event.Command);
  END;
END;

(* ====================== TDateInputLine ================================ *)

CONSTRUCTOR TDateInputLine.Init (VAR Bounds: TRect; ADate: TXDate);
VAR
  s : String;
BEGIN
  TInputLine.Init (Bounds, 10);
  Datum.Init (ADate.GetDay, ADate.GetMonth, ADate.GetYear);
  s := ConvertDate (ADate);
  TDateInputLine.SetData (s);
END;

PROCEDURE TDateInputLine.HandleEvent (VAR Event: TEvent);
VAR
  R : TRect;
  Command : WORD;
  CalWindow : PLittleCal;
  s : String;
BEGIN
  IF ((Event.What = evMouseDown) AND (Event.Buttons = mbRightButton)) THEN
  BEGIN
    R.Assign (20, 10, 44, 21);
    CalWindow := New (PLittleCal, Init(R, Datum));
    Command := DeskTop^.ExecView (CalWindow);
    IF (Command = brValidDate) THEN
    BEGIN
      CalWindow^.GetData (Datum);
      s := ConvertDate (Datum);
      TDateInputLine.SetData (s);
    END;
    IF (Command = cmCancel) THEN
    BEGIN

    END;
    Dispose (CalWindow, Done);
    ClearEvent(Event);
  END ELSE TInputLine.HandleEvent (Event);
END;

(* ======================== TTestDialog ================================= *)

CONSTRUCTOR TTestDialog.Init (VAR Bounds: TRect; ATitle: String);
VAR
  Control : PView;
  DateInput: PDateInputLine;
  R : TRect;
  t : TXDate;
BEGIN
  TDialog.Init (Bounds, ATitle);
  GetExtent (R);
  R.Assign (8, 3, 25, 4);
  t.MakeMyDay;
  DateInput := New (PDateInputLine, Init(R, t));
  Insert (DateInput);
  R.Assign (8, 2, 25, 3);
  Control := New (PLabel, Init(R, 'Datum eingeben :', DateInput));
  Insert (Control);
  SelectNext (False);
END;

(* ========================== TCalApp =================================== *)

CONSTRUCTOR TCalApp.Init;
CONST
  Title : String = 'Eingabezeile fÅr Daten';
VAR
  MyDialog : PTestDialog;
  R : TRect;
  Control: WORD;
BEGIN
  TApplication.Init;
  (* room for further initialization *)

  R.Assign (25,10,60,16);

  MyDialog := New (PTestDialog, Init(R, 'Eingabezeile fÅr Daten'));
  Control := DeskTop^.ExecView (MyDialog);

  EventMask := EventMask OR evBroadcast;
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options  := Options XOR OfFramed;
END (* TCalApp.Init *);

PROCEDURE TCalApp.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R, NewStatusDef (0, $FFFF,
                   NewStatusKey ('~Alt-X~ zum Beenden', kbAltX, cmQuit,
                   nil),
                nil)));
END (* TCalApp.InitStatusLine *);

PROCEDURE TCalApp.HandleEvent (VAR Event : TEvent);
VAR
  C        : WORD;
  PPtr     : PPoint;
  Dialog   : PDialog;
  R        : TRect;
  s,t      : String;
  PleaseSave : WORD;
BEGIN
  TApplication.HandleEvent (Event);
  IF (Event.What = evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmNothing: ;
      ELSE EXIT;
    END;
  END;
  IF (Event.What = evBroadcast) THEN
  BEGIN
    CASE Event.Command OF
      brValidDate : Write(#7);
    END;
  END;
  ClearEvent (Event);
END (* TCalApp.HandleEvent *);

{=========================================================================}

BEGIN
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
END (* DateInputLine *).
