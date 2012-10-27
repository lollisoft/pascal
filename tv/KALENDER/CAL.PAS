PROGRAM Calendar;

{$S+}
{$M 8192,0,500000}

USES (* Turbo Pascal Standardlib *)
     DOS,
     (* Turbo VISION Stuff *)
     App, Dialogs, Drivers, Memory, MsgBox, Menus, Objects, StdDlg, Views,
     (* Ralph's beautiful new editor object *)
     Editor, Strings,
     (* anything else *)
     MyDate;

CONST
  CalendarBase          = 4000;
  EdBroadcastBase       = 5000;
  Extension             = '.MMO';
  (* Commands occuring during program run *)
  cmAbout               = CalendarBase + 1;
  cmOpenCalWindow       = CalendarBase + 2;
  brOpenMemo            = CalendarBase + 3;
  brNoValidDate         = CalendarBase + 4;
  brEditorClosed        = CalendarBase + 5;

  CalendarWindowNo      = 0;
  MemoEditorWindowNo    = 1;

  (* other constants *)
  MaxLines              = 512;
  LineCount             : INTEGER = 500;

TYPE
  PPoint       = ^TPoint;

  PMyString   = ^TMyString;
  TMyString   = String[28];

  PCalendar   = ^TCalendar;
  TCalendar   = OBJECT (TScroller)
                   CONSTRUCTOR Init (VAR Bounds  : TRect;
                                     AVScrollBar : PScrollBar);
                   PROCEDURE Draw; VIRTUAL;
                   PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
                END;

  PCalWindow  = ^TCalWindow;
  TCalWindow  = OBJECT (TWindow)
                   CONSTRUCTOR Init;
                   PROCEDURE SizeLimits (VAR Min, Max : TPoint); VIRTUAL;
                END;

  TCalApp     = OBJECT (TApplication)
                     CONSTRUCTOR Init;
                     PROCEDURE InitStatusLine; VIRTUAL;
                     PROCEDURE FillCalLines;
                     PROCEDURE OpenCalWindow;
                     PROCEDURE OpenMemoWindow;
                     PROCEDURE SaveMemo (P : PCollection);
                     PROCEDURE LoadMemo;
                     PROCEDURE HandleEvent (VAR Event : TEvent); VIRTUAL;
                   PRIVATE
                     EditBuffer : PCollection;
                     MemoDate   : TDate;
                     PROCEDURE EvaluateDate (Column, Line : WORD);
                     FUNCTION  LoadFileCollect (VAR Name : PathStr): PCollection;
                END;

VAR
  MyApp       : TCalApp;
  Lines       : ARRAY [0..MaxLines-1] OF PMyString;
  Today       : TDate;

{=========================================================================}
{ Hilfsroutinen }

FUNCTION NewMyStr(S: String): PMyString;
VAR
  P: PMyString;
BEGIN
  IF S = '' THEN P := nil ELSE
  BEGIN
    GetMem(P, Length(S) + 1);
    P^ := S;
  END;
  NewMyStr := P;
END;

PROCEDURE DisposeMyStr(P: PMyString);
BEGIN
  if P <> nil then FreeMem(P, Length(P^) + 1);
END;

FUNCTION FileExist (VAR FileName : PathStr): BOOLEAN;
VAR
  TextFile : Text;
BEGIN
  Assign (TextFile, FileName);
  {$I-}
  Reset (TextFile);
  {$I+}
  IF (IOResult <> 0)
    THEN FileExist := FALSE
    ELSE BEGIN
           FileExist := TRUE;
           Close (TextFile);
         END;
END;

FUNCTION BuildFileName (VAR Date : TDate): String;
VAR
  TmpDate : LONGINT;
  TmpStr  : String;
BEGIN
  TmpDate := LONGINT(10000) * (Date.GetYear - 1900) + (Date.GetMonth * 100) + Date.GetDay;
  Str (TmpDate:6, TmpStr);
  BuildFileName := Concat(TmpStr, Extension);
END;

{=========================================================================}
{ TCalendar }

CONSTRUCTOR TCalendar.Init (VAR Bounds  : TRect;
                            AVScrollBar : PScrollBar);
BEGIN
  TScroller.Init (Bounds, NIL, AVScrollBar);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options  := Options XOR OfFramed;
END (* TCalendar.Init *);

PROCEDURE TCalendar.Draw;
VAR
  Color : BYTE;
  Y, I  : INTEGER;
  B     : TDrawBuffer;
BEGIN
  Color := GetColor ($02);
  FOR Y := 0 TO Size.Y -1 DO
  BEGIN
    MoveChar (B, ' ', BYTE(Color), Size.X);
    I := Delta.Y + Y;
    IF (I < LineCount) AND (Lines[I] <> NIL) THEN
      MoveStr (B, Copy(Lines[I]^, Delta.X + 1, Size.X), BYTE(Color));
    WriteLine (0, Y, Size.X, 1, B);
  END;
END;

PROCEDURE TCalendar.HandleEvent (VAR Event : TEvent);
VAR
  P        : TPoint;
  PPtr     : PPoint;
  ch       : CHAR;
  NewEvent : TEvent;
  VScroll  : PScrollBar;
BEGIN
  TScroller.HandleEvent (Event);
  IF (Event.What = evMouseDown) THEN               (* if mouse was pressed *)
  BEGIN
    NewEvent.What := evBroadcast;
    P := Event.Where;                 (* where did the mouse click occur ? *)
    INC (P.Y, Delta.Y);               (* add already scrolled lines        *)

    (* MakeLocal : return row and column position in the calendar window   *)
    DEC (P.X,2);
    DEC (P.Y,3);

    (* Check if it is a valid date the mouse cursor is standing on *)
    IF ((Lines[P.Y]^[3] IN [' ','0'..'9']) AND (Lines[P.Y]^[P.X+1] IN ['0'..'9']))
      THEN NewEvent.Command := brOpenMemo
      ELSE NewEvent.Command := brNoValidDate;

    PPtr^ := P;
    NewEvent.InfoPtr := PPtr;
    TopView^.HandleEvent (NewEvent);       (* send out event to be handled *)
    ClearEvent (Event);     (* clear old event, so no other will handle it *)
  END;
END (* TCalendar.HandleEvent *);

{=========================================================================}
{ TCalWindow }

CONSTRUCTOR TCalWindow.Init;
VAR
  Scroller          : PScroller;
  VScrollBar        : PScrollBar;
  VWeekDays, VToday : PStaticText;
  R                 : TRect;
  DayStr, YearStr   : String[5];
  TodaysDate        : String;
  Min, Max          : TPoint;
BEGIN
  Today.MakeMyDay;                     (* initialize with current date       *)
  R.Assign (0,0,32,23);                (* assigning size of window to open   *)
  Str (Today.GetYear , YearStr);       (* convert numbers into strings       *)
  TWindow.Init (R, 'VISIONÑrer Kalender',CalendarWindowNo);

  GetExtent (R);
  R.Assign (R.A.X+2, R.B.Y-2, R.A.X+29, R.B.Y-1);
  Str (Today.GetDay, DayStr);
  DayStr := ' ' + DayStr;
  TodaysDate := 'Heute: ' + CDDayNamesSh[Today.GetWeekDay];
  TodaysDate := TodaysDate + DayStr + '.'+ CDMonthNames[Today.GetMonth] + ' ' +YearStr;
  VToday := New (PStaticText, Init (R, TodaysDate));
  VToday^.GrowMode := gfGrowLoY + gfGrowHiY;
  Insert (VToday);

  GetExtent (R);
  R.Assign (R.B.X-1, R.A.Y+1, R.B.X, R.B.Y-1);
  New (VScrollBar, Init(R));
  VScrollBar^.Options := VScrollBar^.Options OR ofPostProcess;
  Insert (VScrollBar);

  GetExtent (R);
  R.Assign (R.A.X + 2, R.A.Y + 2, R.B.X - 2, R.B.Y - 3);
  Scroller := New (PCalendar, Init(R, VScrollBar));
  Scroller^.Options := Scroller^.Options OR ofFramed;
  Scroller^.SetLimit (30, LineCount);
  Insert (Scroller);
END (* TCalWindow.Init *);

PROCEDURE TCalWindow.SizeLimits (VAR Min, Max : TPoint);
CONST
  CalMin : TPoint = (X: 32; Y: 13);
  CalMax : TPoint = (X: 32; Y: 23);
BEGIN
  Min := CalMin;
  Max := CalMax;
END (* TCalWindow.SizeLimits *);

{=========================================================================}
{ TCalApp }

CONSTRUCTOR TCalApp.Init;
BEGIN
  InitMemory;
  TApplication.Init;
  (* room for further initialization *)
  FillCalLines;
  EventMask := EventMask OR evBroadcast;
  (* allow broadcast events to be handled *)
  EditBuffer := NIL;
END (* TCalApp.Init *);


PROCEDURE TCalApp.FillCalLines;
VAR
  t                     : TDate;
  Offset, Counter, loop : INTEGER;
  AWeek, ADay, Equals   : String;
BEGIN
  (* initialize all lines *)
  HideCursor;
  LineCount := 500;
  t.Init (01,01,1990);

  Counter := 0;
  WHILE (Counter < LineCount) DO
  BEGIN
    Str (T.GetYear, AWeek);
    Offset := (22 - Length(CDMonthNames[t.GetMonth])) DIV 2;
    Equals := '';
    FOR loop := 1 TO Offset DO
      Equals := Equals + #205;

    Lines[Counter] := NewMyStr(Equals + ' ' + CDMonthNames[t.GetMonth] + ' ' + AWeek + ' ' + Equals);
    INC(Counter);

    Lines[Counter] := NewMyStr(' Mo  Di  Mi  Do  Fr  Sa  So ');
    (* Lines[Counter] := NewMyStr(' Mo  Tu  We  Th  Fr  Sa  Su '); *)
    INC(Counter);

    Offset := INTEGER (t.GetWeekDay) * 4;

    (* Generate a month *)
    REPEAT
      AWeek := ' ';
      FOR loop := 1 TO Offset DO
        AWeek := AWeek + ' ';            (* clear string                     *)

      (* Generate a week or till end of month *)
      REPEAT
        Str (t.GetDay:2,ADay);           (* convert DayIndex into a string   *)
        AWeek := AWeek + ADay + '  ';    (* increment offset to put next day *)
        INC(Offset,4);
        t.INCDay;                        (* increment julian date            *)
      UNTIL ((t.GetWeekDay = 0) OR (t.GetDay = 1));
                                         (* day is sunday or end of month    *)
      Lines[Counter] := NewMyStr(AWeek); (* print out the whole week         *)
      INC(Counter);                      (* increment line counter           *)
      Offset := 0;                       (* reset offset to zero for monday  *)
    UNTIL (t.GetDay = 1);

    (* insert blank line at end of month *)
    AWeek := '                           ';
    Lines[counter] := NewMyStr(AWeek);
    INC(Counter);
  END;
END (* TCalApp.FillCalLines *);

PROCEDURE TCalApp.EvaluateDate (Column, Line : WORD);
VAR
  Day, Month, Year : WORD;
  Counter, a, b    : WORD;
  Dummy            : INTEGER;
  TempStr, YearStr : String;
  Found            : BOOLEAN;
BEGIN
  (* get the day *)
  TempStr := Copy (Lines[Line]^,Column,3);
  (* val doesn't like trailing blanks, so just in case, get rid of them *)
  IF (TempStr[3] = ' ') THEN TempStr[0]:=#2;
  Val (TempStr, Day, Dummy);

  (* get month and year *)
  Counter := 0;
  WHILE (POS ('ÕÕÕ',Lines[Line - Counter]^) = 0) DO
    INC (Counter);
  TempStr := Lines[Line - Counter]^;
  a := POS ('Õ ',TempStr)+2;        (* characters preceding month and year *)
  b := POS (' Õ',TempStr);          (* characters trailing month and year  *)
  TempStr := Copy (TempStr,a,b-a);

  a := POS (' ',TempStr);
  YearStr := Copy (TempStr, a+1,4);
  Val (YearStr, Year, Dummy);

  YearStr := Copy (TempStr, 1, a-1);
  Counter := 0;
  REPEAT
    INC(Counter);
    Found := (YearStr = CDMonthNames[Counter]);
  UNTIL (Found OR (Counter >=12));

  MemoDate.Init (Day, Counter, Year);
END (* TCalApp.EvaluateDate *);


FUNCTION TCalApp.LoadFileCollect (VAR Name : PathStr): PCollection;
VAR
  p        : PCollection;
  ALine    : String;
  TextFile : Text;
BEGIN
  Assign (TextFile, Name);
  {$I-}
  Reset (TextFile);
  {$I+}
  IF (IOResult = 0) THEN
  BEGIN
    p := New(PCollection, Init(20,1));
    WHILE NOT EOF(TextFile) DO
    BEGIN
      ReadLn(TextFile, ALine);
      p^.Insert (New(PStr, Init(ALine)));
    END;
    Close (TextFile);
  END ELSE p := NIL;
  LoadFileCollect := p;
END;


PROCEDURE TCalApp.OpenMemoWindow;
VAR
  R                         : TRect;
  DayStr, MonthStr, YearStr : String[15];
  TodaysDate, ALine         : String;
  MemoFileName              : PathStr;
  Command                   : WORD;
  MemoEditor                : PEditor;
BEGIN
  Str (MemoDate.GetDay,   DayStr);               (* generate day string    *)
  MonthStr := CDMonthNames[MemoDate.GetMonth];   (* generate month string  *)
  Str (MemoDate.GetYear,  YearStr);              (* generate year string   *)
  TodaysDate := 'Tag = ' + DayStr + '. ' + MonthStr + ' ' + YearStr;
  R.Assign (32, 0, 80, 23);

  MemoFileName := BuildFileName (MemoDate);
  IF FileExist (MemoFileName) THEN BEGIN
    (* Datei_laden *)
    ALine := 'Zu diesem Datum gibt es bereits eine Notiz => Laden der Datei ';
    Command := MessageBox(ALine+MemoFileName, NIL, mfOkButton+mfInformation);
    EditBuffer := LoadFileCollect (MemoFileName);
  END;
  New (MemoEditor, Init(R, MemoEditorWindowNo, TodaysDate, EditBuffer, EdBroadcastBase));
  Desktop^.Insert (MemoEditor);
END (* TCalApp.OpenMemoWindow *);

PROCEDURE TCalApp.OpenCalWindow;
VAR
  VCalWindow : PCalWindow;
BEGIN
  VCalWindow := New (PCalWindow, Init);
  DeskTop^.Insert (VCalWindow);
END (* TCalApp.OpenCalWindow *);

PROCEDURE TCalApp.SaveMemo (P : PCollection);
(* saves the PCollection indicated by EditBuffer *)
VAR
  TextFile : TEXT;
  Counter  : INTEGER;
  FileName : String;
  Dialog   : PDialog;
  R        : TRect;
  Command  : WORD;
BEGIN
  FileName := BuildFileName (MemoDate);
  Command := MessageBox ('Editordatei '+FileName+' abspeichern', NIL, mfOkButton+mfInformation);
  Assign (TextFile,FileName);
  Rewrite (TextFile);
  FOR Counter := 0 TO (P^.Count-1) DO
    WriteLn (TextFile, PStr(P^.At(Counter))^.GetStr);
  Close (TextFile);
END;

PROCEDURE TCalApp.LoadMemo;
(* loads a file to be specified into EditBuffer *)
VAR
  FileName : PathStr;
  Dialog   : PDialog;
  Command  : WORD;
  NewEvent : TEvent;
BEGIN
  (* pop up file selection box *)
  Dialog := New(PFileDialog, Init('*'+Extension,'Notizdatei laden','~N~ame', fdOkButton, 101));
  Command := DeskTop^.ExecView (Dialog);
  PFileDialog(Dialog)^.GetFileName(FileName);
  Dispose (Dialog, Done);
  (* hier kînnen nur existierende Dateien ausgewÑhlt werden *)

  NewEvent.What := evBroadcast;
  NewEvent.Command := EdBroadcastBase + cmEdSend;
  NewEvent.InfoPtr := LoadFileCollect (FileName);
  TopView^.HandleEvent (NewEvent);
END;

PROCEDURE TCalApp.InitStatusLine;
VAR
  R : TRect;
BEGIN
  GetExtent (R);
  R.A.Y := R.B.Y -1;
  StatusLine := New (PStatusLine, Init(R, NewStatusDef (0, $FFFF,
                   NewStatusKey ('~Alt-K~alender', kbAltK, cmOpenCalWindow,
                   NewStatusKey ('DrÅcken Sie ~Alt-X~ zum Beenden', kbAltX, cmQuit,
                   nil)),
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
      cmOpenCalWindow : OpenCalWindow;
      ELSE EXIT;
    END;
  END;
  IF (Event.What = evBroadcast) THEN
  BEGIN
    CASE Event.Command OF
      brOpenMemo    : BEGIN
                         PPtr := PPOINT(Event.InfoPtr);
                         EvaluateDate (PPtr^.X, PPtr^.Y);
                         OpenMemoWindow;
                      END;
      brNoValidDate : BEGIN
                        (* open alarm box telling that this is not a nice date *)
                        s := '  Dies ist kein gÅltiges Datum !    ';
                        t := 'Sie mÅssen auf eine Ziffer des gewÅnschten Datums klicken.';
                        C := MessageBox(s+t, NIL, mfError+mfOkButton);
                      END;
      EdBroadcastBase + cmEdSave : SaveMemo (Event.InfoPtr);
      EdBroadcastBase + cmEdRead : LoadMemo;
    END;
  END;
  ClearEvent (Event);
END (* TCalApp.HandleEvent *);

{=========================================================================}

BEGIN
  MyApp.Init;
  MyApp.Run;
  MyApp.Done;
END (* Calendar *).
