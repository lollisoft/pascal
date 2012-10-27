{*******************************************************}
{                                                       }
{  Unit zum Telekommunikationsprogramm VisionTerminal   }
{                                                       }
{*******************************************************}

UNIT VTDlg;

{$X+}

INTERFACE

USES
  ComIO, Dos, VtType, MsgBox, App, Dialogs,
  Drivers, Memory, Objects, StdDlg, TextView, Views;

TYPE
  PLogFileIndicatOR = ^TLogFileIndicatOR;
  TLogFileIndicatOR = OBJECT(TView)
    Name: PathStr;
    CONSTRUCTOR Init(VAR Bounds: TRect; AName: PathStr);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE SetState(AState: Word; Enable: Boolean); VIRTUAL;
  END;

  PLogTerminal = ^TLogTerminal;
  TLogTerminal = OBJECT(TTerminal)
    Logging: Boolean;
    CurLine: STRING;
    PROCEDURE StrWrite(VAR S: TextBuf; Count: Byte); VIRTUAL;
    PROCEDURE StrWriteOneChar(VAR S: TextBuf; Count: Byte);
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
  END;

  PComWindow = ^TComWindow;
  TComWindow = OBJECT(TWindow)
    Terminal: PLogTerminal; { Das ist unser Terminal-Window }
    Com: TCom; { Mit diesem Objekt steuern wir die serielle Schnittstelle }
    Indicator: PLogFileIndicatOR;
    LineSettings: TLineSettings;
    LocalEcho,
    Logging: Boolean;
    LogFile: Text;
    LogFileName: PathStr;
    Win: Text;
    CONSTRUCTOR Init;
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    FUNCTION InitCOM(LineParam: TLineSettings): Boolean;
    PROCEDURE SetLocalEcho;
    PROCEDURE OpenLogFile;
    PROCEDURE CloseLogFile;
    PROCEDURE SetLineDialog;
  END;

IMPLEMENTATION

TYPE
  PRadioButtonsChecked = ^TRadioButtonsChecked;
  TRadioButtonsChecked = OBJECT(TRadioButtons)
    Min, Max: Word;
    CONSTRUCTOR Init(VAR R: TRect; AStrings: PSItem; AMin, AMax: Word);
    FUNCTION Valid(Command: Word): Boolean; VIRTUAL;
  END;


{ TRadioButtonsChecked }

CONSTRUCTOR TRadioButtonsChecked.Init(VAR R: TRect; AStrings: PSItem;
                                      AMin, AMax: Word);
BEGIN
  TRadioButtons.Init(R, AStrings);
  Min := AMin;
  Max := AMax;
END;

FUNCTION TRadioButtonsChecked.Valid(Command: Word): Boolean;
BEGIN
  Valid := True;
  IF (Command <> cmCancel) and (Command <> 0) THEN
    IF NOT(Value IN [Min..Max]) THEN
    BEGIN
      Select;
      MessageBox('This selection is not allowed', NIL, mfError);
      Valid := False;
    END;
END;


{ TLogFileIndicator }

CONSTRUCTOR TLogFileIndicatOR.Init(VAR Bounds: TRect; AName: PathStr);
BEGIN
  TView.Init(Bounds);
  EventMask := EventMask OR evBroadCast;
END;

PROCEDURE TLogFileIndicatOR.HandleEvent(VAR Event: TEvent);
BEGIN
  TView.HandleEvent(Event);
  IF (Event.What = evMouseDown) THEN PWindow(Owner)^.Frame^.HandleEvent(Event);
   { weird al jankovich }
END;

PROCEDURE TLogFileIndicatOR.SetState(AState: Word; Enable: Boolean);
BEGIN
  TView.SetState(AState, Enable);
  IF AState and (sfActive + sfDragging) <> 0 THEN DrawView;
END;

PROCEDURE TLogFileIndicatOR.Draw;
VAR
  B: TDrawBuffer;
  C: Byte;
  S: STRING;
BEGIN
  IF Name <> '' THEN
    S := ' Logged to: ' + Name + ' '
  ELSE S := '';
  IF State and sfDragging <> 0 THEN
  BEGIN
    C := GetColor(3);
    IF Length(Name) <> 0 THEN IF
      PLogTerminal(Owner)^.Size.X < Length(Name)+23
      THEN GrowTo(PLogTerminal(Owner)^.Size.X-10,1)
      ELSE GrowTo(Length(Name)+13,1);
  END
  ELSE C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, 1, B);
END;


{ TLogTerminal }

PROCEDURE TLogTerminal.StrWrite(VAR S: TextBuf; Count: Byte);
VAR
  L: Byte;
  T: TextBuf;
BEGIN
  FOR L := 0 TO Count-1 DO BEGIN
    T[0] := S[L];
    StrWriteOneChar(T, 1);
  END;
END;

PROCEDURE TLogTerminal.StrWriteOneChar(VAR S: TextBuf; Count: Byte);
VAR
  I, J: Word;
  ScreenLines: Word;
BEGIN
  IF Logging THEN BEGIN
    IF S[0] = ^H THEN
      Dec(Byte(CurLine[0]))
    ELSE BEGIN
      Move(S,CurLine[Length(CurLine)+1],Count);
      CurLine[0] := Char(Length(CurLine) + Count);
    END;
    IF CurLine[Length(CurLine)] = #10 THEN BEGIN
      Message(Owner, evBroadCast, cmLogThisLine, @CurLine);
      CurLine := '';
    END;
  END;
  IF Limit.Y > 0 THEN
    ScreenLines := Limit.Y
  ELSE
    ScreenLines := 1;
  J := 0;
  FOR I := 0 TO Count-1 DO
    CASE S[I] OF
      #13: Dec(Count)
      ELSE
      BEGIN
        IF S[I] = #10 THEN Inc(ScreenLines);
        S[J] := S[I];
        Inc(J);
      END;
    END;
  WHILE NOT CanInsert(Count) DO
  BEGIN
    QueBack := NextLine(QueBack);
    Dec(ScreenLines);
  END;
  IF (S[0] = ^H) and (Count > 0) THEN
  BEGIN
    IF (QueFront <> QueBack) THEN
      IF QueFront = 0 THEN
        IF Buffer^[BufSize-1] <> #10    { Ist es ein LF? }
        THEN QueFront := BufSize - 1
        ELSE { es ist ein LF, deshalb tun wir nichts }
      ELSE IF Buffer^[QueFront-1] <> #10
           THEN Dec(QueFront)
  END
  ELSE
  IF QueFront + Count > BufSize THEN BEGIN
    I := BufSize - QueFront;
    Move(S,Buffer^[QueFront], I);
    Move(S[I],Buffer^, Count - I);
    QueFront := Count - I;
  END
  ELSE BEGIN
    Move(S,Buffer^[QueFront],Count);
    Inc(QueFront,Count);
  END;
  I := PrevLines(QueFront,1);
  IF I <= QueFront THEN
    I := QueFront - I
  ELSE
    I := BufSize - (I - QueFront);
  SetCursor(I-Delta.X, ScreenLines-Delta.Y-1);
  IF I+1 > Limit.X THEN
    SetLimit(I+1,ScreenLines)
  ELSE
    SetLimit(Limit.X, ScreenLines);
  ScrollTo(I-Size.X+1, ScreenLines-Size.Y+1);
  DrawView;
END;

PROCEDURE TLogTerminal.HandleEvent(VAR Event: TEvent);
BEGIN
  TTerminal.HandleEvent(Event);
  IF (Event.What = evBroadCast) and (Event.Command = cmFlushLog) THEN
    Message(Owner, evBroadCast, cmLogThisLine, @CurLine);
END;


{ TComWindow }

CONSTRUCTOR TComWindow.Init;
VAR R: TRect;
BEGIN
  Desktop^.GetExtent(R);
  TWindow.Init(R, '', wnNoNumber);
  R.Grow(-1,-1);
  Terminal := New(PLogTerminal, Init(R,
                  StandardScrollBar(sbHorizontal + sbHandleKeyboard),
                  StandardScrollBar(sbVertical + sbHandleKeyboard),
                  8000) );
  Options := Options OR (ofSelectable+ofTileable);
  Flags := Flags AND NOT wfClose;
  Insert(Terminal);
  R.Assign(R.A.X+3, R.A.Y-1, R.A.X+3, R.A.Y);
  Indicator := New(PLogFileIndicatOR, Init(R, ''));
  Insert(Indicator);
  AssignDevice(Win, Terminal);
  Rewrite(Win);
  WITH LineSettings DO
  BEGIN
    Port := 0;     { COM 1        }
    Baud := 2;     { 2400 Baud    }
    DataBits := 1; { 8 Data Bits  }
    Parity := 0;   { 1 Parity Bit }
    StopBits := 1; { 1 Stop Bit   }
  END;
  Com.Init(4000);
  InitCOM(LineSettings);
  LocalEcho := True;
END;

DESTRUCTOR TComWindow.Done;
BEGIN
  Com.Done;
  TWindow.Done;
  IF Logging THEN System.Close(LogFile);
END;

PROCEDURE TComWindow.HandleEvent(VAR Event: TEvent);
BEGIN
  IF (Event.What AND evKeyDown) <> 0 THEN
  BEGIN
    IF Event.CharCode <> #0 THEN
      CASE Event.CharCode OF
        #13:
          BEGIN
            Com.Put(13);
            IF LocalEcho THEN WriteLn(Win);
            ClearEvent(Event);
          END;
        ELSE BEGIN
          Com.Put(Byte(Event.CharCode));
          IF LocalEcho THEN Write(Win, Event.CharCode);
          ClearEvent(Event);
        END;
      END
    END
  ELSE
  IF Event.What = evBroadcast THEN
     CASE Event.Command OF
       cmDoDial:
         BEGIN
           LineSettings := TDialEntry(Event.InfoPtr^).LineSettings;
           LocalEcho :=
                NOT Boolean(TdialEntry(Event.InfoPtr^).LocalEcho);
           IF InitCOM(LineSettings) and
              (TDialEntry(Event.InfoPtr^).Number <> '') THEN
                  Com.PutString(DialPrefix+
                  TDialEntry(Event.InfoPtr^).Number+^M);
           ClearEvent(Event);
         END;
       cmLogThisLine: Write(LogFile, PString(Event.InfoPtr)^);
     END;
  TWindow.HandleEvent(Event);
END;


FUNCTION TComWindow.InitCOM(LineParam: TLineSettings): Boolean;
TYPE
  TBaudArr = ARRAY[0..4] OF Byte;
  TDataBitsArr = ARRAY[0..1] OF Byte;
  TParityArr = ARRAY[0..2] OF Byte;
  TStopBitsArr = ARRAY[0..1] OF Byte;
CONST
  BaudArr: TBaudArr = (Baud300, Baud1200, Baud2400, Baud4800, Baud9600);
  DataBitsArr: TDataBitsArr = (WordSize7, WordSize8);
  ParityArr: TParityArr = (NoParity, EvenParity, OddParity);
  StopBitsArr: TStopBitsArr = (StopBits1, StopBits2);
BEGIN
  WITH LineParam DO
  IF NOT Com.SetParam(Port,
                    BaudArr[Baud] OR
                    DataBitsArr[DataBits] OR
                    ParityArr[Parity] OR
                    StopBitsArr[StopBits] ) THEN
  BEGIN
    InitCOM := False;
    MessageBox(^C+'Cannot initialize COM '+Char(Ord('1')+Port)+
                ^M+^C+'Maybe already in use', NIL, mfError + mfOKButton);
  END
  ELSE
    InitCOM := True;
END;

PROCEDURE TComWindow.OpenLogFile;
CONST fdOpenReplace = fdOpenButton + fdReplaceButton;
VAR
   D: PFileDialog;
   C: Word;
BEGIN
  D := PFileDialog(Application^.ValidView(
    New(PFileDialog, Init('*.LOG', 'Log File','~N~ame',fdOpenReplace,100))
  ));
  IF D <> NIL THEN
  BEGIN
    C := Desktop^.ExecView(D);
    IF C <> cmCancel THEN
    BEGIN
      D^.GetFileName(LogFileName);
      Logging := True;
      Terminal^.Logging := True;
      Indicator^.Name := LogFileName;
      IF Terminal^.Size.X < Length(LogFileName)+18
      THEN Indicator^.GrowTo(Terminal^.Size.X-8,1)
      ELSE Indicator^.GrowTo(Length(LogFileName)+13,1);
      Indicator^.DrawView;
      Assign(LogFile, LogFileName);
      IF C = cmFileReplace THEN
        Rewrite(LogFile)
      ELSE BEGIN
        {$I-}
        Reset(LogFile);
        {$I+}
        IF IOResult <> 0 THEN Rewrite(LogFile)
         { Datei existiert nicht - also erzeugen }
        ELSE BEGIN
          System.Close(LogFile);
          Append(LogFile); { wir h„ngen es an das Ende der Datei }
        END;
      END;
    END;
  END;
END;

PROCEDURE TComWindow.CloseLogFile;
BEGIN
  Logging := False;
  Terminal^.Logging := False;
  Message(Owner, evBroadCast, cmFlushLog, NIL);
  System.Close(LogFile);
  Indicator^.Name := '';
  Indicator^.GrowTo(0,1);
  Indicator^.DrawView;
END;

PROCEDURE TComWindow.SetLocalEcho;
VAR
  C: Word;
BEGIN
  C := MessageBox('Set local echo ?', NIL,
            mfYesNoCancel + mfInformation);
  CASE C OF
    cmYes: LocalEcho := True;
    cmNo:  LocalEcho := False;
  END;
END;

PROCEDURE TComWindow.SetLineDialog;
VAR
  D: PDialog;
  Control: PView;
  Command: Word;
  R: TRect;
  Help: TLineSettings;
BEGIN
  R.Assign(0,0,37,21);
  D := New(PDialog, Init(R, 'Line Settings'));
  D^.Options := D^.Options OR ofCentered;
  R.Assign(5, 3, 16, 5);
  Control := New(PRadioButtons, Init(R,
    NewSItem('COM ~1~',
    NewSItem('COM ~2~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(4, 2, 11, 3);
  Control := New(PLabel, Init(R, '~P~ort', Control));
  D^.Insert(Control);
  R.Assign(5, 8, 16, 16);
  Control := New(PRadioButtonsChecked, Init(R,
    NewSItem('300',
    NewSItem('1200',
    NewSItem('2400',
    NewSItem('4800',
    NewSItem('9600',
    NewSItem('19200',
    NewSItem('38400',
    NewSItem('57600',
    NIL)))))))),
    0, 4
  ));
  D^.Insert(Control);
  R.Assign(4, 7, 11, 8);
  Control := New(PLabel, Init(R, '~B~aud', Control));
  D^.Insert(Control);
  R.Assign(22, 3, 32, 5);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~7~',
    NewSItem('~8~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(21, 2, 32, 3);
  Control := New(PLabel, Init(R, '~D~ata Bits', Control));
  D^.Insert(Control);
  R.Assign(22, 8, 32, 11);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~N~one',
    NewSItem('~E~ven',
    NewSItem('~O~dd',
    NIL)))
  ));
  D^.Insert(Control);
  R.Assign(21, 7, 32, 8);
  Control := New(PLabel, Init(R, 'p~A~rity', Control));
  D^.Insert(Control);
  R.Assign(22, 14, 32, 16);
  Control := New(PRadioButtons, Init(R,
    NewSItem('~1~',
    NewSItem('~2~',
    NIL))
  ));
  D^.Insert(Control);
  R.Assign(21, 13, 32, 14);
  Control := New(PLabel, Init(R, '~S~top Bits', Control));
  D^.Insert(Control);
  R.Assign(4, 18, 14, 20);
  Control := New(PButton, Init(R, 'O~k~', cmOK, bfDefault));
  D^.Insert(Control);
  R.Assign(23, 18, 33, 20);
  Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
  D^.Insert(Control);
  D^.SelectNext(False);
  IF LowMemory THEN
  BEGIN
    Dispose(D, Done);
    MessageBox('Out of memory',NIL,mfError);
    DoneMemory;
  END ELSE
  BEGIN
    D^.SetData(LineSettings);
    Command := DeskTop^.ExecView(D);
    IF Command = cmOK THEN
    BEGIN
      D^.GetData(Help);
      IF InitCOM(Help) THEN LineSettings := Help;
    END;
    Dispose(D, Done);
  END;
END;

END.