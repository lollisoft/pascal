{************************************************}
{                                                }
{      Beispielprogramm fÅr Vision's CD          }
{                                                }
{************************************************}

{$V-,X+}

PROGRAM TreeApp;

USES
  OBJECTs, Drivers, Memory, Views, Menus,
  Dialogs, StdDlg, App, Dos, DirTree, MsgBox;

CONST
  cmFileOpen = 102;
  cmVisionCD = 103;

TYPE
  PLineCollection = ^TLineCollection;
  TLineCollection = OBJECT(TCollection)
    PROCEDURE FreeItem(P: Pointer); VIRTUAL;
  END;

  PFileViewer = ^TFileViewer;
  TFileViewer = OBJECT(TScroller)
    FileLines: PCollection;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      VAR FileName: PathStr);
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
  END;

  PFileWindow = ^TFileWindow;
  TFileWindow = OBJECT(TWindow)
    CONSTRUCTOR Init(VAR FileName: PathStr);
  END;

  PTreeViewerApp = ^TTreeViewerApp;
  TTreeViewerApp = OBJECT(TApplication)
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
    PROCEDURE InitStatusLine; VIRTUAL;
    PROCEDURE VisionCD;
  END;


PROCEDURE TLineCollection.FreeItem(P: Pointer);
BEGIN
  DisposeStr(P);
END;

CONSTRUCTOR TFileViewer.Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
  VAR FileName: PathStr);
VAR
  FileToView: Text;
  Line: STRING;
  MaxWidth: Integer;
BEGIN
  TScroller.Init(Bounds, AHScrollbar, AVScrollBar);
  GrowMode := gfGrowHiX + gfGrowHiY;
  FileLines := New(PLineCollection, Init(5,5));
  {$I-}
  Assign(FileToView, FileName);
  Reset(FileToView);
  IF IOResult <> 0 THEN
    MessageBox('Cannot open file '+Filename+'.',NIL, mfError+mfOKButton)
  ELSE
  BEGIN
    MaxWidth := 0;
    WHILE NOT EoF(FileToView) AND NOT LowMemory DO
    BEGIN
      ReadLn(FileToView, Line);
      IF Length(Line) > MaxWidth THEN MaxWidth := Length(Line);
      FileLines^.Insert(NewStr(Line));
    END;
  END;
  {$I+}
  SetLimit(MaxWidth, FileLines^.Count);
END;

DESTRUCTOR TFileViewer.Done;
BEGIN
  Dispose(FileLines, Done);
  TScroller.Done;
END;

PROCEDURE TFileViewer.Draw;
VAR
  B: TDrawBuffer;
  C: Byte;
  I: Integer;
  S: STRING;
  P: PString;
BEGIN
  C := GetColor(1);
  FOR I := 0 TO Size.Y-1 DO
  BEGIN
    MoveChar(B, ' ', C, Size.X);
    IF Delta.Y + I < FileLines^.Count THEN
    BEGIN
      P := FileLines^.At(Delta.Y+I);
      IF P <> NIL THEN S := Copy(P^, Delta.X+1, Size.X)
      ELSE S := '';
      MoveStr(B, S, C);
    END;
    WriteLine(0, I, Size.X, 1, B);
  END;
END;

CONSTRUCTOR TFileWindow.Init(VAR FileName: PathStr);
CONST
  WinNumber: Integer = 1;
VAR
  R: TRect;
BEGIN
  Desktop^.GetExtent(R);
  TWindow.Init(R, Filename, WinNumber);
  Inc(WinNumber);
  GetExtent(R);
  R.Grow(-1,-1);
  Insert(New(PFileViewer, Init(R,
    StandardScrollBar(sbHorizontal + sbHandleKeyboard),
    StandardScrollBar(sbVertical + sbHandleKeyboard), Filename)));
END;


{ TTreeViewerApp }

PROCEDURE TTreeViewerApp.HandleEvent(VAR Event: TEvent);

PROCEDURE FileOpen;
VAR
  D: PCdDialog;
  R: TRect;
  C: Word;
  TCdDialogRec: STRING;
  W: PFileWindow;
BEGIN
  D := New(PCdDialog, Init(cdSelectButton));
  C := Desktop^.ExecView(D);
  IF C = cmOK THEN BEGIN
    D^.GetData(TCdDialogRec);
    IF TCdDialogRec[Length(TCdDialogRec)] = '\' THEN Exit;
    W := New(PFileWindow, Init(TCdDialogRec));
    IF W <> NIL THEN Desktop^.Insert(W);
  END;
  Dispose(D, Done);
END;

BEGIN
  TApplication.HandleEvent(Event);
  CASE Event.What OF
    evCommand:
      BEGIN
        CASE Event.Command OF
          cmFileOpen: FileOpen;
          cmVisionCD: VisionCD;
        ELSE
          Exit;
        END;
        ClearEvent(Event);
      END;
  END;
END;

PROCEDURE TTreeViewerApp.InitMenuBar;
VAR
  R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', 100, NewMenu(
      NewItem('~O~pen...', 'F3', kbF3, cmFileOpen, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext, NIL))),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      NewItem('~R~esize/move','Ctrl-F5',kbCtrlF5,cmResize, hcNoContext,
      NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
      NewItem('~N~ext', 'F6', kbF6, cmNext, hcNoContext,
      NewItem('~T~ree', '', 0, cmVisionCD, hcNoContext,
      NewItem('~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext,
      NIL)))))), NIL)))));
END;

PROCEDURE TTreeViewerApp.InitStatusLine;
VAR
  R: TRect;
BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y-1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~F3~ Open', kbF3, cmFileOpen,
      NewStatusKey('~F7~ Vision''s CD', kbF7, cmVisionCD,
      NewStatusKey('~Alt-F3~ Close', kbAltF3, cmClose, NIL)))), NIL)));
END;

PROCEDURE TTreeViewerApp.VisionCD;
VAR
  D: PCdDialog;
  C: Word;
BEGIN
  D := New(PCdDialog, Init(cdChangeButton));
  C := Desktop^.ExecView(D);
  Dispose(D, Done);
END;

VAR
  TreeViewerApp: TTreeViewerApp;
BEGIN
  TreeViewerApp.Init;
  TreeViewerApp.Run;
  TreeViewerApp.Done;
END.
