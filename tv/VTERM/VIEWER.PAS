{************************************************}
{                                                }
{   Copyright (c) 1990 by Borland International  }
{                                                }
{************************************************}

{$X+}

UNIT Viewer;

INTERFACE

USES
  Dos, Objects, Drivers, Memory, MsgBox,
  Views, Menus, Dialogs, StdDlg, App;

CONST
  cmTile      = 100;
  cmCascade   = 101;
  cmFileOpen  = 102;
  cmChangeDir = 103;

TYPE
  PLineCollection = ^TLineCollection;
  TLineCollection = OBJECT(TCollection)
    PROCEDURE FreeItem(P: Pointer); VIRTUAL;
  END;

  PFileViewer = ^TFileViewer;
  TFileViewer = OBJECT(TScroller)
    FileLines: PCollection;
    IsValid: Boolean;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      VAR FileName: PathStr);
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    FUNCTION Valid(Command: Word): Boolean; VIRTUAL;
  END;

  PFileWindow = ^TFileWindow;
  TFileWindow = OBJECT(TWindow)
    CONSTRUCTOR Init(VAR FileName: PathStr);
  END;

IMPLEMENTATION

{ TLineCollection }

PROCEDURE TLineCollection.FreeItem(P: Pointer);
BEGIN
  DisposeStr(P);
END;


{ TFileViewer }

CONSTRUCTOR TFileViewer.Init(VAR Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; VAR FileName: PathStr);
VAR
  FileToView: Text;
  Line: STRING;
  MaxWidth: Integer;
BEGIN
  TScroller.Init(Bounds, AHScrollbar, AVScrollBar);
  GrowMode := gfGrowHiX + gfGrowHiY;
  IsValid := True;
  FileLines := New(PLineCollection, Init(5,5));
  {$I-}
  Assign(FileToView, FileName);
  Reset(FileToView);
  IF IOResult <> 0 THEN
  BEGIN
    MessageBox('Cannot open file '+Filename+'.', NIL, mfError + mfOkButton);
    IsValid := False;
  END
  ELSE
  BEGIN
    MaxWidth := 0;
    WHILE NOT EoF(FileToView) AND NOT LowMemory DO
    BEGIN
      ReadLn(FileToView, Line);
      IF Length(Line) > MaxWidth THEN MaxWidth := Length(Line);
      FileLines^.Insert(NewStr(Line));
    END;
    Close(FileToView);
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
  FOR I := 0 TO Size.Y - 1 DO
  BEGIN
    MoveChar(B, ' ', C, Size.X);
    IF Delta.Y + I < FileLines^.Count THEN
    BEGIN
      P := FileLines^.At(Delta.Y + I);
      IF P <> NIL THEN S := Copy(P^, Delta.X + 1, Size.X)
      ELSE S := '';
      MoveStr(B, S, C);
    END;
    WriteLine(0, I, Size.X, 1, B);
  END;
END;

FUNCTION TFileViewer.Valid(Command: Word): Boolean;
BEGIN
  Valid := IsValid;
END;


{ TFileWindow }

CONSTRUCTOR TFileWindow.Init(VAR FileName: PathStr);
CONST
  WinNumber: Integer = 1;
VAR
  R: TRect;
BEGIN
  Desktop^.GetExtent(R);
  TWindow.Init(R, Filename, WinNumber);
  Options := Options OR ofTileable;
  Inc(WinNumber);
  GetExtent(R);
  R.Grow(-1, -1);
  Insert(New(PFileViewer, Init(R,
    StandardScrollBar(sbHorizontal + sbHandleKeyboard),
    StandardScrollBar(sbVertical + sbHandleKeyboard), Filename)));
END;

END.