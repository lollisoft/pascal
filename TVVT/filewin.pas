(* ---------------------------------------------------------------- *)
(*                            FILEWIN.PAS                           *)
(*                                                                  *)
(* tFileWindow kann als Grundlage fÅr eine BenutzeroberflÑche Ö la  *)
(* NortonCommander genutzt werden. ErklÑrungen im Listing;          *)
(* Anwendung in FWDEMO.PAS                                          *)
(*                                                                  *)
(* (c) rr, 04.09.1991, 18.09.1991, 22.09.1991, 23.09.1991           *)
(* ---------------------------------------------------------------- *)
UNIT FileWin;

INTERFACE

USES Dos, Objects, Drivers, Memory,
     Views, Dialogs, App, Menus,
     StdDlg, MsgBox, Gadgets;

CONST
  cmNewInfoStr   = 4000;
  cmItemSelected = 4001;
  cmNewMasks     = 4003;
  cmOpenFileWin  = 4004;
  cmToggleNewWin = 4005;
  cmDirInWin     = 4006;
  cmClearDesk    = 4008;
  cmEnterNewMask = 4010;
  cmEnterNewDir  = 4011;
  cmDosShell     = 4012;

TYPE
  TSearchRec = RECORD
    Attr   : BYTE;
    Time   : LONGINT;
    Size   : LONGINT;
    Name   : STRING [12];
    Marked : BOOLEAN;
  END;
  PSearchRec = ^TSearchRec;

  PFileCollectionDF = ^TFileCollectionDF;
  TFileCollectionDF = OBJECT (TFileCollection)
    FUNCTION Compare (Key1, Key2: Pointer): Integer;          VIRTUAL;
  END;

  PMarkingList = ^TMarkingList;
  TMarkingList = OBJECT (TListBox)
    MarkNum : INTEGER;
    Marking : BOOLEAN;

    CONSTRUCTOR Init (VAR Bounds: TRect; Num : BYTE; 
		      AScrollBar: PScrollBar);
    PROCEDURE HandleEvent (VAR Event: TEvent);                VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE FocusItem (Item: INTEGER);                      VIRTUAL;
    PROCEDURE MarkItem (Item : INTEGER; Mark : BOOLEAN);      VIRTUAL;
    PROCEDURE GetData (VAR Rec);                              VIRTUAL;
    FUNCTION DataSize: Word;                                  VIRTUAL;
    FUNCTION GetPalette : PPalette;                           VIRTUAL;
    FUNCTION GetText (Item: INTEGER;
		      MaxLen: INTEGER): STRING;               VIRTUAL;
    FUNCTION GetInfoStr : STRING;                             VIRTUAL;
    FUNCTION IsMarked (Item : INTEGER) : BOOLEAN;             VIRTUAL;
    FUNCTION GetDummyLine : STRING;                           VIRTUAL;
    DESTRUCTOR Done;                                          VIRTUAL;
  END;

  PFileListBox = ^TFileListBox;
  TFileListBox = OBJECT (TMarkingList)
    MarkedSize : LONGINT;

    CONSTRUCTOR Init (VAR Bounds: TRect; 
		      AScrollBar: PScrollBar);
    PROCEDURE MarkItem (Item : INTEGER; Mark : BOOLEAN);      VIRTUAL;
    PROCEDURE ReadDirectory (Path : PathStr; NewL: BOOLEAN);  VIRTUAL;
    FUNCTION GetText (Item: INTEGER;
		      MaxLen: INTEGER): STRING;               VIRTUAL;
    FUNCTION GetInfoStr : STRING;                             VIRTUAL;
    FUNCTION IsMarked (Item : INTEGER) : BOOLEAN;             VIRTUAL;
    FUNCTION GetDummyLine : STRING;                           VIRTUAL;
  END;

  PFileWindow = ^TFileWindow;
  TFileWindow = OBJECT (TWindow)
    Masks  : STRING;
    CurDir : STRING;
    FileBox: PFileListBox;
    NewInfo: BOOLEAN;

    CONSTRUCTOR Init (VAR Bounds: TRect; Nr : INTEGER;
		      StartDir, NMasks: STRING);
    PROCEDURE HandleEvent (VAR Event : TEvent);               VIRTUAL;
    PROCEDURE Draw;                                           VIRTUAL;
    PROCEDURE ChangeBounds (VAR Bounds: TRect);               VIRTUAL;
    PROCEDURE ReadDirectory (Path, NMasks : STRING);          VIRTUAL;
    PROCEDURE SetDirectory (NewDir : STRING);                 VIRTUAL;
    PROCEDURE SetMasks (NewMasks : STRING);                   VIRTUAL;
    FUNCTION GetMasks : STRING;                               
    FUNCTION GetDirectory : STRING;                           
  END;

  PFileApplication = ^TFileApplication;
  TFileApplication = OBJECT (TApplication)
    WinNr       : INTEGER;
    NewWinBySel : BOOLEAN;

    CONSTRUCTOR Init;
    PROCEDURE OutOfMemory;                                    VIRTUAL;
    PROCEDURE Idle;                                           VIRTUAL;
    PROCEDURE HandleEvent (VAR Event : TEvent);               VIRTUAL;
  END;

FUNCTION UpString (S : STRING) : STRING;

FUNCTION DriveValid (Drive: CHAR): BOOLEAN; 

FUNCTION PathValid (VAR Path: STRING): BOOLEAN;

FUNCTION GetDir (Drive : BYTE) : STRING;

FUNCTION GetNewDir (CurDir : STRING; Info : PSearchRec) : STRING;

FUNCTION FormatSearchRec (Info : PSearchRec;
			  Lines: BOOLEAN) : STRING;

IMPLEMENTATION

(* ================================================================ *)
(*                          Hilfsroutinen                           *)
(* ================================================================ *)
FUNCTION UpString (S : STRING) : STRING;
  VAR i : BYTE;
BEGIN
  FOR i := 1 TO Length (s) DO
    s [i] := UpCase (s [i]);
  UpString := s;
END;

FUNCTION DriveValid (Drive: CHAR): BOOLEAN; ASSEMBLER;
ASM
	MOV     DL,Drive
	MOV     AH,36H
	SUB     DL,'A'-1
	INT     21H
	INC     AX
	JE      @@2
@@1:    MOV     AL,1
@@2:
END;

FUNCTION PathValid (VAR Path: STRING): BOOLEAN;
  VAR
    ExpPath: PathStr;
    F      : File;
    SR     : SearchRec;
BEGIN
  ExpPath := FExpand (Path);
  IF Length (ExpPath) <= 3 THEN
    PathValid := DriveValid (ExpPath [1])
  ELSE BEGIN
    IF ExpPath [Length (ExpPath)] = '\' THEN Dec (ExpPath [0]);
    FindFirst (ExpPath, Directory, SR);
    PathValid := (DosError = 0) AND (SR.Attr AND Directory <> 0);
  END;
END;

FUNCTION GetDir (Drive : BYTE) : STRING;
  VAR s : STRING;
BEGIN
  System.GetDir (Drive, s);
  GetDir := s;
END;

(* ---------------------------------------------------------------- *)
(* Erweitert oder kÅrzt das Verzeichnis CurDir, je nachdem, ob      *)
(* Info^.Name einen Unterverzeichnisnamen oder '..' enthÑlt.        *)
(* ---------------------------------------------------------------- *)
FUNCTION GetNewDir (CurDir : STRING; Info : PSearchRec) : STRING;
BEGIN
  IF Info^.Attr AND Directory <> 0 THEN BEGIN
    IF Info^.Name<>'..' THEN
      CurDir := CurDir+'\'+Info^.Name
    ELSE BEGIN
      WHILE CurDir [Length (CurDir)] <> '\' DO
	Dec (CurDir [0]);
      Dec (CurDir [0]);
    END;
    GetNewDir := CurDir;
  END ELSE
    GetNewDir := '';
END;

(* ---------------------------------------------------------------- *)
(* Formatiert einen Info-Record NortonCommander-like: Dateiname,    *)
(* Extension, Dateigrîsse bzw SUB-DIR oder UP--DIR, Datum und Zeit- *)
(* punkt der Erstellung sowie die Attribute in Form von Buchstaben, *)
(* z.B. AHS (Archive-Hidden-System). Ist Lines True, so werden die  *)
(* einzelnen EintrÑge mit einem senkrechten Strich (ASCII 179) von- *)
(* einander abgetrennt.                                             *)
(* ---------------------------------------------------------------- *)
FUNCTION FormatSearchRec (Info : PSearchRec;
			  Lines: BOOLEAN) : STRING;
  TYPE
    TParams = RECORD
		PName, PExt, PSize,
		PDate, PTime,PAttr : PString;
	      END;
  VAR
    Params : TParams;
    hs     : STRING;
    ht     : DateTime;
    Name, Ext,
    FSize, Time,
    Date, Attr,
    Result : STRING;

  FUNCTION LeadingZero (w : WORD) : STRING;
    VAR s : STRING;
  BEGIN
    Str (w:0, s);
    IF Length (s) = 1 THEN s := '0' + s;
    LeadingZero := s;
  END;

BEGIN
  IF Info^.Attr AND Directory > 0 THEN BEGIN
    Ext := '';
    Name:= Info^.Name;
    IF Info^.Name='..' THEN
      FSize := #16'UP--DIR'#17
    ELSE
      FSize := #16'SUB-DIR'#17;
  END ELSE BEGIN
    hs  := Info^.Name;
    IF Pos ('.', hs) > 0 THEN BEGIN
      { es gibt Dateien ohne Extension, bei denen diese Formatierung
	ziemlich falsch ausfallen wÅrde; daher die Fallunterscheidung}
      Name:= Copy (hs, 1, Pred (Pos ('.', hs)));
      Ext := Copy (hs, Succ (Pos ('.', hs)), 3);
    END ELSE BEGIN
      Name := hs;
      Ext := '';
    END;
    Str (Info^.Size, FSize);
  END;
  Params.PName := @Name;
  Params.PExt  := @Ext;
  Params.PSize := @FSize;

  UnpackTime (Info^.Time, ht);
  Str (ht.Day, hs);
  Date := hs+'.'+
	  LeadingZero(ht.Month)+'.'+
	  LeadingZero(ht.Year);
  Delete (Date, Length (Date)-3, 2);
  Params.PDate := @Date;

  Time := LeadingZero(ht.hour)+':'+
	  LeadingZero(ht.min);
  Params.PTime := @Time;

  Attr := '˙˙˙˙˙';  
  IF Info^.Attr AND Archive > 0 THEN Attr [1] := 'A';
  IF Info^.Attr AND ReadOnly> 0 THEN Attr [2] := 'R';
  IF Info^.Attr AND Hidden  > 0 THEN Attr [3] := 'H';
  IF Info^.Attr AND SysFile > 0 THEN Attr [4] := 'S';
  IF Info^.Attr AND Directory>0 THEN Attr [5] := 'D';
  Params.PAttr := @Attr;

  IF Lines THEN
    FormatStr (Result, '%-8s %3s≥%9s≥%8s≥%6s≥%5s', Params)
  ELSE
    FormatStr (Result, '%-8s %3s %9s %8s %6s %5s', Params);
  FormatSearchRec := Result;
END;

(* ================================================================ *)
(*                       TFileCollectionDF                          *)
(* ================================================================ *)
(* Compare ist TFileCollection.Compare aus STDDLG nachempfunden,    *)
(* nur mit dem Unterschied, dass es die Verzeicnisse VOR die Dateien*)
(* einordnet.                                                       *)
(* ---------------------------------------------------------------- *)
FUNCTION TFileCollectionDF.Compare (Key1, Key2: POINTER): INTEGER;
BEGIN
  IF PSearchRec(Key1)^.Name = PSearchRec(Key2)^.Name THEN Compare := 0
  ELSE IF PSearchRec(Key1)^.Name = '..' THEN Compare := -1
  ELSE IF PSearchRec(Key2)^.Name = '..' THEN Compare := 1
  ELSE IF (PSearchRec(Key1)^.Attr AND Directory <> 0) AND
     (PSearchRec(Key2)^.Attr AND Directory = 0) THEN Compare := -1
  ELSE IF (PSearchRec(Key2)^.Attr AND Directory <> 0) AND
     (PSearchRec(Key1)^.Attr AND Directory = 0) THEN Compare := 1
  ELSE IF PSearchRec(Key1)^.Name > PSearchRec(Key2)^.Name THEN
    Compare := 1
  ELSE Compare := -1;
END;

(* ================================================================ *)
(*                         TMarkingList                             *)
(* ================================================================ *)
CONSTRUCTOR TMarkingList.Init (VAR Bounds: TRect;
			       Num       : BYTE;
			       AScrollBar: PScrollBar);
BEGIN
  TListBox.Init (Bounds, Num, AScrollBar);
  Marking := FALSE;
  MarkNum := 0;
END;

(* ---------------------------------------------------------------- *)
(* HandleEvent schickt bei Selektierung eines Eintrages, dh bei     *)
(* dessen Anwahl mit Enter oder Doppelklick der Maus, einen cmItem- *)
(* Selected-Broadcast an APPLICATION ab und nicht an den Owner (der *)
(* die Nachricht natÅrlich auch empfangen kann), weil es in bestimm-*)
(* ten FÑllen nîtig sein kann, dass auf Programmebene Åber die Aus- *)
(* wertung entschieden wird. So bei TFileWindow: Das Programm, nicht*)
(* das Fenster, muss ein angewÑhltes Programm starten. Des weiteren *)
(* sind EintrÑge Åber INS oder mit der rechten Maustaste markierbar.*)
(* Bei der Mausmarkierung Åbernimmt HandleEvent das Maustracking    *)
(* selbst. Ist der erste angeklickte Eintrag nicht markiert, wird er*)
(* und alle folgenden markiert und umgekehrt. Deshalb die Verwendung*)
(* von First.                                                       *)
(* ---------------------------------------------------------------- *)
PROCEDURE TMarkingList.HandleEvent (VAR Event: TEvent);
  CONST
    Info   : POINTER = NIL;
  VAR
    NewPos     : TPoint;
    Factor     : SHORTINT;
    First      : BOOLEAN;
BEGIN
  IF ((Event.What = evKeyDown) AND (Event.KeyCode=kbEnter)) OR
     ((Event.What = EvMouseDown) AND (Event.Double)) THEN BEGIN
    Info := List^.At (Focused);
    Message(Application, evBroadCast, cmItemSelected, Info);
    ClearEvent (Event);
    { ausgeschicktes Ereignis lîschen, sonst lÑuft es noch weiter
      bis zu TApplication.EventError }
  END;

  IF (Event.What = evKeyDown) AND (Event.KeyCode=kbIns) THEN BEGIN
    MarkItem (Focused, NOT IsMarked (Focused));
    IF Focused < Pred (Range) THEN
      Inc (Focused);
    FocusItem (Focused);
    DrawView;
    ClearEvent (Event);
  END;

  IF (Event.What = evMouseDown) AND
     (Event.Buttons=mbRightButton) THEN BEGIN
    Factor := 0; First := TRUE;
    REPEAT
      MakeLocal (Event.Where, NewPos);
      IF MouseInView (Event.Where) AND     { Maus in View ? }
	 (TopItem+NewPos.Y < Range) THEN
	Focused := TopItem+NewPos.Y        { ja, Position ausrechnen }
      ELSE BEGIN                           { nein: }
	IF NewPos.Y < 0 THEN               { oberhalb ? }
	  Factor := -1
	ELSE                     { oder unterhalb der eigenen View ? }
	  IF NewPos.Y > Size.Y THEN Factor := 1;
	IF (Focused+Factor > Range-1) OR
	   (Focused+Factor < 0) THEN
	  Factor := 0;            { nicht ausserhalb von Range gehen }
	Inc (Focused, Factor);
      END;
      IF First THEN BEGIN 
	Marking := NOT IsMarked (Focused);
	First := FALSE;
      END;
      MarkItem (Focused, Marking);
      FocusItem (Focused);
      DrawView;
    UNTIL NOT MouseEvent (Event, evMouseMove+evMouseAuto);
    { Solange, bis Taste losgelassen ist }
    ClearEvent (Event);
  END;
  IF Event.What<>evNothing THEN
    TListBox.HandleEvent (Event);
END;

(* ---------------------------------------------------------------- *)
(* Die Darstellung muss nun auch berÅcksichtigen, ob ein Eintrag    *)
(* markiert ist. Festgestellt wird das Åber die abstrakte Funktion  *)
(* IsMarked. Anhand dieser Information wird die Farbe fÅr den Ein-  *)
(* trag gewÑhlt, wobei fÅr den fokussierten Eintrag eine jeweils    *)
(* andere verwendet wird. Hat die Liste weniger EintrÑge als der    *)
(* Darstellungsbereich Zeilen, werden die nicht ausgefÅllten Zeilen *)
(* mit GetDummyLines gefÅllt. Hat die Liste also Abtrennungen wie   *)
(* z.B. bei FileListBox, so muss diese Methode einen entsprechenden *)
(* String liefern.                                                  *)
(* ---------------------------------------------------------------- *)
PROCEDURE TMarkingList.Draw;
  VAR
    i    : INTEGER;
    Line : TDrawBuffer;
    s    : STRING;
    Col  : BYTE;
BEGIN
  IF TopItem+Size.Y-1 > Range-1 THEN
    TopItem := Range-Size.Y;
  IF TopItem < 0 THEN
    TopItem := 0;

  FOR i := TopItem TO TopItem+Size.Y-1 DO BEGIN
    IF i < Range THEN BEGIN
      IF (i=Focused) AND (IsMarked (i)) THEN Col := 5
	ELSE IF (i=Focused) THEN Col := 3
	  ELSE IF IsMarked (i) THEN Col := 1
	    ELSE Col := 2;
    END ELSE
      Col := 2;
    MoveChar (Line, ' ', GetColor (Col), Size.X+1);
    IF i < Range THEN BEGIN
      s := GetText (i, Size.Y);
      MoveStr (Line[1], s, GetColor (Col));
    END ELSE
      MoveStr (Line[1], GetDummyLine, GetColor (Col));
    WriteLine (0, i-TopItem, Size.X+1, 1, Line);
  END;
END;

(* ---------------------------------------------------------------- *)
(* FocusItem sendet einen BroadCast aus, dessen Zeiger auf den      *)
(* aktuellen InfoString zeigt. "S" ist eine Konstante, weil ein     *)
(* Zeiger auf eine lokale Variable eben nur lokal gÅltig ist, die   *)
(* konstante aber im globalen Datensegment ist.                     *)
(* ---------------------------------------------------------------- *)
PROCEDURE TMarkingList.FocusItem (Item: Integer);
  CONST
    s : STRING = '';
BEGIN
  TListBox.FocusItem (Item);
  s := GetInfoStr;
  Message (Owner, evBroadcast, cmNewInfoStr, @s);
END;

(* ---------------------------------------------------------------- *)
(* MarkItem hat den Eintrag zu markieren. Wie diese Information kon-*)
(* kret festgehalten wird, muss der Nachkomme entscheiden (z.B. wie *)
(* bei TFileListBox Åber das Feld Marked von TSearchRec).           *)
(* ---------------------------------------------------------------- *)
PROCEDURE TMarkingList.MarkItem (Item : INTEGER; Mark : BOOLEAN);
BEGIN
  IF Mark THEN Inc (MarkNum) ELSE Dec (MarkNum);
END;

PROCEDURE TMarkingList.GetData (VAR Rec);
BEGIN
  { Daten sollen nicht mehr als Strings verschickt werden;
    die DatenÅbertragung wird direkt Åber List abgewickelt. }
END;

FUNCTION TMarkingList.DataSize: Word;
BEGIN
  DataSize := 0;
END;

FUNCTION TMarkingList.GetPalette : PPalette;
  CONST CMyPal = #2#1#7#1#3;
	PMyPal : STRING [5] = CMyPal;
BEGIN
  GetPalette := @PMyPal;
END;

FUNCTION TMarkingList.GetText (Item: INTEGER;
			       MaxLen: INTEGER): STRING;
BEGIN
  GetText := '';    { Nachkommen mÅssen Daten besorgen }
END;

FUNCTION TMarkingList.GetInfoStr : STRING;
BEGIN
  GetInfoStr := '';  
END;

FUNCTION TMarkingList.IsMarked (Item : INTEGER) : BOOLEAN;
BEGIN
  IsMarked := FALSE; { siehe MarkItem }
END;

FUNCTION TMarkingList.GetDummyLine : STRING;
BEGIN
  GetDummyLine := '';
END;

DESTRUCTOR TMarkingList.Done;
BEGIN
  IF List <> NIL THEN
    Dispose (List, Done);   { Liste freigeben }
  TListBox.Done;
END;

(* ================================================================ *)
(*                         TFileListBox                             *)
(* ================================================================ *)
CONSTRUCTOR TFileListBox.Init (VAR Bounds: TRect;
			       AScrollBar: PScrollBar);
BEGIN
  TMarkingList.Init (Bounds, 1, AScrollBar);
  MarkedSize := 0;
END;

PROCEDURE TFileListBox.MarkItem (Item : INTEGER; Mark : BOOLEAN);
  VAR Info :PSearchRec;
BEGIN
  { NICHT Vorfahren verwenden, weil der nicht berÅcksichtigt, ob
    Directory oder nicht ! Directories sollen nicht markiert werden
    kînnen. }
  Info := PSearchRec (List^.At (Item));
  IF (Info^.Attr AND Directory=0) AND
     (Info^.Marked<>Mark) THEN BEGIN
    Info^.Marked := Mark;
    IF Mark THEN BEGIN
      Inc (MarkedSize, Info^.Size);
      Inc (MarkNum);
    END ELSE BEGIN
      Dec (MarkedSize, Info^.Size);
      Dec (MarkNum);
    END;
  END;
END;

(* ---------------------------------------------------------------- *)
(* Liest die Dateien aus Path ein, wobei erwartet wird, dass Path   *)
(* Verzeichnis+'\'+Suchmaske enthÑlt. Ist NewL TRUE, so wird die    *)
(* bisherige Liste gelîscht und eine neue erstellt. Wenn aber FALSE,*)
(* dann wird die alte Liste um neue EintrÑge ergÑnzt, wobei die     *)
(* alphabetische Ordnung natÅrlich erhalten bleibt. Der Zweck ist,  *)
(* dass mehrere Suchmasken in einem Fenster berÅcksichtigt werden   *)
(* kînnen (siehe TFileWindow). - Hat es nicht mehr genÅgend         *)
(* Speicher, wird OutOfMemory aufgerufen und die Liste freigegeben. *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileListBox.ReadDirectory (Path: PathStr;
				      NewL: BOOLEAN);
  VAR
    FileInfo : SearchRec;
    PInfo    : PSearchRec;
    FileBox  : PFileCollectionDF;  
BEGIN
  IF NewL THEN
    New (FileBox, Init (100, 10))
  ELSE
    FileBox :=  PFileCollectionDF (List);
  FindFirst (Path, AnyFile, FileInfo);
  WHILE DosError = 0 DO BEGIN
    New (PInfo);
    IF (PInfo=NIL) OR (LowMemory) THEN BEGIN
      Application^.OutOfMemory;
      Dispose (List, Done);
      Exit;
    END ELSE BEGIN
      PInfo^.Marked := FALSE;
      Move (FileInfo.Attr, PInfo^, SizeOf (PInfo^)-1);
      IF PInfo^.Name<>'.' THEN
	FileBox^.Insert (PInfo);
      FindNext(FileInfo);
    END;
  END;
  IF NewL THEN NewList (FileBox)
	  ELSE SetRange (List^.Count);
END;

FUNCTION TFileListBox.GetText (Item: INTEGER; MaxLen: INTEGER): STRING;
BEGIN
  GetText := FormatSearchRec (List^.At (Item), TRUE);
END;

FUNCTION TFileListBox.GetInfoStr : STRING;
  VAR
    SizeStr, NumStr, s : STRING;
BEGIN
  IF MarkNum=0 THEN
    GetInfoStr := FormatSearchRec (List^.At (Focused), FALSE)
  ELSE BEGIN
    Str (MarkedSize, SizeStr);
    Str (MarkNum, NumStr);
    s := SizeStr+' Bytes in '+NumStr+' Dateien.';
    GetInfoStr := s;
  END;
END;

FUNCTION TFileListBox.IsMarked (Item : INTEGER) : BOOLEAN;
  VAR Info : PSearchRec;
BEGIN
  Info := PSearchRec (List^.At (Item));
  IF Info^.Attr AND Directory > 0 THEN
    IsMarked := FALSE  { Directories kînnen nicht markiert werden }
  ELSE
    IsMarked := BOOLEAN (Info^.Marked);
END;

FUNCTION TFileListBox.GetDummyLine : STRING;
 CONST Dummy : STRING = '            ≥         ≥        ≥      ≥';
BEGIN
  GetDummyLine := Dummy;
END;

(* ================================================================ *)
(*                          TFileWindow                             *)
(* ================================================================ *)
(* Init prÅft zuerst, ob das angegebene Verzeichnis existiert. Wenn *)
(* nicht, wird das aktuelle genommen. Nach dem Aufruf von           *)
(* TWindow.Init wird eine FileListBox erzeugt und, sofern LowMemory *)
(* nicht FALSE ist, mit Insert in die Gruppe eingefÅgt.             *)
(* ---------------------------------------------------------------- *)
CONSTRUCTOR TFileWindow.Init (VAR Bounds: TRect;
			      Nr        : INTEGER;
			      StartDir  : STRING;
			      NMasks    : STRING);
  VAR R : TRect;
BEGIN
  IF (NOT PathValid (StartDir)) OR (StartDir='') THEN
    StartDir := GetDir (0);
  CurDir := StartDir;
  NewInfo := FALSE;

  TWindow.Init (Bounds, CurDir, Nr);
		    { Twindow.Init setzt Parameter fÅr Title auf '' !}
  CurDir := StartDir;                       { daher nochmal kopieren }

  R.Assign (1, 3, Size.X-1, Size.Y-3);
  FileBox := New (PFileListBox,
		  Init (R, 
			StandardScrollBar (sbVertical+
					   sbHandleKeyboard)));
  IF Application^.ValidView (FileBox)<>NIL THEN BEGIN
    FileBox^.GrowMode := gfGrowHiY+gfGrowHiX;
    SetMasks (NMasks);
    Insert (FileBox);
  END ELSE
    Fail;
END;

(* ---------------------------------------------------------------- *)
(* Draw gibt eine Kopfzeile (Head) zur Beschriftung der Liste aus   *)
(* ergÑnzt den Inhalt um "Eckzeichen". Zwei davon liegen auf dem    *)
(* Rahmen des Fensters und dÅrfen daher wÑhrend der Vergrîsserung   *)
(* oder Bewegung des Fensters nicht dargestellt werden. Damit der   *)
(* Fensterinhalt (und somit die Liste) nicht unnîtig oft dargestellt*)
(* wird, wird auf NewInfo geprÅft. Dieses Flag ist dann TRUE, wenn  *)
(* HandleEvent den BroadCast cmNewInfoStr empfangen hat und als     *)
(* Reaktion NewInfo auf TRUE setzt, DrawView aufruft und NewInfo    *)
(* wieder auf FALSE setzt. - Je nachdem, ob das Fenster aktiv oder  *)
(* passiv ist, wird eine andere Farbe benutzt.                      *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileWindow.Draw;
  CONST
    PasCharSet = '√ƒ≈¡';
    ActCharSet = '«ƒ≈¡';
    Head : STRING = ' Name    Ext ≥ Grîsse  ≥ Datum  ≥ Zeit ≥ Attr'+
		    '                                             ';
    Empty: STRING = '                                             '+
		    '                                             ';
  VAR
    Col      : BYTE;
    CharSet  : STRING;
BEGIN
  IF NOT NewInfo THEN
    TWindow.Draw;
  Col := 1; CharSet := ActCharSet;
  IF State AND sfActive > 0 THEN BEGIN
    Col := 2;  CharSet := ActCharSet;
  END ELSE BEGIN
    Col := 1;  CharSet := PasCharSet;
  END;
  IF NOT NewInfo THEN BEGIN
    WriteStr (1, 1, Copy (Head, 1, Size.X-2), Col);
    IF State AND sfDragging = 0 THEN BEGIN
      WriteChar (0, 2, CharSet[1], Col, 1);
      WriteChar (0, Size.Y-3, CharSet[1], Col, 1);
    END;
    WriteChar (1,  2, CharSet[2], Col, Size.X-2);
    WriteChar (14, 2, CharSet[3], Col, 1);
    WriteChar (24, 2, CharSet[3], Col, 1);
    WriteChar (33, 2, CharSet[3], Col, 1);
    WriteChar (40, 2, CharSet[3], Col, 1);
    WriteChar (1, Size.Y-3, CharSet[2], Col, Size.X-2);
    WriteChar (14,Size.Y-3, CharSet[4], Col, 1);
    WriteChar (24,Size.Y-3, CharSet[4], Col, 1);
    WriteChar (33,Size.Y-3, CharSet[4], Col, 1);
    WriteChar (40,Size.Y-3, CharSet[4], Col, 1);
  END;

  FillChar (Empty, 80, ' ');                    { Infozeile ausgeben }
  Empty := FileBox^.GetInfoStr;
  Empty[0] := CHAR (Size.X-3);
  WriteStr (2, Size.Y-2, Empty, Col);
END;

(* ---------------------------------------------------------------- *)
(* HandleEvent reagiert auf cmNewInfoStr mit dessen Darstellung     *)
(* (siehe Draw), auf cmNewMasks mit Aufruf von SetMasks und auf die *)
(* Anfrage, ob das Fenster ein Verzeichnis darstelle (das beim      *)
(* Broadcast cmDirInWin im InfoPtr-Feld Åbergeben wird), mit dem    *)
(* Aufruf von ClearEvent, sollte das Åbergebene Verzeichnis mit     *)
(* CurDir Åbereinstimmen.                                           *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileWindow.HandleEvent (VAR Event : TEvent);
BEGIN
  IF (Event.What = EvBroadCast) THEN BEGIN
    CASE Event.Command OF
      cmNewInfoStr   : BEGIN
			 NewInfo:= TRUE;
			 DrawView;
			 NewInfo := FALSE;
		       END;
      cmNewMasks     : SetMasks (STRING (Event.InfoPtr^));
      cmDirInWin     : IF STRING (Event.InfoPtr^)=CurDir THEN
			 ClearEvent (Event);
    END;
  END;

  IF Event.What<>evNothing THEN
    TWindow.HandleEvent (Event);
END;

(* ---------------------------------------------------------------- *)
(* ChangeBounds fordert eine gewisse Mindestgrîsse, damit es nicht  *)
(* mîglich ist, keinen Listeneintrag mehr darzustellen.             *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileWindow.ChangeBounds (VAR Bounds : TRect);
BEGIN
  IF Bounds.B.Y-Bounds.A.Y < 10 THEN
    Bounds.B.Y := Bounds.A.Y+10;
  TWindow.ChangeBounds (Bounds);
END;

(* ---------------------------------------------------------------- *)
(* ReadDirectory sucht fÅr alle in NMasks enthaltenen, durch "/"    *)
(* abgetrennten Masken die Dateien im Verzeichnis Path. Dadurch ist *)
(* es mîglich, in einem Fenster mehrere Suchmasken darzustellen.    *)
(* Sollen Directories berÅcksichtigt werden, so muss "*." in NMasks *)
(* enthalten sein. Ein Beispiel: "*.exe/*.com/*.bat/*." sucht alle  *)
(* ausfÅhrbaren Dateien sowie Verzeichnisse.                        *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileWindow.ReadDirectory (Path, NMasks : STRING);
  VAR FirstM : BOOLEAN;
      OneMask : STRING;
BEGIN
  FirstM := TRUE;
  WHILE NMasks<>'' DO BEGIN
    IF Pos ('/', NMasks) > 0 THEN BEGIN
      OneMask := Copy (NMasks, 1, Pos ('/', NMasks));
      Dec (OneMask [0]);
    END ELSE
      OneMask := NMasks;
    FileBox^.ReadDirectory (CurDir+'\'+OneMask, FirstM);
      { fÅr jede Suchmaske die entsprechenden EintrÑge suchen lassen }
    FirstM := FALSE;
    IF Pos ('/', NMasks) > 0 THEN
      System.Delete (NMasks, 1, Pos ('/', NMasks))
    ELSE
      NMasks := '';
  END;
  FileBox^.FocusItem (0);
  FileBox^.DrawView;
END;

(* ---------------------------------------------------------------- *)
(* SetDirectory muss den "Title" des Fensters anpassen und die Liste*)
(* neu erstellen lassen.                                            *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileWindow.SetDirectory (NewDir : STRING);
  CONST NewDirStr : STRING = '';
BEGIN
  CurDir := NewDir;  NewDirStr := NewDir;
  DisposeStr (Title);
  Title := NewStr (NewDirStr);
  Lock;
  ReadDirectory (CurDir, Masks);
  ReDraw;
  DrawView;
  UnLock;
END;

PROCEDURE TFileWindow.SetMasks (NewMasks : STRING); 
BEGIN
  Masks := NewMasks;
  ReadDirectory (CurDir, Masks);
END;

FUNCTION TFileWindow.GetMasks : STRING;
BEGIN
  GetMasks := Masks;
END;

FUNCTION TFileWindow.GetDirectory : STRING;
BEGIN
  GetDirectory := CurDir;
END;

(* ================================================================ *)
(*                         TFileApplication                         *)
(* ================================================================ *)
(* Das Flag NewWinBySel gibt an, ob bei der Anwahl eines            *)
(* Verzeichnisses in einem TFileWindow das neue Verzeichnis im      *)
(* gleichen Fenster (NewWinBySel=FALSE) oder in einem neuen Fenster *)
(* (TRUE) dargestellt werden soll (siehe HandleEvent).              *)
(* ---------------------------------------------------------------- *)
CONSTRUCTOR TFileApplication.Init;
  VAR R : TRect;
BEGIN
  TApplication.Init;
  WinNr := 0;
  NewWinBySel := TRUE;
END;

(* ---------------------------------------------------------------- *)
(* Frischt MenÅ und Statuszeile auf, dh lÑsst cmTile und cmCascade  *)
(* zu, sobald ein Fenster geîffnet ist, das auf diese Befehle rea-  *)
(* giert.                                                           *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileApplication.Idle;

FUNCTION IsTileable (P: PView): BOOLEAN; FAR;
BEGIN
  IsTileable := P^.Options AND ofTileable <> 0;
END;

BEGIN
  TApplication.Idle;
  IF Desktop^.FirstThat (@IsTileable) <> NIL THEN
    EnableCommands ([cmTile, cmCascade])
  ELSE
    DisableCommands ([cmTile, cmCascade]);
END;

PROCEDURE TFileApplication.OutOfMemory;
BEGIN
  MessageBox('Not enough memory available to complete operation.',
    nil, mfError + mfOkButton);
END;

(* ---------------------------------------------------------------- *)
(* HandleEvent reagiert auf einige Befehle und Broadcasts:          *)
(* Befehle:                                                         *)
(* - cmOpenFileWin : Ein neues TFileWindow wird mit dem aktuellen   *)
(*   Verzeichnis                                                    *)
(* - cmTile, cmCascade : Desktop^.Tile bzw. Desktop^.Cascade werden *)
(*   aufgerufen.                                                    *)
(* - cmToggleNewWin : Schaltet Flag NewWinBySel um (siehe Init).    *)
(* - cmDosShell : Command.Com wird gestartet.                       *)
(* - ClearDesktop : Alle Views werden vom Desktop genommen.         *)
(* - cmEnterNewMask : öber eine Inputbox kann eine/mehrere neue     *)
(*   Suchmaske(n) eingeben werden.                                  *)
(* - cmEnterNewDir : Ein neues Verzeichnis/Laufwerk kann eingegen   *)
(*   werden.                                                        *)
(* BroadCast:                                                       *)
(* - cmItemSelected : Ist der Item ein Verzeichnis, so wird in Ab-  *)
(*   hÑngigkeit von NewWinBySel entweder ein neues Fenster geîffnet *)
(*   oder der Inhalt des aktiven angepasst. Ist der Item eine Datei,*)
(*   so wird, sofern es eines ist, das Programm gestartet.          *)
(* ---------------------------------------------------------------- *)
PROCEDURE TFileApplication.HandleEvent (VAR Event : TEvent);

  (* -------------------------------------------------------------- *)
  (* Desktop aufrÑumen. Dazu wird an alle Views der Befehl cmClose  *)
  (* gesandt. Damit es nicht allzu sehr flackert, wird die Ausgabe  *)
  (* zwischenzeitlich blockiert (Lock).                             *)
  (* -------------------------------------------------------------- *)
  PROCEDURE ClearDesktop;
    PROCEDURE CloseView (P: PView); FAR;
    BEGIN
      Message (P, evCommand, cmClose, NIL);
    END;
  BEGIN
    Desktop^.Lock;
    IF Desktop^.Valid (cmClose) THEN
      Desktop^.ForEach(@CloseView);
    Desktop^.UnLock;
    WinNr := 0;
  END;

  (* -------------------------------------------------------------- *)
  (* DosShell, aus TVDEMO Åbernommen. Falls ein Programm aufgefÅhrt *)
  (* werden soll, so muss sein VOLLSTéNDIGER Name (Pfad+Name) Åber- *)
  (* geben werden. Vor dem Aufruf von Exec wird der Speicher so weit*)
  (* als mîglich freigegeben, hinterher alles wieder neu            *)
  (* initialisiert und der gesamte Bildschirm nochmal dargestellt.  *)
  (* -------------------------------------------------------------- *)
  PROCEDURE DosShell (Prog, CmdLine : STRING);
  BEGIN
    DoneSysError;
    DoneEvents;
    DoneVideo;
    DoneMemory;
    SetMemTop (HeapPtr);
    SwapVectors;

    Exec (Prog, CmdLine);

    SwapVectors;
    SetMemTop (HeapEnd);
    InitMemory;
    InitVideo;
    InitEvents;
    InitSysError;
    Redraw;
  END;

  (* -------------------------------------------------------------- *)
  (* Ein TFileWindow fÅr das Verzeichnis Path mit den Masken Masks  *)
  (* îffnen. Zuerst wird mit einem BroadCast cmDirInWin geprÅft, ob *)
  (* ein Fenster dieses Verzeichnis bereits darstellt. Wenn ja, wird*)
  (* es in den Vordergrund geholt (MakeFirst). Sonst wird die Grîsse*)
  (* des neuen Fensters von dem aktiven, falls schon eins sichtbar  *)
  (* ist, Åbernommen. Dabei wird vorausgesetzt, dass die aktive View*)
  (* ein TFileWindow ist. Das wird von DoAction ÅberprÅft, das diese*)
  (* Prozedur aufruft. Dann wird das Fenster in die ArbeitsflÑche   *)
  (* eingefÅgt. Falls noch keine View eingefÅgt ist, wird das Fen-  *)
  (* ster zentiert.                                                 *)
  (* -------------------------------------------------------------- *)
  PROCEDURE DoOpenFileWin (Path, Masks : STRING);
    CONST
      Dir : STRING = '';
    VAR
      R : TRect;
      FW : PFileWindow;
  BEGIN
    Dir := Path;
    FW := Message (Desktop, evBroadCast, cmDirInWin, @Dir);
    IF FW<>NIL THEN
      FW^.MakeFirst
    ELSE BEGIN
      IF Desktop^.Current <> NIL THEN BEGIN
	Desktop^.Current^.GetBounds (R);
	Inc (R.A.X); Inc (R.A.Y);
      END ELSE
	R.Assign (0, 0, 47, 18);

      Inc (WinNr);  { mitzÑhlen Anzahl Fenster }

      FW := New (PFileWindow,
		 Init (R, WinNr, UpString (Path), Masks));
      FW^.Options := FW^.Options OR ofTileable;
      IF Desktop^.Current=NIL THEN
	FW^.Options := FW^.Options OR ofCentered;
      Desktop^.Insert (ValidView (FW));
      ClearEvent (Event);
    END;
  END;

  (* -------------------------------------------------------------- *)
  (* DoAction wird aufgerufen, wenn HandleEvent ein cmItemSelected  *)
  (* abarbeitet. In CurDir wird zunÑchst das Verzeichnis des aktiven*)
  (* TFileWindow's eingetragen. Dabei wird angenommen, das die      *)
  (* aktive View ein TFileWindow ist, da nur dieses cmItemSelected  *)
  (* aussendet. Ist der in Event.InfoPtr Åbergebene Item ein        *)
  (* Directory, so wird ein neues Fenster geîffnet (NewWinBySel=    *)
  (* TRUE) mit dem neuen Verzeichnis oder dem aktiven ein neues Ver-*)
  (* zeichnis zugewiesen. Ist der Item ein ausfÅhrbares Programm,   *)
  (* so wird die DosShell mit CurDir+'\'+Info^.Name aufgerufen.     *)
  (* Parameter werden dem Programm keine mitgegeben.                *)
  (* -------------------------------------------------------------- *)
  PROCEDURE DoAction;
    VAR
      Info   : PSearchRec;
      CurDir : STRING;
      TopFW  : PFileWindow;
  BEGIN
    TopFW := PFileWindow (Desktop^.Current);
    CurDir := TopFW^.GetDirectory;
    Info := Event.InfoPtr;
    IF (Info^.Attr AND Directory > 0) THEN BEGIN
      IF NewWinBySel THEN
	DoOpenFileWin (GetNewDir (CurDir, Info), TopFW^.GetMasks)
      ELSE
	TopFW^.SetDirectory (GetNewDir (CurDir, Info));
    END ELSE 
      IF (Pos ('.COM', Info^.Name) > 0) OR
	 (Pos ('.EXE', Info^.Name) > 0) OR
	 (Pos ('.BAT', Info^.Name) > 0) THEN
	DosShell (CurDir+'\'+Info^.Name, '');
  END;

  (* -------------------------------------------------------------- *)
  (* DoTile und DoCascade brauchen nur die Grîsse des Desktop's     *)
  (* festzustellen, den Rest Åbernimmt dieser selber.               *)
  (* -------------------------------------------------------------- *)
  PROCEDURE DoTile;
    VAR R : TRect;
  BEGIN
    Desktop^.GetExtent (R);
    Desktop^. Tile (R);
  END;

  PROCEDURE DoCascade;
    VAR R : TRect;
  BEGIN
    Desktop^.GetExtent (R);
    Desktop^.Cascade (R);
  END;

  (* -------------------------------------------------------------- *)
  (* EnterNewMask fordert den Benutzer in einer InputBox auf, eine  *)
  (* neue Suchmaske einzugeben (mehrere kînnen durch "/" getrennt   *)
  (* eingegeben werden), doch nur, wenn die aktive View vom Typ     *)
  (* TFileWindow ist.                                               *)
  (* -------------------------------------------------------------- *)
  PROCEDURE EnterNewMask;
    VAR w : WORD;
	FW: PFileWindow;
	s : STRING;
  BEGIN
    IF TypeOf (Desktop^.Current^)=TypeOf (TFileWindow) THEN BEGIN
      FW := PFileWindow (Desktop^.Current);
      s := FW^.GetMasks;
      w := InputBox (' Eingabe der neuen Suchmaske(n) ',
		     '', s, 40);
      IF (w=cmOk) THEN
	FW^.SetMasks (UpString (s));
    END;
  END;

  (* -------------------------------------------------------------- *)
  (* Ist die aktive View ein TFileWindow, so wird ihr Verzeichnis   *)
  (* in die InputBox Åbernommen. Ist das eingegebene Verzeichnis    *)
  (* gÅltig, so wird, wieder in AbhÑngigkeit von NewWinBySel,       *)
  (* reagiert, falls die Eingabe nicht abgebrochen wurde.           *)
  (* -------------------------------------------------------------- *)
  PROCEDURE EnterNewDir;
    VAR w : WORD;
	FW: PFileWindow;
	m, s : STRING;
  BEGIN
    IF TypeOf (Desktop^.Current^)=TypeOf (TFileWindow) THEN BEGIN
      FW := PFileWindow (Desktop^.Current);
      s := FW^.GetDirectory;
    END ELSE BEGIN
      FW := NIL;
      s := '';
    END;
    m := '*.*';
    w := InputBox (' Eingabe des neuen Verzeichnisses ',
		   '', s, 255);
    IF s='' THEN
      s := GetDir (0);
    IF (PathValid (s)) AND (w=cmOk) THEN BEGIN
      IF s [Length (s)]='\' THEN Dec (s [0]);
      IF (NewWinBySel) OR (WinNr=0) THEN
	DoOpenFileWin (UpString (s), m)
      ELSE
	IF (w=cmOk) AND (FW <> NIL) THEN
	  FW^.SetDirectory (UpString (s));
    END;
  END;

BEGIN
  IF (Event.What=EvCommand) AND
     (Event.Command=cmClose) AND
     (WinNr > 0) THEN
    Dec (WinNr);
    { mitzÑhlen, muss aber vor der Behandlung durch
      TApplication.HandleEvent erledigt werden, weil danach das
      Ereignis schon abgearbeitet ist und auch als solches
      gekennzeichnet. }

  TApplication.HandleEvent (Event);

  IF Event.What=EvBroadCast THEN
    CASE Event.Command OF
      cmItemSelected : DoAction;
    END;

  IF Event.What=EvCommand THEN
    CASE Event.Command OF
      cmOpenFileWin : DoOpenFileWin (GetDir (0), '*.*');
      cmTile        : DoTile;
      cmCascade     : DoCascade;
      cmToggleNewWin: NewWinBySel := NOT NewWinBySel;
      cmDosShell    : DosShell (GetEnv('COMSPEC'), '');
      cmClearDesk   : ClearDesktop;
      cmEnterNewMask: EnterNewMask;
      cmEnterNewDir : EnterNewDir;
    END;
END;

END.
(* ---------------------------------------------------------------- *)
(*                         Ende von FILEWIN.PAS                     *)
(* ---------------------------------------------------------------- *)
