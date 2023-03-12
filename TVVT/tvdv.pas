Program TVDV;
(*{$DEFINE DEMO}*)

{$M 65520,8192,655360}
{$X+,S+,O+,F+}
{$G-}
{$W+}
Uses
   (*  TvGraph,*)
{$IFNDEF FPC}
     Overlay,
{$ENDIF}
     App,
     TvdvCmds,
     HelpCmds,
     HelpFile,
     Crt,
     Dos,
     Objects,
     Memory,
     GadGets,
     Drivers,
     Views,
     Menus,
     Dialogs,
     MsgBox,
     LabHist,
     Editors,
   (* FÅr Editor: *)
     Editor,
     Strings,
     Colors,
     TVDVImp,
     CursCont,
     FileFind,
     FileShow,
     StdDlg,
     StrTools,
     DDN,
     EMS,
     XMS,
     XHeap,
     DEdit,
     Video;

{$IFNDEF FPC}
{$O TvdvImp }
{$O CursCont}
{$O FileShow}
{$O Labhist}
{$O GadGets}
{$O HelpFile}
{$O Memory}
{$O MsgBox}
{$O Dialogs}
{$O Views}
{$O Menus}
{$O App}
{$O LabHist}
{$O Editors}
{$O StdDlg}
{$O DDN}
(*
{$O FileFind}
{$O DEdit     Geht nicht}
{$O TVDVDATA  Geht nicht}
{$O TvdvCmds  Geht nicht}
{$O HelpCmds  Geht nicht}
{$O Crt       Geht nicht}
{$O Drivers   Geht nicht}
{$O Editor    Geht nicht}
{$O Strings   Geht nicht}
{$O Colors    Geht nicht}
{$O StrTools  Geht nicht}
{$O Objects   Runtime Error 208}
{$O Dos       Runtime Error 208}
{$O EMS		  ?}
{$O XMS       ?}
{$O XHeap     ?}
*)
{$ENDIF}

(*{$O StrTools }*)
(*{$O Dos }*)
(*{$O Objects }*)

Type

  ColorFiletyp         = File of TPalette;


  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Heap        : PHeapView;
    Editor: ARRAY[1..9] OF RECORD
      Ed: PEditorWindow;
      Name: STRING;
    END;
    Constructor Init;
    Destructor  Done;                           Virtual;
    Procedure   Idle;                           Virtual;
    PROCEDURE   NewEditor(Name: STRING);
    Procedure   InitClipBoard;
    FUNCTION    GetFile(Arg: STRING): PCollection;
    PROCEDURE   SendFile(WindowNo, cmBase: WORD);
    PROCEDURE   SaveFile (WindowNo: WORD; P: PCollection );
    PROCEDURE   SaveFileAs (WindowNo: WORD; P: PCollection );
    Function    GetPalette: PPalette;           Virtual;
    Procedure   GetEvent(Var Event: tEvent);    Virtual;
    Procedure   HandleEvent(Var Event: TEvent); Virtual;
    Procedure   InitMenuBar;                    Virtual;
    Procedure   InitStatusLine;                 Virtual;
    Procedure   OutOfMemory;                    Virtual;
  END;




var

    Laufwerk     : String[12];

    ColorFile    : ColorFiletyp;
    ColorDat     : PPalette;
    S            : TStream;



(* Editor: *)

    EDCommand, WindowNo: WORD;
    ClipWindow: PEditorWindow;


function CalcHelpName: PathStr;
var
  EXEName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
begin
  if Lo(DosVersion) >= 3 then EXEName := ParamStr(0)
  else EXEName := FSearch('TVDV.EXE', GetEnv('PATH'));
  FSplit(EXEName, Dir, Name, Ext);
  if Dir[Length(Dir)] = '\' then Dec(Dir[0]);
  CalcHelpName := FSearch('TVDVHELP.HLP', Dir);
end;


PROCEDURE ProgramminformationDialog;
  VAR
    R: tRect;
    View: pView;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (13, 2, 62, 20);
  Dialog := New (pDialog, Init (R, 'Programminformation'));

{$IFDEF DEMO}


  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Testversion !')));

{$ELSE}

  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Voll geil !')));

{$ENDIF}

  R.Assign (10, 4, 37, 5);
  Dialog^.Insert (New (pStaticText, Init (R, 'Katalogsystem fÅr Disketten')));

  R.Assign (16, 5, 31, 6);
  Dialog^.Insert (New (pStaticText, Init (R, 'und Festplatten')));

  R.Assign (13, 7, 34, 8);
  Dialog^.Insert (New (pStaticText, Init (R, 'Copyright (c) 1993 by')));

  R.Assign (14, 10, 28, 11);
  Dialog^.Insert (New (pStaticText, Init (R, 'Lothar Behrens')));

  R.Assign (14, 11, 26, 12);
  Dialog^.Insert (New (pStaticText, Init (R, 'Quellenweg 1')));

  R.Assign (14, 12, 34, 13);
  Dialog^.Insert (New (pStaticText, Init (R, 'W-74889 Sinsheim Ad.')));

  R.Assign (14, 13, 26, 14);
  Dialog^.Insert (New (pStaticText, Init (R, '(07261) 4671')));

  R.Assign (18, 15, 28, 17);
  Dialog^.Insert (New (pButton, Init (R, ' ~O~K ', 10, 1)));

  Dialog^.SelectNext (FALSE);

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
  END;
  IF Dialog <> NIL THEN
    Dispose (Dialog, Done);
END;

FUNCTION TDateiverApp.GetPalette: PPalette;
CONST
  CNewColor = CColor + CHelpColor;
  CNewBW    = CBlackWhite + CHelpBlackWhite;
  CNewMono  = CMonochrome + CHelpMonochrome;
  P : ARRAY [apColor..apMonochrome] OF String[Length(CNewColor)] =
              (CNewColor, CNewBW, CNewMono);
BEGIN
  GetPalette := @P[AppPalette];
END;

PROCEDURE TDateiverApp.GetEvent (VAR Event : TEvent);
VAR
  W: PWindow;
  HFile: PHelpFile;
  HelpStrm: PDosStream;
CONST
  HelpInUse: Boolean = False;
BEGIN
  TApplication.GetEvent(Event);
  case Event.What of
    evCommand:
      if (Event.Command = cmHelp) and not HelpInUse then
      begin
        HelpInUse := True;
        HelpStrm := New(PDosStream, Init(CalcHelpName, stOpenRead));
        HFile := New(PHelpFile, Init(HelpStrm));
        if HelpStrm^.Status <> stOk then
        begin
          MessageBox('Kann Hilfedatei nicht îffnen.', nil, mfError + mfOkButton);
          Dispose(HFile, Done);
        end
        else
        begin
          W := New(PHelpWindow,Init(HFile, GetHelpCtx));
          if ValidView(W) <> nil then
          begin
            ExecView(W);
            Dispose(W, Done);
          end;
          ClearEvent(Event);
        end;
        HelpInUse := False;
      end
      Else
        if (Event.Command = cmHelpContents) and not HelpInUse then
        begin
          HelpInUse := True;
          HelpStrm := New(PDosStream, Init(CalcHelpName, stOpenRead));
          HFile := New(PHelpFile, Init(HelpStrm));
          if HelpStrm^.Status <> stOk then
          begin
            MessageBox('Kann Hilfedatei nicht îffnen.', nil, mfError + mfOkButton);
            Dispose(HFile, Done);
          end
          else
          begin
            W := New(PHelpWindow,Init(HFile, hcContents));
            if ValidView(W) <> nil then
            begin
              ExecView(W);
              Dispose(W, Done);
            end;
            ClearEvent(Event);
          end;
          HelpInUse := False;
        end;

  end;
end;


Constructor TDateiverApp.Init;
var
  H: Word;
  R: TRect;
  Dummy: STRING;
  Search: SearchRec;
  I: INTEGER;
  F: Text;
  S: String;

BEGIN

  cha := ' ';
  Nachgetragen := True;
  Raus := 'A';
  for ch := 'A' to 'J' do
  BEGIN
    MediumFeld[ch].DateiDif := False;
    MediumFeld[ch].PfadDif := False;
    MediumFeld[ch].LabelDif := False;
    With MediumFeld[ch] do
    BEGIN
      n  := '';
      L  := Nil;
      P  := Nil;
      D  := Nil;
      S  := Nil;
      es := Nil;
      as := Nil;
      H  := Nil;
      eh := Nil;
      ah := Nil;
      Z  := Nil;
      ez := Nil;
      na := Nachgetragen;
    end
  END;
  M[1] := MediumFeld['A'];
  M[2] := MediumFeld['A'];
  N := '';

  SetallNil;
  LabelHistList := Nil;

  Initfiles;

  Laufwerk := '';

  TApplication.Init;

  ColorDat := Application^.GetPalette;
  Assign(ColorFile, GetFName + '.COL');
  If (LastMode <> Mono) and
     (LastMode <> BW80) then
  Begin
    {$I-}
    Reset(ColorFile);
    If IOResult = 0 then
    Begin
      Read(ColorFile, S);
      Close(ColorFile);
      ColorDat^ := S;
      DeskTop^.ReDraw
    End;
    {$I+}
  End;

  RegisterHelpFile;


  DisableCommands([cmPruefen,
                   cmSpeichern,
                   cmZeigeDaten,
                   cmNachtrag,
                   cmAufreum,
                   cmDatenAufgeben               ]);


(* Editor: *)

  DisableCommands([cmEditorSave,
                   cmEditorSaveAs,
                   cmTile,
                   cmCascade,
                   cmNext,
                   cmPrev]);

{$IfDef DEMO}
  DisableCommands([cmDEdit]);
{$EndIf}


  GetExtent(R);
  R.A.X := R.B.X - 9;
  R.B.Y := R.A.Y + 1;
  Heap := New(PHeapView, Init(R));
  Insert(Heap);

  ProgrammInformationDialog;

  InitClipBoard;
  I := 1;
  WNo := 1;


END;

FUNCTION ReadFile(Name: STRING): PCollection;
  VAR
    P: PCollection;
    ALine: STRING;
    AFile: TEXT;
  BEGIN
    New(P, Init(20,1));
    Assign(AFile, Name);
    {$I-}Reset(AFile);{$I+}
    IF IOResult = 0 THEN BEGIN
      WHILE NOT EOF(AFile) DO BEGIN
        ReadLn(AFile, ALine);
        P^.AtInsert( P^.Count, New(PStr, Init ( ALine )));
      END;
      Close(AFile);
    END;
    ReadFile := P;
  END;


Destructor TDateiverApp.Done;
Begin
  ReWrite(ColorFile);
  Write(ColorFile, ColorDat^);
  Close(ColorFile);
  TApplication.Done
END;

(* FÅr Editor: *)

PROCEDURE TDateiverApp.NewEditor(Name: STRING);
  VAR
    R: TRect;
	Dir: DirStr;
    FName: NameStr;
    Ext: ExtStr;
  BEGIN
    R.Assign(0,0,80,23);
    FSplit(Name, Dir, FName, Ext);
    {Ext := DownStr(Ext);}
    IF Name = ''
      THEN Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,
                                               edBase+10*WNo))
     ELSE IF (Ext = '.txt') THEN BEGIN
       Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, Name, ReadFile(Name),
                                           edBase+10*WNo));
       Editor[WNo].Name := Name;
      END
      ELSE IF (Ext<>'.exe') AND (Ext<>'.com') AND (Ext<>'.bak') THEN BEGIN
        Editor[WNo].Ed := New(PLineEditor, Init(R, WNo, Name, ReadFile(Name),
                                                128, edBase+10*WNo));
        Editor[WNo].Name := Name;
       END
       ELSE Exit;
    DeskTop^.Insert(Editor[WNo].Ed);
    Inc(WNo);
  END;

PROCEDURE TDateiverApp.InitClipBoard;
  VAR
    R: TRect;
	Dir: DirStr;
    FName: NameStr;
    Ext: ExtStr;
    Name: String;
  BEGIN
    Name := '';
    GetExtent(R);
    Dec(R.B.Y, 2);
    FSplit(Name, Dir, FName, Ext);
    {Ext := DownStr(Ext);}

    Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,
                                               edBase+10*WNo));

    ClipWindow := Editor[WNo].Ed;
    if ClipWindow <> nil then
      Clipboard := ClipWindow^.Editor;

    ClipWindow^.Close;
    DeskTop^.Insert(Editor[WNo].Ed);

  END;



PROCEDURE TDateiverApp.SaveFile(WindowNo: WORD; P: PCollection);
  VAR
    TF: TEXT;
    I: INTEGER;
  BEGIN
    IF WindowNo > 1 THEN BEGIN
      Assign(TF,Editor[WindowNo].Name);
      Rewrite(TF);
      FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);
      Close(TF);
    END;
  END;

PROCEDURE TDateiverApp.SaveFileAs(WindowNo: WORD; P: PCollection);
  VAR
    TF: TEXT;
    I: INTEGER;
    D: PFileDialog;
    FileName: PathStr;

  BEGIN
    IF WindowNo > 1 THEN BEGIN
      D := PFileDialog(ValidView(New(PFileDialog, Init(Editor[WindowNo].Name, 'Datei speichern als',
        '~N~ame', fdOkButton, 100))));
      If D <> Nil then
      Begin
        If Desktop^.ExecView(D) <> cmCancel then
        Begin
          D^.GetFileName(FileName);
          Editor[WindowNo].Name := FileName;

          Assign(TF,Editor[WindowNo].Name);
          Rewrite(TF);
          FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);
          Close(TF);
        End;
        Dispose(D, Done)
      End;
    END;
  END;



FUNCTION TDateiverApp.GetFile(Arg: STRING): PCollection;
  VAR
    D: PFileDialog;
    FileName: PathStr;
    P: PCollection;
  BEGIN
    D := PFileDialog(ValidView(New(PFileDialog, Init(Arg, 'Laden',
                                                'File', 3, 12))));
    IF (D <> Nil) AND (Desktop^.ExecView(D) <> cmCancel) THEN BEGIN
      D^.GetFileName(FileName);
      GetFile := ReadFile(FileName);
     END
     ELSE GetFile := Nil;
  END;

PROCEDURE TDateiverApp.SendFile(WindowNo, cmBase: WORD);
  VAR Event: TEvent;
  BEGIN
    Event.What := evBroadCast;
    Event.Command := cmBase+WindowNo*10+cmEdSend;
    Event.InfoPtr := GetFile('*.*');
    TopView ^.HandleEvent(Event);
  END;

(*****************************************************************)

Procedure TDateiverApp.HandleEvent(Var Event: TEvent);
Var R: TRect;

(* FÅr Editor: *)

  FUNCTION HandleBroadCast: BOOLEAN;
    BEGIN
      IF Event.Command DIV 100 * 100 = edBase THEN BEGIN
        HandleBroadCast := True;
        WindowNo := Event.Command MOD 100 DIV 10;
        EDCommand := Event.Command MOD 10;
        CASE EDCommand OF
          cmEdSave: SaveFile(WindowNo, Event.InfoPtr);
          cmEdSaveAs: SaveFileAs(WindowNo, Event.InfoPtr);
          cmEdSaveAndExit: SaveFile(WindowNo, Event.InfoPtr);
          cmEdRead: SendFile(WindowNo,edBase);
          cmEdExit: Dispose(Editor[WindowNo].Ed, Done);
          ELSE HandleBroadCast := False;
        END;
       END
       ELSE HandleBroadCast := False;
    END;



procedure FileOpen;
var
  D: PFileDialog;
  FileName: PathStr;
  W: PWindow;
begin
  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'ôffnen einer Datei',
    '~N~ame', fdOpenButton, 100))));
  if D <> nil then
  begin
    if Desktop^.ExecView(D) <> cmCancel then
    begin
      D^.GetFileName(FileName);
      NewEditor(FileName);
    end;
    Dispose(D, Done);
  end;
end;
(************************************************************)

procedure FileImport;
var
  D: PFileDialog;
  FileName: PathStr;
  W: PWindow;
begin
  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'Importieren einer Datei',
    '~N~ame', fdOkButton, 100))));
  if D <> nil then
  begin
    if Desktop^.ExecView(D) <> cmCancel then
    begin
      D^.GetFileName(FileName);
      Import(FileName);
    end;
    Dispose(D, Done);
  end;
end;


Procedure Scanner;
var ch    : char;
    LName : String;
    Lauf  : StrucktListPtrtyp;
BEGIN
  LabelHistList := Nil;
  LName := '';
  if Nachgetragen = True then
  BEGIN
    Dir(LName);
    if DirStatus then
    Nachgetragen := False
  end
  else
    MessageBox('Es sind noch Daten im Arbeitsbereich!',
         nil, mfError + mfOkButton)
END;


Procedure Scann;
var Info     : ^String;
    Zahl     : String[5];

BEGIN
  LabelHistList := Nil;
  New(Info);
  Repeat
    info^ := Laufwerk;
    If ExecDialog(ReadString('Welches Laufwerk'), Info) = cmCancel then Exit;
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  if System.MemAvail > 1024 then
    Scanner
  Else
    Wait('Speicherplatz reicht nicht mehr aus fÅr diese Aufgabe!')
END;

Procedure DiskAS;

var  Ch            : char;
     LName         : String;

BEGIN
  LabelHistList := Nil;

  LName := '';

  repeat
    Mask := Laufwerk + ':\*.*';
    N := Mask;
    (*$I-*)
    if System.MemAvail > 1024 then
    BEGIN
      Dir(LName);
      If LName <> '' then
        If LabelInLabelHistList(LName) then
          MessageBox('Sie haben entweder eine Festplatte gewÑhlt, oder eine Disk ' +
                     'nochmal eingelegt!', nil, mfOkButton)
        else
          PutLabelInHistList(LName);

      If MessageBox('Noch eine Disk einlesen?',
              nil, mfInformation + mfYesButton + mfNoButton) =
         cmYes then
           Ch := 'J'
         else
           Ch := 'N'
    end
    else
    BEGIN
      MessageBox('Speicher ist voll !', Nil, mfOkButton);
      ch := 'N'
    end
    (*$I+*)
  until ch = 'N';

  Nachgetragen := False;

  DelLabelHistList
END;


Procedure DiskAutoScann;
Var Info : ^String;
BEGIN
  New(Info);
  Repeat
    info^ := Laufwerk;
    If ExecDialog(ReadString('Welches Laufwerk'), Info) = cmCancel then Exit;
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  DiskAS
END;


Procedure LoescheLPDTrees;

Procedure ReinigeLTree(Root: LabelListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeLTree(Root^.Links);
    ReinigeLTree(Root^.Rechts);
    Dispose(Root)

(*    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);*)
  End;
End;

Procedure ReinigePTree(Root: PfadListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigePTree(Root^.Links);
    ReinigePTree(Root^.Rechts);
    Dispose(Root)

(*    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);*)
  End;
End;

Procedure ReinigeDTree(Root: DateiListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeDTree(Root^.Links);
    ReinigeDTree(Root^.Rechts);
    Dispose(Root)

(*    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);*)
  End;
End;

Begin
  ReinigeLTree(LabelList);
  ReinigePTree(PfadList);
  ReinigeDTree(DateiList);
(*  If LabelList <> Nil then Dispose(LabelList);
  If PfadList <> Nil then Dispose(PfadList);
  If DateiList <> Nil then Dispose(DateiList)*)
End;


Procedure DatenAufgeben;
BEGIN
  CursorAus;
  If StrucktList <> Nil then
  Begin
    While StrucktList^.Next <> Nil do
    Begin
      StrucktList := StrucktList^.Next;
      Dispose(StrucktList^.Back)
    End;
    Dispose(StrucktList)
  End;

  If ZusatzList <> Nil then
  Begin
    While ZusatzList^.Next <> Nil do
    Begin
      ZusatzList := ZusatzList^.Next;
      Dispose(ZusatzList^.Back)
    End;
    Dispose(ZusatzList)
  End;

  If HilfList <> Nil then
  Begin
    While HilfList^.Next <> Nil do
    Begin
      HilfList := HilfList^.Next;
      Dispose(HilfList^.Back)
    End;
    Dispose(HilfList)
  End;

  LoescheLPDTrees;
  SetAllNil;

  CursorEin
END;



(***********************************************)
(*                                             *)
(*         Proceduren fÅr Nachtrag             *)
(*                                             *)
(***********************************************)



Procedure PutMxonMy(x,y: byte);

begin

(* Diese Prozedur hÑngt an die jeweiligen Listen x *)
(* die Listen aus y hinten an: *)

  with M[y] do
  begin
    eh^.Next := M[x].h;
    es^.Next := M[x].s;

    M[x].h^.Back := eh;
    M[x].s^.Back := es;
  end;
end;

Procedure CopyBaumDataMxtoBaumDataMy(x, y: Byte);
var Hilf1,
    Hilf2,
    Hilf3  : StrucktListPtrtyp;
begin
  Hilf3 := M[x].s;
  CopyMinWork(y);

  While M[x].s <> Nil do
  begin
    case M[x].s^.StrucktData.art of
      1 : begin
            If M[x].s^.StrucktData.LabelList^.LabelData.Anzahl > 0 then
            begin
              SpeichereLabel(M[x].s^.StrucktData.
                             LabelList^.LabelData.
                             LabelName,
                             LabelList,
                             LabelDif, NoNew);

              Dec (M[x].s^.StrucktData.LabelList^.LabelData.Anzahl);

              If M[x].s^.StrucktData.LabelList^.LabelData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.LabelList);


              M[x].s^.StrucktData.LabelList := labeladress;
              Hilf1 := M[x].s
            end
            Else
              Wait('LabelData.Anzahl ist <= 0')
          end;
      2 : begin
            If M[x].s^.StrucktData.PfadList^.PfadData.Anzahl > 0 then
            begin
              SpeicherePfad(M[x].s^.StrucktData.
                            PfadList^.PfadData.
                            PfadName,
                            PfadList,
                            PfadDif, NoNew);

              Dec(M[x].s^.StrucktData.PfadList^.PfadData.Anzahl);

              If M[x].s^.StrucktData.PfadList^.PfadData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.PfadList);


              M[x].s^.StrucktData.PfadList := pfadadress;
              Hilf2 := M[x].s
            end
            Else
              Wait('PfadData.Anzahl ist <= 0')
          end;
      3 : begin
            If M[x].s^.StrucktData.DateiList^.DateiData.Anzahl > 0 then
            begin
              Dateiadress := Nil;
              SpeichereDatei(M[x].s^.StrucktData.
                             DateiList^.DateiData.
                             DateiName,
                             DateiList,
                             DateiDif, NoNew);

              If Dateiadress = Nil then Halt;

              Dec(M[x].s^.StrucktData.DateiList^.DateiData.Anzahl);

              If M[x].s^.StrucktData.DateiList^.DateiData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.DateiList);


              M[x].s^.StrucktData.DateiList := dateiadress;

              SpeichereSuch(Hilf1,Hilf2,dateiadress);
              if Dateiadress^.dateidata.loc = Nil then Exit
            end
            Else
              Wait('DateiData.Anzahl ist <= 0')

          end
      Else Wait('Element ist ungÅltig');
    end;

    If M[x].s = Nil then Exit;
    M[x].s := M[x].s^.Next
  end;
  CopyWorkinM(y);
  M[x].s := Hilf3;

  (* Lîsche Zusatzliste, wenn vorhanden: *)

  While M[x].Z^.Next <> Nil do
  Begin
    M[x].Z := M[x].Z^.Next;
    Dispose(M[x].Z^.Back)
  End;
  Dispose(M[x].Z)

end;





Procedure Nachtr;
var Help     : HilfListPtrtyp;
    HilfLauf : HilfListPtrtyp;

Procedure LoescheDiskBereich;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    Lauf : StrucktListPtrtyp;

Procedure LoescheElement(Var Anfang,
                              Ende,
                              Element          : ListenPtrtyp);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure LoescheStrucktElement(var Anfang,
                                      Ende,
                                      Element          : StrucktListPtrtyp);
var Hilf : StrucktListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!');
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheHilfElement(var Anfang,
                                   Ende,
                                   Element          : HilfListPtrtyp);
var Hilf : HilfListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!');
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheZusatzElement(var Anfang,
                                   Ende,
                                   Element    : ZusatzListPtrtyp);
var Hilf: ZusatzListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!');
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;



Procedure LoescheLabel(var l: String);
begin
  l := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
  Dec(Lauf^.StrucktData.LabelList^.LabelData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)
end;

Procedure LoeschePfad(var p: String);
begin
  p := Lauf^.StrucktData.PfadList^.PfadData.PfadName;
  Dec(Lauf^.StrucktData.PfadList^.PfadData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)
end;

Procedure LoescheDatei(zl, zp : String);
var Zaehler : Byte;
    Help,
    LocHelp : ZusatzListPtrtyp;
begin
  Zaehler := 1;

  Help := Lauf^.StrucktData.DateiList^.DateiData.Loc;
  LocHelp := Help;

  If Help = Nil then
  Begin
    Wait('Dateielement hat keine verbindung mehr zur Zusatzliste!');
    Halt
  End;

  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          LabelList^.LabelData.LabelName) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          PfadList^.PfadData.PfadName)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler)
  end;

  If Lauf^.StrucktData.DateiList^.DateiData.Anzahl = 1 then
  begin
    If Help <> LocHelp Then
    Begin
      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )');
      Exit
    End;
    Lauf^.StrucktData.DateiList^.DateiData.Loc := Nil;
  end
  else
    If Help = LocHelp then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)

      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp^.Next;
    end
    Else
    Begin
      If Help = Nil then
      Begin
        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )');
        Exit
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp;
    End;



  LoescheZusatzElement(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.DateiList^.DateiData.Anzahl);

  If Lauf^.StrucktData.DateiList^.DateiData.Loc = Nil then
  Begin
    If Lauf^.StrucktData.DateiList^.DateiData.Anzahl > 0 then
    Begin
      Wait('Loc ist auf Nil gesetzt und Anzahl ist nicht 0');
      Halt
    End
  End;

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
end;


begin
  Wait('Lîsche Diskbereich (Hauptprogramm)');

(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)

  Lauf := HilfLauf^.HilfData.LabelList;

  (* Laufe die Strucktliste durch und lîsche die Daten: *)

  repeat
    case Lauf^.StrucktData.art of
      1 : LoescheLabel(l);
      2 : LoeschePfad(p);
      3 : LoescheDatei(l, p);
      Else
        Wait('Fehlerhaftes Element in Lîsche Diskbereich!');
    end

  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
end;


begin
(* Speichern der gescannten Daten in M1: *)

  CopyWorkinM(1);

(* ZurÅcksetzen des Mediums Work: *)

  SetAllNil;

  LadeDateien;

  CopyWorkinM(2);

  SetallNil;

  Help := M[1].h;

(* Entfernen von alten Daten: *)

  While Help <> Nil do
  begin
    HilfLauf := M[2].h;

    (* Suche Label in Diskdaten: *)

    While (HilfLauf <> Nil) and
          (HilfLauf^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName
           <>
           Help^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName)

          do HilfLauf := HilfLauf^.Next;

    (* Wenn nicht Nil, dann ist ein alter
       Label unter diesem Namen vorhanden *)

    If HilfLauf <> Nil then
      LoescheDiskBereich;

    (* Hole neuen Label aus gescannten Daten: *)

    repeat
      Help := Help^.Next
    until (Help^.HilfData.art = 1) or
          (Help = Nil)
  end; (* Von While M1... *)

  If M[2].s <> Nil then
  begin
    CopyBaumDataMxtoBaumDataMy(1, 2);
    PutMxonMy(1, 2);
    CopyMinWork(2)
  end
  else
    CopyMinWork(1);

  Erase(DateiDataFile);
  Erase(PfadDataFile);
  Erase(LabelDataFile);
  Erase(ZusatzDataFile);
  Erase(HilfDataFile);
  Erase(StrucktDataFile);

  SpeichereDateien
end;


Procedure Nachtr1;
Type AltLabelListPtrTyp = ^AltLabelListTyp;
     AltLabelListTyp    = Record
                            LabelName: String12;
                            Next     : AltLabelListPtrTyp
                          End;

var Help     : HilfListPtrtyp;
    HilfLauf : HilfListPtrtyp;
    SZahl    : String;
    Lauf     : StrucktListPtrtyp;




Procedure LoescheDiskBereich;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    VorDatei: String;

Procedure LoescheElement(Var Anfang,
                             Ende,
                             Element          : ListenPtrtyp);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure LoescheStrucktElement(var Anfang,
                                    Ende,
                                    Element          : StrucktListPtrtyp);
var Hilf : StrucktListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Struckt - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheHilfElement(var Anfang,
                                 Ende,
                                 Element          : HilfListPtrtyp);
var Hilf : HilfListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Hilf - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheZusatzElement(var Anfang,
                                   Ende,
                                   Element    : ZusatzListPtrtyp);
var Hilf: ZusatzListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Zusatz - Element lîschen, das nicht da ist!');
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheLabel(var l: String);
begin
  l := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
  Dec(Lauf^.StrucktData.LabelList^.LabelData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoeschePfad(var p: String);
begin
  p := Lauf^.StrucktData.PfadList^.PfadData.PfadName;
  Dec(Lauf^.StrucktData.PfadList^.PfadData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoescheDatei(zl, zp : String);
var Zaehler : Byte;
    Help,
    LocHelp : ZusatzListPtrtyp;
    Anz     : String;
begin

  Zaehler := 1;
  If Lauf = Nil then
  Begin
    Wait('Lauf ist Nil, Dateielement kann nicht gelîscht werden!');
    Halt
  End;

  If Lauf^.StrucktData.DateiList = Nil then
  Begin
    Wait('Lauf^.StrucktData.DateiList = Nil!');
    Halt
  End;

  Help := Lauf^.StrucktData.DateiList^.DateiData.Loc;

  LocHelp := Help;

  If Help = Nil then
  Begin
    Wait('Help = Nil!' + Chr(13) +
         'DateiData.Anzahl: ' +
          GetString(Lauf^.StrucktData.DateiList^.DateiData.Anzahl) + Chr(13) +
         'Datei: ' +
          Lauf^.StrucktData.DateiList^.DateiData.DateiName + Chr(13) +
         'Datei, die vorher gelîscht wurde: ' + VorDatei);
    Halt
  End;


  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          LabelList^.LabelData.LabelName) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          PfadList^.PfadData.PfadName)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler);
    If Zaehler > Lauf^.StrucktData.DateiList^.DateiData.Anzahl then
    Begin
      Wait('Zusatz - Element nicht gefunden!');
      Halt
    End
  end;

  If Lauf^.StrucktData.DateiList^.DateiData.Anzahl = 1 then
  begin
    If Help <> LocHelp Then
    Begin
      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )');
      Halt
    End;

    Lauf^.StrucktData.DateiList^.DateiData.Loc := Nil;

  end
  else
    If Help = LocHelp then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)
      If LocHelp^.Next = Nil then
      Begin
        Clrscr;
        Writeln('DateiData.Loc ist letztes Element, weiterrÅcken nicht mîglich!');
        Writeln('Anzahl: ', Lauf^.StrucktData.DateiList^.DateiData.Anzahl);
        Writeln('Datei : ', Lauf^.StrucktData.DateiList^.DateiData.DateiName);
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp^.Next;
    end
    Else
    Begin
      If Help = Nil then
      Begin
        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )');
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp;
    End;


  LoescheZusatzElement(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.DateiList^.DateiData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

end;



begin

  Lauf := HilfLauf^.HilfData.LabelList;


  If Lauf^.Back <> Nil then
    Wait('Strucktelement ist nicht am Anfang der Liste!');

  A := 0;
(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)



  (* Laufe die Strucktliste durch und lîsche die Daten: *)


  repeat
    Inc(A);

    Str(A, SZahl);
    case Lauf^.StrucktData.art of
      1 : LoescheLabel(l);
      2 : LoeschePfad(p);
      3 : Begin
            VorDatei := Lauf^.StrucktData.DateiList^.DateiData.DateiName;
            LoescheDatei(l, p)
          End
    Else
      Wait('Falsches Element in Strucktliste!');
    end


  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
end;



Procedure ReinigeLPDTrees;

Procedure ReinigeLTree(Root: LabelListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeLTree(Root^.Links);
    ReinigeLTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.LabelData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.LabelData.Anzahl = 0) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigePTree(Root: PfadListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigePTree(Root^.Links);
    ReinigePTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.PfadData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.PfadData.Anzahl = 0) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigeDTree(Root: DateiListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeDTree(Root^.Links);
    ReinigeDTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.DateiData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.DateiData.Anzahl = 0) then
      Dispose(Root^.Rechts);
  End;
End;

Begin
  ReinigeLTree(LabelList);
  ReinigePTree(PfadList);
  ReinigeDTree(DateiList);
End;

Var Df,
    Pf,
    Lf,
    Sf,
    Hf,
    Zf          : String12;

    AltLabelList,
    LHelp       : AltLabelListPtrTyp;
    HilfListHilf: HilfListPtrTyp;

    Mem1,
    Mem2,
    Mem3,
    Mem4,
    Mem5,
    Mem6,
    Mem7,
    Mem8,
    Mem9,
    Mem10       : Longint;

    SMem1       : String;

    AltName     : String;


Procedure FindeDoppeld;
Begin
  Lauf := StrucktList;
  AltName := '';

  While Lauf <> Nil do
  Begin
    If Not(Lauf^.StrucktData.Art In [1, 2, 3]) then
      Wait('Vor Speichern der neuen Daten ist in Strucktliste ein fehlerhaftes Element');
    If Lauf^.StrucktData.Art in [1, 2] then
    Begin
      If Lauf^.StrucktData.LabelList^.LabelData.LabelName = AltName then
        Wait('Gleicher Pfad oder Label hintereinander vorhanden!' + Chr(13) +
             'Vor Speichern der neuen Daten.');

      AltName := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
    End;
    Lauf := Lauf^.Next
  End;
End;

Procedure DisposeAltLabels;
Begin
  While AltLabelList^.Next <> Nil do
  Begin
    LHelp := AltLabelList^.Next;
    Dispose(AltLabelList);
    AltLabelList := LHelp
  End;
  Dispose(AltLabelList)
End;

Procedure SaveAltLabels;
Begin
  New(AltLabelList);
  LHelp := AltLabelList;

  HilfListHilf := HilfList;

  While HilfListHilf <> Nil do
  Begin
    If HilfListHilf^.HilfData.art = 1 then

    Begin
      New(LHelp^.Next);

      LHelp^.Next^.LabelName := HilfListHilf^.HilfData.LabelList^.
                               StrucktData.LabelList^.LabelData.
                               LabelName;
      LHelp := LHelp^.Next;
      LHelp^.Next := Nil
    End;

    HilfListHilf := HilfListHilf^.Next
  End;

  LHelp := AltLabelList^.Next;
  Dispose(AltLabelList);
  AltLabelList := LHelp
End;

begin

  (* Speichern der Labels in einer zusÑtzlichen Liste: *)

  SaveAltLabels;

  (*****************************************************)

  Df := Dateifilename;
  Pf := Pfadfilename;
  Lf := Labelfilename;
  Sf := Strucktfilename;
  Hf := Hilffilename;
  Zf := Zusatzfilename;

  Dateifilename   := 'Datei.tmp';
  Pfadfilename    := 'Pfad.tmp';
  Labelfilename   := 'Label.tmp';
  Strucktfilename := 'Struckt.tmp';
  Hilffilename    := 'Hilf.tmp';
  Zusatzfilename  := 'Zusatz.tmp';

  InitFiles;

  Video.SpeichereDateien;
  Wait('TemponÑre Dateien geschrieben');

  SetAllNil;



  Dateifilename   := Df;
  Pfadfilename    := Pf;
  Labelfilename   := Lf;
  Strucktfilename := Sf;
  Hilffilename    := Hf;
  Zusatzfilename  := Zf;

  InitFiles;

  OpenAllFiles;
  (* Abfrage, ob die Dateien in den Speicher passen: *)

  Mem1 := FileSize(DateiDatafile)   * SizeOf(DateiList^)   +
          FileSize(DateiDatafile)   * SizeOf(Pointer)      +

          FileSize(PfadDatafile)    * SizeOf(PfadList^)    +
          FileSize(PfadDatafile)    * SizeOf(Pointer)      +

          FileSize(LabelDatafile)   * SizeOf(LabelList^)   +
          FileSize(LabelDatafile)   * SizeOf(Pointer)      +

          FileSize(StrucktDatafile) * SizeOf(StrucktList^) +
          FileSize(StrucktDatafile) * SizeOf(Pointer)      +

          FileSize(ZusatzDatafile)  * SizeOf(ZusatzList^)  +
          FileSize(ZusatzDatafile)  * SizeOf(Pointer)      +


          FileSize(HilfDatafile)    * SizeOf(HilfList^);


  CloseAllFiles;

  If Mem1 > System.MemAvail then
  Begin
    DDNachtrag('.tmp', FileName);
    DisposeAltLabels;
    Exit
  End;


  Video.LadeDateien;
  Wait('Alte Daten geladen');

  CopyWorkInM(2);

  SetAllNil;

  Help := M[1].h;

(* Entfernen von alten Daten mit zuhilfenahme *)
(* von gesicherten Labels:   LHelp            *)

  While LHelp <> Nil do
  begin

    HilfLauf := M[2].h;

    (* Suche Label in Diskdaten: *)

    While (HilfLauf <> Nil) and
          (HilfLauf^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName
           <>
           LHelp^.LabelName)

          do HilfLauf := HilfLauf^.Next;

    (* Wenn nicht Nil, dann ist ein alter
       Label unter diesem Namen vorhanden *)


    If HilfLauf <> Nil then
    Begin
      LoescheDiskBereich;
    End;

    (* Hole neuen Label aus gescannten Daten: *)

    repeat
      LHelp := LHelp^.Next
    until LHelp = Nil
  end; (* Von While M1... *)

  HilfLauf := M[2].h;

  CopyMInWork(2);


(************************************************************)
(*                                                          *)
(* Unterscheidung, ob Medium leer ist oder noch andere      *)
(* Diskbereiche vorhanden sind:                             *)
(*                                                          *)
(************************************************************)

  If M[2].eh <> M[2].h then
  Begin
    Erase(DateiDataFile);
    Erase(PfadDataFile);
    Erase(LabelDataFile);
    Erase(ZusatzDataFile);
    Erase(HilfDataFile);
    Erase(StrucktDataFile);

    Video.SpeichereDateien;

    SetAllNil;

    Video.LadeDateien;

    CopyWorkInM(2);
    SetAllNil
  End
  Else
  Begin
    DatenAufgeben;
    CopyWorkInM(2);
    SetAllNil
  End;

  DisposeAltLabels;


  Df := Dateifilename;
  Pf := Pfadfilename;
  Lf := Labelfilename;
  Sf := Strucktfilename;
  Hf := Hilffilename;
  Zf := Zusatzfilename;

  Dateifilename   := 'Datei.tmp';
  Pfadfilename    := 'Pfad.tmp';
  Labelfilename   := 'Label.tmp';
  Strucktfilename := 'Struckt.tmp';
  Hilffilename    := 'Hilf.tmp';
  Zusatzfilename  := 'Zusatz.tmp';

  InitFiles;

  Video.LadeDateien;

  CopyWorkInM(1);

  (* Lîschen der temponÑren Dateien: *)

  Erase(DateiDataFile);
  Erase(PfadDataFile);
  Erase(LabelDataFile);
  Erase(ZusatzDataFile);
  Erase(HilfDataFile);
  Erase(StrucktDataFile);

  Dateifilename   := Df;
  Pfadfilename    := Pf;
  Labelfilename   := Lf;
  Strucktfilename := Sf;
  Hilffilename    := Hf;
  Zusatzfilename  := Zf;

  InitFiles;

  If M[2].s <> Nil then
  begin
    CopyBaumDataMxtoBaumDataMy(1, 2);
    PutMxonMy(1, 2);
    CopyMinWork(2);
  end
  else
    CopyMinWork(1);

  Video.SpeichereDateien
end;



Procedure Nachtrag;
Var MemNew : Longint;
    Lauf   : StrucktListPtrtyp;
    ZLauf  : ZusatzListPtrtyp;
BEGIN
  If Not(TestFiles) then
  Begin
    SpeichereDateien;
    SetAllNil;
    Exit
  End;

  OpenallFiles;
  Anzahl := FileSize(StrucktDataFile) +
            FileSize(HilfDataFile) +
            FileSize(ZusatzDataFile) +

            FileSize(LabelDataFile) +
            FileSize(PfadDataFile) +
            FileSize(DateiDataFile);

  MemNew := Anzahl * 2 * SizeOf(Pointer);

  MemNew := MemNew + FileSize(StrucktDataFile) * SizeOf(StrucktDataTyp);
  MemNew := MemNew + FileSize(HilfDataFile) * SizeOf(HilfDataTyp);
  MemNew := MemNew + FileSize(ZusatzDataFile) * SizeOf(ZusatzDataTyp);
  MemNew := MemNew + FileSize(LabelDataFile) * SizeOf(LabelDataTyp);
  MemNew := MemNew + FileSize(PfadDataFile) * SizeOf(PfadDataTyp);
  MemNew := MemNew + FileSize(DateiDataFile) * SizeOf(DateiDataTyp);

  CloseallFiles;

  (* Teste, ob Daten im Speicher auch nach dem Laden der alten Daten *)
  (* noch umgewandelt werden kînnen: *)

  Anzahl := 0;

  ZaehleElemente(GlobalBaumPtrtyp(DateiList));
  ZaehleElemente(GlobalBaumPtrtyp(PfadList));
  ZaehleElemente(GlobalBaumPtrtyp(LabelList));

  Lauf := StrucktList;
  While Lauf <> Nil do
  Begin
    Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  ZLauf := ZusatzList;
  While ZLauf <> Nil do
  Begin
    Inc(Anzahl);
    ZLauf := ZLauf^.Next
  End;

  Anzahl := Anzahl * SizeOf(Pointer);

  MemNew := MemNew + Anzahl + 100000;


  If MemNew >= System.MemAvail then
  Begin
    Wait('Nicht genug Speicher vorhanden.');
    CursorAus;
    OvrClearBuf;
    Nachtr1;
    OvrSetBuf(50 * 1024);
    CursorEin;
  End
  Else
  Begin
    Wait('Genug Speicher vorhanden.');
    CursorAus;
    OvrClearBuf;
    Nachtr;
    OvrSetBuf(50 * 1024);
    CursorEin;
  End;
  (*DatenAufgeben*)
  SetAllNil
END;

(***********************************************)
(*                                             *)
(*            Ende von Nachtrag                *)
(*                                             *)
(***********************************************)



Procedure Aufreum;
BEGIN
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
END;


Procedure LadeDateien;
BEGIN
  (*CursorAus;*)
  if StrucktList = nil then
    if TestFiles then
    Begin
      MessageBox('Lade Dateien.', nil, mfOkButton);
      Video.LadeDateien
    End
    else
      MessageBox('Es sind keine Dateien unter diesem Namen vorhanden!',
             nil, mfError + mfOkButton)

  else MessageBox('Es sind noch Daten im Arbeitsbereich!',
             nil, mfError + mfOkButton);
  (*CursorEin;*)
END;

Procedure SpeichereDateien;
BEGIN
  CursorAus;
  Video.SpeichereDateien;
  SetallNil;
  CursorEin;
END;

procedure ChangeDir;
BEGIN
  ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
  InitFiles
END;

Procedure LoadFileNames;
Begin
End;

Procedure SaveFileNames;
Begin
End;





Procedure ZeigeDaten;
BEGIN
  ZeigeDateien
END;


Procedure SucheMehrfache(Var SM: String);
BEGIN
  Mehrfache := True;

  ExecView(New(PFileFindWindow, Init(SM)));

  SM := FileFindWindow^.GetMaskBack;

  If FileFindWindow <> Nil then
    Dispose(FileFindWindow, Done);
  CloseallFiles;

  Mehrfache := False;
END;




PROCEDURE SuchmaskenVoreinstellungDialog(Var SuchMaske: String);
TYPE
    SuchmaskenVoreinstellungData = RECORD
      TextLen0: WORD;
      TextRec0: ARRAY [0..12] OF CHAR;
    END;

Type LaengenTyp = Record
                    Lo,
                    Hi : Byte
                  END;



  VAR
    R      : tRect;
    View   : pView;
    Data   : SuchmaskenVoreinstellungData;
    Code   : INTEGER;
    Dialog : pDialog;

    Laenge : LaengenTyp;



BEGIN
  R.Assign (20, 5, 64, 14);
  Dialog := New (pDialog, Init (R, 'Suchmasken - Voreinstellung'));

  R.Assign (16, 3, 28, 4);
  View := New (pMemo, Init (R, NIL, NIL, NIL, 12));
  Dialog^.Insert (View);
  R.Assign (15, 2, 22, 3);
  Dialog^.Insert (New (pLabel, Init (R, 'Maske:', View)));

  R.Assign (17, 6, 25, 8);
  Dialog^.Insert (New (pButton, Init (R, '~O~K', 10, 1)));

  Dialog^.SelectNext (FALSE);

  { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (SuchmaskenVoreinstellungData), 0);


  For A := 1 to Length(SuchMaske) do
    Data.TextRec0[A - 1] := SuchMaske[A];

  Data.TextLen0 := Length(SuchMaske);

  Dialog^.SetData (Data);

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN

    Dialog^.GetData (Data);


    For A := 1 to Data.TextLen0 do
      SuchMaske[A] := Data.TextRec0[A - 1];
    Word(Laenge) := Data.TextLen0;
    SuchMaske[0] := Char(Laenge.Lo);
    SuchMaske := UpDate(SuchMaske);



  END;
  IF Dialog <> NIL THEN
    Dispose (Dialog, Done);
END;



Procedure SucheDaten(Var SM: String);
Begin
  ExecView(New(PFileFindWindow, Init(SM)));

  SM := FileFindWindow^.GetMaskBack;

  If FileFindWindow <> Nil then
    Dispose(FileFindWindow, Done);
  CloseallFiles
end;


Procedure Memory;
Var LMem : Longint;
    SMem,
    EMem,
    XMem : Word;
    PA,
    PT   : Word;


Function GetVersion: String;
Begin
  GetVersion := EMS.GetVersion
End;


BEGIN
  EMem := 0;
  XMem := 0;

  LMem := System.MemAvail;

  If EMS.TestEMMDriver then
    EMS.EMMPageCount(EMem, PT)

  Else EMem := 0;

  XMem := 0;
  If XMS.Result = 0 then
    XMem := XMS.MemAvail

  Else XMem := 0;



  MessageBox('          Speicherplatz:' +
             Chr(13) +
             Chr(13) +
             '   Konventionell  : ' + GetString(System.MemAvail) + ' Byte.' +
             Chr(13) +
             Chr(13) +
             '   EMS ' + GetVersion + '        : ' + GetString(PT) + ' Seiten.' +
             Chr(13) +
             Chr(13) +
             '   EMS VerfÅgbar  : ' + GetString(EMem) + ' Seiten.' +
             Chr(13) +
             Chr(13) +
             '   XMS            : ' +
              GetString(XMem) + ' kByte.',
             Nil, mfInformation + mfOkButton);

END;

procedure ShowClip;
begin
  ClipWindow^.Select;
  ClipWindow^.Show;
end;



(* BEGIN von TDareiverApp.HandleEvent: *)

Const SM: String = '*.*'; (* SuchmaskenVoreinstellung *)

Var Regs: Registers;

BEGIN
  TApplication.HandleEvent(Event);



  case Event.What of
    evCommand:
    Begin
      GetExtent(R);
      Dec(R.B.Y, 2);

      case Event.Command of

      (* FÅr Editor: *)

        cmFileOpen            : FileOpen;

        cmIfCursor            : BEGIN
(*
                                  IfCursor := Not(IfCursor);
                                  CursorAus
*) 
                                END;
        cmColors              : Begin
                                  ColorDialog;
                                  DeskTop^.ReDraw
                                End;
        cmTile                : DeskTop^.Tile(R);
        cmCascade             : DeskTop^.Cascade(R);
        cmShowClip            : ShowClip;
        cmEditorSave          : Begin
                                  Event.What := evBroadCast;
                                  Event.Command := edBase + cmEdSave;
                                  PutEvent(Event);
                                  IF NOT HandleBroadCast THEN
                                    ClearEvent(Event);
                                End;
        cmEditorSaveAs        : Begin
                                  Event.What := evBroadCast;
                                  Event.Command := edBase + cmEdSaveAs;
                                  PutEvent(Event);
                                  IF NOT HandleBroadCast THEN
                                    ClearEvent(Event);
                                End;

(*******************************************************************)

        cmProgramminfo        : ProgrammInformationDialog;
        cmMemory              : Memory;
        cmDatenAufgeben       : DatenAufgeben;
        cmChangeDir           : ChangeDir;
        cmScann               : Scann;

        cmImport              : FileImport;
        cmIN                  : ImportNachtrag;
        cmIP                  : ImportPruefen;


{$IFDEF DEMO}
        cmDiskAutoScann       : Begin
                                  (* DiskAutoScann; *)
                                  Wait('Dies ist die Demoversion');
                                End;
{$ELSE}
        cmDiskAutoScann       : DiskAutoScann;
{$ENDIF}
        cmNachtrag            : Nachtrag;
        cmAufreum             : Aufreum;
        cmLaden               : LadeDateien;
        cmSpeichern           : SpeichereDateien;
        cmPruefen             : PruefeDaten('Teste Daten');
        cmNewFileName         : NewFileName('Dateiname fÅr Daten ?', FileName);
        cmChangeDir           : ChangeDir;
        cmDEdit               : DateneditorDialog;
        cmZeigeDaten          : ZeigeDaten;
{$IFDEF DEMO}
        cmSucheDaten          : Begin
                                  (* SucheDaten(SM); *)
                                  Wait('Dies ist die Demoversion');
                                End;
{$ELSE}
        cmSucheDaten          : SucheDaten(SM);
{$ENDIF}
{$IFDEF DEMO}
        cmSucheMehrfache      : Begin
                                  (* SucheMehrfache(SM); *)
                                  Wait('Dies ist die Demoversion');
                                End;
{$ELSE}
        cmSucheMehrfache      : SucheMehrfache(SM);
{$ENDIF}
        cmMaske               : SuchmaskenVoreinstellungDialog(SM);

      else
        Exit
      END;
    End;
    evBroadCast: IF NOT HandleBroadCast THEN ClearEvent(Event);
  else
    Exit
  END;

  If WNo > 1 then EnableCommands ([cmEditorSave,
                                   cmEditorSaveAs,
                                   cmTile,
                                   cmCascade,
                                   cmNext,
                                   cmPrev])
  else            DisableCommands([cmEditorSave,
                                   cmEditorSaveAs,
                                   cmTile,
                                   cmCascade,
                                   cmNext,
                                   cmPrev]);

  If StrucktList <> Nil then
  BEGIN
    EnableCommands([cmPruefen,
                    cmSpeichern,
                    cmDatenAufgeben,
                    cmZeigeDaten,
                    cmNachtrag,
                    cmAufreum ]);

    DisableCommands([cmScann,
                     cmImport,
                     cmLaden,
                     cmDiskAutoScann])
  end

  else
  BEGIN
    DisableCommands([cmPruefen,
                     cmSpeichern,
                     cmDatenAufgeben,
                     cmZeigeDaten,
                     cmNachtrag,
                     cmAufreum]);

    EnableCommands( [cmScann,
                     cmImport,
                     cmLaden,
                     cmDiskAutoScann])
  END;

  ClearEvent(Event);
END;

Procedure TDateiverApp.InitMenuBar;
Var R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('',                                                 hcNoContext, NewMenu(
      NewItem('~I~nfo', '',              kbNoKey, cmProgramminfo,                hcNoContext, Nil)),

    NewSubMenu('Da~t~ei',                                           hcDatei, NewMenu(
      NewSubMenu('~E~ditor',                                        hcEditor, NewMenu(
        NewItem('~L~aden...',        '', kbNoKey, cmFileOpen,       hcEditor,
        NewItem('~S~peichern',       '', kbNoKey, cmEditorSave,     hcEditor,
        NewItem('Speichern ~a~ls',   '', kbNoKey, cmEditorSaveAs,   hcEditor,
        NewLine(
        NewItem('~C~lipboard',       '', kbNoKey, cmShowClip,       hcEditor,
        Nil)))))),
      NewItem('~L~aden',           'F3', kbF3,    cmLaden, hcLaden,
      NewItem('~S~peichern',       'F2', kbF2,    cmSpeichern,      hcSpeichern,
      NewLine(
      NewItem('~V~erzeichnis Ñndern','', kbNoKey, cmChangeDir,      hcChangeDir,
      NewItem('~B~eenden',      'Alt-X', kbAltX,  cmQuit,           hcNoContext,
      nil))))))),

    NewSubMenu('Daten ~A~ktualisieren',                             hcDatenAkt, NewMenu(
      NewSubMenu('~D~atenimport',                                   hcImport, NewMenu(
        NewItem('~I~mportieren',        '', kbNoKey, cmImport,      hcImport,
        NewItem('Importdaten ~P~rÅfen', '', kbNoKey, cmIP,          hcImport,
        NewItem('Import ~N~achtragen',  '', kbNoKey, cmIN,          hcImport,
        Nil)))),
      NewItem('~E~inlesen',          '', kbNoKey, cmScann,          hcScann,
      NewItem('~M~ehrfach einlesen', '', kbNoKey, cmDiskAutoScann,  hcDAS,
      NewLine(
      NewItem('~N~achtrag',          '', kbNoKey, cmNachtrag,       hcNachtrag,
      NewItem('~A~ufreum',           '', kbNoKey, cmAufreum,        hcAufreum,
      nil))))))),

    NewSubMenu('~D~aten',                                           hcDaten, NewMenu(
      NewItem('Daten im Speicher ~P~rÅfen', '', kbNoKey, cmPruefen,        hcPruefen,
      NewItem('~E~dit Daten',               '', kbNoKey, cmDEDIT,          hcDEdit,
      NewItem('~Z~eigen',                   '', kbNoKey, cmZeigeDaten,     hcZeigeDaten,
      NewItem('~S~uchen',                   '', kbNoKey, cmSucheDaten,     hcSucheDaten,
      NewItem('Suche ~M~ehrfache',          '', kbNoKey, cmSucheMehrfache, hcSucheMehrfache,
      NewLine(
      NewItem('~D~aten aufgeben  ',
                               'Alt-F3', kbAltF3, cmDatenAufgeben,  hcDatenAufgeben,
      NewItem('~F~reier Speicher',   '', kbNoKey, cmMemory,         hcMemory,
      nil))))))))),

    NewSubMenu('~E~instellungen',                                   hcEinstellungen, NewMenu(
      NewSubMenu('~F~enster',                                       hcEditor, NewMenu(
        NewItem('~N~Ñchstes Fenster','', kbNoKey, cmNext,           hcEditor,
        NewItem('~L~etztes Fenster', '', kbNoKey, cmPrev,           hcEditor,
        NewLine(
        NewItem('~T~ile',            '', kbNoKey, cmTile,           hcNoContext,
        NewItem('C~a~scade',         '', kbNoKey, cmCascade,        hcNoContext,
        Nil)))))),
      NewItem('~F~arben',            '', kbNoKey, cmColors,         hcNoContext,
      NewItem('~C~ursor Ein/Aus',    '', kbNoKey, cmIfCursor,       hcNoContext,
      NewItem('~N~eue Dateinamen',   '', kbNoKey, cmNewFileName,    hcNewFileName,
      nil))))),

    NewSubMenu('~H~ilfe', hcNoContext, NewMenu(
      NewItem('~H~ilfe - MenÅ',      '', kbNoKey, cmHelpContents, hcContents,
      nil)),

    nil)))))))));
END;

Procedure TDateiverApp.InitStatusLine;
var
  R: TRect;
BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F1~ Hilfe', kbF1, cmHelp,
      NewStatusKey('~Alt-X~ Ende', kbAltX, cmQuit,
      NewStatusKey('~F2~ Speichern', kbF2, cmSpeichern,
      NewStatusKey('~F3~ Laden', kbF3, cmLaden,
      NewStatusKey('~Alt-F3~ Daten aufgeben', kbAltF3, cmDatenAufgeben,
      NewStatusKey('~F10~ Menu', kbF10, cmMenu,
      nil)))))),
    nil)));

END;

Procedure TDateiverApp.OutOfMemory;
BEGIN
  MessageBox('Nicht genug Speicher fÅr diese Operation.',
    nil, mfError + mfOkButton);
END;

PROCEDURE TDateiverApp.Idle;
BEGIN
  TApplication.Idle;
  Heap^.Update;
END;



(* Hauptprogramm: *)

Var DateiverApp : TDateiverApp;



BEGIN

  OvrInit('TVDV.OVR');
  OvrSetBuf(64 * 1024);

  If OvrResult <> ovrOk then
  Begin
    Writeln('Overlay init failed.');
    Exit;
  End;
  OvrInitEMS;
  If OvrResult = OvrNoEMSDriver then
  Begin
    Writeln('Kein EMS - Treiber da!');
    Readln
  End;

  InitDEdit;


  EMSSF := False;
  EMSLF := False;
  EMSPF := False;
  EMSDF := False;
  EMSZF := False;

  EMSDLF := False;
  EMSDPF := False;
  EMSDDF := False;



  SaveCursor;

  RegisterStrings;

  (*DirTree.RegisterTypes;*)
  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done;

  OrgCursor

END.
