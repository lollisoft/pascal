Program TVDV;

(*{$DEFINE DEMO}*)



{$M 65520,8192,655360}

{$X+,S+,O+,F+}

{$G-}

{$W+}

Uses

   (*  TvGraph,*)    (* Grafikbildschirm *)
     TVDVDATA,
     TVCURSOR,
     Overlay,
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
     Editor,
     Strings,
     Colors,
     App,
     TVDVImp,
     FileFind,
     FileShow,
     DEdit,
     StdDlg,
     StrTools,
     DDN,
     EMS,
     XMS,
     XHeap,

	 IdleKey;

Type
  ColorFiletyp         = File of TPalette;
  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Heap        : PHeapView;
    Editor: ARRAY[1..9] OF RECORD
      Ed: PEditorWindow;
      Name: STRING;
    End;

    Constructor Init;
    Destructor  Done;                           Virtual;
    Procedure   Idle;                           Virtual;
    PROCEDURE   NewEditor(Name: STRING);
    Procedure   InitClipBoard;
    FUNCTION    GetFile(Arg: STRING): PCollection;
    PROCEDURE   SEndFile(WindowNo, cmBase: WORD);
    PROCEDURE   SaveFile (WindowNo: WORD; P: PCollection );
    PROCEDURE   SaveFileAs (WindowNo: WORD; P: PCollection );
    Function    GetPalette: PPalette;           Virtual;
    Procedure   GetEvent(Var Event: tEvent);    Virtual;
    Procedure   HandleEvent(Var Event: TEvent); Virtual;
    Procedure   InitMenuBar;                    Virtual;
    Procedure   InitStatusLine;                 Virtual;
    Procedure   OutOfMemory;                    Virtual;
    Procedure   DatenAufgeben;                  Virtual;
  End;

Var
    ColorFile    : ColorFiletyp;
    ColorDat     : PPalette;
    S            : TStream;
    MsgRes       : Word;

(* Editor: *)

    EDCommand, WindowNo: WORD;
    ClipWindow: PEditorWindow;

function CalcHelpName: PathStr;
Var
  EXEName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
Begin
  if Lo(DosVersion) >= 3 then EXEName := ParamStr(0)
  else EXEName := FSearch('TVDV.EXE', GetEnv('PATH'));
  FSplit(EXEName, Dir, Name, Ext);
  if Dir[Length(Dir)] = '\' then Dec(Dir[0]);
  CalcHelpName := FSearch('TVDVHELP.HLP', Dir);
End;

PROCEDURE ProgramminformationDialog;
  Var
    R: tRect;
    View: pView;
    Code: INTEGER;
    Dialog: pDialog;
Begin
  R.Assign (13, 2, 62, 20);
  Dialog := New (pDialog, Init (R, 'Programminformation'));
{$IFDEF DEMO}
  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Testversion !')));
{$ELSE}
  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Vollversion !')));
{$EndIF}
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

  IF Code <> cmCancel then Begin

    { cmCancel muss ev ersetzt werden }

    { Code auswerten }

  End;

  IF Dialog <> NIL then

    Dispose (Dialog, Done);

End;



FUNCTION TDateiverApp.GetPalette: PPalette;

CONST

  CNewColor = CColor + CHelpColor;

  CNewBW    = CBlackWhite + CHelpBlackWhite;

  CNewMono  = CMonochrome + CHelpMonochrome;

  P : ARRAY [apColor..apMonochrome] OF String[Length(CNewColor)] =

              (CNewColor, CNewBW, CNewMono);

Begin

  GetPalette := @P[AppPalette];

End;



PROCEDURE TDateiverApp.GetEvent (Var Event : TEvent);

Var

  W: PWindow;

  HFile: PHelpFile;

  HelpStrm: PDosStream;

CONST

  HelpInUse: Boolean = False;

Begin

  TApplication.GetEvent(Event);

  case Event.What of

    evCommand:

      if (Event.Command = cmHelp) and not HelpInUse then

      Begin

        HelpInUse := True;

        HelpStrm := New(PDosStream, Init(CalcHelpName, stOpenRead));

        HFile := New(PHelpFile, Init(HelpStrm));

        if HelpStrm^.Status <> stOk then

        Begin

          MsgRes := MessageBox('Kann Hilfedatei nicht îffnen.', nil, mfError + mfOkButton);

          Dispose(HFile, Done);

        End

        else

        Begin

          W := New(PHelpWindow,Init(HFile, GetHelpCtx));

          if ValidView(W) <> nil then

          Begin

            MsgRes := ExecView(W);

            Dispose(W, Done);

          End;

          ClearEvent(Event);

        End;

        HelpInUse := False;

      End

      Else

        if (Event.Command = cmHelpContents) and not HelpInUse then

        Begin

          HelpInUse := True;

          HelpStrm := New(PDosStream, Init(CalcHelpName, stOpenRead));

          HFile := New(PHelpFile, Init(HelpStrm));

          if HelpStrm^.Status <> stOk then

          Begin

            MsgRes := MessageBox('Kann Hilfedatei nicht îffnen.', nil, mfError + mfOkButton);

            Dispose(HFile, Done);

          End

          else

          Begin

            W := New(PHelpWindow,Init(HFile, hcContents));

            if ValidView(W) <> nil then

            Begin

              MsgRes := ExecView(W);

              Dispose(W, Done);

            End;

            ClearEvent(Event);

          End;

          HelpInUse := False;

        End;



  End;

End;





Constructor TDateiverApp.Init;

Var

  H: Word;

  R: TRect;

  Dummy: STRING;

  Search: SearchRec;

  I: INTEGER;

  F: Text;

  S: String;



Begin
  Writeln('Init Main Application.');


  cha := ' ';

  Nachgetragen := True;

  Raus := 'A';

  for ch := 'A' to 'J' do

  Begin

    MediumFeld[ch].DateiDif := False;

    MediumFeld[ch].PfadDif := False;

    MediumFeld[ch].LabelDif := False;

    With MediumFeld[ch] do

    Begin

      n  := '';

      L.Ptr  := Nil;

      L.BlockNr := 0;



      P.Ptr  := Nil;

      P.BlockNr := 0;



      D.Ptr  := Nil;

      D.BlockNr := 0;



      S.Ptr  := Nil;

      S.BlockNr := 0;



      es.Ptr := Nil;

      es.BlockNr := 0;



      as.Ptr := Nil;

      as.BlockNr := 0;



      H.Ptr  := Nil;

      H.BlockNr := 0;



      eh.Ptr := Nil;

      eh.BlockNr := 0;



      ah.Ptr := Nil;

      ah.BlockNr := 0;



      Z.Ptr  := Nil;

      Z.BlockNr := 0;



      ez.Ptr := Nil;

      ez.BlockNr := 0;



      na := Nachgetragen;

    End

  End;

  M[1] := MediumFeld['A'];

  M[2] := MediumFeld['A'];

  N := '';



  PData := New(TData, Init);





  PData^.SetallNil;

  LabelHistList := Nil;



  PData^.Initfiles;



  PData^.SetLaufwerk('');



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





End;



FUNCTION ReadFile(Name: STRING): PCollection;

  Var

    P: PCollection;

    ALine: STRING;

    AFile: TEXT;

  Begin

    New(P, Init(20,1));

    Assign(AFile, Name);

    {$I-}Reset(AFile);{$I+}

    IF IOResult = 0 then Begin

      WHILE NOT EOF(AFile) DO Begin

        ReadLn(AFile, ALine);

        P^.AtInsert( P^.Count, New(PStr, Init ( ALine )));

      End;

      Close(AFile);

    End;

    ReadFile := P;

  End;





Destructor TDateiverApp.Done;

Begin

  ReWrite(ColorFile);

  Write(ColorFile, ColorDat^);

  Close(ColorFile);

  DatenAufgeben;

  TApplication.Done

End;



(* FÅr Editor: *)



PROCEDURE TDateiverApp.NewEditor(Name: STRING);

  Var

    R: TRect;

    Dir: DirStr;
    FName: NameStr;
    Ext: ExtStr;

  Begin

    R.Assign(0,0,80,23);

    FSplit(Name, Dir, FName, Ext);

    {Ext := DownStr(Ext);}

    IF Name = ''

      then Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,

                                               edBase+10*WNo))

     ELSE IF (Ext = '.txt') then Begin

       Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, Name, ReadFile(Name),

                                           edBase+10*WNo));

       Editor[WNo].Name := Name;

      End

      ELSE IF (Ext<>'.exe') AND (Ext<>'.com') AND (Ext<>'.bak') then Begin

        Editor[WNo].Ed := New(PLineEditor, Init(R, WNo, Name, ReadFile(Name),

                                                128, edBase+10*WNo));

        Editor[WNo].Name := Name;

       End

       ELSE Exit;

    DeskTop^.Insert(Editor[WNo].Ed);

    Inc(WNo);

  End;



PROCEDURE TDateiverApp.InitClipBoard;

  Var

    R: TRect;

    Dir:DirStr;
    FName: NameStr;
    Ext: ExtStr;

    Name: String;

  Begin

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



  End;







PROCEDURE TDateiverApp.SaveFile(WindowNo: WORD; P: PCollection);

  Var

    TF: TEXT;

    I: INTEGER;

  Begin

    IF WindowNo > 1 then Begin

      Assign(TF,Editor[WindowNo].Name);

      Rewrite(TF);

      FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);

      Close(TF);

    End;

  End;



PROCEDURE TDateiverApp.SaveFileAs(WindowNo: WORD; P: PCollection);

  Var

    TF: TEXT;

    I: INTEGER;

    D: PFileDialog;

    FileName: PathStr;



  Begin

    IF WindowNo > 1 then Begin

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

    End;

  End;







FUNCTION TDateiverApp.GetFile(Arg: STRING): PCollection;

  Var

    D: PFileDialog;

    FileName: PathStr;

    P: PCollection;

  Begin

    D := PFileDialog(ValidView(New(PFileDialog, Init(Arg, 'Laden',

                                                'File', 3, 12))));

    IF (D <> Nil) AND (Desktop^.ExecView(D) <> cmCancel) then Begin

      D^.GetFileName(FileName);

      GetFile := ReadFile(FileName);

     End

     ELSE GetFile := Nil;

  End;



PROCEDURE TDateiverApp.SEndFile(WindowNo, cmBase: WORD);

  Var Event: TEvent;

  Begin

    Event.What := evBroadCast;

    Event.Command := cmBase+WindowNo*10+cmEdSEnd;

    Event.InfoPtr := GetFile('*.*');

    TopView ^.HandleEvent(Event);

  End;





Procedure LoescheLPDTrees;



Procedure ReinigeLTree(Root: LabelListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    BlEndeBlockEin(Root.BlockNr);

    ReinigeLTree(Root.Ptr^.Links);



    BlEndeBlockEin(Root.BlockNr);

    ReinigeLTree(Root.Ptr^.Rechts);



    FreememX(Root, SizeOf(Root.Ptr^))

  End;

End;



Procedure ReinigePTree(Root: PfadListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    BlEndeBlockEin(Root.BlockNr);

    ReinigePTree(Root.Ptr^.Links);



    BlEndeBlockEin(Root.BlockNr);

    ReinigePTree(Root.Ptr^.Rechts);



    FreeMemX(Root, SizeOf(Root.Ptr^))

  End;

End;



Procedure ReinigeDTree(Root: DateiListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    BlEndeBlockEin(Root.BlockNr);

    ReinigeDTree(Root.Ptr^.Links);



    BlEndeBlockEin(Root.BlockNr);

    ReinigeDTree(Root.Ptr^.Rechts);



    FreeMemX(Root, SizeOf(Root.Ptr^))

  End;

End;



Begin

  ReinigeLTree(LabelList);

  ReinigePTree(PfadList);

  ReinigeDTree(DateiList);

End;





Procedure TDateiverApp.DatenAufgeben;

Begin

  CursorAus;



  If StrucktList.Ptr <> Nil then

  Begin

    While StrucktList.Ptr^.Next.Ptr <> Nil do

    Begin

      BlEndeBlockEin(StrucktList.BlockNr);



      StrucktList := StrucktList.Ptr^.Next;

      FreeMemX(StrucktList.Ptr^.Back, SizeOf(StrucktList.Ptr^.Back.Ptr^))

    End;

    FreeMemX(StrucktList, SizeOf(StrucktList.Ptr^))

  End;



  If ZusatzList.Ptr <> Nil then

  Begin

    While ZusatzList.Ptr^.Next.Ptr <> Nil do

    Begin

      BlEndeBlockEin(ZusatzList.BlockNr);



      ZusatzList := ZusatzList.Ptr^.Next;

      FreeMemX(ZusatzList.Ptr^.Back, SizeOf(ZusatzList.Ptr^.Back.Ptr^))

    End;

    FreeMemX(ZusatzList, SizeOf(ZusatzList.Ptr^))

  End;



  If HilfList.Ptr <> Nil then

  Begin

    While HilfList.Ptr^.Next.Ptr <> Nil do

    Begin

      BlEndeBlockEin(HilfList.BlockNr);



      HilfList := HilfList.Ptr^.Next;

      FreeMemX(HilfList.Ptr^.Back, SizeOf(HilfList.Ptr^.Back.Ptr^))

    End;

    FreeMemX(HilfList, SizeOf(HilfList.Ptr^))

  End;



  LoescheLPDTrees;

  PData^.SetAllNil;



  CursorEin

End;







(*****************************************************************)



Procedure TDateiverApp.HandleEvent(Var Event: TEvent);

Var R: TRect;



(* FÅr Editor: *)



  FUNCTION HandleBroadCast: BOOLEAN;

    Begin

      IF Event.Command DIV 100 * 100 = edBase then Begin

        HandleBroadCast := True;

        WindowNo := Event.Command MOD 100 DIV 10;

        EDCommand := Event.Command MOD 10;

        CASE EDCommand OF

          cmEdSave: SaveFile(WindowNo, Event.InfoPtr);

          cmEdSaveAs: SaveFileAs(WindowNo, Event.InfoPtr);

          cmEdSaveAndExit: SaveFile(WindowNo, Event.InfoPtr);

          cmEdRead: SEndFile(WindowNo,edBase);

          cmEdExit: Dispose(Editor[WindowNo].Ed, Done);

          ELSE HandleBroadCast := False;

        End;

       End

       ELSE HandleBroadCast := False;

    End;







procedure FileOpen;

Var

  D: PFileDialog;

  FileName: PathStr;

  W: PWindow;

Begin

  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'ôffnen einer Datei',

    '~N~ame', fdOpenButton, 100))));

  if D <> nil then

  Begin

    if Desktop^.ExecView(D) <> cmCancel then

    Begin

      D^.GetFileName(FileName);

      NewEditor(FileName);

    End;

    Dispose(D, Done);

  End;

End;

(************************************************************)





procedure FileImport;

Var

  D: PFileDialog;

  FileName: PathStr;

  W: PWindow;

Begin

  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', 'Importieren einer Datei',

    '~N~ame', fdOkButton, 100))));

  if D <> nil then

  Begin

    if Desktop^.ExecView(D) <> cmCancel then

    Begin

      D^.GetFileName(FileName);

      Import(FileName);

    End;

    Dispose(D, Done);

  End;

End;









Procedure Scann;

Var Info     : ^String;

    Zahl     : String[5];



Begin

  LabelHistList := Nil;

  New(Info);

  Repeat
(*
    info^ := PData^.GetLaufwerk;
*)

    info^ := 'B';

    If ExecDialog(ReadString('Welches Laufwerk'), Info) = cmCancel then Exit;

    If Length(Info^) <> 1 then

      MsgRes := MessageBox('Fehler in der Eingabe!',

         nil, mfError + mfOkButton);

  Until Length(Info^) = 1;

  PData^.SetLaufwerk(Info^);

  Mask := Info^ + ':\*.*';

  Dispose(Info);

  N := Mask;



  if System.MemAvail > 1024 then

    PData^.Scanner

  Else

    Wait('Speicherplatz reicht nicht mehr aus fÅr diese Aufgabe!')

End;







Procedure DiskAutoScann;

Var Info : ^String;

Begin

  New(Info);

  Repeat

(*
    info^ := PData^.GetLaufwerk;
*)

    info^ := 'B';

    If ExecDialog(ReadString('Welches Laufwerk'), Info) = cmCancel then Exit;

    If Length(Info^) <> 1 then

    MsgRes := MessageBox('Fehler in der Eingabe!',

         nil, mfError + mfOkButton);

  Until Length(Info^) = 1;

  PData^.SetLaufwerk(Info^);

  Mask := Info^ + ':\*.*';

  Dispose(Info);

  N := Mask;



  PData^.DiskAS

End;











(***********************************************)

(*                                             *)

(*         Proceduren fÅr Nachtrag             *)

(*                                             *)

(***********************************************)







Procedure PutMxonMy(x,y: byte);



Begin



(* Diese Prozedur hÑngt an die jeweiligen Listen x *)

(* die Listen aus y hinten an: *)



  with M[y] do

  Begin

    BlEndeBlockEin(eh.BlockNr);

    eh.Ptr^.Next := M[x].h;



    BlEndeBlockEin(es.BlockNr);

    es.Ptr^.Next := M[x].s;



    BlEndeBlockEin(M[x].h.BlockNr);

    M[x].h.Ptr^.Back := eh;



    BlEndeBlockEin(M[x].s.BlockNr);

    M[x].s.Ptr^.Back := es;

  End;

End;



Procedure CopyBaumDataMxtoBaumDataMy(x, y: Byte);

Var Hilf1,

    Hilf2,

    Hilf3  : StrucktListXPtrtyp;





(* HilfsVariabeln fÅr EinblEndungen: *)



    LabelP   : LabelDataXtyp;

    PfadP    : PfadDataXtyp;

    DateiP   : DateiDataXtyp;

    StrucktP : StrucktDataXtyp;





Begin

  Hilf3 := M[x].s;

  PData^.CopyMinWork(y);



  While M[x].s.Ptr <> Nil do

  Begin

    BlEndeBlockEin(M[x].s.BlockNr);

    StrucktP.XPtr := M[x].s;

    StrucktP.StrucktData := M[x].s.Ptr^.StrucktData;



    case M[x].s.Ptr^.StrucktData.art of

      1 : Begin

            (* EinblEnden der noch benîtigten Daten, und kopieren *)

            (* in PufferVariabeln.                                *)





            BlEndeBlockEin(StrucktP.StrucktData.LabelList.BlockNr);

            LabelP.LabelData := LabelListPtrtyp(StrucktP.StrucktData.LabelList.Ptr)^.LabelData;

            LabelP.XPtr := StrucktP.StrucktData.LabelList;



            If LabelP.LabelData.Anzahl > 0 then

            Begin

              PData^.SpeichereLabel(LabelP.LabelData.

                             LabelName,

                             LabelList,

                             LabelDif, NoNew);





              BlEndeBlockEin(LabelP.XPtr.BlockNr);



              Dec (LabelP.XPtr.Ptr^.LabelData.Anzahl);



              If LabelP.XPtr.Ptr^.LabelData.Anzahl = 0 then

              Begin

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



                FreeMemX(StrucktP.XPtr.Ptr^.StrucktData.LabelList,

                         SizeOf(StrucktP.XPtr.Ptr^.StrucktData.LabelList.Ptr^))

              End

              Else

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



              StrucktP.XPtr.Ptr^.StrucktData.LabelList := labeladress;

              Hilf1 := StrucktP.XPtr

            End

            Else

              Wait('LabelData.Anzahl ist <= 0')

          End;

      2 : Begin

            (* EinblEnden der noch benîtigten Daten, und kopieren *)

            (* in PufferVariabeln.                                *)



            BlEndeBlockEin(StrucktP.StrucktData.PfadList.BlockNr);

            PfadP.PfadData := StrucktP.StrucktData.PfadList.Ptr^.PfadData;

            PfadP.XPtr := StrucktP.StrucktData.PfadList;



            If PfadP.PfadData.Anzahl > 0 then

            Begin

              PData^.SpeicherePfad(PfadP.PfadData.

                            PfadName,

                            PfadList,

                            PfadDif, NoNew);



              BlEndeBlockEin(PfadP.XPtr.BlockNr);



              Dec(PfadP.XPtr.Ptr^.PfadData.Anzahl);



              If PfadP.XPtr.Ptr^.PfadData.Anzahl = 0 then

              Begin

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



                FreeMemX(StrucktP.XPtr.Ptr^.StrucktData.PfadList,

                         SizeOf(StrucktP.XPtr.Ptr^.StrucktData.PfadList.Ptr^))

              End

              Else

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



              StrucktP.XPtr.Ptr^.StrucktData.PfadList := pfadadress;

              Hilf2 := StrucktP.XPtr

            End

            Else

              Wait('PfadData.Anzahl ist <= 0')

          End;

      3 : Begin

            (* EinblEnden der noch benîtigten Daten, und kopieren *)

            (* in PufferVariabeln.                                *)



            BlEndeBlockEin(StrucktP.StrucktData.DateiList.BlockNr);

            DateiP.DateiData := DateiListPtrtyp(StrucktP.StrucktData.DateiList.Ptr)^.DateiData;

            DateiP.XPtr := StrucktP.StrucktData.DateiList;



            If DateiP.DateiData.Anzahl > 0 then

            Begin

              Dateiadress.Ptr := Nil;

              PData^.SpeichereDatei(DateiP.DateiData.

                             DateiName,

                             DateiList,

                             DateiDif, NoNew);



              If Dateiadress.Ptr = Nil then Halt;



              BlEndeBlockEin(DateiP.XPtr.BlockNr);



              Dec(DateiP.XPtr.Ptr^.DateiData.Anzahl);



              If DateiP.XPtr.Ptr^.DateiData.Anzahl = 0 then

              Begin

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



                FreeMemX(StrucktP.XPtr.Ptr^.StrucktData.DateiList,

                         SizeOf(StrucktP.XPtr.Ptr^.StrucktData.DateiList.Ptr^));

              End

              Else

                BlEndeBlockEin(StrucktP.XPtr.BlockNr);



              StrucktP.XPtr.Ptr^.StrucktData.DateiList := dateiadress;



              PData^.SpeichereSuch(Hilf1,Hilf2,dateiadress);



              BlEndeBlockEin(DateiAdress.BlockNr);

              if Dateiadress.Ptr^.dateidata.loc.Ptr = Nil then Exit

            End

            Else

              Wait('DateiData.Anzahl ist <= 0')

            

          End

      Else Wait('Element ist ungÅltig');

    End;

    

    If M[x].s.Ptr = Nil then Exit;



    BlEndeBlockEin(M[x].s.BlockNr);



    M[x].s := M[x].s.Ptr^.Next

  End;

  PData^.CopyWorkinM(y);

  M[x].s := Hilf3;



  (* Lîsche Zusatzliste, wenn vorhanden: *)



  BlEndeBlockEin(M[x].z.BlockNr);



  While M[x].z.Ptr^.Next.Ptr <> Nil do

  Begin

    M[x].z := M[x].z.Ptr^.Next;



    BlEndeBlockEin(M[x].z.BlockNr);



    FreeMemX(M[x].z.Ptr^.Back,

             SizeOf(M[x].z.Ptr^.Back.Ptr^))

  End;



  FreeMemX(M[x].z,SizeOf(M[x].z.Ptr^))



End;











Procedure Nachtr;

Var Help     : HilfListXPtrtyp;

    HilfLauf : HilfListXPtrtyp;



Procedure LoescheDiskBereich;



(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)

(* gehîhrt, entfernen.                                            *)



Var p, l : String;

    Lauf : StrucktListXPtrtyp;



Procedure LoescheElement(Var Anfang,

                             Ende,

                             Element          : ListenXPtrtyp);

Var ElementHilf1,

    ElementHilf2: ListEndataXtyp;



Begin

  If (Anfang.Ptr = Element.Ptr) and

     (Anfang.BlockNr = Element.BlockNr) then

                                         (* Anfang der Liste weiterrÅcken *)

  Begin

    BlEndeBlockEin(Anfang.BlockNr);

    Anfang := Anfang.Ptr^.Next               (* oder vorhergehEnden Next -    *)

  End

  else                                   (* Zeiger auf Element nach       *)

  Begin                                  (* Element zeigen lassen:        *)

    BlEndeBlockEin(Element.BlockNr);

    ElementHilf1.XPtr := Element;

    ElementHilf1.Element := Element.Ptr^;



    BlEndeBlockEin(ElementHilf1.Element.Back.BlockNr);

    ElementHilf2.XPtr := ElementHilf1.Element.Back;

    ElementHilf2.Element := ElementHilf1.Element.Back.Ptr^;



    ElementHilf2.XPtr.Ptr^.Next := ElementHilf1.Element.Next;

  End;





  If (Ende.Ptr = Element.Ptr) and

     (Ende.BlockNr = Element.BlockNr) then

                                         (* Ende der Liste zurÅckrÅcken *)

  Begin

    BlEndeBlockEin(Ende.BlockNr);

    Ende := Ende.Ptr^.Back                   (* oder nachvolgEnden Back -   *)

  End

  else                                   (* Zeiger auf Element vor      *)

  Begin                                  (* Element zeigen lassen:      *)

    BlEndeBlockEin(Element.BlockNr);

    ElementHilf1.XPtr := Element;

    ElementHilf1.Element := Element.Ptr^;



    BlEndeBlockEin(ElementHilf1.Element.Next.BlockNr);

    ElementHilf2.XPtr := ElementHilf1.Element.Next;

    ElementHilf2.Element := ElementHilf1.Element.Next.Ptr^;



    ElementHilf2.XPtr.Ptr^.Back := ElementHilf1.Element.Back;

  End

End;



Procedure LoescheStrucktElement(Var Anfang,

                                    Ende,

                                    Element          : StrucktListXPtrtyp);

Var Hilf : StrucktListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Element lîschen, das nicht da ist!');

    Exit

  End;

  Hilf := Element;



  BlendeBlockEin(Element.BlockNr);



  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));



  FreeMemX(Hilf, SizeOf(Hilf.Ptr^))

End;





Procedure LoescheHilfElement(Var Anfang,

                                 Ende,

                                 Element          : HilfListXPtrtyp);

Var Hilf : HilfListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Element lîschen, das nicht da ist!');

    Exit

  End;

  Hilf := Element;



  BlendeBlockEin(Element.BlockNr);



  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));

  FreeMemX(Hilf, SizeOf(Hilf.Ptr^))

End;





Procedure LoescheZusatzElement(Var Anfang,

                                   Ende,

                                   Element    : ZusatzListXPtrtyp);

Var Hilf: ZusatzListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Element lîschen, das nicht da ist!');

    Exit

  End;

  Hilf := Element;



  BlendeBlockEin(Element.BlockNr);



  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));

  FreeMemX(Hilf, SizeOf(Hilf.Ptr^))

End;







Procedure LoescheLabel(Var l: String);

Var LaufHilf: StrucktDataXtyp;

    LabelHilf: LabelDataXtyp;

Begin

  BlendeBlockEin(Lauf.BlockNr);

  LaufHilf.XPtr := Lauf;

  LaufHilf.StrucktData := Lauf.Ptr^.StrucktData;



  BlendeBlockEin(LaufHilf.StrucktData.LabelList.BlockNr);

  LabelHilf.XPtr := LaufHilf.StrucktData.LabelList;

  LabelHilf.LabelData := LaufHilf.StrucktData.LabelList.Ptr^.LabelData;



  l := LabelHilf.LabelData.LabelName;



  Dec(LabelHilf.XPtr.Ptr^.LabelData.Anzahl);



  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)

End;



Procedure LoeschePfad(Var p: String);

Var LaufHilf: StrucktDataXtyp;

    PfadHilf: PfadDataXtyp;

Begin

  BlendeBlockEin(Lauf.BlockNr);

  LaufHilf.XPtr := Lauf;

  LaufHilf.StrucktData := Lauf.Ptr^.StrucktData;



  BlendeBlockEin(LaufHilf.StrucktData.PfadList.BlockNr);

  PfadHilf.XPtr := LaufHilf.StrucktData.PfadList;

  PfadHilf.PfadData := LaufHilf.StrucktData.PfadList.Ptr^.PfadData;



  p := PfadHilf.PfadData.PfadName;



  Dec(PfadHilf.XPtr.Ptr^.PfadData.Anzahl);



  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)

End;



Procedure LoescheDatei(zl, zp : String);

Var Zaehler : Byte;

    Help,

    LocHelp : ZusatzListXPtrtyp;



    LaufHilf : StrucktDataXtyp;

    DateiHilf: DateiDataXtyp;

    LocHilf  : ZusatzDataXtyp;



Begin

  Zaehler := 1;



  BlendeBlockEin(Lauf.BlockNr);

  LaufHilf.XPtr := Lauf;

  LaufHilf.StrucktData := Lauf.Ptr^.StrucktData;



  BlendeBlockEin(LaufHilf.StrucktData.DateiList.BlockNr);

  DateiHilf.XPtr := LaufHilf.StrucktData.DateiList;

  DateiHilf.DateiData := LaufHilf.StrucktData.DateiList.Ptr^.DateiData;



  BlendeBlockEin(DateiHilf.DateiData.Loc.BlockNr);

  LocHilf.XPtr := DateiHilf.DateiData.Loc;

  LocHilf.ZusatzData := DateiHilf.DateiData.Loc.Ptr^.ZusatzData;



(*

  Help := DateiHilf.DateiData.Loc;

  LocHelp := Help;

*)

  If LocHilf.XPtr.Ptr = Nil then

  Begin

    Wait('Dateielement hat keine verbindung mehr zur Zusatzliste!');

    Halt

  End;



(********************************************************************)

(*                                                                  *)

(* Volgende Variabeln mÅssen fÅr den Vergleich eingeblendet werden: *)

(*                                                                  *)

(* zl <> LabelData.LabelName:                                       *)

(*                                                                  *)

(* LocHilf.XPtr.BlockNr, zeigt auf Zusatzelement.                   *)

(* ZusatzData.sl.BlockNr, zeigt auf Strucktelement.                 *)

(* LabelList.BlockNr, zeigt auf LabelData.                          *)

(*                                                                  *)

(* zp <> PfadData.PfadName:                                         *)

(*                                                                  *)

(* ZusatzData.sp.BlockNr, zeigt auf Strucktelement.                 *)

(* PfadList.BlockNr, zeigt auf PfadData.                            *)

(*                                                                  *)

(********************************************************************)



  While  (zl <> LocHilf.XPtr.Ptr^.ZusatzData.sl.Ptr^.StrucktData.

          LabelList.Ptr^.LabelData.LabelName) or

         (zp <> Help.Ptr^.ZusatzData.sp.Ptr^.StrucktData.

          PfadList.Ptr^.PfadData.PfadName)

  do

  Begin

    Help := Help.Ptr^.Next;

    Inc(Zaehler)

  End;



  If Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl = 1 then

  Begin

    If (Help.Ptr <> LocHelp.Ptr) or

       (Help.BlockNr <> LocHelp.BlockNr) then

    Begin

      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )');

      Exit

    End;

    Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc.Ptr := Nil;

  End

  else

    If (Help.Ptr = LocHelp.Ptr) and

       (Help.BlockNr = LocHelp.BlockNr) then

    Begin



      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)



      Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc := LocHelp.Ptr^.Next;

    End

    Else

    Begin

      If Help.Ptr = Nil then

      Begin

        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )');

        Exit

      End;

      Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc := LocHelp;

    End;







  LoescheZusatzElement(M[2].z, M[2].ez, Help);



  Dec(Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl);



  If Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc.Ptr = Nil then

  Begin

    If Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl > 0 then

    Begin

      Wait('Loc ist auf Nil gesetzt und Anzahl ist nicht 0');

      Halt

    End

  End;



  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

End;

Begin
  Wait('Lîsche Diskbereich (Hauptprogramm)');

(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)
  Lauf := HilfLauf.Ptr^.HilfData.LabelList;
  (* Laufe die Strucktliste durch und lîsche die Daten: *)
  repeat
    case Lauf.Ptr^.StrucktData.art of

      1 : LoescheLabel(l);

      2 : LoeschePfad(p);

      3 : LoescheDatei(l, p);

      Else

        Wait('Fehlerhaftes Element in Lîsche Diskbereich!');

    End



  until (Lauf.Ptr^.StrucktData.art = 1) or

        (Lauf.Ptr = Nil);

End;





Begin

(* Speichern der gescannten Daten in M1: *)



  PData^.CopyWorkinM(1);



(* ZurÅcksetzen des Mediums Work: *)



  PData^.SetAllNil;



  PData^.LadeDateien;



  PData^.CopyWorkinM(2);



  PData^.SetallNil;



  Help := M[1].h;



(* Entfernen von alten Daten: *)



  While Help.Ptr <> Nil do

  Begin

    HilfLauf := M[2].h;



    (* Suche Label in Diskdaten: *)



    While (HilfLauf.Ptr <> Nil) and

          (HilfLauf.Ptr^.HilfData.LabelList.Ptr^.StrucktData.

           LabelList.Ptr^.LabelData.LabelName

           <>

           Help.Ptr^.HilfData.LabelList.Ptr^.StrucktData.

           LabelList.Ptr^.LabelData.LabelName)



          do HilfLauf := HilfLauf.Ptr^.Next;



    (* Wenn nicht Nil, dann ist ein alter

       Label unter diesem Namen vorhanden *)



    If HilfLauf.Ptr <> Nil then

      LoescheDiskBereich;



    (* Hole neuen Label aus gescannten Daten: *)



    repeat

      Help := Help.Ptr^.Next

    until (Help.Ptr^.HilfData.art = 1) or

          (Help.Ptr = Nil)

  End; (* Von While M1... *)



  If M[2].s.Ptr <> Nil then

  Begin

    CopyBaumDataMxtoBaumDataMy(1, 2);

    PutMxonMy(1, 2);

    PData^.CopyMinWork(2)

  End

  else

    PData^.CopyMinWork(1);



  Erase(DateiDataFile);

  Erase(PfadDataFile);

  Erase(LabelDataFile);

  Erase(ZusatzDataFile);

  Erase(HilfDataFile);

  Erase(StrucktDataFile);



  PData^.SpeichereDateien

End;





Procedure Nachtr1;

Type AltLabelListPtrTyp = ^AltLabelListTyp;

     AltLabelListTyp    = Record

                            LabelName: String12;

                            Next     : AltLabelListPtrTyp

                          End;



Var Help     : HilfListXPtrtyp;

    HilfLauf : HilfListXPtrtyp;

    SZahl    : String;

    Lauf     : StrucktListXPtrtyp;









Procedure LoescheDiskBereich;



(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)

(* gehîhrt, entfernen.                                            *)



Var p, l : String;

    VorDatei: String;



Procedure LoescheElement(Var Anfang,

                             Ende,

                             Element          : ListenXPtrtyp);

Begin

  If (Anfang.Ptr = Element.Ptr) and

     (Anfang.BlockNr = Element.BlockNr) then

                                             (* Anfang der Liste weiterrÅcken *)

    Anfang := Anfang.Ptr^.Next               (* oder vorhergehEnden Next -    *)

  else                                       (* Zeiger auf Element nach       *)

    Element.Ptr^.Back.Ptr^.Next :=

    Element.Ptr^.Next;

                                             (* Element zeigen lassen:        *)



  If (Ende.Ptr = Element.Ptr) and

     (Ende.BlockNr = Element.BlockNr) then   (* Ende der Liste zurÅckrÅcken *)

    Ende := Ende.Ptr^.Back                   (* oder nachvolgEnden Back -   *)

  else                                       (* Zeiger auf Element vor      *)

    Element.Ptr^.Next.Ptr^.Back := Element.Ptr^.Back;

                                             (* Element zeigen lassen:      *)

End;



Procedure LoescheStrucktElement(Var Anfang,

                                    Ende,

                                    Element          : StrucktListXPtrtyp);

Var Hilf : StrucktListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Struckt - Element lîschen, das nicht da ist!');

    Halt

  End;

  Hilf := Element;

  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));

  Dispose(Hilf.Ptr)

End;





Procedure LoescheHilfElement(Var Anfang,

                                 Ende,

                                 Element          : HilfListXPtrtyp);

Var Hilf : HilfListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Hilf - Element lîschen, das nicht da ist!');

    Halt

  End;

  Hilf := Element;

  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));

  Dispose(Hilf.Ptr)

End;





Procedure LoescheZusatzElement(Var Anfang,

                                   Ende,

                                   Element    : ZusatzListXPtrtyp);

Var Hilf: ZusatzListXPtrtyp;

Begin

  If Element.Ptr = Nil then

  Begin

    Wait('Kann kein Zusatz - Element lîschen, das nicht da ist!');

    Halt

  End;

  Hilf := Element;

  Element := Element.Ptr^.Next;

  LoescheElement(ListenXPtrtyp(Anfang),

                 ListenXPtrtyp(Ende),

                 ListenXPtrtyp(Hilf));

  Dispose(Hilf.Ptr)

End;





Procedure LoescheLabel(Var l: String);

Begin

  l := Lauf.Ptr^.StrucktData.LabelList.Ptr^.LabelData.LabelName;

  Dec(Lauf.Ptr^.StrucktData.LabelList.Ptr^.LabelData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);

End;



Procedure LoeschePfad(Var p: String);

Begin

  p := Lauf.Ptr^.StrucktData.PfadList.Ptr^.PfadData.PfadName;

  Dec(Lauf.Ptr^.StrucktData.PfadList.Ptr^.PfadData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);

End;



Procedure LoescheDatei(zl, zp : String);

Var Zaehler : Byte;

    Help,

    LocHelp : ZusatzListXPtrtyp;

    Anz     : String;

Begin



  Zaehler := 1;

  If Lauf.Ptr = Nil then

  Begin

    Wait('Lauf ist Nil, Dateielement kann nicht gelîscht werden!');

    Halt

  End;



  If Lauf.Ptr^.StrucktData.DateiList.Ptr = Nil then

  Begin

    Wait('Lauf^.StrucktData.DateiList = Nil!');

    Halt

  End;



  Help := Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc;



  LocHelp := Help;



  If Help.Ptr = Nil then

  Begin

    Wait('Help = Nil!' + Chr(13) +

         'DateiData.Anzahl: ' +

          GetString(Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl) + Chr(13) +

         'Datei: ' +

          Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.DateiName + Chr(13) +

         'Datei, die vorher gelîscht wurde: ' + VorDatei);

    Halt

  End;





  While  (zl <> Help.Ptr^.ZusatzData.sl.Ptr^.StrucktData.

          LabelList.Ptr^.LabelData.LabelName) or

         (zp <> Help.Ptr^.ZusatzData.sp.Ptr^.StrucktData.

          PfadList.Ptr^.PfadData.PfadName)

  do

  Begin

    Help := Help.Ptr^.Next;

    Inc(Zaehler);

    If Zaehler > Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl then

    Begin

      Wait('Zusatz - Element nicht gefunden!');

      Halt

    End

  End;



  If Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl = 1 then

  Begin

    If (Help.Ptr <> LocHelp.Ptr) or

       (Help.BlockNr <> LocHelp.BlockNr) then

    Begin

      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )');

      Halt

    End;



    Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc.Ptr := Nil;



  End

  else

    If (Help.Ptr = LocHelp.Ptr) and

       (Help.BlockNr = LocHelp.BlockNr) then

    Begin



      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)

      If LocHelp.Ptr^.Next.Ptr = Nil then

      Begin

        Wait('DateiData.Loc ist letztes Element, weiterrÅcken nicht mîglich!' +

             'Anzahl: ' + GetString(Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl) + Chr(13) +

             'Datei : ' + Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.DateiName);

        Halt

      End;

      Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc := LocHelp.Ptr^.Next;

    End

    Else

    Begin

      If Help.Ptr = Nil then

      Begin

        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )');

        Halt

      End;

      Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Loc := LocHelp;

    End;





  LoescheZusatzElement(M[2].z, M[2].ez, Help);



  Dec(Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.Anzahl);



  LoescheStrucktElement(M[2].s, M[2].es, Lauf);



End;







Begin



  Lauf := HilfLauf.Ptr^.HilfData.LabelList;





  If Lauf.Ptr^.Back.Ptr <> Nil then

    Wait('Strucktelement ist nicht am Anfang der Liste!');



  A := 0;

(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)



  



  (* Laufe die Strucktliste durch und lîsche die Daten: *)





  repeat

    Inc(A);



    Str(A, SZahl);

    case Lauf.Ptr^.StrucktData.art of

      1 : LoescheLabel(l);

      2 : LoeschePfad(p);

      3 : Begin

            VorDatei := Lauf.Ptr^.StrucktData.DateiList.Ptr^.DateiData.DateiName;

            LoescheDatei(l, p)

          End

    Else

      Wait('Falsches Element in Strucktliste!');

    End

    

    

  until (Lauf.Ptr^.StrucktData.art = 1) or

        (Lauf.Ptr = Nil);

End;







Procedure ReinigeLPDTrees;



Procedure ReinigeLTree(Root: LabelListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    ReinigeLTree(Root.Ptr^.Links);

    ReinigeLTree(Root.Ptr^.Rechts);



    If (Root.Ptr^.Links.Ptr <> Nil) and

       (Root.Ptr^.LabelData.Anzahl = 0) then

      Dispose(Root.Ptr^.Links.Ptr);



    If (Root.Ptr^.Rechts.Ptr <> Nil) and

       (Root.Ptr^.LabelData.Anzahl = 0) then

      Dispose(Root.Ptr^.Rechts.Ptr);

  End;

End;



Procedure ReinigePTree(Root: PfadListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    ReinigePTree(Root.Ptr^.Links);

    ReinigePTree(Root.Ptr^.Rechts);



    If (Root.Ptr^.Links.Ptr <> Nil) and

       (Root.Ptr^.PfadData.Anzahl = 0) then

      Dispose(Root.Ptr^.Links.Ptr);



    If (Root.Ptr^.Rechts.Ptr <> Nil) and

       (Root.Ptr^.PfadData.Anzahl = 0) then

      Dispose(Root.Ptr^.Rechts.Ptr);

  End;

End;



Procedure ReinigeDTree(Root: DateiListXPtrTyp);

Begin

  If Root.Ptr <> Nil then

  Begin

    ReinigeDTree(Root.Ptr^.Links);

    ReinigeDTree(Root.Ptr^.Rechts);



    If (Root.Ptr^.Links.Ptr <> Nil) and

       (Root.Ptr^.DateiData.Anzahl = 0) then

      Dispose(Root.Ptr^.Links.Ptr);



    If (Root.Ptr^.Rechts.Ptr <> Nil) and

       (Root.Ptr^.DateiData.Anzahl = 0) then

      Dispose(Root.Ptr^.Rechts.Ptr);

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

    HilfListHilf: HilfListXPtrTyp;



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



  While Lauf.Ptr <> Nil do

  Begin

    If Not(Lauf.Ptr^.StrucktData.Art In [1, 2, 3]) then

      Wait('Vor Speichern der neuen Daten ist in Strucktliste ein fehlerhaftes Element');

    If Lauf.Ptr^.StrucktData.Art in [1, 2] then

    Begin

      If Lauf.Ptr^.StrucktData.LabelList.Ptr^.LabelData.LabelName = AltName then

        Wait('Gleicher Pfad oder Label hintereinander vorhanden!' + Chr(13) +

             'Vor Speichern der neuen Daten.');



      AltName := Lauf.Ptr^.StrucktData.LabelList.Ptr^.LabelData.LabelName;

    End;

    Lauf := Lauf.Ptr^.Next

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



  While HilfListHilf.Ptr <> Nil do

  Begin

    If HilfListHilf.Ptr^.HilfData.art = 1 then



    Begin

      New(LHelp^.Next);



      LHelp^.Next^.LabelName := HilfListHilf.Ptr^.HilfData.LabelList.Ptr^.

                               StrucktData.LabelList.Ptr^.LabelData.

                               LabelName;

      LHelp := LHelp^.Next;

      LHelp^.Next := Nil

    End;



    HilfListHilf := HilfListHilf.Ptr^.Next

  End;



  LHelp := AltLabelList^.Next;

  Dispose(AltLabelList);

  AltLabelList := LHelp

End;



Begin



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



  PData^.InitFiles;



  PData^.SpeichereDateien;

  Wait('TemponÑre Dateien geschrieben');



  PData^.SetAllNil;



  



  Dateifilename   := Df;

  Pfadfilename    := Pf;

  Labelfilename   := Lf;

  Strucktfilename := Sf;

  Hilffilename    := Hf;

  Zusatzfilename  := Zf;



  PData^.InitFiles;



  PData^.OpenAllFiles;

  (* Abfrage, ob die Dateien in den Speicher passen: *)



  Mem1 := FileSize(DateiDatafile)   * SizeOf(DateiList.Ptr^)   +

          FileSize(DateiDatafile)   * SizeOf(Pointer)      +



          FileSize(PfadDatafile)    * SizeOf(PfadList.Ptr^)    +

          FileSize(PfadDatafile)    * SizeOf(Pointer)      +



          FileSize(LabelDatafile)   * SizeOf(LabelList.Ptr^)   +

          FileSize(LabelDatafile)   * SizeOf(Pointer)      +



          FileSize(StrucktDatafile) * SizeOf(StrucktList.Ptr^) +

          FileSize(StrucktDatafile) * SizeOf(Pointer)      +



          FileSize(ZusatzDatafile)  * SizeOf(ZusatzList.Ptr^)  +

          FileSize(ZusatzDatafile)  * SizeOf(Pointer)      +





          FileSize(HilfDatafile)    * SizeOf(HilfList.Ptr^);





  PData^.CloseAllFiles;



  If Mem1 > System.MemAvail then

  Begin

    DDNachtrag('.tmp', FileName);

    DisposeAltLabels;

    Exit

  End;





  PData^.LadeDateien;

  Wait('Alte Daten geladen');



  PData^.CopyWorkInM(2);



  PData^.SetAllNil;



  Help := M[1].h;



(* Entfernen von alten Daten mit zuhilfenahme *)

(* von gesicherten Labels:   LHelp            *)



  While LHelp <> Nil do

  Begin



    HilfLauf := M[2].h;



    (* Suche Label in Diskdaten: *)



    While (HilfLauf.Ptr <> Nil) and

          (HilfLauf.Ptr^.HilfData.LabelList.Ptr^.StrucktData.

           LabelList.Ptr^.LabelData.LabelName

           <>

           LHelp^.LabelName)



          do HilfLauf := HilfLauf.Ptr^.Next;



    (* Wenn nicht Nil, dann ist ein alter

       Label unter diesem Namen vorhanden *)





    If HilfLauf.Ptr <> Nil then

    Begin

      LoescheDiskBereich;

    End;



    (* Hole neuen Label aus gescannten Daten: *)



    repeat

      LHelp := LHelp^.Next

    until LHelp = Nil

  End; (* Von While M1... *)



  HilfLauf := M[2].h;

  

  PData^.CopyMInWork(2);





(************************************************************)

(*                                                          *)

(* Unterscheidung, ob Medium leer ist oder noch andere      *)

(* Diskbereiche vorhanden sind:                             *)

(*                                                          *)

(************************************************************)



  If (M[2].eh.Ptr <> M[2].h.Ptr) or

     (M[2].eh.BlockNr <> M[2].h.BlockNr) then

  Begin

    Erase(DateiDataFile);

    Erase(PfadDataFile);

    Erase(LabelDataFile);

    Erase(ZusatzDataFile);

    Erase(HilfDataFile);

    Erase(StrucktDataFile);



    PData^.SpeichereDateien;



    PData^.SetAllNil;



    PData^.LadeDateien;



    PData^.CopyWorkInM(2);

    PData^.SetAllNil

  End

  Else

  Begin

    DatenAufgeben;

    PData^.CopyWorkInM(2);

    PData^.SetAllNil

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



  PData^.InitFiles;



  PData^.LadeDateien;



  PData^.CopyWorkInM(1);



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



  PData^.InitFiles;



  If M[2].s.Ptr <> Nil then

  Begin

    CopyBaumDataMxtoBaumDataMy(1, 2);

    PutMxonMy(1, 2);

    PData^.CopyMinWork(2);

  End

  else

    PData^.CopyMinWork(1);



  PData^.SpeichereDateien

End;







Procedure Nachtrag;

Var MemNew : Longint;

    Lauf   : StrucktListXPtrtyp;

    ZLauf  : ZusatzListXPtrtyp;

Begin

  If Not(PData^.TestFiles) then

  Begin

    PData^.SpeichereDateien;

    PData^.SetAllNil;

    Exit

  End;



  PData^.OpenallFiles;

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



  PData^.CloseallFiles;



  (* Teste, ob Daten im Speicher auch nach dem Laden der alten Daten *)

  (* noch umgewandelt werden kînnen: *)



  Anzahl := 0;



  PData^.ZaehleElemente(GlobalBaumXPtrtyp(DateiList));

  PData^.ZaehleElemente(GlobalBaumXPtrtyp(PfadList));

  PData^.ZaehleElemente(GlobalBaumXPtrtyp(LabelList));



  Lauf := StrucktList;

  While Lauf.Ptr <> Nil do

  Begin

    Inc(Anzahl);

    Lauf := Lauf.Ptr^.Next

  End;



  ZLauf := ZusatzList;

  While ZLauf.Ptr <> Nil do

  Begin

    Inc(Anzahl);

    ZLauf := ZLauf.Ptr^.Next

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

  PData^.SetAllNil

End;



(***********************************************)

(*                                             *)

(*            Ende von Nachtrag                *)

(*                                             *)

(***********************************************)







Procedure Aufreum;

Begin

  MsgRes := MessageBox('Noch nicht Programmiert.',

    nil, mfError + mfOkButton);

End;





Procedure LadeDateien;

Begin

  CursorAus;

  if StrucktList.Ptr = nil then

    if PData^.TestFiles then

      PData^.LadeDateien

    else

      MsgRes := MessageBox('Es sind keine Dateien unter diesem Namen vorhanden!',

             nil, mfError + mfOkButton)



  else
  Begin
    MsgRes := MessageBox('Es sind noch Daten im Arbeitsbereich!',

             nil, mfError + mfOkButton);
  End;
  CursorEin;

End;



Procedure SpeichereDateien;

Begin

  CursorAus;

  PData^.SpeichereDateien;

  PData^.SetallNil;

  CursorEin;

End;







procedure ChangeDir;

Begin

  MsgRes := ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);

  PData^.InitFiles

End;



Procedure LoadFileNames;

Begin

End;



Procedure SaveFileNames;

Begin

End;











Procedure ZeigeDaten;

Begin

  ZeigeDateien

End;





Procedure SucheMehrfache(Var SM: String);

Begin

  Mehrfache := True;



  MsgRes := ExecView(New(PFileFindWindow, Init(SM)));



  SM := FileFindWindow^.GetMaskBack;



  If FileFindWindow <> Nil then

    Dispose(FileFindWindow, Done);

  PData^.CloseallFiles;



  Mehrfache := False;

End;









PROCEDURE SuchmaskenVoreinstellungDialog(Var SuchMaske: String);

TYPE

    SuchmaskenVoreinstellungData = RECORD

      TextLen0: WORD;

      TextRec0: ARRAY [0..12] OF CHAR; 

    End;



Type LaengenTyp = Record

                    Lo,

                    Hi : Byte

                  End;







  Var

    R      : tRect;

    View   : pView;

    Data   : SuchmaskenVoreinstellungData;

    Code   : INTEGER;

    Dialog : pDialog;



    Laenge : LaengenTyp;







Begin

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

  IF Code <> cmCancel then Begin



    Dialog^.GetData (Data);





    For A := 1 to Data.TextLen0 do

      SuchMaske[A] := Data.TextRec0[A - 1];

    Word(Laenge) := Data.TextLen0;

    SuchMaske[0] := Char(Laenge.Lo);

    SuchMaske := UpDate(SuchMaske);







  End;

  IF Dialog <> NIL then

    Dispose (Dialog, Done);

End;







Procedure SucheDaten(Var SM: String);

Begin

  MsgRes := ExecView(New(PFileFindWindow, Init(SM)));



  SM := FileFindWindow^.GetMaskBack;



  If FileFindWindow <> Nil then

    Dispose(FileFindWindow, Done);

  PData^.CloseallFiles

End;





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





Begin

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







  MsgRes := MessageBox('          Speicherplatz:' +

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



End;



procedure ShowClip;

Begin

  ClipWindow^.Select;

  ClipWindow^.Show;

End;

(* Begin von TDareiverApp.HandleEvent: *)

Const SM: String = '*.*'; (* SuchmaskenVoreinstellung *)

Var Regs: Registers;
Var FN: String;

Begin
  TApplication.HandleEvent(Event);

  case Event.What of

    evCommand:

    Begin

      GetExtent(R);

      Dec(R.B.Y, 2);



      case Event.Command of



      (* FÅr Editor: *)



        cmFileOpen            : FileOpen;



        cmIfCursor            : Begin
(*
                                  IfCursor := Not(IfCursor);

                                  CursorAus
*)
                                End;

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

                                  IF NOT HandleBroadCast then

                                    ClearEvent(Event);

                                End;

        cmEditorSaveAs        : Begin

                                  Event.What := evBroadCast;

                                  Event.Command := edBase + cmEdSaveAs;

                                  PutEvent(Event);

                                  IF NOT HandleBroadCast then

                                    ClearEvent(Event);

                                End;



(*******************************************************************)



        cmProgramminfo        : ProgrammInformationDialog;

        cmMemory              : Memory;

        cmChangeDir           : ChangeDir;



        cmScann               : Scann;

        cmDatenAufgeben       : PData^.DatenAufgeben;

        cmImport              : FileImport;

        cmIN                  : PData^.ImportNachtrag;

        cmIP                  : PData^.ImportPruefen;

        cmNachtrag            : PData^.Nachtrag;

        cmAufreum             : PData^.Aufreum;

        cmLaden               : PData^.LadeDateien;

        cmSpeichern           : PData^.SpeichereDateien;

        cmPruefen             : (*PruefeDaten('Teste Daten');*) Begin End;

        cmNewFileName         :
                                Begin
                                     PData^.NewFileName('Dateiname fÅr Daten ?', FileName);
                                End;

        cmDEdit               : DateneditorDialog;

        cmZeigeDaten          : ZeigeDaten;

        cmMaske               : Begin
                                     SuchmaskenVoreinstellungDialog(FN);
                                     SM := FN;
                                End;





{$IFDEF DEMO}

        cmDiskAutoScann       : Begin

                                  (* DiskAutoScann; *)

                                  Wait('Dies ist die Demoversion');

                                End;

{$ELSE}

        cmDiskAutoScann       : DiskAutoScann;

{$EndIF}

{$IFDEF DEMO}

        cmSucheDaten          : Begin

                                  (* SucheDaten(SM); *)

                                  Wait('Dies ist die Demoversion');

                                End;

{$ELSE}

        cmSucheDaten          : SucheDaten(SM);

{$EndIF}

{$IFDEF DEMO}

        cmSucheMehrfache      : Begin

                                  (* SucheMehrfache(SM); *)

                                  Wait('Dies ist die Demoversion');

                                End;

{$ELSE}

        cmSucheMehrfache      : SucheMehrfache(SM);

{$EndIF}

        cmMaske               : SuchmaskenVoreinstellungDialog(SM);



      else

        Exit

      End;

    End;

    evBroadCast: IF NOT HandleBroadCast then ClearEvent(Event);

  else

    Exit

  End;



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



  If StrucktList.Ptr <> Nil then

  Begin

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

  End



  else

  Begin

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

  End;



  ClearEvent(Event);

End;



Procedure TDateiverApp.InitMenuBar;

Var R: TRect;

Begin

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

      NewItem('~B~eEnden',      'Alt-X', kbAltX,  cmQuit,           hcNoContext,

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

End;



Procedure TDateiverApp.InitStatusLine;

Var

  R: TRect;

Begin

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



End;



Procedure TDateiverApp.OutOfMemory;

Begin

  MsgRes := MessageBox('Nicht genug Speicher fÅr diese Operation.',

    nil, mfError + mfOkButton);

End;



PROCEDURE TDateiverApp.Idle;

Begin
  (* http://www.delphigroups.info/2/ac/3804.html *)
  asm    int $28   end;
  TApplication.Idle;
  Heap^.Update;
End;

{$O TvdvImp }

{$O FileFind}
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
(* Hauptprogramm: *)
Var DateiverApp : TDateiverApp;
Begin
  (* http://www.delphigroups.info/2/ac/3804.html *)
  InitIdleKey;
  Writeln('Init TVDV.OVR');
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
  
  EMSSF := False;
  EMSLF := False;
  EMSPF := False;
  EMSDF := False;
  EMSZF := False;
  EMSDLF := False;
  EMSDPF := False;
  EMSDDF := False;

  SaveCursor;

  Writeln('Call RegisterStrings');
  RegisterStrings;

  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done;

  OrgCursor;
  UninitIdleKey
End.