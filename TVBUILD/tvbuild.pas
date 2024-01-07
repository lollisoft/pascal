Program TVBUILD;

(*{$DEFINE DEMO}*)

{$DEFINE XHEAP}

{$M 65520,8192,655360}

{$X+,S-,F+}

{$G-}

{$W+}

Uses App,
     Crypt,
     TVBUCmds,
     TVBUHelp,
     Projekt,
     HelpFile,
     Dos,
     Objects,
     Memory,
     GadGets,
     Drivers,
     Views,
     Menus,
     Dialogs,
     MsgBox,
     Editors,
     TVCursor,
     Colorsel,
     Colors,
     MyEditor,
     MyStr,
     StrTools,
     CursCont,
{$IFDEF XHEAP}
     XHeap,
{$ENDIF}
     Dirs,
     StdDlg;

Type

     String12 = String[12];



     CopyRightTyp = Record

                      CopyrightHead : String;

                      Anzahl: Byte;

                      cName : String;

                      cADR  : String;

                      cADR1 : String;

                      cTel  : String;

                      cTest : String;

                      cTVVT : String;

                      cPName: String;

                      cCopyr: String;

                      cFor  : String

                    End;





Const CopyRight : CopyRightTyp = (

                         CopyrightHead : 'Copyright-Header';

                         Anzahl: 8;

                         cName : '     Lothar Behrens      ';

                         cADR  : '     Redacted 1          ';

                         cADR1 : '     W-12345 Redacted    ';

                         cTel  : '     (01234) 1234        ';

                         cTest : 'TVBUILD fÅr DOS - Testversion !';

                         cTVVT : ' TVBUILD fÅr DOS - Version 1.0 ';

                         cPName: '       Turbo - Vision - Programmgenerator       ';

                         cCopyr: '     Copyright (c) 1994     ';

                         cFor  : '');



  fdCloseButton   = $0020;      { Put a Close button in the dialog }




Type



  ColorFiletyp         = File of TPalette;



  PTVBuildApp         = ^TTVBUILDApp;

  TTVBUILDApp         = Object(TApplication)



    MenuExtent : TRect;

    Heap       : PHeapView;



    Editor: ARRAY[1..9] OF RECORD

                             Ed: PEditorWindow;

                             Name: STRING;

                           END;



    Constructor Init;

    Destructor  Done;                           Virtual;

    Procedure   Idle;                           Virtual;



    Procedure   DosShell(Command: String);



    Procedure   InitClipBoard;

    Function    FileOpen(Text: String; Var FileName: PathStr): Boolean;

    Procedure   StartEditor(FileName:PathStr);

    PROCEDURE   NewEditor(Name: STRING);

    FUNCTION    GetFile(Arg: STRING): PCollection;

    PROCEDURE   SendFile(WindowNo, cmBase: WORD);

    PROCEDURE   SaveFile (WindowNo: WORD; P: PCollection );

    PROCEDURE   SaveFileAs (WindowNo: WORD; P: PCollection );

    Procedure   DoneEditor(Ed: PEditorWindow);





    Function    GetPalette: PPalette;           Virtual;



    Procedure   HandleEvent(Var Event: TEvent); Virtual;

    Procedure   InitMenuBar;                    Virtual;

    Procedure   InitStatusLine;                 Virtual;

    Procedure   OutOfMemory;                    Virtual;

  END;

    







var



    IfCursor     : Boolean;


    Laufwerk     : String[12];

    ColorFile    : ColorFiletyp;

    S            : TStream;

    FName        : PathStr;



    WorkDir      : String;





(* Editor: *)



    EDCommand, WindowNo: WORD;

    ClipWindow: PEditorWindow;





Procedure EnCode;

Begin

  EnCryptStruc(Copyright.CopyrightHead[0], 256, 19);





  EnCryptStruc(Copyright.cTest[0], 256, 19);

  EnCryptStruc(Copyright.cName[0], 256, 19);

  EnCryptStruc(Copyright.cADR[0], 256, 19);

  EnCryptStruc(Copyright.cADR1[0], 256, 19);

  EnCryptStruc(Copyright.cTel[0], 256, 19);

  EnCryptStruc(Copyright.cTest[0], 256, 19);

  EnCryptStruc(Copyright.cTVVT[0], 256, 19);

  EnCryptStruc(Copyright.cPName[0], 256, 19);

  EnCryptStruc(Copyright.cCopyr[0], 256, 19);

  EnCryptStruc(Copyright.cFor[0], 256, 19);



End;



Procedure DeCode;

Begin

  DeCryptStruc(Copyright.CopyrightHead[0], 256, 19);





  DeCryptStruc(Copyright.cTest[0], 256, 19);

  DeCryptStruc(Copyright.cName[0], 256, 19);

  DeCryptStruc(Copyright.cADR[0], 256, 19);

  DeCryptStruc(Copyright.cADR1[0], 256, 19);

  DeCryptStruc(Copyright.cTel[0], 256, 19);

  DeCryptStruc(Copyright.cTest[0], 256, 19);

  DeCryptStruc(Copyright.cTVVT[0], 256, 19);

  DeCryptStruc(Copyright.cPName[0], 256, 19);

  DeCryptStruc(Copyright.cCopyr[0], 256, 19);

  DeCryptStruc(Copyright.cFor[0], 256, 19);



End;









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

    R, Rect : tRect;

    View    : pView;

    Code    : INTEGER;

    Dialog  : pDialog;

    SText   : String;



    A       : Integer;



Procedure R_Assign(t: String; i: Integer);

Begin

  Inc(A, I);

  R.Assign ((Rect.B.X - Rect.A.X) Div 2 - Length(t) Div 2, A,

           ((Rect.B.X - Rect.A.X) Div 2 - Length(t) Div 2) +

           Length(SText), A + 1)

End;





BEGIN

  A := 0;



  DeCode;







  Rect.Assign (2, 2, 77, 20);

  Dialog := New (pDialog, Init (Rect, 'Programminformation'));





{$IFDEF DEMO}



  SText := Copyright.cTest;



  R_Assign(SText, 1);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



{$ELSE}





  SText := Copyright.cTVVT;



  R_Assign(SText, 2);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



{$ENDIF}



  SText := Copyright.cPName;

  R_Assign(SText, 2);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cFor;

  R_Assign(SText, 2);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cCopyr;

  R_Assign(SText, 2);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cName;

  R_Assign(SText, 2);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cADR;

  R_Assign(SText, 1);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cADR1;

  R_Assign(SText, 1);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));



  SText := Copyright.cTel;

  R_Assign(SText, 1);



  Dialog^.Insert (New (pStaticText, Init (R, SText)));





(*  R.Assign (Rect.B.X - Rect.A.X, 15, Rect.B.X - Rect.A.X, 17);*)



  R.Assign ((Rect.B.X - Rect.A.X) Div 2 - 10 Div 2, 15,

           ((Rect.B.X - Rect.A.X) Div 2 - 10 Div 2) +

           10, 17);







  Dialog^.Insert (New (pButton, Init (R, ' ~O~K ', 10, 1)));

 

  Dialog^.SelectNext (FALSE);



  EnCode;



  Code := Desktop^.ExecView (Application^.ValidView (Dialog));

  IF Code <> cmCancel THEN BEGIN

    { cmCancel muss ev ersetzt werden }

    { Code auswerten }

  END;

  IF Dialog <> NIL THEN 

    Dispose (Dialog, Done);

END;





FUNCTION TTVBUILDApp.GetPalette: PPalette;

CONST

  CNewColor = CColor + CHelpColor;

  CNewBW    = CBlackWhite + CHelpBlackWhite;

  CNewMono  = CMonochrome + CHelpMonochrome;

  P : ARRAY [apColor..apMonochrome] OF String[Length(CNewColor)] =

              (CNewColor, CNewBW, CNewMono);

BEGIN

  GetPalette := @P[AppPalette];

END;





Function GetFName: String;

Var Name: String;

Begin

  Name := ParamStr(0);

  While Pos('.', Name) <> 0 do

    System.Delete(Name, Length(Name), 1);

  GetFName := Name

End;











Constructor TTVBUILDApp.Init;

var

  H: Word;

  R: TRect;

  Dummy: STRING;

  Search: SearchRec;

  I: INTEGER;

  F: Text;

  S: String;

  V: PView;

BEGIN



  RegisterEditors;

  RegisterHelpFile;

  RegisterEditor;

  RegisterColorsel;

  RegisterObjects;

  RegisterViews;

  RegisterMenus;

  RegisterDialogs;

  RegisterApp;



  TApplication.Init;









  WorkDir := 'C:\';


  InitProjekt(WorkDir);



  R := MenuExtent;



  R.A.X := R.B.X - 9;

  R.B.Y := R.A.Y + 1;

  Heap := New(PHeapView, Init(R));

  Insert(Heap);





  InitClipBoard;

  I := 1;



  DisableCommands([cmVVX, cmVVS, cmSave, cmSaveAs]);



  ProgrammInformationDialog;





END;







Destructor TTVBUILDApp.Done;

Var S :String;

BEGIN

{IFNDEF DEMO}

  DoneProjekt; (* Projekt - Verwaltung *)





(*  If Save then SpeichereDateienDialog;*)

{ENDIF}

  ReWrite(ColorFile);

  Write(ColorFile, WorkDir);



  Close(ColorFile);

  TApplication.Done

END;



(* FÅr Editor: *)



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





Procedure TTVBUILDApp.DoneEditor(Ed: PEditorWindow);

Begin

End;



PROCEDURE TTVBUILDApp.NewEditor(Name: STRING);

  VAR

    R: TRect;

    Dir, FName, Ext: STRING;

  BEGIN

    Inc(WNo);



    R.Assign(0,0,80,23);

    FSplit(Name, Dir, FName, Ext);

    Ext := DownStr(Ext);

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

  END;



PROCEDURE TTVBUILDApp.InitClipBoard;

  VAR

    R: TRect;

    Dir, FName, Ext: STRING;

    Name: String;

  BEGIN

    Name := '';

    GetExtent(R);

    Dec(R.B.Y, 2);

    FSplit(Name, Dir, FName, Ext);

    Ext := DownStr(Ext);



    Editor[WNo].Ed := New(PEditorWindow, Init(R, WNo, 'Clipboard', Nil,

                                               edBase+10*WNo));



    ClipWindow := Editor[WNo].Ed;

    if ClipWindow <> nil then

      Clipboard := ClipWindow^.Editor;



    ClipWindow^.Close;

    DeskTop^.Insert(Editor[WNo].Ed);



  END;





PROCEDURE TTVBUILDApp.SaveFile(WindowNo: WORD; P: PCollection);

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



PROCEDURE TTVBUILDApp.SaveFileAs(WindowNo: WORD; P: PCollection);

  VAR

    TF: TEXT;

    I: INTEGER;

    D: PFileDialog;

    FileName: PathStr;



  BEGIN

    IF WindowNo > 1 THEN BEGIN

      D := PFileDialog(ValidView(New(PFileDialog, Init(Editor[WindowNo].Name, 'Datei speichern als',

        '~N~ame', fdCloseButton, 100))));

      If D <> Nil then

      Begin

        If Desktop^.ExecView(D) <> cmCancel then

        Begin

          D^.GetFileName(FileName);

          Editor[WindowNo].Name := FileName;

          Editor[WindowNo].Ed^.NewTitle(FileName);



          Assign(TF,Editor[WindowNo].Name);

          Rewrite(TF);

          FOR I := 0 TO P^.Count-1 DO Writeln(TF, PStr(P^.At(I))^.GetStr);

          Close(TF);

        End;

        Dispose(D, Done)

      End;

    END;

  END;





FUNCTION TTVBUILDApp.GetFile(Arg: STRING): PCollection;

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



PROCEDURE TTVBUILDApp.SendFile(WindowNo, cmBase: WORD);

  VAR Event: TEvent;

  BEGIN

    Event.What := evBroadCast;

    Event.Command := cmBase+WindowNo*10+cmEdSend;

    Event.InfoPtr := GetFile('*.*');

    TopView ^.HandleEvent(Event);

  END;



Function TTVBUILDApp.FileOpen(Text: String; Var FileName: PathStr): Boolean;

var

  D: PFileDialog;

  W: PWindow;

begin

  FileOpen := False;



  D := PFileDialog(ValidView(New(PFileDialog, Init('*.*', Text,

    '~N~ame', fdOpenButton, 100))));

  if D <> nil then

  begin

    if Desktop^.ExecView(D) <> cmCancel then

    begin

      D^.GetFileName(FileName);

      FileOpen := True

    end;

    Dispose(D, Done);

  end;

end;



Procedure TTVBUILDApp.StartEditor(FileName: PathStr);

Var A: Integer;

Begin

  For A := 1 to 9 do If (Editor[A].Ed <> Nil) and

                        (Editor[A].Name = FileName) then Exit;



  NewEditor(FileName);

End;



procedure TTVBUILDApp.DosShell(Command: String);

begin

  DoneSysError;

  DoneEvents;

  DoneVideo;

  DoneMemory;

  SetMemTop(HeapPtr);



  PrintStr(GetString(MemAvail));



  PrintStr('Type EXIT to return...');

  SwapVectors;

  Exec(GetEnv('COMSPEC'), Command);

  SwapVectors;

  SetMemTop(HeapEnd);

  InitMemory;

  InitVideo;

  InitEvents;

  InitSysError;

  Redraw;

end;





(*****************************************************************)



Procedure TTVBUILDApp.HandleEvent(Var Event: TEvent);

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

          cmEdKillWnd: Editor[WindowNo].Ed := Nil;

          ELSE HandleBroadCast := False;

        END;

        CASE Event.Command OF

          edBase + cmEdSave: SaveFile(WindowNo, Event.InfoPtr);

          edBase + cmEdSaveAs: SaveFileAs(WindowNo, Event.InfoPtr);

          ELSE HandleBroadCast := False;

        END;

       END

       ELSE HandleBroadCast := False;

    END;













procedure ShowClip;

begin

  ClipWindow^.Select;

  ClipWindow^.Show;

end;





{

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

}













(* BEGIN von TDareiverApp.HandleEvent: *)



Const SM: String12 = '*.*'; (* SuchmaskenVoreinstellung *)







Var Regs: Registers;

    Info: ^String;

    Result: Word;



    OldDir: String;



    Name  : String;

    P     : PView;



BEGIN

  If AProjekt^.ASelectedProjekt = - 1 then

  Begin

    DisableCommands([cmProjektSpeichern,

                     cmProjektSelektieren,

                     cmProjektSpeichern,

                     cmRunAppQuelle,

                     cmAktualQuelle,

                     cmWriteAppQuelle,

                     cmCompileAppQuelle,

                     cmSubsEditieren,

                     cmUnitEditieren,

                     cmMainEditieren,

                     cmVariablenEditieren,

                     cmConstantenEditieren,

                     cmEditAppQuelle,

                     cmMakeAndRun,

                     cmProjektModus]);

  End

  Else

  Begin

    EnableCommands ([cmProjektSpeichern,

                     cmProjektSelektieren,

                     cmProjektSpeichern,

                     cmRunAppQuelle,

                     cmAktualQuelle,

                     cmWriteAppQuelle,

                     cmCompileAppQuelle,

                     cmSubsEditieren,

                     cmUnitEditieren,

                     cmMainEditieren,

                     cmVariablenEditieren,

                     cmConstantenEditieren,

                     cmEditAppQuelle,

                     cmMakeAndRun,

                     cmProjektModus]);

  End;







  Case Event.What of

    evBroadCast:

    Begin

      Begin

        If Event.Command = cmSubListItemSelected then

        Begin

          P := Event.InfoPtr;

          If TypeOf(P^) = TypeOf(TListBox) then

          Begin

            Wait('ListBox');

          End;



          If PListBox(Event.InfoPtr)^.Owner <> Nil then

            Name := PLabel(PListBox(Event.InfoPtr)^.Owner)^.Text^

          Else

            Name := 'Nil';



          ClearEvent(Event);

          Wait(Name + ' was Selected');



        End

      End;

    End;

  End;



  TApplication.HandleEvent(Event);



  case Event.What of

    evCommand:

    Begin

      GetExtent(R);

      R.Assign(R.A.X, R.A.Y, R.B.X, R.B.Y-2);



      case Event.Command of



        cmInfo                : Begin

                                  ProgramminformationDialog;

                                End;



        cmTile                : DeskTop^.Tile(R);

        cmCascade             : DeskTop^.Cascade(R);

        cmColors              : Begin

                                  ColorDialog;

                                  DeskTop^.ReDraw

                                End;



        cmVVE                 : AProjekt^.HilfeKontextEditierenDialog;



        cmHML                 : AProjekt^.ScannProjektforCommands;



        cmProjektLaden        : AProjekt^.Laden;

        cmProjektSpeichern    : AProjekt^.Speichern;

        cmProjektDefinieren   : AProjekt^.Definieren;

        cmProjektSelektieren  : AProjekt^.Selektieren;

        cmProjektModus        : AProjekt^.SetProjektModus;





        cmClassenEditieren    : AProjekt^.ClassenEditieren;





        cmVVL                 : Wait('MenÅ cmVVL');



        cmVVDA                : Wait('MenÅ cmVVDA');



        cmVVDD                : Wait('MenÅ cmVVDD');



        cmVVDE                : Wait('MenÅ cmVVDE');



        cmVVI                 : Wait('MenÅ cmVVI');



        cmVVX                 : Wait('MenÅ cmVVX');



        cmVVS                 : Wait('MenÅ cmVVS');

        cmVVA                 : Wait('MenÅ cmVVA');



        cmVVD                 : Wait('MenÅ cmVVD');



        cmModuleHirachie      : Wait('cmModuleHirachie');

        cmModuleBAssistent    : Wait('cmModuleBAssistent');





        cmReadQuelle          : AProjekt^.ReadAppSources;

        cmAktualQuelle        : Wait('MenÅ cmAktualQuelle');



        cmVariablenEditieren  : AProjekt^.VariablenEditieren;

        cmConstantenEditieren : AProjekt^.ConstantenEditieren;





        cmEditAppQuelle       : If AProjekt^.EditAppQuelle(FName) then

                                  StartEditor(FName);



        cmMainEditieren       : AProjekt^.EditMain;

        cmWriteAppQuelle      : AProjekt^.WriteAppSources;

        cmRunAppQuelle        : AProjekt^.RunApp;

        cmCompileAppQuelle    : AProjekt^.CompileApp;

        cmMakeAndRun          : AProjekt^.MakeAndRun;



        cmMenusEditieren      : Begin

                                  MenuBar^.Hide;

                                  AProjekt^.EditMenu;

                                  MenuBar^.Show;

                                End;



        cmSubsEditieren       : AProjekt^.EditiereSubroutinen;

        cmUnitEditieren       : AProjekt^.EditiereUnits;



        cmEditorSave          : Begin

                                  Event.What := evBroadCast;

                                  Event.Command := edBase + cmEdSave;

                                  PutEvent(Event);

                                  Exit

                                  (*IF NOT HandleBroadCast THEN

                                    ClearEvent(Event);*)

                                End;

        cmEditorSaveAs        : Begin

                                  Event.What := evBroadCast;

                                  Event.Command := edBase + cmEdSaveAs;

                                  PutEvent(Event);

                                  Exit

                                  (*IF NOT HandleBroadCast THEN

                                    ClearEvent(Event);*)

                                End;



        cmArbeitsverz         : AProjekt^.ChWorkDir;



        cmChangeDir           : ChangeDir;

        cmShowClip            : ShowClip;



        cmChangeDir           : ChangeDir;



        cmIfCursor            : BEGIN

                                  IfCursor := Not(IfCursor);

                                  CursorAus

                                END;



(* Editor: *)

        cmFileOpen            : Begin

                                  If FileOpen('Dateiauswahl', FName) then

                                    StartEditor(FName)

                                End;

      else

        Exit

      END;

    End;

    evBroadCast: IF NOT HandleBroadCast THEN

                 Begin

                   If Event.Command = cmDosShell then

                   Begin

                     DosShell(PString(Event.InfoPtr)^);

                   End;

                   ClearEvent(Event);

                 End;



    



  else

    Exit

  END;





  





  

  If WNo > 1 then EnableCommands ([cmEditorSave, cmEditorSaveAs])

  else            DisableCommands([cmEditorSave, cmEditorSaveAs]);



  ClearEvent(Event);

END;



Procedure TTVBUILDApp.InitMenuBar;

Var R: TRect;

BEGIN

  GetExtent(R);

  R.B.Y := R.A.Y + 1;



(*  R.Grow(-1, 0);*)

  MenuExtent := R;



  MenuBar := New(PMenuBar, Init(R, NewMenu(



    NewSubMenu('~D~atei',    hcNoContext, NewMenu(



      NewItem('~I~nfo Åber Programm...',   '',  kbNoKey,

                                                cmInfo,

                                                hcNoContext,

      NewItem('~V~erzeichnis wechseln...', '',  kbNoKey,

                                                cmChangeDir,

                                                hcNoContext,

      NewLine(

      NewItem('~L~ade Projekt...',         '',  kbNoKey,

                                                cmProjektLaden,

                                                hcNoContext,

      NewItem('~S~peichere Project',       '',  kbNoKey,

                                                cmProjektSpeichern,

                                                hcNoContext,

      NewLine(

      NewItem('~E~nde',               'Alt-X',  kbAltX,

                                                cmQuit,

                                                hcNoContext,

      Nil)))))))),



    NewSubMenu('~A~pplikation',    hcNoContext, NewMenu(

      

      NewItem('~D~efiniere Project...',    '',  kbNoKey,

                                                cmProjektDefinieren,

                                                hcNoContext,

      NewItem('~S~elektiere Projekt...',   '',  kbNoKey,

                                                cmProjektSelektieren,

                                                hcNoContext,

      NewSubMenu('~M~odule',    hcNoContext, NewMenu(



        NewItem('~H~irachie...',           '',    kbNoKey,

                                                  cmModuleHirachie,

                                                  hcNoContext,

        NewItem('~B~uild assistent...',   '',     kbNoKey,

                                                  cmModuleBAssistent,

                                                  hcNoContext,

        Nil))),

      NewLine(

      NewItem('Lese ~P~rojekt aus Quelle', '',  kbNoKey,

                                                cmReadQuelle,

                                                hcNoContext,

      NewItem('Aktualisiere ~Q~uelle',     '',  kbNoKey,

                                                cmAktualQuelle,

                                                hcNoContext,



      NewItem('Quell~t~ext editieren',    'F6', kbF6,

                                                cmEditAppQuelle,

                                                hcNoContext,



      NewItem('~E~rzeuge Quelltexte',     'F7', kbF7,

                                                cmWriteAppQuelle,

                                                hcNoContext,



      NewLine(

      NewItem('~C~ompiliere Quelltexte',   '',  kbNoKey,

                                                cmCompileAppQuelle,

                                                hcNoContext,



      NewItem('~R~un Applikation',         '',  kbNoKey,

                                                cmRunAppQuelle,

                                                hcNoContext,



      NewLine(

      NewItem('Teste ~A~pplikation',    'F5',   kbF5,

                                                cmMakeAndRun,

                                                hcNoContext,





      Nil)))))))))))))),





    NewSubMenu('~E~ditor',                      hcEditor, NewMenu(



      NewItem('~L~aden...',             'F3',   kbNoKey,

                                                cmFileOpen,

                                                hcEditor,

      NewItem('~S~peichern',            'F2',   kbNoKey,

                                                cmEditorSave,

                                                hcEditor,

      NewItem('Speichern ~a~ls...',       '',   kbNoKey,

                                                cmEditorSaveAs,

                                                hcEditor,

      NewLine(

      NewItem('~C~lipboard',              '',   kbNoKey,

                                                cmShowClip,

                                                hcEditor,

      Nil)))))),





    NewSubMenu('Ein~s~tellungen',  hcEinstellungen, NewMenu(

      NewItem('~C~ursor',                     '',   kbNoKey,

                                                    cmIfCursor,

                                                    hcNoContext,

      NewItem('~F~arben...',                  '',   kbNoKey,

                                                    cmColors,

                                                    hcNoContext,

      NewLine(

      NewItem('~T~ile',                       '',   kbNoKey,

                                                    cmTile,

                                                    hcNoContext,

      NewItem('C~a~scade',                    '',   kbNoKey,

                                                    cmCascade,

                                                    hcNoContext,

      NewLine(

      NewItem('~A~rbeitsverz. fÅr Projekt',   '',   kbNoKey,

                                                    cmArbeitsverz,

                                                    hcNoContext,

      NewItem('~D~ateiname fÅr Projekt...',   '',   kbNoKey,

                                                    cmVVD,

                                                    hcNoContext,

      NewItem('~P~rojektmodus',               '',   kbNoKey,

                                                    cmProjektModus,

                                                    hcNoContext,

      nil)))))))))),





    NewSubMenu('~P~rojekt', hcNoContext, NewMenu(

      NewSubMenu('~K~lassen', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmClassenEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~M~enÅ', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,



        NewItem('Event~b~ehandlung',            '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,

        nil))),





      NewSubMenu('~D~ialoge', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,



        NewItem('Event~b~ehandlung',            '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,

        nil))),





      NewSubMenu('~V~ariablen', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmVariablenEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~C~onstanten', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmConstantenEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~D~aten', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~S~ub''s', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmSubsEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~U~nit''s', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmUnitEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~H~auptprogramm', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmMainEditieren,

                                                    hcNoContext,

        nil)),





      NewSubMenu('~H~ilfe - System', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmVVE,

                                                    hcNoContext,



        NewItem('~M~enÅ aus Quelle einlesen',   '', kbNoKey,

                                                    cmHML,

                                                    hcNoContext,

        nil))),

      nil))))))))))),



   NewSubMenu('~K~lassen', hcNoContext, NewMenu(

      NewSubMenu('~V~isuelle', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmClassenEditieren,

                                                    hcNoContext,

        nil)),



      NewSubMenu('~D~aten', hcNoContext, NewMenu(

        NewItem('~E~ditor...',                  '', kbNoKey,

                                                    cmMenusEditieren,

                                                    hcNoContext,

        nil)),

      nil))),

    nil)))))))));



END;



Procedure TTVBUILDApp.InitStatusLine;

var

  R: TRect;

BEGIN

  GetExtent(R);

  R.A.Y := R.B.Y - 1;

  New(StatusLine, Init(R,

    NewStatusDef(0, $FFFF,

      NewStatusKey('~Alt-X~ Ende', kbAltX,

                   cmQuit,

      NewStatusKey('~F10~ MenÅ', kbF10,

                   cmMenu,

      NewStatusKey('Quelle: ~F6~ Edit', kbF6,

                   cmEditAppQuelle,

      NewStatusKey('~F7~ Make', kbF7,

                   cmWriteAppQuelle,

      nil)))),

    nil)));



END;



Procedure TTVBUILDApp.OutOfMemory;

BEGIN

  MessageBox('Nicht genug Speicher fÅr diese Operation.',

    nil, mfError + mfOkButton);

END;



PROCEDURE TTVBUILDApp.Idle;

BEGIN

  TApplication.Idle;

  Heap^.Update;

END;







(* Hauptprogramm: *)





Var TVBUILDApp : TTVBUILDApp;





BEGIN
  IfCursor := True;

  SaveCursor;

  RegisterStrings;



  TVBUILDApp.Init;



  InitKeyWords;



  TVBUILDApp.Run;

  TVBUILDApp.Done;

  OrgCursor

END.
