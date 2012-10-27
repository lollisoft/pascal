Program TVtest;
(*{$DEFINE DEMO}*)

{$M 65520,8192,655360}
{$X+,S+,O+,F+}
{$G-}
{$W+}
Uses 
     Overlay,
     TvdvCmds,
(*     HelpCmds,
     HelpFile,*)
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
(*     LabHist,
     Editors,  *)
   (* FÅr Editor: *)
(*     Editor,     *)
(*     Strings,
     StrTools,
     Colors,   *)
     App,
(*     TVDVImp,*)
(*     Vid, *)
(*     CursCont, *)
     FileFind,
(*     FileShow,*)
(*     DEdit, *)
     StdDlg
(*     EMS,
     XMS*);


(*{$O Dos     }*)
(*{$O Objects }*)
(*{$O Video   }
{$O BDF     }
{$O TvdvImp }
{$O Labhist }
{$O Memory  }
{$O FileFind}
{$O CursCont}
{$O FileShow}
{$O MsgBox  }
{$O StdDlg  }
{$O App     }
{$O Editors }
{$O Views   }
{$O Menus   }
{$O Dialogs }
{$O DEdit   }
*)

Type

  ColorFiletyp         = File of TPalette;


  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Heap        : PHeapView;
    Constructor Init;
    Destructor  Done;                           Virtual;
    Procedure   Idle;                           Virtual;
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
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Vollversion !')));

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

  DisableCommands([cmPruefen,
                   cmDEDIT,
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

(*  ProgrammInformationDialog;*)

  I := 1;
  WNo := 1;


END;



Destructor TDateiverApp.Done;
Begin
  ReWrite(ColorFile);
  Write(ColorFile, ColorDat^);
  Close(ColorFile);
  TApplication.Done
END;




Procedure TDateiverApp.HandleEvent(Var Event: TEvent);
Var R: TRect;


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

END;

Procedure LadeDateien;
BEGIN
  CursorAus;
  if StrucktList = nil then
    if TestFiles then
      Video.LadeDateien
    else
      MessageBox('Es sind keine Dateien unter diesem Namen vorhanden!',
             nil, mfError + mfOkButton)

  else MessageBox('Es sind noch Daten im Arbeitsbereich!',
             nil, mfError + mfOkButton);
END;

Procedure SpeichereDateien;
BEGIN
  CursorAus;
  Video.SpeichereDateien;
  SetallNil;
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

Procedure SucheDaten(Var SM: String12);
Begin
  ExecView(New(PFileFindWindow, Init(SM)));

  SM := FileFindWindow^.GetMaskBack;

  If FileFindWindow <> Nil then
    Dispose(FileFindWindow, Done);
  CloseallFiles
end;










(* BEGIN von TDareiverApp.HandleEvent: *)

Const SM: String12 = '*.*'; (* SuchmaskenVoreinstellung *)

Var Regs: Registers;

BEGIN
  TApplication.HandleEvent(Event);



  case Event.What of
    evCommand:
    Begin

      case Event.Command of

      (* FÅr Editor: *)

        cmIfCursor            : BEGIN
                                  IfCursor := Not(IfCursor);
                                  CursorAus
                                END;
        cmTile                : DeskTop^.Tile(R);
        cmCascade             : DeskTop^.Cascade(R);

        cmProgramminfo        : ProgrammInformationDialog;
        cmDatenAufgeben       : DatenAufgeben;
        cmChangeDir           : ChangeDir;


        cmSucheDaten          : SucheDaten(SM);
        cmLaden               : LadeDateien;
        cmSpeichern           : SpeichereDateien;
        cmPruefen             : PruefeDaten('Teste Daten');
        cmNewFileName         : NewFileName('Dateiname fÅr Daten ?', FileName);
        cmChangeDir           : ChangeDir;
        (*cmDEdit               : DateneditorDialog;*)

      else
        Exit
      END;
    End;
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
                    cmDEDIT,
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
                     cmDEDIT,
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
      NewItem('~I~nfo', '',              kbNoKey, cmProgramminfo,   hcNoContext,
      Nil)),

    NewSubMenu('Da~t~ei',                                           hcNoContext, NewMenu(
      NewSubMenu('~E~ditor',                                        hcNoContext, NewMenu(
        NewItem('~L~aden...',        '', kbNoKey, cmFileOpen,       hcNoContext,
        NewItem('~S~peichern',       '', kbNoKey, cmEditorSave,     hcNoContext,
        NewItem('Speichern ~a~ls',   '', kbNoKey, cmEditorSaveAs,   hcNoContext,
        NewLine(
        NewItem('~C~lipboard',       '', kbNoKey, cmShowClip,       hcNoContext,
        Nil)))))),
      NewItem('~L~aden',           'F3', kbF3,    cmLaden,          hcNoContext,
      NewItem('~S~peichern',       'F2', kbF2,    cmSpeichern,      hcNoContext,
      NewLine(
      NewItem('~V~erzeichnis Ñndern','', kbNoKey, cmChangeDir,      hcNoContext,
      NewItem('~B~eenden',      'Alt-X', kbAltX,  cmQuit,           hcNoContext,
      nil))))))),


    NewSubMenu('~D~aten',                                           hcNoContext, NewMenu(
      NewItem('Daten im Speicher ~P~rÅfen', '', kbNoKey, cmPruefen,        hcNoContext,
      NewItem('~E~dit Daten',               '', kbNoKey, cmDEDIT,          hcNoContext,
      NewItem('~Z~eigen',                   '', kbNoKey, cmZeigeDaten,     hcNoContext,
      NewItem('~S~uchen',                   '', kbNoKey, cmSucheDaten,     hcNoContext,
      NewItem('Suche ~M~ehrfache',          '', kbNoKey, cmSucheMehrfache, hcNoContext,
      NewLine(
      NewItem('~D~aten aufgeben  ',
                               'Alt-F3', kbAltF3, cmDatenAufgeben,  hcNoContext,
      NewItem('~F~reier Speicher',   '', kbNoKey, cmMemory,         hcNoContext,
      nil))))))))),

    nil))))));
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

(*  OvrInit('TVDV.OVR');
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
*)
  EMSSF := False;
  EMSLF := False;
  EMSPF := False;
  EMSDF := False;
  EMSZF := False;

  EMSDLF := False;
  EMSDPF := False;
  EMSDDF := False;


  Regs.AH := $03;
  Intr($10, Regs);
  CursorSave := Regs.CX;

  (*DirTree.RegisterTypes;*)
  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done;

  OrgCursor


END.