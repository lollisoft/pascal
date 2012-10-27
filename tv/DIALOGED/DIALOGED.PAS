PROGRAM DialogED;

USES
  App, Dialogs, Drivers, Menus, Objects, StdDlg, Views, MsgBox,
  {$IFDef Debug} Monitor, StackDmp, Streamer, HeapWalk, NameReg,
                 Heap, Tree, InstTree, {$EndIF}
  IDXFile, BaseDlg,
  Editor, StdTypes, UseDlg, WorkDlg, DosTools, Inputs, Strings, Templ;

CONST
  cmLoad        = 2201;
  cmSave        = 2202;
  cmRetrieve    = 2211;
  cmStore       = 2212;
  cmStoreUse    = 2213;
  cmDelete      = 2214;
  cmNew         = 2215;
  cmTitle       = 2216;
  cmInsStatic   = 2221;
  cmInsInput    = 2222;
  cmInsValid    = 2261;
  cmInsValue    = 2262;
  cmInsHex      = 2263;
  cmInsInt      = 2264;
  cmInsFloat    = 2265;
  cmInsRadio    = 2223;
  cmInsCheck    = 2224;
  cmInsLabel    = 2225;
  cmInsButton   = 2226;
  cmInsMemo     = 2227;
  cmInsListBox  = 2228;
  cmDelItem     = 2241;
  cmMonitor     = 2299;

{**** Date, Time and Version stamp. (c) 1990 by EMS. For Pascal! ****}
Version: WORD =       2;    { 24.11.1990 20: 1 }

TYPE
  PDialogEDApp = ^TDialogEDApp;
  TDialogEDApp = OBJECT (TApplication)
    Dialog: PWorkDialog;
    WrkDlgFile: PIndexFile;
    WrkFileName: PStr;
    {$IFDef Debug} Heap: PHeapView; {$EndIf}
    CONSTRUCTOR Init;
    PROCEDURE InitMenuBar; VIRTUAL;
    PROCEDURE InitStatusLine; VIRTUAL;
    PROCEDURE HandleEvent (VAR Event: TEvent); VIRTUAL;
    PROCEDURE Idle; VIRTUAL;
    DESTRUCTOR Done; VIRTUAL;
  END;

VAR
  DlgIDXFile: TIndexFile;
  TheFirst: BOOLEAN;

PROCEDURE ErrorBox (Message: STRING; ErrorCode: INTEGER);
  CONST Messages: ARRAY[1..2] OF STRING[20] =
         ( ( '%s not an Item in dialog'),
           ( '%s not found') );
  BEGIN
    IF MessageBox('An error occured!!!!!!!!!!!!!!!'+#13#10+
                  '%s'+#13#10,
                  @Message,
                  mfOkButton) <> cmOk THEN {$IFDEF Debug}
                                        InstanceMonitor {$ENDIf};
    RunError(255);
  END;

PROCEDURE InfoBox (Message: STRING; ErrorCode: INTEGER);
  CONST Messages: ARRAY[1..1] OF STRING[20] =
         ( ( '%s is not loadable') );
  BEGIN
    IF MessageBox('Warning!!!!!!!!!!!!!!!'+#13#10+
                  Messages[ErrorCode]+#13#10,
                  @Message,
                  mfOkButton) <> cmOk THEN {$IFDEF Debug}
                                        InstanceMonitor {$ENDIF};
  END;

PROCEDURE OpenDialogFile (S: STRING; VAR FName: PStr; VAR IFile: PIndexFile);
  VAR WrkStream: PDosStream;
      D: PDialog;
  BEGIN
    IF S <> '' THEN BEGIN
      IF FName = Nil THEN New(FName, Init(S))
                     ELSE FName^.SetStr(S);
    END;
    IF IFile <> Nil THEN Dispose(IFile,Done);
    WrkStream := New(PDosStream, Init( FName^.GetStr, stOpen));
    WHILE WrkStream^.Status <> 0 DO BEGIN
      D := PFileDialog(New(PFileDialog,
             Init(FName^.GetStr, 'Open a Dialogfile', '~N~ame', fdOpenButton, 100)));
      IF D <> Nil THEN BEGIN
        IF DeskTop^.ExecView(D) <> cmCancel THEN BEGIN
          D^.GetData(S);
          FName^.SetStr(S);
          WrkStream := New(PDosStream, Init( S, stOpen));
          IF WrkStream^.Status <> 0 THEN BEGIN
            WrkStream := New(PDosStream, Init( S+'.DLG', stOpen));
            FName^.SetStr(S+'.DLG');
            IF WrkStream^.Status <> 0 THEN BEGIN
              IF Pos('.', S) = 0 THEN S := S+'.DLG';
              WrkStream := New(PDosStream, Init( S, stCreate));
              FName^.SetStr(S);
            END;
          END;
        END;
        Dispose(D, Done);
      END;
    END;
    IFile := New(PIndexFile, Init(WrkStream, 128));
  END;

PROCEDURE NewDialog (VAR Dialog: PWorkDialog);
  VAR R: TRect;
  BEGIN
    IF Dialog <> Nil THEN BEGIN
      DeskTop^.Delete(Dialog);
      Dispose(Dialog, Done);
    END;
    R.Assign(10, 8, 70, 16);
    Dialog := New(PWorkDialog, Init(R));
    DeskTop^.Insert(Dialog);
  END;

CONSTRUCTOR TDialogEDApp.Init;
  VAR R: TRect;
      Count,I: INTEGER;
      StmVersion: WORD;
      DlgStream, WrkStream: PDosStream;
      Template: PViewTemplate;
  BEGIN
    TApplication.Init;

    DlgStream := New(PDosStream, Init(LoadPath(ParamStr(0))+'DialogED.DLG',
                                      stOpenRead));
    IF DlgStream^.Status <> 0 THEN ErrorBox('DialogED.STM',2);
    DlgIDXFile.Init(DlgStream, 128);

    WrkStream := New(PDosStream, Init( LoadPath(ParamStr(0))+'DialogED.STM',
                                       stOpenRead));
    IF WrkStream^.Status = 0             { PrÅfen der Version }
     THEN WrkStream^.Read(StmVersion, 2)
     ELSE StmVersion := Version + 1;
    IF Version = StmVersion
     THEN BEGIN
       WrkFileName := PStr(WrkStream^.Get);
       Dialog := PWorkDialog(WrkStream^.Get);
       DeskTop^.Insert(Dialog);
       WrkStream^.Read(Count, 2);
       FOR I := 1 TO Count DO BEGIN
         Template := PViewTemplate(WrkStream^.Get);
         DeskTop^.Insert(Template);
       END;
     END
     ELSE BEGIN
      WrkFileName := New(PStr, Init('*.DLG'));
      Dialog := Nil;
      NewDialog(Dialog);
    END;
    Dispose(WrkStream,Done);
    WrkDlgFile := Nil;
    OpenDialogFile (WrkFileName^.GetStr, WrkFileName, WrkDlgFile);
    R.Assign(70, 0, 80, 1);
    {$IFDEF Debug}Heap := New(PHeapView, Init(R));
    Insert(Heap); {$EndIf}
  END;

PROCEDURE TDialogEDApp.InitMenubar;
  VAR R: TRect;
  BEGIN
    GetExtent(R);
    R.B.Y := R.A.Y+1;
    Menubar := New(PMenubar, Init(R, NewMenu(
      NewSubMenu('~F~ile',   hcNoContext, NewMenu(
         NewItem('~L~oad',       '',     0, cmLoad, hcNoContext,
         NewItem('~S~ave',       '',     0, cmSave, hcNocontext,
         NewLine(
         NewItem('E~x~it',   'Alt-X', kbAltX, cmQuit, hcNoContext, Nil))))),
      NewSubMenu('~D~ialog', hcNoContext, NewMenu(
         NewItem('~R~etrieve',   '', 0, cmRetrieve,  hcNoContext,
         NewSubMenu('~S~tore', hcNoContext, NewMenu(
           NewItem('~S~tandard',    '', 0, cmStore,     hcNoContext,
           NewItem('~N~amed dialog','', 0, cmStoreUse,  hcNoContext, Nil))),
         NewItem('~D~elete',     '', 0, cmDelete,    hcNoContext,
         NewItem('~N~ew',        '', 0, cmNew,       hcNoContext,
         NewItem('~T~itle',      '', 0, cmTitle,     hcNoContext, Nil)))))),
      NewSubMenu('~I~tem',   hcNoContext, NewMenu(
         NewSubMenu('~I~nsert', hcNoContext, NewMenu(
            NewItem('~S~taticText', '', 0, cmInsStatic, hcNoContext,
            NewSubMenu('~I~nputLine', hcNoContext, NewMenu(
              NewItem('~S~tandard',  '', 0, cmInsInput,  hcNoContext,
              NewItem('~a~ny Input', '', 0, cmInsValid,  hcNoContext,
              NewItem('~H~exinput',  '', 0, cmInsHex,    hcNoContext,
              NewItem('~V~alueinput','', 0, cmInsValue,  hcNoContext,
              NewItem('~I~nteger',   '', 0, cmInsInt,    hcNoContext,
              NewItem('~F~loat',     '', 0, cmInsInt,    hcNoContext, Nil))))))),
            NewItem('~B~utton',     '', 0, cmInsButton, hcNoContext,
            NewItem('~R~adioButton','', 0, cmInsRadio,  hcNoContext,
            NewItem('~C~heckBoxes', '', 0, cmInsCheck,  hcNoConText,
            NewItem('~L~abel',      '', 0, cmInsLabel,  hcNoContext,
            NewItem('~M~emofield',  '', 0, cmInsMemo,   hcNoContext,
            NewItem('List~b~ox',    '', 0, cmInsListBox,hcNoConText, Nil))))))))),
         NewItem('~E~dit',       '', 0, cmEditItem,  hcNoContext,
         NewItem('~D~elete',     '', 0, cmDelItem,   hcNoContext,
         Nil)))),
      Nil))))));
  END;

PROCEDURE TDialogEDApp.InitStatusLine;
  VAR R: TRect;
  BEGIN
    GetExtent(R);
    R.A.Y := R.B.Y-1;
    StatusLine := New(PStatusLine, Init(R,
      NewStatusDef(0, $FFFF,
        NewStatusKey('~Alt-X~', kbAltX,  cmQuit,
        NewStatusKey('Monitor', kbNoKey, cmMonitor,
      Nil)),
    Nil)));
  END;

{$I TemplED.INC}

PROCEDURE TDialogEDApp.HandleEvent (VAR Event: TEvent);
  PROCEDURE EditTemplate(Template: PViewTemplate);
    VAR Rect: TRect;
        I: INTEGER;
    BEGIN
      I := 1;
      WHILE (I < LastProc) AND (Types[I].P <> TypeOf(Template^.Client^))
       DO Inc(I);
      IF Types[I].YSize
       THEN Rect.Assign(2, 2, 80, 25)
       ELSE Rect.Assign(2, Template^.Client^.Size.Y, 80, Template^.Client^.Size.Y);
      Template^.MinSize := Rect.A;
      Template^.MaxSize := Rect.B;
      Types[I].Proc(Template);
      TheFirst := False;
      Template^.Redraw;
    END;
  PROCEDURE NewTemplate ( View: PView; Name: STRING);
    BEGIN
      TheFirst := True;
      View := New(PViewTemplate, Init(View, View^.Size, View^.Size, Name));
      DeskTop^.Insert(View);
      EditTemplate(PViewTemplate(View));
    END;
  PROCEDURE DeleteTemplate ( View: PView );
    BEGIN
      DeskTop^.Delete(View);
      Dispose(View, Done);
    END;
  PROCEDURE RetrieveDialog(S: STRING);
    TYPE Image = RECORD Link: WORD; Data: RECORD END; END;
    VAR D: PDialog;
        Next, Scan, View: PView;
        Index: INTEGER;
        R: TRect;
    BEGIN
      IF S = '' THEN S := SelectName(WrkDlgFile, False);
      IF S <> '' THEN BEGIN
        D := PDialog(WrkDlgFile^.Get(S));
        IF D = Nil THEN BEGIN
          InfoBox(S,1);
          Exit;
        END;
        DeskTop^.Delete(Dialog);
        Dispose(Dialog, Done);
        D^.GetBounds(R);
        Dialog := New(PWorkDialog, Init(R));
        Scan := D^.Last^.Prev;
        IF D^.Title <> Nil THEN Dialog^.Title := NewStr(D^.Title^)
                           ELSE Dialog^.Title := NewStr('');
        IF Scan <> Nil THEN REPEAT
          Next := Scan^.Prev;
          IF D^.Current = Scan THEN Dialog^.Current := Scan;
          IF PView(D^.Frame) <> Scan THEN BEGIN
            D^.Delete(Scan);
            IF (TypeOf(D^) <> TypeOf(TWorkDialog)) AND
               (TypeOf(D^) <> TypeOf(TUseDialog))
             THEN Dialog^.Views^.Insert(New(PNamedView, Init('', Scan)))
             ELSE BEGIN
              Index := PUseDialog(D)^.Views^.ViewIndexOf(Scan);
              Dialog^.Views^.Insert(PUseDialog(D)^.Views^.At(Index));
              PUseDialog(D)^.Views^.AtDelete(Index);
            END;
            Dialog^.Insert(Scan);
          END;
          Scan := Next;
        UNTIL Scan = Scan^.Next;
        Dialog^.InsertAll;
        Dispose(D, Done);
        DeskTop^.Insert(Dialog);
      END;
    END;
  PROCEDURE StoreDialog(Standard: BOOLEAN);
    VAR S: STRING;
        D: PDialog;
        R: TRect;
        Index: INTEGER;
        Scan, Next: PView;
    BEGIN
      S := SelectName(WrkDlgFile, True);
      IF S <> '' THEN BEGIN
        Dialog^.GetBounds(R);
        IF Standard THEN D := New(PDialog, Init(R, ''))
                    ELSE D := New(PUseDialog, Init(R, '', Nil));
        IF Dialog^.Title = Nil THEN D^.Title := NewStr('')
                               ELSE D^.Title := NewStr(Dialog^.Title^);
        Scan := Dialog^.Last^.Prev;
        IF Scan <> Nil THEN REPEAT
          Next := Scan^.Prev;
          IF Dialog^.Current = Scan THEN D^.Current := Scan;
          IF PView(Dialog^.Frame) <> Scan THEN BEGIN
            Dialog^.Delete(Scan);
            Index := Dialog^.Views^.ViewIndexOf(Scan);
            IF TypeOf(D^) = TypeOf(TUseDialog)
             THEN PUseDialog(D)^.Views^.Insert(Dialog^.Views^.At(Index));
            Dialog^.Views^.AtDelete(Index);
            D^.Insert(Scan);
          END;
          Scan := Next;
        UNTIL Scan = Scan^.Next;
        DeskTop^.Insert(D);
        WrkDlgFile^.Put(D, S);
        DeskTop^.Delete(D);
        WrkDlgFile^.Flush;
        Dispose(D, Done);
        RetrieveDialog(S);
      END;
    END;
  PROCEDURE TitleDialog;
    VAR D: PUseDialog;
        S: STRING;
    BEGIN
      D := PUseDialog(DlgIDXFile.Get('TitleDialog'));
      IF Dialog^.Title = Nil THEN S := ''
                             ELSE S := Dialog^.Title^;
      IF NOT D^.SetItem('Title', S) THEN ErrorBox('Title',1);
      IF DeskTop^.ExecView(D) = cmOk THEN BEGIN
        IF NOT D^.GetItem('Title', S) THEN ErrorBox('Title',1);
        DisposeStr(Dialog^.Title);
        Dialog^.Title := NewStr(S);
      END;
      Dispose(D, Done);
      Dialog^.Redraw;
    END;
  PROCEDURE DeleteDialog;
    VAR S: STRING;
    BEGIN
      S := SelectName(WrkDlgFile, False);
      IF S <> '' THEN WrkDlgFile^.Delete(S);
    END;
  FUNCTION CommandHandled: BOOLEAN;
    BEGIN
      CommandHandled := False;
      CASE Event.Command OF
        cmLoad:       OpenDialogFile('*.DLG', WrkFileName, WrkDlgFile);
        cmSave:       WrkDlgFile^.Flush;
        cmStore:      StoreDialog(True);
        cmStoreUse:   StoreDialog(False);
        cmRetrieve:   RetrieveDialog('');
        cmNew:        NewDialog(Dialog);
        cmDelete:     DeleteDialog;
        cmTitle:      TitleDialog;
        cmInsButton:  NewTemplate(NewButton, 'NewButton');
        cmInsCheck:   NewTemplate(NewCheckBoxes, 'NewCheckBoxes');
        cmInsRadio:   NewTemplate(NewRadioButtons, 'NewRadioButtons');
        cmInsLabel:   NewTemplate(NewLabel, 'NewLabel');
        cmInsStatic:  NewTemplate(NewStatic, 'NewStatic');
        cmInsInput:   NewTemplate(NewInputLine, 'NewInputLine');
        cmInsValid:   NewTemplate(NewValidInputLine, 'NewValidInputLine');
        cmInsValue:   NewTemplate(NewValInputLine, 'NewValueInputLine');
        cmInsHex:     NewTemplate(NewHexInputLine, 'NewHexInputLine');
        cmInsInt:     NewTemplate(NewIntInputLine, 'NewIntInputLine');
        cmInsFloat:   NewTemplate(NewFloatInputLine, 'NewFloatInputLine');
        cmInsMemo:    NewTemplate(NewMemoField, 'NewMemoField');
        cmInsListBox: NewTemplate(NewListBox, 'NewListBox');
        cmEditItem:   IF TypeOf(DeskTop^.Current^)=TypeOf(TViewTemplate)
                       THEN EditTemplate(PViewTemplate(DeskTop^.Current));
        cmDelItem:    IF TypeOf(DeskTop^.Current^)=TypeOf(TViewTemplate)
                       THEN DeleteTemplate(DeskTop^.Current);
{$IFDEF Debug}
        cmMonitor:    InstanceMonitor; {$ENDIF}
        ELSE Exit;
      END;
      CommandHandled := True;
    END;
  BEGIN
    TApplication.HandleEvent (Event);
    CASE Event.What OF
      evCommand: IF NOT CommandHandled THEN Exit;
      ELSE Exit;
    END;
    ClearEvent(Event);
  END;

PROCEDURE TDialogEDApp.Idle;
  VAR Rect: TRect;
      I: INTEGER;
  BEGIN
    IF (TypeOf(DeskTop^.Current^) = TypeOf(TViewTemplate)) AND
       (PViewTemplate(DeskTop^.Current)^.MaxSize.X =
          PViewTemplate(DeskTop^.Current)^.MinSize.X) THEN
     WITH PViewTemplate(DeskTop^.Current) ^ DO BEGIN
      I := 1;
      WHILE (I < LastProc) AND (Types[I].P <> TypeOf(Client^))
       DO Inc(I);
      IF Types[I].YSize
       THEN Rect.Assign(2, 2, 80, 25)
       ELSE Rect.Assign(2, Client^.Size.Y, 80, Client^.Size.Y);
      PViewTemplate(DeskTop^.Current)^.MinSize := Rect.A;
      PViewTemplate(DeskTop^.Current)^.MaxSize := Rect.B;
    END;
{$IFDEF Debug}
    CheckHeap;
    Heap^.UpDate; {$ENDIF}
  END;

DESTRUCTOR TDialogEDApp.Done;
  VAR Count: INTEGER;
      WrkStream: PDosStream;
      View, Scan: PView;
  BEGIN
    WrkStream := New(PDosStream, Init(LoadPath(ParamStr(0))+'DialogED.STM',
                                      stCreate));
    WrkStream^.Write(Version, 2);
    WrkStream^.Put(WrkFileName);
    WrkStream^.Put(Dialog);
    Count := 0;
    View := DeskTop^.Last;
    Scan := View;
    IF Scan <> Nil THEN REPEAT
      IF TypeOf(Scan^) = TypeOf(TViewTemplate) THEN Inc(Count);
      Scan := Scan^.Next;
    UNTIL Scan = View;
    WrkStream^.Write(Count, 2);
    Scan := View;
    IF Scan <> Nil THEN REPEAT
      IF TypeOf(Scan^) = TypeOf(TViewTemplate) THEN WrkStream^.Put(Scan);
      Scan := Scan^.Next;
    UNTIL Scan = View;
    Dispose(WrkStream,Done);
    Dispose(WrkDlgFile,Done);
    DlgIDXFile.Done;
    TApplication.Done;
  END;

VAR
  DialogEDApp: TDialogEDApp;

{$IFDEF Debug} {$I DialogED.INC} {$ENDIF}

BEGIN
  {$IFDEF Debug} Root := Names; {$ENDIF}
  RegisterObjects;
  RegisterDialogs;
  RegisterViews;
  RegisterEditor;
  RegisterBaseDlg;
  RegisterWorkDlg;
  RegisterUseDlg;
  RegisterInputs;
  RegisterStrings;
  RegisterTempl;
  DialogEDApp.Init;
  DialogEDApp.Run;
  DialogEDApp.Done;
END.