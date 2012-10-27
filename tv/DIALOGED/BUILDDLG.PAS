PROGRAM BuildDlg;

USES
  {$IFDEF Debug} Tree, Insttree, Strings, StdDlg, App, Menus, Templ,
                 WorkDlg, Streamer, NameReg, StackDmp, {$ENDIF}
  Dialogs, Objects, Views,
  IDXFile,
  BaseDlg, DosTools, Inputs, UseDlg;

VAR
  DlgIDXFile: PIndexFile;

{$I DialogED.INC}

PROCEDURE InitProgram;
  VAR DosStream: PDosStream;
  BEGIN
    Root := Names;
    RegisterUseDlg;
    RegisterViews;
    RegisterInputs;
    RegisterBaseDlg;
    RegisterStrings;
    RegisterDialogs;
    RegisterObjects;
    DosStream := New(PDosStream, Init(LoadPath(ParamStr(1))+'DIALOGED.DLG', stCreate));
    DlgIDXFile := New(PIndexFile, Init(DosStream, 128));
  END;

PROCEDURE RegisterButtonDialog;
  VAR Rect: TRect;
      View: PView;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+30, 2+16);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Button', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 0, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+5, 4, 2+5+20, 5);
    View := New(PValidInputLine, Init(Rect, 2, 80));
    Views^.Insert(New(PNamedView, Init('Text', View)));
    Rect.Assign(1, 4, 1+5, 5);
    View := New(PLabel, Init(Rect, '~T~ext', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+6, 6, 2+6+19, 7);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('Value', View)));
    Rect.Assign(1, 6, 1+6, 7);
    View := New(PLabel, Init(Rect, '~V~alue', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+3, 8, 2+3+20, 8+2);
    View := New(PRadioButtons, Init(Rect, NewSItem('~n~ormal', NewSItem('~d~efault', Nil))));
    Views^.Insert(New(PNamedView, Init('Default', View)));
    Rect.Assign(1, 8, 1+3, 9);
    View := New(PLabel, Init(Rect, '~i~s', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+9, 11, 2+5+20, 12);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(1, 11, 1+9, 12);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 13, 10, 15);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(12, 13, 22, 15);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'ButtonDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterClusterDialog;
  VAR Rect: TRect;
      View: PView;
      I: INTEGER;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+57, 2+16);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Cluster', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 1, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(30+9, 2, 30+5+20, 3);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(29, 2, 29+9, 3);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    FOR I := 0 TO 15 DO BEGIN
      Rect.Assign(2+28*(I MOD 2) , 4+(I DIV 2), 2+25+28*(I MOD 2), 5+(I DIV 2));
      View := New(PValidInputLine, Init(Rect, 0, 80));
      Views^.Insert(New(PNamedView, Init(CHR(I+ORD('A')), View)));
    END;

    Rect.Assign(2, 13, 12, 15);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(22, 13, 32, 15);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'ClusterDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterStaticDialog;
  VAR Rect: TRect;
      View: PView;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+30, 2+11);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Static/Label', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 0, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+5, 4, 2+5+20, 5);
    View := New(PValidInputLine, Init(Rect, 2, 80));
    Views^.Insert(New(PNamedView, Init('Text', View)));
    Rect.Assign(1, 4, 1+5, 5);
    View := New(PLabel, Init(Rect, '~T~ext', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+9, 6, 2+5+20, 7);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(1, 6, 1+9, 7);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 8, 10, 10);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(12, 8, 22, 10);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'StaticDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterInputDialog;
  VAR Rect: TRect;
      View: PView;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+30, 2+11);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Input', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 1, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+8, 4, 2+8+17, 5);
    View := New(PIntInputLine, Init(Rect, 1, 2));
    Views^.Insert(New(PNamedView, Init('MaxLen', View)));
    Rect.Assign(1, 4, 1+7, 5);
    View := New(PLabel, Init(Rect, '~M~axLen', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+9, 6, 2+5+20, 7);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(1, 6, 1+9, 7);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 8, 10, 10);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(12, 8, 22, 10);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'InputDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterXInputDialog;
  VAR Rect: TRect;
      View: PView;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+30, 2+12);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Extended Input', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 1, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+8, 4, 2+8+17, 5);
    View := New(PIntInputLine, Init(Rect, 1, 2));
    Views^.Insert(New(PNamedView, Init('MaxLen', View)));
    Rect.Assign(1, 4, 1+7, 5);
    View := New(PLabel, Init(Rect, '~M~axLen', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+8, 5, 2+8+17, 6);
    View := New(PIntInputLine, Init(Rect, 1, 2));
    Views^.Insert(New(PNamedView, Init('MinLen', View)));
    Rect.Assign(1, 5, 1+7, 6);
    View := New(PLabel, Init(Rect, 'M~i~nLen', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+9, 7, 2+5+20, 8);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(1, 7, 1+9, 8);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 9, 10, 11);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(12, 9, 22, 11);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'XInputDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterAnyViewDialog;
  VAR Rect: TRect;
      View: PView;
      I: INTEGER;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+29, 2+9);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Any View', Views));

    Rect.Assign(2+5, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 0, 80));
    Views^.Insert(New(PNamedView, Init('Name', View)));
    Rect.Assign(1, 2, 1+5, 3);
    View := New(PLabel, Init(Rect, '~N~ame', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2+9, 4, 2+5+20, 5);
    View := New(PIntInputLine, Init(Rect, 1, 5));
    Views^.Insert(New(PNamedView, Init('HelpCtx', View)));
    Rect.Assign(2, 4, 2+9, 5);
    View := New(PLabel, Init(Rect, '~H~elpCtx:', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 6, 12, 8);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(14, 6, 24, 8);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'AnyViewDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE RegisterTitleDialog;
  VAR Rect: TRect;
      View: PView;
      Dialog: PUseDialog;
      Views: PViewCollection;
  BEGIN
    Rect.Assign(2, 2, 2+29, 2+7);
    Views := New(PViewCollection, Init(20, 10));
    Dialog := New(PUseDialog, Init(Rect, 'Title', Views));

    Rect.Assign(2+6, 2, 2+5+20, 3);
    View := New(PValidInputLine, Init(Rect, 0, 80));
    Views^.Insert(New(PNamedView, Init('Title', View)));
    Rect.Assign(1, 2, 1+6, 3);
    View := New(PLabel, Init(Rect, '~T~itle', View));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(2, 4, 12, 6);
    View := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
    Views^.Insert(New(PNamedView, Init('', View)));

    Rect.Assign(14, 4, 24, 6);
    View := New(PButton, Init(Rect, 'Cancel', cmCancel, bfNormal));
    Views^.Insert(New(PNamedView, Init('', View)));

    Dialog^.InsertAll;
    Dialog^.SelectNext(False);

    DlgIDXFile^.Put(Dialog, 'TitleDialog');

    Dispose(Dialog, Done);
  END;

PROCEDURE MainProgram;
  BEGIN
    RegisterButtonDialog;
    RegisterClusterDialog;
    RegisterStaticDialog;
    RegisterInputDialog;
    RegisterXInputDialog;
    RegisterAnyViewDialog;
    RegisterTitleDialog;
  END;

PROCEDURE ExitProgram;
  BEGIN
    Dispose(DlgIDXFile, Done);
  END;

BEGIN
  InitProgram;
  MainProgram;
  ExitProgram;
END.