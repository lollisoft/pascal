PROGRAM Beispiel;

USES App, Dialogs, Drivers, Views, Objects,
     IDXFile,
     UseDlg, BaseDlg, Strings,
     Namereg, Streamer;

TYPE TMain = OBJECT (TApplication)
       PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
     END;

PROCEDURE TMain.HandleEvent (VAR Event: TEvent);
  BEGIN
    TApplication.HandleEvent(Event);
  END;

VAR Main: TMain;

PROCEDURE MainProgram;
  VAR S: PDosStream;
      DLG: TIndexFile;
      Dialog: PUseDialog;
  BEGIN
    S := New(PDosStream, Init('Beispiel.DLG', stOpenRead));
    IF S^.Status <> 0 THEN Exit;
    DLG.Init(S, 128);
    Dialog := PUseDialog(DLG.Get('Beispiel'));
    IF (Dialog = Nil) OR (DLG.Stream^.Status <> 0) THEN Exit;
    DLG.Done;
    DeskTop^.Insert(Dialog);
    Main.Run;
  END;

{$I Beispiel.INC}

BEGIN
  Root := Names;
  RegisterDialogs;
  RegisterObjects;
  RegisterViews;
  RegisterUseDlg;
  RegisterBaseDlg;
  RegisterStrings;
  Main.Init;
  MainProgram;
  Main.Done;
END.