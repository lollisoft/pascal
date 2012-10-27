PROGRAM Demo;

USES App, Dialogs, Objects, Views,
     Inputs;

TYPE PDemoApp = ^TDemoApp;
     TDemoApp = OBJECT(TApplication)
        CONSTRUCTOR Init;
     END;

CONSTRUCTOR TDemoApp.Init;
  FUNCTION Dialog: PDialog;
    VAR R: TRect;
        D: PDialog;
        C: PView;
    BEGIN
      R.Assign(2, 2, 30, 10);
      D := New(PDialog, Init(R, ''));
      R.Assign(2, 2, 20, 3);
      C := New(PValidInputLine, Init(R, 5, 20));
      D^.Insert(C);
      R.Assign(2, 4, 20, 5);
      C := New(PHexInputLine, Init(R, 5, 20));
      D^.Insert(C);
      Dialog := D;
    END;
  BEGIN
    TApplication.Init;
    DeskTop^.Insert(Dialog);
  END;

VAR TheDemo: TDemoApp;

BEGIN
  TheDemo.Init;
  TheDemo.Run;
  TheDemo.Done;
END.