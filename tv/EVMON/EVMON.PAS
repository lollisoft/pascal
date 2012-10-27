{************************************************}
{                                                }
{     Beispielprogramm fÅr Event-Monitor         }
{                                                }
{************************************************}

{$X+}

PROGRAM EvMon;

{$M 16384,16384,655360}

USES
  Dos, Objects, Drivers, Memory, Views, Menus,
  Dialogs, StdDlg, MsgBox, App, Ev;

CONST
  cmSomeThing = 1234;

TYPE
  PSampleApp = ^TSampleApp;
  TSampleApp = OBJECT(TEvMonApplication)
    CONSTRUCTOR Init;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
  END;


{ TSampleApp }

CONSTRUCTOR TSampleApp.Init;
VAR
  P: PWindow;
  R: TRect;
BEGIN
  TEvMonApplication.Init;
  R.Assign(2,2,40,18);
  P := New(PWindow, Init(R, 'Dummy Window', wnNoNumber));
  DeskTop^.Insert(P);
END;

PROCEDURE TSampleApp.HandleEvent(VAR Event: TEvent);

PROCEDURE SomeThing;
BEGIN
  Message(Owner, evBroadcast, $1234, @Self);
END;

BEGIN
  TEvMonApplication.HandleEvent(Event);
  CASE Event.What OF
    evCommand:
      BEGIN
        CASE Event.Command OF
          cmSomeThing: SomeThing;
        ELSE
          Exit;
        END;
        ClearEvent(Event);
      END;
  END;
END;

PROCEDURE TSampleApp.InitMenuBar;
VAR
  R: TRect;
BEGIN
  GetExtent(R);
  R.B.Y := R.A.Y+1;
  MenuBar := New(PMenuBar, Init(R, NewMenu(
    NewSubMenu('~F~ile', 100, NewMenu(
      NewItem('~S~omeThing', '', kbF3, cmSomeThing, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext, NIL))),
    NewSubMenu('~C~ommands', hcNoContext, NewMenu(
      NewItem('~S~END Command','cmCascade', kbNoKey, cmCascade, hcNoContext,
      NIL)), NIL)))));
END;

VAR
  SampleApp: TSampleApp;

BEGIN
  SampleApp.Init;
  SampleApp.Run;
  SampleApp.Done;
END.
