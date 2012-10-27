{************************************************}
{                                                }
{    Beispielprogramm fÅr Schalter im MenÅ       }
{                                                }
{************************************************}

{$X+}

PROGRAM Sample;

USES
  Dos, Objects, Drivers, Memory, Views, Menus,
  Dialogs, StdDlg, MsgBox, App, EvCol;

CONST
  cmToggle = 102;
  EvCollection: PEventCollection = NIL;

TYPE
  PSampleApp = ^TSampleApp;
  TSampleApp = OBJECT(TApplication)
    CONSTRUCTOR Init;
    PROCEDURE GetEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE HandleEvent(VAR Event: TEvent); VIRTUAL;
    PROCEDURE InitMenuBar; VIRTUAL;
  END;


{ FindMenuItem }

FUNCTION FindMenuItem(Command: Word): PMenuItem;
VAR
  P: PMenuItem;
  M: PMenu;
BEGIN
  IF MenuBar <> NIL THEN BEGIN
    M := MenuBar^.Menu;
    P := M^.Default;
    WHILE (P <> NIL) AND (P^.Command <> Command) DO
      IF P^.Command = 0 THEN BEGIN
        M := P^.SubMenu;
        P := M^.Default;
      END ELSE P := NIL;
  END;
  FindMenuItem := P;
END;


{ ReturnToDefault }
{
  îffnet das MenÅ und "klappt" es bis zur zuletzt selektierten Stelle auf
}
PROCEDURE ReturnToDefault;
VAR
  P: PMenuItem;
  M: PMenu;
  kbEnterCount: Word;
  E: TEvent;
BEGIN
  kbEnterCount := 0;
  IF MenuBar <> NIL THEN BEGIN
    M := MenuBar^.Menu;
    P := M^.Default;
    WHILE (P <> NIL) AND (P^.Command = 0) DO BEGIN
      M := P^.SubMenu;
      P := M^.Default;
      Inc(kbEnterCount);
    END;
  END;
  E.What := evCommand;
  E.Command := cmMenu;
  EvCollection^.InsertEvent(E);
  E.What := evKeyDown;
  E.KeyCode := kbEnter;
  WHILE kbEnterCount > 0 DO BEGIN
    EvCollection^.InsertEvent(E);
    Dec(kbEnterCount);
  END;
END;


{ TSampleApp }

CONSTRUCTOR TSampleApp.Init;
BEGIN
  TApplication.Init;
  EvCollection := New(PEventCollection, Init(10));
END;

PROCEDURE TSampleApp.GetEvent(VAR Event: TEvent);
BEGIN
  IF EvCollection^.Count <> 0 THEN BEGIN
    EvCollection^.FetchEvent(Event);
    PutEvent(Event);
  END;
  TApplication.GetEvent(Event);
END;


PROCEDURE TSampleApp.HandleEvent(VAR Event: TEvent);

PROCEDURE Toggle;
VAR
  P: PMenuItem;
BEGIN
  P := FindMenuItem(cmToggle);
  IF P <> NIL THEN
    IF P^.Param^ = 'File' THEN BEGIN
      DisposeStr(P^.Param);
      P^.Param := NewStr('Printer');
    END
    ELSE BEGIN
      DisposeStr(P^.Param);
      P^.Param := NewStr('File');
    END;
  ReturnToDefault;
END;

BEGIN
  TApplication.HandleEvent(Event);
  CASE Event.What OF
    evCommand:
      BEGIN
        CASE Event.Command OF
          cmToggle: Toggle;
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
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext, NIL)),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      NewItem('~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext,
    NewSubMenu('~S~witches', hcNoContext, NewMenu(
      NewItem('~D~estination', 'File', kbNoKey, cmToggle, hcNoContext,
      NewItem('~N~othing', '', kbNoKey, 1000, hcNoContext, NIL))), NIL))), NIL)))));
END;

VAR
  SampleApp: TSampleApp;

BEGIN
  SampleApp.Init;
  SampleApp.Run;
  SampleApp.Done;
END.
