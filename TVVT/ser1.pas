Unit Ser1;
{$F+}
Interface

uses Vid, Dos, Printer, Objects, Drivers, Memory, Views, Menus, Dialogs,
     StdDlg, MsgBox, App, Buffers, Editors,


(* Commandos: *)

  TVBCmds;

Var A: Integer;

Const SerInitFileName: String = 'SerInit.Dat';
Type

  Linestyp                   = Array [1..100] of String;


  PDatum                     = ^TDatum;
  TDatum                     = Object(TObject)
    Datum  : String;
    Constructor Init;
    Function GetData: String; Virtual;
    Procedure SetData(Date: String); Virtual;
  End;

  PAdressDatei               = ^TAdressDatei;
  TAdressDatei               = Object(TObject)
    Name   : String;
    Constructor Init(DateiName: String);
    Function GetData: String; Virtual;
    Procedure SetData(DName: String); Virtual;
  End;

  PAdressData               = ^TAdressData;
  TAdressData               = Object (TObject)
    Data: PString;
    Constructor Init(NewData: String);
    Destructor  Done; Virtual;
    Function GetData: PString;
    Procedure SetData(NewData: String);
  End;

  PAbsenderData             = ^TAdressData;
  TAbsenderData             = Object (TObject)
    Data: PString;
    Constructor Init(NewData: String);
    Destructor  Done; Virtual;
    Function GetData: PString;
    Procedure SetData(NewData: String);
  End;


  PAdressCollection         = ^TAdressCollection;
  TAdressCollection         = Object(TCollection)
    Constructor Init(A, B: Integer);
    Procedure LoadCollectionFromFile(FileName: String); Virtual;
  End;

  PAbsenderCollection         = ^TAbsenderCollection;
  TAbsenderCollection         = Object(TCollection)
    Constructor Init(A, B: Integer);
    Procedure LoadCollectionFromFile(FileName: String); Virtual;
  End;



  PMemoData                 = ^TMemoData;

  PSerienbriefData          = ^TSerienbriefData;
  TSerienbriefData          = Object(TObject)
    ADatum                   : PDatum;
    AAdressDatei             : PAdressDatei;
    AAbsenderDatei           : PAdressDatei;
    ABriefDatei              : PAdressDatei;
    ABetreff                 : PAdressDatei;
    Constructor Init;
    Procedure ReLoad; Virtual;
  End;

  PAdressListBox             = ^TAdressListBox;
  TAdressListBox             = OBject(TListBox)
    constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
    Function GetText(Item: Integer; Count: Integer): String; Virtual;
    Procedure HandleEvent(Var Event: TEvent); Virtual;
  End;

  pSerienbriefedruckenDialog = ^tSerienbriefedruckenDialog;
  tSerienbriefedruckenDialog = OBJECT (tDialog)
    AAdressCollection         : PAdressCollection;
    AAbsenderCollection       : PAdressCollection;
    AAdresseScrollBar         : pScrollbar;
    AAdresseListBox           : PListBox;
    AAbsenderListBox          : PListBox;
    AEinstellungenCheckBox    : PCheckBoxes;
    ADatumInput               : PinputLine;
    AAdressDateiInput         : PInputLine;
    AAbsenderDateiInput       : PInputLine;
    ABriefDateiInput          : PInputLine;
    ABetreffInput             : PInputLine;
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Destructor Done; Virtual;
    Procedure HandleEvent(Var Event: tEvent); Virtual;
  END;

Var ASerienBriefData: PSerienBriefData;
    ASerienBriefeDruckenDialog: PSerienBriefeDruckenDialog;
    Name: String;
    AMemo: PMemoData;

    AdressFile  : Text;
    AbsenderFile: Text;
    BriefFile   : Text;
    Lines       : Linestyp;
    Line        : Integer;
    Zaehler     : Integer;
    Stelle      : Integer;
    AbsenderNr  : Integer;

    SerInitFile    : Text;
    



PROCEDURE SerienBriefeDruckenDialog;

Implementation

Function OpenAdressFile(Var TFile: Text; Name: String): Boolean;
Begin
  If Name = '' then
  Begin
    Wait('Ein OpenAdressdateiname fehlt!');
    OpenAdressFile := False;
  End;

  (*$I-*)
  Assign(TFile, Name);
  rewrite(TFile);
  OpenAdressFile := IOResult = 0;
  (*$I+*)
End;

Function TestAdressFile(Var TFile: Text; Name: String): Boolean;
Begin
  If Name = '' then
  Begin
    Wait('Ein TestAdressdateiname fehlt!');
    TestAdressFile := False;
  End;

  (*$I-*)
  Assign(TFile, Name);
  reset(TFile);
  TestAdressFile := IOResult = 0;
  (*$I+*)
End;

Function TestSerInitFile(Var TFile: Text; Name: String): Boolean;
Begin
  If Name = '' then
  Begin
    Wait('Ein Dateiname fehlt!');
    TestSerInitFile := False;
  End;

  (*$I-*)
  Assign(TFile, Name);
  reset(TFile);
  TestSerInitFile := IOResult = 0;
  (*$I+*)
End;

Constructor TAdressData.Init(NewData: String);
Begin
  Data := NewStr(NewData)
End;

Destructor TAdressData.Done;
Begin
  DisposeStr(Data)
End;

Function TAdressData.GetData: PString;
Begin
  GetData := Data
End;

Procedure TAdressData.SetData(NewData: String);
Begin
  Data^ := NewData
End;

Constructor TAbsenderData.Init(NewData: String);
Begin
  Data := NewStr(NewData)
End;

Destructor TAbsenderData.Done;
Begin
  DisposeStr(Data)
End;

Function TAbsenderData.GetData: PString;
Begin
  GetData := Data
End;

Procedure TAbsenderData.SetData(NewData: String);
Begin
  Data^ := NewData
End;



Constructor TAdressCollection.Init(A, B:Integer);
Begin
  TCollection.Init(A, B)
End;

Procedure TAdressCollection.LoadCollectionFromFile(FileName: String);
Var
    S,
    S1: String;
Begin
  S1 := Chr(13);
  S1 := S1 + Chr(10);
  If Not TestAdressFile(AdressFile, FileName) then
    Exit;


  While Not Eof(AdressFile) do
  Begin
    Readln(AdressFile, S);
    While S[1] = ' ' do System.Delete(S, 1, 1);
    If S = S1 then
    Begin
      S := '';
      Write(CHR(7))
    End;
    Insert(New(PAdressData, Init(S)));
  End;
  Close(AdressFile)
End;



Constructor TAbsenderCollection.Init(A, B:Integer);
Begin
  TCollection.Init(A, B)
End;

Procedure TAbsenderCollection.LoadCollectionFromFile(FileName: String);
Var
    S,
    S1: String;
Begin
  S1 := Chr(13);
  S1 := S1 + Chr(10);
  If Not TestAdressFile(AbsenderFile, FileName) then
    Exit;

  While Not Eof(AbsenderFile) do
  Begin
    Readln(AbsenderFile, S);
    While S[1] = ' ' do System.Delete(S, 1, 1);
    If S = S1 then
    Begin
      S := '';
      Write(CHR(7))
    End;
    Insert(New(PAbsenderData, Init(S)));
  End;
  Close(AbsenderFile)
End;



Constructor TAdressListBox.Init(var Bounds: TRect;
                                  ANumCols: Word;
                                AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar);
  TopItem := 0
End;

Function TAdressListBox.GetText(Item: Integer; Count: Integer): String;
Var P: PString;
    S: String;
Begin
  P := PString(PAdressData(List^.At(Item))^.GetData);
  If P = Nil then
  Begin
    S := '';
    GetText := S
  End
  Else
    GetText := P^
End;


Procedure TAdressListBox.HandleEvent(Var Event: TEvent);
Begin
  Focused := - 1;

  If (Event.What = evBroadCast) and
     (Event.Command = cmScrollBarChanged) then ClearEvent(Event);

  If Event.What = evMouseDown then
  Begin
    Select;
    ClearEvent(Event)
  End;

  If Event.What = evKeyDown then
  Case Event.KeyCode of
    kbDown: Begin
              If TopItem + 5 < Range then
              Begin
                TopItem := TopItem + 5;
                Draw
              End;
              ClearEvent(Event)
            End;
    kbUp  : Begin
              If Not (TopItem - 5 < 0) then
              Begin
                TopItem := TopItem - 5;
                Draw
              End;
              ClearEvent(Event)
            End;
    Else If (Event.KeyCode <> kbESC)      and
            (Event.KeyCode <> kbTab)      and
            (Event.KeyCode <> kbShiftTab) then ClearEvent(Event)
  End;
End;


Procedure TDatum.SetData(Date: String);
Begin
  Datum := Date
End;
Procedure TAdressDatei.SetData(DName: String);
Begin
  Name := DName
End;
Function TDatum.GetData: String;
Begin
  GetData := Datum
End;
Function TAdressDatei.GetData: String;
Begin
  GetData := Name
End;



(*******************************************************)
(* Init SerienBriefData ( EintrÑge Lîschen ):          *)
(*******************************************************)

Constructor TDatum.Init;
Var y,
    m,
    d,
    dow  : Word;
Begin
  TObject.Init;

  GetDate(y,m,d,dow);

  SetData(GetString(d) + '.' +
          GetString(m) + '.' +
          GetString(y))
End;

Constructor TAdressDatei.Init(DateiName: String);
Begin
  TObject.Init;
  SetData(DateiName)
End;

Constructor TSerienBriefData.Init;
Var Data: String;
Begin
  ADatum         := New(PDatum, Init);
  If Not TestSerInitFile(SerInitFile, SerInitFileName) then
  Begin
    AAdressDatei   := New(PAdressDatei, Init('ADRESSE.DEV'));
    AAbsenderDatei := New(PAdressDatei, Init('ABSENDER.DEV'));
    ABetreff       := New(PAdressDatei, Init(''));
    ABriefDatei    := New(PAdressDatei, Init('BRIEF.DEV'));
  End
  Else
  Begin
    Readln(SerInitFile, Data);
    AAdressDatei   := New(PAdressDatei, Init(Data));
    Readln(SerInitFile, Data);
    AAbsenderDatei := New(PAdressDatei, Init(Data));
    Readln(SerInitFile, Data);
    ABetreff       := New(PAdressDatei, Init(Data));
    Readln(SerInitFile, Data);
    ABriefDatei    := New(PAdressDatei, Init(Data));
    Close(SerInitFile)
  End
End;

Procedure TSerienBriefData.ReLoad;
Var Data: String;
Begin
  If Not TestSerInitFile(SerInitFile, SerInitFileName) then
  Begin
    AAdressDatei^.SetData('ADRESSE.DEV');
    AAbsenderDatei^.SetData('ABSENDER.DEV');
    ABetreff^.SetData('');
    ABriefDatei^.SetData('BRIEF.DEV');
  End
  Else
  Begin
    Readln(SerInitFile, Data);
    AAdressDatei^.SetData(Data);
    Readln(SerInitFile, Data);
    AAbsenderDatei^.SetData(Data);
    Readln(SerInitFile, Data);
    ABetreff^.SetData(Data);
    Readln(SerInitFile, Data);
    ABriefDatei^.SetData(Data);
    Close(SerInitFile)
  End
End;

Destructor TSerienBriefeDruckenDialog.Done;
Var Name  : String;
Begin
  Assign(SerInitFile, SerInitFileName);
  Rewrite(SerInitFile);

  If IOResult <> 0 then
  Begin
    Wait('Plattenfehler: Dateivorgaben kînnen nicht gespeichert werden!');
  End
  Else
  Begin
    AAdressDateiInput^.GetData(Name);
    Writeln(SerInitFile, Name);
    Flush(SerInitFile);
    ASerienBriefData^.AAdressDatei^.SetData(Name);

    AAbsenderDateiInput^.GetData(Name);
    Writeln(SerInitFile, Name);
    Flush(SerInitFile);
    ASerienBriefData^.AAbsenderDatei^.SetData(Name);

    ABetreffInput^.GetData(Name);
    Writeln(SerInitFile, Name);
    Flush(SerInitFile);
    ASerienBriefData^.ABetreff^.SetData(Name);

    ABriefDateiInput^.GetData(Name);
    Writeln(SerInitFile, Name);
    Flush(SerInitFile);
    ASerienBriefData^.ABriefDatei^.SetData(Name);

    System.Close(SerInitFile)
  End;

  ADatumInput^.GetData(Name);
  ASerienBriefData^.ADatum^.SetData(Name);

  Dispose(AAdresseListBox, Done);
  Dispose(AAbsenderListBox, Done);

  Dispose(AAdressCollection, Done);
  Dispose(AAbsenderCollection, Done);

  Dispose(ADatumInput, Done);
  Dispose(AAdressDateiInput, Done);
  Dispose(AAbsenderDateiInput, Done);
  Dispose(ABetreffInput, Done);
  Dispose(ABriefDateiInput, Done);

  Dispose(AEinstellungenCheckBox, Done);
  TDialog.Done
End;

CONSTRUCTOR tSerienbriefedruckenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
BEGIN
  ASerienBriefData^.ReLoad;

  tDialog.Init (Bounds, aTitle);

  (*************)
  (* Schalter: *)
  (*************)

  R.Assign (50, 3, 73, 5);
  Insert (New (pButton, Init (R, 'A~l~le Briefe drucken', cmSerDialogDruckAlles, 0)));

  R.Assign (33, 3, 50, 5);
  Insert (New (pButton, Init (R, 'Brief Dru~c~ken', cmSerDialogDruck, 0)));

  R.Assign (61, 9, 73, 11);
  Insert (New (pButton, Init (R, '~E~nde', cmOk, 0)));

  R.Assign (41, 7, 73, 9);
  Insert (New (pButton, Init (R, 'Adresse auf ~U~mschlag drucken', cmSerDialogDruckUmschlag, 0)));


  R.Assign (35, 5, 73, 7);
  Insert (New (pButton, Init (R, '~A~lle Adressen auf Umschlag drucken', cmSerDialogDruckAlleUmschlag, 0)));

  (************)
  (* Adresse: *)
  (************)

  R.Assign (4, 3, 32, 8);
  AAdresseListBox := New (pAdressListBox, Init (R, 1, Nil));

  AAdressCollection := New(PAdressCollection, Init(5, 5));
  AAdressCollection^.Count := 0;
  AAdresseListBox^.NewList(AAdressCollection);

  AAdressCollection^.LoadCollectionFromFile(ASerienBriefData^.
                                            AAdressDatei^.GetData);

  AAdresseListBox^.SetRange(AAdresseListBox^.List^.Count);

  AAdresseListBox^.Options := AAdresseListBox^.Options or ofSelectable;
  AAdresseListBox^.FocusItem(0);
  AAdresseListBox^.TopItem := 0;

  R.Assign (4, 2, 20, 3);
  Insert (New (pLabel, Init (R, 'A~d~resse (Up/Dn)', AAdresseListBox)));

  (*************)
  (* Absender: *)
  (*************)

  R.Assign (4, 14, 32, 19);
  AAbsenderListBox := New (pAdressListBox, Init (R, 1, Nil));

  AAbsenderCollection := New(PAdressCollection, Init(5, 5));
  AAbsenderCollection^.Count := 0;
  AAbsenderListBox^.NewList(AAbsenderCollection);

  AAbsenderCollection^.LoadCollectionFromFile(ASerienBriefData^.
                                            AAbsenderDatei^.GetData);

  AAbsenderListBox^.SetRange(AAbsenderListBox^.List^.Count);

  AAbsenderListBox^.Options := AAbsenderListBox^.Options or ofSelectable;
  AAbsenderListBox^.FocusItem(0);
  AAbsenderListBox^.TopItem := 0;

  R.Assign (4, 13, 21, 14);
  Insert (New (pLabel, Init (R, 'A~b~sender (Up/Dn)', AAbsenderListBox)));

  (*************)
  (* Betreff:  *)
  (*************)

  R.Assign (13, 20, 73, 21);
  ABetreffInput := New (pInputLine, Init (R, 80));

  Name := ASerienBriefData^.ABetreff^.GetData;
  ABetreffInput^.SetData(Name);

  R.Assign ( 4, 20, 12, 21);
  Insert (New (pLabel, Init (R, 'Be~t~reff', ABetreffInput)));


  (*************)
  (* Optionen: *)
  (*************)

  R.Assign (4, 10, 32, 12);
  AEinstellungenCheckBox := New (pCheckBoxes,
                            Init (R,
                              NewSItem ('mit RÅckfrage',
                              NewSItem ('Adressen als Textdatei',
                              NIL))));

  R.Assign (4, 9, 20, 10);
  Insert (New (pLabel, Init (R, 'E~i~nstellungen', AEinstellungenCheckBox)));

  (**********)
  (* Datum: *)
  (**********)

  R.Assign (58, 11+1, 72, 12+1);
  ADatumInput := New (pInputLine, Init (R, 10));

  Name := ASerienBriefData^.ADatum^.GetData;
  ADatumInput^.SetData(Name);

  R.Assign (40, 11+1, 48, 12+1);
  Insert (New (pLabel, Init (R, 'Datu~m~', ADatumInput)));

  (************************)
  (* Adresstextdateiname: *)
  (************************)

  R.Assign (58, 13+1, 72, 14+1);
  AAdressDateiInput := New (pInputLine, Init (R, 80));
  Name := ASerienBriefData^.AAdressDatei^.GetData;
  AAdressDateiInput^.SetData(Name);

  R.Assign (40, 13+1, 54, 14+1);
  Insert (New (pLabel, Init (R, 'Adre~s~sdatei', AAdressDateiInput)));
  R.Assign (72, 13+1, 75, 14+1);
  Insert (New (pHistory, Init  (R, pInputLine (AAdressDateiInput), 0)));

  (************************)
  (* Brieftextdateiname:  *)
  (************************)


  R.Assign (58, 15+1, 72, 16+1);
  ABriefDateiInput := New (pInputLine, Init (R, 80));
  Name := ASerienBriefData^.ABriefDatei^.GetData;
  ABriefDateiInput^.SetData(Name);

  R.Assign (40, 15+1, 57, 16+1);
  Insert (New (pLabel, Init (R, 'Brie~f~textdatei', ABriefDateiInput)));
  R.Assign (72, 15+1, 75, 16+1);
  Insert (New (pHistory, Init  (R, pInputLine (ABriefDateiInput), 0)));

  (************************)
  (* Absenderdateiname:   *)
  (************************)


  R.Assign (58, 17+1, 72, 18+1);
  AAbsenderDateiInput := New (pInputLine, Init (R, 80));
  Name := ASerienBriefData^.AAbsenderDatei^.GetData;
  AAbsenderDateiInput^.SetData(Name);

  R.Assign (40, 17+1, 57, 18+1);
  Insert (New (pLabel, Init (R, '~A~bsenderdatei', AAbsenderDateiInput)));
  R.Assign (72, 17+1, 75, 18+1);
  Insert (New (pHistory, Init  (R, pInputLine (AAbsenderDateiInput), 0)));

  (*************)
  (* EinfÅgen: *)
  (*************)

  Insert (AAdresseListBox);
  Insert (AAbsenderListBox);
  Insert (AEinstellungenCheckBox);
  Insert (ADatumInput);
  Insert (AAdressDateiInput);
  Insert (ABriefDateiInput);
  Insert (AAbsenderDateiInput);
  Insert (ABetreffInput);

  SelectNext (FALSE);
END;




Procedure TSerienBriefeDruckenDialog.HandleEvent(Var Event: TEvent);

Procedure MakeLines(Var Line: Integer);
Var AbsenderCursor: Integer;
    Name: String;
Begin
  Line := 1;
  Lines[Line] := '';

  For A := 0 to 4 do
  Begin
    Lines[Line] := AAbsenderListBox^.GetText(A + AbsenderNr,
                                            AAbsenderListBox^.Size.X);

    If Lines[Line] <> '' then
    Begin
      While Lines[Line][1] = ' ' do System.Delete(Lines[Line], 1, 1);
      Lines[Line] := '       ' + Lines[Line];
    End;
    Inc(Line)
  End;

  For A := 1 to 50 - Length(ASerienBriefData^.ADatum^.GetData) do
    Lines[1] := Lines[1] + ' ';

  Lines[1] := Lines[1] + ASerienBriefData^.ADatum^.GetData;

(**************************************)
(* Abstand                            *)
(**************************************)

  For A := 1 to 4 do
  Begin
    Lines[Line] := '';
    Inc(Line)
  End;


(**************************************)
(* Versendungsform:                   *)
(**************************************)

  Lines[Line] := '';
  Inc(Line);

(**************************************)
(* Abstand zu Adresse:                *)
(**************************************)

  Lines[Line] := '';
  Inc(Line);

(**************************************)
(* Adresse einbauen:                  *)
(**************************************)

  For A := 0 to 4 do
  Begin
    Lines[Line] := AAdresseListBox^.GetText(A + Anzahl,
                                            AAdresseListBox^.Size.X);

    If Pos('TEL', UpDate(Lines[Line])) <> 0 then Lines[Line] := '';

    If Lines[Line] <> '' then
    Begin
      While Lines[Line][1] = ' ' do System.Delete(Lines[Line], 1, 1);
      Lines[Line] := '       ' + Lines[Line];
    End;
    Inc(Line)
  End;

(**************************************)
(* Abstand:                           *)
(**************************************)

  For A := 1 to 7 do
  Begin
    If A = 5 then
    Begin
      ABetreffInput^.GetData(Lines[Line]);
      While Lines[Line][1] = ' ' do System.Delete(Lines[Line], 1, 1);
      Lines[Line] := '       ' + Lines[Line]
    End
    Else
      Lines[Line] := '';
    Inc(Line)
  End;

  While Not Eof(BriefFile) do
  Begin
    Readln(BriefFile, Lines[Line]);
    Inc(Line)
  End;
  Write(Chr(7))
End;

Procedure PrintLines(Line: Integer);
Var
    Zeile: Integer;
Begin
  Zeile := 1;
  While Zeile < Line do
  Begin
    If Lines[Zeile] <> '' then
    Begin
      While Lines[Zeile][1] = Chr(13) do
        System.Delete(Lines[Zeile], 1, 1);

      If Pos(#10, Lines[Zeile]) <> 0 then
        System.Delete(Lines[Zeile], Pos(#10, Lines[Zeile]), 1);
    End;
    Writeln(Lst, Lines[Zeile]);
    Inc(Zeile)
  End;
  Write(Lst, Chr(12))
End;

Procedure PrintUmschlagA6;
Var AbsenderCursor,
    Line : Integer;
    Name ,
    Space: String;
Begin
  Line := 1;
  Lines[Line] := '';

  For A := 0 to 4 do
  Begin
    Lines[Line] := AAbsenderListBox^.GetText(A + AbsenderNr,
                                            AAbsenderListBox^.Size.X);

    If Pos('TEL', UpDate(Lines[Line])) <> 0 then Lines[Line] := '';


    If Lines[Line] <> '' then
    Begin
      While Lines[Line][1] = ' ' do System.Delete(Lines[Line], 1, 1);
      Lines[Line] := Lines[Line];
    End;
    If Lines[Line] <> '' then Inc(Line)
  End;

(***********************************************)
(* Master-Print  (Hier kleinschrift)           *)
(***********************************************)

  Lines[1] := Chr(27) + '!' + Chr(4) + Lines[1];
  Lines[Line - 1] := Lines[Line - 1] + Chr(27) + '!' + Chr(0);

(**************************************)
(* Adresse einbauen:                  *)
(**************************************)

  For A := 1 to 7 do
  Begin
    Lines[Line] := '';
    Inc(Line)
  End;

  B := 0;

  Space := '';

  For A := 0 to 4 do
    If Length(AAdresseListBox^.GetText(A + Anzahl,
                                AAdresseListBox^.Size.X)) > B then


      B := Length(AAdresseListBox^.GetText(A + Anzahl,
                                           AAdresseListBox^.Size.X));

  For A := 1 to 50 - B do Space := Space + ' ';

  For A := 0 to 4 do
  Begin
    Lines[Line] := AAdresseListBox^.GetText(A + Anzahl,
                                            AAdresseListBox^.Size.X);

    If Pos('TEL', UpDate(Lines[Line])) <> 0 then Lines[Line] := '';

    If Lines[Line] <> '' then
      While Lines[Line][1] = ' ' do System.Delete(Lines[Line], 1, 1);


    Lines[Line] := Space + Lines[Line];
    Inc(Line)
  End;

  PrintLines(Line)
End;


Function OpenBriefFile(Var TFile: Text; DateiName: String): Boolean;
Begin
  (*$I-*)
  Assign(TFile, DateiName);
  reset(BriefFile);
  OpenBriefFile := IOResult = 0;
  (*$I+*)
End;

Function DateiChanged: Boolean;
Var S1,
    S2: String;
Begin
  DateiChanged := False;
  If (Event.What = evKeyboard) and
     (Event.KeyCode = kbEnter) then
  Begin
    S1 := ASerienBriefData^.AAdressDatei^.GetData;
    AAdressDateiInput^.GetData(S2);
    If S1 <> S2 then
    Begin
      AAdressCollection^.Done;
      AAdressCollection^.Init(5, 5);
      AAdressCollection^.Count := 0;
      AAdresseListBox^.NewList(AAdressCollection);

      AAdressCollection^.LoadCollectionFromFile(ASerienBriefData^.
                                                AAdressDatei^.GetData);

      AAdresseListBox^.SetRange(AAdresseListBox^.List^.Count);

      AAdresseListBox^.Options := AAdresseListBox^.Options or ofSelectable;
      AAdresseListBox^.FocusItem(0);
      AAdresseListBox^.TopItem := 0;

      DateiChanged := True;
    End;

    S1 := ASerienBriefData^.AAbsenderDatei^.GetData;
    AAbsenderDateiInput^.GetData(S2);
    If S1 <> S2 then
    Begin
      AAbsenderCollection^.Done;
      AAbsenderCollection^.Init(5, 5);
      AAbsenderCollection^.Count := 0;
      AAbsenderListBox^.NewList(AAbsenderCollection);

      AAbsenderCollection^.LoadCollectionFromFile(ASerienBriefData^.
                                                AAbsenderDatei^.GetData);

      AAbsenderListBox^.SetRange(AAbsenderListBox^.List^.Count);

      AAbsenderListBox^.Options := AAbsenderListBox^.Options or ofSelectable;
      AAbsenderListBox^.FocusItem(0);
      AAbsenderListBox^.TopItem := 0;

      DateiChanged := True;
    End
  End
End;

Var N:String;
Begin
  
  If (Event.What = evKeyDown) and
     (Event.KeyCode = kbEnter) and
     AAdressDateiInput^.GetState(sfSelected) then
  Begin
  End;

(*

  R.Assign (4, 14, 32, 19);
  AAbsenderListBox := New (pAdressListBox, Init (R, 1, Nil));

  AAbsenderCollection := New(PAdressCollection, Init(5, 5));
  AAbsenderCollection^.Count := 0;
  AAbsenderListBox^.NewList(AAbsenderCollection);

  AAbsenderCollection^.LoadCollectionFromFile(ASerienBriefData^.
                                            AAbsenderDatei^.GetData);

  AAbsenderListBox^.SetRange(AAbsenderListBox^.List^.Count);

  AAbsenderListBox^.Options := AAbsenderListBox^.Options or ofSelectable;
  AAbsenderListBox^.FocusItem(0);
  AAbsenderListBox^.TopItem := 0;

  R.Assign (4, 13, 21, 14);
  Insert (New (pLabel, Init (R, 'A~b~sender (Up/Dn)', AAbsenderListBox)));

*)

  If (Event.What = evKeyDown) and
     (Event.KeyCode = kbEnter) and
     AAbsenderDateiInput^.GetState(sfSelected) then
  Begin
    AAbsenderCollection^.FreeAll;
    AAbsenderCollection^.Count := 0;
    AAbsenderListBox^.NewList(AAbsenderCollection);

    AAbsenderDateiInput^.GetData(N);
    AAbsenderCollection^.LoadCollectionFromFile(N);


    AAbsenderListBox^.SetRange(AAbsenderListBox^.List^.Count);

    AAbsenderListBox^.Options := AAbsenderListBox^.Options or ofSelectable;
    AAbsenderListBox^.FocusItem(0);
    AAbsenderListBox^.TopItem := 0;
    ReDraw;
    ClearEvent(Event)
  End;

  If (Event.What = evKeyDown) and
     (Event.KeyCode = kbEnter) and
     ABriefDateiInput^.GetState(sfSelected) then
  Begin
  End;

  TDialog.HandleEvent(Event);


  If Event.What = evCommand then
  Begin
    Case Event.Command of
      cmSerDialogDruckAlleUmschlag :
                              Begin
                                AAdressDateiInput^.GetData(Name);
                                If Not TestAdressFile(AdressFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                Zaehler := 0;
                                While Not Eof(AdressFile) do
                                Begin
                                  Readln(AdressFile, Name);
                                  Inc(Zaehler)
                                End;
                                Reset(AdressFile);

                                Zaehler := Zaehler Div 5;
                                Anzahl := 0;
                                AbsenderNr := AAbsenderListBox^.TopItem;
                                While Zaehler <> 0 do
                                Begin
                                  PrintUmschlagA6;
                                  Inc(Anzahl, 5);
                                  Dec(Zaehler)
                                End;

                                System.Close(AdressFile);
                                ClearEvent(Event)
                              End;
      cmSerDialogDruckUmschlag :
                              Begin
                                AAdressDateiInput^.GetData(Name);
                                If Not TestAdressFile(AdressFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                Zaehler := 0;
                                While Not Eof(AdressFile) do
                                Begin
                                  Readln(AdressFile, Name);
                                  Inc(Zaehler)
                                End;
                                Reset(AdressFile);

                                Anzahl     := AAdresseListBox^.TopItem;
                                AbsenderNr := AAbsenderListBox^.TopItem;
                                PrintUmschlagA6;

                                System.Close(AdressFile);
                                ClearEvent(Event)
                              End;
      cmSerDialogDruck      : Begin
                                ABriefDateiInput^.GetData(Name);
                                If Not OpenBriefFile(BriefFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                AAdressDateiInput^.GetData(Name);
                                If Not TestAdressFile(AdressFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;
                                Anzahl := AAdresseListBox^.TopItem;

                                AAbsenderDateiInput^.GetData(Name);
                                If Not TestAdressFile(AbsenderFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;
                                AbsenderNr := AAbsenderListBox^.TopItem;

                                MakeLines(Line);
                                PrintLines(Line);

                                System.Close(AdressFile);
                                System.Close(AbsenderFile);
                                System.Close(BriefFile);
                                ClearEvent(Event)
                              End;
      cmSerDialogDruckAlles : Begin
                                ABriefDateiInput^.GetData(Name);
                                If Not OpenBriefFile(BriefFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                AAbsenderDateiInput^.GetData(Name);
                                If Not TestAdressFile(AbsenderFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                Zaehler := 0;
                                While Not Eof(AbsenderFile) do
                                Begin
                                  Readln(AbsenderFile, Name);
                                  Inc(Zaehler)
                                End;
                                Reset(AbsenderFile);

                                If Zaehler Mod 5 <> 0 then
                                Begin
                                  Wait('Fehler in Absender - Datei!');
                                End;

                                AAdressDateiInput^.GetData(Name);
                                If Not TestAdressFile(AdressFile, Name) then
                                Begin
                                  Wait('Datei nicht gefunden!');
                                  ClearEvent(Event);
                                  Exit
                                End;

                                Zaehler := 0;
                                While Not Eof(AdressFile) do
                                Begin
                                  Readln(AdressFile, Name);
                                  Inc(Zaehler)
                                End;
                                Reset(AdressFile);

                                If Zaehler Mod 5 <> 0 then
                                Begin
                                  Wait('Fehler in Adress - Datei!');
                                End;

                                

                                Zaehler := Zaehler Div 5;
                                Anzahl := 0;
                                AbsenderNr := 0;

                                While Zaehler <> 0 do
                                Begin
                                  MakeLines(Line);
                                  PrintLines(Line);
                                  Reset(BriefFile);
                                  Inc(Anzahl, 5);
                                  Inc(AbsenderNr, 5);
                                  Dec(Zaehler)
                                End;

                                System.Close(AdressFile);
                                System.Close(BriefFile);
                                ClearEvent(Event)
                              End;
    End
  End
End;

PROCEDURE SerienbriefedruckenDialog;
  VAR
    R: tRect;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  Stelle := 0;
  R.Assign (0, 0, 80, 23);
  Dialog := New (pSerienbriefedruckenDialog, Init (R, 'Serienbriefe drucken'));
  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
    { Data muss ausgewertet werden ! }
  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;
End.