Program TVDV;


{$M 65520,8192,655360}
{$X+,S+,O+,F+}
{$G-}
{$W+}
Uses Overlay,
     Crt,
     Dos,
     Objects,
     Memory,
     Drivers,
     Views,
     Menus,
     Dialogs,
     StdDlg,
     MsgBox,
     LabHist,
     Editors,
     App,
     Video,
     CursCont,
     FileFind,
     FileShow,
     EMS,
     XMS;
{$O Dos     }
{$O Objects }
{$O Video   }
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


Const


    cmScann                     = 101;
    cmDiskAutoScann             = 102;
    cmNachtrag                  = 103;
    cmAufreum                   = 104;

    cmLaden                     = 106;
    cmSpeichern                 = 107;
    cmDatenAufgeben             = 108;
    cmNewFileName               = 109;
    cmChangeDir                 = 110;

    cmZeigeDaten                = 112;
    cmSucheDaten                = 113;
    cmSucheMehrfache            = 114;
    cmMemory                    = 115;

    cmNextDatei                 = 116;
    cmNextPfad                  = 117;
    cmNextLabel                 = 118;
    cmDateiMaske                = 119;
    cmEnde                      = 120;
    cmMaske                     = 121;
    cmIfCursor                  = 122;
    cmProgramminfo              = 123;

    FileName           : String12 = '.DVD';

    

Type
  PDateiverApp         = ^TDateiverApp;
  TDateiverApp         = Object(TApplication)
    Constructor Init;
    Destructor  Done; Virtual;
    Procedure   HandleEvent(Var Event: TEvent); Virtual;
    Procedure   InitMenuBar;                    Virtual;
    Procedure   InitStatusLine;                 Virtual;
    Procedure   OutOfMemory;                    Virtual;
  END;


var
    
    Laufwerk     : String[12];

    S            : TStream;


PROCEDURE ProgramminformationDialog;
  VAR
    R: tRect;
    View: pView;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (13, 2, 62, 20);
  Dialog := New (pDialog, Init (R, 'Programminformation'));
 
  R.Assign (10, 2, 38, 3);
  Dialog^.Insert (New (pStaticText, Init (R, 'TVDV fÅr DOS - Testversion !')));
 
  R.Assign (10, 4, 37, 5);
  Dialog^.Insert (New (pStaticText, Init (R, 'Katalogsystem fÅr Disketten')));
 
  R.Assign (16, 5, 31, 6);
  Dialog^.Insert (New (pStaticText, Init (R, 'und Festplatten')));
 
  R.Assign (12, 7, 33, 8);
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

  DisableCommands([cmSpeichern,     cmZeigeDaten,
                   cmNachtrag,      cmAufreum,
                   cmDatenAufgeben               ]);

  ProgrammInformationDialog


END;

Destructor TDateiverApp.Done;
BEGIN
  TApplication.Done
END;




Procedure TDateiverApp.HandleEvent(Var Event: TEvent);





function ExecDialog(P: PDialog; Data: Pointer): Word;
var
  Result: Word;
BEGIN
  Result := cmCancel;
  P := PDialog(Application^.ValidView(P));
  if P <> nil then
  BEGIN
    if Data <> nil then P^.SetData(Data^);
    Result := DeskTop^.ExecView(P);
    if (Result <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    Dispose(P, Done);
  END;
  ExecDialog := Result;
END;





function ReadString(DialogTyp: String): PDialog;
var
  D: PDialog;
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(0, 0, 38, 12);
  D := New(PDialog, Init(R, DialogTyp));
  with D^ do
  BEGIN
    Options := Options or ofCentered;

    R.Assign(3, 3, 32, 4);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);

    R.Assign(32, 3, 35, 4);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(14, 9, 24, 11);
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

    SelectNext(False);
  END;
  ReadString := D;
END;




Procedure Scanner;
var ch    : char;
    LName : String12;
    Lauf  : StrucktListPtrtyp;
BEGIN
  LabelHistList := Nil;
  LName := '';
  if Nachgetragen = True then
  BEGIN
    Dir(LName);
    if DirStatus then
    Nachgetragen := False
  end
  else
    MessageBox('Es sind noch Daten im Arbeitsbereich!',
         nil, mfError + mfOkButton)
END;


Procedure Scann;
var Info     : ^String;
    Zahl     : String[5];

BEGIN
  LabelHistList := Nil;
  New(Info);
  Repeat
    info^ := Laufwerk;
    ExecDialog(ReadString('Welches Laufwerk'), Info);
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  Scanner
END;

Procedure DiskAS;

var  Ch            : char;
     LName
   (*  LSName*)        : String12;

BEGIN
 LabelHistList := Nil;

 LName := '';

  repeat
    Mask := Laufwerk + ':\*.*';
    N := Mask;
    (*$I-*)
    if memavail > 1024 then
    BEGIN
      Dir(LName);
      If LName <> '' then
        If LabelInLabelHistList(LName) then
          MessageBox('Sie haben entweder eine Festplatte gewÑhlt, oder eine Disk ' +
                     'nochmal eingelegt!', nil, mfOkButton)
        else
          PutLabelInHistList(LName);

      If MessageBox('Noch eine Disk einlesen?',
              nil, mfInformation + mfYesButton + mfNoButton) =
         cmYes then
           Ch := 'J'
         else
           Ch := 'N'
    end
    else
    BEGIN
      MessageBox('Speicher ist voll !', Nil, mfOkButton);
      ch := 'N'
    end
    (*$I+*)
  until ch = 'N';

  Nachgetragen := False;

  DelLabelHistList
END;


Procedure DiskAutoScann;
Var Info : ^String;
BEGIN
  New(Info);
  Repeat
    info^ := Laufwerk;
    ExecDialog(ReadString('Welches Laufwerk'), Info);
    If Length(Info^) <> 1 then
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
  Until Length(Info^) = 1;
  Laufwerk := Info^;
  Dispose(Info);
  Mask := Laufwerk + ':\*.*';
  N := Mask;

  DiskAS
END;

(***********************************************)
(*                                             *)
(*         Proceduren fÅr Nachtrag             *)
(*                                             *)
(***********************************************)



Procedure PutMxonMy(x,y: byte);

begin

(* Diese Prozedur hÑngt an die jeweiligen Listen x *)
(* die Listen aus y hinten an: *)

  with M[y] do
  begin
    eh^.Next := M[x].h;
    es^.Next := M[x].s;

    M[x].h^.Back := eh;
    M[x].s^.Back := es;
  end;
end;


Procedure CopyBaumDataMxtoBaumDataMy(x, y: Byte);
var Hilf1,
    Hilf2,
    Hilf3  : StrucktListPtrtyp;
begin
  Hilf3 := M[x].s;
  CopyMinWork(y);

  While M[x].s <> Nil do
  begin
    case M[x].s^.StrucktData.art of
      1 : begin
            If M[x].s^.StrucktData.LabelList^.LabelData.Anzahl > 0 then
            begin
              SpeichereLabel(M[x].s^.StrucktData.
                             LabelList^.LabelData.
                             LabelName,
                             LabelList,
                             LabelDif);

              Dec (M[x].s^.StrucktData.LabelList^.LabelData.Anzahl);

              If M[x].s^.StrucktData.LabelList^.LabelData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.LabelList);

              
              M[x].s^.StrucktData.LabelList := labeladress;
              Hilf1 := M[x].s
            end;
            
          end;
      2 : begin
            If M[x].s^.StrucktData.PfadList^.PfadData.Anzahl > 0 then
            begin
              SpeicherePfad(M[x].s^.StrucktData.
                            PfadList^.PfadData.
                            PfadName,
                            PfadList,
                            PfadDif);

              Dec(M[x].s^.StrucktData.PfadList^.PfadData.Anzahl);

              If M[x].s^.StrucktData.PfadList^.PfadData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.PfadList);


              M[x].s^.StrucktData.PfadList := pfadadress;
              Hilf2 := M[x].s
            end;
            
          end;
      3 : begin
            If M[x].s^.StrucktData.DateiList^.DateiData.Anzahl > 0 then
            begin
              Dateiadress := Nil;
              SpeichereDatei(M[x].s^.StrucktData.
                             DateiList^.DateiData.
                             DateiName,
                             DateiList,
                             DateiDif);

              If Dateiadress = Nil then Halt;

              Dec(M[x].s^.StrucktData.DateiList^.DateiData.Anzahl);

              If M[x].s^.StrucktData.DateiList^.DateiData.Anzahl = 0 then
                Dispose(M[x].s^.StrucktData.DateiList);


              M[x].s^.StrucktData.DateiList := dateiadress;

              SpeichereSuch(Hilf1,Hilf2,dateiadress);
              if Dateiadress^.dateidata.loc = Nil then Exit
            end;
            
          end
    end;
    If M[x].s = Nil then Exit;
    M[x].s := M[x].s^.Next
  end;
  CopyWorkinM(y);
  M[x].s := Hilf3
end;


Procedure PruefeDaten(Wo: String);
Var SArt  : String;
    LA,
    HA    : Longint;
    SHA,
    SLA   : String;
    HLauf : HilfListPtrTyp;
    ZLauf : ZusatzListPtrtyp;
    SLP,
    SSP   : String;
    Lauf  : StrucktListPtrtyp;
    Anzahl: Integer;
    HAn,
    ZAn,
    PAn,
    LAn   : Integer;
    SDAn,
    SSDAn,
    SZAn  : String;

Procedure ZaehleAbs(Root: GlobalListPtrtyp);
Begin
  If Root <> Nil then
  Begin
    ZaehleAbs(Root^.Links);

    If Root^.Anzahl > 0 then
      Inc(Anzahl, Root^.Anzahl);

    ZaehleAbs(Root^.Rechts)
  End
End;

Procedure TesteZusatzaufStruckt(ZLauf: ZusatzListPtrtyp);
Var Gef : Boolean;
Begin
  Gef := False;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf = ZLauf^.ZusatzData.sl then
    Begin
      While (Lauf <> Nil) and
            (Lauf <> ZLauf^.ZusatzData.sp) do
        Lauf := Lauf^.Next;
      If Lauf <> Nil then Gef := True
    End;
    If Gef then Exit;
    Lauf := Lauf^.Next
  End;
  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!',0)
End;

Procedure TesteHilfaufStruckt(HLauf: HilfListPtrtyp);
Var Gef : Boolean;
Begin
  Gef := False;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf = HLauf^.HilfData.LabelList then
      Gef := True;
    If Gef then Exit;
    Lauf := Lauf^.Next
  End;
  If Not(Gef) then Wait('Zusatzelement nicht in Strucktliste!',0)
End;

Begin
  Wait(Wo,0);

(* Teste 1, 2, 3 von Strucktliste: *)

  Lauf := StrucktList;

  If Lauf^.Back <> Nil then
    Wait('Strucktliste nicht in ordnung',0);

  While Lauf <> Nil do
  Begin
    If Not(Lauf^.StrucktData.art In [1, 2, 3]) then
      Wait('Strucktliste hat fehlerhafte Elemente!',0);
    Lauf := Lauf^.Next
  End;

(* Teste 1, 2 von Hilfliste: *)

  HLauf := HilfList;

  If HLauf^.Back <> Nil then
    Wait ('Hilfliste nicht in ordnung!',0);

  While HLauf <> Nil do
  Begin
    If Not(HLauf^.HilfData.art In [1, 2]) then
      Wait('Hilfliste hat fehlerhafte Elemente!',0);
    HLauf := HLauf^.Next
  End;



(* PrÅfe Zeiger von Zusatz auf Struckt: *)

  ZLauf := ZusatzList;

  While ZLauf <> Nil do
  Begin
    TesteZusatzaufStruckt(ZLauf);
    ZLauf := ZLauf^.Next
  End;

(* PrÅfe Zeiger von Hilf auf Struckt: *)

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    TesteHilfaufStruckt(HLauf);
    HLauf := HLauf^.Next
  End;


(* Testen, ob die Anzahl der Dateien mit der Anzahl der *)
(* Zusatzelemente Åbereinstimmt:                        *)

  Anzahl := 0;
  ZaehleAbs(GlobalListPtrtyp(DateiList));
  ZAn := Anzahl;
  Str(ZAn, SDAn);

  Anzahl := 0;

  ZLauf := ZusatzList;

  While ZLauf <> Nil do
  Begin
    Inc(Anzahl);
    ZLauf := ZLauf^.Next
  End;
  Str(Anzahl, SZAn);

  If Anzahl <> ZAn then Wait('Anzahl <> ZA ( Datei - Zusatz )!' + Chr(13) +
                             'Datei : ' + SDAn + Chr(13) +
                             'Zusatz: ' + SZAn ,0);

(********************************************************)

(* Testen, ob die Anzahl der Dateien mit der Anzahl der *)
(* Dateielemente in der Strucktliste Åbereinstimmt:     *)

  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 3 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> ZAn then Wait('Anzahl <> SAn ( Datei - Struckt )!',0);

(********************************************************)

(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)
(* Pfadelemente in der Strucktliste Åbereinstimmt:      *)

  Anzahl := 0;

  ZaehleAbs(GlobalListPtrtyp(PfadList));
  PAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 2 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> PAn then Wait('Anzahl <> PAn ( Pfad - Struckt )!',0);

(********************************************************)

(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)
(* Labelelemente in der Strucktliste Åbereinstimmt:     *)

  Anzahl := 0;

  ZaehleAbs(GlobalListPtrtyp(LabelList));
  LAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 1 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> LAn then Wait('Anzahl <> LAn! ( Label - Struckt )',0);

(********************************************************)

(* Testen, ob die Anzahl der Labels mit der Anzahl der  *)
(* Labelelemente in der Hilfliste Åbereinstimmt:        *)

  Anzahl := 0;

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    If HLauf^.HilfData.art = 1 then Inc(Anzahl);
    HLauf := HLauf^.Next
  End;

  HAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 1 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> HAn then Wait('Label: Anzahl <> HAn! ( Hilf - Struckt )',0);

(********************************************************)

(* Testen, ob die Anzahl der Pfade mit der Anzahl der   *)
(* Pfadelemente in der Hilfliste Åbereinstimmt:         *)

  Anzahl := 0;

  HLauf := HilfList;

  While HLauf <> Nil do
  Begin
    If HLauf^.HilfData.art = 2 then Inc(Anzahl);
    HLauf := HLauf^.Next
  End;

  HAn := Anzahl;
  Anzahl := 0;
  Lauf := StrucktList;

  While Lauf <> Nil do
  Begin
    If Lauf^.StrucktData.Art = 2 then Inc(Anzahl);
    Lauf := Lauf^.Next
  End;

  If Anzahl <> HAn then Wait('Pfad: Anzahl <> HAn! ( Hilf - Struckt )',0);

(********************************************************)


  LA := 0;
  HA := 0;

  HLauf := HilfList;
  Lauf := StrucktList;


  While (Lauf <> Nil) do
  Begin
    If Lauf^.StrucktData.art <> 3 then


    Begin
      If Not(Lauf^.StrucktData.Art in [1, 2]) then
        Wait('In Strucktliste ist ein Element,' + chr(13) +
             'dessen Art nicht 1 oder 2 ist!',0);

      Inc(LA);
      Inc(HA);

      If HLauf <> Nil then
        If Not((HLauf^.HilfData.art = 1) or (HLauf^.HilfData.art = 2)) then
        Begin
          SArt := '';
          Str(HLauf^.HilfData.art, SArt);
          Wait('Hilfelement <> 1, 2: ' + SArt +
               ' an Stelle: ' + SHA,0)
        End
        Else
      Else
        Wait('Element: ' + SHA + ' ist Nil',0);

      If HLauf^.HilfData.LabelList <> Lauf then
      Begin
        Wait('Hilfelement zeigt nicht auf zugeh. Strucktelement!',0);
      End;

      HLauf := HLauf^.Next
    End;



    If Lauf^.Next = Nil then
      If Lauf^.StrucktData.Art <> 3 then
        Wait('Fataler Fehler in Strucktliste, nach Pfad oder Label keine Datei mehr!',0);
    Lauf := Lauf^.Next
  End;



  Str(LA, SLA);
  Str(HA, SHA);



  If HA <> LA then
  Begin
    Wait('Anzahl der Strucktelemente' + chr(13) +
         '(au·er Art = 3) stimmt nicht' + chr(13) +
         'Åberein mit Hilfelementen!' + chr(13) + chr(13) +
         'Strucktelemente: ' + SLA + Chr(13) +
         'Hilfelemente   : ' + SHA,0);
    OpenAllFiles;
    HA := FileSize(HilfDataFile);
    CloseAllFiles;
    Str(HA, SHA);
    Wait('Anzahl der Hilfelemente in Datei: ' + SHA,0);
  End;
End;



Procedure Nachtr;
var Help     : HilfListPtrtyp;
    HilfLauf : HilfListPtrtyp;

Procedure LoescheDiskBereich;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    Lauf : StrucktListPtrtyp;

Procedure LoescheElement(Var Anfang,
                              Ende,
                              Element          : ListenPtrtyp);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure LoescheStrucktElement(var Anfang,
                                      Ende,
                                      Element          : StrucktListPtrtyp);
var Hilf : StrucktListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!',0);
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheHilfElement(var Anfang,
                                   Ende,
                                   Element          : HilfListPtrtyp);
var Hilf : HilfListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!',0);
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheZusatzElement(var Anfang,
                                   Ende,
                                   Element    : ZusatzListPtrtyp);
var Hilf: ZusatzListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Element lîschen, das nicht da ist!',0);
    Exit
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                  ListenPtrtyp(Ende),
                  ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;



Procedure LoescheLabel(var l: String12);
begin
  l := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
  Dec(Lauf^.StrucktData.LabelList^.LabelData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)
end;

Procedure LoeschePfad(var p: String);
begin
  p := Lauf^.StrucktData.PfadList^.PfadData.PfadName;
  Dec(Lauf^.StrucktData.PfadList^.PfadData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf)
end;

Procedure LoescheDatei(zl, zp : String);
var Zaehler : Byte;
    Help,
    LocHelp : ZusatzListPtrtyp;
begin
  Zaehler := 1;

  Help := Lauf^.StrucktData.DateiList^.DateiData.Loc;
  LocHelp := Help;

  If Help = Nil then
  Begin
    Wait('Dateielement hat keine verbindung mehr zur Zusatzliste!',0);
    Halt
  End;

  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          LabelList^.LabelData.LabelName) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          PfadList^.PfadData.PfadName)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler)
  end;

  If Lauf^.StrucktData.DateiList^.DateiData.Anzahl = 1 then
  begin
    If Help <> LocHelp Then
    Begin
      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )',0);
      Exit
    End;
    Lauf^.StrucktData.DateiList^.DateiData.Loc := Nil;
  end
  else
    If Help = LocHelp then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)

      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp^.Next;
    end
    Else
    Begin
      If Help = Nil then
      Begin
        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )',0);
        Exit
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp;
    End;


  LoescheZusatzElement(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.DateiList^.DateiData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf)
end;


begin

(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)

  Lauf := HilfLauf^.HilfData.LabelList;

  (* Laufe die Strucktliste durch und lîsche die Daten: *)

  repeat
    case Lauf^.StrucktData.art of
      1 : LoescheLabel(l);
      2 : LoeschePfad(p);
      3 : LoescheDatei(l, p);
      Else
        Wait('Fehlerhaftes Element in Lîsche Diskbereich!',0);
    end

  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
end;


begin

  Wait('Nachtrag ohne Plattenpuffer.',0);

(* PrÅfen der Daten: *)

{  PruefeDaten('PrÅfe gescannte Daten'); }

(* Speichern der gescannten Daten in M1: *)

  CopyWorkinM(1);

(* ZurÅcksetzen des Mediums Work: *)

  SetAllNil;

  LadeDateien;

{  PruefeDaten('PrÅfe alte Daten nach laden'); }

  CopyWorkinM(2);

  SetallNil;

  Help := M[1].h;

(* Entfernen von alten Daten: *)

  While Help <> Nil do
  begin
    HilfLauf := M[2].h;

    (* Suche Label in Diskdaten: *)

    While (HilfLauf <> Nil) and
          (HilfLauf^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName
           <>
           Help^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName)

          do HilfLauf := HilfLauf^.Next;

    (* Wenn nicht Nil, dann ist ein alter
       Label unter diesem Namen vorhanden *)

    If HilfLauf <> Nil then
    Begin
      Wait('Lîsche Diskbereich',0);
      LoescheDiskBereich;
    End;

    (* Hole neuen Label aus gescannten Daten: *)

    repeat
      Help := Help^.Next
    until (Help^.HilfData.art = 1) or
          (Help = Nil)
  end; (* Von While M1... *)

  CopyMinWork(1);

{  PruefeDaten('PrÅfe M1'); }

  CopyMinWork(2);

{  PruefeDaten('PrÅfe M2'); }

  If M[2].s <> Nil then
  begin
    CopyBaumDataMxtoBaumDataMy(1, 2);
    PutMxonMy(1, 2);
    CopyMinWork(2)
  end
  else CopyMinWork(1);

  PruefeDaten('PrÅfe Daten zum speichern');

  SpeichereDateien
end;


Procedure Nachtr1;
Type AltLabelListPtrTyp = ^AltLabelListTyp;
     AltLabelListTyp    = Record
                            LabelName: String12;
                            Next     : AltLabelListPtrTyp
                          End;

var Help     : AltLabelListPtrTyp;
    HilfLauf : HilfListPtrtyp;
    SZahl    : String;
    Lauf     : StrucktListPtrtyp;






Procedure LoescheDiskBereich;

(* Dieses Unterprogramm soll den Datenbereich, der zu einem Label *)
(* gehîhrt, entfernen.                                            *)

var p, l : String;
    

Procedure LoescheElement(Var Anfang,
                             Ende,
                             Element          : ListenPtrtyp);
begin
  If Anfang = Element then               (* Anfang der Liste weiterrÅcken *)
    Anfang := Anfang^.Next               (* oder vorhergehenden Next -    *)
  else                                   (* Zeiger auf Element nach       *)
    Element^.Back^.Next := Element^.Next;(* Element zeigen lassen:        *)

  If Ende = Element then                 (* Ende der Liste zurÅckrÅcken *)
    Ende := Ende^.Back                   (* oder nachvolgenden Back -   *)
  else                                   (* Zeiger auf Element vor      *)
    Element^.Next^.Back := Element^.Back;(* Element zeigen lassen:      *)
end;

Procedure LoescheStrucktElement(var Anfang,
                                    Ende,
                                    Element          : StrucktListPtrtyp);
var Hilf : StrucktListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Struckt - Element lîschen, das nicht da ist!',0);
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheHilfElement(var Anfang,
                                 Ende,
                                 Element          : HilfListPtrtyp);
var Hilf : HilfListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Hilf - Element lîschen, das nicht da ist!',0);
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheZusatzElement(var Anfang,
                                   Ende,
                                   Element    : ZusatzListPtrtyp);
var Hilf: ZusatzListPtrtyp;
begin
  If Element = Nil then
  Begin
    Wait('Kann kein Zusatz - Element lîschen, das nicht da ist!',0);
    Halt
  End;
  Hilf := Element;
  Element := Element^.Next;
  LoescheElement(ListenPtrtyp(Anfang),
                 ListenPtrtyp(Ende),
                 ListenPtrtyp(Hilf));
  Dispose(Hilf)
end;


Procedure LoescheLabel(var l: String12);
begin
  l := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
  Dec(Lauf^.StrucktData.LabelList^.LabelData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoeschePfad(var p: String);
begin
  p := Lauf^.StrucktData.PfadList^.PfadData.PfadName;
  Dec(Lauf^.StrucktData.PfadList^.PfadData.Anzahl);
  LoescheStrucktElement(M[2].s, M[2].es, Lauf);
  LoescheHilfElement(M[2].h, M[2].eh, HilfLauf);
end;

Procedure LoescheDatei(zl, zp : String);
var Zaehler : Byte;
    Help,
    LocHelp : ZusatzListPtrtyp;
    Anz     : String;
begin

  Zaehler := 1;
  If Lauf = Nil then Halt;

  If Lauf^.StrucktData.DateiList = Nil then
  Begin
    Clrscr;
    Writeln('Lauf^.StrucktData.DateiList = Nil!');
    Halt
  End;

  Help := Lauf^.StrucktData.DateiList^.DateiData.Loc;

  LocHelp := Help;

  If Help = Nil then
  Begin
    Clrscr;
    Writeln('Help = Nil!');
    If Lauf^.StrucktData.DateiList^.DateiData.Anzahl < 1 then
    Begin
      Writeln('DateiData.Anzahl: ',
               Lauf^.StrucktData.DateiList^.DateiData.Anzahl);
      Writeln('Datei: ', Lauf^.StrucktData.DateiList^.DateiData.DateiName);
      Halt
    End
  End;


  While  (zl <> Help^.ZusatzData.sl^.StrucktData.
          LabelList^.LabelData.LabelName) or
         (zp <> Help^.ZusatzData.sp^.StrucktData.
          PfadList^.PfadData.PfadName)
  do
  begin
    Help := Help^.Next;
    Inc(Zaehler);
    If Zaehler > Lauf^.StrucktData.DateiList^.DateiData.Anzahl then
    Begin
      Clrscr;
      Writeln('Zusatz - Element nicht gefunden!');
      Halt
    End
  end;

  If Lauf^.StrucktData.DateiList^.DateiData.Anzahl = 1 then
  begin
    If Help <> LocHelp Then
    Begin
      Wait('Zusatzelement nicht gefunden ( DateiData.Antahl = 1 )',0);
      Halt
    End;

    Lauf^.StrucktData.DateiList^.DateiData.Loc := Nil;

  end
  else
    If Help = LocHelp then
    begin

      (* Anzahl ist grî·er als 1 und in Loc steht der gesuchte Eintrag: *)
      If LocHelp^.Next = Nil then
      Begin
        Clrscr;
        Writeln('DateiData.Loc ist letztes Element, weiterrÅcken nicht mîglich!');
        Writeln('Anzahl: ', Lauf^.StrucktData.DateiList^.DateiData.Anzahl);
        Writeln('Datei : ', Lauf^.StrucktData.DateiList^.DateiData.DateiName);
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp^.Next;
    end
    Else
    Begin
      If Help = Nil then
      Begin
        Wait('Fataler Fehler: Zusatzelement nicht gefunden ( Help = Nil )',0);
        Halt
      End;
      Lauf^.StrucktData.DateiList^.DateiData.Loc := LocHelp;
    End;


  LoescheZusatzElement(M[2].z, M[2].ez, Help);

  Dec(Lauf^.StrucktData.DateiList^.DateiData.Anzahl);

  LoescheStrucktElement(M[2].s, M[2].es, Lauf);

end;



begin

  Lauf := HilfLauf^.HilfData.LabelList;


  If Lauf^.Back <> Nil then
    Wait('Strucktelement ist nicht am Anfang der Liste!',0);

  A := 0;
(* Strucktlistenzeiger soll auf Zeiger mit gefundenem Label zeigen: *)

  

  (* Laufe die Strucktliste durch und lîsche die Daten: *)


  repeat
    Inc(A);

    Str(A, SZahl);
    case Lauf^.StrucktData.art of
      1 : LoescheLabel(l);
      2 : LoeschePfad(p);
      3 : LoescheDatei(l, p)
    Else
      Wait('Falsches Element in Strucktliste!',0);
    end
    
    
  until (Lauf^.StrucktData.art = 1) or
        (Lauf = Nil);
end;

Procedure ReinigeLPDTrees;

Procedure ReinigeLTree(Root: LabelListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeLTree(Root^.Links);
    ReinigeLTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.LabelData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.LabelData.Anzahl = 0) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigePTree(Root: PfadListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigePTree(Root^.Links);
    ReinigePTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.PfadData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.PfadData.Anzahl = 0) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigeDTree(Root: DateiListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeDTree(Root^.Links);
    ReinigeDTree(Root^.Rechts);

    If (Root^.Links <> Nil) and
       (Root^.DateiData.Anzahl = 0) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) and
       (Root^.DateiData.Anzahl = 0) then
      Dispose(Root^.Rechts);
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

    AltLabelList: AltLabelListPtrTyp;
    HilfListHilf: HilfListPtrTyp;

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

  While Lauf <> Nil do
  Begin
    If Not(Lauf^.StrucktData.Art In [1, 2, 3]) then
      Wait('Vor Speichern der neuen Daten ist in Strucktliste ein fehlerhaftes Element',0);
    If Lauf^.StrucktData.Art in [1, 2] then
    Begin
      If Lauf^.StrucktData.LabelList^.LabelData.LabelName = AltName then
        Wait('Gleicher Pfad oder Label hintereinander vorhanden!' + Chr(13) +
             'Vor Speichern der neuen Daten.',0);

      AltName := Lauf^.StrucktData.LabelList^.LabelData.LabelName;
    End;
    Lauf := Lauf^.Next
  End;
End;



begin

  Wait('Nachtrag mit Pfattenpuffer.',0);

  (* Speichern der Labels in einer zusÑtzlichen Liste: *)

  (*****************************************************)

  New(AltLabelList);
  Help := AltLabelList;

  HilfListHilf := HilfList;

  While HilfListHilf <> Nil do
  Begin
    If HilfListHilf^.HilfData.art = 1 then

    Begin
      New(Help^.Next);

      Help^.Next^.LabelName := HilfListHilf^.HilfData.LabelList^.
                               StrucktData.LabelList^.LabelData.
                               LabelName;
      Help := Help^.Next;
      Help^.Next := Nil
    End;

    HilfListHilf := HilfListHilf^.Next
  End;

  Help := AltLabelList^.Next;
  Dispose(AltLabelList);
  AltLabelList := Help;

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

  InitFiles;

  FindeDoppeld;

  Video.SpeichereDateien;

  SetAllNil;

  Dateifilename   := Df;
  Pfadfilename    := Pf;
  Labelfilename   := Lf;
  Strucktfilename := Sf;
  Hilffilename    := Hf;
  Zusatzfilename  := Zf;

  InitFiles;

  Mem1 := System.MemAvail;

  Video.LadeDateien;    (******** Hier taucht der Fehler auf! ********)

  FindeDoppeld;

  CopyWorkInM(2);

{  PruefeDaten('PrÅfe alte Daten vor Bearbeitung'); }

  SetAllNil;

(* Entfernen von alten Daten: *)

  While Help <> Nil do
  begin

    HilfLauf := M[2].h;

    (* Suche Label in Diskdaten: *)

    While (HilfLauf <> Nil) and
          (HilfLauf^.HilfData.LabelList^.StrucktData.
           LabelList^.LabelData.LabelName
           <>
           Help^.LabelName)

          do HilfLauf := HilfLauf^.Next;

    (* Wenn nicht Nil, dann ist ein alter
       Label unter diesem Namen vorhanden *)


    If HilfLauf <> Nil then
    Begin
      LoescheDiskBereich;
    End;

    (* Hole neuen Label aus gescannten Daten: *)

    repeat
      Help := Help^.Next
    until Help = Nil
  end; (* Von While M1... *)

  HilfLauf := M[2].h;
  
  CopyMInWork(2);

  If M[2].eh <> M[2].h then
  Begin
    Erase(DateiDataFile);
    Erase(PfadDataFile);
    Erase(LabelDataFile);
    Erase(ZusatzDataFile);
    Erase(HilfDataFile);
    Erase(StrucktDataFile);

{    PruefeDaten('PrÅfe alte Daten vor Speichern und Laden'); }

    Video.SpeichereDateien;

    Mem2 := System.MemAvail;


    SetAllNil;

    Video.LadeDateien;

    Mem3 := System.MemAvail;

    Writeln;
    Writeln(Mem1);
    Writeln(Mem2);
    Writeln(Mem3);
    Readln;


{    PruefeDaten('PrÅfe alte Daten nach Speichern und Laden'); }

    CopyWorkInM(2);
    SetAllNil
  End
  Else
  Begin
    LPDFelderBauen;
    ReinigeLPDTrees;
    CopyWorkInM(2);
    SetAllNil
  End;

  While AltLabelList^.Next <> Nil do
  Begin
    Help := AltLabelList^.Next;
    Dispose(AltLabelList);
    AltLabelList := Help
  End;
  Dispose(AltLabelList);



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

  InitFiles;

  Wait('Lade temponÑre Dateien.',0);

  Video.LadeDateien;

  CopyWorkInM(1);

{  PruefeDaten('PrÅfe TemponÑre Daten nach laden'); }

  FindeDoppeld;

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

  InitFiles;

  CopyMinWork(1);

{  PruefeDaten('PrÅfe M1'); }

  CopyMinWork(2);

{  PruefeDaten('PrÅfe M2'); }


  If M[2].s <> Nil then
  begin
    CopyBaumDataMxtoBaumDataMy(1, 2);
    PutMxonMy(1, 2);
    CopyMinWork(2);
  end
  else
  Begin
    CopyMinWork(1)
  End;

  FindeDoppeld;

  PruefeDaten('PrÅfe Gesamtdaten');

  Video.SpeichereDateien
end;



Procedure Nachtrag;
Var MemNew : Longint;
BEGIN
  OpenallFiles;
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

  CloseallFiles;

  If MemNew >= System.MemAvail then
  Begin
    CursorAus;
    OvrClearBuf;
    Nachtr1;
    OvrSetBuf(50 * 1024);
    CursorEin;
  End
  Else
  Begin
    CursorAus;
    OvrClearBuf;
    Nachtr;
    OvrSetBuf(50 * 1024);
    CursorEin;
  End;
  SetAllNil
END;

(***********************************************)
(*                                             *)
(*            Ende von Nachtrag                *)
(*                                             *)
(***********************************************)



Procedure Aufreum;
BEGIN
  MessageBox('Noch nicht Programmiert.',
    nil, mfError + mfOkButton);
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
  CursorEin;
END;

Procedure SpeichereDateien;
BEGIN
  CursorAus;
  Video.SpeichereDateien;
  SetallNil;
  CursorEin;
END;




procedure ChangeDir;
BEGIN
  ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
  InitFiles
END;

Procedure NewFileName;
var Info: ^String;
BEGIN
  New(Info);
  Repeat
    info^ := FileName;
    ExecDialog(ReadString('Dateiname?'), Info);
    If Info^ = '' then
    Begin
      MessageBox('Fehler in der Eingabe!',
         nil, mfError + mfOkButton);
      Exit
    End;
    If Pos('.', Info^) <> 0 then
    Begin
      If Length(Info^) > 4 then
      Begin
        Wait('Fehler in der Eingabe!',0);
        Exit
      End;
      LabelFileName   := 'LABEL'   + Info^;
      PfadFileName    := 'PFAD'    + Info^;
      DateiFileName   := 'DATEI'   + Info^;
      StrucktFileName := 'STRUCKT' + Info^;
      HilfFileName    := 'HILF'    + Info^;
      ZusatzFileName  := 'ZUSATZ'  + Info^;
      InitFiles
    End
    Else
    Begin
      If Length(Info^) > 8 then
      Begin
        Wait('Fehler in der Eingabe!',0);
        Exit
      End;
      LabelFileName   := Info^ + '.DVL';
      PfadFileName    := Info^ + '.DVP';
      DateiFileName   := Info^ + '.DVD';
      StrucktFileName := Info^ + '.DVS';
      HilfFileName    := Info^ + '.DVH';
      ZusatzFileName  := Info^ + '.DVZ';
      InitFiles
    End;
  Until Info^ <> '';
  FileName := Info^;
  Dispose(Info)
END;




Procedure ZeigeDaten;
BEGIN
  ZeigeDateien
END;


Procedure SucheMehrfache(Var SM: String12);
BEGIN
  Mehrfache := True;

  ExecView(New(PFileFindWindow, Init(SM)));

  SM := FileFindWindow^.GetMaskBack;

  If FileFindWindow <> Nil then
    Dispose(FileFindWindow, Done);
  CloseallFiles;

  Mehrfache := False;
END;


Procedure LoescheLPDTrees;

Procedure ReinigeLTree(Root: LabelListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeLTree(Root^.Links);
    ReinigeLTree(Root^.Rechts);

    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigePTree(Root: PfadListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigePTree(Root^.Links);
    ReinigePTree(Root^.Rechts);

    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);
  End;
End;

Procedure ReinigeDTree(Root: DateiListPtrTyp);
Begin
  If Root <> Nil then
  Begin
    ReinigeDTree(Root^.Links);
    ReinigeDTree(Root^.Rechts);

    If (Root^.Links <> Nil) then
      Dispose(Root^.Links);

    If (Root^.Rechts <> Nil) then
      Dispose(Root^.Rechts);
  End;
End;

Begin
  ReinigeLTree(LabelList);
  ReinigePTree(PfadList);
  ReinigeDTree(DateiList);
  Dispose(LabelList);
  Dispose(PfadList);
  Dispose(DateiList)
End;


Procedure DatenAufgeben;
BEGIN
  CursorAus;

  While StrucktList^.Next <> Nil do
  Begin
    StrucktList := StrucktList^.Next;
    Dispose(StrucktList^.Back)
  End;
  Dispose(StrucktList);

  While ZusatzList^.Next <> Nil do
  Begin
    ZusatzList := ZusatzList^.Next;
    Dispose(ZusatzList^.Back)
  End;
  Dispose(ZusatzList);

  While HilfList^.Next <> Nil do
  Begin
    HilfList := HilfList^.Next;
    Dispose(HilfList^.Back)
  End;
  Dispose(HilfList);

  LoescheLPDTrees;
  SetAllNil;

  CursorEin
END;


PROCEDURE SuchmaskenVoreinstellungDialog(Var SuchMaske: String12);
TYPE
    SuchmaskenVoreinstellungData = RECORD
      TextLen0: WORD;
      TextRec0: ARRAY [0..12] OF CHAR; 
    END;

Type LaengenTyp = Record
                    Lo,
                    Hi : Byte
                  END;



  VAR
    R      : tRect;
    View   : pView;
    Data   : SuchmaskenVoreinstellungData;
    Code   : INTEGER;
    Dialog : pDialog;

    Laenge : LaengenTyp;



BEGIN
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
  IF Code <> cmCancel THEN BEGIN

    Dialog^.GetData (Data);


    For A := 1 to Data.TextLen0 do
      SuchMaske[A] := Data.TextRec0[A - 1];
    Word(Laenge) := Data.TextLen0;
    SuchMaske[0] := Char(Laenge.Lo);
    SuchMaske := UpDate(SuchMaske);



  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;



Procedure SucheDaten(Var SM: String12);
Begin
  ExecView(New(PFileFindWindow, Init(SM)));

  SM := FileFindWindow^.GetMaskBack;

  If FileFindWindow <> Nil then
    Dispose(FileFindWindow, Done);
  CloseallFiles
end;


Procedure Memory;
Var LMem : Longint;
    SMem,
    EMem,
    XMem : Word;
    PA,
    PT   : Word;

Function GetString(Zahl: Longint): String;
Var Help : String;
Begin
  Str(Zahl, Help);
  GetString := Help
End;

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




(* BEGIN von TDareiverApp.HandleEvent: *)

Const SM: String12 = '*.*'; (* SuchmaskenVoreinstellung *)

Var Regs: Registers;

BEGIN
  TApplication.HandleEvent(Event);

  
  case Event.What of
    evCommand:
      case Event.Command of
        cmProgramminfo        : ProgrammInformationDialog;
        cmMemory              : Memory;
        cmDatenAufgeben       : DatenAufgeben;
        cmChangeDir           : ChangeDir;
        cmScann               : Scann;
        cmDiskAutoScann       : Begin
                                  (* DiskAutoScann; *)
                                  Wait('Dies ist die Demoversion',0);
                                End;
        cmNachtrag            : Nachtrag;
        cmAufreum             : Aufreum;
        cmLaden               : LadeDateien;
        cmSpeichern           : SpeichereDateien;
        cmNewFileName         : NewFileName;
        cmChangeDir           : ChangeDir;
        cmZeigeDaten          : ZeigeDaten;
        cmSucheDaten          : Begin
                                  (* SucheDaten(SM); *)
                                  Wait('Dies ist die Demoversion',0);
                                End;
        cmSucheMehrfache      : Begin
                                  (* SucheMehrfache(SM); *)
                                  Wait('Dies ist die Demoversion',0);
                                End;
        cmMaske               : SuchmaskenVoreinstellungDialog(SM);
        cmIfCursor            : BEGIN
                                  IfCursor := Not(IfCursor);
                                  CursorAus
                                END;
      else
        Exit;
      END;
  else
    Exit;
  END;
  If StrucktList <> Nil then
  BEGIN
    EnableCommands([cmSpeichern,
                    cmDatenAufgeben,
                    cmZeigeDaten,
                    cmNachtrag,
                    cmAufreum ]);

    DisableCommands([cmScann,
                     cmDiskAutoScann])
  end

  else
  BEGIN
    DisableCommands([cmSpeichern,
                     cmDatenAufgeben,
                     cmZeigeDaten,
                     cmNachtrag,
                     cmAufreum]);

    EnableCommands( [cmScann,
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
    NewSubMenu('', hcNoContext, NewMenu(
      NewItem('~I~nfo', '', kbNoKey, cmProgramminfo, hcNoContext, Nil)),

    NewSubMenu('Da~t~ei', hcNoContext, NewMenu(
      NewItem('~D~aten aufgeben  ', 'Alt-F3', kbAltF3, cmDatenAufgeben, hcNoContext,
      NewItem('~L~aden', 'F3', kbF3, cmLaden, hcNoContext,
      NewItem('~S~peichern', 'F2', kbF2, cmSpeichern, hcNoContext,
      NewLine(
      NewItem('~C~hange dir...', '', kbNoKey, cmChangeDir, hcNoContext,
      NewItem('~N~eue Datendateien', '', kbNoKey, cmNewFileName, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      nil)))))))),

    NewSubMenu('Daten ~A~ktualisieren', hcNoContext, NewMenu(
      NewItem('~S~cann', '', kbNoKey, cmScann, hcNoContext,
      NewItem('~D~isk Autoscann', '', kbNoKey, cmDiskAutoScann, hcNoContext,
      NewLine(
      NewItem('~N~achtrag', '', kbNoKey, cmNachtrag, hcNoContext,
      NewItem('~A~ufreum', '', kbNoKey, cmAufreum, hcNoContext,
      nil)))))),

    NewSubMenu('~D~aten', hcNoContext, NewMenu(
      NewItem('~Z~eigen',          '', kbNoKey, cmZeigeDaten,     hcNoContext,
      NewItem('~S~uchen',          '', kbNoKey, cmSucheDaten,     hcNoContext,
      NewItem('Suche ~M~ehrfache', '', kbNoKey, cmSucheMehrfache, hcNoContext,
      NewLine(
      NewItem('~F~reier Speicher', '', kbNoKey, cmMemory,         hcNoContext,
      nil)))))),

    NewSubMenu('~E~instellungen', hcNoContext, NewMenu(
      NewItem('~C~ursor',          '', kbNoKey, cmIfCursor,       hcNoContext,
      nil)),

    nil))))))));
END;

Procedure TDateiverApp.InitStatusLine;
var
  R: TRect;
BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~F2~ Speichern', kbF2, cmSpeichern,
      NewStatusKey('~F3~ Laden', kbF3, cmLaden,
      NewStatusKey('~Alt-F3~ Daten aufgeben', kbAltF3, cmDatenAufgeben,
      NewStatusKey('~F10~ Menu', kbF10, cmMenu,
      nil))))),
    nil)));

END;

Procedure TDateiverApp.OutOfMemory;
BEGIN
  MessageBox('Nicht genug Speicher fÅr diese Operation.',
    nil, mfError + mfOkButton);
END;


(* Hauptprogramm: *)

Var DateiverApp : TDateiverApp;



BEGIN
  
  OvrInit('DEMOTVDV.OVR');
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

  Regs.AH := $03;
  Intr($10, Regs);
  CursorSave := Regs.CX;


  DateiverApp.Init;
  DateiverApp.Run;
  DateiverApp.Done;

  OrgCursor

END.