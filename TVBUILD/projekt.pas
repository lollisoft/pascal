Unit Projekt;

Interface


Uses App, Crypt, TVBUCmds, TVBUHelp, HelpFile,
     Crt, Dos,   Objects,  Memory,   GadGets,
     Drivers,    Views,    Menus,    Dialogs,
     MsgBox,     Editors,  TVCursor, Colorsel,
     Colors,     Editor,   Strings,  StrTools,


     Klassen,

     KomBuild,
(*     SubBuild,*)


     AB_Obj,


     MyViews,

{$IFDEF XHEAP}

     XHeap,

{$ENDIF}

     CursCont,
     StdDlg;

Const ProjektFileName : String = 'Projekte.TVB';



Type

  PHelpBuild = ^THelpBuild;


   pProtectedStream = ^tProtectedTStream;
  tProtectedTStream = OBJECT (TBufStream)
    PROCEDURE Error (Code, Info: INTEGER);    VIRTUAL;
  END;









      ProjektDataTyp = Record
                         MainFile,
                         HelpFile: String;

                         AHelpVerwFile: String;

                         DescriptionLen: WORD;
                         DescriptionRec: ARRAY [0..512] OF CHAR;
                       End;




(************************************************)
(*                                              *)
(*         Projekt Daten Objekt:                *)
(*         ---------------------                *)
(*                                              *)
(*  Dieses Objekt enthÑlt alle relevanten       *)
(*  Daten fÅr ein Programm, das mit dieser      *)
(*  Applikation hergestellt werden kann.        *)
(*                                              *)
(*  AData:                                      *)
(*  ------                                      *)
(*                                              *)
(*  AData enthÑlt Dateinamen (Pascaldateien)    *)
(*  die fÅr das Projekt benîtigt oder erzeugt   *)
(*  werden.                                     *)
(*                                              *)
(*  AKommandoEditor:                            *)
(*  ----------------                            *)
(*                                              *)
(*  Grundklasse, in der cm-Konstanten usw.      *)
(*  gespeichert werden. Diese Klasse soll       *)
(*  auch die "...CMDS.PAS" erstellen(Unit).     *)
(*                                              *)
(*  Hilfekonstanten sollen ebenfalls hier       *)
(*  enthalten sein.                             *)
(*                                              *)
(*  Es stellt sich die Frage, ob zwischen:      *)
(*                                              *)
(*  Dialogkonstanten und                        *)
(*                                              *)
(*  MenÅkonstanten                              *)
(*                                              *)
(*  unterschieden werden soll?                  *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*  AMenuBuild:                                 *)
(*  -----------                                 *)
(*                                              *)
(*  AMenuBuild ist ein Container fÅr MenÅs.     *)
(*  Das Objekt soll nicht nur Daten enthalten,  *)
(*  sondern auch die "Editoren" fÅr diese       *)
(*  Daten enthalten.                            *)
(*                                              *)
(*    Hilfe fÅr das MenÅ:                       *)
(*    -------------------                       *)
(*                                              *)
(*    Dadurch, da· die Hilfe auch direckt       *)
(*    mit dem MenÅ zusammenhÑngt, sollten       *)
(*    die Hilfe - Konstanten fÅr das MenÅ       *)
(*    mit den Befehls - Konstanten abge -       *)
(*    speichert werden                          *)
(*                                              *)
(*                                              *)
(*  ADialogBuild:                               *)
(*  -------------                               *)
(*                                              *)
(*  ADialogBuild ist ein Container fÅr Dialoge  *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(*                                              *)
(************************************************)



      PProjektData = ^TProjektData;
      TProjektData = Object(TProcedureObj)
        {$IfDef XHEAP}
        AData       : XtendedPtr;

        {$ELSE}

        AData       : ProjektDatatyp;

        {$ENDIF}

        AKommandoBuild: PKommandoBuild; (* Kommandoverwaltung *)
                                        (* einschl. Dialoge   *)
                                        (* und MenÅs          *)




        AHelpVerw   : PHelpBuild;  (* Die Hilfe fÅr ein Programm *)

        Constructor Init(InitData: ProjektDatatyp);
        Constructor Load(Var S:TStream);

        Procedure   Store(Var S: TStream); Virtual;

        Destructor  Done; Virtual;

        Function    GetKey: String;
        Function    GetMainFile: String;
        Function    GetHelpVerw: PHelpBuild;
        Procedure   GetData(Var Rec); Virtual;
        Procedure   SetDescription(Var Rec; Len: Word);
        Procedure   GetDescription(Var Rec; Var Len: Word);



        Procedure   WriteCode;
        Function    ReadCode(Var ErrorData: PErrorDataList): Boolean;


        Procedure   EditMain;
        Procedure   EditVariablen;
        Procedure   EditConstanten;

        Procedure   EditRoutinen;

        Procedure   UnitsinProjektDialog;

        Procedure   WriteApp;
        Function    ReadApp: Boolean;
        Procedure   CompileApp;
        Procedure   RunApp;

        (*****************************************)

      Private

        AUnitList  : PUnitList;

        Procedure WriteAppFileTypCode;
      End;

      PProjektCollection = ^TProjektCollection;
      TProjektCollection = Object (TSortedCollection)
        Constructor Init(AInit, ADelta: Integer);
        Destructor  Done; Virtual;

        Function    GetKeyCollection: PCollection;
        Procedure   DeleteKeyCollection(Var P: PCollection);

        Function    Compare(Key1, Key2: Pointer): Integer; Virtual;

      End;

      PProjektListBox = ^TProjektListBox;
      TProjektListBox = Object (TMyListBox)
        Constructor Init(var Bounds: TRect;
                           ANumCols: Word;
                         AScrollBar: PScrollBar);

        Function GetText(Item: Integer; MaxLen: Integer): String; virtual;

      End;


      PProjekt = ^TProjekt;
      TProjekt = Object (TObject)

        AProjektData      : PProjektData;

        AProjektCollection: PProjektCollection;
        ASelectedProjekt  : Integer;
        OldFocused        : Integer;

        OldDir  : String;
        WorkDir : String;

        Constructor Init(Dir: String);
        Constructor Load(Var S: TStream);
        Procedure   Store(Var S: TStream);

        Destructor Done; Virtual;

        Procedure Laden; Virtual;
        Procedure Speichern; Virtual;
        Procedure Definieren; Virtual;
        Procedure Selektieren; Virtual;

        Function GetProjekt: Pointer;

        Procedure SetProjektModus;

        Procedure ChWorkDir; Virtual;

        (***********************)
        (* MenÅ - Bearbeitung: *)
        (***********************)

        Procedure EditMenu;

        (*************************)
        (* Subroutinen Editieren *)
        (*************************)

        Procedure EditiereSubroutinen;

        (***************************)
        (* Hauptprogramm Editieren *)
        (***************************)

        Procedure EditMain;


        (***************************)
        (* Quelltext     Editieren *)
        (***************************)

        Function EditAppQuelle(Var S:String): Boolean;


        (***************************)
        (* Variablen     Editieren *)
        (***************************)

        Procedure VariablenEditieren;

        (***************************)
        (* Constanten    Editieren *)
        (***************************)

        Procedure ConstantenEditieren;


        (********************)
        (* Unit's Editieren *)
        (********************)

        Procedure EditiereUnits;


        (******************)
        (* Hilfe - System *)
        (******************)

        Procedure ScannProjektforCommands;

        Procedure HilfeKontextEditierenDialog;

        (* Klassen - Editor *)

        Procedure ClassenEditieren;



        Procedure WriteAppSources;
        Procedure ReadAppSources;
        Procedure CompileApp;
        Procedure RunApp;
        Procedure MakeAndRun;



(*        Procedure HandleEvent(Var Event :TEvent);*)

      End;

      pProjektSelektierenDialog = ^tProjektSelektierenDialog;
      tProjektSelektierenDialog = OBJECT (tDialog)
        CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
        Procedure HandleEvent(Var Event: TEvent); Virtual;
      END;







     PHelpCommandData = ^THelpCommandData;
     THelpCommandData = Record
                          HelpCtxCommandName: String; (* cm...      *)
                          HelpCode: Integer;          (* Int - Wert *)

                          HelpText: Array [0..1024*4] of Char;
                        End;

     PHelpCommand = ^THelpCommand;
     THelpCommand = Object (TObject)
        {$IFDEF XHEAP}

        AData: XtendedPtr;

        {$ELSE}

        AData: THelpCommandData;

        {$ENDIF}


        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure   Store(Var S: TStream);

        Destructor Done; Virtual;

        Function GetCommand: String;
     End;


     PHelpCommandCollection = ^THelpCommandCollection;
     THelpCommandCollection = Object (TSortedCollection)

       Constructor Init(ALimit, ADelta: Integer);
       Destructor Done; Virtual;
       function Compare(Key1, Key2: Pointer): Integer; virtual;
       function KeyOf(Item: Pointer): Pointer; Virtual;

     End;

     THelpBuild = Object (TObject)
       AHelpCommandCollection : PHelpCommandCollection;

(*       Function GetHelpCommandCollection: PHelpCommandCollection;*)


(* Achtung !!!     HelpCtx - Text mu· irgendwo sein! *)


       FileName: String;

       Constructor Init(F: String);
       Constructor Load(Var S: TStream);

       Procedure Store(Var S: TStream);
       Destructor  Done; Virtual;


       Procedure HilfeKontextEditierenDialog(F: String);

       Function  FindNewCommandsFromMain: Boolean; Virtual;
       Function  AreCommandsDifferent: ShortInt; Virtual;
                                 (* 1  = Neue Daten in Main   *)
                                 (* 0  = Kein Unterschied     *)
                                 (* -1 = Daten in Projekt neu ???? *)

       Procedure GetCommandsFromMain(ASelectedProjekt: ProjektDatatyp); (* ---> Einlesen von Programm *)
       Procedure SetCommandsToMain;   (* <--- Ins Programm zurÅck.. *)

       (* Projektdaten der Verwaltung *)

       Procedure LoadCommandsFromProjekt; (* Laden der Daten *)
       Procedure SaveCommandsToProjekt;   (* Speichern der " *)

     End;


     PCommandListBox = ^TCommandListBox;
     TCommandListBox = Object(TListBox)
       Constructor Init(var Bounds: TRect;
                       ANumCols: Word;
                     AScrollBar: PScrollBar);
       Destructor Done; Virtual;
       Function GetText(Item: Integer; MaxLen: Integer): String; Virtual;
     End;



     pHilfeKontextEditierenDialog = ^tHilfeKontextEditierenDialog;
     tHilfeKontextEditierenDialog = OBJECT (tDialog)
       CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr; AFile: String);
       Procedure HandleEvent(Var Event: TEvent); Virtual;
     END;


Const
     RHelpVerw : TStreamRec = (
       ObjType: StRegHelpVerw;
       VmtLink: Ofs(TypeOf(THelpBuild)^);
       Load   : @THelpBuild.Load;
       Store  : @THelpBuild.Store
     );

     RHelpCommand : TStreamRec = (
       ObjType: StRegHelpCommand;
       VmtLink: Ofs(TypeOf(THelpCommand)^);
       Load   : @THelpCommand.Load;
       Store  : @THelpCommand.Store
     );

     RProjekt : TStreamRec = (
       ObjType: StRegProjekt;
       VmtLink: Ofs(TypeOf(TProjekt)^);
       Load   : @TProjekt.Load;
       Store  : @TProjekt.Store
     );

     RProjektData : TStreamRec = (
       ObjType: StRegProjektData;
       VmtLink: Ofs(TypeOf(TProjektData)^);
       Load   : @TProjektData.Load;
       Store  : @TProjektData.Store
     );

     RProjektCollection : TStreamRec = (
       ObjType: StRegProjektCollection;
       VmtLink: Ofs(TypeOf(TProjektCollection)^);
       Load   : @TProjektCollection.Load;
       Store  : @TProjektCollection.Store
     );


Var AProjekt: PProjekt;

VAR
  RezFile:   tResourceFile;
  RezStraem: pStream;

  MyStream: TDosStream;

  


Procedure InitProjekt(D: String);
Procedure DoneProjekt;
Procedure ChangeDir;
Procedure InitKeyWords;

Implementation

{$I HBuild.pas}





Procedure TProjektData.WriteAppFileTypCode;
Begin
  Writeln(F, 'Program ' + GetMainFile + ';');

  Writeln(F);
  Writeln(F, '  (************************************************************)');
  Writeln(F, '  (*                    Achtung, wichtig !!!                  *)');
  Writeln(F, '  (************************************************************)');
  Writeln(F);

  Writeln(F, '  (************************************************************)');
  Writeln(F, '  (* éndern Sie bitte nicht direkt den Quellcode, er wird von *)');
  Writeln(F, '  (* TVBUILD erzeugt und Ihre énderungen werden wieder Åber - *)');
  Writeln(F, '  (* schrieben, wenn der Quellcode neu erzeugt wird !!!       *)');
  Writeln(F, '  (************************************************************)');
  Writeln(F);
End;

Procedure TProjektData.WriteCode;
Begin
  WriteAppFileTypCode;

  If AUnitList      <> Nil then AUnitList^.WriteCode;
  If ATypeList      <> Nil then ATypeList^.WriteCode;
  If AConstList     <> Nil then AConstList^.WriteCode;
  If AVarList       <> Nil then AVarList^.WriteCode;
  If AProcedureList <> Nil then AProcedureList^.WriteCode;

  Writeln(F, 'Begin');

  If AMainCode <> Nil then AMainCode^.WriteCode;

  Writeln(F, 'End.');
End;

Function TProjektData.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Var S: String;
Begin
  (****************************************)
  (* Beginn mit dem Lesen der Quelldatei: *)
  (****************************************)

  While Not ASourceFile^.Is_EOF Do
  Begin
    Ch := ASourceFile^.GetChar;

    Case CheckWort of
      Bez     : Begin
                  If Token = 'USES' then
                  Begin
                    (******************************************)
                    (* Units kînnen nur in TProjektData sein! *)
                    (******************************************)
                    Wait('Uses');
                    AUnitList^.ReadCode(ErrorData);
                  End;

                  If Token = 'PROGRAM' then
                  Begin
                    SkipBlank;
                    S := Token;
                    Token := ReadWord;
                    SkipBlank;

                    If Token <> GetMainFile then
                      Wait('Token ' +
                            Token +
                           ' ist nicht ' +
                            GetMainFile + '.');

                    (*****************************************)
                    (* Ererbte Funktion 'ReadCode' aufrufen. *)
                    (* Diese liest evtl. weitere Daten aus   *)
                    (* der Datei, bevor das Hauptprogramm    *)
                    (* eingelesen wird. (Var, Const...)      *)
                    (*****************************************)

                    Wait('TProjektData Token: ' + Token);
                    TProcedureObj.ReadCode(ErrorData);



                    (* Hauptprogramm: *)

                    Wait('Read Main');

                    If Token = 'BEGIN' then
                    Begin
                      Wait('Reading...');
                      Ending := '.';
                      AMainCode^.ReadCode(ErrorData);
                      Ending := ';'
                    End;
                  End
                End;
      End;
    
  End;
End;


Function TProjektData.ReadApp: Boolean;
Begin
  CommentEbene := 0;
  KlammerEbene := 0;

  Token := '';

  ASourceFile := New(pSourceFile, Init(GetKey));

  Error := ASourceFile^.OpenFile;


  ReadCode(ErrorData);

  Wait('Ergebnis der Bearbeitung: ' + Chr(13) +
       GetString(ASourceFile^.Zeile) + ' Zeilen.');

  Dispose(ASourceFile, Done);
End;

Procedure TProjektData.UnitsinProjektDialog;
Begin
  If AUnitList <> Nil then AUnitList^.Edit;
End;

Procedure TProjektData.EditMain;
Begin
  If AMainCode <> Nil then AMainCode^.Edit;
End;

Procedure TProjektData.EditVariablen;
Begin
  If AVarList <> Nil then AVarList^.Edit;
End;

Procedure TProjektData.EditConstanten;
Begin
  If AConstList <> Nil then AConstList^.Edit;
End;

Procedure TProjektData.EditRoutinen;
Begin
  If AProcedureList <> Nil then AProcedureList^.Edit;
End;


Procedure TProjektData.RunApp;
Var Event: TEvent;
    P,
    D,
    E,
    OldDir,
    Befehl : String;
Begin
  FSplit(GetKey, P, D, E);

  Befehl := ' /c ' + 'C:\tp\tpu\' + D;

  Event.What := evBroadCast;
  Event.Command := cmDosShell;
  Event.InfoPtr := @Befehl;

  Application^.TopView^.HandleEvent(Event);
End;

Procedure TProjektData.CompileApp;
Var Event: TEvent;
    P,
    D,
    E,
    OldDir,
    Befehl : String;
Begin
  FSplit(GetKey, P, D, E);

  If (Length(P) > 3) and (P[Length(P)] = '\') then System.Delete(P, Length(P), 1);

  GetDir(0, OldDir);
  ChDir(P);

  Event.What := evBroadCast;
  Event.Command := cmDosShell;
  Event.InfoPtr := @Befehl;

  Befehl := ' /c compile ' + D  + ' x';

  Application^.TopView^.HandleEvent(Event);

  ChDir(OldDir);
End;





Procedure ChangeDir;
BEGIN
  ExecDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
  (*InitFiles(VFileName)*)
END;


Function ReadString(DialogTyp: String): PDialog;
var
  D: PDialog;
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(2, 0, 78, 8);
  D := New(PDialog, Init(R, DialogTyp));
  with D^ do
  BEGIN
    Options := Options or ofCentered;

    R.Assign(3, 3, 32, 4);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);

    R.Assign(32, 3, 35, 4);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(48, 3, 58, 5);
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    Inc(R.A.X, 12); Inc(R.B.X, 12);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));

    SelectNext(False);
  END;
  ReadString := D;
END;


Procedure NewFileName(Name: String; Var FileName: String);
var Info: ^String;
BEGIN
  New(Info);
  Repeat
    info^ := FileName;
    If ExecDialog(ReadString(Name), Info)
        = cmCancel Then Exit;
    If Info^ = '' Then
    Begin
      Wait('Sie mÅssen mindestens einen Buchstaben eingeben!')
    End
    Else
    If Pos('.', Info^) <> 0 Then
    Begin
      Wait('Bitte nur Dateinamen ohne Extender Thengeben!');
      Info^ := ''
    End
    Else
    Begin
      If Length(Info^) > 8 Then
      Begin
        Wait('Ihr Dateiname hat mehr als 8 Zeichen!');
        Info^ := ''
      End
    End;
  Until Info^ <> '';
  FileName := Info^;
  Dispose(Info)
END;




(***********************************)
(* Verbindung Zur Unit herstellen: *)
(***********************************)

Procedure InitProjekt(D: String);
Begin
  MyStream.Init(ProjektFileName, stOpenRead);
  AProjekt := PProjekt(MyStream.Get);

  If MyStream.ErrorInfo <> stOk then
    AProjekt := New(PProjekt, Init(D));

  MyStream.Done;
End;

(***********************************)
(* Verbindung lîsen und Speicher   *)
(* freigeben.                      *)
(***********************************)

Procedure DoneProjekt;
Begin
  Dispose(AProjekt, Done)
End;



(******************************)
(* Element einer Projektliste *)
(******************************)


Constructor TProjektData.Init(InitData: ProjektDatatyp);
Begin
  TProcedureObj.Init;

  AHelpVerw := New(PHelpBuild, Init(InitData.MainFile));

{$IFDEF XHEAP}

  If MemAvailX(SizeOf(InitData)) Then
  Begin
    GetMemX(AData, SizeOf(InitData));
    ProjektDatatyp(AData.P^) := InitData
  End
  Else
  Begin
    Wait('Kein Speicher mehr da (XHeap)!');
    Halt
  End;
{$ELSE}

  AData := InitData;



{$ENDIF}

  AUnitList   := New(PUnitList, Init);


End;



(*********************)
(* Stream - Transfer *)
(*********************)

Constructor TProjektData.Load(Var S:TStream);
Begin
  TProcedureObj.Load(S);

  AHelpVerw := New(PHelpBuild, Load(S));

{$IFDEF XHEAP}


  If MemAvailX(SizeOf(ProjektDatatyp)) Then
  Begin
    GetMemX(AData, SizeOf(ProjektDatatyp));
    S.Read(ProjektDatatyp(AData.P^), SizeOf(ProjektDatatyp))
  End
  Else
  Begin
    Wait('Kein Speicher mehr da (XHeap)!');
    Halt
  End;

{$ELSE}

    S.Read(AData, SizeOf(ProjektDatatyp));


{$ENDIF}

  AUnitList   := New(PUnitList, Load(S));
End;

Procedure TProjektData.Store(Var S: TStream);
Begin
  TProcedureObj.Store(S);

  AHelpVerw^.Store(S);

{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);
  S.Write(AData.P^, SizeOf(ProjektDatatyp))

{$ELSE}

  S.Write(AData, SizeOf(ProjektDatatyp));


{$ENDIF}

  AUnitList^.Store(S);


End;





Destructor TProjektData.Done;
Begin
{$IFDEF XHEAP}

  If AData.P <> Nil Then FreeMemX(AData, SizeOf(ProjektDatatyp));

{$ENDIF}

  TObject.Done
End;


Function TProjektData.GetKey: String;
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);
  GetKey := ProjektDatatyp(AData.P^).MainFile
{$ELSE}

  GetKey := AData.MainFile

{$ENDIF}

End;

Function TProjektData.GetMainFile: String;
Var P, D, E: String;
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);
  FSplit(ProjektDatatyp(AData.P^).MainFile, P, D, E);

  GetMainFile := D
{$ELSE}

  FSplit(AData.MainFile, P, D, E);


  GetMainFile := D

{$ENDIF}

End;


Procedure TProjektData.GetData(Var Rec);
Var Data: ProjektDataTyp Absolute Rec;
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);

  Data := ProjektDatatyp(AData.P^);

{$ELSE}

  Data := AData;

{$ENDIF}
End;


Function TProjektData.GetHelpVerw: PHelpBuild;
Begin
  GetHelpVerw := AHelpVerw
End;


Procedure TProjektData.SetDescription(Var Rec; Len: Word);
Var Ar: Array[0..512] of Char Absolute Rec;
    A : Integer;
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);

  ProjektDatatyp(AData.P^).DescriptionLen := Len;

  For A := 0 to 512 do
    ProjektDatatyp(AData.P^).DescriptionRec[A] := Ar[A];

{$ELSE}

  AData.DescriptionLen := Len;

  For A := 0 to 512 do
    AData.DescriptionRec[A] := Ar[A];

{$ENDIF}
End;

Procedure TProjektData.GetDescription(Var Rec; Var Len: Word);
Var Ar: Array[0..512] of Char Absolute Rec;
    A : Integer;
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);

  Len := ProjektDatatyp(AData.P^).DescriptionLen;

  For A := 0 to 512 do
    Ar[A] := ProjektDatatyp(AData.P^).DescriptionRec[A];

{$ELSE}

  Len := AData.DescriptionLen;

  For A := 0 to 512 do
    Ar[A] := AData.DescriptionRec[A];


{$ENDIF}

End;


Procedure TProjektData.WriteApp;

Var 

    P,
    D,
    E: String;

Begin
  FSplit(GetKey, P, D, E);

  Assign(F, GetKey);

  ReWrite(F);

  WriteCode;

  Close(F);
End;





(***********************)
(* Liste mit Projekten *)
(***********************)

Constructor TProjektCollection.Init(AInit, ADelta: Integer);
Begin
  TSortedCollection.Init(AInit, ADelta)
End;

Destructor TProjektCollection.Done;
Begin
  TSortedCollection.Done
End;


Function TProjektCollection.GetKeyCollection: PCollection;
Var A: Integer;
    PS: PString;
Begin
  GetKeyCollection := New(PCollection, Init(Count, 1));

  For A := 0 to Count - 1 do
  Begin
    PS := NewStr(PProjektData(At(A))^.GetKey);
    GetKeyCollection^.AtInsert(A, PS)
  End
End;

Procedure TProjektCollection.DeleteKeyCollection(Var P: PCollection);
Var A: Integer;
Begin
  For A := 0 to Count - 1 do
  Begin
    DisposeStr(PString(P^.At(A)))
  End;
  P^.DeleteAll;
  Dispose(P, Done);
End;


Function TProjektCollection.Compare(Key1, Key2: Pointer): Integer;
Begin
  If PProjektData(Key1)^.GetKey <
  PProjektData(Key2)^.GetKey then Compare := -1;

  If PProjektData(Key1)^.GetKey >
  PProjektData(Key2)^.GetKey then Compare := 1;

  If PProjektData(Key1)^.GetKey =
  PProjektData(Key2)^.GetKey then Compare := 0;

End;


(************************)
(* Listbox fÅr Projekte *)
(************************)

Constructor TProjektListBox.Init(var Bounds: TRect;
                           ANumCols: Word;
                         AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar)
End;

Function TProjektListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := PProjektData(Item)^.GetKey;
End;


(*********************************)
(* Die gesamte Projektverwaltung *)
(*********************************)


Constructor TProjekt.Init(Dir: String);
Begin
  WorkDir := Dir;
  ASelectedProjekt := - 1;
  AProjektCollection := New(PProjektCollection, Init(1, 1));

End;

Function TProjekt.GetProjekt: Pointer;
Begin
  GetProjekt := AProjektCollection^.At(ASelectedProjekt);
End;

Constructor TProjekt.Load(Var S:TStream);
Begin
  S.Read(ASelectedProjekt, SizeOf(ASelectedProjekt));
  S.Read(WorkDir, SizeOf(WorkDir));
  AProjektCollection := New(PProjektCollection, Load(S));

  AProjektData := GetProjekt;
End;

Procedure TProjekt.Store(Var S: TStream);
Begin
  S.Write(ASelectedProjekt, SizeOf(ASelectedProjekt));
  S.Write(WorkDir, SizeOf(WorkDir));
  AProjektCollection^.Store(S)
End;

Destructor TProjekt.Done;
Begin
  If ASelectedProjekt <> - 1 then
  Begin
    MyStream.Init(ProjektFileName, stCreate);
    MyStream.Put(AProjekt);
    MyStream.Done;
  End;
  TObject.Done
End;

Procedure TProjekt.Laden;
Begin
  Wait('Laden');
End;

Procedure TProjekt.Speichern;
Begin
  MyStream.Init(ProjektFileName, stCreate);
  MyStream.Put(AProjekt);
  MyStream.Done
End;

Procedure TProjekt.Definieren;
Var Dialog  : PFileDialog;
    FileName: String;
    Data    : ProjektDatatyp;
    Item    : Integer;
Begin
  Dialog := PFileDialog(New(PFileDialog,
                        Init('*.pas',
                             'Mainfile fÅr Projekt ?',
                             '~N~ame',
                             fdCloseButton, 100)));



  If Dialog <> Nil Then
  Begin
    If Desktop^.ExecView(Dialog) <> cmCancel Then
    Begin
      Dialog^.GetFileName(FileName);



      Data.MainFile := FileName;
      Data.DescriptionLen := 0;

      AProjektData := New(PProjektData, Init(Data));

      If Not AProjektCollection^.Search(AProjektData, Item) then
      Begin
        ASelectedProjekt := Item;
        AProjektCollection^.AtInsert(Item, AProjektData)
      End
      Else Dispose(AProjektData, Done)

    End;
    Dispose(Dialog, Done)
  End;

End;

(* ---------------------------------------------------------------- *)
(*                           tProtectedTStream                      *)
(* ---------------------------------------------------------------- *)
PROCEDURE tProtectedTStream.Error (Code, Info: INTEGER);
BEGIN
  RunError (255);
END;



PROCEDURE TProjekt.Selektieren;
  TYPE
    ProjektSelektierenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  VAR  
    Dialog: pDialog;  
    Data:   ProjektSelektierenData;
    Code:   INTEGER;
    R:      tRect;
    RezName: String;

    PS     : PString;
    A      : Integer;

    M1,
    M2,
    M3,
    M4     : Longint;


BEGIN

  R.Assign (0, 0, 80, 23);
  Dialog := New (pProjektSelektierenDialog, Init (R, 'Projekt Selektieren'));


    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (ProjektSelektierenData), 0);


  Data.ListPtr0 := AProjektCollection^.GetKeyCollection;

  Data.Focused0 := ASelectedProjekt;
  OldFocused := ASelectedProjekt;

  If AProjektCollection^.Count <> 0 then
    PProjektData(AProjektCollection^.At(ASelectedProjekt))^.
      GetDescription(Data.TextRec0, Data.TextLen0);


  Dialog^.SetData (Data); 
  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN

    Dialog^.GetData (Data);

    ASelectedProjekt := Data.Focused0;
    AProjektData := AProjektCollection^.At(ASelectedProjekt);

    If AProjektCollection^.Count <> 0 then
      PProjektData(AProjektCollection^.At(ASelectedProjekt))^.
        SetDescription(Data.TextRec0, Data.TextLen0);


  END
  Else
  Begin
    Dialog^.GetData (Data);

    If AProjektCollection^.Count <> 0 then
      PProjektData(AProjektCollection^.At(Data.Focused0))^.
        SetDescription(Data.TextRec0, Data.TextLen0)
  End;
  IF Dialog <> NIL THEN
    Dispose (Dialog, Done);

  AProjektCollection^.DeleteKeyCollection(Data.ListPtr0);
END;




Procedure TProjekt.SetProjektModus;
Begin
  Wait('SetProjektModus');
End;

Procedure TProjekt.ScannProjektforCommands;
Var Data: ProjektDatatyp;
Begin
  If AProjektCollection^.Count = 0 then
  Begin
    Wait('Kein Projekt ausgewÑhlt.');
    Exit
  End;

  PProjektData(AProjektCollection^.
               At(ASelectedProjekt))^.GetData(Data);

  PHelpBuild(PProjektData(AProjektCollection^.
               At(ASelectedProjekt))^.
               GetHelpVerw)^.
               GetCommandsfromMain(Data)
               
End;


Procedure TProjekt.EditMenu;
Begin
End;

Procedure TProjekt.EditiereSubroutinen;
Begin
  If AProjektData <> Nil then AProjektData^.EditRoutinen;
End;




Procedure TProjekt.EditiereUnits;
Begin
  If AProjektData <> Nil then AProjektData^.UnitsinProjektDialog;
End;

Function TProjekt.EditAppQuelle(Var S: String): Boolean;
Begin
  EditAppQuelle := False;
  If AProjektData <> Nil then
  Begin
    EditAppQuelle := True;
    S := AProjektData^.GetKey
  End
End;

Procedure TProjekt.EditMain;
Begin
  If AProjektData <> Nil then AProjektData^.EditMain;
End;

Procedure TProjekt.VariablenEditieren;
Begin
  If AProjektData <> Nil then AProjektData^.EditVariablen;
End;

Procedure TProjekt.ConstantenEditieren;
Begin
  If AProjektData <> Nil then AProjektData^.EditConstanten;
End;


Procedure TProjekt.WriteAppSources;
Begin
  If AProjektData <> Nil then AProjektData^.WriteApp
End;

Procedure TProjekt.ReadAppSources;
Var Dialog  : PFileDialog;
    FileName: String;
    Data    : ProjektDatatyp;
    Item    : Integer;
Begin
  ErrorData := New(PErrorDataList, Init);

  Dialog := PFileDialog(New(PFileDialog,
                        Init('*.pas',
                             'Neues Projekt (Einlesen) ?',
                             '~N~ame',
                             fdCloseButton, 100)));



  If Dialog <> Nil Then
  Begin
    If Desktop^.ExecView(Dialog) <> cmCancel Then
    Begin
      Dialog^.GetFileName(FileName);



      Data.MainFile := FileName;
      Data.DescriptionLen := 0;

      AProjektData := New(PProjektData, Init(Data));

      If Not AProjektCollection^.Search(AProjektData, Item) then
      Begin
        ASelectedProjekt := Item;
        AProjektCollection^.AtInsert(Item, AProjektData)
      End
      Else Dispose(AProjektData, Done)

    End
    Else
    Begin
      Dispose(Dialog, Done);
      Exit
    End;
    Dispose(Dialog, Done);
  End;


  If AProjektData <> Nil then
  Begin
    If Not AProjektData^.ReadApp then
    Begin
    End;
  End;
End;

Procedure TProjekt.CompileApp;
Begin
  If AProjektData <> Nil then AProjektData^.CompileApp;
End;

Procedure TProjekt.RunApp;
Begin
  If AProjektData <> Nil then AProjektData^.RunApp
End;


Procedure TProjekt.MakeAndRun;
Begin
  If AProjektData <> Nil then
  Begin
    AProjektData^.WriteApp;
    AProjektData^.CompileApp;
    AProjektData^.RunApp
  End;
End;






Procedure TProjekt.HilfeKontextEditierenDialog;
Begin
  If AProjektCollection^.Count = 0 then
  Begin
    Wait('Kein Projekt ausgewÑhlt.');
    Exit
  End;

  PHelpBuild(PProjektData(AProjektCollection^.
               At(ASelectedProjekt))^.
               GetHelpVerw)^.
               HilfeKontextEditierenDialog(
               PProjektData(AProjektCollection^.
               At(ASelectedProjekt))^.GetKey)
End;

Procedure TProjekt.ClassenEditieren;
Begin
  KlassenEditorDialog;
End;

Procedure TProjekt.ChWorkDir;
Begin
  GetDir(0, OldDir);
  ChDir(WorkDir);
  ChangeDir;
  GetDir(0, WorkDir);

  If WorkDir[Length(WorkDir)] = '\' Then
    System.Delete(WorkDir, Length(WorkDir), 1);

  ChDir(OldDir);
End;








CONSTRUCTOR tProjektSelektierenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tDialog.Init (Bounds, aTitle);

  R.Assign (76, 2, 77, 9);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0);
  Insert(View);

  R.Assign (3, 2, 76, 9);
  View := New (pMyListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (3, 1, 17, 2);
  Insert (New (pLabel, Init (R, '~M~ain - File', View)));
 
  R.Assign (76, 11, 77, 18);
  View := New (pScrollBar, Init  (R)); 
  Insert (View); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (3, 11, 76, 18);
  View := New (pMyMemo, Init (R, NIL, pScrollBar (View), NIL, 512));
  Insert (View);
  R.Assign (3, 10, 18, 11);
  Insert (New (pLabel, Init (R, '~B~eschreibung', View)));
 
  R.Assign (24, 19, 32, 21);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (41, 19, 54, 21);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);
END;

Procedure tProjektSelektierenDialog.HandleEvent(Var Event: TEvent);
Var ProjektData: ProjektDatatyp;
    AMemo      : PMyMemo;

    TheMemo    : Record
                   A: Word;
                   B: Array[0..512] of Char;
                 End;
    A          : Integer;
    P          : PMyListBox;
Begin
  TDialog.HandleEvent(Event);


  If (Event.What = evBroadCast) And
     (Event.Command = cmItemFocused) then
  Begin
    P := Event.InfoPtr;
    If TypeOf(P^) = TypeOf(TMyListBox) then
    Begin
      AMemo := Message(Owner, evBroadCast, cmWhoIsMemo, Nil);

      If (AMemo <> Nil) and (TypeOf(AMemo^) = TypeOf(TMyMemo)) then
      Begin
        AMemo^.GetData(TheMemo);

        PProjektData(AProjekt^.AProjektCollection^.At(
                     AProjekt^.OldFocused))^.
          SetDescription(TheMemo.B,
                         TheMemo.A);


        PProjektData(AProjekt^.AProjektCollection^.At(P^.Focused))^.
          GetDescription(ProjektData.DescriptionRec,
                         ProjektData.DescriptionLen);

        TheMemo.A := ProjektData.DescriptionLen;

        For A := 0 to 512 do
          TheMemo.B[A] := ProjektData.DescriptionRec[A];


        AMemo^.SetData(TheMemo);
        AMemo^.Draw;

        AProjekt^.OldFocused := P^.Focused;

      End;

      ClearEvent(Event)
    End
  End;



End;



Procedure RegisterUnit;
Begin
  RegisterType(RProjekt);
  RegisterType(RProjektData);
  RegisterType(RProjektCollection);
End;




Procedure InitKeyWords;
Begin
  AHoleKeyWordList := New(PHoleKeyWords, Init);

  AHoleKeyWordList^.InsertKey('Program',   Bez);
  AHoleKeyWordList^.InsertKey('Procedure', Bez);
  AHoleKeyWordList^.InsertKey('Function',  Bez);
  AHoleKeyWordList^.InsertKey('Var',       Bez);
  AHoleKeyWordList^.InsertKey('Object',    Bez);
  AHoleKeyWordList^.InsertKey('Type',      Bez);
  AHoleKeyWordList^.InsertKey('Record',    Bez);
  AHoleKeyWordList^.InsertKey('Begin',     Bez);
  AHoleKeyWordList^.InsertKey('End',       Bez);
  AHoleKeyWordList^.InsertKey('Uses',      Bez);
  AHoleKeyWordList^.InsertKey('Const',     Bez);

  AHoleKeyWordList^.InsertKey('Integer',   Bez);
  AHoleKeyWordList^.InsertKey('Boolean',   Bez);
  AHoleKeyWordList^.InsertKey('Word',      Bez);
  AHoleKeyWordList^.InsertKey('Pointer',   Bez);
  AHoleKeyWordList^.InsertKey('String',    Bez);
  AHoleKeyWordList^.InsertKey('Char',      Bez);
  AHoleKeyWordList^.InsertKey('Longint',   Bez);
  AHoleKeyWordList^.InsertKey('Byte',      Bez);
  AHoleKeyWordList^.InsertKey('Shortint',  Bez);
End;




Begin



  RegisterHelpBuild;


  RegisterUnit
End.

