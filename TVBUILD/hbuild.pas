Constructor THelpBuild.Init(F: String);
Begin
  TObject.Init;

  FileName := F;

  AHelpCommandCollection := New(PHelpCommandCollection, Init(1, 1))
End;

Constructor THelpBuild.Load(Var S: TStream);
Begin
  S.Read(FileName, SizeOf(FileName));
  AHelpCommandCollection := New(PHelpCommandCollection, Load(S));
End;

Procedure THelpBuild.Store(Var S: TStream);
Begin
  S.Write(FileName, SizeOf(FileName));
  AHelpCommandCollection^.Store(S);
End;

Destructor THelpBuild.Done;
Begin
  TObject.Done
End;


Function  THelpBuild.FindNewCommandsFromMain: Boolean;
Begin
End;

Function  THelpBuild.AreCommandsDifferent: ShortInt;
                                 (* 1  = Neue Daten in Main   *)
                                 (* 0  = Kein Unterschied     *)
                                 (* -1 = Daten in Projekt neu ???? *)
Begin
End;

Procedure THelpBuild.GetCommandsFromMain(ASelectedProjekt: ProjektDatatyp); (* ---> Einlesen von Programm *)

Var F: Text;

Function OpenFile(Name: String): Boolean;
Begin
  {$I-}

    Assign(F, Name);
    Reset(F);
    OpenFile := IOResult = 0

  {$I+}
End;

Var Zeile: String;

Begin
  If Not OpenFile(ASelectedProjekt.MainFile) then
  Begin
    Wait('Main - File konnte nicht geîffnet werden');
    Exit
  End;

  While Not Eof(F) do
  Begin
    Readln(F, Zeile);
  End;
End;

Procedure THelpBuild.SetCommandsToMain;   (* <--- Ins Programm zurÅck.. *)
Begin
End;



Procedure THelpBuild.LoadCommandsFromProjekt; (* Laden der Daten *)
Begin
End;
Procedure THelpBuild.SaveCommandsToProjekt;   (* Speichern der " *)
Begin
End;


(***************)
(* HelpCommand *)
(***************)

Constructor THelpCommand.Init;
Begin
  TObject.Init
End;

Constructor THelpCommand.Load(Var S: TStream);
Begin
{$IFDEF XHEAP}


  If MemAvailX(SizeOf(THelpCommandData)) Then
  Begin
    GetMemX(AData, SizeOf(THelpCommandData));
    S.Read(THelpCommandDatatyp(AData.P^), SizeOf(THelpCommandData))
  End
  Else
  Begin
    Wait('Kein Speicher mehr da (XHeap)!');
    Halt
  End;

{$ELSE}

  S.Read(AData, SizeOf(THelpCommandData))

{$ENDIF}
End;

Procedure THelpCommand.Store(Var S: TStream);
Begin
{$IFDEF XHEAP}

  BlendeBlockEin(AData.BlockNr);
  S.Write(AData.P^, SizeOf(THelpCommandData))

{$ELSE}

  S.Write(AData, SizeOf(THelpCommandData))

{$ENDIF}
End;

Destructor THelpCommand.Done;
Begin
  TObject.Done
End;

Function THelpCommand.GetCommand: String;
Begin
  GetCommand := 'Test'
End;


(*************************)
(* HelpCommandCollection *)
(*************************)

Constructor THelpCommandCollection.Init(ALimit, ADelta: Integer);
Begin
  TSortedCollection.Init(ALimit, ADelta)
End;

Destructor THelpCommandCollection.Done;
Begin
  FreeAll;
  TSortedCollection.Done;
End;


Function THelpCommandCollection.KeyOf(Item: Pointer): Pointer;
Var S: String;
Begin
  S := PHelpCommand(Item)^.GetCommand;

  KeyOf := @S
End;

Function THelpCommandCollection.Compare(Key1, Key2: Pointer): Integer;
Var S1, S2: String;

Begin
  (* Diese Routine ist nicht korreckt geschrieben. In Verbindung *)
  (* mit der direckten Anwendung von Search ist alles wieder Ok. *)


  S1 := UpDate(String(Key1^));
  S2 := UpDate(PHelpCommand(Key2)^.GetCommand);


  If S1 < S2 then
    Compare := - 1;
  If S1 = S2 then
    Compare := 0;
  If S1 > S2 then
    Compare := 1;
End;



(******************)
(* CommandListBox *)
(******************)

Constructor TCommandListBox.Init(var Bounds: TRect;
                                   ANumCols: Word;
                                 AScrollBar: PScrollBar);
Begin
  TListBox.Init(Bounds, ANumCols, AScrollBar)
End;


Destructor TCommandListBox.Done;
Begin
  TListBox.Done
End;
Function TCommandListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := PHelpCommand(List^.At(Item))^.GetCommand
End;



(****************************)
(* Dialog fÅr Hilfe - Daten *)
(****************************)


CONSTRUCTOR tHilfeKontextEditierenDialog.Init (VAR Bounds: tRect;
                                                   aTitle: tTitleStr;
                                                   aFile : String);
  VAR
    R: tRect;
    View: pView;
BEGIN
  aFile := 'Projekt: ' + aFile;

  If Length(aFile) Mod 2 = 0 then System.Insert(' ', aFile, Pos(AFile, ' '));

  tDialog.Init (Bounds, aTitle);


  GetExtent(R);

  R.Assign ((R.B.X - R.A.X) Div 2 -
            (Length(aFile) + 1) Div 2,
            2,
            (R.B.X - R.A.X) Div 2 +
            (Length(aFile) + 1) Div 2,
            3);



  Insert (New (pStaticText, Init (R, aFile)));

  R.Assign (45, 5, 46, 12);
  View := New (pScrollBar, Init  (R));
  Insert(View);
  pScrollBar (View)^.SetRange (0, 0);
  R.Assign (4, 5, 45, 12);
  View := New (pCommandListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (4, 4, 14, 5);
  Insert (New (pLabel, Init (R, '~B~efehle', View)));
 
  R.Assign (49, 5, 76, 7);
  Insert (New (pButton, Init (R, '~L~eere Hilfe Fenster ?', cmHilfeEditLeereFenster, 0)));

  R.Assign (49, 8, 76, 10);
  Insert (New (pButton, Init (R, ' ~E~infÅgen in Projekt ', cmHilfeEditEinfuegen, 0)));

  R.Assign (49, 11, 57, 13);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (63, 11, 76, 13);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  R.Assign (75, 14, 76, 21);
  View := New (pScrollBar, Init  (R)); 
  Insert (View); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (4, 14, 75, 21);
  View := New (pMemo, Init (R, NIL, pScrollBar (View), NIL, 512));
  Insert (View);
  R.Assign (4, 13, 19, 14);
  Insert (New (pLabel, Init (R, '~H~ilfe - Text', View)));
 
  SelectNext (FALSE);
END;


Procedure tHilfeKontextEditierenDialog.HandleEvent(Var Event: TEvent);
Begin
  TDialog.HandleEvent(Event);
End;


 
PROCEDURE THelpBuild.HilfeKontextEditierenDialog(F: String);
  TYPE
    HilfeKontextEditierenData = RECORD
      ListPtr0: pHelpCommandCollection;
      Focused0: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..1024*4] OF CHAR;
    END;


  VAR
    R: tRect;
    Data: HilfeKontextEditierenData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pHilfeKontextEditierenDialog, Init (R, 'Hilfe - Kontext Editieren', F));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (HilfeKontextEditierenData), 0);

  Data.ListPtr0 := AHelpCommandCollection;


  Dialog^.SetData (Data);

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN

    Dialog^.GetData (Data);



  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);


END;


Procedure RegisterHelpBuild;
Begin
  RegisterType(RHelpVerw);
  RegisterType(RHelpCommand);
End;



