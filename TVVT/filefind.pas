Unit FileFind;
{$X+ $O+ $F+ $S-}
Interface

Uses Crt,
     BDF,
     TvdvCmds,
     Drivers,
     Memory,
     Objects,
     App,
(*     Editors,*)
     Views,
	 TvvtData,
     Dialogs(*,     StrTools*);



Type
  PDateiDataCollection = ^TDateiDataCollection;
  TDateiDataCollection = Object(TCollection)
    Function  GetItem (Var S: TStream): Pointer; Virtual;
    Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
    Procedure FreeItem(Item : Pointer); Virtual;
    Procedure InsertDateiData(Item: DateiDataTyp);
  End;



  SuchmaskenVoreinstellungData = RECORD
    TextLen0: WORD;
    TextRec0: ARRAY [0..12] OF CHAR;
  END;

  LaengenTyp = Record
                  Lo,
                  Hi : Byte
                END;
  String70   = String[70];
  PString12        = ^String12;
  PString70        = ^String70;

  PLabelCollection = ^TLabelCollection;
  TLabelCollection = Object(TCollection)
    Function  GetItem (Var S: TStream): Pointer; Virtual;
    Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
    Procedure FreeItem(Item : Pointer); Virtual;
    Procedure InsertLabel(Item: String12); Virtual;
  End;

  PPfadCollection = ^TPfadCollection;
  TPfadCollection = Object(TCollection)
    Function  GetItem (Var S: TStream): Pointer; Virtual;
    Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
    Procedure FreeItem(Item : Pointer); Virtual;
    Procedure InsertPfad(Item: String); Virtual;
  End;

  PLabelListBox   = ^TLabelListBox;
  TLabelListBox   = Object(TListBox)
    LastFocused   : Integer;
    B             : TDrawBuffer;
    C             : Byte;
    S             : String12;
    Procedure SelectItem(Item: Integer); Virtual;
    Procedure FocusItem(Item: Integer); Virtual;
    Function  GetText (Item: Integer; MaxLen: Integer): String; Virtual;
  End;

  PPfadListBox    = ^TPfadListBox;
  TPfadListBox    = Object(TListBox)
    LastFocused   : Integer;
    B             : TDrawBuffer;
    C             : Byte;
    S             : String;
    Procedure SelectItem(Item: Integer); Virtual;
    Procedure FocusItem(Item: Integer); Virtual;
    Function  GetText (Item: Integer; MaxLen: Integer): String; Virtual;
  End;

  pGetDatenDialog = ^tGetDatenDialog;
  tGetDatenDialog = OBJECT (tDialog)
    ALabelCollection : PLabelCollection;
    APfadCollection  : PPfadCollection;
    ALabelListBox    : PLabelListBox;
    APfadListBox     : PPfadListBox;
    AScrollBar       : PScrollBar;
    Procedure InitData(Item: Pointer);
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Destructor  Done; Virtual;
    Procedure HandleEvent(Var Event: TEvent); Virtual;
  END;



  PDateiDataListBox    = ^TDateiDataListBox;
  TDateiDataListBox    = Object(TListBox)
    LastFocused            : Integer;
    B                      : TDrawBuffer;
    C                      : Byte;
    S                      : String12;
    MemFail                : Boolean;
    Anzahl,
    Geladen                : Integer;
    Stelle                 : Longint;
    DateiData              : DateiDatatyp;
    ListMaske              : String12;

    Function  GetText   (Item: Integer; MaxLen: Integer): String; Virtual;
    Function  GetMaske: String12;
    Procedure PutMaske(Str: String12);
    Procedure GetFiles(Var Loc: Longint; Var Loadet: Integer);
  end;





  PFileFindWindow      = ^TFileFindWindow;
  TFileFindWindow      = Object(TDialog)
    Laenge             : LaengenTyp;
    ADateiDataListBox  : PDateiDataListBox;
    AScrollBar         : PScrollBar;
    AMemo              : PInputLine;
    ACollection        : PDateiDataCollection;
    Data               : SuchmaskenVoreinstellungData;


    Gefunden,
    Min,
    Max                : Longint;

    Constructor Init(Var SM: String);
    Destructor  Done; Virtual;
    Function  GetMaskBack: String12; Virtual;
    Procedure HandleEvent(Var Event: TEvent); Virtual;
    Procedure PutMaskeToMemo(SM: String12);
    Function  GetPalette: PPalette; Virtual;
  End;


  PDateiDataTyp        = ^DateiDataTyp;

Const FileFindWindow: PFileFindWindow = Nil;
      GetDaten: PGetDatenDialog = Nil;
Implementation


Procedure TGetDatenDialog.InitData(Item: Pointer);
Var DateiData : DateiDataTyp;
    LabelData : LabelDataTyp;
    PfadData  : PfadDataTyp;
    ZusatzData: ZusatzDataTyp;
    Z,
    Z1,
    Z2: String;
Begin

  DateiData := PDateiDataTyp(Item)^;



  For A := 1 to DateiData.Anzahl do
  BEGIN
    if (Filesize(ZusatzDataFile) < Longint(DateiData.Loc) - 1) or
       (Longint(DateiData.Loc) < 1) then
    Begin
      Str(FileSize(ZusatzDataFile), Z);
      Str(Longint(DateiData.Loc) - 1, Z1);

      Wait('Index auf Zusatz - Datei zu gro·!' + Chr(13) +
           'Zusatzelemente: ' + Z + Chr(13) +
           'DateiData.Loc : ' + Z1);
      Halt;
    End;
    seek(ZusatzDataFile, Longint(DateiData.Loc)-2+A);
    Read(ZusatzDataFile, ZusatzData);

    If (Longint(ZusatzData.Sl) > Filesize(StrucktDataFile)) or
       (Longint(ZusatzData.Sl) < 1) then
    Begin
      Str(Longint(ZusatzData.Sl) - 1, Z);
      Wait('Index auf Strucktdatei zu gro·!' + Chr(13) +
           'Sl: ' + Z);
      Halt
    End;
    Seek(StrucktDataFile, Longint(ZusatzData.Sl)-1);
    Read(StrucktDataFile, StrucktData);
    Seek(LabelDataFile, StrucktData.DLabelList - 1);
    Read(LabelDataFile, LabelData);

    If (Longint(ZusatzData.Sp) > Filesize(StrucktDataFile)) or
       (Longint(ZusatzData.Sp) < 1) then
    Begin
      Str(Longint(ZusatzData.Sp) - 1, Z);
      Wait('Index auf Strucktdatei zu gro·!' + Chr(13) +
           'Sp: ' + Z);
      Halt
    End;

    Seek(StrucktDataFile, Longint(ZusatzData.Sp)-1);
    Read(StrucktDataFile, StrucktData);
    Seek(PfadDataFile, StrucktData.DPfadList - 1);
    Read(PfadDataFile, PfadData);

    PLabelCollection(ALabelListBox^.List)^.InsertLabel(LabelData.LabelName);
    ALabelListBox^.SetRange(ALabelListBox^.List^.Count);

    PPfadCollection(APfadListBox^.List)^.InsertPfad(PfadData.PfadName);
    APfadListBox^.SetRange(APfadListBox^.List^.Count)


  end;
  ALabelListBox^.Options := ALabelListBox^.Options or ofSelectable;
  ALabelListBox^.FocusItem(0);
  ALabelListBox^.TopItem := 0;
  APfadListBox^.Options := APfadListBox^.Options or ofSelectable;
  APfadListBox^.FocusItem(0);

  ALabelListBox^.Select;


End;

Const DVZaehler: Integer = 0;
      DZaehler : Integer = 0;
Procedure PruefeDateiDaten(LabelName, PfadName, DateiName: String);
Var StrucktPos,
    LabelPos,
    PfadPos,
    DateiPos  : Longint;
    SD           : StrucktDatatyp;
    DateiData    : DateiDatatyp;
    PfadData     : PfadDatatyp;
    LabelData    : LabelDatatyp;
    DName        : String12;
    DateiVorh    : Boolean;
Begin
  DateiVorh := False;
  (***************************************)
  (* Suche Pfad in IMP.TMP und dann die  *)
  (* Datei in dessen Bereich:            *)
  (***************************************)


  Assign(ImportFile, 'IMP.TMP');
  {$I-}
  Reset(ImportFile);

  If IOResult <> 0 then
  Begin
    Wait('TemponÑr - Datei nicht vorhanden!');
    Exit
  End;
  {$I+}

  If PfadName[Length(PfadName)] = '\' then System.Delete(PfadName, Length(PfadName), 1);

  Seek(ImportFile, 0);
  While Not Eof(ImportFile) do
  Begin
    Read(ImportFile, ImportData);

    ImportData := Copy(ImportData, 17, Length(ImportData) - 16);

    If PfadName = ImportData then
    Begin
      While Not Eof(ImportFile) do
      Begin
        Read(ImportFile, ImportData);
        DName := '';

        If Pos('Verzeichnis', ImportData) <> 0 then
          Seek(ImportFile, FileSize(ImportFile));

        If (Pos('Byte', ImportData) = 0) and
           (ImportData <> '') then
          If Pos('<DIR>', ImportData) = 0 then
          Begin
            DName := ImportData;

            While DName[Length(DName)] = ' ' do System.Delete(DName, Length(DName), 1);
            If Length(DName) > 8 then DName[9] := '.';
            While Pos(' ', DName) <> 0 do System.Delete(DName, Pos(' ', DName), 1);

            If Pos(DateiName, DName) <> 0 then
            Begin
              DateiVorh := True;
              Seek(ImportFile, FileSize(ImportFile))
            End
          End
      End;
      Seek(ImportFile, FileSize(ImportFile))
    End
  End;
  Close(ImportFile);
  If Not DateiVorh then Wait('Fehler in der Datenstrucktur!')
End;


Procedure TGetDatenDialog.HandleEvent(Var Event: TEvent);
Begin
  If (Event.What = evCommand) and
     (Event.Command = cmDateiPruefen) then
  Begin
    PruefeDateiDaten(GetDaten^.ALabelListBox^.
                     GetText(GetDaten^.ALabelListBox^.Focused,
                     GetDaten^.ALabelListBox^.Size.X),

                     GetDaten^.APfadListBox^.
                     GetText(GetDaten^.ALabelListBox^.Focused,
                     GetDaten^.APfadListBox^.Size.X),

                     FileFindWindow^.ADateiDataListBox^.GetText(
                     FileFindWindow^.ADateiDataListBox^.Focused, 0));

    ClearEvent(Event)
  End;
  If Event.What = evBroadCast then
    If Event.Command = cmLabelFocused then
    Begin
      GetDaten^.APfadListBox^.
      FocusItem(GetDaten^.ALabelListBox^.Focused);
      ReDraw;
      ClearEvent(Event)
    End
    Else
  Else
    TDialog.HandleEvent(Event);
  GetDaten^.ALabelListBox^.HandleEvent(Event)
End;

Destructor TGetDatenDialog.Done;
Begin
  Dispose(ALabelCollection, Done);
  Dispose(APfadCollection, Done);
  Dispose(ALabelListBox, Done);
  Dispose(APfadListBox, Done);
  Dispose(AScrollBar, Done);
  TDialog.Done
End;


CONSTRUCTOR tGetDatenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R,
    S    : tRect;

    Datei: String;
BEGIN
  tDialog.Init (Bounds, aTitle);

  GetDaten := @Self;

  S.Assign (18, 3, 19, 7);
  AScrollBar := New (pScrollBar, Init  (S));

  R.Assign (6, 3, 18, 7);
  ALabelListBox := New (pLabelListBox, Init (R, 1, AScrollBar));

  R.Assign (5, 2, 10, 3);
  Insert (New (pLabel, Init (R, 'Disk:', ALabelListBox)));

  R.Assign (6, 10, 66, 11);
  APfadListBox := New (PPfadListBox, Init (R, 1, Nil));

  R.Assign (5, 9, 10, 10);
  Insert (New (pLabel, Init (R, 'Pfad:', APfadListBox)));

  Datei := FileFindWindow^.ADateiDataListBox^.GetText(
           FileFindWindow^.ADateiDataListBox^.Focused, 0);

  Datei := 'Datei: ' + Datei;

  R.Assign (30, 3, 49, 4);
  Insert (New (pStaticText, Init (R, Datei)));

  R.Assign (20, 13, 30, 15);
  Insert (New (pButton, Init (R, '~E~nde', cmCancel, 0)));

  R.Assign (40, 13, 50, 15);
  Insert (New (pButton, Init (R, '~P~rÅfen', cmDateiPruefen, 0)));


  ALabelCollection := New(PLabelCollection, Init(1, 1));

  ALabelListBox^.LastFocused := - 1;

  ALabelListBox^.Options := ALabelListBox^.Options And Not ofSelectable;

  ALabelListBox^.NewList(ALabelCollection);

  

  APfadCollection := New(PPfadCollection, Init(1, 1));

  APfadListBox^.LastFocused := - 1;

  APfadListBox^.Options := APfadListBox^.Options And Not ofSelectable;

  APfadListBox^.NewList(APfadCollection);

  Insert(ALabelListBox);
  Insert(APfadListBox);
  Insert(AScrollBar);



  SelectNext (False);

  InitData(FileFindWindow^.
           ACollection^.At(FileFindWindow^.
           ADateiDataListBox^.Focused));


END;
 
PROCEDURE GetDatenDialog;
  VAR
    R: tRect;
    Code: INTEGER;
    D: ^TView;
BEGIN
  R.Assign (1, 0, 79, 17);
  D := New (pGetDatenDialog, Init (R, ''));

  IF Application^.ValidView (D) <> NIL THEN
  begin
	DeskTop^.ExecView (D);
    Dispose (D, Done);
  end;
END;


{ PDateiDataCollection }

Function TDateiDataCollection.GetItem(Var S: TStream): Pointer;
Var Item: PDateiDataTyp;
Begin
  Item := New(PDateiDataTyp);
  S.Read(Item^, SizeOf(DateiDataTyp));
  GetItem := Item
end;

Procedure TDateiDataCollection.PutItem(Var S: TStream; Item: Pointer);
Begin
  S.Write(PDateiDataTyp(Item)^, SizeOf(DateiDataTyp))
end;

Procedure TDateiDataCollection.FreeItem(Item: Pointer);
Begin
  Dispose(PDateiDataTyp(Item))
End;


Procedure TDateiDataCollection.InsertDateiData(Item: DateiDataTyp);
Var P : ^DateiDataTyp;
Begin
  P := MemAlloc(SizeOf(DateiDataTyp));
  If P = Nil then
  Begin
    Wait('Kein Speicherplatz mehr da!');
    Halt
  End;
  Move(Item, P^, SizeOf(DateiDataTyp));
  Insert(P)
end;


{ PLabelCollection }

Function  TLabelCollection.GetItem (Var S: TStream): Pointer;
Var Item: PString12;
Begin
  Item := New(PString12);
  S.Read(Item^, SizeOf(String12));
  GetItem := Item
End;
Procedure TLabelCollection.PutItem (Var S: TStream; Item: Pointer);
Begin
  S.Write(PString12(Item)^, SizeOf(String12))
End;
Procedure TLabelCollection.FreeItem(Item : Pointer);
Begin
  Dispose(PString12(Item))
End;
Procedure TLabelCollection.InsertLabel(Item: String12);
Var P: ^String12;
Begin
  P := MemAlloc(SizeOf(String12));
  If P = Nil then
  Begin
    Wait('Kein Speicherplatz mehr da!');
    Halt
  End;
  Move(Item, P^, SizeOf(String12));
  Insert(P)
End;


{ PPfadCollection }

Function  TPfadCollection.GetItem (Var S: TStream): Pointer;
Var Item: PString70;
Begin
  Item := New(PString70);
  S.Read(Item^, SizeOf(String70));
  GetItem := Item
End;
Procedure TPfadCollection.PutItem (Var S: TStream; Item: Pointer);
Begin
  S.Write(PString70(Item)^, SizeOf(String70))
End;
Procedure TPfadCollection.FreeItem(Item : Pointer);
Begin
  Dispose(PString70(Item))
End;
Procedure TPfadCollection.InsertPfad(Item: String);
Var P: ^String;
Begin
  P := MemAlloc(SizeOf(String70));
  If P = Nil then
    Halt;
  Move(Item, P^, SizeOf(String70));
  Insert(P)
End;


{ PLabelListBox }

Function  TLabelListBox.GetText (Item: Integer; MaxLen: Integer): String;
Begin
  GetText := String12(List^.At(Item)^)
End;

Procedure TLabelListBox.SelectItem(Item: Integer);
Begin
  TListBox.SelectItem(Item);
  Message(TopView, evCommand, cmSelectLabel, @Item)
End;

Procedure TLabelListBox.FocusItem (Item: Integer);
Begin
  If Item <> LastFocused then
  Begin
    TListBox.FocusItem(Item);
    LastFocused := Item;
    Message(Owner, evBroadCast, cmLabelFocused, @Self)
  End
End;


{ PPfadListBox }


Function  TPfadListBox.GetText (Item: Integer; MaxLen: Integer): String;
Begin
  GetText := String(List^.At(Item)^)
End;

Procedure TPfadListBox.SelectItem(Item: Integer);
Begin
  TListBox.SelectItem(Item);
End;

Procedure TPfadListBox.FocusItem (Item: Integer);
Begin
  If Item <> LastFocused then
  Begin
    TListBox.FocusItem(Item);
    LastFocused := Item;
  End
End;

{ TDateiDataListBox }

Function TDateiDataListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := DateiDataTyp(List^.At(Item)^).DateiName
end;

Function TDateiDataListBox.GetMaske: String12;
Begin
  GetMaske := ListMaske
End;

Procedure TDateiDataListBox.PutMaske(Str: String12);
Begin
  ListMaske := Str
End;

Procedure TDateiDataListBox.GetFiles(Var Loc: Longint; Var Loadet: Integer);
Begin
  Loadet := 0;

  Seek(DateiDataFile, Loc - 1);
  If Eof(DateiDataFile) then
    Wait('Dateizeiger am ende der Datei!');

  While (Loadet < (System.MemAvail - 17408) Div 24) And
        Not (Eof(DateiDataFile)) do
  Begin
    Read(DateiDataFile,DateiData);

    If Mehrfache = False then
      If PasstinMaske(ListMaske, DateiData.DateiName) then
      BEGIN
        Inc(Loadet);
        PDateiDataCollection(List)^.InsertDateiData(Dateidata);
        SetRange(List^.Count)
      END
      Else
    Else
      If PasstinMaske(ListMaske, DateiData.DateiName) And
                                  (DateiData.Anzahl > 1) then
      Begin
        Inc(Loadet);
        PDateiDataCollection(List)^.InsertDateiData(Dateidata);
        SetRange(List^.Count)
      End
  End;
  Loc := FilePos(DateiDataFile)
End;

{ TFileFindWindow }

Function TFileFindWindow.GetPalette: PPalette;
Const TempPal: String[Length(CDialog)] = CDialog;
Begin
  GetPalette := @TempPal
End;

Procedure TFileFindWindow.PutMaskeToMemo(SM: String12);
Begin
  DisposeStr(AMemo^.Data);
  AMemo^.Data := NewStr(SM)
End;


Constructor TFileFindWindow.Init(Var SM: String);

Var R, S        : TRect;
    Event       : TEvent;
Begin

  DisableCommands([cmDateiBack]);
  DisableCommands([cmDateiNext]);

  R.Assign(0, 1, 35, 14);
  TDialog.Init(R, 'Dateisuche');

  FileFindWindow := @Self;

  R.Assign (20, 2, 32, 4);
  Insert (New (pButton, Init (R, '~W~eiter', cmDateiNext, 0)));

  R.Assign (20, 4, 32, 6);
  Insert (New (pButton, Init (R, '~Z~urÅck', cmDateiBack, 0)));

  R.Assign (20, 6, 32, 8);
  Insert (New (pButton, Init (R, '~A~nfang', cmDateiAnfang, 0)));

  R.Assign (20, 10, 32, 12);
  Insert (New (pButton, Init (R, ' ~E~nde ', cmCancel, 0)));

  R.Assign(2, 2, 16, 8);
  S.Assign(16, 2, 17, 8);
  AScrollBar := New(PScrollBar, Init(S));

  ADateiDataListBox := New(PDateiDataListBox, Init(R, 1, AScrollBar));
  R.Assign(2, 1, 15, 2);
  Insert (New(PLabel, Init(R, '~D~ateien', ADateiDataListBox)));

  ADateiDataListBox^.Options := ADateiDataListBox^.Options And Not ofSelectable;
  ADateiDataListBox^.MemFail := False;

  R.Assign (2, 10, 17, 11);
  AMemo := New (PInputLine, Init (R, 12));

  ADateiDataListBox^.PutMaske(SM);

  PutMaskeToMemo(SM);

  R.Assign (1, 9, 8, 10);
  Insert (New (pLabel, Init (R, '~M~aske:', AMemo)));

  ACollection := New(PDateiDataCollection, Init(5, 5));

  Insert(AMemo);

  ADateiDataListBox^.LastFocused := - 1;

  ADateiDataListBox^.NewList(ACollection);

  Insert(ADateiDataListBox);

  Insert(AScrollBar);

  If Not TestFiles then
  Begin
    Wait('Dateivoreinstellung fehlerhaft!' + Chr(13) +
         'Eine oder mehrere Dateien nicht vorhanden.');
    Repeat
      NewFileName('Dateiname fÅr Daten?', FileName)
    Until TestFiles;

  End;

  OpenAllFiles;

  SelectNext(False);



end;

Destructor TFileFindWindow.Done;
Begin
  Dispose(ACollection, Done);
  Dispose(ADateiDataListBox, Done);
  TDialog.Done;
end;

Function TFileFindWindow.GetMaskBack: String12;
Var MaskeHilf: String;
Begin
  MaskeHilf := AMemo^.Data^;
  MaskeHilf := UpDate(MaskeHilf);

  GetMaskBack := MaskeHilf;
End;


Procedure TFileFindWindow.HandleEvent(Var Event: TEvent);
Begin
  
  If (Event.What = evBroadCast) and
     (Event.Command = cmListItemSelected) and
     (Event.InfoPtr = ADateiDataListBox) then
  Begin
    SetState(sfVisible, False);

    GetDatenDialog;

    SetState(sfVisible, True);

    ClearEvent(Event)
  End;


  If (Event.What = evKeyDown) and
     (Event.KeyCode = kbEnter) and
     AMemo^.GetState(sfSelected) then
  Begin
    Event.What := evCommand;
    Event.Command := cmDateiAnfang
  End;

  If Event.What = evCommand then
  Case Event.Command of
    cmDateiAnfang,
    cmDateiNext      : Begin
                         ADateiDataListBox^.ListMaske :=
                         FileFindWindow^.GetMaskBack;
                         FileFindWindow^.PutMaskeToMemo(ADateiDataListBox^.
                                                        ListMaske);

                         If Event.Command = cmDateiAnfang then
                         Begin
                           Anzahl := (MemAvail - 17408) Div 24;

                           If Anzahl > FileSize(DateiDataFile) then
                             Anzahl := FileSize(DateiDataFile);

                           Stelle := 1;
                           EnableCommands([cmDateiNext]);
                           DisableCommands([cmDateiBack]);
                         End;


                         With FileFindWindow^.ACollection^ do
                         Begin
                           Done;
                           Init(5, 5)
                         end;

                         ADateiDataListBox^.Options := ADateiDataListBox^.Options And Not ofSelectable;
                         ADateiDataListBox^.TopItem := 0;
                         ADateiDataListBox^.SetRange(0);

                         ADateiDataListBox^.GetFiles(Stelle, ADateiDataListBox^.Geladen);

                         If ADateiDataListBox^.Geladen <> 0 then
                         Begin
                           ADateiDataListBox^.Options := ADateiDataListBox^.Options or ofSelectable;
                           ADateiDataListBox^.FocusItem(0);
                           ADateiDataListBox^.Draw;
                           ADateiDataListBox^.Select;
                           If Eof(DateiDataFile) then
                           Begin
                             DisableCommands([cmDateiNext]);
                             EnableCommands([cmDateiBack])
                           End
                           Else
                             If FilePos(DateiDataFile) - ADateiDataListBox^.Geladen > 0 then
                               EnableCommands([cmDateiBack]);

                           If (ADateiDataListBox^.ListMaske <> '*.*') and
                              Eof(DateiDataFile)   and
                              (Event.Command = cmDateiAnfang) then
                             DisableCommands([cmDateiNext, cmDateiBack]);
                           If Mehrfache and
                              Eof(DateiDataFile)   and
                              (Event.Command = cmDateiAnfang) then
                             DisableCommands([cmDateiNext, cmDateiBack])
                         End
                         Else Wait('Keine Dateien da mit dieser Maske!')
                       End;
    cmDateiBack      : Begin
                         ADateiDataListBox^.ListMaske := FileFindWindow^.GetMaskBack;
                         FileFindWindow^.PutMaskeToMemo(ADateiDataListBox^.ListMaske);

                         With FileFindWindow^.ACollection^ do
                         Begin
                           Done;
                           Init(5, 5)
                         end;

                         ADateiDataListBox^.Options := ADateiDataListBox^.Options And Not ofSelectable;
                         ADateiDataListBox^.TopItem := 0;
                         ADateiDataListBox^.SetRange(0);

                         Dec(Stelle, Anzahl * 2);
                         If Stelle < 1 then Stelle := 1;

                         ADateiDataListBox^.GetFiles(Stelle, ADateiDataListBox^.Geladen);

                         If ADateiDataListBox^.Geladen <> 0 then
                         Begin
                           ADateiDataListBox^.Options := ADateiDataListBox^.Options or ofSelectable;
                           ADateiDataListBox^.FocusItem(0);
                           ADateiDataListBox^.Draw;
                           ADateiDataListBox^.Select;
                           If Stelle = 1 then
                           Begin
                             DisableCommands([cmDateiBack]);
                             EnableCommands([cmDateiNext])
                           End
                           Else
                             EnableCommands([cmDateiNext])
                         End
                         Else Wait('Keine Dateien da mit dieser Maske!')
                       End;

  End;

  TDialog.HandleEvent(Event)

End;

End.

