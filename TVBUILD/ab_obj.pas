Unit AB_OBJ; (* Gundlegende Klassen fÅr Elemente eines Programms *)



Interface
Uses App, Objects, Views, Strings, Editor, Dialogs, MsgBox, Var_Obj,
     TVBUCMDS;


(* Grund - Objekte die den Programmcode Representieren *)

Const
      ProcDef  = 1; (* Deklaration einer Procedure *)
      FuncDef  = 2; (* Deklaration einer Funktion  *)
      VarDef   = 3; (* Deklaration einer Variablen *)
      ClassDef = 4; (* Deklaration einer Klasse    *)
      TypDef   = 5; (* Deklaration eines Typs      *)


      Bez      = 6; (* Var, Type, Begin...         *)

      Ending : Char = ';';





      WortChar   : Set of Char = ['A'..'Z', 'a'..'z'];
      NumberChar : Set of Char = ['0'..'9'];


Var
        Error       : Integer;
        Ch          : Char;
        CommentEbene: Integer;
        KlammerEbene: Integer;
        BlockEbene  : Integer;
        Token,
        HilfsToken  : String; (* Ein Wort, Bezeichner, Zahl *)




Type

      PErrorDataTyp  = ^TErrorDataTyp;
      TErrorDataTyp  = Object (TObject)
                         Zeile,
                         Spalte     : Integer;
                         FileName   : String;
                         ErrorString: String;

                         Constructor Init(AZeile, ASpalte: Integer;
                                          AFileName, AErrorString: String);
                         Destructor  Done; Virtual;
                       End;


      PErrorDataList = ^TErrorDataList;
      TErrorDataList = Object(TCollection)
        Constructor Init;
        Destructor  Done; Virtual;

        Procedure   InsertError(Zeile, Spalte: Integer;
                                FileName, ErrorString: String);
        Function    GetErrorAt(I: Integer): PErrorDataTyp;
      End;



      PMyList = ^TMyList;
      TMyList = Object(TCollection)
        Focused: Integer;
        Name   : String;

        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure SetName(S: String);
        Function  GetName: String;

        Procedure Store(Var S: TStream); Virtual;
      End;

      (**********************)
      (* Ein SchlÅsselwort: *)
      (**********************)

      PKeyWord = ^TKeyWord;
      TKeyWord = Object(TObject)
        Wort   : String;

        Constructor Init(KeyWord: String);
        Destructor  Done; Virtual;

        Function    GetKeyWord: String;
      End;

      (****************************************)
      (* Eine SchlÅsselwortliste eines Typ's: *)
      (****************************************)

      PKeyWordList = ^TKeyWordList;
      TKeyWordList = Object(TCollection)
        Typ        : Integer;

        Constructor Init(KeyTyp: Integer);
        Destructor  Done; Virtual;


        Function   GetKeyTyp: Integer;
        Procedure  InsertKey(KeyWord: String);
        Function   GetKey: String;
        Function   FindKey(KeyWord: String): Integer;
      End;

      (*************************)
      (* Alle SchlÅsselwîrter: *)
      (*************************)

      PHoleKeyWords = ^THoleKeyWords;
      THoleKeyWords = Object(TObject)
        AKeyTypList : PCollection;


        Constructor Init;
        Destructor  Done; Virtual;

        Procedure  InsertKey(KeyWord: String; KeyTyp: Integer);
        Function   GetKey: String;

        Function   GetKeyWordTyp(Token: String): Integer;
      End;

      (********************************)
      (* Die Quelldatei ( zum Lesen ) *)
      (********************************)


      PSourceFile = ^TSourceFile;
      TSourceFile = Object(TObject)
        LastRet     : Longint;
        ZwischenRet : Longint;
        F           : File of Char;
        IsEOF       : Boolean;
        FilePosition: Longint;
        Opened      : Boolean;

        Zeile,
        Spalte      : Integer;

        Constructor Init(FileName: String);
        Destructor  Done; Virtual;
        Function    OpenFile: Integer;

        Function    Is_EOF: Boolean;
        Procedure   SetRet;

        Function    GetLine: String;
        Function    GetChar: Char;
        Function    GetWort: String;
        Procedure   PutBack(I: Integer);
      End;


      PObjectStack = ^TObjectStack;
      TObjectStack = Object(TCollection)
        Procedure Push(Item: Pointer);
        Procedure Pop(Var Item: Pointer);
      End;













      (*******************)
      (* Hauptquellcode: *)
      (*******************)


      PMainCode = ^TMainCode;
      TMainCode = Object(TObject)
        (* Syntax - Check *)

        


        AText: PCollection;





        Procedure SkipComment(CommentChar: Char);
        Function  ReadWord: String;
        Function  ReadZahl: String;
        Procedure CheckKommentar;
        Function  CheckString: Boolean;
        Procedure CheckPoint;
        Procedure CheckPointerDeRef;
        Procedure CheckDoppelpunkt;
        Procedure SkipBlank;
        Procedure SkipTil(S: String);
        Function  CheckWort: Integer;


        (**************************************************************)
        (* Einlesen von Definitionen, und SchlÅsselwîrter generieren: *)
        (**************************************************************)

        Procedure ReadTypDef;
        Procedure VarOfTyp;
        Procedure ReadVarDef;
        Procedure ReadConstDef;
        Procedure ReadUsesDef;


        Procedure ReadObjectDef;
        Procedure ReadObjectCode;


        Constructor Init;
        Constructor Load(Var S: TStream);
        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Procedure Edit;
      End;

      PUnitObj  = ^TUnitObj;
      TUnitObj  = Object(TObject)
        UnitName: String;
        Constructor Init;
        Constructor Load(Var S:TStream);

        Procedure   Store(Var S:TStream); Virtual;

        Procedure WriteCode(S: Char);
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Function  GetName: String;
        Procedure SetName(S: String);
      End;

      PUnitList = ^TUnitList;
      TUnitList = Object(TMyList)
        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Procedure Edit;
      End;

      PTypeList = ^TTypeList;
      TTypeList = Object(TMyList)
        Constructor Init;
        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
      End;

      PVarObj = ^TVarObj;
      TVarObj = Object(TObject)
        VarName: String;


        Constructor Init;
        Constructor Load(Var S: TStream);
        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode(S: String);
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Function  GetName: String;
        Procedure SetName(S: String);
      End;

      PVarTypList = ^TVarTypList;
      TVarTypList = Object(TMyList)
        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure Store(Var S: TStream); Virtual;


        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Procedure Edit;
      End;


      PVarList = ^TVarList;
      TVarList = Object(TMyList)
        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
        Procedure Edit;
      End;

      PConstObj = ^TConstObj;
      TConstObj = Object(TObject)
        ConstName  : String;
        ConstTyp   : String;
        ConstValue : String;


        Constructor Init;
        Constructor Load(Var S: TStream);
        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;

        Function  GetName: String;
        Procedure SetName(S: String);
        Function  GetTyp: String;
        Procedure SetTyp(S: String);
        Function  GetValue: String;
        Procedure SetValue(S: String);

      End;

      PConstList = ^TConstList;
      TConstList = Object(TMyList)
        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure Store(Var S: TStream); Virtual;

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;

        Procedure Edit;
      End;

      PBeginBlock = ^TBeginBlock;
      PEndBlock = ^TEndBlock;
      PProcedureList = ^TProcedureList;


      PProcedureObj = ^TProcedureObj;
      TProcedureObj = Object(TMainCode)
        Name        : String;
        Typ         : String;


        ATypeList   : PTypeList;

        AConstList  : PConstList;

        AVarList    : PVarList;

        AProcedureList: PProcedureList;

        AMainCode : PMainCode;


        

        
        (*******************)



        Constructor Init;
        Constructor Load(Var S: TStream);
        Procedure Store(Var S: TStream); Virtual;

        Procedure CheckForParameter;
        Function  CheckFuncReturn: Boolean;

        Function  GetProcedureName: String;
        Procedure SetProcedureName(S: String);

        Function  GetProcedureTyp: String;
        Procedure SetProcedureTyp(S: String);

        Procedure WriteCode;
        Function  ReadCode(ErrorData: PErrorDataList): Boolean;
      End;


      TProcedureList = Object(TMyList)
        Constructor Init;
        Constructor Load(Var S: TStream);

        Procedure Store(Var S: TStream); Virtual;


        Procedure Edit;

        Procedure InsertProcedure(Var Proc: PProcedureObj);

        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
      End;



      TBeginBlock = Object(TObject)
        Constructor Init;
        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
      End;

      TEndBlock = Object(TObject)
        Constructor Init;
        Procedure WriteCode;
        Function  ReadCode(Var ErrorData: PErrorDataList): Boolean;
      End;




Const
     RMainCode : TStreamRec = (
       ObjType: StRegMainCode;
       VmtLink: Ofs(TypeOf(TMainCode)^);
       Load   : @TMainCode.Load;
       Store  : @TMainCode.Store
     );

     RUnitList : TStreamRec = (
       ObjType: StRegUnitList;
       VmtLink: Ofs(TypeOf(TUnitList)^);
       Load   : @TUnitList.Load;
       Store  : @TUnitList.Store
     );

     RUnitObj : TStreamRec = (
       ObjType: StRegUnitObj;
       VmtLink: Ofs(TypeOf(TUnitObj)^);
       Load   : @TUnitObj.Load;
       Store  : @TUnitObj.Store
     );

     RConstList : TStreamRec = (
       ObjType: StRegConstList;
       VmtLink: Ofs(TypeOf(TConstList)^);
       Load   : @TConstList.Load;
       Store  : @TConstList.Store
     );

     RTypeList : TStreamRec = (
       ObjType: StRegTypeList;
       VmtLink: Ofs(TypeOf(TTypeList)^);
       Load   : @TTypeList.Load;
       Store  : @TTypeList.Store
     );

     RMyList : TStreamRec = (
       ObjType: StRegMyList;
       VmtLink: Ofs(TypeOf(TMyList)^);
       Load   : @TMyList.Load;
       Store  : @TMyList.Store
     );

     RVarObj : TStreamRec = (
       ObjType: StRegVarObj;
       VmtLink: Ofs(TypeOf(TVarObj)^);
       Load   : @TVarObj.Load;
       Store  : @TVarObj.Store
     );

     RVarList : TStreamRec = (
       ObjType: StRegVarList;
       VmtLink: Ofs(TypeOf(TVarList)^);
       Load   : @TVarList.Load;
       Store  : @TVarList.Store
     );

     RVarTypList : TStreamRec = (
       ObjType: StRegVarTypList;
       VmtLink: Ofs(TypeOf(TVarTypList)^);
       Load   : @TVarTypList.Load;
       Store  : @TVarTypList.Store
     );

     RConstObj : TStreamRec = (
       ObjType: StRegConstObj;
       VmtLink: Ofs(TypeOf(TConstObj)^);
       Load   : @TConstObj.Load;
       Store  : @TConstObj.Store
     );

     RProcedureObj : TStreamRec = (
       ObjType: StRegProcedureObj;
       VmtLink: Ofs(TypeOf(TProcedureObj)^);
       Load   : @TProcedureObj.Load;
       Store  : @TProcedureObj.Store
     );

     RProcedureList : TStreamRec = (
       ObjType: StRegProcedureList;
       VmtLink: Ofs(TypeOf(TProcedureList)^);
       Load   : @TProcedureList.Load;
       Store  : @TProcedureList.Store
     );



Var F: Text;

    ASourceFile : PSourceFile;
    AHoleKeyWordList: PHoleKeyWords;

    ErrorData : PErrorDataList;

    ObjectStack: PObjectStack;

Implementation
Uses UInPro, VarInPro, ConInPro, SubBuild;



PROCEDURE EditorDialog(Var T: PCollection);

  VAR
    R: tRect;
    View: pView;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (1, 4, 77, 21);
  Dialog := New (pDialog, Init (R, 'Editor'));
  Dialog^.State := 2171;
 
  R.Assign (69, 3, 70, 12);
  View := New (pScrollBar, Init  (R)); 
  Dialog^.Insert (View); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (6, 3, 69, 12);

  View := New (pMyMemoEdit, Init (R, NIL, pScrollBar (View), T, 4711, 3838));
  Dialog^.Insert (View);

  R.Assign (6, 2, 24, 3);
  Dialog^.Insert (New (pLabel, Init (R, '~P~rogramm - Text', View)));
 
  R.Assign (18, 13, 28, 15);
  Dialog^.Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (46, 13, 59, 15);
  Dialog^.Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  Dialog^.SelectNext (FALSE);




    { Datenrecord initialisieren ! }

  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN

    Dialog^.GetData(T);

  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;



(*********************************************)

Constructor TKeyWord.Init(KeyWord: String);
Begin
  Wort := KeyWord
End;

Destructor  TKeyWord.Done;
Begin
End;

Function    TKeyWord.GetKeyWord: String;
Begin
  GetKeyWord := Wort;
End;

(**********************************************)

Constructor TKeyWordList.Init(KeyTyp: Integer);
Begin
  TCollection.Init(1, 1);
  Typ := KeyTyp
End;

Destructor  TKeyWordList.Done;
Begin
End;

Function    TKeyWordList.GetKeyTyp: Integer;
Begin
  GetKeyTyp := Typ;
End;

Procedure   TKeyWordList.InsertKey(KeyWord: String);
Begin
  Insert(New(PKeyWord, Init(UpDate(KeyWord))));
End;

Function    TKeyWordList.GetKey: String;
Begin
End;

Function    TKeyWordList.FindKey(KeyWord: String): Integer;
Var A: Integer;
Begin
  FindKey := - 1;
  If Count = 0 then
  Begin
    Exit;
  End;
  For A := 0 to Count - 1 do
    if PKeyWord(At(A))^.GetKeyWord = KeyWord then
    Begin
      FindKey := Typ;
      Exit
    End
End;

(************************************************)

Constructor THoleKeyWords.Init;
Begin
  TObject.Init;
  AKeyTypList := New(PCollection, Init(1, 1));
End;

Destructor  THoleKeyWords.Done;
Begin

End;

Procedure   THoleKeyWords.InsertKey(KeyWord: String; KeyTyp: Integer);
Var A: Integer;
Begin
  If AKeyTypList^.Count = 0 then
  Begin
    A := AKeyTypList^.Count;
    AKeyTypList^.AtInsert(0, New(PKeyWordList, Init(KeyTyp)));
    PKeyWordList(AKeyTypList^.At(0))^.InsertKey(KeyWord);
  End
  Else
  Begin
    For A := 0 to AKeyTypList^.Count - 1 do
    Begin
      If PKeyWordList(AKeyTypList^.At(A))^.GetKeyTyp = KeyTyp then
      Begin
        If PKeyWordList(AKeyTypList^.At(A))^.FindKey(KeyWord) = - 1 then
          PKeyWordList(AKeyTypList^.At(A))^.InsertKey(KeyWord);

        Exit;
      End;
    End;

    (* Keytyp noch nicht vorhanden: *)
    AKeyTypList^.AtInsert(0, New(PKeyWordList, Init(KeyTyp)));
    PKeyWordList(AKeyTypList^.At(0))^.InsertKey(KeyWord);
  End;
End;

Function    THoleKeyWords.GetKey: String;
Begin
End;

Function    THoleKeyWords.GetKeyWordTyp(Token: String): Integer;
Var A: Integer;
    Index: Integer;
Begin
  GetKeyWordTyp := - 1;

  If AKeyTypList^.Count = 0 then
  Begin
    Wait('Keine SchlÅsselwîrter vorhanden!');
    Exit;
  End;

  For A := 0 to AKeyTypList^.Count - 1 do
  Begin
    Index := PKeyWordList(AKeyTypList^.At(A))^.FindKey(Token);

    If Index <> - 1 then
    Begin
      GetKeyWordTyp := PKeyWordList(AKeyTypList^.At(A))^.GetKeyTyp;
      Exit
    End
  End;
End;

(************************************************)



Constructor TSourceFile.Init(FileName: String);
Begin
  Assign(F, FileName);
  Opened := False;
  LastRet := 0;
  ZwischenRet := 0;
  IsEOF := False;
  Zeile := 1;
  Spalte := 1;
  FilePosition := 0;
End;

Function   TSourceFile.OpenFile: Integer;
Begin
  {$I-}
  Reset(F);
  OpenFile := 0;

  if IOResult <> 0 then
  Begin
    OpenFile := - 1;
    ReWrite(F);
  End;

  if IOResult <> 0 then
  Begin
    OpenFile := - 2;
    Exit;
  End;
  Opened := True;

  {$I+}
End;

Function   TSourceFile.GetLine: String;
Begin
End;

Function   TSourceFile.Is_EOF: Boolean;
Begin
  Is_EOF := IsEOF;
End;

Procedure  TSourceFile.SetRet;
Begin
  LastRet := ZwischenRet;
  ZwischenRet := FilePosition
End;

Procedure  TSourceFile.PutBack(I: Integer);
Begin
  Dec(FilePosition, I);
  If FilePosition < 0 then
  Begin
    FilePosition := 0;
  End;

  Dec(Spalte);
  If Spalte = 0 then
    Spalte := FilePosition - LastRet;


  Seek(F, FilePosition);
End;

Function   TSourceFile.GetChar: Char;
Var Ch: Char;
Begin
  Read(F, Ch);

  If Eof(F) then IsEOF := True;

  Inc(FilePosition);

  If Ch = Chr(10) then
  Begin
    Read(F, Ch);
    Inc(FilePosition)
  End;

  If Ch = Chr(13) then
  Begin
    SetRet;
    Inc(Zeile);
    Spalte := 1;
    Ch := ' '
  End
  Else
    Inc(Spalte);

  If Spalte > 255 then
  Begin
    Wait('Zeile zu lang!');
    IsEOF := True
  End;

  GetChar := Ch;
End;


Function   TSourceFile.GetWort: String;
Begin
End;

Destructor TSourceFile.Done;
Begin
  If Opened then Close(F)
End;


Procedure TObjectStack.Push(Item: Pointer);
Begin
  AtInsert(0, Item)
End;

Procedure TObjectStack.Pop(Var Item: Pointer);
Begin
  If Count > 0 then
  Begin
    Item := At(0);
    AtDelete(0)
  End;
End;


Constructor TMainCode.Init;
Begin
  AText := New(PCollection, Init(1, 1))
End;

Constructor TMainCode.Load(Var S: TStream);
Begin
  AText := New(PCollection, Load(S))
End;

Procedure TMainCode.Store(Var S:TStream);
Begin
  AText^.Store(S)
End;

Procedure TMainCode.WriteCode;
Var A: Integer;
Begin
  If AText <> Nil then
  Begin
    For A := 0 to AText^.Count - 1 do
      Writeln(F, PStr(AText^.At(A))^.GetStr);
  End
End;

Procedure TMainCode.SkipComment(CommentChar: Char);
Begin
  Inc(CommentEbene);

  If CommentChar = '(' then
    While (Not ASourceFile^.Is_EOF) and
          (CommentEbene <> 0) Do
    Begin
      Ch := ASourceFile^.GetChar;


      If Ch = '(' then
        If ASourceFile^.GetChar = '*' then
          SkipComment(CommentChar)
        Else
          ASourceFile^.PutBack(1);

      If Ch = '*' then
        If ASourceFile^.GetChar = ')' then
        Begin
          Dec(CommentEbene);
          If CommentEbene < 0 then Wait('Fehler im Kommentar - Block')
        End
        Else
          ASourceFile^.PutBack(1);
    End
  Else
    While (Not ASourceFile^.Is_EOF) and
          (CommentEbene <> 0) Do
    Begin
      Ch := ASourceFile^.GetChar;

      If Ch = '{' then
          SkipComment(CommentChar)
        Else
          ASourceFile^.PutBack(1);

      If ASourceFile^.GetChar = '}' then
      Begin
        Dec(CommentEbene);
        If CommentEbene < 0 then Wait('Fehler im Kommentar - Block')
      End
      Else
        ASourceFile^.PutBack(1);
    End;
End;

Function TMainCode.ReadWord: String;
Begin
  Token := '';
  ASourceFile^.PutBack(1);
  Repeat
    Ch := ASourceFile^.GetChar;

    If Ch in ['0'..'9',
              'A'..'Z',
              'a'..'z'] then
      Token := Token + Ch

    Else ASourceFile^.PutBack(1);

  Until Not (Ch in ['0'..'9',
                    'A'..'Z',
                    'a'..'z']) or ASourceFile^.Is_EOF;


  ReadWord := Token;
End;

Function TMainCode.ReadZahl: String;
Var AHoleKeyWordList: PHoleKeyWords;
Begin
  Token := '';
  ASourceFile^.PutBack(1);
  Repeat
    Ch := ASourceFile^.GetChar;

    If Ch in ['0'..'9', '.'] then
      Token := Token + Ch
    Else ASourceFile^.PutBack(1);

  Until Not (Ch in ['0'..'9', '.']) or ASourceFile^.Is_EOF;
  ReadZahl := Token;
End;


(********************)
(*     Checker:     *)
(********************)

Procedure TMainCode.CheckKommentar;
Begin
    If Ch = '(' then
      If ASourceFile^.GetChar = '*' then
        SkipComment('(')
      Else
        ASourceFile^.PutBack(1);

    If Ch = '{' then
      SkipComment('{')
    Else
      ASourceFile^.PutBack(1);
End;

Function TMainCode.CheckString: Boolean;
Var S    : String;
    Ch1,
    Ch2  : Char;
Begin
  S := '';

  If Ch <> '''' then
    CheckString := False
  Else
    CheckString := True;

  While Ch = '''' do
  Begin
    Ch1 := ASourceFile^.GetChar;
    Ch2 := ASourceFile^.GetChar;

    if (Ch1 = '''') and
       (Ch2 = '''') then
    Begin
      S := S + '''';
    End

    Else
    Begin
      If Ch1 = '''' then
      Begin
        Ch := '+'; (* Ende des Strings *)
        ASourceFile^.PutBack(1) (* Ch2 mu· wieder gelesen werden *)
      End;

      If Ch2 = '''' then
      Begin
        S := S + Ch1;
        ASourceFile^.PutBack(1)
      End;

      if (Ch1 <> '''') and
         (Ch2 <> '''') then
      Begin
        S := S + Ch1;
        ASourceFile^.PutBack(1)
      End

    End;
  End;
End;


Procedure TMainCode.CheckPoint;
Begin
    If Ch = '.' then
    Begin
      If UpDate(Token) <> 'END' then
      Begin
        Token := '';
        HilfsToken := Ch;

        Repeat
          Ch := ASourceFile^.GetChar;
          Token := Token + Ch;
        Until Ch <> ' ';

        If Not (Ch in ['A'..'Z', 'a'..'z']) then
        Begin
          Wait('Erwarte Feldbezeichner: ' + Ch);
        End
      End;
    End;
End;

Procedure TMainCode.CheckPointerDeRef;
Begin
    If Ch = '^' then
    Begin
      HilfsToken := Ch;
      Ch := ASourceFile^.GetChar;

      Case Ch of
        '.' : Begin
                Ch := ASourceFile^.GetChar;
                While Ch = ' ' do
                  Ch := ASourceFile^.GetChar;

                If Ch in ['A'..'Z', 'a'..'z'] then
                Begin
                  Token := ReadWord;
                End
                Else
                  Wait('Erwarte Bezeichner ' + Ch)
              End;
        ' ' : Begin
                Token := '';
                While (Ch = ' ') do
                Begin
                  Ch := ASourceFile^.GetChar;
                  Token := Token + Ch;
                End;
                If Ch <> ':' then Wait('Erwarte :=.' + Chr(13) +
                                       'Erhalte: ' + Token);
                Token := '';
              End;
      End;
    End;
End;

Procedure TMainCode.CheckDoppelpunkt;
Begin
    If Ch = ':' then
    Begin
      Ch := ASourceFile^.GetChar;

      If Ch = '=' then
      Begin
        Ch := ASourceFile^.GetChar;

        While Ch = ' ' do Ch := ASourceFile^.GetChar;

        Case Ch of
          'A'..'Z', 'a'..'z' : Begin
                                 Token := Token + ' := ' + ReadWord;
                                 (*Wait(Token);*)
                               End;
          '0'..'9', '.'      : Begin
                                 Token := Token + ' := ' + ReadZahl;
                                 (*Wait(Token);*)
                               End;
          ';'                : Wait('Erwarte Ausdruck,' + Chr(13) +
                                    'Erhalte: ''' + Ch + '''');

          ''''               : CheckString;
        End;
      End;
    End;
End;

Procedure TMainCode.SkipBlank;
Var OCh: Char;
Begin
  While (Ch = ' ') and
        (Ch = Chr(10)) and
        (Ord(Ch) < 33) and
        Not (ASourceFile^.Is_EOF) do
    Ch := ASourceFile^.GetChar;

  If ASourceFile^.Is_EOF then
  Begin
    Wait('Error while skipping blank!');
    Halt
  End;
End;





(**************************************************************)
(* Einlesen von Definitionen, und SchlÅsselwîrter generieren: *)
(**************************************************************)


Procedure TMainCode.ReadTypDef;
Var ATypBez: String;
    Check: Integer;
Begin
  SkipBlank;
  ASourceFile^.PutBack(1); (* Zeichen wird erst in der Schleife ausgelesen *)

  While Not ASourceFile^.Is_EOF do
  Begin
    Ch := ASourceFile^.GetChar;


    (* Hole neuen Bezeichner, der spÑter *)
    (* als Typ verwendet werden soll.    *)

    Check := CheckWort;

    If (Check <> - 1) and
       (Check <> - 2) then
    Begin
      Wait('Engelesenes Wort ist ein schon bekanntes SchlÅsselwort!');
    End
    Else
    Begin
      If Check <> - 2 then
      Begin
        Wait('Token ist ' + Token);

        ATypBez := Token; (* Name eines Typ's *)

        SkipBlank;(* Zeichen => Ch *)

        Wait('Zeichen nach SkipBlank ist ' + Ch);

        Case Ch of
          ':' : Begin
                  Wait('Erwarte =');
                  Halt;
                End;
          '=' : Begin
                  Ch := ASourceFile^.GetChar;

                  If Ch <> ' ' then Wait('SkipBlank kann hier nicht gehen');

                  SkipBlank;

                  If Not ((Ch In WortChar) or
                         (Ch = '^')) then
                  Begin
                    Wait('Nach TYPE ... = folgt ein "Wort" oder ^"Wort"' + Chr(13) +
                         'Aber es folgt ' + GetString(Ord(Ch)));
                    Halt
                  End;


                  If Ch = '^' then
                  Begin
                    Wait('Pointer - Forward - Deklaration (^)');
                    ASourceFile^.GetChar;
                    ReadWord;
                    SkipBlank;
                    If Ch <> ';' then Wait('Erwarte ;');
                    Halt
                  End;

                  (* Eigentlich kînnte hier in Variablen definieren *)
                  (* eingesprungen werden (Procedure VarOfTyp)      *)

                  Wait('Bin in ReadTypDef und gehe in VarOfTyp');

                  VarOfTyp;


                End;
          Else Wait(':, = nicht vorhanden!');
        End;



        (*Halt*)
      End;
    End;
    (*

    Case CheckWort of
      Bez     : Begin
                  If Token = 'BEGIN' then
                  Begin
                    Wait('Token ist ' + Token + ', -> Exit');
                    Exit
                  End;

                  If Token = 'TYPE' then
                  Begin
                    ReadTypDef;
                  End;

                  If Token = 'CONST' then
                  Begin
                    ReadConstDef;
                  End;

                  If Token = 'VAR' then
                  Begin
                    ReadVarDef;
                  End;

                  If Token = 'PROCEDURE' then
                  Begin
                    Wait('Token = ' + Token);
                  End;

                  If Token = 'FUNCTION' then
                  Begin
                    Wait('Token = ' + Token);
                  End;
                End;
      ProcDef : Begin End;
      FuncDef : Begin End;
      VarDef  : Begin End;
      TypDef  : Begin End;
      ClassDef: Begin End;
    End
    *)
  End;
  While Not ASourceFile^.Is_EOF do
  Begin
    ASourceFile^.GetChar;


    If Token = 'RECORD'then
    Begin
      Wait('Token ist ' + Token);
      SkipTil('END');
      ASourceFile^.GetChar
    End;
  End;
End;

(*****************************************************)
(* VarOfTyp wird aufgerufen, wenn mit Type ein neuer *)
(* Typ definiert wird und nach dem '=' ein einfacher *)
(* Variablentyp angegeben wird, oder wenn z.B. in    *)
(* einem Record Bezeichner eingelesen werden mÅssen. *)
(*****************************************************)

Procedure TMainCode.VarOfTyp;
Var AStr: PStr;
Begin
  Case CheckWort of
    Bez : Begin
            Wait('Token ist ' + Token);

            If Token = 'END' then
            Begin
              (* Skope resolution verlassen        *)
              (* (mit Hilfe eines "Skope - Staks") *)

              (*ObjectStack^.Pop(Pointer(AStr));*)

              Wait('Skope resolution verlassen.');

              ASourceFile^.GetChar;

              Exit;
            End;

            (*****************************************************)
            (* Ein Record kann verschiedene Variablen Speichern. *)
            (*****************************************************)


            If Token = 'RECORD' then
            Begin
              (*SkipBlank;*)

              (* Der Record hat weitere Bezeichner!    *)
              (* Eine Liste mit Bezeichnern eines Typs *)
              (* in einer Liste mit Typen              *)

              ReadVarDef; 

              (*ObjectStack^.Push(New(PStr, Init(Token)));*)
            End;


            (******************************************************)
            (* Ein Array kann nicht nur ordinale Typen Speichern, *)
            (* sondern auch einen Typ (Type...) oder z.B. Record. *)
            (******************************************************)

            If Token = 'ARRAY' then
            Begin
              SkipBlank;
              If Ch <> '[' then Wait('[ Erwartet');

              While (Ch <> ']') and
                    (Ch <> ';') do
                Ch := ASourceFile^.GetChar;

              If Ch = ']' then
              Begin
                ASourceFile^.GetChar;
                SkipBlank;
                if CheckWort = Bez then
                Begin
                  If Token = 'OF' then
                  Begin
                    SkipBlank;
                    VarOfTyp  (* Of Variablen - Typ *)
                  End
                  Else
                    Wait('Error, Of erwartet!');
                End;
              End
              Else
              Begin
                Wait('Error, Typ des Arrays nicht angegeben (Of ...)')
              End
            End;

            If Token = 'STRING' then
            Begin
            End;

            If Token = 'FILE' then
            Begin
            End;

            If Token = 'OBJECT'then
            Begin
            End;
            Wait('ObjectStack^.Count: ' + GetString(ObjectStack^.Count))
          End;
  End;
End;


Procedure TMainCode.ReadVarDef;
Begin
  (* Token 'VAR' erkannt:
       (Repeat) Lese Token(Name der Variable),
                Speichere in ANameList,
                SkipBlank,
                ',' mu· folgen
       (Until   Ch = ':')
                             *)


  SkipBlank;

  If CheckWort <> - 1 then
    Wait('Variablen Name schon definiert!')
  Else
    Wait('ReadVarDef: Variablenname ist ' + Token);

  SkipBlank;

  If Ch <> ':' then
  Begin
    Wait('Erwarte : , Erhalte ' + Ch);
    Halt
  End;

  ASourceFile^.GetChar;
  SkipBlank;

  Wait('Variablen Typ ist ' + ReadWord);

  SkipBlank;

  If Ch <> ';' then Wait('Erwarte ;')

End;
Procedure TMainCode.ReadConstDef;
Begin
End;
Procedure TMainCode.ReadUsesDef;
Begin
End;




Procedure TMainCode.ReadObjectDef;
Begin
  Wait('Token ist ' + Token);
  SkipTil('END');
  ASourceFile^.GetChar
End;

Procedure TMainCode.ReadObjectCode;
Begin
End;



(**************************************************************)



Procedure TMainCode.SkipTil(S: String);
Begin
  While (UpDate(ReadWord) <> UpDate(S)) and
        Not (ASourceFile^.Is_EOF) do
  Begin
    SkipBlank;

    While Not (ASourceFile^.GetChar In (['A'..'Z',
                                         'a'..'z',
                                         '0'..'9'])) do;
  End;

  If ASourceFile^.Is_EOF then
  Begin
    Wait('SkipTil -> Eof!');
    Halt
  End;
End;

Function TMainCode.CheckWort: Integer;
Begin
  CheckWort := - 1;
  If Ch in ['A'..'Z', 'a'..'z'] then
  Begin
    Token := UpDate(ReadWord);
    CheckWort := AHoleKeyWordList^.GetKeyWordTyp(Token)
  End
  Else CheckWort := - 2
End;

(********************************************************)


Function TMainCode.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
  While Not ASourceFile^.Is_EOF do
  Begin
    Case Ch of
      '('      : Begin
                   CheckKommentar;
                   If Ch = '(' then Inc(KlammerEbene)
                 End;
      ')'      : If KlammerEbene = 0 then
                   Wait('Klammer zu viel!')
                 Else
                   Dec(KlammerEbene);


      ''''     : If CheckString and (KlammerEbene = 0) then
                   Wait('String hier nicht erlaubt!');
      '.'      : CheckPoint;
      '^'      : CheckPointerDeRef;
      ':'      : CheckDoppelpunkt;
      'A'..'Z',
      'a'..'z' : Begin
                   Case CheckWort of
                     Bez : Begin
                             If Token = 'END' then
                               Exit;
                             (*
                             Begin
                               If Ch <> Ending then
                               Begin
                                 If Ch = ' ' then SkipBlank;

                                 If Ch <> Ending then
                                   Wait('Erwarte ' +
                                         Ending +
                                        ', Erhalte ' +
                                        Ch)
                               End
                             End*)
                           End;
                     (*Else
                       Wait('Wort unbekannt: ' + Token);*)
                   End;
                 End;

      ' ',
      Chr(10),
      '.',
      ';'      : Begin End; (*Wait('''' + Ch + '''');*)

      (*Else
        Wait('Syntax Error ''' + Ch + '''');*)

    End;
    Ch := ASourceFile^.GetChar;
  End
End;

Procedure TMainCode.Edit;
Begin
  EditorDialog(AText)
End;

Constructor TBeginBlock.Init;
Begin
End;

Procedure TBeginBlock.WriteCode;
Begin
  Writeln(F, 'Begin');
End;

Function TBeginBlock.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Constructor TEndBlock.Init;
Begin
End;

Procedure TEndBlock.WriteCode;
Begin
  Writeln(F, 'End.');
End;

Function TEndBlock.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Constructor TUnitObj.Init;
Begin
  UnitName := '';
End;

Constructor TUnitObj.Load(Var S: TStream);
Begin
  S.Read(UnitName, SizeOf(UnitName));
End;

Procedure TUnitObj.Store(Var S:TStream);
Begin
  S.Write(UnitName, SizeOf(UnitName));
End;

Procedure TUnitObj.WriteCode(S: Char);
Begin
  Writeln(F, '  ' + UnitName + S)
End;

Function TUnitObj.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Function TUnitObj.GetName: String;
Begin
  GetName := UnitName;
End;

Procedure TUnitObj.SetName(S: String);
Begin
  UnitName := S;
End;

Constructor TMyList.Init;
Begin
  TCollection.Init(1, 1);
  Focused := 0;
  Name := ''
End;

Constructor TMyList.Load(Var S: TStream);
Begin
  TCollection.Load(S);
  S.Read(Focused, SizeOf(Focused));
  S.Read(Name, SizeOf(Name));
End;

Procedure TMyList.Store(Var S: TStream);
Begin
  TCollection.Store(S);
  S.Write(Focused, SizeOf(Focused));
  S.Write(Name, SizeOf(Name));
End;

Procedure TMyList.SetName(S: String);
Begin
  Name := S
End;

Function TMyList.GetName: String;
Begin
  GetName := Name
End;


Constructor TUnitList.Init;
Begin
  TMyList.Init;
End;

Constructor TUnitList.Load(Var S: TStream);
Begin
  TMyList.Load(S);
End;

Procedure TUnitList.Store(Var S: TStream);
Begin
  TMyList.Store(S);
End;

Procedure TUnitList.WriteCode;
Var A: Integer;
Begin
  If Count > 0 then
  Begin
    Writeln(F, 'Uses ');
    For A := 0 to Count - 2 do
      PUnitObj(At(A))^.WriteCode(',');

    PUnitObj(At(Count - 1))^.WriteCode(';');
    Writeln(F);
  End;
End;

Function TUnitList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TUnitList.Edit;
Begin
  UnitsinProjektDialog(@Self);
End;

Constructor TTypeList.Init;
Begin
End;

Procedure TTypeList.WriteCode;
Begin
End;

Function TTypeList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Constructor TConstList.Init;
Begin
  TMyList.Init;
End;

Constructor TConstList.Load(Var S: TStream);
Begin
  TMyList.Load(S);
End;

Procedure TConstList.Store(Var S: TStream);
Begin
  TMyList.Store(S);
End;

Procedure TConstList.WriteCode;
Var A: Integer;
Begin
  If Count > 0 then
  Begin
    Writeln(F, 'Const');

    For A := 0 to Count - 1 do PConstObj(At(A))^.WriteCode;

    Writeln(F);
  End;
End;

Function TConstList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TConstList.Edit;
Begin
  ConstanteninProjektDialog(@Self);
End;

Constructor TVarList.Init;
Begin
  TMyList.Init;
End;

Constructor TVarList.Load(Var S: TStream);
Begin
  TMyList.Load(S);
End;

Procedure TVarList.Store(Var S: TStream);
Begin
  TMyList.Store(S);
End;

Procedure TVarList.WriteCode;
Var A: Integer;
Begin
  If Count > 0 then
  Begin
    Writeln(F, 'Var');

    (* Jede Liste mit Constiablen eines Typ's bearbeiten *)

    For A := 0 to Count - 2 do PVarTypList(At(A))^.WriteCode;

    PVarTypList(At(Count - 1))^.WriteCode;

    Writeln(F);
  End;
End;

Function TVarList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TVarList.Edit;
Begin
  VariableninProjektDialog(@Self);
End;

Constructor TVarTypList.Init;
Begin
  TMyList.Init;
End;

Constructor TVarTypList.Load(Var S: TStream);
Begin
  TMyList.Load(S);
End;

Procedure TVarTypList.Store(Var S: TStream);
Begin
  TMyList.Store(S);
End;

Procedure TVarTypList.WriteCode;
Var A: Integer;
Begin
  If Count > 0 then
  Begin
    For A := 0 to Count - 2 do
      PVarObj(At(A))^.WriteCode(',');

    PVarObj(At(Count - 1))^.WriteCode(': ' + Name + ';');
    Writeln(F);
  End;
End;

Function TVarTypList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TVarTypList.Edit;
Begin
  Wait('Edit Constiablen Typ')
End;


Constructor TProcedureList.Init;
Begin
  TMyList.Init;
End;

Constructor TProcedureList.Load(Var S: TStream);
Begin
  TMyList.Load(S);
End;

Procedure TProcedureList.Store(Var S: TStream);
Begin
  TMyList.Store(S);
End;

Procedure TProcedureList.InsertProcedure(Var Proc: PProcedureObj);
Begin
  Proc := New(PProcedureObj, Init);
  Insert(Proc)
End;

Procedure TProcedureList.WriteCode;
Var A: Integer;
Begin
  For A := 0 to Count - 1 do PProcedureObj(At(A))^.WriteCode;
End;

Function TProcedureList.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
(*  AtInsert(0, New(PProcedureObj, Init));

  PProcedureObj(At(0))^.ReadCode(ErrorData);*)
End;


Procedure TProcedureList.Edit;
Begin
  EditiereSubRoutinenDialog(@Self)
End;


Constructor TProcedureObj.Init;
Begin
  Name := '';
  Typ  := '';

  ATypeList      := Nil;

  AProcedureList := New(PProcedureList, Init);
  AConstList  := New(PConstList, Init);
  AVarList    := New(PVarList, Init);
  AMainCode   := New(PMainCode, Init);

End;

Constructor TProcedureObj.Load(Var S: TStream);
Begin
  S.Read(Name, SizeOf(Name));
  S.Read(Typ, SizeOf(Typ));

  ATypeList      := Nil;

  AProcedureList := New(PProcedureList, Load(S));
  AConstList  := New(PConstList,  Load(S));
  AVarList    := New(PVarList,  Load(S));
  AMainCode   := New(PMainCode, Load(S));
End;

Procedure TProcedureObj.Store(Var S: TStream);
Begin
  S.Write(Name, SizeOf(Name));
  S.Write(Typ, SizeOf(Typ));

  AProcedureList^.Store(S);
  AConstList^.Store(S);
  AVarList^.Store(S);
  AMainCode^.Store(S)

End;

Procedure TProcedureObj.WriteCode;
Begin
  If Typ <> '' then Writeln(F, 'Function ' + Name + ': ' + Typ + ';');
  If Typ = '' then Writeln(F, 'Procedure ' + Name + ';');



  Writeln(F, 'Begin');

  If AMainCode <> Nil then AMainCode^.WriteCode;

  Writeln(F, 'End;');
  Writeln(F)
End;




Procedure TProcedureObj.CheckForParameter;
Begin
  If Ch = '(' then
  Begin
    While Ch <> ')' do
    Begin
      Ch := ASourceFile^.GetChar;

      if (CheckWort = Bez) and
         (Token = 'BEGIN') then
        Wait('Error')
    End
  End;
End;

Function TProcedureObj.CheckFuncReturn: Boolean;
Begin
  CheckFuncReturn := False;

  Ch := ASourceFile^.GetChar;

  While Ch <> ':' do
  Begin
    If CheckWort = Bez then Exit;
    If Ch = ';' then Exit;
    ASourceFile^.GetChar
  End;
  (*Wait('1....');*)


  ASourceFile^.GetChar;

  SkipBlank;

  If CheckWort <> Bez then Exit;
(*  Wait('2....');    *)
  SkipBlank;
  If ASourceFile^.GetChar <> ';' then
    Wait('; Erwarted')
  else
    CheckFuncReturn := True;
  (*Wait('3....')*)
End;


Function TProcedureObj.ReadCode(ErrorData: PErrorDataList): Boolean;
Var AProc: PProcedureObj;
Begin
  While Not ASourceFile^.Is_EOF Do
  Begin
    Ch := ASourceFile^.GetChar;

    Case CheckWort of
      Bez     : Begin
                  If Token = 'BEGIN' then
                  Begin
                    Wait('Token ist ' + Token + ', -> Exit');
                    Exit
                  End;

                  If Token = 'TYPE' then
                  Begin
                    ReadTypDef;
                  End;

                  If Token = 'CONST' then
                  Begin
                    ReadConstDef;
                  End;

                  If Token = 'VAR' then
                  Begin
                    ReadVarDef;
                  End;

                  If Token = 'PROCEDURE' then
                  Begin
                    Wait('Token = ' + Token);

                    AProc := New(PProcedureObj, Init);

                    AProcedureList^.InsertProcedure(AProc);

                    AProc^.SetProcedureTyp('');

                    SkipBlank;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 1');
                      Halt
                    End;

                    Wait('Procedure: ' + GetProcedureName);
                    AProc^.SetProcedureName(ReadWord);
                    Wait('Procedure: ' + GetProcedureName);

                    CheckForParameter;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 2');
                      Halt
                    End;

                    SkipBlank;
                    AProc^.ReadCode(ErrorData); (* !!! *)

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 3');
                      Halt
                    End;

                    Wait('Read Procedure - Code');

                    SkipBlank;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 4');
                      Halt
                    End;

                    AProc^.AMainCode^.ReadCode(ErrorData); (* !!! *)
                    Wait('Read Proc vor Proc - Main');
                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 5');
                      Halt
                    End;
                  End;

                  If Token = 'FUNCTION' then
                  Begin
                    Wait('Token = ' + Token);

                    AProc := New(PProcedureObj, Init);

                    AProcedureList^.InsertProcedure(AProc);

                    SkipBlank;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 1');
                      Halt
                    End;

                    Wait('Function: ' + GetProcedureName);
                    AProc^.SetProcedureName(ReadWord);
                    Wait('Function: ' + GetProcedureName);

                    SkipBlank;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 2');
                      Halt
                    End;

                    (* Suche nach Parameter *)

                    CheckForParameter;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 3');
                      Halt
                    End;

                    SkipBlank;

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 4');
                      Halt
                    End;

                    If CheckFuncReturn then
                      AProc^.SetProcedureTyp(Token);
                    (*Else
                      Wait('Fehler in Funktionstyp');*)

                    If Token = 'FUNCTION' then Wait('Token noch '+Token);

                    SkipBlank;

                    Wait('Read Proc vor Proc - Main');

                    AProc^.ReadCode(ErrorData); (* !!! *)

                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 5');
                      Halt
                    End;

                    Wait('Read Function - Code');

                    SkipBlank;
                    AProc^.AMainCode^.ReadCode(ErrorData); (* !!! *)
                    Wait('Ready');
                    If ASourceFile^.Is_EOF then
                    Begin
                      Wait('Eof 6');
                      Halt
                    End;

                  End;
                End;
      ProcDef : Begin End;
      FuncDef : Begin End;
      VarDef  : Begin End;
      TypDef  : Begin End;
      ClassDef: Begin End;
    End;
  End;
End;


Function TProcedureObj.GetProcedureName: String;
Begin
  GetProcedureName := Name
End;

Procedure TProcedureObj.SetProcedureName(S: String);
Begin
  Name := S
End;

Function TProcedureObj.GetProcedureTyp: String;
Begin
  GetProcedureTyp := Typ
End;

Procedure TProcedureObj.SetProcedureTyp(S: String);
Begin
  Typ := S
End;


Constructor TVarObj.Init;
Begin
  VarName := '';
End;

Constructor TVarObj.Load(Var S: TStream);
Begin
  S.Read(VarName, SizeOf(VarName));
End;

Procedure TVarObj.Store(Var S: TStream);
Begin
  S.Write(VarName, SizeOf(VarName));
End;

Procedure TVarObj.WriteCode(S: String);
Begin
  Writeln(F, '  ' + VarName + S);
End;

Function TVarObj.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TVarObj.SetName(S: String);
Begin
  VarName := S
End;

Function TVarObj.GetName: String;
Begin
  GetName := VarName
End;


Constructor TConstObj.Init;
Begin
  ConstName := '';
End;

Constructor TConstObj.Load(Var S: TStream);
Begin
  S.Read(ConstName, SizeOf(ConstName));
  S.Read(ConstTyp, SizeOf(ConstTyp));
  S.Read(ConstValue, SizeOf(ConstValue));
End;

Procedure TConstObj.Store(Var S: TStream);
Begin
  S.Write(ConstName, SizeOf(ConstName));
  S.Write(ConstTyp, SizeOf(ConstTyp));
  S.Write(ConstValue, SizeOf(ConstValue));
End;

Procedure TConstObj.WriteCode;
Begin
  If ConstTyp = '' then
    Writeln(F, '  ' +
               ConstName +
               ' = ' +
               ConstValue + ';')
  Else
    Writeln(F, '  ' +
               ConstName +
               ' : ' +
               ConstTyp +
               ' = ' +
               ConstValue + ';')
End;


Function TConstObj.ReadCode(Var ErrorData: PErrorDataList): Boolean;
Begin
End;


Procedure TConstObj.SetName(S: String);
Begin
  ConstName := S
End;

Function TConstObj.GetName: String;
Begin
  GetName := ConstName
End;

Procedure TConstObj.SetTyp(S: String);
Begin
  ConstTyp := S
End;

Function TConstObj.GetTyp: String;
Begin
  GetTyp := ConstTyp
End;

Procedure TConstObj.SetValue(S: String);
Begin
  ConstValue := S
End;

Function TConstObj.GetValue: String;
Begin
  GetValue := ConstValue
End;






Constructor TErrorDataTyp.Init(AZeile, ASpalte: Integer;
                               AFileName, AErrorString: String);
Begin
End;
Destructor  TErrorDataTyp.Done;
Begin
End;


Constructor TErrorDataList.Init;
Begin
End;
Destructor  TErrorDataList.Done;
Begin
End;

Procedure   TErrorDataList.InsertError(Zeile, Spalte: Integer;
                        FileName, ErrorString: String);
Begin
End;
Function    TErrorDataList.GetErrorAt(I: Integer): PErrorDataTyp;
Begin
End;






Procedure InitUnit;
Begin
  ObjectStack := New(PObjectStack, Init(1, 1));

  RegisterType(RMainCode);

  RegisterType(RMyList);


  RegisterType(RUnitList);
  RegisterType(RUnitObj);

  RegisterType(RConstList);
  RegisterType(RConstObj);

  RegisterType(RTypeList);

  RegisterType(RVarObj);
  RegisterType(RVarList);
  RegisterType(RVarTypList);
  RegisterType(RProcedureObj);
  RegisterType(RProcedureList);
End;

Begin
  InitUnit;
End.