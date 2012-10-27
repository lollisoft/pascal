PROCEDURE TestFirst;
  VAR View: PView;
  BEGIN
    IF TheFirst THEN BEGIN
      View := DeskTop^.Current;
      DeskTop^.Delete(View);
      Dispose(View, Done);
    END;
  END;

PROCEDURE EditButton(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, aTitle, ValueS, HelpCtxS: STRING;
      Default, Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('ButtonDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PButton(Template^.Client)^ DO BEGIN
      IF Title <> Nil THEN aTitle := Title^
                      ELSE aTitle := '';
      IF NOT Dialog^.SetItem('Text',    aTitle) THEN ErrorBox('Text',1);
      Str(Command, ValueS);
      IF NOT Dialog^.SetItem('Value',   ValueS) THEN ErrorBox('Value',1);
      IF AmDefault THEN Default := 1 ELSE Default := 0;
      IF NOT Dialog^.SetItem('Default', Default) THEN ErrorBox('Default',1);
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PButton(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('Text',    aTitle) THEN ErrorBox('Text',1);
        DisposeStr(Title);
        Title := NewStr(aTitle);
        IF NOT Dialog^.GetItem('Value',   ValueS) THEN ErrorBox('Value',1);
        Val(ValueS, Command, Error);
        IF NOT Dialog^.GetItem('Default', Default) THEN ErrorBox('Default',1);
        AmDefault := Default <> 0;
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditCluster(Template: PViewTemplate); FAR;
  VAR Rect: TRect;
      Dialog: PUseDialog;
      Name, HelpCtxS: STRING[80];
      Items: ARRAY[0..15] OF STRING[80];
      I, Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('ClusterDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PCluster(Template^.Client)^ DO BEGIN
      FOR I := 0 TO Strings.Count-1 DO BEGIN
        IF Strings.At(I) = Nil
         THEN Items[I] := ''
         ELSE Items[I] := PString(Strings.At(I))^;
        IF NOT Dialog^.SetItem(CHR(I+ORD('A')), Items[I]) THEN ErrorBox(CHR(I+ORD('A')),1);
      END;
      FOR I := Strings.Count TO 15 DO BEGIN
        Items[I] := '';
        IF NOT Dialog^.SetItem(CHR(I+ORD('A')), Items[I]) THEN ErrorBox(CHR(I+ORD('A')),1);
      END;
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      FOR I := 0 TO 15 DO IF NOT Dialog^.GetItem(CHR(I+ORD('A')), Items[I]) THEN ErrorBox(CHR(I+ORD('A')),1);
      IF Items[0] = ''
       THEN BEGIN
        DeskTop^.Delete(Template);
        Dispose(Template, Done);
       END
       ELSE BEGIN
        IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
        Template^.Name^.SetStr(Name);
        WITH PCluster(Template^.Client)^ DO BEGIN
          Strings.Done;
          Strings.Init(1,1);
          WHILE (Strings.Count < 15) AND (Items[Strings.Count] <> '')
           DO Strings.Insert(NewStr(Items[Strings.Count]));
          IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
          Val(HelpCtxS, HelpCtx, Error);
          Template^.GetBounds(Rect);
          Rect.B.Y := Rect.A.Y + Strings.Count;
          Template^.ChangeBounds(Rect);
          Template^.MaxSize.Y := Strings.Count;
        END;
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditStatic(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, aTitle, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('StaticDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PStaticText(Template^.Client)^ DO BEGIN
      IF Text <> Nil THEN aTitle := Text^
                     ELSE aTitle := '';
      IF NOT Dialog^.SetItem('Text',    aTitle) THEN ErrorBox('Text',1);
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PStaticText(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('Text',    aTitle) THEN ErrorBox('Text',1);
        DisposeStr(Text);
        Text := NewStr(aTitle);
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditInput(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, MaxLenS, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('InputDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PInputLine(Template^.Client)^ DO BEGIN
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
      Str(MaxLen, MaxLenS);
      IF NOT Dialog^.SetItem('MaxLen', MaxLenS) THEN ErrorBox('MaxLen',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PInputLine(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('MaxLen', MaxLenS) THEN ErrorBox('MaxLen',1);
        Val(MaxLenS, MaxLen, Error);
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditXInput(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, MaxLenS, MinLenS, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('XInputDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PValidInputLine(Template^.Client)^ DO BEGIN
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
      Str(MaxLen, MaxLenS);
      IF NOT Dialog^.SetItem('MaxLen', MaxLenS) THEN ErrorBox('MaxLen',1);
      Str(MinLen, MinLenS);
      IF NOT Dialog^.SetItem('MinLen', MinLenS) THEN ErrorBox('MinLen',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PValidInputLine(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('MaxLen', MaxLenS) THEN ErrorBox('MaxLen',1);
        Val(MaxLenS, MaxLen, Error);
        IF NOT Dialog^.GetItem('MinLen', MinLenS) THEN ErrorBox('MinLen',1);
        Val(MinLenS, MinLen, Error);
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditListBox(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, NumColsS, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('ListBoxDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PListBox(Template^.Client)^ DO BEGIN
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
      Str(NumCols, NumColsS);
      IF NOT Dialog^.SetItem('NumCols', NumColsS) THEN ErrorBox('NumColS',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PListBox(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('NumCols', NumColsS) THEN ErrorBox('NumCols',1);
        Val(NumColsS, NumCols, Error);
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditMemoEdit(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, LineLenS, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('MemoFieldDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PMemoEdit(Template^.Client)^ DO BEGIN
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
      Str(TheLineLen, LineLenS);
      IF NOT Dialog^.SetItem('LineLen', LineLenS) THEN ErrorBox('LineLen',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PMemoEdit(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('LineLen', LineLenS) THEN ErrorBox('LineLen',1);
        Val(LineLenS, TheLineLen, Error);
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

PROCEDURE EditAnyView(Template: PViewTemplate); FAR;
  VAR Dialog: PUseDialog;
      Name, HelpCtxS: STRING[80];
      Error: WORD;
  BEGIN
    Dialog := PUseDialog(DlgIDXFile.Get('AnyViewDialog'));
    Name := Template^.Name^.GetStr;
    IF NOT Dialog^.SetItem('Name',    Name) THEN ErrorBox('Name',1);
    WITH PView(Template^.Client)^ DO BEGIN
      Str(HelpCtx, HelpCtxS);
      IF NOT Dialog^.SetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
    END;
    IF DeskTop^.ExecView(Dialog) = cmOk THEN BEGIN
      IF NOT Dialog^.GetItem('Name',    Name) THEN ErrorBox('Name',1);
      Template^.Name^.SetStr(Name);
      WITH PView(Template^.Client)^ DO BEGIN
        IF NOT Dialog^.GetItem('HelpCtx', HelpCtxS) THEN ErrorBox('HelpCtx',1);
        Val(HelpCtxS, HelpCtx, Error);
      END;
     END
     ELSE TestFirst;
    Dispose(Dialog, Done);
  END;

TYPE TProc  = PROCEDURE (Template: PViewTemplate);
     TTypes = RECORD P: POINTER; YSize: BOOLEAN; Proc: TProc; END;

CONST LastProc = 13;
      Types: ARRAY [1..LastProc+1] OF TTypes =
       ( (P: TypeOf(TButton);         YSize: True;   Proc: EditButton),
         (P: TypeOf(TRadioButtons);   YSize: False;  Proc: EditCluster),
         (P: TypeOf(TCheckBoxes);     YSize: False;  Proc: EditCluster),
         (P: TypeOf(TStaticText);     YSize: False;  Proc: EditStatic),
         (P: TypeOf(TLabel);          YSize: False;  Proc: EditStatic),
         (P: TypeOf(TInputLine);      YSize: False;  Proc: EditInput),
         (P: TypeOf(TValidInputLine); YSize: False;  Proc: EditXInput),
         (P: TypeOf(TValInputLine);   YSize: False;  Proc: EditXInput),
         (P: TypeOf(THexInputLine);   YSize: False;  Proc: EditXInput),
         (P: TypeOf(TIntInputLine);   YSize: False;  Proc: EditXInput),
         (P: TypeOf(TRealInputLine);  YSize: False;  Proc: EditXInput),
         (P: TypeOf(TListBox);        YSize: True;   Proc: EditListBox),
         (P: TypeOf(TMemoEdit);       YSize: True;   Proc: EditMemoEdit),
         (P: Nil;                     YSize: False;  Proc: EditAnyView) );

