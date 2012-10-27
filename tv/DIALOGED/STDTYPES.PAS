UNIT StdTypes;

INTERFACE

USES
  Inputs,
  Editor, Dialogs, Objects, Views;

FUNCTION NewButton: PButton;

FUNCTION NewRadioButtons: PRadioButtons;

FUNCTION NewCheckBoxes: PCheckBoxes;

FUNCTION NewLabel: PLabel;

FUNCTION NewStatic: PStaticText;

FUNCTION NewInputLine: PInputLine;

FUNCTION NewValidInputLine: PValidInputLine;

FUNCTION NewHexInputLine: PHexInputLine;

FUNCTION NewValInputLine: PValInputLine;

FUNCTION NewIntInputLine: PIntInputLine;

FUNCTION NewFloatInputLine: PRealInputLine;

FUNCTION NewListBox: PListBox;

FUNCTION NewMemoField: PMemoEdit;

IMPLEMENTATION

FUNCTION NewButton: PButton;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,10,4);
    NewButton := New(PButton, Init(Rect, 'O~k~ay', cmOk, bfDefault));
  END;

FUNCTION NewRadioButtons: PRadioButtons;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,13,4);
    NewRadioButtons := New( PRadioButtons, Init(Rect,
                            NewSItem('Item1', NewSItem('Item2', Nil))));
  END;

FUNCTION NewCheckBoxes: PCheckBoxes;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,13,4);
    NewCheckBoxes := New( PCheckBoxes, Init(Rect,
                          NewSItem('Item1', NewSItem('Item2', Nil))));
  END;

FUNCTION NewLabel: PLabel;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+7,3);
    NewLabel := New( PLabel, Init(Rect, 'Label', Nil));
  END;

FUNCTION NewStatic: PStaticText;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewStatic := New( PStatictext, Init(Rect, 'Static'));
  END;

FUNCTION NewInputLine: PInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewInputLine := New( PInputLine, Init(Rect, 20));
  END;

FUNCTION NewValidInputLine: PValidInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewValidInputLine := New( PValidInputLine, Init(Rect, 2, 20));
  END;

FUNCTION NewValInputLine: PValInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewValInputLine := New( PValInputLine, Init(Rect, 2, 20));
  END;

FUNCTION NewIntInputLine: PIntInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewIntInputLine := New( PIntInputLine, Init(Rect, 2, 20));
  END;

FUNCTION NewFloatInputLine: PRealInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewFloatInputLine := New( PRealInputLine, Init(Rect, 2, 20));
  END;

FUNCTION NewHexInputLine: PHexInputLine;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewHexInputLine := New( PHexInputLine, Init(Rect, 2, 20));
  END;

FUNCTION NewListBox: PListBox;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewListBox := New( PListBox, Init(Rect, 1, Nil));
  END;

FUNCTION NewMemoField: PMemoEdit;
  VAR Rect: TRect;
  BEGIN
    Rect.Assign(2,2,2+8,3);
    NewMemoField := New( PMemoEdit, Init(Rect, Nil, Nil, Nil, 4711, 3838));
  END;

END.