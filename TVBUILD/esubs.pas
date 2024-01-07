  TYPE
    EditiereSubroutinenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      ListPtr1: pCollection;
      Focused1: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  pEditiereSubroutinenDialog = ^tEditiereSubroutinenDialog;
  tEditiereSubroutinenDialog = OBJECT (tDialog)
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
  END;
 
CONSTRUCTOR tEditiereSubroutinenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tDialog.Init (Bounds, aTitle);
  R.Assign (26, 3, 27, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (5, 3, 26, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (5, 2, 19, 3);
  Insert (New (pLabel, Init (R, 'Routinen ~i~n', View)));
 
  R.Assign (55, 3, 56, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (32, 3, 55, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (32, 2, 42, 3);
  Insert (New (pLabel, Init (R, '~R~outine', View)));
 
  R.Assign (55, 12, 56, 17);
  View := New (pScrollBar, Init  (R)); 
  Insert (View); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (5, 12, 55, 17);
  View := New (pMemo, Init (R, NIL, pScrollBar (View), NIL, 512));
  Insert (View);
  R.Assign (5, 11, 14, 12);
  Insert (New (pLabel, Init (R, 'In~h~alt', View)));
 
  R.Assign (58, 13, 70, 15);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (58, 15, 71, 17);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);
END;
 
PROCEDURE EditiereSubroutinenDialog;
  VAR
    R: tRect;
    Data: EditiereSubroutinenData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (3, 2, 77, 21);
  Dialog := New (pEditiereSubroutinenDialog, Init (R, 'Editiere Subroutinen'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (EditiereSubroutinenData), 0);
  Dialog^.SetData (Data); 
  Code := Desktop^.ExecView (Application^.ValidView (Dialog));
  IF Code <> cmCancel THEN BEGIN
    { cmCancel muss ev ersetzt werden }
    { Code auswerten }
    Dialog^.GetData (Data);
    { Data muss ausgewertet werden ! }
  END;
  IF Dialog <> NIL THEN 
    Dispose (Dialog, Done);
END;
