PROCEDURE EditorDialog;
  TYPE
    EditorData = RECORD
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  VAR
    R: tRect;
    View: pView;
    Data: EditorData;
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
  View := New (pMemo, Init (R, NIL, pScrollBar (View), NIL, 512));
  Dialog^.Insert (View);
  R.Assign (6, 2, 24, 3);
  Dialog^.Insert (New (pLabel, Init (R, '~P~rogramm - Text', View)));
 
  R.Assign (18, 13, 28, 15);
  Dialog^.Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (46, 13, 59, 15);
  Dialog^.Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  Dialog^.SelectNext (FALSE);
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (EditorData), 0);
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
