

  TYPE
    ProjektSelektierenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  pProjektSelektierenDialog = ^tProjektSelektierenDialog;
  tProjektSelektierenDialog = OBJECT (tDialog)
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
    Procedure HandleEvent(Var Event: TEvent); Virtual;
  END;
 
CONSTRUCTOR tProjektSelektierenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tDialog.Init (Bounds, aTitle);
  State := 2171;
  R.Assign (76, 2, 77, 9);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (3, 2, 76, 9);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (3, 1, 17, 2);
  Insert (New (pLabel, Init (R, '~M~ain - File', View)));
 
  R.Assign (76, 11, 77, 18);
  View := New (pScrollBar, Init  (R)); 
  Insert (View); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (3, 11, 76, 18);
  View := New (pMemo, Init (R, NIL, pScrollBar (View), NIL, 512));
  Insert (View);
  R.Assign (3, 10, 18, 11);
  Insert (New (pLabel, Init (R, '~B~eschreibung', View)));
 
  R.Assign (24, 19, 32, 21);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (41, 19, 54, 21);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);
END;
 
PROCEDURE ProjektSelektierenDialog;
  VAR
    R: tRect;
    Data: ProjektSelektierenData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pProjektSelektierenDialog, Init (R, 'Projekt Selektieren'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (ProjektSelektierenData), 0);
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
