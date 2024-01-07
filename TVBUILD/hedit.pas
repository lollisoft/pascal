  TYPE
    HilfeKontextEditierenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  pHilfeKontextEditierenDialog = ^tHilfeKontextEditierenDialog;
  tHilfeKontextEditierenDialog = OBJECT (tDialog)
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
  END;
 
CONSTRUCTOR tHilfeKontextEditierenDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tDialog.Init (Bounds, aTitle);
  R.Assign (45, 3, 46, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  R.Assign (4, 3, 45, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (4, 2, 14, 3);
  Insert (New (pLabel, Init (R, '~B~efehle', View)));
 
  R.Assign (49, 3, 76, 5);
  Insert (New (pButton, Init (R, '~L~eere Hilfe Fenster ?', 0, 0)));
 
  R.Assign (49, 6, 76, 8);
  Insert (New (pButton, Init (R, ' ~E~infÅgen in Projekt ', 0, 0)));
 
  R.Assign (49, 9, 57, 11);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (63, 9, 76, 11);
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
 
PROCEDURE HilfeKontextEditierenDialog;
  VAR
    R: tRect;
    Data: HilfeKontextEditierenData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pHilfeKontextEditierenDialog, Init (R, 'Hilfe - Kontext Editieren'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (HilfeKontextEditierenData), 0);
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
