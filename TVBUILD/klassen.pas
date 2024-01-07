Unit Klassen; (* Ein Editor fÅr Turbo - Pascal Klassen *)


Interface

Uses App, Objects, Dialogs, Views;


  TYPE
    KlassenEditorData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      ListPtr1: pCollection;
      Focused1: INTEGER;
      String0: STRING [80];
      Cluster0: WORD;
      Cluster1: WORD;
    END;
  
  pKlassenEditorDialog = ^tKlassenEditorDialog;
  tKlassenEditorDialog = OBJECT (tDialog)
    CONSTRUCTOR Init (VAR Bounds: tRect; aTitle: tTitleStr);
  END;


PROCEDURE KlassenEditorDialog;


Implementation

CONSTRUCTOR tKlassenEditorDialog.Init (VAR Bounds: tRect; aTitle: tTitleStr);
  VAR
    R: tRect;
    View: pView;
BEGIN
  tDialog.Init (Bounds, aTitle);
  R.Assign (37, 3, 38, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  Insert (View);
  R.Assign (4, 3, 37, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (4, 2, 15, 3);
  Insert (New (pLabel, Init (R, '~K~lassen:', View)));
 
  R.Assign (75, 3, 76, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  Insert (View);
  R.Assign (42, 3, 75, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (42, 2, 54, 3);
  Insert (New (pLabel, Init (R, '~M~ethoden:', View)));
 
  R.Assign (4, 12, 37, 13);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (4, 11, 14, 12);
  Insert (New (pLabel, Init (R, '~E~ingabe', View)));
  R.Assign (37, 12, 40, 13);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (42, 12, 59, 15);
  View := New (pRadioButtons,
               Init (R, 
                     NewSItem ('Constructor',
                     NewSItem ('Destructor',
                     NewSItem ('Member',
                     NIL)))));
  Insert (View);
  R.Assign (42, 11, 54, 12);
  Insert (New (pLabel, Init (R, 'Methodentyp', View)));
 
  R.Assign (63, 12, 76, 14);
  Insert (New (pButton, Init (R, '~H~ilfe', 9, 0)));
 
  R.Assign (4, 15, 19, 17);
  View := New (pRadioButtons,
               Init (R, 
                     NewSItem ('Kla~s~se',
                     NewSItem ('Me~t~hode',
                     NIL))));
  Insert (View);
  R.Assign (4, 14, 10, 15);
  Insert (New (pLabel, Init (R, 'A~r~t', View)));
 
  R.Assign (41, 16, 76, 18);
  Insert (New (pButton, Init (R, 'Methode ed~i~tieren', 0, 0)));
 
  R.Assign (3, 19, 12, 21);
  Insert (New (pButton, Init (R, '~N~eu', 0, 0)));
 
  R.Assign (14, 19, 27, 21);
  Insert (New (pButton, Init (R, '~L~îschen', 0, 0)));
 
  R.Assign (41, 19, 53, 21);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (63, 19, 76, 21);
  Insert (New (pButton, Init (R, '~A~bbruch', 11, 0)));
 
  SelectNext (FALSE);
END;


 
PROCEDURE KlassenEditorDialog;
  VAR
    R: tRect;
    Data: KlassenEditorData;
    Code: INTEGER;
    Dialog: pDialog;
BEGIN
  R.Assign (0, 0, 80, 23);
  Dialog := New (pKlassenEditorDialog, Init (R, 'Klassen - Editor'));
 
    { Datenrecord initialisieren ! }
  FillChar (Data, SizeOf (KlassenEditorData), 0);
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


End.