  TYPE
    EditiereSubroutinenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      Cluster0: WORD;
      String0: STRING [80];
      String1: STRING [80];
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
  R.Assign (54, 3, 55, 10);
  View := New (pScrollBar, Init  (R)); 
  pScrollBar (View)^.SetRange (0, 0); 
  Insert (View);
  R.Assign (3, 3, 54, 10);
  View := New (pListBox, Init (R, 1, pScrollBar (View)));
  Insert (View);
  R.Assign (3, 2, 14, 3);
  Insert (New (pLabel, Init (R, '~R~outinen', View)));
 
  R.Assign (58, 3, 74, 5);
  View := New (pRadioButtons,
               Init (R, 
                     NewSItem ('~P~rozedur',
                     NewSItem ('~F~unktion',
                     NIL))));
  Insert (View);
  R.Assign (58, 2, 64, 3);
  Insert (New (pLabel, Init (R, '~A~rt', View)));
 
  R.Assign (57, 7, 77, 9);
  Insert (New (pButton, Init (R, '~Z~urÅck aus Sub', 0, 0)));
 
  R.Assign (57, 9, 77, 11);
  Insert (New (pButton, Init (R, '~E~dit Block', 0, 0)));
 
  R.Assign (3, 12, 54, 13);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (3, 11, 13, 12);
  Insert (New (pLabel, Init (R, 'Ro~u~tine', View)));
  R.Assign (54, 12, 57, 13);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (30, 14, 57, 16);
  Insert (New (pButton, Init (R, '~P~arameter der Routine', 0, 0)));
 
  R.Assign (64, 14, 77, 16);
  Insert (New (pButton, Init (R, '~H~ilfe', 9, 0)));
 
  R.Assign (3, 17, 21, 18);
  View := New (pInputLine, Init (R, 80));
  Insert (View);
  R.Assign (3, 16, 21, 17);
  Insert (New (pLabel, Init (R, '~F~unktions - Typ', View)));
  R.Assign (21, 17, 24, 18);
  Insert (New (pHistory, Init  (R, pInputLine (View), 0)));
 
  R.Assign (33, 17, 57, 19);
  Insert (New (pButton, Init (R, 'T~y~pen der Funktion', 0, 0)));
 
  R.Assign (2, 20, 11, 22);
  Insert (New (pButton, Init (R, '~N~eu', 0, 0)));
 
  R.Assign (12, 20, 25, 22);
  Insert (New (pButton, Init (R, '~L~îschen', 0, 0)));
 
  R.Assign (42, 20, 57, 22);
  Insert (New (pButton, Init (R, '~O~K', 10, 1)));
 
  R.Assign (64, 20, 77, 22);
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
  R.Assign (0, 0, 80, 23);
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
