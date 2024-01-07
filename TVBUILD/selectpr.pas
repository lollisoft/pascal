PROCEDURE ProjektSelektierenDialog;  
  TYPE
    ProjektSelektierenData = RECORD
      ListPtr0: pCollection;
      Focused0: INTEGER;
      TextLen0: WORD;
      TextRec0: ARRAY [0..512] OF CHAR; 
    END;
  
  VAR  
    Dialog: pDialog;  
    Data:   ProjektSelektierenData;
    Code:   INTEGER;
    R:      tRect;  
BEGIN  
  Dialog := pDialog (RezFile.Get ('Projekt Selektieren'));  
 
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
