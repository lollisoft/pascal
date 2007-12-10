Unit TVVTCmds;

Interface

const
  WNo: Integer                 = 1;

  cmOpen                       = 100;
  cmNew                        = 101;
  cmChangeDir                  = 102;
  cmDosShell                   = 103;
  cmCalculator                 = 104;

  (* Vokabeltrainer *)

  cmVVE                        = 105;
  cmVVL                        = 106;
  cmVVDD                       = 107;
  cmVVESpeichern               = 108;
  cmVVA                        = 109;
  cmVVS                        = 110;
  cmVVI                        = 111;
  cmVVX                        = 112;
  cmVVDE                       = 113;
  cmWortOk                     = 114;
  cmVVV                        = 115;
  cmVVDA                       = 116;
  cmVVEEntfernen               = 117;
  cmVVD                        = 118;
  cmAuto                       = 119;

  (******************)

  cmIfCursor                   = 140;
  cmShowClip                   = 141;
  cmEditorSave                 = 142;
  cmEditorSaveAs               = 143;
  cmColors                     = 145;
  cmSaveDesktop                = 146;
  cmRetrieveDesktop            = 147;

  edBase                       = 1200;


Implementation

End.