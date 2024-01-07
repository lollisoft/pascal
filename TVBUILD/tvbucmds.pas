Unit TVBUCmds;

Interface

Uses Objects, Views;

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
  cmInfo                       = 120;
  cmArbeitsverz                = 121;

  cmMenusEditieren             = 122;

  (******************)

  cmIfCursor                   = 140;
  cmShowClip                   = 141;
  cmEditorSave                 = 142;
  cmEditorSaveAs               = 143;
  cmColors                     = 145;
  cmSaveDesktop                = 146;
  cmRetrieveDesktop            = 147;

  (******************)

  cmProjektLaden               = 150;
  cmProjektSpeichern           = 151;
  cmProjektDefinieren          = 152;
  cmProjektSelektieren         = 153;
  cmProjektModus               = 154;
  cmRunAppQuelle               = 156;



  cmReadQuelle                 = 157;
  cmAktualQuelle               = 158;
  cmWriteAppQuelle             = 159;
  cmCompileAppQuelle           = 160;
  cmSubsEditieren              = 161;


  cmUnitEditieren              = 162;

  cmUnitNeu                    = 165;
  cmUnitDelete                 = 166;


  cmSubListItemSelected        = 163;
  cmMainEditieren              = 164;
  cmVariablenEditieren         = 167;

  cmVarNeu                     = 168;
  cmVarDelete                  = 169;


  cmConstantenEditieren        = 170;
  cmEditAppQuelle              = 171;
  cmMakeAndRun                 = 172;

  cmNeu                        = 173;
  cmDelete                     = 174;
  cmZurueckAusSub              = 175;
  cmEditBlock                  = 176;
  cmParameter                  = 177;
  cmFuncTyp                    = 178;
  cmEdTextDoubleClicked        = 179;

  (***********)
  (* Classen *)
  (***********)


  cmClassenEditieren           = 190;

  cmHML                        = 180;

  cmHilfeEditLeereFenster      = 181;

  cmHilfeEditEinfuegen         = 182;



  cmModuleHirachie             = 182;
  cmModuleBAssistent           = 184;


  edBase                       = 1200;



  (************************)
  (* Stream Registration: *)
  (************************)

  StRegProjekt                 = 600;
  StRegProjektData             = 601;
  StRegProjektCollection       = 602;
  StRegHelpVerw                = 603;
  StRegHelpCommand             = 604;
  StRegMainCode                = 605;
  StRegUnitList                = 606;
  StRegTypeList                = 608;
  StRegVarList                 = 609;
  StRegUnitObj                 = 610;
  StRegVarTypList              = 611;
  StRegVarObj                  = 612;
  StRegMyList                  = 613;
  StRegConstObj                = 614;
  StRegConstList               = 615;
  StRegConstTypList            = 616;
  StRegProcedureObj            = 617;
  StRegProcedureList           = 618;



Type MyEditMessageRecord = Record
                             Zeile, Spalte: Integer;
                             Coll         : PCollection;
                             View         : PView;
                           End;


Implementation

End.