Unit TvdvCmds;
Interface

CONST
    cmTile                   = 144;
    cmCasCade                = 145;
    edBase                   = 1200;
    WNo: INTEGER             = 1;


{ Hauptprogramm }
    cmScann                  = 101;
    cmDiskAutoScann          = 102;
    cmNachtrag               = 103;
    cmAufreum                = 104;
    cmLaden                  = 106;
    cmSpeichern              = 107;
    cmDatenAufgeben          = 108;
    cmNewFileName            = 109;
    cmChangeDir              = 110;
    cmZeigeDaten             = 112;
    cmSucheDaten             = 113;
    cmSucheMehrfache         = 114;
    cmMemory                 = 115;

    cmNextDatei              = 116;
    cmNextPfad               = 117;
    cmNextLabel              = 118;
    cmDateiMaske             = 119;
    cmEnde                   = 120;
    cmMaske                  = 121;
    cmIfCursor               = 122;
    cmProgramminfo           = 123;

    cmDEdit                  = 124;
    cmPruefen                = 125;

    cmHelpContents           = 126;
    cmImport                 = 147;
    cmIP                     = 155;
    cmIN                     = 156;
    cmColors                 = 157;
    cmWindowListItemSelected = 158;
	cmHeapUpdate			 = 159;

{ FileFind }

    Mehrfache: Boolean = False;
    cmShowPfad         = 130;
    cmDateiNext        = 131;
    cmDateiBack        = 132;
    cmSelectFile       = 133;
    cmFileFocused      = 134;
    cmDateiAnfang      = 135;
    cmLabelFocused     = 136;
    cmSelectLabel      = 137;




{ DEdit }

    cmDEDITBemEdit             = 138;
    cmDEDITSpeichern           = 139;
    cmDEDITLabeledit           = 140;
    cmPressRadioButton         = 141;
    cmDEDITDiskBereichLoeschen = 142;
    cmContents                 = 143;
    cmDEditSuchen              = 146;

(* Serienbrief: *)

    cmBriefeDrucken       = 148;

(* SerienBriefDialog: *)

    cmSerDialogDruckAlles = 149;
    cmSerDialogDruck      = 150;
    cmShowClip            = 151;
    cmEditorSave          = 152;
    cmEditorSaveAs        = 153;
    cmDateiPruefen        = 154;
Implementation

End.
