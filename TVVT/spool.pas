{***********************************************************
  File       : SPOOL.PAS
  Vers.      : 1.0
  Last Edit  : 10.12.88
  Autor      : G. Born
  File I/O   : INPUT, OUTPUT, FILE
  Progr. Spr.: Turbo Pascal 4.0 / 5.0
  Betr. Sys. : DOS 2.1 - 3.3
  Funktion: Das Programm dient zur Ausgabe von Listings mit
            Seitennummern, Datum, Dateinamen und einer wÑhl-
            baren Zeilennumerierung. Weiterhin wird nach n
            Zeilen ein Papiervorschub auf dem Drucker ausge-
            lîst. Es lassen sich beliebige Textdateien mit
            diesem Programm ausgeben.
 
  Aufruf:   SPOOL Filename /Optionen
            Optionen:  /N   Zeilennumerierung ein  [Aus]
                       /Lxx linker Rand            [ 0 ]
                       /Rxx rechter Rand           [75 ]
                       /Zxx Zeilen pro Seite       [60 ]

            Die Werte in [] geben die Standardeinstellung
            wieder. Wird das Programm ohne Parameter aufge-
            rufen, sind Dateiname und Optionen explizit ab-
            zufragen. Das Programm erzeugt eine Datei mit
            dem Namen der Eingabedatei und der Extension
            TMP. Anschlie·end wird diese Datei mit Hilfe des
            DOS Spoolers PRINT im Hintergrund ausgegeben.
**********************************************************}
USES Dos,Crt;
{$M $4000,100,100}                    {* reserviere Memory   *}

{* Variable definieren *}
CONST space = ' ';                    {* Leerzeichen          *}
      Formfeed = #12;                 {* Seitenvorschub       *}
      Pathlen = 65;                   {* LÑnge Filename       *}

      nummer : boolean = false;       {* keine Zeilennummern  *}
      zeile  : longint = 0;           {* Zeilennummer Listing *}
      seite  : integer = 1;           {* Seitennummer Listing *}
      maxzeile : integer = 60;        {* Zeilen pro Seite     *}
      rechts: integer = 80;           {* rechter Rand         *}
      links : integer = 5;            {* linker Rand          *}
      spalte : integer = 0;           {* EinrÅckung           *}

Type  Filetype = Text;                {* Textfile             *}
      Fname    = string[Pathlen];

Var   Filename1 : Fname;              {* Dateiname Eingabe    *}
      Filename2 : Fname;              {* Ausgabedatei         *}
      Eindev   : Filetype;            {* Handle               *}
      Outdev   : Filetype;            {* Output Handle        *}

      Options  : String[20];          {* String mit Optionen  *}
      Linie    : String[255];         {* Zeile mit String     *}

      Jahr, Monat, Tag, Wtag : Word;  {* Datum                *}
      szeile   : integer;             {* lokale Zeilenzahl    *}
      j,ptr1   : Integer;

{#########################################################}
{#                   Hilfsroutinen                       #}
{#########################################################}

function Open(var fp: Filetype; Name: Fname): boolean;
{---------------------------------------------------------}
{  ôffne Input Datei, falls vorhanden und gebe Status     }
{      zurÅck: open -> true, sonst -> false               }
{---------------------------------------------------------}
begin
 Assign(fp,Name);                     {* Zuweisung Dateiname   *}
 {$I-}
 Reset(fp);                           {* ôffne Datei           *}
 {$I+}
 Open := IOResult = 0;                {* Gebe Status zurÅck    *}
end { Open };

function Openout(var fp: Filetype; Name: Fname): boolean;
{---------------------------------------------------------}
{  ôffne Output Datei, falls vorhanden und gebe Status    }
{      zurÅck: open -> true, sonst -> false               }
{---------------------------------------------------------}
begin
 Assign(fp,Name);                     {* Zuweisung Dateiname   *}
 {$I-}
 Rewrite(fp);                         {* ôffne Datei           *}
 {$I+}
 Openout := IOResult = 0;             {* Gebe Status zurÅck    *}
end { Open };

Procedure skipblank;
{---------------------------------------------------------}
{  Das Modul ermittelt, wieviele Blanks am Anfang einer   }
{  eingelesenen Zeile stehen und erhîht jeweils die       }
{  Variable spalte                                        }
{---------------------------------------------------------}
Var i    : Integer;
    lang : Integer;

begin
 lang := Length(linie);               {* ZeilenlÑnge            *}
 i := 1;
 WHILE (i < lang) AND (linie[i] = space) DO
  begin
   i := i + 1;
   spalte := spalte + 1;
  end;
end;

Procedure getval (ptr: Integer; Var wert : integer);
{---------------------------------------------------------}
{ Decodiere den Eingabestring der Variablen options ab    }
{ dem Zeichen(ptr) in eine Integerzahl                    }
{---------------------------------------------------------}
 Var i      : Integer;
     status : Integer;
     lang   : Integer;
     tmp    : Integer;

begin
 i := 1;
 lang := Length(options);             {* StringlÑnge          *}
 WHILE ((ptr+i) <= lang) AND          {* suche das Ende der   *}
    (options[ptr+i] <> space) DO      {* Zahl                 *}
  i := i + 1;                         {* Ziffernzahl + 1      *}
 VAL(Copy(options,ptr,i),tmp,status); {* decodiere die Zahl   *}
 IF status = 0 THEN
  wert := tmp                         {* ok -> setze Wert     *}
 ELSE
  Writeln ('Fehler in Options ',options,' bei Zeichen ',status);

end; {* Getval *}


Procedure Parameter;
{---------------------------------------------------------}
{ Decodiere die Eingabeoptionen aus dem String options    }
{---------------------------------------------------------}
Var ptr : integer;

begin

 IF Pos('/N', Options) > 0 THEN       {* Zeilennumerierung   *}
  nummer := true;                     {* einschalten         *}

 ptr := Pos ('/Z=',Options);          {* suche /Z Switch     *}
 IF ptr > 0 THEN                      {* gefunden ?          *}
  begin                               {* ja -> decodiere Wert*}
   getval(ptr+3,maxzeile);            {* Zeilen / Seite      *}
  end;
  szeile := maxzeile + 1;             {* Kopf auf 1. Seite   *}

 ptr := Pos ('/L=',Options);          {* suche /L= Switch    *}
 IF ptr > 0 THEN                      {* gefunden ?          *}
  getval(ptr+3,links);                {* decodiere Eingabe   *}

 ptr := Pos ('/R=',Options);          {* suche /R= Switch    *}
 IF ptr > 0 THEN                      {* gefunden ?          *}
  getval(ptr+3,rechts);               {* decodiere Eingabe   *}

end; {* Parameter *}


Procedure pageskip;
{---------------------------------------------------------}
{  Seitenvorschub mit Kopf (Dateiname, Datum u. Seite     }
{---------------------------------------------------------}

begin

 IF szeile <= maxzeile THEN
  exit;                               {* kein Seitenwechsel   *}

 IF seite > 1 THEN                    {* 1. Seite k. Vorschub *}
  begin
   Writeln(outdev, Formfeed);         {* Vorschub 1 Seite     *}
   szeile := 3;                       {* Seitekopf = 3 Zeilen *}
  end
 ELSE
  begin                               {* Kopf Seite 1         *}
   Write (outdev,'SPOOL   ',options,'                             ');
   Writeln(outdev,' (c) Born Version 1.0');
   szeile := 4;                       {* Kopf = 4 Zeilen      *}
  end;

  Write(outdev,'Datei : ',Filename1,'          Datum : ');
  GetDate (Jahr,Monat,Tag,WTag);      {* lese Datum           *}
  Writeln (outdev,Tag,'.',Monat,'.',Jahr,'        Seite : ',seite);
  Writeln (outdev);

  seite := seite + 1;                 {* Seitennummer erhîhen *}

end; {* pageskip *}


Procedure ausgabe;
{---------------------------------------------------------}
{ Ausgabe der eingelesenen Zeile auf dem Printer.         }
{ rest gibt an, wieviele Zeichen pro Zeile gedruckt       }
{ werden dÅrfen. Ist die eingelesene Zeile lÑnger, wird   }
{ sie auf mehrere Ausgabezeilen aufgeteilt.               }
{---------------------------------------------------------}
Var i :    Integer;
    rest : Integer;                   {* LÑnge Ausgabezeile    *}

begin
 zeile := zeile + 1;                  {* Zeile im Listing      *}
 pageskip;                            {* Seitenvorschub ?      *}

 spalte := links;                     {* linker Rand           *}
 For i:= 1 To spalte do
  Write(outdev,space);                {* linker Rand           *}

 IF nummer THEN                       {* Zeilennumerierung ?   *}
 begin
  Write(outdev,zeile:5,': ');         {* Zeilennummer drucken  *}
  spalte := spalte + 7;               {* Spalte 7 setzen       *}
 end;

 rest := rechts - spalte;             {* RestzeilenlÑnge       *}
 skipblank;                           {* merke Blanks          *}
 Writeln(outdev,Copy(linie,1,rest));  {* Ausgabe Teilstring    *}
 linie := Copy(linie,rest+1, Length(linie));{* Reststring      *}
 szeile := szeile + 1;
 rest := rechts - spalte;             {* korrigiere Rest       *}

 WHILE Length(linie) > rest DO        {* String > Zeile        *}
  begin
   pageskip;                          {* Seitenvorschub ?      *}
   For i:= 1 To spalte do
    Write(outdev,space);              {* linker Rand setzen    *}

   Writeln (outdev,Copy(linie,1,rest));{* Teilstring ausgeben  *}
   linie := Copy(linie,rest + 1, Length(linie));{* Reststring  *}
   szeile := szeile + 1;              {* Zeile im Listing      *}
  end;

 IF Length(linie) > 0 THEN
  begin
   pageskip;                          {* Seitenvorschub ?      *}
   For i:= 1 To spalte do
    Write(outdev,space);              {* linker Rand           *}
   Writeln(outdev,linie);             {* Reststring ausgeben   *}
   szeile := szeile + 1;
  end;

end;

Procedure install;
{---------------------------------------------------------}
{ PrÅfe, ob der residente Teil von PRINT installiert ist. }
{ Falls nein, terminiere mit einer Fehlermeldung          }
{---------------------------------------------------------}
Var Register : Registers;

begin
 With Register do
  begin
   AX := $0100;                       {* Check Print Status   *}
   Intr ($2F,Register);               {* INT 2F Check Status  *}
   IF (AX AND $FF) <> $FF THEN        {* Print installiert ?  *}
    begin
     Write ('Bitte installieren Sie zuerst das MS-DOS');
     Writeln(' Programm PRINT');
     halt(1);
    end;
   end;
end { install };

Procedure spool (name : Fname);
{---------------------------------------------------------}
{ Starte die Ausgabe Åber einen Aufruf von Print          }
{---------------------------------------------------------}

begin
 EXEC ('\COMMAND.COM', '/C PRINT ' + name); {* aktiviere Print *}
end { spool };

{#########################################################}
{#                     Hauptprogramm                     #}
{#########################################################}

Var OldDir : String;

begin

 SetIntVec($24,SaveInt24);            {* restore DOS INT 24   *}
 install;                             {* PRINT installiert    *}

 Filename1 := '';                     {* Clear Variable       *}
 options  := '';                      {*        "             *}

IF ParamCount < 1 THEN                {* User Mode ?          *}
 begin
  ClrScr;                             {* clear Screen         *}
  Writeln ('S P O O L                            (c) Born Version 1.0');
  Writeln;
  Writeln('Optionen [ /L=05 linker Rand         /R=75  rechter Rand   ]');
  Writeln('         [ /Z=60 Zeilen pro Seite    /N  Zeilennumerierung ]');
  Writeln;
  Write('File     : ');
  Readln (Filename1);
  Write('Optionen : ');
  Readln (options);
  Writeln;
 end
ELSE                                  {* Kommando Mode       *}
 begin

  IF ParamCount < 2 THEN
   Filename1 := ParamStr(1)           {* nur Filename1 separ. *}
  ELSE
   begin
    Filename1 := ParamStr(1);         {* Filename1 separieren *}
    For j:= 2 TO ParamCount do
    options := options + ParamStr(j) + ' ';{* Optionen lesen *}
   end;
  end;

 parameter;                           {* Optionen decodieren *}

 IF (rechts < links) OR (maxzeile < 10) THEN
  begin                               {* sinnlose Einstellung*}
   Writeln;
   Writeln('Bitte Randeinstellung neu setzen');
   halt(1);                           {* Fehlerexit          *}
  end;

 IF Length(Filename1) < 1 THEN         {* Leereingabe ?       *}
  begin
   Writeln;
   Writeln ('Der Dateiname fehlt');
   halt(1);                           {* Fehler Exit         *}
  end;

                      {* ermittle den Namen der Ausgabedatei *}
 ptr1 := POS('.',Filename1);          {* suche . im Namen    *}
 IF ptr1 > 0 THEN
  Filename2 := Copy(Filename1,1,ptr1) + 'TMP' {* Endung TMP  *}
 ELSE
  Filename2 := Filename1 + '.TMP';

 Filename2 := 'C:\SpoolTmp\' + Filename2;

 GetDir(0, OldDir);

 {$I-}
 ChDir('C:\SpoolTmp');
 if IOResult <> 0 then
   MkDir('C:\SpoolTmp');
 {$I+}
 ChDir(OldDir);


{* prÅfe ob Datei vorhanden, nein -> exit     *}

 IF Not OPEN (Eindev, Filename1) THEN
  begin
   writeln('Die Datei ',Filename1,' kann nicht geîffnet werden ');
   halt(1);
  end;

 IF Not OpenOut (Outdev, Filename2) THEN
  begin
   writeln('Die Datei ',Filename2,' kann nicht geîffnet werden ');
   halt(1);
  end;

 Writeln ('Die Datei: ', Filename1,' wird bearbeitet');

 pageskip;                            {* Seitenkopf ausgeben  *}

 WHILE NOT (EOF(Eindev)) DO           {* Datei sequentiell lesen *}
  begin
   Readln (Eindev,linie);             {* lese Zeile           *}
   ausgabe;                           {* drucke Zeile         *}
  end;

 Writeln (outdev,Formfeed);           {* Seitenvorschub       *}

 CLOSE (Eindev);                      {* Datei schlie·en      *}
 CLOSE (outdev);                      {* Datei schlie·en      *}

 spool (Filename2);                   {* Spooler aktivieren   *}
 Writeln;
 Writeln ('Die Datei ',Filename2,' wird auf dem Drucker ausgegeben');
 Writeln('Spool beendet');

end.
