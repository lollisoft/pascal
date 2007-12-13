{************************************************}
{                                                }
{   Funktioniert „hnlich wie TResourceFile,      }
{   allerdings versucht TIndexFile die           }
{   entstehenden L”cher wieder aufzufllen.      }
{   Dazu wird eine bestimmte Granularit„t bei    }
{   der Speicherplatzvergabe verwendet.          }
{         corrected: FE/4.4.91                   }
{************************************************}

{$B-}

UNIT IdxFile;

INTERFACE

USES
  Objects;

TYPE
  {
   dieser Stream wird zur Gr”áenbestimmung verwendet und schickt
   die ihm bergebenen Objekte einfach in's "Nirwana"
  }
  PNullStream = ^TNullStream;
  TNullStream = OBJECT(TStream)
    Size: LongInt;
    FUNCTION GetSize: LongInt; VIRTUAL;
    PROCEDURE Write(VAR Buf; Count: Word); VIRTUAL;
    FUNCTION GetPos: LongInt; VIRTUAL;
  END;

  PIndexRec = ^TIndexRec;
  TIndexRec = RECORD
    Pos: LongInt;
    Len: Word;
    Unused: Boolean;
    Key: STRING;
  END;

  PIndexCollection = ^TIndexCollection;
  TIndexCollection = OBJECT(TStringCollection)
    PROCEDURE FreeItem(Item: Pointer); VIRTUAL;
    FUNCTION GetItem(VAR S: TStream): Pointer; VIRTUAL;
    PROCEDURE PutItem(VAR S: TStream; Item: Pointer); VIRTUAL;
    FUNCTION KeyOf(Item: Pointer): Pointer; VIRTUAL;
    FUNCTION BlockAvail(Size: Word; VAR P: PIndexRec): Boolean;
    FUNCTION OrdinalIndex(Item: Pointer): Integer;
  END;

  PIndexFile = ^TIndexFile;
  TIndexFile = OBJECT(TObject)
    Stream: PStream;
    Index: TIndexCollection;
    Modified: Boolean;
    CONSTRUCTOR Init(AStream: PStream; AClusterSize: Word);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION Count: Integer;
    PROCEDURE Flush;
    PROCEDURE Delete(Key: STRING);
    FUNCTION Get(Key: STRING): PObject;
    PROCEDURE Put(Item: PObject; Key: STRING);
    FUNCTION KeyAt(I: Integer): STRING;
  PRIVATE
    Signature: LongInt;
    IndexPos: LongInt;
    ClusterSize: Word;
  END;


CONST
  RIndexCollection: TStreamRec = (
    ObjType: 20000;
    VmtLink: Ofs(TypeOf(TIndexCollection)^);
    Load:    @TIndexCollection.Load;
    Store:   @TIndexCollection.Store);


IMPLEMENTATION


{ TNullStream }

FUNCTION TNullStream.GetSize: LongInt;
BEGIN
  GetSize := Size;
END;

PROCEDURE TNullStream.Write(VAR Buf; Count: Word);
BEGIN
  Inc(Size, Count);
END;

FUNCTION TNullStream.GetPos: LongInt;
BEGIN
  GetPos := 0;
END;


{ TIndexCollection }

FUNCTION TIndexCollection.BlockAvail(Size: Word; VAR P: PIndexRec): Boolean;
VAR
  Temp: PIndexRec;

FUNCTION FitsBest(P: PIndexRec): Boolean; FAR;
BEGIN
  FitsBest := (P^.Len = Size) AND P^.Unused;
END;

FUNCTION Fits(P: PIndexRec): Boolean; FAR;
BEGIN
  Fits := (P^.Len > Size) AND P^.Unused;
END;

BEGIN
  Temp := FirstThat(@FitsBest);
  IF Temp = NIL THEN
    Temp := FirstThat(@Fits);
  IF Temp <> NIL THEN BEGIN
    BlockAvail := True;
    P := Temp;
  END
  ELSE BlockAvail := False;
END;

PROCEDURE TIndexCollection.FreeItem(Item: Pointer);
BEGIN
  FreeMem(Item, Length(PIndexRec(Item)^.Key) +
    (SizeOf(TIndexRec) - SizeOf(STRING) + 1));
END;

FUNCTION TIndexCollection.GetItem(VAR S: TStream): Pointer;
VAR
  Pos: LongInt;
  W: Word;
  B: Boolean;
  L: Byte;
  P: PIndexRec;
BEGIN
  S.Read(Pos, SizeOf(LongInt));
  S.Read(W, SizeOf(Word));
  S.Read(B, SizeOf(Boolean));
  S.Read(L, 1);
  GetMem(P, L + (SizeOf(TIndexRec) - SizeOf(STRING) + 1));
  P^.Pos := Pos;
  P^.Len := W;
  P^.Unused := B;
  P^.Key[0] := Char(L);
  S.Read(P^.Key[1], L);
  GetItem := P;
END;

FUNCTION TIndexCollection.KeyOf(Item: Pointer): Pointer; ASSEMBLER;
ASM
  MOV	AX,Item.Word[0]
  MOV	DX,Item.Word[2]
  ADD	AX,OFFSET TIndexRec.Key
END;

PROCEDURE TIndexCollection.PutItem(VAR S: TStream; Item: Pointer);
BEGIN
  S.Write(PIndexRec(Item)^.Pos, SizeOf(LongInt) + SizeOf(Word)
          + SizeOf(Boolean) + 1 + Length(PIndexRec(Item)^.Key));
END;

FUNCTION TIndexCollection.OrdinalIndex(Item: Pointer): Integer;
BEGIN
  OrdinalIndex := TCollection.IndexOf(Item);
END;


{ TIndexFile }

CONSTRUCTOR TIndexFile.Init(AStream: PStream; AClusterSize: Word);
CONST
  RIndexFileMagic = $45464641; { 'AFFE' }
BEGIN
  TObject.Init;
  Stream := AStream;
  ClusterSize := AClusterSize;
  Stream^.Read(Signature, SizeOf(LongInt));
  IF Stream^.Status <> 0 THEN Stream^.Reset;
  IF Signature = RIndexFileMagic THEN BEGIN
    Stream^.Seek(SizeOf(LongInt));
    Stream^.Read(IndexPos, SizeOf(LongInt));
    Stream^.Seek(IndexPos);
    Index.Load(Stream^);
  END ELSE BEGIN
    Signature := RIndexFileMagic;
    IndexPos := SizeOf(LongInt) * 2;
    Index.Init(0, 8);
  END;
END;

DESTRUCTOR TIndexFile.Done;
BEGIN
  Flush;
  Index.Done;
  Dispose(Stream, Done);
END;

FUNCTION TIndexFile.Count: Integer;
VAR
  I, L: Integer;
BEGIN
  I := 0;
  FOR L := 0 TO Index.Count-1 DO
    IF PIndexRec(Index.At(L))^.Unused THEN Inc(I);
  Count := Index.Count - I;
END;

FUNCTION TIndexFile.KeyAt(I: Integer): STRING;
VAR
  RealIndex, K: Integer;
BEGIN
  IF I > Index.Count-1 THEN RunError(213);
  RealIndex := 0;
  K := 0;
  WHILE K <= I DO BEGIN
    IF NOT PIndexRec(Index.At(RealIndex))^.Unused THEN
       Inc(K);
    Inc(RealIndex);
  END;
  Dec(RealIndex);
  KeyAt := PIndexRec(Index.At(RealIndex))^.Key;
END;

PROCEDURE TIndexFile.Delete(Key: STRING);
VAR
  I: Integer;
BEGIN
  IF Index.Search(@Key, I) THEN BEGIN
    PIndexRec(Index.At(I))^.Unused := True;
{ wir tun das nicht mehr: PIndexRec(Index.At(I))^.Key := ''; }
    Modified := True;
  END;
END;

FUNCTION TIndexFile.Get(Key: STRING): PObject;
VAR
  I: Integer;
BEGIN
  IF NOT Index.Search(@Key, I) THEN
    Get := NIL
  ELSE BEGIN
    Stream^.Seek(PIndexRec(Index.At(I))^.Pos);
    Get := Stream^.Get;
  END;
END;

PROCEDURE TIndexFile.Put(Item: PObject; Key: STRING);
VAR
  NullStream: PNullStream;
  SizeOfObject,
  RequestedSize: Word;
  P: PIndexRec;
  NewRecPtr: Pointer;
  KeyFound: Boolean;
  I: Integer;
BEGIN
  NullStream := New(PNullStream, Init);
  NullStream^.Put(Item);
  SizeOfObject := NullStream^.GetSize;
  Dispose(NullStream, Done);
  RequestedSize := SizeOfObject;
  WHILE RequestedSize MOD ClusterSize <> 0 DO Inc(RequestedSize);
  KeyFound := Index.Search(@Key, I);
  IF KeyFound AND (PIndexRec(Index.At(I))^.Len >= RequestedSize) THEN
    P := Index.At(I)
  ELSE BEGIN
    IF KeyFound THEN BEGIN
        {  PIndexRec(Index.At(I))^.Key := ''; wir prfen nur noch UnUsed!}
      PIndexRec(Index.At(I))^.Unused := True;
      { als unbenutzt markieren }
     END;
    IF NOT Index.BlockAvail(RequestedSize, P) THEN BEGIN
      GetMem(P, Length(Key) + (SizeOf(TIndexRec) - SizeOf(STRING) + 1));
      P^.Key := Key;
      P^.Pos := IndexPos;
      P^.Len := RequestedSize;
      Index.AtInsert(I, P);
    END
    ELSE BEGIN
      GetMem(NewRecPtr, Length(Key) + (SizeOf(TIndexRec) - SizeOf(STRING) + 1));
      PIndexRec(NewRecPtr)^.Pos := P^.Pos;
      PIndexRec(NewRecPtr)^.Len := P^.Len;
      PIndexRec(NewRecPtr)^.Key := Key;
      Index.Items^[Index.OrdinalIndex(P)] := NewRecPtr;
      FreeMem(P, Length(P^.Key) + (SizeOf(TIndexRec) - SizeOf(STRING) + 1));
      P := NewRecPtr;
    END
  END;
  Stream^.Seek(P^.Pos);
  P^.Unused := False;
  Stream^.Put(Item);
  Stream^.Seek(Stream^.GetPos + RequestedSize-SizeOfObject); { Lcke fllen }
  IF Stream^.GetPos > IndexPos THEN IndexPos := Stream^.GetPos;
  Modified := True;
END;

PROCEDURE TIndexFile.Flush;
BEGIN
  IF Modified THEN BEGIN
    Stream^.Seek(IndexPos);
    Index.Store(Stream^);
    Stream^.Seek(0);
    Stream^.Write(Signature, SizeOf(LongInt));
    Stream^.Write(IndexPos, SizeOf(LongInt));
    Stream^.Flush;
    Modified := False;
  END;
END;

BEGIN
  RegisterType(RIndexCollection);
END.