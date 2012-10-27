{***************************************************}
{                                                   }
{     TConstCollection implementiert eine           }
{     Sammlung von symbolischen Bezeichnern,        }
{     denen ein Wert zugeordnet. Dieser             }
{     symbolische Bezeichner kann dann z.B.         }
{     innerhalb einer TSmartInputLine eingegeben    }
{     werden                                        }
{                                                   }
{***************************************************}

UNIT ConstCol;

INTERFACE

USES
  Objects, Tools;

TYPE
  PConstRec = ^TConstRec;
  TConstRec = RECORD
    Value: Word;
    Key: String;
  END;

  PConstCollection = ^TConstCollection;
  TConstCollection = OBJECT(TStringCollection)
    PROCEDURE FreeItem(Item: Pointer); VIRTUAL;
    FUNCTION GetItem(VAR S: TStream): Pointer; VIRTUAL;
    PROCEDURE PutItem(VAR S: TStream; Item: Pointer); VIRTUAL;
    FUNCTION KeyOf(Item: Pointer): Pointer; VIRTUAL;
    PROCEDURE InsertConst(aConstant: String; aValue: Word);
    FUNCTION GetValue(aConstant: String): Word;
    FUNCTION Compare(Key1, Key2: Pointer): Integer; VIRTUAL;
  END;

CONST
  StreamName = 'CONST.STM';

  RConstCollection: TStreamRec = (
    ObjType: 20001;
    VmtLink: Ofs(TypeOf(TConstCollection)^);
    Load: @TConstCollection.Load;
    Store: @TConstCollection.Store);

VAR
  ConstCollection: PConstCollection;
    {
      Dies ist die nach auáen sichtbare Collection, die im
      Initialisierungsteil instantiiert und mit dem Inhalt
      des Streams "StreamName" besetzt wird.
    }

IMPLEMENTATION


{ TConstCollection }

FUNCTION TConstCollection.Compare(Key1, Key2: Pointer): Integer;
VAR S1, S2: String;
BEGIN
  S1 := UpStr(PString(Key1)^);
  S2 := UpStr(PString(Key2)^);
  Compare := TStringCollection.Compare(@S1, @S2);
END;

PROCEDURE TConstCollection.InsertConst(aConstant: String; aValue: Word);
VAR P: PConstRec;
BEGIN
  P := New(PConstRec);
  P^.Value := aValue;
  P^.Key := aConstant;
  Insert(P);
END;

FUNCTION TConstCollection.GetValue(aConstant: String): Word;
VAR T: TConstRec;
BEGIN
  T.Key := aConstant;
  IF IndexOf(@T) = -1 THEN GetValue := $FFFF
  ELSE GetValue := PConstRec(At(IndexOf(@T)))^.Value;
END;

PROCEDURE TConstCollection.FreeItem(Item: Pointer);
BEGIN
  FreeMem(Item, Length(PConstRec(Item)^.Key) +
    (SizeOf(TConstRec) - SizeOf(String) + 1));
END;

FUNCTION TConstCollection.GetItem(VAR S: TStream): Pointer;
VAR
  Pos: Longint;
  V: Word;
  L: Byte;
  P: PConstRec;
BEGIN
  S.Read(V, SizeOf(Word));
  S.Read(L, 1);
  GetMem(P, L + (SizeOf(TConstRec) - SizeOf(String) + 1));
  P^.Value := V;
  P^.Key[0] := Char(L);
  S.Read(P^.Key[1], L);
  GetItem := P;
END;

FUNCTION TConstCollection.KeyOf(Item: Pointer): Pointer; ASSEMBLER;
ASM
  MOV	AX,Item.Word[0]
  MOV	DX,Item.Word[2]
  ADD	AX,OFFSET TConstRec.Key
END;

PROCEDURE TConstCollection.PutItem(VAR S: TStream; Item: Pointer);
BEGIN
  S.Write(PConstRec(Item)^.Value, SizeOf(Word) + 1
         + Length(PConstRec(Item)^.Key));
END;

VAR
  ConstStream: TDosStream;

BEGIN
  RegisterType(RConstCollection);
  ConstStream.Init(StreamName, stOpenRead);
  ConstCollection := PConstCollection(ConstStream.Get);
  ConstStream.Done;
END.