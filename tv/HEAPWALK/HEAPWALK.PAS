UNIT HeapWalk;

{
 implements a heap walker to check integrity of free list
}

INTERFACE

TYPE
  PFreeRec = ^TFreeRec;
  TFreeRec = RECORD
    Next: PFreeRec;
    Size: Pointer;
  END;

  PtrRec = RECORD
    Lo, Hi: Word;
  END;

PROCEDURE CheckHeap;

FUNCTION HeapSpace: LongInt;

FUNCTION BlockSize(Size: Pointer): LongInt;

IMPLEMENTATION


FUNCTION PtrCompare(P1, P2: Pointer): Integer;
BEGIN
  IF P1 = P2 THEN BEGIN
    PtrCompare := 0;
    Exit;
  END;
  IF (LongInt(PtrRec(P1).Hi) * 16 + PtrRec(P1).Lo) -
     (LongInt(PtrRec(P2).Hi) * 16 + PtrRec(P2).Lo) > 0
  THEN PtrCompare := +1
  ELSE PtrCompare := -1;
END;


FUNCTION BlockSize(Size: Pointer): LongInt;
BEGIN
  BlockSize := LongInt(PtrRec(Size).Hi) * 16 + PtrRec(Size).Lo;
END;

FUNCTION HeapSpace: LongInt;
BEGIN
  HeapSpace := (LongInt(PtrRec(HeapPtr).Hi) SHL 4 + PtrRec(HeapPtr).Lo) -
               (LongInt(PtrRec(HeapOrg).Hi) SHL 4 + PtrRec(HeapOrg).Lo);
END;

PROCEDURE CheckHeap;
CONST
  MaxFreeBlocks = 5000;
VAR
  P: PFreeRec;
  Count: Word;
  Size: LongInt;
BEGIN
  P := FreeList;
  Count := 0;
  Size := 0;
  WHILE (PtrCompare(P, HeapPtr) = -1) AND
        (Count < MaxFreeBlocks) AND
        (PtrRec(P).Lo <= 8) AND
        (PtrCompare(P, P^.Next) = -1) DO
  BEGIN
    Size := Size + BlockSize(P^.Size);
    P := P^.Next;
    Inc(Count);
  END;
  IF (P <> HeapPtr) OR (Size > HeapSpace) THEN RunError(255);
END;

END.