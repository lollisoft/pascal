unit crypt;      (* Siehe TPTool - Heft 3/93 S. 77 *)

{$O+,F+}

interface

procedure EnCryptStruc(Var CryptPtr;
                           Size: Word; Key: Byte);

procedure DeCryptStruc(Var CryptPtr;
                           Size: Word; Key: Byte);


Implementation

type bytelist = array[1..$FFFF] of byte;

procedure EnCryptStruc;
var count   : Word;
    Startkey: Byte;

Begin
  StartKey := (Size mod $FF) xor key;

  ByteList(CryptPtr)[1] := ByteList(CryptPtr)[1] xor StartKey;

  For Count := 2 to Size do
    ByteList(CryptPtr)[Count] := ByteList(CryptPtr)[Count] xor
    Byte((ByteList(CryptPtr)[Count - 1]) + StartKey);
End;

procedure DeCryptStruc;
var count   : Word;
    Startkey: Byte;

Begin
  StartKey := (Size mod $FF) xor key;


  For Count := Size Downto 2 do
    ByteList(CryptPtr)[Count] := ByteList(CryptPtr)[Count] xor
    Byte((ByteList(CryptPtr)[Count - 1]) + StartKey);

  ByteList(CryptPtr)[1] := ByteList(CryptPtr)[1] xor StartKey;
End;


End.