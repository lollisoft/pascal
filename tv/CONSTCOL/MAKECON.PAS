{****************************************************}
{     Erzeugt einen Stream zur Verwendung mit        }
{     dem Objekt TConstCollection aus dem Unit       }
{     ConstCol. Dabei wird eine Textdatei geparst,   }
{     die der Pascal-Konvention genÅgen mu·.         }
{****************************************************}

PROGRAM MakeCon;

USES
  ConstCol, Objects;

CONST
  ConstStreamName = 'CONST.STM';

VAR
  T: Text;
  S: String;
  ConstStream: TDosStream;
  CreateConstCollection: PConstCollection;

PROCEDURE ParseLineAndStore(S: String);
VAR
  V: Word;
  K: String;
  W: String;
  Error: Integer;
BEGIN
  K := Copy(S,1,Pos(' ',S)-1);
  WHILE S[1] <> '=' DO Delete(S,1,1);
  Delete(S,1,1);
  WHILE S[1] = ' ' DO Delete(S,1,1);
  W := Copy(S,1,Pos(';',S)-1);
  Val(W, V, Error);
  IF Error <> 0 THEN RunError(255);
  CreateConstCollection^.InsertConst(K, V);
END;


BEGIN
  CreateConstCollection := New(PConstCollection, Init(100,10));
  Assign(T, 'CONST.TXT');
  Reset(T);
  WHILE NOT EOF(T) DO BEGIN
    ReadLn(T, S);
    ParseLineAndStore(S);
  END;
  Close(T);
  ConstStream.Init(ConstStreamName, stCreate);
  ConstStream.Put(CreateConstCollection);
  ConstStream.Done;
  Dispose(CreateConstCollection, Done);
END.