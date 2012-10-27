{$N+}
UNIT StrTools;

INTERFACE

FUNCTION Chars               { Liefert eine mit                             }
  ( C: CHAR;                 { .. diesem Zeichen gefÅllte auf               }
    Count: INTEGER)          { .. diese LÑnge begrenzte                     }                               
  : STRING;                  { .. Zeichenkette zurÅck                       }

FUNCTION DOUBLEtoSTRING      { Wandelt                                      }
  ( Value: DOUBLE;           { .. diesen Wert                               }
    Width,                   { .. auf diese LÑnge                           }
    Decimals: INTEGER)       { .. mit diesen Nachkommastellen in            }
  : STRING;                  { .. eine Zeichenkette um.                     }

FUNCTION DownCase            { Liefert dieses                               }
  ( C: CHAR)                 { .. Zeichen in Kleinbuchstaben (incl. éôö)    }
  : CHAR;                    { .. als Zeichen zurÅck                        }

PROCEDURE DownString         { Wandelt Gro·buchstaben in Kleinbuchstaben    }
  ( VAR S: STRING );         { .. in dieser Zeichenkette um (incl éôö)      }

FUNCTION DownStr             { Liefert in Klienbuchstaben gewandelt         }
  ( S: STRING)               { .. diese Zeichenkette                        }
  : STRING;                  { .. als Zeichenkette zurÅck                   }

FUNCTION IsInside            { PrÅft ob                                     }
  ( Item,                    { .. diese Zeichenkette in                     }
    Target: STRING;           { .. dieser Zeichenkette enthalten ist         }
    IgnoreCase : BOOLEAN)    { ( True: gro·/klein ist egal )                }
  : BOOLEAN;                 { True wenn enthalten, sonst False             }

FUNCTION LONGINTtoSTRING     { Wandelt                                      }
  ( Value: LONGINT;          { .. diesen Wert                               }
    Width,                   { .. auf diese LÑnge                           }
    Base: INTEGER)           { .. mit dieser Zahlenbasis in                 }
  : STRING;                  { .. eine Zeichenkette um.                     }

PROCEDURE Replace            { Ersetzt                                      }
  ( OldPart,                 { .. diese Zeichenkette                        }
    NewPart : STRING;        { .. durch diese Zeichenkette                  }
    VAR Line : STRING;       { .. in dieser Zeichenkette                    }
    IgnoreCase : BOOLEAN;    { ( True: gro·/klein ist egal )                }
    Replacements,            { .. (Anzahl der Ersetzungen)                  }
    Skipped: INTEGER);       { .. (Anzahl der Auslassungen)                 }


CONST
  FromStart = 1;
  FromEnd   = 2;

FUNCTION Trim                { Entferne alle Leerzeichen                    }
  ( Line: STRING;            { .. aus dieser Zeile                          }
    Mode: BYTE )             { (FromStart = fÅhrende/ FromEnd = vom Ende)   }
  : STRING;                  { .. Ergenis ist eine Zeichenkette             }

FUNCTION ToBit               { Liefert die BinÑrdarstellung                 }
  ( Value : LONGINT;         { .. dieses Wertes                             }
    Width : INTEGER)         { .. mit dieser LÑnge                          }
  : STRING;                  { .. als Zeichenkette                          }

FUNCTION ToHex               { Liefert die Hexdarstellung                   }
  ( Value : LONGINT;         { .. dieses Wertes                             }
    Width : INTEGER)         { .. mit dieser LÑnge                          }
  : STRING;                  { .. als Zeichenkette                          }

VAR wildCard: STRING[2];

IMPLEMENTATION

FUNCTION Chars               { Liefert eine mit                             }
  ( C: CHAR;                 { .. diesem Zeichen gefÅllte auf               }
    Count: INTEGER)          { .. diese LÑnge begrenzte                     }                               
  : STRING;                  { .. Zeichenkette zurÅck                       }
  VAR S: STRING;
  BEGIN
    IF (Count > 0) AND (Count < SizeOf(S))
     THEN BEGIN
      S[0] := Chr(Count);
      FillChar(S[1],Count,C);
      Chars := S;
     END
     ELSE Chars := '';
  END;

FUNCTION DOUBLEtoSTRING      { Wandelt                                      }
  ( Value: DOUBLE;           { .. diesen Wert                               }
    Width,                   { .. auf diese LÑnge                           }
    Decimals: INTEGER)       { .. mit diesen Nachkommastellen in            }
  : STRING;                  { .. eine Zeichenkette um.                     }
  VAR S: STRING;
  BEGIN
    IF Width = 0
     THEN Str(Value,S)
     ELSE Str(Value:Width:Decimals, S);
    DOUBLEtoSTRING := S;
  END;

FUNCTION DownCase            { Liefert dieses                               }
  ( C: CHAR)                 { .. Zeichen in Kleinbuchstaben (incl. éôö)    }
  : CHAR;                    { .. als Zeichen zurÅck                        }
ASSEMBLER;
  ASM
    MOV AL,C
    CMP AL,'A'
    JB  @@4                  { No conversion below 'A'                      }
    CMP AL,'Z'
    JBE @@3                  { Conversion between 'A' and 'Z'               }
    CMP AL,'é'
    JNZ @@1
    MOV AL,'Ñ'               { Conversion if 'é'                            }
  @@1:
    CMP AL,'ô'
    JNZ @@2
    MOV AL,'î'               { Conversion if 'ô'                            }
  @@2:
    CMP AL,'ö'
    JNZ @@4                  { No conversion at all                         }
    MOV AL,'Å'               { Conversion if 'ö'                            }
    JMP @@4
  @@3:
    ADD AL,$20
  @@4:
END;

PROCEDURE DownString         { Wandelt Gro·buchstaben in Kleinbuchstaben    }
  ( VAR S: STRING );         { .. in dieser Zeichenkette um (incl éôö)      }
  VAR I: INTEGER;
  BEGIN
    FOR I := 1 TO Length(S) DO S[I] := DownCase(S[I]);
  END;

FUNCTION DownStr             { Liefert in Klienbuchstaben gewandelt         }
  ( S: STRING)               { .. diese Zeichenkette                        }
  : STRING;                  { .. als Zeichenkette zurÅck                   }
  BEGIN
    DownString(S);
    DownStr := S;
  END;

FUNCTION LONGINTtoSTRING     { Wandelt                                      }
  ( Value : LONGINT;         { .. diesen Wert                               }
    Width,                   { .. auf diese LÑnge                           }
    Base: INTEGER)           { .. mit dieser Zahlenbasis in                 }
  : STRING;                  { .. eine Zeichenkette um.                     }
  CONST HexStr: STRING[16] = '0123456789ABCDEF';
  VAR
    S: STRING;
  BEGIN
    S := '';
    WHILE Width > 0 DO BEGIN
      S := HexStr[(Value MOD Base) + 1] + S;
      Value := Value DIV Base;
      Dec(Width);
    END;
    LONGINTtoSTRING := S;
  END;

FUNCTION IsInside            { PrÅft ob                                     }
  ( Item,                    { .. diese Zeichenkette in                     }
    Target : STRING;         { .. dieser Zeichenkette enthalten ist         }
    IgnoreCase : BOOLEAN)    { ( True: gro·/klein ist egal )                }
  : BOOLEAN;                 { True wenn enthalten, sonst False             }
  VAR ScanType: BYTE;
  BEGIN
    IF IgnoreCase THEN BEGIN
      DownString(Item);
      DownString(Target);
    END;
    CASE Pos(wildcard, Item) OF
      0    : ScanType := 0;
      1    : BEGIN
        Delete(Item, 1, Length(wildcard));
        IF Pos(wildcard,Item) > 0
         THEN BEGIN
          ScanType := 3;
          Delete(Item, Pos(wildcard, Item), 255);
         END
         ELSE ScanType := 2;
      END;
      ELSE BEGIN
        ScanType := 1;
        Delete(Item, Pos(wildcard, Item), 255);
      END;
    END;
    CASE ScanType OF
      0: IsInside := Item = Target;
      1: IsInside := (Pos(Item, Target) = 1);
      2: IsInside := (Pos(Item, Target) > 0) AND
                     (Pos(Item, Target) = Length(Target)-Length(Item)+1);
      3: IsInside := (Pos(Item, Target) > 0) AND
                     (Pos(Item, Target) < Length(Target)-Length(Item)+1);
    END;
  END;

PROCEDURE Replace            { Ersetzt                                      }
  ( OldPart,                 { .. diese Zeichenkette                        }
    NewPart : STRING;        { .. durch diese Zeichenkette                  }
    VAR Line : STRING;       { .. in dieser Zeichenkette                    }
    IgnoreCase : BOOLEAN;    { ( True: gro·/klein ist egal )                }
    Replacements,            { .. (Anzahl der Ersetzungen)                  }
    Skipped: INTEGER);       { .. (Anzahl der Auslassungen)                 }
  VAR H,LineCopy: STRING;
  BEGIN
    H := '';
    LineCopy := Line;
    IF IgnoreCase THEN BEGIN
      DownString(LineCopy);
      DownString(OldPart);
    END;
    WHILE (Pos(OldPart, LineCopy) > 0) AND (Skipped > 0) DO BEGIN
      H := H + Copy(Line, 1, Pos(OldPart, LineCopy) - 1 + Length(OldPart));
      Delete(Line, 1, Pos(OldPart, LineCopy) - 1 + Length(OldPart));
      Delete(LineCopy, 1, Pos(OldPart, LineCopy) - 1 + Length(OldPart));
      Dec(Skipped);
    END;
    WHILE (Pos(OldPart, LineCopy) > 0) AND (Replacements > 0) DO BEGIN
      H := H + Copy(Line, 1, Pos(OldPart, LineCopy)-1) + NewPart;
      Delete(Line, 1, Pos(OldPart, LineCopy) - 1 + Length(OldPart));
      Delete(LineCopy, 1, Pos(OldPart, LineCopy) - 1 + Length(OldPart));
      Inc(Replacements);
    END;
    Line := H + Line;
  END;

FUNCTION Trim                { Entferne alle Leerzeichen                    }
  ( Line: STRING;            { .. aus dieser Zeile                          }
    Mode: BYTE )             { (FromStart = fÅhrende/ FromEnd = vom Ende)   }
  : STRING;                  { .. Ergenis ist eine Zeichenkette             }
  VAR Len: BYTE ABSOLUTE Line;
  BEGIN
    IF Mode AND FromStart <> 0
     THEN WHILE (Len > 0) AND (Line[1] = ' ') DO Delete(Line, 1, 1);
    IF Mode AND FromEnd <> 0
     THEN WHILE (Len > 0) AND (Line[Len] = ' ') DO Dec(Len);
    Trim := Line;
  END;

FUNCTION ToBit               { Liefert die BinÑrdarstellung                 }
  ( Value : LONGINT;         { .. dieses Wertes                             }
    Width : INTEGER)         { .. mit dieser LÑnge                          }
  : STRING;                  { .. als Zeichenkette                          }
  BEGIN
    ToBit := LONGINTToSTRING( Value, Width, 2 );
  END;

FUNCTION ToHex               { Liefert die Hexdarstellung                   }
  ( Value : LONGINT;         { .. dieses Wertes                             }
    Width : INTEGER)         { .. mit dieser LÑnge                          }
  : STRING;                  { .. als Zeichenkette                          }
  BEGIN
    ToHex := LONGINTToSTRING( Value, Width, 16);
  END;

PROCEDURE InitUnit;
  BEGIN
    WildCard := '..';
  END;

BEGIN
  InitUnit;
END.
