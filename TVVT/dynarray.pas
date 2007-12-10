(*************************************************************************)
(*                                                                       *)
(*        Include_Datei zur Erzeugung von Integer/Real Arrays            *)
(*                     mit variablen Grenzen                             *)
(*                                                                       *)
(*************************************************************************)

(*=======================================================================*)
(* In Type_Def werden alle Typdefinitionen aufgefÅhrt, zu denen mit      *)
(* der Procedur New_Array ein Feld beliebiger Grî·e erzeugt werden soll. *)
(* In Type_Size mu· der zu diesem Type benîtigte Speicherbedarf(in Byte) *)
(* angegeben werden. (Reihenfolge beachten!)                             *)
(*=======================================================================*)

type type_def    = (integer_type,real_type,string_type);

const type_size : array [0..2] of integer = (    2   ,   6   ,  255  );

(*=======================================================================*)
(* Array- und Pointerdefinitionen zu den Typen aus Type-def.             *)
(*=======================================================================*)

type integer_array = array [1..1] of integer;

     real_array    = array [1..1] of real;

     string_array  = array [1..1] of string;

     Pointer_to_integer_array = ^integer_array;

     Pointer_to_real_array    = ^real_array;

     Pointer_to_string_array  = ^string_array;

(*=======================================================================*)
(* Mit den Proceduren New_Array bzw. Dispose_Array werden die Felder er- *)
(* zeugt bzw. gelîscht. Die Proceduren erhalten als Parameter einen      *)
(* Pointer, welcher auf das zu erzeugende Feld zeigen soll, sowie den    *)
(* Typ und die anzahl der Elemente.                                      *)
(* Achtung : Wird ein Feld mit Dispose_Array gelîscht, so mu· Array_Size *)
(*           genau den Wert haben, den Array_Size bei der Erzeugung mit  *)
(*           New_Array hatte!                                            *)
(*=======================================================================*)

procedure New_Array(var pointer;Element_type:type_def;Array_Size:integer);

var Any_pointer : ^integer absolute pointer;

begin
getmem(any_pointer,type_size[integer(element_type)]*array_size)
end;

procedure Dispose_Array(var pointer;Element_type:type_def;Array_Size:integer);

var Any_pointer : ^integer absolute pointer;

begin
freemem(any_pointer,type_size[integer(element_type)]*array_size)
end;

(*=======================================================================*)