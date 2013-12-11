
with Ada.Text_IO;
with Templates_Parser;

procedure Matrix is

   package TP renames Templates_Parser;

   use type TP.Tag;

   V1 : constant TP.Vector_Tag := +"A1.1" & "A1.2";
   V2 : constant TP.Vector_Tag := +"A2.1" & "A2.2";
   V3 : constant TP.Vector_Tag := +"A3.1" & "A3.2";

   M  : constant TP.Matrix_Tag := +V1 & V2 & V3;

begin
   Ada.Text_IO.Put_Line
     (TP.Parse ("matrix.tmplt",
                TP.Translate_Table'(1 => TP.Assoc ("MAT", M))));
end Matrix;
