
with Ada.Text_IO;

with Templates_Parser;

procedure Nullset is
   use Ada;
   use Templates_Parser;
   S : Translate_Set := Null_Set;
begin
   Text_IO.Put_Line (Parse ("nullset.tmplt", S));
end Nullset;
