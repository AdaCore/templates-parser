
with Ada.Text_IO;
with Templates_Parser;

procedure Table_Inline is

   use type Templates_Parser.Vector_Tag;

   Colors : constant Templates_Parser.Vector_Tag :=
              +"Red" & "Green" & "Blue";

   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("COLORS", Colors));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_inline.tmplt", Translations));
end Table_Inline;
