
with Ada.Text_IO;
with Templates_Parser;

procedure If_Inline is

   use type Templates_Parser.Vector_Tag;

   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("COND", True));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("if_inline.tmplt", Translations));
end If_Inline;
