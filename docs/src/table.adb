
with Ada.Text_IO;
with Templates_Parser;

procedure Table is

   use type Templates_Parser.Vector_Tag;

   Names : constant Templates_Parser.Vector_Tag :=
             +"Bob" & "Bill" & "Toto";
   Ages  : constant Templates_Parser.Vector_Tag :=
             +"10" & "30" & "5";

   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("NAME", Names),
                     2 => Templates_Parser.Assoc ("AGE", Ages));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table.tmplt", Translations));
end Table;
