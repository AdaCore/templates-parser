with Ada.Text_IO;
with Templates_Parser;

procedure Demo is
   Translations : constant Templates_Parser.Translate_Table :=
     (1 => Templates_Parser.Assoc ("NAME", "Ada"));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("demo.tmplt", Translations));
end Demo;
