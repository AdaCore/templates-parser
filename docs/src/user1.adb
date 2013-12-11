
with Ada.Text_IO;
with Templates_Parser;

procedure User1 is
   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("USER", True));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("user.tmplt", Translations));
end User1;
