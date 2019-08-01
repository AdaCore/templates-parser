
with Ada.Text_IO;
with Templates_Parser;

procedure Table_Align is

   use type Templates_Parser.Vector_Tag;

   Decls : constant Templates_Parser.Vector_Tag :=
             +"Count : constant Integer := 8;"
            & "Name : String := ""A Name"";"
            & "I : Integer;";

   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("DECLS", Decls));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_align.tmplt", Translations));
end Table_Align;
