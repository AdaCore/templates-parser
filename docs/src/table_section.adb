
with Ada.Text_IO;
with Templates_Parser;

procedure Table_Section is

   use type Templates_Parser.Vector_Tag;

   Devices : constant Templates_Parser.Vector_Tag :=
               +"Screen" & "Keyboard" & "Mouse" & "Hard Drive";
   Prices  : constant Templates_Parser.Vector_Tag :=
               +"$500" & "$20" & "$15" & "$140";

   Translations : constant Templates_Parser.Translate_Table :=
                    (1 => Templates_Parser.Assoc ("DEVICES", Devices),
                     2 => Templates_Parser.Assoc ("PRICES", Prices));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_section.tmplt", Translations));
end Table_Section;
