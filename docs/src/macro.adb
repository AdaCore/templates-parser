
with Ada.Text_IO;
with Templates_Parser;

procedure Macro is

   use type Templates_Parser.Vector_Tag;

   Translations : Templates_Parser.Translate_Set;

begin
   Templates_Parser.Insert
     (Translations,
      Templates_Parser.Assoc ("VAR", "Templates_Parser"));
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("macro.tmplt", Translations));
end Macro;
