
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with Templates_Parser;

with Cursor_L1.Cursors;

procedure Main is
   use Ada;

   N   : constant Natural :=
           Natural'Value (Ada.Command_Line.Argument (1));
   Set : Templates_Parser.Translate_Set;
begin
   for Index in 1 .. N loop
      Cursor_L1.Cursors.Values.Append (Index);
   end loop;

   Text_IO.Put_Line ("N:" & Natural'Image (N));

   declare
      Content : constant String :=
        Templates_Parser.Parse
          (Filename     => "tmplt/cl1.tmplt",
           Translations => Set,
           Cursor_Tag   => Cursor_L1.Cursors.Handler'Access);
   begin
      Text_IO.Put_Line (Content);
   end;
exception
   when E : others =>
      Text_IO.Put_Line ("(FF)" & Ada.Exceptions.Exception_Information (E));
end Main;
