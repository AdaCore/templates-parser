
-- $Id$

with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Templates_Parser is

   use Ada;

   -----------
   -- Assoc --
   -----------

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Begin_Tag : in String := Default_Begin_Tag;
                   End_Tag   : in String := Default_End_Tag)
                   return Association is
   begin
      return Association'
        (Strings_1024.To_Bounded_String (Begin_Tag & Variable & End_Tag),
         Strings_1024.To_Bounded_String (Value));
   end Assoc;

   -----------
   -- Parse --
   -----------

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String
   is

      procedure Translate (Str : in out Strings_1024.Bounded_String) is
         Pos : Natural;
      begin
         for K in Translations'Range loop
            Pos := Strings_1024.Index
              (Str,
               Strings_1024.To_String (Translations (K).Variable));

            if Pos /= 0 then
               Strings_1024.Replace_Slice
                 (Str,
                  Pos,
                  Pos + Strings_1024.Length (Translations (K).Variable) - 1,
                  Strings_1024.To_String (Translations (K).Value));
            end if;
         end loop;
      end Translate;

      use Ada.Strings.Unbounded;

      Buffer : String (1 .. 1_024);
      Last   : Natural;
      Str    : Strings_1024.Bounded_String;
      Result : Unbounded_String;

      File   : Text_IO.File_Type;

   begin
      Text_IO.Open (File => File,
                    Name => Template_Filename,
                    Mode => Text_IO.In_File);

      while not Text_IO.End_Of_File (File) loop
         Text_IO.Get_Line (File, Buffer, Last);

         if Last /= 0 then
            Str := Strings_1024.To_Bounded_String (Buffer (1 .. Last));
            Translate (Str);
            Result := Result & Strings_1024.To_String (Str);
         end if;

         Result := Result & ASCII.LF;
      end loop;

      Text_IO.Close (File);

      return To_String (Result);
   end Parse;

end Templates_Parser;
