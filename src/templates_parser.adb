------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 1999                            --
--                               Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Strings_Cutter;

package body Templates_Parser is

   use Ada;
   use Ada.Strings;

   Max_Section     : constant := 10;

   subtype Buffer_Type is String (1 .. 1_024);

   Table_Token     : constant String := "@@TABLE@@";
   Section_Token   : constant String := "@@SECTION@@";
   End_Table_Token : constant String := "@@END_TABLE@@";
   If_Token        : constant String := "@@IF@@";
   Else_Token      : constant String := "@@ELSE@@";
   End_If_Token    : constant String := "@@END_IF@@";
   Include_Token   : constant String := "@@INCLUDE@@";

   subtype Table_Range     is Positive range Table_Token'Range;
   subtype Section_Range   is Positive range Section_Token'Range;
   subtype End_Table_Range is Positive range End_Table_Token'Range;
   subtype If_Range        is Positive range If_Token'Range;
   subtype Else_Range      is Positive range Else_Token'Range;
   subtype End_If_Range    is Positive range End_If_Token'Range;
   subtype Include_Range   is Positive range Include_Token'Range;

   -----------
   -- Assoc --
   -----------

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag;
                   Separator : in Character := Default_Separator)
                   return Association is
   begin
      return Association'
        (To_Unbounded_String (Begin_Tag & Variable & End_Tag),
         To_Unbounded_String (Value),
         Separator);
   end Assoc;

   -----------
   -- Assoc --
   -----------

   function Assoc (Variable  : in String;
                   Value     : in Boolean;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag)
                   return Association is
   begin
      if Value then
         return Assoc (Variable, "TRUE", Begin_Tag, End_Tag);
      else
         return Assoc (Variable, "FALSE", Begin_Tag, End_Tag);
      end if;
   end Assoc;

   -----------
   -- Parse --
   -----------

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String
   is

      File         : Text_IO.File_Type;
      Current_Line : Natural := 0;
      Table_Level  : Natural := 0;
      If_Level     : Natural := 0;

      Max_Lines    : Natural := 0;
      --  maximum number if lines in the table. This variable is always set to
      --  0 outside of a table.

      procedure Fatal_Error (Message : in String);
      --  raise Template_Error exception with message.

      function Exist (Translations : in Translate_Table;
                      Tag          : in String;
                      Value        : in String)
                     return Boolean;
      --  check that Tag exist in the translation table and that its value is
      --  set to Value.

      procedure Translate (Str : in out Unbounded_String;
                           Tag : in     String;
                           To  : in     String);
      --  Translate all tags named Tag in Str by To

      procedure Translate (Str : in out Unbounded_String);
      --  Translate all tags in the translations table and the specials tags
      --  in Str by their corresponding value.

      procedure Translate (Str  : in out Unbounded_String;
                           N    : in     Positive;
                           Stop :    out Boolean);
      --  Translate all tags in Str with the Nth Tag's value. This procedure
      --  is used to build the tables. Tags used in a table are a set of
      --  values separated by a special character.

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error (Message : in String) is
      begin
         Exceptions.Raise_Exception
           (Template_Error'Identity,
            "In " & Template_Filename
            & " at line" & Natural'Image (Current_Line) & ' ' & Message);
      end Fatal_Error;

      -----------
      -- Exist --
      -----------

      function Exist (Translations : in Translate_Table;
                      Tag          : in String;
                      Value        : in String)
        return Boolean is
      begin
         for K in Translations'Range loop
            if Translations (K).Variable = Tag
              and then Translations (K).Value = Value then
               return True;
            end if;
         end loop;
         return False;
      end Exist;

      ---------------
      -- Translate --
      ---------------

      procedure Translate (Str : in out Unbounded_String;
                           Tag : in     String;
                           To  : in     String) is
         Pos : Natural;
      begin
         loop
            Pos := Index (Str, Tag);

            exit when Pos = 0;

            if Pos /= 0 then
               Replace_Slice (Str, Pos, Pos + Tag'Length - 1, To);
            end if;
         end loop;
      end Translate;

      procedure Translate (Str : in out Unbounded_String) is
      begin
         for K in Translations'Range loop

            Translate (Str,
                       To_String (Translations (K).Variable),
                       To_String (Translations (K).Value));
         end loop;

         Translate (Str,
                    Tag => "@@_NUMBER_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (Max_Lines),
                                       Strings.Both));

         Translate (Str,
                    Tag => "@@_TABLE_LEVEL_@@",
                    To  => Fixed.Trim (Positive'Image (Table_Level),
                                       Strings.Both));
      end Translate;

      procedure Translate (Str  : in out Unbounded_String;
                           N    : in     Positive;
                           Stop :    out Boolean)
      is
         use Strings_Cutter;
         Pos   : Natural;
         CS    : Cutted_String;

      begin
         Stop := False;

         --  for every entry in the translation table

         for K in Translations'Range loop

            loop
               Pos := Index
                 (Str,
                  To_String (Translations (K).Variable));

               exit when Pos = 0;

               Create (CS,
                       To_String (Translations (K).Value),
                       String'(1 => Translations (K).Separator));

               --  we stop when we reach the maximum number of field

               Stop := (N = Field_Count (CS) or else Field_Count (CS) = 0);

               --  if there is no value for the tag, just replace it with an
               --  emptry string (i.e. removing it from the template).

               if Field_Count (CS) = 0 then
                  Replace_Slice
                    (Str,
                     Pos,
                     Pos + Length (Translations (K).Variable) - 1,
                     "");
               else
                  Replace_Slice
                    (Str,
                     Pos,
                     Pos + Length (Translations (K).Variable) - 1,
                     Field (CS, N));
               end if;

               Destroy (CS);
            end loop;

         end loop;

         --  translate special tags

         Translate (Str,
                    Tag => "@@_TABLE_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (N), Strings.Both));

         Translate (Str,
                    Tag => "@@_NUMBER_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (Max_Lines),
                                       Strings.Both));

         Translate (Str,
                    Tag => "@@_TABLE_LEVEL_@@",
                    To  => Fixed.Trim (Positive'Image (Table_Level),
                                       Strings.Both));

      end Translate;

      --  variables used by Parse

      Buffer        : Buffer_Type;
      Last          : Natural;
      Result        : Unbounded_String;
      Trimed_Buffer : Buffer_Type;

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line (Buffer, Trimed_Buffer : out String;
                               Last                  : out Natural);
      pragma Inline (Get_Next_Line);

      procedure Get_Next_Line (Buffer, Trimed_Buffer : out String;
                               Last                  : out Natural) is
      begin
         Text_IO.Get_Line (File, Buffer, Last);
         Current_Line := Current_Line + 1;

         Fixed.Move (Fixed.Trim (Buffer (1 .. Last), Strings.Left),
                     Trimed_Buffer);
      end Get_Next_Line;


      -----------
      -- Parse --
      -----------

      function Parse (Line : in String) return String is

         Str : Unbounded_String;



         function Parse_If (Condition       : in String  := "";
                            Check_Condition : in Boolean := True)
           return String;
         --  Parse an if statement (from If_Token to End_If_Token). Condition
         --  is the if conditional part. It will be checked only if
         --  Check_Condition is True.  At the time of this call If_Token as
         --  been read. When returning the file index is positioned on the
         --  line just after the End_If_Token is ready.

         function Parse_Table return String;
         --  Parse a table statement (from Table_Token to End_Table_Token). At
         --  the time of this call Table_Token as been read. When returning
         --  the file index is positioned on the line just after the
         --  End_Table_Token line.

         function Parse_Include (Filename : in String) return String;
         --  Parse an include statement (Include_Token).  At the time of this
         --  call Include_Token as been read. When returning the file index is
         --  positioned on the line just after the Include_Token line.

         function Parse_Table return String is

            type Sections is array (Positive range <>) of Unbounded_String;

            Buffer        : Buffer_Type;
            Trimed_Buffer : Buffer_Type;
            Last          : Natural;
            Lines         : Sections (1 .. Max_Section);
            Str           : Unbounded_String;
            Result        : Unbounded_String;
            Section       : Positive := 1;

            function Count_Lines (Lines : Sections) return Natural is
               use Strings_Cutter;
               Max_Values : Natural := 0;
               CS         : Cutted_String;
            begin
               for S in Lines'Range loop
                  for T in Translations'Range loop
                     if Index (Lines (S),
                               To_String (Translations (T).Variable)) /= 0
                     then
                        Create (CS,
                                To_String (Translations (T).Value),
                                String'(1 => Translations (T).Separator));
                        Max_Values := Natural'Max (Max_Values,
                                                   Field_Count (CS));
                        Destroy (CS);
                     end if;
                  end loop;
               end loop;
               return Max_Values;
            end Count_Lines;

         begin

            Table_Level := Table_Level + 1;

            loop
               Get_Next_Line (Buffer, Trimed_Buffer, Last);

               if Last = 0 then
                  --  this is an empty line
                  Lines (Section) := Lines (Section) & ASCII.LF;
               else
                  if Trimed_Buffer (If_Range) = If_Token then
                     Lines (Section) := Lines (Section) & Parse_If
                       (Fixed.Trim
                        (Trimed_Buffer (If_Range'Last + 2 .. Last), Both));

                  elsif Trimed_Buffer (Table_Range) = Table_Token then
                     Lines (Section) := Lines (Section) & Parse_Table;

                  elsif Trimed_Buffer (Include_Range) = Include_Token then
                     Lines (Section) := Lines (Section) & Parse_Include
                       (Fixed.Trim
                        (Trimed_Buffer (Include_Range'Last + 2 .. Last),
                         Both));

                  elsif Trimed_Buffer (End_Table_Range) = End_Table_Token then
                     declare
                        K    : Positive := 1;
                        Stop : Boolean;
                     begin

                        Max_Lines := Count_Lines (Lines (1 .. Section));

                        if Max_Lines = 0 then

                           Fatal_Error ("A table does not contain variables");

                        else

                           loop
                              Str := Lines ((K - 1) mod Section + 1);
                              Translate (Str, K, Stop);

                              Result := Result & To_String (Str);

                              exit when Stop;

                              K := K + 1;
                           end loop;

                        end if;

                        Max_Lines := 0;

                        exit;
                     end;

                  elsif Trimed_Buffer (Section_Range) = Section_Token then

                     Section := Section + 1;

                  elsif Trimed_Buffer (End_If_Range) = End_If_Token then
                     Fatal_Error (End_If_Token & " found, "
                                  & End_Table_Token & " expected.");

                  else
                     Lines (Section) := Lines (Section)
                       & Buffer (1 .. Last) & ASCII.LF;
                  end if;
               end if;
            end loop;

            Table_Level := Table_Level - 1;
            return To_String (Result);

         exception
            when Text_IO.End_Error =>
               Fatal_Error ("found end of file, expected " & End_Table_Token);
               return "";
         end Parse_Table;

         function Parse_If (Condition       : in String  := "";
                            Check_Condition : in Boolean := True)
           return String
         is
            Buffer        : Buffer_Type;
            Trimed_Buffer : Buffer_Type;
            Last          : Natural := 0;
            Lines         : Unbounded_String;
         begin
            --  if no need to check condition or condition is true

            if Check_Condition = False
              or else Exist (Translations, Condition, "TRUE")
            then

               loop

                  Get_Next_Line (Buffer, Trimed_Buffer, Last);

                  if Last = 0 then
                     Lines := Lines & ASCII.LF;
                  else
                     if Trimed_Buffer (Table_Range) = Table_Token then
                        Lines := Lines & Parse_Table;

                     elsif Trimed_Buffer (If_Range) = If_Token then
                        Lines := Lines & Parse_If
                          (Fixed.Trim
                           (Trimed_Buffer (If_Range'Last + 2 .. Last),
                            Both));

                     elsif Trimed_Buffer (Include_Range) = Include_Token then
                        Lines := Lines & Parse_Include
                          (Fixed.Trim
                           (Trimed_Buffer (Include_Range'Last + 2 .. Last),
                            Both));

                     elsif Trimed_Buffer (End_If_Range) = End_If_Token then
                        exit;

                     elsif Trimed_Buffer (Else_Range) = Else_Token then

                        --  skip to End_If_Token at the same level
                        If_Level := 0;

                        loop
                           Get_Next_Line (Buffer, Trimed_Buffer, Last);

                           if Trimed_Buffer (If_Range) = If_Token then
                              If_Level := If_Level + 1;
                           end if;

                           if Trimed_Buffer (End_If_Range) = End_If_Token then

                              if If_Level = 0 then
                                 exit;
                              else
                                 If_Level := If_Level - 1;
                              end if;
                           end if;
                        end loop;

                        exit;

                     elsif
                       Trimed_Buffer (End_Table_Range) = End_Table_Token
                     then
                        Fatal_Error (End_Table_Token & " found, "
                                     & End_If_Token & " expected.");

                     else
                        Lines := Lines & Buffer (1 .. Last) & ASCII.LF;

                     end if;
                  end if;
               end loop;

               --  We want to translate if's body only if it is not inside a
               --  table. In this case we return the if's body untranslated.
               --  The translation will take place in the table using the
               --  table translation mechanism (i.e. using tag's values set).

               if Table_Level = 0 then
                  Translate (Lines);
               end if;

               return To_String (Lines);

            else

               --  condition is false, we skip lines until End_If_Token (at
               --  the same level) or if we found a Else_Token part we parse
               --  it.

               If_Level := 0;

               loop
                  Get_Next_Line (Buffer, Trimed_Buffer, Last);

                  if Trimed_Buffer (If_Range) = If_Token then
                     If_Level := If_Level + 1;
                  end if;

                  if If_Level = 0
                    and then Trimed_Buffer (Else_Range) = Else_Token
                  then
                     --  else part found, parse else-block as a main-if block
                     --  without checking the condition.

                     return Parse_If (Check_Condition => False);
                  end if;

                  if Trimed_Buffer (End_If_Range) = End_If_Token then

                     if If_Level = 0 then
                        exit;
                     else
                        If_Level := If_Level - 1;
                     end if;
                  end if;
               end loop;

               return "";

            end if;
         exception
            when Text_IO.End_Error =>
               Fatal_Error ("found end of file, expected " & End_If_Token);
               return "";
         end Parse_If;

         function Parse_Include (Filename : in String) return String is

            --  returns last directory separator in Filename.
            --  0 means that there is not directory separator in Filename.

            function Dir_Sep_Index (Filename : in String) return Natural is
               use Ada.Strings;
            begin
               return Fixed.Index (Filename,
                                   Set   => Maps.To_Set ("/\"),
                                   Going => Strings.Backward);
            end Dir_Sep_Index;

            K      : Natural;

         begin
            --  if the file to be included is specified with a path or if
            --  there is no path specified with the current template then
            --  we will open the included template as specified.

            K := Dir_Sep_Index (Template_Filename);

            if Dir_Sep_Index (Filename) /= 0 or else K = 0 then
               return Parse (Filename, Translations);
            else

               --  concat current template file pathname with the included
               --  filename. it means that we want to open the new template
               --  filename in the directory were the current template is.

               return Parse (Template_Filename (1 .. K) & Filename,
                             Translations);
            end if;

         exception
            when others =>
               Fatal_Error ("include file " & Filename & " not found.");
               return "";
         end Parse_Include;

      begin
         if Line'Length /= 0 then

            if Trimed_Buffer (Table_Range) = Table_Token then

               return Parse_Table;

            elsif Trimed_Buffer (If_Range) = If_Token then

               return Parse_If
                 (Fixed.Trim
                  (Trimed_Buffer (If_Range'Last + 2 .. Last), Both));

            elsif Trimed_Buffer (Include_Range) = Include_Token then

               return Parse_Include
                 (Fixed.Trim
                  (Trimed_Buffer (Include_Range'Last + 2 .. Last), Both));

            else
               Str := To_Unbounded_String (Buffer (1 .. Last));
               Translate (Str);
               return To_String (Str) & ASCII.LF;
            end if;

         else
            --  empty line

            return String'(1 => ASCII.LF);

         end if;
      end Parse;

   begin
      begin
         Text_IO.Open (File => File,
                       Name => Template_Filename,
                       Mode => Text_IO.In_File);
      exception
         when Text_IO.Name_Error =>
            Exceptions.Raise_Exception (Template_Error'Identity,
                                        "template file not found");
      end;

      while not Text_IO.End_Of_File (File) loop

         Get_Next_Line (Buffer, Trimed_Buffer, Last);

         Result := Result & Parse (Line => Buffer (1 .. Last));

      end loop;

      Text_IO.Close (File);

      return To_String (Result);
   exception
      when others =>
            raise Template_Error;
   end Parse;

end Templates_Parser;
