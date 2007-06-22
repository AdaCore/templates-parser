------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                         Copyright (C) 1999-2007                          --
--                                 AdaCore                                  --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with Strings_Maps;

package Templates_Parser is

   use Ada.Strings.Unbounded;

   Template_Error : exception;

   Default_Begin_Tag : constant String := "@_";
   Default_End_Tag   : constant String := "_@";

   Default_Separator : constant String := ", ";

   procedure Set_Tag_Separators
     (Start_With : in String := Default_Begin_Tag;
      Stop_With  : in String := Default_End_Tag);
   --  Set the tag separators for the whole session. This should be changed as
   --  the very first API call and should not be changed after.

   -----------------
   -- Generic Tag --
   -----------------

   type Tag is private;
   --  A tag is using a by reference semantic

   function "+" (Value : in String)           return Tag;
   function "+" (Value : in Character)        return Tag;
   function "+" (Value : in Boolean)          return Tag;
   function "+" (Value : in Unbounded_String) return Tag;
   function "+" (Value : in Integer)          return Tag;
   function "+" (Value : in Tag)              return Tag;
   --  Tag constructors

   function "&" (T : in Tag; Value : in String)           return Tag;
   function "&" (T : in Tag; Value : in Character)        return Tag;
   function "&" (T : in Tag; Value : in Boolean)          return Tag;
   function "&" (T : in Tag; Value : in Unbounded_String) return Tag;
   function "&" (T : in Tag; Value : in Integer)          return Tag;
   function "&" (T : in Tag; Value : in Tag)              return Tag;
   --  Add Value at the end of the tag

   function "&" (Value : in String;           T : in Tag) return Tag;
   function "&" (Value : in Character;        T : in Tag) return Tag;
   function "&" (Value : in Boolean;          T : in Tag) return Tag;
   function "&" (Value : in Unbounded_String; T : in Tag) return Tag;
   function "&" (Value : in Integer;          T : in Tag) return Tag;
   --  Add Value at the front of the tag

   procedure Set_Separator (T : in out Tag; Separator : in String);
   --  Set separator to be used when building a flat representation of
   --  a composite tag.

   procedure Clear (T : in out Tag);
   --  Removes all values in the tag. Current tag T is not released but
   --  the returned object is separated (not using the same reference) than
   --  the original one.

   function Size (T : in Tag) return Natural;
   --  Returns the number of value into T

   function Item (T : in Tag; N : in Positive) return String;
   --  Returns the Nth Tag's item. Raises Constraint_Error if there is
   --  no such Item in T (i.e. T length < N).

   function Composite (T : in Tag; N : in Positive) return Tag;
   --  Returns the Nth Tag's item. Raises Constraint_Error if there is
   --  no such Item in T (i.e. T length < N).

   subtype Vector_Tag is Tag;
   subtype Matrix_Tag is Tag;

   ------------------
   -- Associations --
   ------------------

   type Association is private;

   Null_Association : constant Association;

   type Association_Kind is (Std, Composite);
   --  The kind of association which is either Std (a simple value), a vector
   --  tag or a Matrix tag.

   function Assoc
     (Variable : in String;
      Value    : in String) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is a string.

   function Assoc
     (Variable : in String;
      Value    : in Unbounded_String) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an
   --  Unbounded_String.

   function Assoc
     (Variable : in String;
      Value    : in Integer) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an Integer.
   --  It will be displayed without leading space if positive.

   function Assoc
     (Variable : in String;
      Value    : in Boolean) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. It set the variable to TRUE or FALSE depending on
   --  value.

   function Assoc
     (Variable  : in String;
      Value     : in Tag;
      Separator : in String := Default_Separator) return Association;
   --  Build an Association (Variable = Value) to be added to Translate_Table.
   --  This is a tag association. Separator will be used when outputting the
   --  a flat representation of the Tag (outside a table statement).

   function Get (Assoc : in Association) return Tag;
   --  Returns the Tag in Assoc, raise Constraint_Error if Assoc is not
   --  containing a Tag (Association_Kind is Std).
   --  See also the Templates_Parser.Query package for other functions to
   --  manipulate associations.

   function Get (Assoc : in Association) return String;
   --  Returns the value in Assoc, raise Constraint_Error if Assoc is not
   --  containing a simple value (Association_Kind is Composite).
   --  See also the Templates_Parser.Query package for other functions to
   --  manipulate associations.

   ---------------------------
   -- Association table/set --
   ---------------------------

   type Translate_Table is array (Positive range <>) of Association;
   --  A table with a set of associations, note that it is better to use
   --  Translate_Set below as it is more efficient.

   No_Translation : constant Translate_Table;

   type Translate_Set is private;
   --  This is a set of association like Translate_Table but it is possible to
   --  insert item into this set more easily, furthermore there is no need to
   --  know the number of item before hand. This is the object used internally
   --  by the templates engine as it is far more efficient to retrieve a
   --  specific item from it.

   Null_Set : constant Translate_Set;

   procedure Insert (Set : in out Translate_Set; Item : in Association);
   --  Add Item into the translate set. If an association for this variable
   --  already exists it just replaces it by the new item.

   procedure Insert (Set : in out Translate_Set; Items : in Translate_Set);
   --  Add Items into the translate set. If an association for variables in
   --  Items already exists it just replaces it by the new one.

   procedure Remove (Set : in out Translate_Set; Name : in String);
   --  Removes association named Name from the Set. Does nothing if there is
   --  not such association in the set.

   function Get (Set : in Translate_Set; Name : in String) return Association;
   --  Returns the association named Name in the Set. Returns Null_Association
   --  is no such association if found in Set.

   function Exists
     (Set      : in Translate_Set;
      Variable : in String) return Boolean;
   --  Returns True if an association for Variable exists into the Set

   generic
      with procedure Action
        (Item : in     Association;
         Quit : in out Boolean);
   procedure For_Every_Association (Set : in Translate_Set);
   --  Iterates through all associations in the set, call Action for each one.
   --  Set Quit to True to stop the iteration.

   function To_Set (Table : in Translate_Table) return Translate_Set;
   --  Convert a Translate_Table into a Translate_Set

   -------------
   -- Dynamic --
   -------------

   package Dynamic is

      --------------
      -- Lazy_Tag --
      --------------

      type Lazy_Tag is abstract tagged private;
      type Lazy_Tag_Access is access all Lazy_Tag'Class;

      procedure Value
        (Lazy_Tag     : not null access Dynamic.Lazy_Tag;
         Var_Name     : in String;
         Translations : in out Translate_Set) is abstract;
      --  Value is called by the Parse routines below if a tag variable was not
      --  found in the set of translations. This routine must then add the
      --  association for variable Name. It is possible to add other
      --  associations in the translation table but a check is done to see if
      --  the variable Name as been set or not. The default implementation does
      --  nothing.

      Null_Lazy_Tag : constant Lazy_Tag_Access;

      ----------------
      -- Cursor_Tag --
      ----------------

      type Cursor_Tag is abstract tagged private;
      type Cursor_Tag_Access is access all Cursor_Tag'Class;
      --  In some cases it is difficult and not efficient to have to map all
      --  Ada data into a template Tag. A Cursor_Tag object gives the ability
      --  to iterate through a data structure which is living on the Ada side
      --  only.

      function Dimension
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : in String) return Natural is abstract;
      --  Must return the number of dimensions for the given variable name. For
      --  a matrix this routine should return 2 for example.

      type Path is array (Positive range <>) of Natural;
      --  A Path gives the full position of a given element in the cursor tag

      function Length
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : in String;
         Path       : in Dynamic.Path) return Natural is abstract;
      --  Must return the number of item for the given path. The first
      --  dimension is given by the Path (1), for the second column the Path is
      --  (1, 2). Note that each dimension can have a different length. For
      --  example a Matrix is not necessary square.

      function Value
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : in String;
         Path       : in Dynamic.Path) return String is abstract;
      --  Must return the value for the variable at the given Path. Note that
      --  this routine will be called only for valid items as given by the
      --  Dimension and Length above.

      Null_Cursor_Tag : constant Cursor_Tag_Access;

   private

      type Lazy_Tag is abstract tagged null record;

      Null_Lazy_Tag : constant Lazy_Tag_Access := null;

      type Cursor_Tag is abstract tagged null record;

      Null_Cursor_Tag : constant Cursor_Tag_Access := null;

   end Dynamic;

   package Dyn renames Dynamic;

   --------------------
   -- User's Filters --
   --------------------

   type Filter_Context is record
      Translations : Translate_Set;
      Lazy_Tag     : Dynamic.Lazy_Tag_Access;
   end record;

   type Callback is access function
     (Value      : in String;
      Parameters : in String;
      Context    : in Filter_Context) return String;
   --  User's filter callback

   type Callback_No_Param is access function
     (Value   : in String;
      Context : in Filter_Context) return String;
   --  User's filter callback

   procedure Register_Filter
     (Name    : in String;
      Handler : in Callback);
   --  Register user's filter Name using the specified Handler

   procedure Register_Filter
     (Name    : in String;
      Handler : in Callback_No_Param);
   --  Register user's filter Name using the specified Handler

   type User_Filter is abstract tagged private;
   type User_Filter_Access is access all User_Filter'Class;
   function Execute
     (Filter     : access User_Filter;
      Value      : in String;
      Parameters : in String;
      Context    : in Filter_Context) return String is abstract;
   --  User filters can also be implemented through a tagged type, which allows
   --  you to added your own user data to reuse a filter in several
   --  applications, perhaps with a slightly different behavior each time.
   --  It is possible for the callback to modify the data stored in Filter, but
   --  this needs to be done with care, since multiple concurrent calls to
   --  Callback might happen.

   procedure Register_Filter
     (Name       : in String;
      Filter     : access User_Filter'Class);
   --  Register a new filter. Filter must not be freed by the caller, since no
   --  copy is made.

   -----------------------------
   -- Parsing and Translating --
   -----------------------------

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table       := No_Translation;
      Cached            : in Boolean               := False;
      Keep_Unknown_Tags : in Boolean               := False;
      Lazy_Tag          : in Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : in Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String;
   --  Parse the Template_File replacing variables' occurrences by the
   --  corresponding values. If Cached is set to True, Filename tree will be
   --  recorded into a cache for quick retrieval. If Keep_Unknown_Tags is set
   --  to True then tags that are not in the translate table are kept
   --  as-is if it is part of the template data. If this tags is part of a
   --  condition (in an IF statement tag), the condition will evaluate to
   --  False.

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table       := No_Translation;
      Cached            : in Boolean               := False;
      Keep_Unknown_Tags : in Boolean               := False;
      Lazy_Tag          : in Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : in Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String;
   --  Idem but returns an Unbounded_String

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Set;
      Cached            : in Boolean               := False;
      Keep_Unknown_Tags : in Boolean               := False;
      Lazy_Tag          : in Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : in Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String;
   --  Idem with a Translation_Set

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Set;
      Cached            : in Boolean               := False;
      Keep_Unknown_Tags : in Boolean               := False;
      Lazy_Tag          : in Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : in Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String;
   --  Idem with a Translation_Set

   function Translate
     (Template     : in String;
      Translations : in Translate_Table := No_Translation) return String;
   --  Just translate the discrete variables in the Template string using the
   --  Translations table. This function does not parse the command tag (TABLE,
   --  IF, INCLUDE). All composite tags are replaced by the empty string.

   function Translate
     (Template     : in String;
      Translations : in Translate_Set) return String;
   --  Idem with a Translation_Set

   procedure Release_Cache;
   --  Release the internal cache. This free the memory used for all currently
   --  loaded template trees.

private

   type Integer_Access is access Integer;

   ------------------
   -- Generic Tags --
   ------------------

   type Node_Kind is (Value, Value_Set);

   type Tag_Access is access Tag;

   type Tag_Node (Kind : Node_Kind);
   type Tag_Node_Access is access Tag_Node;

   type Tag_Node (Kind : Node_Kind) is record
      Next : Tag_Node_Access;
      case Kind is
         when Value     => V  : Unbounded_String;
         when Value_Set => VS : Tag_Access;
      end case;
   end record;

   type Access_Tag_Node_Access is access Tag_Node_Access;

   type Tag_Node_Arr is array (Positive range <>) of Tag_Node_Access;
   type Tag_Node_Arr_Access is access Tag_Node_Arr;

   type Tag_Data is record
      Count        : Natural;  -- Number of items
      Min, Max     : Natural;  -- Min/Max item's sizes, equal to 1 if leaf
      Nested_Level : Positive; -- Number of composite structures
      Separator    : Unbounded_String;
      Head         : Tag_Node_Access;
      Last         : Tag_Node_Access;
      Tag_Nodes    : Tag_Node_Arr_Access;
      --  This array will be setup during parsing to ensure fast iteration
      --  in reverse order.
   end record;

   type Tag_Data_Access is access Tag_Data;

   type Tag is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Data      : Tag_Data_Access;
   end record;

   procedure Initialize (T : in out Tag);
   procedure Finalize   (T : in out Tag);
   procedure Adjust     (T : in out Tag);

   subtype Indices is Dynamic.Path;
   --  Set of indices that reference a specific item into a composite tag.
   --  Used by the parser.

   procedure Field
     (T      : in     Tag;
      N      : in     Positive;
      Result :    out Tag;
      Found  :    out Boolean);
   --  Returns the N'th item in Tag. Found is set to False is there is no
   --  such item.

   ------------------
   --  Association --
   ------------------

   type Association (Kind : Association_Kind := Std) is record
      Variable : Unbounded_String;

      case Kind is
         when Std =>
            Value : Unbounded_String;

         when Composite =>
            Comp_Value : Tag;
      end case;
   end record;

   Null_Association : constant Association
     := (Std, Null_Unbounded_String, Null_Unbounded_String);

   No_Translation : constant Translate_Table := (2 .. 1 => Null_Association);

   procedure Print_Tree (Filename : in String);
   --  Use for debugging purpose only, it will output the internal tree
   --  representation.

   --------------------
   --  Translate_Set --
   --------------------

   package Association_Set is new Strings_Maps (Association, "=");
   use Association_Set;

   type Map_Access is access Containers.Map;

   type Translate_Set is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Set       : Map_Access;
   end record;

   procedure Initialize (Set : in out Translate_Set);
   procedure Finalize   (Set : in out Translate_Set);
   procedure Adjust     (Set : in out Translate_Set);

   function Image (N : in Integer) return String;
   pragma Inline (Image);
   --  Returns N image without leading blank

   Null_Set : constant Translate_Set :=
                (Ada.Finalization.Controlled with null, null);

   ------------------
   -- User filters --
   ------------------

   type User_Filter is abstract tagged null record;

end Templates_Parser;
