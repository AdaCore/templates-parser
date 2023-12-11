
with Ada.Containers.Vectors;

with Templates_Parser;

package Cursor_L1.Cursors is

   package Values_Container is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Natural);

   Values : Values_Container.Vector;


   type Cursor_Type is new Templates_Parser.Dynamic.Cursor_Tag with null record;

   ---------------
   -- Dimension --
   ---------------

   function Dimension
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String) return Natural;

   ------------
   -- Length --
   ------------

   function Length
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String;
      Path       : Templates_Parser.Dynamic.Path) return Natural;

   -----------
   -- Value --
   -----------

   function Value
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String;
      Path       : Templates_Parser.Dynamic.Path) return String;

   Handler : aliased Cursor_L1.Cursors.Cursor_Type;

end Cursor_L1.Cursors;
