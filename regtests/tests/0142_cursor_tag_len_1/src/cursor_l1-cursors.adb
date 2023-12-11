
package body Cursor_L1.Cursors is

   ---------------
   -- Dimension --
   ---------------

   function Dimension
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String) return Natural is
   begin
      return 1;
   end Dimension;

   ------------
   -- Length --
   ------------

   function Length
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String;
      Path       : Templates_Parser.Dynamic.Path) return Natural is
   begin
      return Natural (Values.Length);
   end Length;

   -----------
   -- Value --
   -----------

   function Value
     (Cursor_Tag : not null access Cursor_Type;
      Var_Name   : String;
      Path       : Templates_Parser.Dynamic.Path) return String is
   begin
      return Values.Element (Path(1))'Image;
   end Value;

end Cursor_L1.Cursors;
