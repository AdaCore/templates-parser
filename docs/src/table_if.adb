
with Ada.Text_IO;
with Templates_Parser;

procedure Table_If is

   use type Templates_Parser.Vector_Tag;

   function In_Stock (Device : in String) return Boolean;
   --  Complex function. Does a SQL access to the right database to know if
   --  the Device is available and thus can be ordered.

   procedure Add (Device, Price : in String);
   --  Add the device into the list to be displayed

   Devices   : Templates_Parser.Tag;
   Prices    : Templates_Parser.Tag;
   Available : Templates_Parser.Tag;

   ---------
   -- Add --
   ---------

   procedure Add (Device, Price : in String) is
   begin
      Devices   := Devices & Device;
      Prices    := Prices & Price;
      Available := Available & In_Stock (Device);
   end Add;

   --------------
   -- In_Stock --
   --------------

   function In_Stock (Device : in String) return Boolean is
   begin
      if Device = "Keyboard" then
         return True;
      else
         return False;
      end if;
   end In_Stock;

   Translations : Templates_Parser.Translate_Table (1 .. 3);

begin
   Add ("Screen", "$500");
   Add ("Keyboard", "$15");
   Add ("Mouse", "$15");
   Add ("Hard Drive", "$140");

   Translations := (Templates_Parser.Assoc ("DEVICES", Devices),
                    Templates_Parser.Assoc ("PRICES", Prices),
                    Templates_Parser.Assoc ("AVAILABLE", Available));

   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_if.tmplt", Translations));
end Table_If;
