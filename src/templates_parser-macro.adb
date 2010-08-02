------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

separate (Templates_Parser)

package body Macro is

   function Default_Callback
     (Name : String; Params : Parameter_Set) return String;
   --  Default macro callback

   package Registry is new Containers.Indefinite_Hashed_Maps
     (String, Tree, Strings.Hash_Case_Insensitive, "=");

   Set : Registry.Map;

   --------------
   -- Register --
   --------------

   procedure Register (Name : String; T : Tree) is
      Old : Tree := Get (Name);
   begin
      if Old /= null then
         Set.Delete (Name);
         Release (Old);
      end if;
      Set.Insert (Name, T);
   end Register;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Tree is
      Position : constant Registry.Cursor := Set.Find (Name);
   begin
      if Registry.Has_Element (Position) then
         return Registry.Element (Position);
      else
         return null;
      end if;
   end Get;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite
     (T          : in out Tree;
      Parameters : not null access Data.Parameter_Set)
   is

      procedure Rewrite (T : in out Data.Tree);
      --  Rewrite every variable references @_$N_@ (where N is a
      --  number) by the corresponding variable or value found in
      --  Parameters(N).

      procedure Rewrite (T : in out Expr.Tree);
      --  Rewrite condition.
      --  In @@IF@@ @_$N_@ = val, replace $N by Parameters(N) or does nothing
      --                          if Parameters(N) does not exists.

      procedure Rewrite (T : in out Definitions.Tree);
      --  Rewrite variable references with and without default value.
      --  In @@SET@@ VAR=$N, replace $N by Parameters(N) or does
      --                     nothing if Parameters(N) does not exists.
      --  In @@SET@@ VAR=$N|val replace $N|val by Parameters(N) if it
      --                        exists or by val otherwise.

      package Set_Var is new Containers.Indefinite_Hashed_Maps
        (String, String, Strings.Hash_Case_Insensitive, "=");

      Vars : Set_Var.Map;

      -------------
      -- Rewrite --
      -------------

      procedure Rewrite (T : in out Data.Tree) is
         use type Data.Tree;
         D, Prev : Data.Tree;
      begin
         D    := T;
         Prev := null;

         while D /= null loop
            case D.Kind is
               when Data.Text =>
                  null;

               when Data.Var =>
                  if D.Var.N > 0 then

                     if D.Var.N <= Parameters'Length
                       and then Parameters (D.Var.N) /= null
                     then
                        --  This is a reference to replace
                        declare
                           New_Node : constant Data.Tree :=
                                        Data.Clone (Parameters (D.Var.N));
                        begin
                           New_Node.Next := D.Next;
                           if Prev = null then
                              Data.Release (T, Single => True);
                              T := New_Node;
                           else
                              Data.Release (Prev.Next, Single => True);
                              Prev.Next := New_Node;
                           end if;
                        end;

                     elsif Vars.Contains (To_String (D.Var.Name)) then
                        --  This is a variable that exists into the map.
                        --  It means that this variable is actually the
                        --  name of a SET which actually has been passed
                        --  a reference to another variable.

                        D.Var.Name := To_Unbounded_String
                          (Vars.Element (To_String (D.Var.Name)));
                     end if;
                  end if;

                  --  Rewrite also the macro call if any

                  if D.Var.Is_Macro then
                     Rewrite (D.Var.Def, Parameters);
                  end if;
            end case;

            Prev := D;
            D    := D.Next;
         end loop;
      end Rewrite;

      -------------
      -- Rewrite --
      -------------

      procedure Rewrite (T : in out Definitions.Tree) is

         use type Data.Tree;

         procedure Replace
           (Def   : in out Definitions.Tree;
            Value : Data.Tree);
         pragma Inline (Replace);
         --  Replace the given Def with the Value. Raises
         --  Templates_Error if Value is a tag variable.

         procedure Replace
           (Def   : in out Definitions.Tree;
            Value : Unbounded_String);
         pragma Inline (Replace);
         --  As above but for a string

         -------------
         -- Replace --
         -------------

         procedure Replace
           (Def   : in out Definitions.Tree;
            Value : Unbounded_String)
         is
            Name : constant Unbounded_String := Def.Name;
            V    : constant Unbounded_String := Value;
         begin
            Definitions.Release (Def);
            Def := new Definitions.Def'(Name, (Definitions.Const, V, 1));
         end Replace;

         procedure Replace
           (Def   : in out Definitions.Tree;
            Value : Data.Tree)
         is
            use type Data.NKind;
         begin
            case Value.Kind is
               when Data.Var  =>
                  --  This is a variable reference, record this association
                  --  into the map. The variable name will be changed later.
                  Vars.Include
                    (To_String (Def.Name), To_String (Value.Var.Name));
               when Data.Text =>
                  Replace (Def, Value.Value);
            end case;
         end Replace;

      begin
         case T.N.Kind is
            when Definitions.Const =>
               null;

            when Definitions.Ref =>
               if T.N.Ref <= Parameters'Length
                 and then Parameters (T.N.Ref) /= null
               then
                  Replace (T, Parameters (T.N.Ref));
               end if;

            when Definitions.Ref_Default =>
               if T.N.Ref <= Parameters'Length
                 and then Parameters (T.N.Ref) /= null
               then
                  Replace (T, Parameters (T.N.Ref));
               else
                  Replace (T, T.N.Value);
               end if;
         end case;
      end Rewrite;

      -------------
      -- Rewrite --
      -------------

      procedure Rewrite (T : in out Expr.Tree) is
         use type Expr.Tree;
         Value   : Unbounded_String;
         Tag_Var : Data.Tag_Var;
      begin
         case T.Kind is
            when Expr.Value =>
               null;

            when Expr.Var =>
               if T.Var.N > 0
                 and then T.Var.N <= Parameters'Length
               then
                  --  This is a reference to replace

                  case Parameters (T.Var.N).Kind is
                     when Data.Text =>
                        Value := Parameters (T.Var.N).Value;
                        Expr.Release (T, Single => True);
                        T := new Expr.Node'(Expr.Value, V => Value);

                     when Data.Var =>
                        Tag_Var := Data.Clone (Parameters (T.Var.N).Var);
                        Data.Release (T.Var);
                        T.Var := Tag_Var;
                  end case;
               end if;

            when Expr.Op =>
               Rewrite (T.Left);
               Rewrite (T.Right);

            when Expr.U_Op =>
               Rewrite (T.Next);
         end case;
      end Rewrite;

      N : Tree := T;

   begin
      T := N;

      while N /= null loop
         case N.Kind is
            when Text =>
               Rewrite (N.Text);

            when If_Stmt =>
               Rewrite (N.Cond);

               --  ??? After the rewrite above we could check the condition to
               --  see if it is always true or false and remove the
               --  corresponding branch.

               Rewrite (N.N_True, Parameters);
               Rewrite (N.N_False, Parameters);

            when Set_Stmt =>
               Rewrite (N.Def);

            when Table_Stmt =>
               Rewrite (N.Blocks, Parameters);

            when Section_Block =>
               Rewrite (N.Common, Parameters);
               Rewrite (N.Sections, Parameters);

            when Section_Stmt =>
               Rewrite (N.N_Section, Parameters);

            when others =>
               null;
         end case;

         N := N.Next;
      end loop;
   end Rewrite;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback
     (Name : String; Params : Parameter_Set) return String
   is
      function Parameters return String;
      --  Returns parameters

      ----------------
      -- Parameters --
      ----------------

      function Parameters return String is
         R : Unbounded_String;
      begin
         for K in Params'Range loop
            Append (R, Params (K));

            if K /= Params'Last then
               Append (R, ",");
            end if;
         end loop;

         return To_String (R);
      end Parameters;

   begin
      return To_String (Begin_Tag) & Name
        & "(" & Parameters & ")" & To_String (End_Tag);
   end Default_Callback;

begin
   Callback := Default_Callback'Access;
end Macro;
