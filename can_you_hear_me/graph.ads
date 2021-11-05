with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package Graph is

   package StringLists is new Ada.Containers.Doubly_Linked_Lists
      (Unbounded_String);

   type Node is record
      Name : Unbounded_String;
      List : StringLists.List;
      Visited : Boolean := False;
   end record;

   package NodeLists is new Ada.Containers.Doubly_Linked_Lists (Node);

   procedure CreateLink (List   : in out NodeLists.List;
                         Start  : in     Unbounded_String;
                         Finish : in     Unbounded_String);
   function QueryPath (List   : in out NodeLists.List;
                       Start  : in     Unbounded_String;
                       Finish : in     Unbounded_String)
                       return Boolean;

end Graph;
