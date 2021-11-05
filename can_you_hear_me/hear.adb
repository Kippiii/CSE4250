--  Author: Ian Orzel, iorzel2019@my.fit.edu
--  Course: CSE 4250, Summer 2021
--  Project: Proj3, Can you hear me now?

with Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Graph;

procedure Hear is
   Valid_Chars : Character_Ranges := (('A', 'Z'),
                                      ('a', 'z'),
                                      ('_', '_'),
                                      ('0', '9'));
   Start, Finish, Temp_Str : Unbounded_String;
   First_Num, Last_Num : Natural;
   Char : Character;

   MyGraph : Graph.NodeLists.List;
begin
   while not Text_IO.End_Of_File loop
      --  Computes the start string of a command
      First_Num := 1;
      Temp_Str := To_Unbounded_String (Text_IO.Get_Line);
      Find_Token (Temp_Str, To_Set (Valid_Chars), Inside, First_Num, Last_Num);
      Start := Unbounded_Slice (Temp_Str, First_Num, Last_Num);
      Temp_Str := Unbounded_Slice (Temp_Str, Last_Num + 1, Length (Temp_Str));

      --  Computes the finish string of a command
      Find_Token (Temp_Str, To_Set (Valid_Chars), Inside, First_Num, Last_Num);
      Finish := Unbounded_Slice (Temp_Str, First_Num, Last_Num);
      Temp_Str := Unbounded_Slice (Temp_Str, Last_Num + 1, Length (Temp_Str));

      --  Finds out what kind of command it is
      Find_Token (Temp_Str, To_Set (".?"), Inside, First_Num, Last_Num);
      Char := Element (Temp_Str, First_Num);

      if Char = '.' then
         --  Creates a link between start and finish
         Graph.CreateLink (MyGraph, Start, Finish);
      else
         --  Outputs whether a path exists between start and finish
         if Graph.QueryPath (MyGraph, Start, Finish) then
            Text_IO.Put ("+ ");
         else
            Text_IO.Put ("- ");
         end if;
         Text_IO.Put (To_String (Start) & " => " & To_String (Finish));
         Text_IO.New_Line;
      end if;
   end loop;
end Hear;
