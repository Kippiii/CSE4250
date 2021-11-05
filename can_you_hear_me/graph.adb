package body Graph is

   --  Adds a connection between two nodes in the graph,
   --  creates new nodes if they did not already exist
   --
   --  Inputs:
   --    List: Adjacency list form of the graph
   --    Start: The string representing the start of the new path
   --    Finish: The string representing the end of the new path
   --
   --  Outputs:
   --    List: Updated adjacency list
   procedure CreateLink (List   : in out NodeLists.List;
                         Start  : in     Unbounded_String;
                         Finish : in     Unbounded_String) is
      Found : Boolean := False;
      StartNode : Node;
      StartList : StringLists.List;

      --  Checks if the current position in the list is the start node
      --
      --  Inputs:
      --    Position: Cursor pointing to the current spot in the list
      procedure FindStart (Position : in NodeLists.Cursor);
      procedure FindStart (Position : in NodeLists.Cursor) is
         CurNode : Node;
      begin
         CurNode := NodeLists.Element (Position);
         if CurNode.Name = Start then
            Found := True;
            StartList := CurNode.List;
            StartList.Append (Finish);
            CurNode.List := StartList;
            List.Replace_Element (Position, CurNode);
         end if;
      end FindStart;

   begin
      --  Find a node with Start
      List.Iterate (FindStart'Access);
      --  If node is found
      if Found then
         --  Add Finish to the end of it
         StartList.Append (Finish);
      --  Else
      else
         --  Create new list for start with finish in it
         StartList.Append (Finish);
         List.Append ((Start, StartList, False));
      end if;
   end CreateLink;


   --  Checks the graph to see if a path exists from
   --  the start to the end
   --
   --  Inputs:
   --    List: Adjacency list form of the graph
   --    Start: String form of the first node in the path
   --    Finish: String form of last node in the path
   --  Outputs:
   --    Return: Whether or not such a path exists
   function QueryPath (List   : in out NodeLists.List;
                       Start  : in     Unbounded_String;
                       Finish : in     Unbounded_String)
                       return Boolean is
      Queue : StringLists.List;
      CurrentString : Unbounded_String;
      Found : Boolean := False;
      FoundList : StringLists.List;

      --  Sets a node as being not visited by the search
      --
      --  Input:
      --    Position: The current node in the list being considered
      procedure SetVisited (Position : in NodeLists.Cursor);
      procedure SetVisited (Position : in NodeLists.Cursor) is
         CurrentNode : Node;
      begin
         CurrentNode := NodeLists.Element (Position);
         CurrentNode.Visited := False;
         List.Replace_Element (Position, CurrentNode);
      end SetVisited;

      --  Checks if a node is the one being searched for
      --
      --  Input:
      --    Position: Position of the node being checked
      procedure FindNode (Position : in NodeLists.Cursor);
      procedure FindNode (Position : in NodeLists.Cursor) is
         CurrentNode : Node;
      begin
         CurrentNode := NodeLists.Element (Position);
         if CurrentNode.Name = CurrentString and not CurrentNode.Visited then
            Found := True;
            FoundList := CurrentNode.List;
            CurrentNode.Visited := True;
            List.Replace_Element (Position, CurrentNode);
         end if;
      end FindNode;

      --  Adds a node into the queue
      --
      --  Input:
      --    Position: Position of the node being added
      procedure AddToQueue (Position : in StringLists.Cursor);
      procedure AddToQueue (Position : in StringLists.Cursor) is
      begin
         Queue.Append (StringLists.Element (Position));
      end AddToQueue;

   begin
      --  Set the visited values of all nodes to FALSE
      List.Iterate (SetVisited'Access);
      --  Loop while the queue is not empty and top of the queue is not Finish
      Queue.Append (Start);
      while not Queue.Is_Empty and then Queue.First_Element /= Finish loop
         --  Pop the front of the queue
         CurrentString := Queue.First_Element;
         Queue.Delete_First;
         --  Find this node in list
         Found := False;
         List.Iterate (FindNode'Access);
         --  If this UNVISITED node is found
         if Found then
            --  Add all of the elements in its list to the queue
            FoundList.Iterate (AddToQueue'Access);
         end if;
      end loop;
      --  Return whether the queue is empty
      return not Queue.Is_Empty;
   end QueryPath;
end Graph;
