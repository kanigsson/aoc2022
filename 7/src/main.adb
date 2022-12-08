with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with FS;

procedure Main is

   type Command_Type is (CD, LS);

   type Command (Kind : Command_Type := CD) is record
      case Kind is
      when CD =>
         Dir_Name : Unbounded_String;
      when others =>
         null;
      end case;
   end record;

   type Ls_Entry_Kind is (Dir_Kind, File_Kind);
   type Ls_Entry (Kind : Ls_Entry_Kind := Dir_Kind) is record
      Name : Unbounded_String;
      case Kind is
         when Dir_Kind => null;
         when File_Kind =>
            Size : FS.File_Size;
      end case;
   end record;

   function Index (S : String; C : Character) return Integer
     with Pre => (if S'Length > 0 then S'First = 1);

   function Index (S : String; C : Character) return Integer is
   begin
      for I in S'Range loop
         if S (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function Is_Command (S : String) return Boolean is
     (S (S'First) = '$');

   function Parse_Command (S : String) return Command is
      I : Natural := S'First + 2;
   begin
      if S'Length = 4 and then S (I .. S'Last) = "ls" then
         return Command'(Kind => LS);
      end if;
      while S (I) /= ' ' loop
         I := I + 1;
      end loop;
      pragma Assert (S (S'First + 2 .. I - 1) = "cd");
      return
        Command'
          (Kind => CD,
           Dir_Name => To_Unbounded_String (S (I + 1 .. S'Last)));
   end Parse_Command;

   function Parse_Ls_Entry (S : String) return Ls_Entry is
      I : Natural := S'First;
   begin
      if S (S'First .. S'First + 2) = "dir" then
         return Ls_Entry'(Kind => Dir_Kind,
                          Name => To_Unbounded_String (S (S'First + 4 .. S'Last)));
      else
         while S (I) /= ' ' loop
            I := I + 1;
         end loop;
         return Ls_Entry'(Kind => File_Kind,
                          Name => To_Unbounded_String (S (I + 1 .. S'Last)),
                          Size => FS.File_Size'Value (S (S'First .. I - 1)));
      end if;
   end Parse_Ls_Entry;

   procedure Parse (File : File_Type) is
      C : Command;
      E : Ls_Entry;
      Cur_Entry : FS.Dir_Id;
      Line : String (1 .. 1000);
      Last : Natural;
   begin
      Get_Line (File, Line, Last);
      loop
         pragma Assert (Is_Command (Line (1 .. Last)));
         C := Parse_Command (Line (1 .. Last));
         case C.Kind is
            when CD =>
               if C.Dir_Name = To_Unbounded_String ("/") then
                  Cur_Entry := FS.Root_Dir;
               elsif C.Dir_Name = To_Unbounded_String ("..") then
                  Cur_Entry := FS.Get_Parent (Cur_Entry);
               else
                  Cur_Entry := FS.Get_Subdir (Cur_Entry, C.Dir_Name);
               end if;
               exit when End_Of_File (File);
               Get_Line (File, Line, Last);
            when LS =>
               loop
                  exit when End_Of_File (File);
                  Get_Line (File, Line, Last);
                  exit when Is_Command (Line (1 .. Last));
                  E := Parse_Ls_Entry (Line (1 .. Last));
                  case E.Kind is
                  when Dir_Kind =>
                     FS.New_Dir_Entry (Cur_Entry, E.Name);
                  when File_Kind =>
                     FS.New_File (Cur_Entry, E.Size);
                  end case;
               end loop;
         end case;
      end loop;
   end Parse;

   File : File_Type;
   Required : constant FS.File_Size := 30000000;
   Total : constant FS.File_Size := 70000000;
   Need_Free : FS.File_Size;
   Min : FS.File_Size;
   use type FS.File_Size;
begin
   Open (File, In_File, "input.txt");
   FS.Init;
   Parse (File);
   Min := FS.Size (1);
   Need_Free := Required - (Total - Min);
   for I in 1 .. FS.Dir_Storage_Pack.Last_Index (FS.Data) loop
      declare
         Size : FS.File_Size := FS.Size (I);
         use type FS.File_Size;
      begin
         if Size >= Need_Free and then Size < Min then
            Min := Size;
         end if;
      end;
   end loop;
   Close (File);
   Ada.Text_IO.Put_Line (FS.File_Size'Image (Min));
end Main;
