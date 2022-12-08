with Ada.Text_IO;

package body FS is

   procedure Init is
      Root_Entry : Dir_Entry :=
        (Name => To_Unbounded_String ("/"),
         Subdirs => <>,
         Files => <>,
         Parent  => 1);
   begin
      Dir_Storage_Pack.Append (Data, Root_Entry);
   end Init;

   procedure New_Dir_Entry (Parent : Dir_Id; Name : Unbounded_String) is
      New_Entry : Dir_Entry :=
        (Name => Name,
         Subdirs => <>,
         Files => <>,
         Parent  => Parent);
      Parent_Entry : Dir_Entry := Dir_Storage_Pack.Element (Data, Parent);
   begin
      Dir_Storage_Pack.Append (Data, New_Entry);
      DirLists.Append (Parent_Entry.Subdirs, Dir_Storage_Pack.Last_Index (Data));
      Dir_Storage_Pack.Replace_Element (Data, Parent, Parent_Entry);
   end New_Dir_Entry;

   procedure New_File (Parent : Dir_Id; Size : File_Size) is
      Parent_Entry : Dir_Entry := Dir_Storage_Pack.Element (Data, Parent);
   begin
      FileLists.Append (Parent_Entry.Files, Size);
      Dir_Storage_Pack.Replace_Element (Data, Parent, Parent_Entry);
   end New_File;

   function Get_Subdir (Parent : Dir_Id; Name : Unbounded_String) return Dir_Id is
      Dirs : DirLists.Vector renames Dir_Storage_Pack.Element (Data, Parent).Subdirs;
   begin
      for I in 1 .. DirLists.Last_Index (Dirs) loop
         declare
            Index : Dir_Id := DirLists.Element (Dirs, I);
         begin
            if Dir_Storage_Pack.Element (Data, Index).Name = Name then
               return Index;
            end if;
         end;
      end loop;
      return 1;
   end Get_Subdir;

   function Size (Dir : Dir_Id) return File_Size is
      Files : FileLists.Vector renames Dir_Storage_Pack.Element (Data, Dir).Files;
      Dirs : DirLists.Vector renames Dir_Storage_Pack.Element (Data, Dir).Subdirs;
      Sum : File_Size := 0;
   begin
      for I in 1 .. FileLists.Last_Index(Files) loop
         Sum := Sum + FileLists.Element (Files, I);
      end loop;
      for I in 1 .. DirLists.Last_Index (Dirs) loop
         Sum := Sum + Size (DirLists.Element (Dirs, I));
      end loop;
      return Sum;
   end Size;

   procedure Show_Tree is

      procedure Print_With_Indent (S : String; Indent : Natural) is
      begin
         for I in 1 .. Indent loop
            Ada.Text_IO.Put (" ");
         end loop;
         Ada.Text_IO.Put_Line (S);
      end Print_With_Indent;

      procedure Show_Tree_Internal (D : Dir_Id; Indent : Natural) is
         Dir : Dir_Entry renames Dir_Storage_Pack.Element (Data, D);
      begin
         Print_With_Indent (To_String (Dir.Name), Indent);
         for I in 1 .. FileLists.Last_Index (Dir.Files) loop
            Print_With_Indent (File_Size'Image (FileLists.Element (Dir.Files, I)), Indent);
         end loop;
         for I in 1 .. DirLists.Last_Index (Dir.Subdirs) loop
            Show_Tree_Internal (DirLists.Element (Dir.Subdirs, I), Indent + 1);
         end loop;
      end Show_Tree_Internal;
   begin
      Show_Tree_Internal (1, 0);
   end Show_Tree;

end FS;
