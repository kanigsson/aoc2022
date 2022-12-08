with SPARK.Containers.Formal.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package FS is

   --  we can probably ignore file names
   type File_Size is new Natural;

   package FileLists is new
     SPARK.Containers.Formal.Vectors (Positive, File_Size, "=");

   type Dir_Id is new Positive;

   package DirLists is new
     SPARK.Containers.Formal.Vectors (Positive, Dir_Id, "=");

   type Dir_Entry is record
      Name : Unbounded_String;
      Subdirs : DirLists.Vector (1000);
      Files   : FileLists.Vector (1000);
      Parent  : Dir_Id;
   end record;

   package Dir_Storage_Pack is new
     SPARK.Containers.Formal.Vectors (Dir_Id, Dir_Entry, "=");

   Data : Dir_Storage_Pack.Vector (1000);

   procedure Init;

   function Root_Dir return Dir_Id is (1);

   procedure New_Dir_Entry (Parent : Dir_Id; Name : Unbounded_String);

   function Get_Subdir (Parent : Dir_Id; Name : Unbounded_String) return Dir_Id;

   procedure New_File (Parent : Dir_Id; Size : File_Size);

   function Size (Dir : Dir_Id) return File_Size;

   function Get_Parent (Dir : Dir_ID) return Dir_Id is
      (Dir_Storage_Pack.Element (Data, Dir).Parent);

   procedure Show_Tree;

end FS;
