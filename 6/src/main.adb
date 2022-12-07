with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Directories;

procedure Main with SPARK_Mode is

   function Read_File_Into_String (Fn : String) return String
   with Global => null;

   function Read_File_Into_String (Fn : String) return String
     with SPARK_Mode => Off
   is
      File_Size : constant Natural := Natural (Ada.Directories.Size (Fn));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin

      --  The read operation below will crash with an empty buffer

      if File_Size = 0 then
         return "";
      end if;

      File_String_IO.Open  (File, Mode => File_String_IO.In_File, Name => Fn);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      return Contents;
   end Read_File_Into_String;

   type Character_Set is array (Character) of Boolean;

   function To_Set (X : String) return Character_Set is
      Result : Character_Set := (others => False);
   begin
      for I in X'Range loop
         Result (X (I)) := True;
      end loop;
      return Result;
   end To_Set;

   function Length (X : Character_Set) return Natural is
      Cnt : Natural := 0;
   begin
      for I in X'Range loop
         pragma Loop_Invariant (Cnt <= Character'Pos (I));
         if X (I) then
            Cnt := Cnt + 1;
         end if;
      end loop;
      return Cnt;
   end Length;

   function All_Different (X : String) return Boolean is
      C : Character_Set := To_Set (X);
   begin
      return Length (C) = X'Length;
   end All_Different;

   function Scan (Input : String) return Integer is
   begin
      if Input'Length <= 13 then
         return 0;
      end if;
      for I in Input'First + 13 .. Input'Last loop
         if All_Different (Input (I - 13 .. I)) then
            return I;
         end if;
      end loop;
      return 0;
   end Scan;

begin
   Ada.Text_IO.Put_Line
     (Integer'Image (Scan (Read_File_Into_String ("input.txt"))));
end Main;
