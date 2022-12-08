with Ada.Text_IO;

procedure Main is

   subtype Cell is Character range '0' .. '9';

   type Grid is array (Positive range <>, Positive range <>) of Cell;

   function Read_Grid return Grid is
      use Ada.Text_IO;
      File : File_Type;
      Line : String (1 .. 200);
      Last : Natural;
   begin
      Open (File, In_File, "input.txt");
      Get_Line (File, Line, Last);
      return Result : Grid (1 .. Last, 1 .. Last) do
         for I in 1 .. Last loop
            Result (1, I) := Line (I);
         end loop;
         for K in 2 .. Last loop
            Get_Line (File, Line, Last);
            for I in 1 .. Last loop
               Result (K, I) := Line (I);
            end loop;
         end loop;
         pragma Assert (End_Of_File (File));
         Close (File);
      end return;
   end Read_Grid;

   function Scenic_Score (G : Grid; A, B : Positive) return Natural is
      SCAD, SCAI, SCBD, SCBI : Natural := 0;
      K : Natural;
   begin
      K := A - 1;
      while K in G'Range (1) and then G(K, B) < G (A, B) loop
         K := K - 1;
      end loop;
      SCAD := A - K - (if K in G'Range(1) then 0 else 1);
      K := A + 1;
      while K in G'Range (1) and then G(K, B) < G (A, B) loop
         K := K + 1;
      end loop;
      SCAI := K - A - (if K in G'Range (1) then 0 else 1);
      K := B - 1;
      while K in G'Range (2) and then G(A, K) < G (A, B) loop
         K := K - 1;
      end loop;
      SCBD := B - K - (if K in G'Range (2) then 0 else 1);
      K := B + 1;
      while K in G'Range (2) and then G(A, K) < G (A, B) loop
         K := K + 1;
      end loop;
      SCBI := K - B - (if K in G'Range (2) then 0 else 1);
      return SCAD * SCAI * SCBD * SCBI;
   end Scenic_Score;

   function Is_Visible (G : Grid; A, B : Positive) return Boolean is
   begin
      return (for all I in 1 .. B - 1 => G (A, I) < G (A, B))
        or else (for all I in B + 1 .. G'Last (2) => G (A, I) < G (A, B))
        or else (for all I in 1 .. A - 1 => G (I, B) < G (A, B))
        or else (for all I in A + 1 .. G'Last (1) => G (I, B) < G (A, B));
   end Is_Visible;

   G : Grid := Read_Grid;
   Max : Natural := 0;
begin
   for A in G'Range (1) loop
      for B in G'Range (2) loop
         declare
            SC : Natural renames Scenic_Score (G, A, B);
         begin
            if SC > Max then
               Max := SC;
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line (Natural'Image (Max));
end Main;
