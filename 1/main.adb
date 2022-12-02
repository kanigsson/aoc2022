with SPARK.Containers.Formal.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode is

   File : File_Type;
   Line : String (1 .. 100);
   Last : Natural := 0;
   Cur : Natural := 0;

   package Vec is new SPARK.Containers.Formal.Vectors (Positive, Natural, "=");

   package Sorting is new Vec.Generic_Sorting ("<");

   Counts : Vec.Vector (1000);
begin
   Open (File, In_File, "input.txt");
   loop
      Get_Line (File, Line, Last);
      if Last = 0 then
         Vec.Append (Counts, Cur);
         Cur := 0;
      else
         Cur := Cur + Natural'Value (Line (1 .. Last));
      end if;
      exit when End_Of_File (File);
   end loop;
   Vec.Append (Counts, Cur);
   Close (File);
   Sorting.Sort (Counts);
   declare
      LI : Natural := Vec.Last_Index (Counts);
   begin
      Put_Line (Natural'Image
                (Vec.Element (Counts, LI)
                   + Vec.Element (Counts, LI - 1)
                   + Vec.Element (Counts, LI - 2)));
   end;
end Main;
