with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Main with SPARK_Mode, Annotate => (Gnatprove, Might_Not_Return) is

   procedure Fail (X : String) with No_Return;

   procedure Fail (X : String) is
   begin
      Put_Line (X);
      raise Program_Error;
   end Fail;

   type Range_Rec is record
      First, Last : Integer;
   end record;

   function Overlaps (A, B : Range_Rec) return Boolean is
   begin
      return A.First in B.First .. B.Last or else
        A.Last in B.First .. B.Last or else
        B.First in A.First .. A.Last or else
        B.Last in A.First .. A.Last;
   end Overlaps;

   File : File_Type;

   Unused : Character;
   R1, R2 : Range_Rec;
   Sum : Natural := 0;
begin
   Open (File, In_File, "input.txt");
   loop
      Ada.Integer_Text_IO.Get (File, R1.First);
      Get (File, Unused);
      Ada.Integer_Text_IO.Get (File, R1.Last);
      Get (File, Unused);
      Ada.Integer_Text_IO.Get (File, R2.First);
      Get (File, Unused);
      Ada.Integer_Text_IO.Get (File, R2.Last);
      if Overlaps (R1, R2) then
         if Sum = Integer'Last then
            Fail ("overflow");
         end if;
         Sum := Sum + 1;
      end if;
      exit when End_Of_File (File);
   end loop;
   Ada.Text_IO.Put_Line (Integer'Image (Sum));
end Main;
