with Ada.Containers;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with SPARK.Containers.Formal.Vectors;

procedure Main with SPARK_Mode is
   subtype Crate is Character range 'A' .. 'Z';

   package Vec is new SPARK.Containers.Formal.Vectors (Positive, Crate, "=");

   subtype Stack is Vec.Vector (1000);

   type Stacks_Type is array (Positive range <>) of Stack;

   type Instruction is record
      From, To, Amount: Positive;
   end record;

   procedure Fail (X : String) with No_Return;

   procedure Fail (X : String) is
   begin
      Ada.Text_IO.Put_Line (X);
      raise Program_Error;
   end Fail;

   procedure Apply (S: in out Stacks_Type; I : Instruction) is
      F : Stack renames S (I.From);
      T : Stack renames S (I.To);
      Tmp : Stack;
      E : Crate;
      use type Ada.Containers.Count_Type;
   begin
      for K in 1 .. I.Amount loop
         if Vec.Is_Empty (F) then
            Fail ("stack to take from is empty");
         end if;
         E := Vec.Last_Element (F);
         Vec.Delete (F, Vec.Last_Index (F));
         Vec.Append (Tmp, E);
      end loop;
      for K in 1 .. I.Amount loop
         if Vec.Length (T) = Vec.Capacity (T) then
            Fail ("capacity issue");
         end if;
         E := Vec.Last_Element (Tmp);
         Vec.Delete (Tmp, Vec.Last_Index (Tmp));
         Vec.Append (T, E);
      end loop;
   end Apply;

   procedure Get_Token (File : Ada.Text_IO.File_Type; S : String) is
      C : Character;
   begin
      for I in S'Range loop
         Ada.Text_IO.Get (File, C);
         pragma Assert (C = S (I));
      end loop;
   end Get_Token;


   procedure Parse is
      use Ada.Text_IO;
      Line : String (1 .. 1000);
      Last : Natural;
      Len  : Natural;
      First : Boolean := True;
      File : File_Type;
      Res : Stacks_Type (1 .. 10);
   begin
      Open (File, In_File, "input.txt");
      loop
         Get_Line (File, Line, Last);
         if First then
            pragma Assert ((Last + 1) mod 4 = 0);
            Len := (Last + 1) / 4;
            First := False;
         else
            pragma Assert (Last = 0 or else Len = (Last + 1)/ 4);
         end if;
         exit when Last = 0;
         for I in 1 .. Len loop
            declare
               Ind : constant Positive := (I - 1) * 4 + 2;
               subtype Dig is Character range '0' .. '9';
            begin
               exit when Line (Ind)in Dig;
               if Line (Ind) /= ' ' then
                  Vec.Prepend (Res (I), Line (Ind));
               end if;
            end;
         end loop;
      end loop;
      loop
         declare
            I : Instruction;
         begin
            Get_Token (File, "move");
            Ada.Integer_Text_IO.Get (File, I.Amount);
            Get_Token (File, " from");
            Ada.Integer_Text_IO.Get (File, I.From);
            Get_Token (File, " to");
            Ada.Integer_Text_IO.Get (File, I.To);
            Apply (Res, I);
         end;
         exit when End_Of_File (File);
      end loop;
      for I in 1 .. Len loop
         Ada.Text_IO.Put (Vec.Last_Element (Res (I)));
      end loop;
   end Parse;

begin
   Parse;
end Main;
