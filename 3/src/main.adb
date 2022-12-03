with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode, Annotate => (Gnatprove, Might_Not_Return) is
   subtype Item_Type is Character range 'A' .. 'z';

   function Priority (X : Item_Type) return Natural is
      P : Natural := Item_Type'Pos (X);
      Result : Natural;
   begin
      if P >= 97 then
         Result := P - 96;
      else
         Result := P - 64 + 26;
      end if;
      return Result;
   end Priority;

   type Pres_Type is array (Item_Type) of Boolean;

   procedure Fail (X : String) with No_Return;

   procedure Fail (X : String) is
   begin
      Put_Line (X);
      raise Program_Error;
   end Fail;

   procedure Single_Scan (X : String; P : out Pres_Type)
     with Annotate => (Gnatprove, Might_Not_Return)
   is
   begin
      P := (others => False);
      for I in X'Range loop
         if X (I) not in Item_Type then
            Fail ("");
         end if;
         P (X (I)) := True;
      end loop;
   end Single_Scan;

   function Scan (X1, X2, X3 : String) return Item_Type is
      P1, P2, P3 : Pres_Type;
   begin
      Single_Scan (X1, P1);
      Single_Scan (X2, P2);
      Single_Scan (X3, P3);
      for I in Item_Type loop
         if P1 (I) and P2 (I) and P3 (I) then
            return I;
         end if;
      end loop;
      return Item_Type'First;
   end Scan;

   File : File_Type;
   L1, L2, L3 : String (1 .. 1024);
   I1, I2, I3 : Natural;
   Sum : Natural := 0;
   C : Natural;
begin
   Open (File, In_File, "input.txt");
   loop
      Get_Line (File, L1, I1);
      Get_Line (File, L2, I2);
      Get_Line (File, L3, I3);
      C := Priority (Scan (L1 (1 .. I1), L2 (1 .. I2), L3 (1 .. I3)));
      if Natural'Last - C < Sum then
         Fail ("overflow");
      end if;
      Sum := Sum + C;
      exit when End_Of_File (File);
   end loop;
   Ada.Text_IO.Put_Line (Integer'Image (Sum));
end Main;
