with Ada.Text_IO; use Ada.Text_IO;
procedure Main with SPARK_Mode, Annotate => (Gnatprove, Might_Not_Return) is

   subtype Opp is Character range 'A' .. 'C';
   subtype Us is Character range 'X' .. 'Z';

   type RPS is (Rock, Paper, Scissors);
   type Outcome is (L, D, W);

   function Convert_Opp (X : Opp) return RPS is
      (case X is when 'A' => Rock, when 'B' => Paper, when 'C' => Scissors);

   function Convert_Us (X : Us) return RPS is
      (case X is when 'X' => Rock, when 'Y' => Paper, when 'Z' => Scissors);

   function Convert_Outcome (X : Us) return Outcome is
      (case X is when 'X' => L, when 'Y' => D, when 'Z' => W);

   type Response_Type is array (RPS, Outcome) of RPS;

   Response : Response_Type :=
     (Rock => (Scissors, Rock, Paper),
      Paper => (Rock, Paper, Scissors),
      Scissors => (Paper, Scissors, Rock));

   function Score (O, U : RPS) return Natural
   is
      Base : constant Natural :=
        (if U = Rock then 1 elsif U = Paper then 2 else 3);
      Mult : constant Natural :=
        (if U = Rock then
           (if O = Paper then 0 elsif O = Rock then 3 else 6)
         elsif U = Paper then
           (if O = Rock then 6 elsif O = Paper then 3 else 0)
         else (if O = Rock then 0 elsif O = Paper then 6 else 3));

      Result : Natural := Base + Mult;
   begin
      return Result;
   end Score;

   procedure Fail (X : String) with No_Return;

   procedure Fail (X : String) is
   begin
      Put_Line (X);
      raise Program_Error;
   end Fail;

   File : File_Type;
   O, U : Character;
   S : Natural := 0;
   C : Natural;
begin
   Open (File, In_File, "input.txt");
   loop
      Get (File, O);
      Get (File, U);
      Get (File, U);
      if O not in Opp then
         Fail ("oppenent's code wrong");
      end if;
      if U not in Us then
         Fail ("our code wrong");
      end if;
      C := Score (Convert_Opp (O), Response (Convert_Opp (O), Convert_Outcome (U)));
      if Natural'Last - C < S then
         Fail ("possible overflow");
      end if;
      S := S + C;
      exit when End_Of_File (File);
   end loop;
   Ada.Text_IO.Put_Line (Integer'Image (S));
end Main;
