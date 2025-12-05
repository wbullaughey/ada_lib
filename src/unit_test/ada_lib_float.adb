with Ada_Lib.Float_N;
with Text_IO;

procedure Ada_Lib_Float is
   package X is new Ada_Lib.Float_N (6, 10);

   type Float_Triple       is record
      First             : X.Float_N_Type;
      Second               : X.Float_N_Type;
      Third             : X.Float_N_Type;
   end record;
   pragma Pack (Float_Triple);
   for Float_Triple'size use 48;

   type Float_Triple_Array    is array (Positive range 1..100) of Float_Triple;
   pragma Pack (Float_Triple_Array);

   Test1             : Float := 0.1234;
   Test2             : Float := 1.0;
   Test3             : Float := 0.009999;
   Test4             : Float := 100.0;
   Test5             : Float := 0.9999;
   Test6             : Float := 0.2;
   Test7             : Float := 0.0000000000000001111;
   Test8             : Float := 0.00000000000000001111;
   Test9             : Float := 0.0000000000000001;

begin
   Text_IO.Put_Line ("Float size " & X.Float_N_Type'size'img );
   Text_IO.Put_Line ("Triple Size " & Float_Triple'size'img );
   Text_IO.Put_Line ("Array Size " & Float_Triple_Array'size'img );

   Text_IO.Put_Line (Test1'img & " : " & X.Image (X.Convert( Test1 )));
   Text_IO.Put_Line (Test2'img & " : " & X.Image (X.Convert( Test2 )));
   Text_IO.Put_Line (Test3'img & " : " & X.Image (X.Convert( Test3 )));
   Text_IO.Put_Line (Test4'img & " : " & X.Image (X.Convert( Test4 )));
   Text_IO.Put_Line (Test5'img & " : " & X.Image (X.Convert( Test5 )));
   Text_IO.Put_Line (Test6'img & " : " & X.Image (X.Convert( Test6 )));
   Text_IO.Put_Line (Test7'img & " : " & X.Image (X.Convert( Test7 )));
   Text_IO.Put_Line (Test8'img & " : " & X.Image (X.Convert( Test8 )));
   Text_IO.Put_Line (Test9'img & " : " & X.Image (X.Convert( Test9 )));
end Ada_Lib_Float;