with Ada_Containers;

package body Ada_Lib.Unit_Test.Reporter is

   use type Ada_Containers.Count_Type;

   ---------------------------------------------------------------
   overriding
   procedure Report (
      Engine                     : in     Reporter_Type;
      Results                    : in out AUnit.Test_Results.Result'Class;
      Options                    : in     AUnit.Options.AUnit_Options :=
                                             AUnit.Options.Default_Options) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Failure_Count" & Results.Failure_Count'img);
      if Results.Test_Count > 0 then
         AUnit.Reporter.Text.Text_Reporter (Engine).Report (Results, Options);
         if Results.Failure_Count > 0 then
            Set_Failed;
         end if;
      end if;
      Log_Out (Debug);
   end Report;

end Ada_Lib.Unit_Test.Reporter;


