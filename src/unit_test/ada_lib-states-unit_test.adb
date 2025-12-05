with Ada_Lib.Unit_Test;
with AUnit.Test_Cases;

separate (Ada_Lib.States)
package body Unit_Test is




begin
   if Trace_Tests then
      Unit_Test_Debug := Trace_Tests;
   end if;
--Unit_Test_Debug := True;
   Log_Here (Elaborate or Trace_Options);

end Unit_Test;
