with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Ada_Lib.Database.Updater.Unit_Test is

   ---------------------------------------------------------------
   overriding
   function Name_Value (
      Subscription               : in     Subscription_Type
   ) return Name_Value_Type'class is
   ---------------------------------------------------------------

--    Result                     : Name_Value_Type;
   begin
      return Name_Value_Type'(
         Ada_Lib.Database.Name_Index_Tag_Type'(
            Index          => Subscription.Index,
            Name           => Ada_Lib.Strings.Unlimited.Coerce (Subscription.Name),
            Tag            => Ada_Lib.Strings.Unlimited.Coerce (Subscription.DBDaemon_Tag)
         ) with
            Value       => Ada_Lib.Strings.Unlimited.Null_String);
   end Name_Value;


  ---------------------------------------------------------------------------------
  overriding
  procedure Update (
     Subscription               : in out Subscription_Type;
     Address                    : in     Ada_Lib.Database.Updater.Abstract_Address_Type'class;
     Tag                        : in     String;
     Value                      : in     String;
     Update_Kind                : in     Ada_Lib.Database.Updater.Update_Kind_Type;
     From                       : in     String := Ada_Lib.Trace.Here) is
  ---------------------------------------------------------------------------------

  begin
     Log_In (Debug_Subscribe, Subscription.Image);
--      Subscription.Set_Value (Value);

--      case Update_Kind is
--
--         when Ada_Lib.Database.Updater.Internal =>
--            null;
--
--         when others =>
--            Subscription.Update_Count := Subscription.Update_Count + 1;
--
--      end case;
     Log (Debug_Subscribe, Here, Who & Subscription.Image &
        " update count" & Subscription.Update_Count'img & " update kind " & Update_Kind'img &
        " subscription tag " & Tag_Name (Ada_Lib.Database.Updater.Unit_Test.Subscription_Type'class (Subscription)'tag) &
        " subscription address " & Image (Subscription'address) & " from " & From);
  end Update;


end Ada_Lib.Database.Updater.Unit_Test;
