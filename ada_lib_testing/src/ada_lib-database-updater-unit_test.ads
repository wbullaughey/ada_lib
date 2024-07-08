package Ada_Lib.Database.Updater.Unit_Test is

   type Subscription_Type is new Ada_Lib.Database.Updater.Abstract_Updater_Type with null record;

   overriding
   function Name_Value (
      Subscription               : in     Subscription_Type
   ) return Name_Value_Type'class;

   overriding
   procedure Update (
     Subscription               : in out Subscription_Type;
     Address                    : in     Ada_Lib.Database.Updater.Abstract_Address_Type'class;
     Tag                        : in     String;
     Value                      : in     String;
     Update_Kind                : in     Ada_Lib.Database.Updater.Update_Kind_Type;
     From                       : in     String := Ada_Lib.Trace.Here);

end Ada_Lib.Database.Updater.Unit_Test;
