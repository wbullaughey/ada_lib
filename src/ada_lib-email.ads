with Ada.Finalization;
with Ada_Lib.Socket_IO.Client;
with Ada_Lib.Strings.Unlimited;
with GNATCOLL.Email;

package Ada_Lib.EMail is

   Failed                        : exception;

   type Emailer_Type               is new  Ada.Finalization.
                                    Limited_Controlled with private;

   GMail_Port                    : constant := 465;
   Gmail_Server                  : constant String := "smtp.gmail.com";

   procedure Initialize (
      Emailer                    : in out Emailer_Type;
      Sender_Email               : in     String;
      Sender_Name                : in     String;
      Sender_Password            : in     String;
      Server                     : in     String := Gmail_Server;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type := GMail_Port);

-- type Email_Type               is new Ada.Finalization.
--                                     Controlled with private;
   type Email_Type               is tagged private;

   procedure Send (
      Emailer                    : in out Emailer_Type;
      Email                      : in     Email_Type'class;
      Verbose                    : in     Boolean := False);

   function New_Message (
      Emailer                    : in out Emailer_Type'class;
      To_Email                   : in     String;
      To_Name                    : in     String;
      Subject                    : in     String;
      Contents                   : in     String
   ) return Email_Type;

   Debug                         : Boolean := False;

private
   type Emailer_Type               is new  Ada.Finalization.
                                    Limited_Controlled with record
      Email                      : Ada_Lib.Strings.Unlimited.String_Type;
      Name                       : Ada_Lib.Strings.Unlimited.String_Type;
      Password                   : Ada_Lib.Strings.Unlimited.String_Type;
      Port                       : Ada_Lib.Socket_IO.Port_Type;
      Socket                     : Ada_Lib.Socket_IO.Client.Client_Socket_Type;
      URL                        : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

-- type Message_Access           is access GNATCOLL.Email.Message;
--
   overriding
   procedure Finalize (
      Object                     : in out Emailer_Type);

-- type Email_Type               is new Ada.Finalization.
--                                     Controlled with record
--    Message                    : Message_Access;
-- end record;

   type Email_Type               is tagged record
--    Address                    : Ada_Lib.Strings.Unlimited.String_Type;
      Message                    : GNATCOLL.Email.Message;
--    Subject                    : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

-- overriding
-- procedure Finalize (
--    Object                     : in out Email_Type);

end Ada_Lib.EMail;
