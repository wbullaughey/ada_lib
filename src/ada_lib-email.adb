with Ada.Characters.Latin_1;
-- with Ada.Streams;
-- with Ada.Strings.Fixed;
-- with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
-- with Ada_Lib.Directory;
-- with Ada_Lib.OS;
with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with GNATCOLL.Email.Utils;

package body Ada_Lib.EMail is

   -----------------------------------------------------------------------
   procedure Initialize (
      Emailer                    : in out Emailer_Type;
      Sender_Email               : in     String;
      Sender_Name                : in     String;
      Sender_Password            : in     String;
      Server                     : in     String := Gmail_Server;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type := GMail_Port) is
   -----------------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("sender", Sender_Email) & Quote (" name", Sender_Name) &
         Quote (" server", Server));

      Emailer.URL.Construct (Server);
      Emailer.Email.Construct (Sender_Email);
      Emailer.Name.Construct (Sender_Name);
      Emailer.Password.Construct (Sender_Password);
      Emailer.Port := Port;

      Emailer.Socket.Connect (Server, Port);
--
--    declare
--       Buffer_Length           : constant Natural := EMailer.Socket.In_Buffer;
--       Response                : String (1 .. Buffer_Length);
--       Buffer                  : Ada_Lib.Socket_IO.Buffer_Type (
--                                  1 .. Ada.Streams.Stream_Element_Offset (Buffer_Length));
--       for Buffer'Address use Response'address;
--
--    begin
--       if Buffer_Length > 0 then
--          EMailer.Socket.Read (Buffer, Timeout_Length => 2.0);
--          Log_Here (Debug, Quote ("Response", Response ));
--       end if;
--
--    end;
      Log_Out (Debug);

   exception
      when Fault: Ada_Lib.SOCKET_IO.STREAM_IO.TIMEOUT =>
         Trace_Exception (Debug, Fault, "no response from read");

      when Fault: Ada_Lib.SOCKET_IO.Failed =>
         declare
            Message              : constant String :=
                                    "could not connect to server " &
                                    Server & " port" & Port'img;
         begin
            Trace_Exception (Debug, Fault, Message);
            raise Failed with Message;
         end;

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Initialize;

   -----------------------------------------------------------------------
   overriding
   procedure Finalize (
      Object                     : in out Emailer_Type) is
   -----------------------------------------------------------------------

   begin
   Log_In (Debug);
      Object.Socket.Close;
   Log_Out (Debug);
   end Finalize;

-- -----------------------------------------------------------------------
-- overriding
-- procedure Finalize (
--    Object                     : in out Email_Type) is
-- -----------------------------------------------------------------------
--
--    procedure Free is new Ada.Unchecked_Deallocation
--      (GNATCOLL.Email.Message, Message_Access);
--
-- begin
--    Free (Object.Message);
-- end Finalize;

   -----------------------------------------------------------------------
   function New_Message (
      Emailer                    : in out Emailer_Type'class;
      To_Email                   : in     String;
      To_Name                    : in     String;
      Subject                    : in     String;
      Contents                   : in     String
   ) return Email_Type is
   -----------------------------------------------------------------------

      Address                    : constant GNATCOLL.Email.Email_Address := GNATCOLL.Email.Email_Address'(
                                       Address     => Ada.Strings.Unbounded.
                                                         To_Unbounded_String (To_Email),
                                       Real_Name   => Ada.Strings.Unbounded.
                                                         To_Unbounded_String (To_Name));
      Address_List               : constant GNATCOLL.Email.Charset_String_List.List :=
                                    GNATCOLL.Email.Utils.Format_Address (Address);
      Header                     : constant GNATCOLL.Email.Header := GNATCOLL.Email.Create ("TO", Address_List);
      EMail                    : Email_Type;

   begin
      EMail.Message := GNATCOLL.Email.New_Message;

      EMail.Message.Set_Default_Headers (
         From_Email        => Emailer.Email.Coerce,
         From_Real_Name    => Emailer.Name.Coerce,
         Subject           => Subject);

      EMail.Message.Set_Text_Payload (Contents);
      EMail.Message.Add_Header (Header);
--    EMail.Subject.Construct (Subject);
--    EMail.Contents.Construct (Contents);
--    EMail.To_Email.Construct (To_Email);

      declare
         Text                    : Ada.Strings.Unbounded.Unbounded_String;

      begin
         EMail.Message.To_String (Result => Text);
         Log_Here (Debug, Ada.Strings.Unbounded.To_String (Text));
      end;
      return EMail;
   end New_Message;

   -----------------------------------------------------------------------
   procedure Send (
      Emailer                    : in out Emailer_Type;
      Email                      : in     Email_Type'class;
      Verbose                    : in     Boolean := False) is
   -----------------------------------------------------------------------

      CR_LF                      : constant String := String'(
                                    Ada.Characters.Latin_1.CR &
                                    Ada.Characters.Latin_1.LF);

      ------------------------------------------------------------
      procedure Send_Receive (
         Request                 : in     String) is
      ------------------------------------------------------------

         Last                    : Ada_Lib.Socket_IO.Index_Type;
         Receive_Buffer          : Ada_Lib.Socket_IO.Buffer_Type (1 .. 100);
         Output_Buffer           : constant String := Request & CR_LF;
         Send_Buffer             : Ada_Lib.Socket_IO.Buffer_Type (1 ..
                                    Ada_Lib.Socket_IO.Index_Type (Request'length));
         for Send_Buffer'address use Output_Buffer'address;

      begin
         Log_In (Debug, Quote ("Request", Request));
         Emailer.Socket.Write (Send_Buffer);
         Emailer.Socket.Read (Receive_Buffer, Last, Wait => 0.5);

         declare
            Response             : String (1 .. Natural (Last));
            for Response'address use Receive_Buffer'address;

         begin
            if Verbose then
               Ada.Text_IO.Put_Line (Response);
            end if;
            Log_Out (Debug, Quote ("Response", Response));
         end;

         if    Emailer.Socket.Reader_Stopped or else
               Emailer.Socket.Writer_Stopped then
            raise Failed with "reader or write task stopped";
         end if;

         Log_Out (Debug);
      end Send_Receive;
      ------------------------------------------------------------

      Hello                      : constant String := "HELO " &
                                    "wbullaughey.dyndns-server.com" & CR_LF;
      Login                      : constant String := "AUTH LOGIN" & CR_LF &
                                    Emailer.Name.Coerce & CR_LF & Emailer.Password.Coerce  & CR_LF;
--    From                       : constant String := "MAIL FROM: " & Emailer.Email.Coerce & CR_LF;
--    Reply_To                   : constant String := "RCPT TO: " & Emailer.Name.Coerce & CR_LF;
--    MIME                       : constant String := "text/plain";
      Quit                       : constant String := "QUIT" & CR_LF;
--    Request                    : constant String :=
--                                  "DATE: 4/9/2023" & CR_LF &
--                                  "X-Mailer: Ada_Lib.Mail" & CR_LF &
--                                  "FROM: " & Emailer.Email.Coerce & CR_LF &
--                                  "TO: & To_Address: " & CR_LF &
--                                  "Subject: " & Email.Subject.Coerce & CR_LF &
--                                  "MIME-Version: 1.0" & CR_LF &
--                                  "Content - type: " & MIME & "; charset=" & "UTF-8" & CR_LF & CR_LF;
         Contents                : Ada.Strings.Unbounded.Unbounded_String;

   begin
      Log_In (Debug);

      Send_Receive (Hello);
      Send_Receive (Login);

      EMail.Message.To_String (
         Envelope => True,
         Result   => Contents);

      declare
--       Last                    : Ada_Lib.Socket_IO.Index_Type;
--       Receive_Buffer          : Ada_Lib.Socket_IO.Buffer_Type (1 .. 100);
         Text                    : constant String := Ada.Strings.Unbounded.To_String (Contents);
         Send_Buffer             : Ada_Lib.Socket_IO.Buffer_Type (1 ..
                                    Ada_Lib.Socket_IO.Index_Type (Text'length));
         for Send_Buffer'address use Text'address;

      begin
         Log_Here (Debug, Quote ("Message", Text));
         Send_Receive (Text);
--       Emailer.Socket.Write (Send_Buffer);
--
--       Emailer.Socket.Read (Receive_Buffer, Last, Timeout_Length => 0.5);
--
--       declare
--          Response             : String (1 .. Natural (Last));
--          for Response'address use Receive_Buffer'address;
--
--       begin
--          if Verbose then
--             Ada.Text_IO.Put_Line (Response);
--          end if;
--          Log_Out (Debug, Quote ("Response", Response));
--       end;

         if    Emailer.Socket.Reader_Stopped or else
               Emailer.Socket.Writer_Stopped then
            raise Failed with "reader or write task stopped";
         end if;
      end;
--    Send_Receive (From);
--    Send_Receive (Reply_To);
--    Send_Receive (Request & Email.Message.Coerce);
      Send_Receive (Quit);
      Log_Out (Debug);
   end Send;

-- -----------------------------------------------------------------------
-- procedure Set_Sender (
--    Email                      : in out Emailer_Type;
--    Sender_Email               : in     String;
--    Sender_Name                : in     String) is
-- -----------------------------------------------------------------------
--
-- begin
--    Log_In (Debug, Quote ("email", Sender_Email) & Quote ("name", Sender_Name));
--    Email.Email.Construct (Sender_Email);
--    Email.Name.Construct (Sender_Name);
--    Log_Out (Debug);
-- end Set_Sender;

end Ada_Lib.EMail;
