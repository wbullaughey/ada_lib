-- package that provides stream IO for sockets with a timeout
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Time;
with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks; use Ada_Lib.Trace_Tasks;
--with Hex_IO;
with Interfaces;

package body Ada_Lib.Socket_IO.Stream_IO is

   use type Ada_Lib.Strings.String_Constant_Access;
   use type Ada_Lib.Strings.String_Access_All;
-- use type Ada_Lib.Time.Time_Type;
-- use type Ada_Lib.Timer.Event_Class_Access;
   use type Index_Type;
   use type GNAT.Sockets.Error_Type;
   use type GNAT.Sockets.Socket_Type;

-- type Protected_Buffer_Access  is access all Protected_Buffer_Type;

   procedure Free is new Ada.Unchecked_Deallocation (
      Name     => GNAT.Sockets.Stream_Access,
      Object   => Ada.Streams.Root_Stream_Type'Class);

   procedure Free is new Ada.Unchecked_Deallocation (
      Object   => Input_Task,
      name  => Input_Task_Access);

   procedure Free is new Ada.Unchecked_Deallocation (
      Object   => Output_Task,
      name  => Output_Task_Access);

   procedure Trace_Read (
      Line                 : in   Buffer_Type;
      Last                 : in   Index_Type);

   ---------------------------------------------------------------------------
   overriding
   procedure Close (
      Socket                     : in out Stream_Socket_Type) is
   ---------------------------------------------------------------------------

      Close_Flag                 : constant Buffer_Type (1 .. 1) := (
                                    1 => Data_Type (Character'Pos (
                                       Character'last)));
      Event                      : Event_Type;

   begin
      Log_In (Trace, Socket.Image & " Open " & Socket.Is_Open'img &
         " addresses socket " & Image (Socket'address) &
         " stream " & Image (Socket.Stream'address));

      if not Socket.Is_Open then
         Log_Out (Trace);
         return;
      end if;
log_here;
      Socket_Type (Socket).Close;
log_here;
      Socket.Stream.Socket_Closed := True;
log_here;
      Put (Socket.Stream.Output_Buffer, Close_Flag, Event);
log_here;
      Log_Here (Tracing, "output buffer event " & Event'img);
      delay 0.2;     -- let task complete

      Socket.Stream.Output_Buffer.Set_Event (Closed); -- signal output task to close

      Log_Here (Tracing, "wait for write task to exit");
      while not Socket.Stream.Writer_Stopped loop
         delay 0.1;
      end loop;
      Log_Here (Tracing, "wait for reader task to exit");
log_here ("wait for Reader_Stopped value " & Socket.Stream.Reader_Stopped'img & " address " & Image (Socket.Stream.Reader_Stopped'address));
      while not Socket.Stream.Reader_Stopped loop
         delay 0.1;
      end loop;
      Socket.Stream.Close;
      Log_Out (Trace);

   exception
      when Fault: others =>
         Trace_Exception (Trace, Fault);
         raise;

   end Close;

   ---------------------------------------------------------------------------
   procedure Close (
      Stream               : in out Stream_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Stream.Image &
         " address stream " & Image (Stream'address));

      Stream.Input_Buffer.Set_Event (Closed);
      Stream.Output_Buffer.Set_Event (Closed);
      if Stream.Socket /= Null and then
            Stream.Socket.Is_Open then
         Log_Here (Tracing);
         Socket_Type (Stream.Socket.all).Close;
      end if;
      Free (Stream.Reader);
      Free (Stream.Writer);
      Log_Out (Tracing);
   end Close;

   ---------------------------------------------------------------------------
   overriding
   procedure Connect (
      Socket                     : in out Stream_Socket_Type;
      Server_Name                : in     String;
      Port                       : in     Port_Type;
      Connection_Timeout         : in     Timeout_Type := 1.0;
      Description                : in     String := "";
      Expected_Read_Callback     : access procedure (
         Socket                  : in     Socket_Class_Access) := Null) is
   pragma Unreferenced (Socket, Server_Name, Port, Connection_Timeout, Expected_Read_Callback);
   ---------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Connect;

   ---------------------------------------------------------------------------
   overriding
   procedure Connect (
      Socket                     : in out Stream_Socket_Type;
      IP_Address                 : in     IP_Address_Type;
      Port                       : in     Port_Type;
      Connection_Timeout         : in     Timeout_Type := 1.0;
      Description                : in     String := "";
      Expected_Read_Callback     : access procedure (
         Socket                  : in     Socket_Class_Access) := Null) is
   ---------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Connect;

   ---------------------------------------------------------------------------
   overriding
   procedure Connect (
      Socket                     : in out Stream_Socket_Type;
      Address                    : in     Address_Type'class;
      Port                       : in     Port_Type;
      Connection_Timeout         : in     Timeout_Type := 1.0;
      Description                : in     String := "";
      Expected_Read_Callback     : access procedure (
         Socket                  : in     Socket_Class_Access) := Null) is
   ---------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Connect;

   ---------------------------------------------------------------------------
   procedure Create (
      Stream                     : in out Stream_Type;
      Socket                     : in out Stream_Socket_Type'class;
      Default_Read_Timeout       : in   Duration := No_Timeout;
      Default_Write_Timeout      : in   Duration := No_Timeout;
      Priority                   : in   Ada_Lib.OS.Priority_Type :=
                                          Ada_Lib.OS.Default_Priority) is
   ---------------------------------------------------------------------------

      Reader_Task_Description    : constant String_Constant_Access := new String'(
                                       (if Socket.Description = Null then
                                             "undescribed socket"
                                          else
                                             Socket.Description.all & " reader task"));
      Writer_Task_Description    : constant String_Constant_Access := new String'(
                                       (if Socket.Description = Null then
                                             "undescribed socket"
                                          else
                                             Socket.Description.all & " write task"));

   begin
      Log_In (Tracing, Socket.Image &
         Quote (" reader description", Reader_Task_Description) &
         Quote (" writer description", Writer_Task_Description) &
         " Default_Read_Timeout " &
         Format_Timeout (Default_Read_Timeout) &
         " Default_Write_Timeout " &
         Format_Timeout (Default_Write_Timeout) & Socket.Image);
      Stream.Socket := Socket_Type (Socket)'unchecked_access;
      Stream.GNAT_Stream :=  GNAT.Sockets.Stream (Socket.GNAT_Socket);
      Stream.Default_Read_Timeout := Default_Read_Timeout;
      Stream.Default_Write_Timeout := Default_Write_Timeout;
log_here;
      Stream.Reader := new Input_Task (Reader_Task_Description);
log_here;
      Stream.Reader.Open (Stream'unchecked_access, Priority);
log_here;
      Stream.Writer := new Output_Task (Writer_Task_Description);
log_here;
      Stream.Writer.Open (Stream'unchecked_access, Priority);
      Log_Out (Tracing, "was created " & Stream.Was_Created'img);
   end Create;

   ---------------------------------------------------------------------------
   overriding
   procedure Create_Stream (
      Socket                     : in out Stream_Socket_Type;
      Default_Read_Timeout       : in     Duration := No_Timeout;
      Default_Write_Timeout      : in     Duration := No_Timeout;
      Description                : in     String := "") is
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Socket.Image & Quote (" Description", Description) &
         " socket " & Socket.Image & " no description expected ");
--       "addresses socket " & Image (Socket'address) &
--       " stream " & Image (Socket.Stream'address));

      if Socket.Description = Null then
         Socket.Set_Description (Description);
      elsif Description'length > 0 and then
            Socket.Description.all /= Description then
         Log_Exception (Trace, "missmatch description");
         raise Different_Description with Quote ("Missmatch description. Socket",
            Socket.Image) & Quote ("create description", Description);
      end if;

      Socket.Stream.GNAT_Stream := GNAT.Sockets.Stream (Socket.GNAT_Socket);
      Socket.Stream.Create (
         Socket                  => Socket,
         Default_Read_Timeout    => Default_Read_Timeout,
         Default_Write_Timeout   => Default_Write_Timeout);
      Log_Out (Tracing, "socket " & Socket.Image);
   end Create_Stream;

   ---------------------------------------------------------------------------
   procedure Dump (
      Description          : in     String;
      Data                 : in     Buffer_Type;
      From                 : in     String := Here) is
   ---------------------------------------------------------------------------

      Length               : constant Natural := Data'length;

   begin
      if Length = 0 then
         Put_Line ("dump for " & Description & " for 0 bytes called from " & From);
      else
         Ada_Lib.Trace.Dump (Data'address, Length, 8,
            Ada_Lib.Trace.Width_8, Description & " length" & Length'img, From);
      end if;
   end Dump;

   ---------------------------------------------------------------------------
   overriding
   procedure Finalize (
      Socket                     : in out Stream_Socket_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Socket.Image & " open " & Socket.Is_Open'img & " address " &
         Image (Socket'address));
      if Socket.Is_Open then
         Log_Here (Trace);
         Socket.Close;
      end if;
      Socket_Type (Socket).Finalize;
      Free (Socket.Stream.GNAT_Stream);
      Log_Out (Trace);
   end Finalize;

   ---------------------------------------------------------------------------
   function Image (
      Stream                     : in   Stream_Type
   ) return String is
   ---------------------------------------------------------------------------

   begin
      return "socket " & (if Stream.Socket = Null then
            "null"
         else
            Stream.Socket.Image);
   end Image;

   ---------------------------------------------------------------------------
   function In_Buffer (
      Stream               : in   Stream_Type
   ) return Index_Type is
   ---------------------------------------------------------------------------

   begin
      return Stream.Input_Buffer.In_Buffer;
   end In_Buffer;

   ---------------------------------------------------------------------------
   overriding
   function In_Buffer (
      Socket                     : in   Stream_Socket_Type
   ) return Index_Type is
   ---------------------------------------------------------------------------

   begin
      return Socket.Stream.In_Buffer;
   end In_Buffer;

   ---------------------------------------------------------------------------
   overriding
   procedure Initialize (
      Socket                     : in out Stream_Socket_Type) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Socket.Image & " socket address " & Image (Socket'address));
      Socket_Type (Socket).Initialize;
      Log_Out (Trace);

   exception
      when Fault: others =>
         Trace_Exception (Fault, Here);
         raise;

   end Initialize;

   -----------------------------------------------------------------------
   procedure Put (
      Buffer                     : in out Protected_Buffer_Type;
      Data                    : in     Buffer_Type;
      Event                   :    out Event_Type) is
   -----------------------------------------------------------------------

   begin
      Log_In (Tracing, "length" & Data'length'img );
      Buffer.Prime_Output (Data'length);
      Buffer.Put (Data, Event);
      Log_Out (Tracing);
   end Put;

   ---------------------------------------------------------------------------
   procedure Read (
      Stream                     : in out Stream_Type;
      Item                       :    out Buffer_Type;
      Last                       :    out Index_Type; -- in Item
      Wait                       : in     Boolean;
      Throw_Exception            : in     Boolean;
      Timeout_Length             : in     Duration := No_Timeout) is
   ---------------------------------------------------------------------------

   begin
      Ada_Lib.Trace.Log_In (Trace,
         Index_Type'image (Item'first) & " .." &
         Index_Type'image (Item'last) &
         " socket " & Stream.Image &
--       " input buffer " & Image (Stream.Input_Buffer'address) &
         " wait " & Wait'img &
         " state " & Stream.Input_Buffer.Get_State'img &
         " item last" & Item'last'img &
         " Timeout_Length " & Format_Timeout (Timeout_Length) &
         " throw " & Throw_Exception'img);

      Last := 0;
      Stream.Input_Buffer.Set_Event (OK);
      if not Wait and then Stream.Input_Buffer.Empty (Throw_Exception) then
         Log_Out (Trace);
         return;
      end if;

      declare
         Event                   : Event_Type;
         Get_Last                : Index_Type;
         Start_Get               : Index_Type := Item'first;

      begin
         Log_Here (Trace, "Timeout_Length " & Timeout_Length'img &
            " input buffer " & Image (Stream.Input_Buffer'address));

         loop
            declare
               type Timeout_Event_Type
                                 is new Ada_Lib.Timer.Event_Type
                                    with null record;
               overriding
               procedure Callback (
                  Event          : in out Timeout_Event_Type);

               Buffer_Empty      : constant Boolean := Stream.Input_Buffer.Empty (
                                    Throw_Exception);
               Timeout_Event     : aliased Timeout_Event_Type;

               --------------------------------------------------------------
               overriding
               procedure Callback (
                  Event             : in out Timeout_Event_Type) is
               pragma Unreferenced (Event);
               --------------------------------------------------------------

               begin
                  Log_Here (Trace);
                  Stream.Input_Buffer.Set_Event (Timed_Out);
               end Callback;
               --------------------------------------------------------------

            begin
               Log_Here (Trace, "Buffer_Empty " & Buffer_Empty'img &
                  " Timeout_Length " & Timeout_Length'img &
                  " buffer " & Image (Stream.Input_Buffer'address));

               if Timeout_Length /= No_Timeout then
                  Timeout_Event.Initialize (Timeout_Length,
                  "stream " & Stream.Image & " read timeout",
                     False, False);
               end if;

               Stream.Input_Buffer.Get (Item (Start_Get .. Item'last),
                  Get_Last, Event);

               Log_Here (Trace, "Get_Last" & Get_Last'img &
                  " event " & Event'img);

               case Event is

                  when OK =>
                     if Get_Last = Item'last then
                        Last := Item'last;
                        Log_Here (Trace);
                        exit;
                     end if;

                     if not Wait then
                        Last := Get_Last;
                        Log_Here (Trace);
                        exit;
                     end if;

                     Start_Get := Get_Last + 1;
                     Last := Get_Last;

                  when Closed =>
                     if Last > 0 then
                        exit;
                     end if;

                     Log_Exception (Trace, "Peer_Closed");
                     raise Peer_Closed;

                  when Failed =>
                     Log_Exception (Trace, "failed");
                     raise IO_Failed;

                  when Timed_Out =>
                     if Throw_Exception then
                        Log_Exception (Trace, "time out");
                        raise Timeout with "from " & here;
                     else
                        Stream.Close;
                        Log_Here (Trace);
                        exit;
                     end if;

               end case;
            end;
         end loop;
      end;

      if Trace then
         if Last = 0  then
            Log_Here ("zero length read");
         else
            Dump ("read", Item (Item'first .. Last));
         end if;
      end if;

      Log_Out (Trace, "socket " & Stream.Image & " last" & Last'img);

   exception
      when Fault: Timeout =>
         Log_Exception (Trace, Fault, (if Throw_Exception then
               "expected"
            else
               "unexpected") &
            " exception. read timed out" &
            " Throw_Exception " & Throw_Exception'img);
         raise;

      when Fault: others =>
         Trace_Message_Exception (Trace, Fault, "read failed");
         Log_Exception (Trace);
         raise;
   end Read;

   ---------------------------------------------------------------------------
   -- throws timeout if timeout reached and Item array not filled
   procedure Read(
      Stream               : in out Stream_Type;
      Item                 :        out Buffer_Type;
      Timeout_Length       : in     Duration := No_Timeout) is
   ---------------------------------------------------------------------------

      Last                 : Index_Type;

   begin
--    Log_In (Tracing, Stream.Image & " length" & Item'length'img);
      Read (Stream, Item, Last, True, True, Timeout_Length);
--    Log_Out (Tracing, "last" & Last'img);
   end Read;

   ---------------------------------------------------------------------------
   -- throws timeout if timeout reached and Item array not filled
   procedure Read (
      Stream                     : in out Stream_Type;
      Buffer                     :    out Buffer_Type;
      Last                       :    out Index_Type; -- index in Buffer
      Timeout_Length             : in     Duration := No_Timeout) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Stream.Image & " length" & BUFFER'length'img);
      Read (Stream, Buffer, Last,
         Wait              => True,
         Throw_Exception   => False,
         Timeout_Length    => Timeout_Length);

      if Tracing then
         Dump ("read", Buffer);
      end if;
      Log_Out (Tracing, "last" & Last'img);
   end Read;

   ---------------------------------------------------------------------------
   -- returns Last when default timeout reached
   overriding
   procedure Read(
      Stream               : in out Stream_Type;
      Item                 : out Buffer_Type;
      Last                 : out Index_Type) is -- index in Buffer
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Stream.Image & " length" & Item'length'img);
      Read (Stream, Item, Last, True, False, Stream.Default_Read_Timeout);
      if Tracing then
         Trace_Read (Item, Last);
      end if;
      Log_Out (Tracing, "last" & Last'img);
   end Read;

   ---------------------------------------------------------------------------
   overriding
   procedure Read (
      Socket                     : in out Stream_Socket_Type;
      Buffer                     :    out Buffer_Type;
      Timeout_Length             : in     Duration := No_Timeout) is
   ---------------------------------------------------------------------------

   begin
      Socket.Stream.Read (Buffer, Timeout_Length);
   end Read;

   ---------------------------------------------------------------------------
   overriding
   procedure Read (
      Socket                     : in out Stream_Socket_Type;
      Buffer                     :    out Buffer_Type;
      Last                       :    out Index_Type;    -- index in Buffer
      Timeout_Length             : in     Duration := No_Timeout) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Socket.Image & " length" & Buffer'length'img &
         " timeout " & Format_Timeout (Timeout_Length));

      Socket.Stream.Read (Buffer, Last,
         Wait              => True,
         Throw_Exception   => False,
         Timeout_Length    => Timeout_Length);

      if Tracing then
         if Last = 0  then
            Log_Here ("zero length read");
         else
            Dump ("read", Buffer (Buffer'first .. Last));
         end if;
      end if;
      Log_Out (Tracing, "last" & Last'img);
   end Read;

   ---------------------------------------------------------------------------
   -- returns just data available without wait
   procedure Read_Immediate (
      Stream               : in out Stream_Type;
      Item                 : out Buffer_Type;
      Last                 : out Index_Type) is -- index in Buffer
   ---------------------------------------------------------------------------

   begin
      Log_In (Tracing, Stream.Image);
      Read (Stream, Item, Last, False, False, 0.0);
      if Tracing then
         Trace_Read (Item, Last);
      end if;
      Log_Out (Tracing, "last" & Last'img);
   end Read_Immediate;

   ---------------------------------------------------------------------------
   function Reader_Stopped (
      Socket                     : in   Stream_Socket_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      Log_Here (Tracing, Socket.Image & " reader stopped " & Socket.Stream.Reader_Stopped'img);
log_here ("get Reader_Stopped normal exit value " & Socket.Stream.Reader_Stopped'img & " address " & Image (Socket.Stream.Reader_Stopped'address));

      return Socket.Stream.Reader_Stopped;
   end Reader_Stopped;

   ---------------------------------------------------------------------------
   function Socket_ID (
      Socket                     : in   Stream_Socket_Type
   ) return String is
   ---------------------------------------------------------------------------

   begin
      return GNAT.Sockets.Image (Socket.GNAT_Socket);
   end Socket_ID;

   ---------------------------------------------------------------------------
   procedure Trace_Read (
      Line                 : in   Buffer_Type;
      Last                 : in   Index_Type) is
   ---------------------------------------------------------------------------

   begin
      if Last < Line'first then
         Log_Here ("read null line");
      else
         Dump ("read", Line (Line'first .. Last));
      end if;
   end Trace_Read;

   ---------------------------------------------------------------------------
   function Was_Created (
      Stream               : in     Stream_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      return Stream.Socket /= Null;
   end Was_Created;

   ---------------------------------------------------------------------------
   overriding
   procedure Write (
      Socket                     : in out Stream_Socket_Type;
      Buffer                     : in     Buffer_Type) is
   ---------------------------------------------------------------------------

   begin
--    Log_In (Tracing, Socket.Image);
      Socket.Stream.Write (Buffer);
--    Log_Out (Tracing);
   end Write;

   ---------------------------------------------------------------------------
   overriding
   procedure Write(
      Stream               : in out Stream_Type;
      Item                 : in   Buffer_Type) is
   ---------------------------------------------------------------------------

   begin
--    Log_In (Tracing, Stream.Image);
      Write (Stream, Item, Stream.Default_Write_Timeout);
--    Log_Out (Tracing);
   end Write;

   ---------------------------------------------------------------------------
   procedure Write (
      Stream               : in out Stream_Type;
      Item                 : in   Buffer_Type;
      Timeout_Length       : in   Duration) is
   ---------------------------------------------------------------------------

   begin
      Log_In (Trace, Stream.Image & " " &
         " length" & Item'length'img &
         Index_Type'image (Item'first) & " .." &
         Index_Type'image (Item'last) & " timeout " &
            Format_Timeout (Timeout_Length));
--       " output buffer " & Image (Stream.Output_Buffer'address));
      if Trace then
         Dump ("write", Item);
      end if;

      declare
         Event                   : Event_Type;
         Left                    : Index_Type := Item'length;
         Start_Put               : Index_Type := Item'first;
--       Timeout_Set             : constant Boolean := Timeout_Length /= No_Timeout;
--       Timeout_Event           : constant Timer_Event_Access := (
--                                  if Timeout_Set then
--                                     Allocate_Event (
--                                        Buffer   => Stream.Input_Buffer'unchecked_access,
--                                        Description
--                                                 => "write",
--                                        Wait     => Timeout_Length)
--                                  else
--                                     Null);
--
      begin
         loop
            declare
               Length            : constant Index_Type := (
                                    if Left > Buffer_Length then
                                       Buffer_Length
                                    else
                                       Left);
            begin
               Log_Here (Trace, "start_put" & Start_Put'img &
                  " Length" & Length'img );
               Put (Stream.Output_Buffer, Item (Start_Put .. Start_Put + Length - 1),
                  Event); -- will block until room

               Start_Put := Start_Put + Length;
               Left := Left - Length;
               case Event is

                  when OK =>  -- | Buffer_Limit =>
                     if Left = 0 then   -- last buffer
                        exit;
                     end if;

                  when Closed =>
                     Log_Exception (Trace, "peer closed");
                     raise Peer_Closed;

                  when Failed =>
                     Log_Exception (Trace, "io failed");
                     raise IO_Failed;

                  when Timed_Out =>
                     Log_Exception (Trace, "time out");
                     raise Timeout with "from " & here;

               end case;
            end;
         end loop;
--log_here("Timeout_Set " & Timeout_Set'img);
--         if Timeout_Set then
--            declare
--               Canceled : Boolean;
--
--            begin
--log_here;
--               Canceled := Timeout_Event.Cancel;
--log_here("canceled " & canceled'img);
----             pragma Unreferenced (Canceled);
--            end;
--         end if;

      end;
      Log_out (Trace);

   exception
      when Fault: others =>
         Log_Exception (Trace, FAult);
         raise;

   end Write;

   ---------------------------------------------------------------------------
   function Writer_Stopped (
      Socket                     : in   Stream_Socket_Type
   ) return Boolean is
   ---------------------------------------------------------------------------

   begin
      Log_Here (Tracing, Socket.Image & " writer stopped " & Socket.Stream.Writer_Stopped'img);

      return Socket.Stream.Writer_Stopped;
   end Writer_Stopped;
   ---------------------------------------------------------------------------

   protected body Protected_Buffer_Type is

      -----------------------------------------------------------------------
      function Empty (
         Throw_Expression        : in     Boolean
      ) return Boolean is
      -----------------------------------------------------------------------

      begin
         Log_In (Tracing, Kind'img & " Buffer_Count" & Buffer_Count'img &
            " state " & State'img);

         case State is

            when Ok =>
               Log_Out (Tracing,  (if Buffer_Count = 0 then "" else "not ") & "empty");
               return Buffer_Count = 0;

            when Closed | Failed =>
               Log_Out (Tracing,  (if Buffer_Count = 0 then "" else "not ") & "empty");
               return Buffer_Count = 0;

            when Timed_Out =>
               if Throw_Expression then
                  Log_Exception (Tracing);
                  raise IO_Failed with "buffer state " & State'img & " not ok at " & Here;
               else
                  Log_Out (Tracing, "empty");
                  return True;
               end if;

         end case;
      end Empty;

      -----------------------------------------------------------------------
      -- get as much data as available up to size of data
      -- will wait if buffer was empty as long as state is ok
      entry Get (
         Data                    :    out Buffer_Type;
         Last                    :    out Index_Type;
         Event                   :    out Event_Type
      ) when Buffer_Count > 0 or else State /= Ok is
      -----------------------------------------------------------------------

      begin
         Log_In (Tracing, Kind'img & " State " & State'img &
            " Buffer_Count" & Buffer_Count'img &
            " tail" & Tail'img & " data first" & Data'first'img);

         case State is

         when Closed =>
            Log_Exception (Trace, "socket closed");
            raise Aborted with "Get called with closed socket at " & Here;

         when OK =>
            if Buffer_Count = 0 then
               State := Failed;
               Log_Exception (Tracing, "time out");
               raise Timeout with "buffer empty and state ok called from " & Here;
            end if;

            Last := Data'last;

            for Index in Data'range loop
               Data (Index) := Buffer (Tail);

               if Tail = Buffer'last then
                  Tail := Buffer'first;
               else
                  Tail := Tail + 1;
               end if;

               Buffer_Count := Buffer_Count - 1;

               if Buffer_Count = 0 then
--                State := Buffer_Limit;
                  Last := Index;
                  if Tracing then
                     Dump ("got", Data (Data'first .. Last));
                  end if;

                  exit;
               end if;
            end loop;

         when Timed_Out =>
            Event := Timed_Out;
            Log_Out (Tracing, "timed out");
            return;

         when Failed =>
            Log_Exception (Tracing);
            raise Aborted with "unexpected state " & State'img & " in get";

         end case;

         Event := State;

         Log_Out (Tracing, "Event " & Event'img & " Last" & Last'img &
            " tail" & Tail'img &
            " Buffer_Count" & Buffer_Count'img);
--          " data " & Image (Data'address) &
--          " buffer " & Image (Buffer'address));
      end Get;

      -----------------------------------------------------------------------
      function Get_State return Event_Type is
      -----------------------------------------------------------------------

      begin
         return State;
      end Get_State;

      -----------------------------------------------------------------------
      function In_Buffer
      return Index_Type is
      -----------------------------------------------------------------------

      begin
         Log_Here (Tracing, Kind'img & " Buffer_Count" & Buffer_Count'img &
            " state " & State'img);

         if State /= Ok then
            raise IO_Failed with "buffer state " & State'img & " not ok at " & Here;
         end if;
         return Buffer_Count;
      end In_Buffer;

      -----------------------------------------------------------------------
      procedure Prime_Output (
         Length                  : in     Index_Type) is
      -----------------------------------------------------------------------

      begin
         Primed_Output := Length;
      end Prime_Output;

      -----------------------------------------------------------------------
      entry Put (
         Data                    : in     Buffer_Type;
         Event                   :    out Event_Type
      ) when Buffer_Length - Buffer_Count >= Primed_Output or else State /= Ok is
      -- block when not room in buffer or bad state
      -----------------------------------------------------------------------

      begin
         Log_In (Tracing, Kind'img & " primed" & Primed_Output'img &
            " head" & Head'img &
            " buffer count" & Buffer_Count'img & " state " & State'img);
         pragma Assert (Primed_Output > 0, "not primed for put");
         Event := State;
         case State is

          when Ok =>
            null;

         when Timed_Out =>
            Log_Here (Tracing, "timed out");
            State := Ok;      -- more data now availabl
--          Timeout_Time := Ada_Lib.Time.No_Time;
--          return;

         when Closed =>
            Log_Out (Tracing, "closed");
            return;

         when Failed =>
            Log_Exception (Tracing, "failed");
            raise IO_Failed with "buffer state " & State'img & " not ok at " & Here;
         end case;

         for Index in Data'range loop
            Buffer (Head) := Data (Index);

            if Head = Buffer'last then
               Head := Buffer'first;   -- loop around
            else
               Head := Head + 1;
            end if;

            Buffer_Count := Buffer_Count + 1;

            if Buffer_Count = Buffer'length then -- buffer is full, block until there is room
               if Tracing then
                  Dump ("put full ", Buffer);
               end if;

               Log_Out (Trace, "Buffer_Count" & Buffer_Count'img &
                  " head" & Head'img & " event " & Event'img);
               return;
            end if;
         end loop;

         if Tracing then
            Dump ("put all data ", Data);
            Dump ("Buffer", Buffer);
         end if;

         Log_Out (Tracing, "Buffer_Count" & Buffer_Count'img &
            " Head" & Head'img & " event " & Event'img);
      end Put;

      -----------------------------------------------------------------------
      procedure Set_Event (
         Event          : in     Event_Type;
         From           : in     String := Here) is
      -----------------------------------------------------------------------

         type Change_Action_Type is (Allow, Ignore, No_Change, Raise_Exception);


         Valid_Transitions    : constant array (
                                 Event_Type'range,                   -- current
                                 Event_Type'range) of Change_Action_Type := (   -- new
                                    Closed      => (
                                       Closed      => No_Change,
                                       Failed      => Allow,
                                       OK          => Raise_Exception,
                                       Timed_Out   => Raise_Exception
                                    ),
                                    Failed      => (
                                       Closed      => Ignore,
                                       Failed      => No_Change,
                                       OK          => Raise_Exception,
                                       Timed_Out   => Raise_Exception
                                    ),
                                    OK          => (
                                       Closed      => Allow,
                                       Failed      => Allow,
                                       OK          => No_Change,
                                       Timed_Out   => Allow
                                    ),
                                    Timed_Out   => (
                                       Closed      => Allow,
                                       Failed      => Allow,
                                       OK          => Allow,
                                       Timed_Out   => No_Change
                                    )
                                 );
      begin
         Log_In (Tracing, Kind'img & " event " & Event'img &
            " old state " & State'img &
            " from " & From);

         case Valid_Transitions (State, Event) is

            when Allow =>
               State := Event;

            when Ignore | No_Change =>
               null;

            when Raise_Exception =>
               raise Aborted with "invalid state transition from " & State'img &
                  " to " & Event'img;
         end case;

         Log_Out (Tracing, "state " & State'img);
      end Set_Event;

      -----------------------------------------------------------------------
      function Timed_Out
      return Boolean is
      -----------------------------------------------------------------------

      begin
         Log_In (Trace, "kind " & Kind'img & " state " & State'img);
         return State = Timed_Out;
--
--       declare
--          Now                     : constant Ada_Lib.Time.Time_Type := Ada_Lib.Time.Now;
--          Result                  : constant Boolean :=
--                                     (if Timeout_Time = Ada_Lib.Time.No_Time then
--                                           False
--                                        else
--                                           Now > Timeout_Time);
--       begin
--          Log_Out (Trace, "now " & From_Start (Now) &
--             " result " & Result'img);
--
--          return Result;
--       end;
--
--    exception
--       when Fault: others =>
--          Trace_Exception (Trace, Fault);
--          Log_Exception (Trace);
--          raise;
--
      end Timed_Out;

   end Protected_Buffer_Type;

--   -----------------------------------------------------------------------
--   package body Timer_Event_Package is
--
--      ---------------------------------------------------------------------------
--      function Allocate_Event (
--         Buffer                     : in     Protected_Buffer_Access;
--         Wait                       : in     Duration;
--         Description                : in String := ""
--      ) return Timer_Event_Access is
--      ---------------------------------------------------------------------------
--
--         Local_Description          : aliased String := Description; -- pointer only
--                                       -- temperarily used
--
--      begin
--         return new Timer_Event_Type (
--            Buffer      => Buffer,
--            Description => (if Description'length = 0 then
--                              Null
--                           else
--                              Local_Description'unchecked_access),
--            Offset      => Ada_Lib.Timer.Wait (Wait));
--      end Allocate_Event;
--
--      ---------------------------------------------------------------------------
--      overriding
--      procedure Callback (
--         Event             : in out Timer_Event_Type) is
--      ---------------------------------------------------------------------------
--
--      begin
--         Log_In (Trace, "Timed_Out");
--   --    if not Event.Cancel then
--   --       raise IO_Failed with "could not cancel event";
--   --    end if;
--         Event.Buffer.Set_Event (Timed_Out);
--         Log_Out (Trace);
--
--      exception
--         when Fault: others =>
--            Trace_Exception (Trace, Fault);
--            raise;
--
--      end Callback;
--
----    ---------------------------------------------------------------------------
----    overriding
----    function Create_Event (
----       Offset                  : in     Ada_Lib.Timer.Duration;
----       Dynamic                 : in     Boolean;
----       Repeating               : in     Boolean;
----       Description             : in     String
----    ) return Timer_Event_Type is
----    ---------------------------------------------------------------------------
----
----       Local_Dexcription       : aliased String := Description;
----
----    begin
----       return Result: Timer_Event_Type (
----          Description => Local_Dexcription'unchecked_access,
----          Buffer      => Null,
----          Offset      => Offset
----       ) do
----          null;
----       end Return;
----    end Create_Event;
----
--   end Timer_Event_Package;

   -----------------------------------------------------------------------
   task body Input_Task is

      Stream_Pointer             : Stream_Access;

   begin
      Log_In (Trace, Quote ("description", Description));
      Ada_Lib.Trace_Tasks.Start ("input task", Here);
      accept Open (
         Stream                  : in   Stream_Access;
         Priority                : in   Ada_Lib.OS.Priority_Type;
         From                    : in   String := Ada_Lib.Trace.Here) do

         Stream_Pointer := Stream;
         Stream_Pointer.Reader_Task_Id := Ada.Task_Identification.Current_Task;
         Ada_Lib.OS.Set_Priority (Priority);
         Stream_Pointer.Reader_Stopped := False;
         Log_Here (Trace, "from " & From &
            " socket " & Stream.Socket.Image);

      end Open;

      loop
         Log_Here (Tracing, "socket " & Stream_Pointer.Socket.Image &
            " closed " & Stream_Pointer.Socket_Closed'img);
--          " addresses socket " & Image (Stream_Pointer.Socket.all'address) &
--          " stream " & Image (Stream_Pointer.all'address));

         if Stream_Pointer.Socket_Closed then      -- socket closed
            Stream_Pointer.Input_Buffer.Set_Event (Closed);
            exit;
         end if;

         declare
            Data              : Buffer_Type (1 .. 1);
            Event             : Event_Type;
            Last_Read         : Index_Type;

         begin
            GNAT.Sockets.Receive_Socket (Stream_Pointer.Socket.GNAT_Socket,
               Data, Last_Read, GNAT.Sockets.Wait_For_A_Full_Reception);
               -- block on one byte read if no data in socet
            Log_Here (Trace, "Last_Read" & Last_Read'img &
               " poll byte " & Hex_IO.Hex (Interfaces.Unsigned_8 (Data (1))) &
               " closed " & Stream_Pointer.Socket_Closed'img);

            if Last_Read = 0 then   -- socket was closed
               Log_Here (Trace, "GNAT socket closed");
               exit;
            end if;
            if Stream_Pointer.Socket_Closed then      -- socket closed
               Stream_Pointer.Input_Buffer.Set_Event (Closed);
               Log_Here (Trace, "loop exit ");
               exit;
            end if;

            Put (Stream_Pointer.Input_Buffer, Data, Event);
            if Event /= Ok then
               raise IO_Failed with "put failed at " & Here &
                  " event " & Event'img;
            end if;

         exception
            when Fault: GNAT.Sockets.Socket_Error =>
--             declare
--                Flag           : Buffer_Type (1 .. 1) := (others =>
--                                  Data_Type (
--                                     Character'Pos (Character'last)));
--                Event          : Event_Type;

               begin
                  Trace_Message_Exception (Trace, Fault,
                     "expected exception Socket closed " & Stream_Pointer.Image);
                  Stream_Pointer.Input_Buffer.Set_Event (Closed);
                  exit;
               end;
         end;

         if Stream_Pointer.Socket_Closed then      -- socket closed
            Stream_Pointer.Input_Buffer.Set_Event (Closed);
            Log_Here (Tracing, "closing socket");
            exit;
         end if;

         declare
            Data              : Buffer_Type (1 .. 1024);
            Last_Read         : Index_Type;
            Size_Request      : GNAT.Sockets.Request_Type := (
                                 Name  => GNAT.Sockets.N_Bytes_To_Read,
                                 Size  => 0);
            Trace_Buffer      : String (1 .. Data'length);
            for Trace_Buffer'Address use Data'Address;

         begin
            -- find out how many more bytes are available to read
            GNAT.Sockets.Control_Socket (Stream_Pointer.Socket.GNAT_Socket,
               Size_Request);

            if Size_Request.Size > 0 then   -- data available, read it
               Log_Here (Tracing, "request size" & Size_Request.Size'img);
               -- read exact number of bytes available limited by size of buffer
               declare
                  Read_Length    : constant Index_Type := Index_Type'min (
                                    Index_Type (Size_Request.Size), Data'length);
               begin
                  GNAT.Sockets.Receive_Socket (Stream_Pointer.Socket.GNAT_Socket,
                     Data (Data'first .. Read_Length), Last_Read);
                  Log_Here (Tracing, "Read_Length" & Read_Length'img &
                     " Last_Read" & Last_Read'img );

                  if Last_Read /= Read_Length then
                     Log_Exception (Trace, "short read");
                     raise IO_Failed with "short read at " & Here &
                        " read" & Last_Read'img & " expected" & Read_Length'img;
                  end if;

                  declare
                     Event       : Event_Type;

                  begin
                     -- put will block until room for whole buffer is available
                     Put (Stream_Pointer.Input_Buffer,
                        Data (Data'first .. Read_Length), Event);
                     if Event /= Ok then
                        Log_Exception (Trace, "put failed");
                        raise IO_Failed with "put failed at " & Here &
                           " event " & Event'img;
                     end if;
                  end;
               end;
            end if;

         exception
            when Fault: GNAT.Sockets.Socket_Error =>
               Trace_Exception (Fault);
               if GNAT.Sockets.Resolve_Exception (Fault) /=
                     GNAT.Sockets.Resource_Temporarily_Unavailable then
                  Stream_Pointer.Input_Buffer.Set_Event (Failed);
               else
                  delay 0.05;
               end if;
         end;
      end loop;
      Log_Here (Trace, "loop exit " & Quote ("description", Description));
log_here ("set Reader_Stopped normal exit " & Quote ("description", Description) & " address " & Image (Stream_Pointer.Reader_Stopped'address));
      Stream_Pointer.Reader_Stopped := True;

      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Trace);

   exception
      when Fault: others =>
         Trace_Message_Exception (Fault, Here, Stream_Pointer.Socket.Image);
log_here ("set Reader_Stopped exception exit " & Image (Stream_Pointer.Reader_Stopped'address));
         Stream_Pointer.Reader_Stopped := True;
         Stream_Pointer.Input_Buffer.Set_Event (Failed);
         Stream_Pointer.Output_Buffer.Set_Event (Failed);
         Ada_Lib.Trace_Tasks.Stop;
         Log_Out (Tracing);

   end Input_Task;

   task body Output_Task is

      Stream_Pointer             : Stream_Access;

   begin
      Log_In (Tracing, Quote ("description", Description));
      Ada_Lib.Trace_Tasks.Start ("output task", Here);

      accept Open (
         Stream                  : in   Stream_Access;
         Priority                : in   Ada_Lib.OS.Priority_Type;
         From                    : in   String := Ada_Lib.Trace.Here) do

         Stream_Pointer := Stream;
         Stream_Pointer.Writer_Task_Id := Ada.Task_Identification.Current_Task;
         Ada_Lib.OS.Set_Priority (Priority);
         Stream_Pointer.Writer_Stopped := False;
         Log_Here (Trace, "from " & From &
            " socket " & Stream.Socket.Image);
--          " socket " & Image (Stream_Pointer.Socket'address));

      end Open;

      loop  -- loop getting buffers to write
         Log_Here (Tracing,
            Stream_Pointer.Socket.Image);
--          GNAT.Sockets.Image (Stream_Pointer.Socket.GNAT_Socket));
         case Stream_Pointer.Output_Buffer.Get_State is

            when Closed =>
               Log_Here (Tracing, "output buffer closed");
               exit;

            when Failed =>
               Log_Here (Tracing, "output buffer failed");
               exit;

            when Ok =>  -- | Buffer_Limit =>
               null;

            when Timed_Out =>
               Log_Here (Tracing, "output buffer timed out");
               exit;

         end case;

         declare
            Data     : Buffer_Type (1 .. 1024);
            Event    : Event_Type;
            Last        : Index_Type;

         begin
            Log_Here (Trace, "data " & Image (Data'address));

            begin
               Stream_Pointer.Output_Buffer.Get (Data, Last, Event);

            exception
               when Fault: Aborted =>     -- socket got closed
                  Trace_Message_Exception (Trace, Fault, "expected");
                  exit;

               when Fault: others =>
                  Trace_Exception (Trace, Fault, Here);
                  raise;

            end;

            Log_Here (Tracing, "event " & Event'img & " last" & Last'img &
               " closed " & Stream_Pointer.Socket_Closed'img);

            if Stream_Pointer.Socket_Closed then
               exit;
            end if;

            case Event is

--             when Buffer_Limit =>
--                raise Aborted with "buffer limit after get";

               when OK =>
                  Log_Here (Tracing);
                  declare
                     Start_Send  : Index_Type := Data'first;
                     Send_Last   : Index_Type;

                  begin
                     loop  -- until whole buffer sent
                        begin
                           if Tracing then
                              Dump ("sending", Data (Start_Send .. Last));
                           end if;

pragma Assert (Stream_Pointer /= Null, "stream pointer null");
pragma Assert (Stream_Pointer.socket /= Null, "socket null");
pragma Assert (Stream_Pointer.socket.GNAT_Socket /= GNAT.Sockets.No_Socket,
   "no socket");
                           GNAT.Sockets.Send_Socket (
                              Stream_Pointer.Socket.GNAT_Socket,
                              Data (Start_Send .. Last), Send_Last);

                           Log_Here (Trace, "Start_Send" & Start_Send'img &
                              " Send_Last " & Send_Last'img &
                              " last " & Last'img & " on socket" &
                                 Stream_Pointer.Socket.Image);

                           if Send_Last = Last then
                              exit;
                           end if;

                           Start_Send := Send_Last + 1;

                        exception
                           when Fault: GNAT.Sockets.Socket_Error =>
                              Trace_Exception (Trace, Fault,
                                 "stream " & Image (Stream_Pointer.all'address));

                              if GNAT.Sockets.Resolve_Exception (Fault) /=
                                    GNAT.Sockets.Resource_Temporarily_Unavailable then
                                 Stream_Pointer.Output_Buffer.Set_Event (Failed);
                                 exit;
                              else
                                 delay 0.05;
                              end if;
                        end;
                     end loop;
                  end;

               when Closed =>
                  Log_Here (Tracing);
                  exit;

               when Failed | Timed_Out =>
                  null;

            end case;
         end;
      end loop;
      Log_Here (Tracing, "socket " & Image (Stream_Pointer.Socket'address));
      Stream_Pointer.Writer_Stopped := True;

      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Tracing);

   exception
      when Fault: others =>
         Trace_Exception (Fault, Here);
         Stream_Pointer.Writer_Stopped := True;
         Ada_Lib.Trace_Tasks.Stop;
         Log_Out (Tracing);

   end Output_Task;

begin
--Trace := True;
--Tracing := True;
   Log_Here (Elaborate or Tracing);
end Ada_Lib.Socket_IO.Stream_IO;

