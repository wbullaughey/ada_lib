with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Task_Identification;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.OS;
with Ada_Lib.Substiture_For_Non_Alpha;
with Ada_Lib.Time;
with Ask;
with Hex_IO;
with Interfaces.C;
with System.Address_Image;

package body Ada_Lib.Trace is

   use type Ada.Calendar.Time;
   use type Ada.Task_Identification.Task_Id;
   use type System.Address;

   Maximum_Tasks                 : constant := 100;

   type Context_Type             is (Decrement, Report_Exception, Increment,
                                       Raise_Exception, Same);


   type Task_Count_Type          is range 0 .. Maximum_Tasks;
   subtype Task_Index_Type       is Task_Count_Type range 1 .. Maximum_Tasks;

   type Task_Type is record
      Buffer                     : Ada_Lib.Strings.Unlimited.String_Type;
      Level                      : Level_Type := Level_Type'first;
      Task_ID                    : Ada.Task_Identification.Task_ID;
   end record;

   type Tasks_Type               is array (Task_Index_Type) of Task_Type;

   package Locked_Package is

      procedure Dump (
         Address                 : in     System.Address;
         Length                  : in     Natural;
         Width                   : in     Positive;
         Dump_Width              : in     Dump_Width_Type;
         Description             : in     String;
         From                    : in     String);

      procedure Override_Level (
         Level                   : in     Level_Type);

      procedure Pause (
         Prompt                  : in     String;
         From                    : in     String;
         Trace                   : in     Boolean );

      procedure Put (
         Enable                  : in     Boolean := True;
         Context                 : in     Context_Type;
         Message                 : in     String;
         Where                   : in     String;
         Who                     : in     String);

      procedure Replace_Output_File (
         New_File                   : in     File_Class_Access;
         Previous_File              :    out File_Class_Access);

      procedure Trace_Message_Exception (
         Fault                      : in     Ada.Exceptions.Exception_Occurrence;
         Message                    : in     String;
         From                       : in     String);

      protected type Protected_Type is

         procedure Dump (
            Address              : in     System.Address;
            Length               : in     Natural;
            Width                : in     Positive;
            Dump_Width           : in     Dump_Width_Type;
            Description          : in     String;
            From                 : in     String);

         procedure Override_Level (
            Level                   : in     Level_Type);

         procedure Put (
            Enable               : in     Boolean;
            Context              : in     Context_Type;
            Text                 : in     String;
            Where                : in     String;
            Who                  : in     String);

         procedure Pause (
            Prompt               : in     String;
            From                 : in     String;
            Trace                : in     Boolean);

         procedure Trace_Message_Exception (
            Fault                : in     Ada.Exceptions.Exception_Occurrence;
            Message              : in     String;
            From                 : in     String);

      private
         Tasks                   : Tasks_Type;

      end Protected_Type;

   end Locked_Package;

   Check_Address                 : System.Address := System.Null_Address;
   Indent_Amount                 : constant := 2; -- spaces per level
   LF                            : Character renames Ada.Characters.Latin_1.LF;
   Next_Free_Task                : Task_Index_Type := Task_Index_Type'first;
   Start_Time                    : constant Ada.Calendar.Time :=
                                    Ada.Calendar.Clock;

   procedure Format_Output (    -- only call from within locked object
      Output_File             : in     File_Class_Access;
      Text                    : in     String;
      Where                   : in     String;
      Who                     : in     String;
      Task_Data               : in out Task_Type;
      Indent                  : in     Boolean);

-- function Get_Start_Time
-- return Ada.Calendar.Time;

   procedure Put (
      Enable                     : in     Boolean;
      Context                    : in     Context_Type;
      Text                       : in     String;
      Where                      : in     String;
      Who                        : in     String);

   ---------------------------------------------------------------
   function Ask_Pause (
      Manual                     : in     Boolean;
      Prompt                     : in     String
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      if Manual then
         loop
            declare
               Response          : constant Character :=
                                    Ask.Ask_Character (Prompt & "[y/n]");
            begin
               case Response is

                  when 'n' | 'N' =>
                     return False;

                  when 'y' | 'Y' =>
                     return True;

                  when others =>
                     Put_Line (Quote ("unexpected response", Response));

               end case;
            end;
         end loop;
      else
         return True;
      end if;
   end Ask_Pause;

   --------------------------------------------------------------------
   -- return task identification for calling task
   function Current_Task
   return String is
   --------------------------------------------------------------------

   begin
      return Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
   end Current_Task;

   --------------------------------------------------------------------
   procedure Dump (
      Address                 : in     System.Address;
      Length                  : in     Natural;
      Width                   : in     Positive;
      Dump_Width              : in     Dump_Width_Type;
      Description             : in     String;
      From                    : in     String) is
   --------------------------------------------------------------------

   begin
      Locked_Package.Dump (Address, Length, Width, Dump_Width, Description, From);
   end Dump;

   -------------------------------------------------------------------
   function Exception_Info (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "";
      Whom                       : in     String := GNAT.Source_Info.Enclosing_Entity;
      Where                      : in     String := GNAT.Source_Info.Source_Location
   ) return String is
   -------------------------------------------------------------------

   begin
      return Message &
         (if Message'length > 0 then " " else "") &
         Ada.Exceptions.Exception_Name (Fault) & " " &
         Ada.Exceptions.Exception_Message (Fault) &
         " from " & Whom & ":" & Where;
   end Exception_Info;

   --------------------------------------------------------------------
   overriding
   procedure Flush (
      File                       : in     Default_File_Type) is
   --------------------------------------------------------------------

   begin
      Flush (File.File);
   end Flush;

   -------------------------------------------------------------------
   function Format (
      Seconds              : in   Integer;
      Show_Days            : in   Boolean := False
   ) return String is
   -------------------------------------------------------------------

   begin
      if Show_Days and then Seconds >= 86400 then
         return
            Pad (Integer'image (Seconds / 86400)) & ":" &
            Pad (Integer'image ((Seconds / 3600) mod 24)) & ":" &
            Pad (Integer'image ((Seconds / 60) mod 60)) & ":" &
            Pad (Integer'image (Seconds mod 60));
      else
         return
            Pad (Integer'image (Seconds / 3600)) & ":" &
            Pad (Integer'image ((Seconds / 60) mod 60)) & ":" &
            Pad (Integer'image (Seconds mod 60));
      end if;
   end Format;

   --------------------------------------------------------------------
   procedure Format_Output (
      Output_File                : in     File_Class_Access;
      Text                       : in     String;
      Where                      : in     String;
      Who                        : in     String;
      Task_Data                  : in out Task_Type;
      Indent                     : in     Boolean) is
   --------------------------------------------------------------------

   begin
      T (Debug_Trace, "indent " & Indent'img &
         " level" & Task_Data.Level'img & Quote (" text", Text));

      if Include_Program then
         Task_Data.Buffer.Append (Ada.Command_Line.Command_Name & "=> ");
      end if;

      if Include_Task then
         declare
            Current_Task_ID      : constant Ada.Task_Identification.Task_ID :=
                                    Ada.Task_Identification.Current_Task;

         begin
            Task_Data.Buffer.Append (
               Ada.Task_Identification.Image (Current_Task_ID) & ": ");
         end;
      end if;

      if Include_Time then
         Task_Data.Buffer.Append ("[" & From_Start (Ada_Lib.Time.Now,
            Include_Hundreds) & "] ");
      end if;

      Task_Data.Buffer.Append (Where & " " & Who & " (" &
         Ada_Lib.Strings.Trim (Task_Data.Level'img) & ") " & Text);

      T(Debug_Trace, "length" & Task_Data.Buffer.Length'img);
      if Task_Data.Buffer.Length > 0 then
         declare
            Has_LF               : constant Boolean :=
                                    Task_Data.Buffer.Element (
                                       Task_Data.Buffer.Length) = LF;
         begin
            T (Debug_Trace, "has lf " & Has_LF'img);
            if not Has_LF then
               Task_Data.Buffer.Append (LF);
            end if;

            if Indent and then Indent_Trace and then Task_Data.Level > 0 then
               declare
                  Padding           : constant String (1 .. Positive (
                                       Task_Data.Level * Indent_Amount)) := (
                                          others => ' ');
               begin
                  Task_Data.Buffer.Append (Padding);

               exception
                  when Fault: others =>
                     Trace_Message_Exception (Fault,
                        Quote ("text", Text), Where & " " & Who);
                     Ada_Lib.OS.Immediate_Halt (
                        Ada_Lib.OS.Exception_Exit);

               end;
            end if;

            T (Debug_Trace, Quote ("buffer", Task_Data.Buffer));
            Output_File.Output (Task_Data.Buffer.Coerce);
            Output_File.Flush;
            Task_Data.Buffer := Ada_Lib.Strings.Unlimited.Null_String;

         exception
            when Fault: others =>
               Trace_Message_Exception (Fault, Quote ("text", Text),
                  Where & " " & Who);
               Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

         end;
      end if;

      if Check_Address /= System.Null_Address then
         declare
            Value                : Interfaces.Unsigned_64;
            for Value'address use Check_Address;

         begin
            Output_File.Output ("**** " & Hex_IO.Hex (Value) &
               " Address " & Image (Check_Address) & " *****");
         exception
            when Fault: others =>
               Trace_Message_Exception (Fault, Quote ("text", Text),
                  Where & " " & Who);
               Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

         end;
      end if;

   exception

      when Fault: others =>
         Trace_Message_Exception (Fault, Quote ("text", Text) &
            " buffer length" & Task_Data.Buffer.Length'img,
            Where & " " & Who);
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

   end Format_Output;

   --------------------------------------------------------------------
   function From_Start
   return Duration is
   --------------------------------------------------------------------

   begin
      return From_Start (Ada.Calendar.Clock);
   end From_Start;

   --------------------------------------------------------------------
   function From_Start (
      Time                 : in   Ada.Calendar.Time
   ) return Duration is
   --------------------------------------------------------------------

   begin
      return Time - Start_Time;
   end From_Start;

   --------------------------------------------------------------------
   function From_Start (
      Hundreds             : in   Boolean := False;
      Show_Days            : in   Boolean := False
   ) return String is
   --------------------------------------------------------------------

   begin
      return From_Start (Ada.Calendar.Clock, Hundreds, Show_Days);
   end From_Start;

   --------------------------------------------------------------------
   function From_Start (
      Time                 : in   Ada.Calendar.Time;
      Hundreds            : in   Boolean := False;
      Show_Days            : in   Boolean := False
   ) return String is
   --------------------------------------------------------------------

   begin
      if Time = No_Time then
         return "no time";
      else
         return Image ((
            if Start_Time = No_Time then
               0.0
            else
               Time - Start_Time),
            Hundreds, Show_Days);
      end if;
   end From_Start;

-- --------------------------------------------------------------------
-- function Get_Start_Time
-- return Ada.Calendar.Time is
-- --------------------------------------------------------------------
--
-- begin
--    return Start_Time;
-- end Get_Start_Time;

   -------------------------------------------------------------------
   function Image (
      Time              : in   Ada.Calendar.Time;
      Hundreds            : in   Boolean := False
   ) return String is
   -------------------------------------------------------------------

      Year              : Ada.Calendar.Year_Number;
      Month             : Ada.Calendar.Month_Number;
      Day                  : Ada.Calendar.Day_Number;
      Seconds              : Ada.Calendar.Day_Duration;

   begin
      if Time = No_Time then
         return "no time";
      end if;

      Ada.Calendar.Split (Time, Year, Month, Day, Seconds);

      if Hundreds then
         return
            Ada_Lib.Strings.Trim (Year'img) & "/" &
            Pad (Month'img) & "/" &
            Pad (Day'img) & " " &
            Format (Integer (Seconds)) & "." &
            Pad (Integer'Image (Integer (Seconds * 100) mod 100));
      else
         return
            Ada_Lib.Strings.Trim (Year'img) & "/" &
            Pad (Month'img) & "/" &
            Pad (Day'img) & " " &
            Format (Integer (Seconds));
      end if;
   exception
      when Ada.Calendar.Time_Error =>
         return "INVALID";
   end Image;

   -------------------------------------------------------------------
   function Image (
      Time                       : in   Duration;
      Hundreds                  : in   Boolean := False;
      Show_Days                  : in   Boolean := False
   ) return String is
   -------------------------------------------------------------------

   begin
      if Time = Ada_Lib.Time.No_Duration then
         return "no duration";
      end if;

      if Hundreds then
         return Format (Integer (Float'Floor (Float (Time))), Show_Days => Show_Days) & "." &
            Pad (Integer'image (Integer (Time * 100) mod 100));
      else
         if Time >= Duration (Integer'last) or else Time <= Duration (Integer'first) then
            return "****";
         else
            return Format (Integer (Time), Show_Days => Show_Days);
         end if;
      end if;
   exception
      when Ada.Calendar.Time_Error =>
         return "INVALID";
   end Image;

   -------------------------------------------------------------------
   function Image (
      Address              : in   System.Address
   ) return String is
   -------------------------------------------------------------------

   begin
      return System.Address_Image (Address);
   end Image;

   -------------------------------------------------------------------
   function Image (
      Fault                      : Ada.Exceptions.Exception_Occurrence
   ) return String is
   -------------------------------------------------------------------

   begin
      return "Exception Name: " & Ada.Exceptions.Exception_Name (Fault) &
         " Message: " & Ada.Exceptions.Exception_Message (Fault);
   end Image;

   --------------------------------------------------------------------
   function Image_Pointer (                     -- print content of pointer with checking for constraint error
      Address              : in   System.Address;   -- address of pointer
      Bits                 : in     Natural := 32       -- in bits
   ) return String is
   --------------------------------------------------------------------

      function memcpy (
         Destination             : in     System.Address;
         Source                  : in     System.Address;
         Number_Bytes            : in     Interfaces.C.size_t
      ) return System.Address;

      pragma Import (C, memcpy);

   begin
      case Bits is
         when 32 =>
            declare
               Value             : Interfaces.Integer_32;
               Result            : System.Address;
               pragma Unreferenced (Result);

            begin
               Result := memcpy (Value'address, Address, Interfaces.C.size_t (Bits/8));
               return Hex_IO.Hex (Value);
            end;

         when 64 =>
            declare
               Value             : Interfaces.Integer_64;
               Result            : System.Address;
               pragma Unreferenced (Result);

            begin
               Result := memcpy (Value'address, Address, Interfaces.C.size_t (Bits/8));
               return Hex_IO.Hex (Value);
            end;

         when others =>
            raise Trace_Failure with "unsupport number if bits" & Bits'img;
      end case;
   end Image_Pointer;

   -------------------------------------------------------------------
   procedure Log (
      Enable                     : in     Boolean := True;
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Message                    : in     String := "") is
   -------------------------------------------------------------------

   begin
      Put (Enable, Same, Message, Where, "");
   end Log;

   -------------------------------------------------------------------
   procedure Log_Exception (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Put (
         Enable      => Enable,
         Context     => Raise_Exception,
         Text        => Message & LF,
         Where       => Where,
         Who         => Who);
   end Log_Exception;

   -------------------------------------------------------------------
   procedure Log_Exception (
      Enable                     : in     Boolean := True;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Put (
         Enable      => Enable,
         Context     => Report_Exception,
         Text        => LF & "exception: " &
                           Ada.Exceptions.Exception_Name (Fault) & LF &
                        "exception message: " &
                           Ada.Exceptions.Exception_Message (Fault) & LF &
                        (if Message'length > 0 then
                              Quote ("log message", Message) & LF
                           else
                              "") &
                        "caught at " & Where & LF,
         Where       => Where,
         Who         => Who);
--put_Line (here & " enable " & Enable'img & " '" & Where & "'");
   end Log_Exception;

   -------------------------------------------------------------------
   procedure Log_Here (
      Enable                     : in     Boolean;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
--ada.Text_io.put_line (here);
      Put (
         Enable      => Enable,
         Context     => Same,
         Text        => Message & LF,
         Where       => Where,
         Who         => Who);
--ada.Text_io.put_line (here);
   end Log_Here;

   -------------------------------------------------------------------
   procedure Log_Here (
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Log_Here (True, Message, Where, Who);
   end Log_Here;

   -------------------------------------------------------------------
   function Log_Here (
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      Log_Here (Enable, "result " & Result'img & " " & Message, Where, Who);
      return Result;
   end Log_Here;

   -------------------------------------------------------------------
   procedure Log_In (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Put (
         Enable      => Enable,
         Context     => Increment,
         Text        => "in " & Message & LF,
         Where       => Where,
         Who         => Who);
   end Log_In;

   -------------------------------------------------------------------
   procedure Log_In_Checked (
      Recursed                   : in out Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Log_In (Enable, Message, Where, Who);

      if Recursed then
         Put_Line ("recursive call from " & Where & " by " & Who &
            Quote (" message", Message));
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Recursion_Exit);
      else
         Recursed := True;
      end if;
   end Log_In_Checked;

   -------------------------------------------------------------------
   procedure Log_Out (
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Locked_Package.Put (
         Enable      => Enable,
         Context     => Decrement,
         Message     => "out " & Message & LF,
         Where       => Where,
         Who         => Who);
   end Log_Out;

   -------------------------------------------------------------------
   function Log_Out (
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      Log_Out (Enable, "result " & Result'img & " " & Message, Where, Who);
      return Result;
   end Log_Out;

   -------------------------------------------------------------------
   procedure Log_Out_Checked (
      Recursed                   : in out Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      if Recursed then
         Recursed := False;
         Log_Out (Enable, Message, Where, Who);
      else
         raise Recursive_Failure with "not logged in from " & Where & " by " & Who;
      end if;
   end Log_Out_Checked;

   -------------------------------------------------------------------
   function Log_Out_Checked (
      Recursed                   : in     Boolean;
      Result                     : in     Boolean;
      Enable                     : in     Boolean := True;
      Message                    : in     String := "";
      Where                      : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      if Recursed then
--       Recursed := False;
         Log_Out (Enable, "result " & Result'img & " " & Message, Where, Who);
         return Result;
      else
         raise Recursive_Failure with "not logged in from " & Where & " by " & Who;
      end if;
   end Log_Out_Checked;

   -------------------------------------------------------------------
   -- aborts after printing message
   procedure Not_Implemented (
      Why                        : in     String := "";
      Here                       : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      Put (
         Enable      => True,
         Context     => Same,
         Text        => Why & " not implemented" & LF,
         Where       => Here,
         Who         => Who);
      Ada_Lib.OS.Immediate_Halt(Ada_Lib.OS.Not_Implemented_Exit);
   end Not_Implemented;

   --------------------------------------------------------------------
   overriding
   procedure Output (
      File                       : in out Default_File_Type;
      Data                       : in     String) is
   --------------------------------------------------------------------

   begin
--ada.Text_io.put_line (here);
      Put (File.File, Data);
--ada.Text_io.put_line (here);
   end Output;

   -------------------------------------------------------------------
   procedure Override_Level (
      Level                      : in     Level_Type) is
   -------------------------------------------------------------------

   begin
      Locked_Package.Override_Level (Level);
   end Override_Level;

   -------------------------------------------------------------------
   function Pad (
      Source            : in   String
   ) return String is
   -------------------------------------------------------------------

      Trimmed           : constant String := Ada_Lib.Strings.Trim (Source);

   begin
      case Trimmed'length is

         when 0 =>
            return "00";

         when 1 =>
            return "0" & Trimmed;

         when others =>
            return Trimmed;

      end case;
   end Pad;

   -------------------------------------------------------------------
   procedure Pause (
      Prompt                     : in     String := "";
      From                       : in     String := Here;
      Trace                      : in     Boolean := False) is
   -------------------------------------------------------------------

   begin
      Pause (True, Prompt, From, Trace);
   end Pause;

   -------------------------------------------------------------------
   procedure Pause (
      Condition                  : in     Boolean;
      Prompt                     : in     String := "";
      From                       : in     String := Here;
      Trace                      : in    Boolean := False) is
   -------------------------------------------------------------------

   begin
      if Condition then
         Locked_Package.Pause (Prompt, From, Trace);
      end if;
   end Pause;

   -------------------------------------------------------------------
   procedure Pause_On_Flag (
      Prompt                     : in     String;
      From                       : in     String := Here;
      Trace                      : in    Boolean := False) is
   -------------------------------------------------------------------

   begin
      if Pause_Flag then
         Pause (Prompt, From, Trace);
      end if;
   end Pause_On_Flag;

   --------------------------------------------------------------------
   procedure Put (
      Enable                     : in     Boolean;
      Context                    : in     Context_Type;
      Text                       : in     String;
      Where                      : in     String;
      Who                        : in     String) is
   --------------------------------------------------------------------

   begin
--ada.Text_io.put_line (here);
      T (Debug_Trace, "enable " & Enable'img &
         Quote (" text", Text) & Quote (" where", Where) &
         Quote (" who", Who));
--ada.Text_io.put_line (here);
      Locked_Package.Put (Enable, Context, Text, Where, Who);
--ada.Text_io.put_line (here);
   end Put;

   -------------------------------------------------------------------
   function Quote (
      Value                : in   Character
   ) return String is
   -------------------------------------------------------------------

   begin
      return (if Value = Ada.Characters.Latin_1.Nul then
            "NUL"
         else
            String'(1 => Ada_Lib.Substiture_For_Non_Alpha.Mapper (Value)));
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Value                : in   String
   ) return String is
   -------------------------------------------------------------------

   begin
      return "'" & Ada_Lib.Substiture_For_Non_Alpha.Substitute (Value) & "'";
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Value                : in   Ada_Lib.Strings.Unlimited.String_Type
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote (Value.Coerce);
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Value                : in   Ada.Strings.Unbounded.Unbounded_String
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote (Ada.Strings.Unbounded.To_String (Value));
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Variable             : in   String;
      Value                : in   Character
   ) return String is
   -------------------------------------------------------------------

   begin
      return Variable & ": " & Quote (Value);
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Variable             : in   String;
      Value                : in   String
   ) return String is
   -------------------------------------------------------------------

   begin
      return Variable & ": " & Quote (Value);
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Variable             : in   String;
      Value                : access constant String
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote (Variable, (if Value = Null then
            "null pointer"
         else
            Value.all));
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Variable             : in   String;
      Value                : in   Ada_Lib.Strings.Unlimited.String_Type
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote (Variable, Value.Coerce);
   end Quote;

   -------------------------------------------------------------------
   function Quote (
      Variable             : in   String;
      Value                : in   Ada.Strings.Unbounded.Unbounded_String
   ) return String is
   -------------------------------------------------------------------

   begin
      return Quote (Variable, Ada.Strings.Unbounded.To_String (Value));
   end Quote;

   ---------------------------------------------------------------
   procedure Replace_Output_File (
      New_File                   : in     File_Class_Access;
      Previous_File              :    out File_Class_Access) is
   ---------------------------------------------------------------

   begin
      Locked_Package.Replace_Output_File (New_File, Previous_File);
   end Replace_Output_File;

   --------------------------------------------------------------------
   procedure Set_Check_Address (
      Address              : in     System.Address) is
   --------------------------------------------------------------------

   begin
      Check_Address := Address;

      declare
         Value                : Interfaces.Unsigned_64;
         for Value'address use Check_Address;

      begin
         Put_Line ("**** setting " &
            " Address " & Image (Check_Address) &
            " initial value " & Hex_IO.Hex (Value) & " *****");
      end;
   end Set_Check_Address;

   --------------------------------------------------------------------
   procedure T (
      Enable                     : in     Boolean := True;
      What                       : in     String := "";
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity;
      Where                      : in     String :=  GNAT.Source_Info.Source_Location) is
   --------------------------------------------------------------------

   begin
--ada.Text_io.put_line (here & " enable " & enable'img);
      if Enable then
         Put_Line ("------> " & Where & " " & Who & " " & Current_Task &
            (if What'length = 0 then "" else " " & What) &
            " <-------");
      end if;
   end T;

   --------------------------------------------------------------------
   procedure Tag_History (
      Tag_Value                  : in     Ada.Tags.Tag;
      From                       : in     String := GNAT.Source_Info.
                                             Source_Location) is
   --------------------------------------------------------------------

      use type Ada.Tags.Tag;

      This_Tag                   : Ada.Tags.Tag := Tag_Value;

   begin
      Put_Line ("Tag history for " & Ada.Tags.Expanded_Name (Tag_Value) &
         " from " & From);
      loop
         declare
            Parent               : constant Ada.Tags.Tag :=
                                    Ada.Tags.Parent_Tag (This_Tag);
         begin
            if Parent = Ada.Tags.No_Tag then
               Put_Line ("root " & Ada.Tags.Expanded_Name (This_Tag));
               exit;
            else
               Put_Line ("Parent " & Ada.Tags.Expanded_Name (Parent));
               This_Tag := Parent;
            end if;
         end;
      end loop;
   end Tag_History;

   --------------------------------------------------------------------
   function Tag_Name (
      Tag_Value                  : Ada.Tags.Tag
   ) return String is
   --------------------------------------------------------------------

   begin
      return "tag " & Ada.Tags.Expanded_Name (Tag_Value);
   end Tag_Name;

   --------------------------------------------------------------------
   procedure Tag_History (
      Enable                     : in     Boolean;
      Tag_Value                  : in     Ada.Tags.Tag;
      From                       : in     String := GNAT.Source_Info.
                                             Source_Location) is
   --------------------------------------------------------------------

   begin
      if Enable then
         Tag_History (Tag_Value, From);
      end if;
   end Tag_History;

   --------------------------------------------------------------------
   procedure Trace_Exception (
      Debug                      : in   Boolean;
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Where                      : in   String := GNAT.Source_Info.Source_Location) is
   --------------------------------------------------------------------

   begin
      Trace_Message_Exception (Debug, Fault, "", Where);
   end Trace_Exception;

   --------------------------------------------------------------------
   procedure Trace_Exception (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Where                      : in   String := GNAT.Source_Info.Source_Location) is
   --------------------------------------------------------------------

   begin
      Trace_Message_Exception (True, Fault, "", Where);
   end Trace_Exception;

   --------------------------------------------------------------------
   procedure Trace_Message_Exception (
      Debug                      : in   Boolean;
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in   String;
      Where                      : in   String := GNAT.Source_Info.Source_Location) is
   --------------------------------------------------------------------

   begin
      Ada_Lib.Exception_Occured := True;

      if Debug then
         Locked_Package.Trace_Message_Exception (Fault, Message, Where);
      end if;
   end Trace_Message_Exception;

   --------------------------------------------------------------------
   procedure Trace_Message_Exception (
      Fault                      : in   Ada.Exceptions.Exception_Occurrence;
      Message                    : in   String;
      Where                      : in   String :=
                                          GNAT.Source_Info.Source_Location) is
   --------------------------------------------------------------------

   begin
      Trace_Message_Exception (True, Fault, Message, Where);
   end Trace_Message_Exception;

   -------------------------------------------------------------------
   package body Locked_Package is

      Output_File                : File_Class_Access :=
                                    new Default_File_Type'(
                                       File  => Ada.Text_IO.Standard_Output);
      State                      : Protected_Type;

      ---------------------------------------------------------------
      procedure Dump (
         Address                 : in     System.Address;
         Length                  : in     Natural;
         Width                   : in     Positive;
         Dump_Width              : in     Dump_Width_Type;
         Description             : in     String;
         From                    : in     String) is
      ---------------------------------------------------------------


      begin
         State.Dump (Address, Length, Width, Dump_Width,
            Description, From);
      end Dump;

      -------------------------------------------------------------------
      procedure Override_Level (
         Level                      : in     Level_Type) is
      -------------------------------------------------------------------

      begin
         State.Override_Level (Level);
      end Override_Level;

      ---------------------------------------------------------------
      procedure Pause (
         Prompt                     : in     String;
         From                       : in     String;
         Trace                      : in     Boolean) is
      ---------------------------------------------------------------

      begin
         State.Pause (Prompt, From, Trace);
      end Pause;

      ---------------------------------------------------------------
      procedure Put (
         Enable                  : in     Boolean := True;
         Context                 : in     Context_Type;
         Message                 : in     String;
         Where                   : in     String;
         Who                     : in     String) is
      ---------------------------------------------------------------

      begin
--ada.Text_io.put_line (here);
         State.Put (Enable, Context, Message, Where, Who);
--ada.Text_io.put_line (here);
      end Put;

      ---------------------------------------------------------------
      procedure Replace_Output_File (
         New_File                   : in     File_Class_Access;
         Previous_File              :    out File_Class_Access) is
      ---------------------------------------------------------------

      begin
--ada.Text_io.put_line (here & " new file " & Image (New_File.all'address));
         Previous_File := Output_File;
         Output_File := New_File;
--ada.Text_io.put_line (here & " Previous_File " & Image (Previous_File.all'address));
      end Replace_Output_File;

      ------------------------------------------------------------------
      procedure Trace_Message_Exception (
         Fault                   : in     Ada.Exceptions.Exception_Occurrence;
         Message                 : in     String;
         From                    : in     String) is
      -------------------------------------------------------------------

      begin
         Ada_Lib.Exception_Occured := True;
         State.Trace_Message_Exception (Fault, Message, From);
      end Trace_Message_Exception;

      ---------------------------------------------------------------
      protected body Protected_Type is

         ---------------------------------------------------------------
         procedure Dump (
            Address                 : in     System.Address;
            Length                  : in     Natural;          -- in bytes
            Width                   : in     Positive;         -- line to print
            Dump_Width              : in     Dump_Width_Type;  -- format
            Description             : in     String;
            From                    : in     String) is
         ---------------------------------------------------------------

            Routine                 : constant array (Dump_Width_Type) of
                                          access procedure (
                                          Source  : in     System.Address;
                                          Size    : in     Positive;        -- size in bits
                                          Width   : in     Positive;
                                          Message : in     String) := (
                  Hex_IO.Dump_8'access,
                  Hex_IO.Dump_16'access,
                  Hex_IO.Dump_32'access,
                  Hex_IO.Dump_64'access);

         begin
--T(true, "in");
            if Length = 0 then
               Put_Line ("dump for " & Description &
                  " for 0 bytes called from " & From);
            else
               Routine (Dump_Width) (Address, Length * 8, Width,
                  "dump for " & Description & " called from " & From);
            end if;
--T(true, "out");
         end Dump;

         ---------------------------------------------------------------
         procedure Find_Task (
            Result               :    out Task_Index_Type) is
         ---------------------------------------------------------------

            Current_Task_ID            : constant Ada.Task_Identification.Task_ID :=
                                          Ada.Task_Identification.Current_Task;
            Free_Task_Index            : Task_Count_Type := 0;

         begin
            for Index in Tasks'first .. Next_Free_Task - 1 loop
               declare
                  Item                 : Task_Type renames Tasks (Index);

               begin
                  if Item.Task_ID = Current_Task_ID then
                     T(Debug_Trace, "task" & Index'img);
                     Result := Index;
                     return;
                  elsif Free_Task_Index = 0 and then
                        Ada.Task_Identification.Is_Terminated (
                           Item.Task_ID) then
                     Free_Task_Index := Index;
                     T (Debug_Trace, "free index " & Free_Task_Index'img);
                  end if;
               end;
            end loop;

            if Free_Task_Index /= 0 then
               Result := Free_Task_Index;
               T(Debug_Trace, "use free task index" & Result'img);
            else
               declare
                  Task_Index                 : constant Task_Index_Type := Next_Free_Task;

               begin
                  Next_Free_Task := Next_Free_Task + 1;
                  Result := Task_Index;
                  T(Debug_Trace, "new task index" & Result'img);
               end;
            end if;

            Tasks (Result).Task_ID := Current_Task_ID;
         end Find_Task;

         -------------------------------------------------------------------
         procedure Override_Level (
            Level                      : in     Level_Type) is
         -------------------------------------------------------------------

            Task_Index              : Task_Index_Type;

         begin
            Find_Task (Task_Index);

            declare
               Task_Entry           : Task_Type renames
                                       Tasks (Task_Index);

            begin
               Task_Entry.Level := Level;
            end;
         end Override_Level;

         ------------------------------------------------------------------
         procedure Trace_Message_Exception (
            Fault                      : in     Ada.Exceptions.Exception_Occurrence;
            Message                    : in     String;
            From                       : in     String) is
         -------------------------------------------------------------------

            Task_Index              : Task_Index_Type;

         begin
            Ada_Lib.Exception_Occured := True;
            Find_Task (Task_Index);

            declare
               Task_Entry           : Task_Type renames
                                       Tasks (Task_Index);

            begin
               Output_File.Output ("----------- exception --------------"& LF);
               Output_File.Output ("Exception name:" &
                  Ada.Exceptions.Exception_Name (Fault)& LF);
               Output_File.Output ("Exception message:" &
                  Ada.Exceptions.Exception_Message (Fault)& LF);
               if Message'length > 0 then
                  Output_File.Output ("handler message:" & Quote (Message) & LF);
               end if;
               Format_Output (Output_File, "caught at " & From, "", "",
                  Task_Entry, True);
               Output_File.Output ("------------------------------------"& LF);
            end;

         exception

            when Fault: others =>
               Output_File.Output  (Ada.Exceptions.Exception_Name (Fault) &
                  "Exception message:" & Ada.Exceptions.Exception_Message (Fault));
               Ada_Lib.OS.Immediate_Halt(Ada_Lib.OS.Exception_Exit);
         end Trace_Message_Exception;

         ------------------------------------------------------------
         procedure Pause (
            Prompt                  : in     String;
            From                    : in     String;
            Trace                   : in     Boolean) is
         pragma Unreferenced (Trace);
         ------------------------------------------------------------

            Task_Index              : Task_Index_Type;

         begin
            Find_Task (Task_Index);

            declare
               Task_Entry           : Task_Type renames
                                       Tasks (Task_Index);
            begin
               Format_Output (Output_File, "pause called from ", From, "",
                  Task_Entry, False);

               declare
                  Answer               : constant Character :=
                                          Ask.Ask_Character (Prompt);
                  pragma Unreferenced (Answer);
               begin
                  New_Line;
               end;
            end;
         end Pause;

         ------------------------------------------------------------
         procedure Put (
            Enable                  : in     Boolean;
            Context                 : in     Context_Type;
            Text                    : in     String;
            Where                   : in     String;
            Who                     : in     String) is
         ------------------------------------------------------------

            Task_Index              : Task_Index_Type;

         begin
--ada.Text_io.put_line (here);
            Find_Task (Task_Index);
--ada.Text_io.put_line (here);

            declare
               Task_Entry           : Task_Type renames
                                       Tasks (Task_Index);
            begin
--ada.Text_io.put_line (here);
               T (Debug_Trace, "in enable " & Enable'img & " context " & Context'img &
                  " level" & Task_Entry.Level'img & Quote (" text", Text) &
                  " where " & Where & " who " & Who);
--ada.Text_io.put_line (here & " emab;e " & enable'img);
               if Enable then
                  case Context is

                     when Report_Exception =>
                        Ada_Lib.Exception_Occured := True;
                        Output_File.Output (
                           "-------------------- exception ----------------" & LF);

                     when Increment =>
                        Task_Entry.Level := Task_Entry.Level + 1;

                     when others =>
                        null;

                  end case;

--ada.Text_io.put_line (here & " emab;e " & enable'img);
                  Format_Output (Output_File, Text, Where, Who, Task_Entry, True);
--ada.Text_io.put_line (here & " emab;e " & enable'img);
                  case Context is

                     when Decrement =>
                        null;

                     when Report_Exception =>
                        Output_File.Output (
                           "-----------------------------------------------" & LF);

                     when others =>
                        return;

                  end case;

                  if Task_Entry.Level = 0 then
                     Output_File.Output ("missing log in from " &
                        Where & ":" & Who & LF);
                  else
                     Task_Entry.Level := Task_Entry.Level - 1;
                  end if;
               end if;
--ada.Text_io.put_line (here);
               T (Debug_Trace, "out enable " & Enable'img & " context " & Context'img &
                  " level" & Task_Entry.Level'img & Quote (" text", Text));
--ada.Text_io.put_line (here);
            end;
--ada.Text_io.put_line (here);
         end Put;

      end Protected_Type;

   end Locked_Package;
   ---------------------------------------------------------------

   begin
--Debug_Trace := True;
--Elaborate := True;
--Trace_Set_Up := True;
--Trace_Tests := True;
   Include_Hundreds := True;
-- Include_Program := True;
   Include_Task := True;
   Include_Time := True;
   Indent_Trace := True;
   Log_Here (Debug_Trace or Elaborate or Trace_Options or Trace_Tests);

end Ada_Lib.Trace;
