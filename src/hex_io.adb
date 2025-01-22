--$Header$

-----------------------------------------------------------------------------
--  Copyright (c) 2003 - 2004  All rights reserved
--
--  This file is a product of Communication Automation & Control, Inc. (CAC)
--  and is provided for unrestricted use WITH CAC PRODUCTS ONLY provided
--  this legend is included on all media and as a part of the software
--  program in whole or part.
--
--  Users may copy or modify this file without charge, but are not authorized
--  to license or distribute it to anyone else except as part of a product or
--  program developed by the user incorporating CAC products.
--
--  THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
--  WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
--  PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
--
--  In no event will CAC be liable for any lost revenue or profits, or other
--  special, indirect and consequential damages, which may arise from the use
--  of this software.
--
--  Communication Automation & Control, Inc.
--  1180 McDermott Drive, West Chester, PA (USA) 19380
--  (877) 284-4804 (Toll Free)
--  (610) 692-9526 (Outside the US)
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Text_IO.Integer_IO;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada.Unchecked_Conversion;

package body Hex_IO is

   use type Ada_Lib.Strings.Unlimited.String_Type;

   package Unsigned_8_IO is new Ada.Text_IO.Modular_IO (
      Interfaces.Unsigned_8);

   package Unsigned_16_IO is new Ada.Text_IO.Modular_IO (
      Interfaces.Unsigned_16);

   package Unsigned_32_IO is new Ada.Text_IO.Modular_IO (
      Interfaces.Unsigned_32);

   package Unsigned_64_IO is new Ada.Text_IO.Modular_IO (
      Interfaces.Unsigned_64);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Unsigned_32,
      Target => Integer);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Integer_8,
      Target => Interfaces.Unsigned_8);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Integer_16,
      Target => Interfaces.Unsigned_16);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Integer_32,
      Target => Interfaces.Unsigned_32);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Integer_64,
      Target => Interfaces.Unsigned_64);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Unsigned_8,
      Target => Interfaces.Integer_8);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Unsigned_16,
      Target => Interfaces.Integer_16);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Unsigned_32,
      Target => Interfaces.Integer_32);

   function Coerce            is new Ada.Unchecked_Conversion (
      Source => Interfaces.Unsigned_64,
      Target => Interfaces.Integer_64);

   generic
      Width                      : in Positive;
      type Data_Type             is mod <>;
   procedure Dump (
      Source                     : in     System.Address;
      Size                       : in     Positive;
      Line_Limit                 : in     Positive;
      Message                    : in     String := "");

   -------------------------------------------------------------------
   procedure Dump (
      Source                     : in     System.Address;
      Size                       : in     Positive;     -- size in bits
      Line_Limit                 : in     Positive;
      Message                    : in     String := "") is
   -------------------------------------------------------------------
      type Buffer_Type     is array (Natural range <>) of Data_Type;
      pragma Pack (Buffer_Type);

      Values               : constant Positive := (Size - 1) / Data_Type'size + 1;
                              -- in bytes
      Buffer               : Buffer_Type (1 .. Values);
      Last_Line            : Buffer_Type (1 .. Values) := (
                                             others => Data_Type'first);
      This_Line            : Buffer_Type (1 .. Values);

      for Buffer'address use Source;
--    On_Line              : Natural := 0;
--    First                : Natural := 0;   -- index in buffer for start of line
--    Last                 : Natural := Line_Limit - 1;
--    Line_Index           : Natural := 1;
      Skipping             : Natural := 0;
      Do_Skipping          : Boolean := False;
      First_Skip           : Boolean := True;

   begin
--Put_Line ("size" & Size'img & " Values" & Values'img & " limit" & Line_Limit'img &
-- " data size" & Data_Type'size'img &
-- " buffer first" & Buffer'first'img &
-- " buffer last" & Buffer'last'img &
-- " buffer length" & Buffer'length'img &
-- Quote (" message", Message));

      if Message'length > 0 then
         if Include_Task then
            Put (Ada_Lib.Trace.Current_Task & ": ");
         end if;
        Put_Line (Message & " source " & Ada_Lib.Trace.Image (Source'address));
      end if;

      for Index in Buffer'range loop
         declare
            Line_Index     : constant Positive :=
                              (Index - 1) mod Line_Limit + 1;
         begin
            if Line_Index = 1 then   -- new line
               if First_Skip or else not Do_Skipping then
                  if Include_Task then
                     Put (Ada_Lib.Trace.Current_Task & ": ");
                  end if;
                  Put (Hex ((Index - 1) * (Width / 2), 4) & ": ");   -- address
               end if;
               if First_Skip and then Do_Skipping then
                  Put_Line ("*");
                  First_Skip := False;
               end if;
            end if;

            This_Line (Line_Index) := Buffer (Index);

            if Do_Skipping then
               Skipping := Skipping + 1;
            else     -- print the value
               Put (Hex (Interfaces.Unsigned_64 (Buffer (Index)), Width) & " ");
--Put (Index'img & ":" &Hex (Interfaces.Unsigned_64 (Buffer (Index)), Width) & " ");
            end if;

            if    Line_Index = Line_Limit or else  -- last of line
                  Index = Buffer'last then         -- end of buffer
               if Last_Line = This_Line then
                  Do_Skipping := True;
--put_line ("start skipping");
               else
                  if Do_Skipping then
                     Put (Skipping'img);
                     Do_Skipping := False;
                     First_Skip := True;
                     Skipping := 0;
                  end if;
                  Last_Line := This_Line;
               end if;

               if not Do_Skipping or else First_Skip then
                  New_Line;
               end if;
            end if;

         exception
            when Fault: others =>
               Trace_Message_Exception (Fault, "index" & Index'img &
                  " line index" & Line_Index'img &
                  " Line_Limit" & Line_Limit'img);
               raise;
         end;
--if index = buffer'last then
--put_line ("end of buffer" & index'img);
--end if;
      end loop;

--         if Skipping = 0 then
--            if (  Index > 0 and then
--                  Index = First and then
--                  Last <= Buffer'last) and then
--                  Buffer (First .. Last) = Last_Line then   -- skip whole line
--               Skipping := Line_Limit - 1;
--               Was_Skipping := True;
--            end if;
--
--            if Skipping = 0 then    -- start of skipping but not whole line
--               if On_Line = 0 then  -- start of line
--                  if Was_Skipping then
--                     Put_Line ("*");
--                     if Include_Task then
--                        Put (Ada_Lib.Trace.Current_Task & ": ");
--                     end if;
--                     Was_Skipping := False;  -- end of skippng
----                   if Include_Task then
----                      Put (Ada_Lib.Trace.Current_Task & ": ");
----                   end if;
--                  end if;
--
--                  Put (Hex (Index * (Width / 2), 4) & ": ");   -- address
--               end if;
--
--               Put (Hex (Interfaces.Unsigned_64 (Buffer (Index)), Width) & " ");
--               On_Line := On_Line + 1;
--               if On_Line = Line_Limit then
--                  New_Line;
--                  On_Line := 0;
--               end if;
--            end if;
--         else
--            Skipping := Skipping - 1;
--         end if;
--
--         if Index = Last then
--            Last_Line := Buffer (First .. Last);
--            First := First + Line_Limit;
--            Last := Last + Line_Limit;
--         end if;
--      end loop;
--
--      if On_Line > 0 then
--         New_Line;
--         if Include_Task then
--            Put (Ada_Lib.Trace.Current_Task & ": ");
--         end if;
--      end if;
--      if Was_Skipping then
--         Put_Line ("*");
--         if Include_Task then
--            Put (Ada_Lib.Trace.Current_Task & ": ");
--         end if;
--         Put_Line (Hex (Buffer'Last * (Width / 2), 4) & ": ");
--      end if;
      Flush;
--    Unlock_Trace;
--       Log_Out (Debug);
   end Dump;

   -------------------------------------------------------------------
   procedure Dump8 is new Dump (
      Width       => 2,
      Data_Type   => Interfaces.Unsigned_8);
   -------------------------------------------------------------------

   -------------------------------------------------------------------
   procedure Dump_8 (
      Source                     : in     System.Address;
      Size                       : in     Positive;
      Width                      : in     Positive := 16;
      Message                    : in     String := "") is
   -------------------------------------------------------------------

   begin
      Dump8 (Source, Size, Width, Message);
   end Dump_8;

   -------------------------------------------------------------------
   procedure Dump16 is new Dump (
      Width       => 4,
      Data_Type   => Interfaces.Unsigned_16);
   -------------------------------------------------------------------

   -------------------------------------------------------------------
   procedure Dump_16 (
      Source                     : in   System.Address;
      Size                       : in   Positive;
      Width                      : in   Positive := 8;
      Message                    : in     String := "") is
   -------------------------------------------------------------------

   begin
      Dump16 (Source, Size, Width, Message);
   end Dump_16;

   -------------------------------------------------------------------
   procedure Dump32 is new Dump (
      Width       => 8,
      Data_Type   => Interfaces.Unsigned_32);
   -------------------------------------------------------------------

   -------------------------------------------------------------------
   procedure Dump_32 (
      Source                     : in   System.Address;
      Size                       : in   Positive;
      Width                      : in   Positive := 4;
      Message                    : in   String := "") is
   -------------------------------------------------------------------

   begin
      Dump32 (Source, Size, Width, Message);
   end Dump_32;

   -------------------------------------------------------------------
   procedure Dump64 is new Dump (
      Width       => 16,
      Data_Type   => Interfaces.Unsigned_64);
   -------------------------------------------------------------------

   -------------------------------------------------------------------
   procedure Dump_64 (
      Source                     : in   System.Address;
      Size                       : in   Positive;
      Width                      : in   Positive := 64;
      Message                    : in     String := "") is
   -------------------------------------------------------------------

   begin
      Dump64 (Source, Size, Width, Message);
   end Dump_64;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Integer is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_32;

   begin
      Unsigned_32_IO.Get ("16#" & Source & "#", Result, Last);

-- put_line ("get hex '" & Source
      return Coerce (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Integer_8 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_8;

   begin
      Unsigned_8_IO.Get ("16#" & Source & "#", Result, Last);

      return Coerce (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Integer_16 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_16;

   begin
      Unsigned_16_IO.Get ("16#" & Source & "#", Result, Last);

      return Coerce (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Integer_32 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_32;

   begin
      Unsigned_32_IO.Get ("16#" & Source & "#", Result, Last);

      return Coerce (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Integer_64 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_64;

   begin
      Unsigned_64_IO.Get ("16#" & Source & "#", Result, Last);

      return Coerce (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Unsigned_8 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_32;

   begin
      Unsigned_32_IO.Get ("16#" & Source & "#", Result, Last);
      return Interfaces.Unsigned_8 (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Unsigned_16 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_32;

   begin
      Unsigned_32_IO.Get ("16#" & Source & "#", Result, Last);
      return Interfaces.Unsigned_16 (Result);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Unsigned_32 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_32;

   begin
      Unsigned_32_IO.Get ("16#" & Source & "#", Result, Last);
      return Result;
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Source               : in   String
   ) return Interfaces.Unsigned_64 is
   -------------------------------------------------------------------

      Last              : Natural;
      Result               : Interfaces.Unsigned_64;

   begin
      Unsigned_64_IO.Get ("16#" & Source & "#", Result, Last);
      return Result;
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Address              : in   System.Address
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_32 (
         System.Storage_Elements.To_Integer (Address)), 8);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Address              : in   System.Storage_Elements.Storage_Offset
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_32 (Address), 8);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Integer;
      Width             : in   Positive := 8
   ) return String is
   -------------------------------------------------------------------

      Extended          : constant Interfaces.Integer_64 :=
                           Interfaces.Integer_64 (Value);

   begin
      return Hex (Coerce (Extended), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.C.Unsigned
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), 8);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Unsigned_8;
      Width             : in   Positive := 2
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Unsigned_16;
      Width             : in   Positive := 4
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Unsigned_32;
      Width             : in   Positive := 8
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), Width);
   end Hex;

   -- raises Width_Error
   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Unsigned_64;
      Width             : in   Positive := 16
   ) return String is
   -------------------------------------------------------------------

      Found_Start          : Boolean := False;
      Result               : String (1 .. 20);
      Start             : Positive := Result'length - Width;

   begin
      if Width > 16 then
         raise Width_Error;
      end if;

      Unsigned_64_IO.Put (Result, Value, Base => 16);
-- put_line ("raw '" & Result & "'");

      for Index in reverse 4 .. Result'length - 2 loop
         case Result (Index) is

            when '#' | ' ' =>
               Result (Index) := '0';
               Found_Start := True;

            when others =>
               if Found_Start then
                  Result (Index) := '0';
               end if;

         end case;
      end loop;

      for Index in 4 ..  Result'length - 1 - Width loop
         if Result (Index) /= '0' then
            Start := Index;
            exit;
         end if;
      end loop;

      return Result (Start .. Result'last - 1);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Integer_8;
      Width             : in   Positive := 2
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Coerce (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Integer_16;
      Width             : in   Positive := 4
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Coerce (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Integer_32;
      Width             : in   Positive := 8
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Coerce (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.Integer_64;
      Width             : in   Positive := 16
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Coerce (Value), Width);
   end Hex;

   -------------------------------------------------------------------
   function Hex (
      Value             : in   Interfaces.C.Unsigned_Char
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), 2);
   end Hex;

   -------------------------------------------------------------------
   function Integer_Hex (
      Value             : in   Data_Type;
      Width             : in   Positive := Data_Type'size / 4
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), Width);
   end Integer_Hex;

   -------------------------------------------------------------------
   function Modular_Hex (
      Value             : in   Data_Type;
      Width             : in   Positive := Data_Type'size / 4
   ) return String is
   -------------------------------------------------------------------

   begin
      return Hex (Interfaces.Unsigned_64 (Value), Width);
   end Modular_Hex;

   -------------------------------------------------------------------
   function Modular_Hex_Address (
      Address           : in   System.Address;
      Width             : in   Positive
   ) return String is
   -------------------------------------------------------------------

      Buffer            : array (1 .. Width) of Interfaces.Unsigned_8;
      for Buffer'address use Address;
      Result            : Ada_Lib.Strings.Unlimited.String_Type;

   begin
      for Index in Buffer'range loop
         Result := Result & Hex (Buffer (Index));
      end loop;

      return Result.Coerce;
   end Modular_Hex_Address;

end Hex_IO;
