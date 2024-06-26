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

with Ada.Exceptions;
with Ada_Lib.Parser;
with Ada_Lib.Strings;

package body Ada_Lib.Time is

   use type Ada.Calendar.Time;

   function Pad (
      Source            : in   String
   ) return String;

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

   -------------------------------------------------------------------
   function Image (
      Time              : in   Duration;
      Hundreths            : in   Boolean := False;
      Show_Days            : in   Boolean := False
   ) return String is
   -------------------------------------------------------------------

   begin
      if Time = No_Duration then
         return "no duration";
      end if;

      if Hundreths then
         return Format (Integer (Float'Floor (Float (Time))), Show_Days => Show_Days) & "." &
            Pad (Integer'image (Integer (Time * 100) mod 100));
      else
         return Format (Integer (Time), Show_Days => Show_Days);
      end if;
   exception
      when Ada.Calendar.Time_Error =>
         return "INVALID";
   end Image;

   -------------------------------------------------------------------
   function Image (
      Time              : in   Ada.Calendar.Time;
      Hundreths            : in   Boolean := False
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

      if Hundreths then
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
   function Pad (
      Source            : in   String
   ) return String is
   -------------------------------------------------------------------

      Trimmed           : constant String :=Ada_Lib.Strings.Trim (Source);

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
   function Parse_Date_Time (
      Source               : in   String
   ) return Ada.Calendar.Time is
   -------------------------------------------------------------------

      procedure Raise_Bad_Time (
         Field          : in   String);

      pragma No_Return (Raise_Bad_Time);

      ---------------------------------------------------------------
      procedure Raise_Bad_Time (
         Field          : in   String) is
      ---------------------------------------------------------------

      begin
         Ada.Exceptions.Raise_Exception (Bad_Time'identity,
            "Invalid " & Field & " in Ada_Lib.Time.Parse_Date_Time source parameter '" &
               Source & "'");
      end Raise_Bad_Time;

      ---------------------------------------------------------------
      function Parse_Time (
         Value          : in   String)
      return Duration is
      ---------------------------------------------------------------
         subtype Hour_Type    is Integer range 0 .. 23;
         subtype Minute_Type     is Integer range 0 .. 59;
         subtype Second_Type     is Integer range 0 .. 59;

         Hour              : Hour_Type;
         Iterator          : Ada_Lib.Parser.Iterator_Type :=
                              Ada_Lib.Parser.Initialize (
                                 Value                =>Ada_Lib.Strings.Trim (Value),
                                 Seperators              => ":",
                                 Ignore_Multiple_Seperators => False);
         Minute               : Minute_Type;
         Second               : Second_Type;

      begin
         Hour := Hour_Type'value (Ada_Lib.Parser.Get_Value (Iterator));
         Ada_Lib.Parser.Next (Iterator);

         Minute := Minute_Type'value (Ada_Lib.Parser.Get_Value (Iterator));
         Ada_Lib.Parser.Next (Iterator);

         Second := Second_Type'value (Ada_Lib.Parser.Get_Value (Iterator));
         Ada_Lib.Parser.Next (Iterator);

         if not Ada_Lib.Parser.At_End (Iterator) then
            Raise_Bad_Time ("time");
         end if;

         return Duration (((Hour * 60) + Minute) * 60 + Second);

      exception
         when  Constraint_Error |
               Ada_Lib.Parser.Underflow =>
            Raise_Bad_Time ("time");

      end Parse_Time;
      ---------------------------------------------------------------

      Day                  : Ada.Calendar.Day_Number;
      Iterator          : Ada_Lib.Parser.Iterator_Type :=
                           Ada_Lib.Parser.Initialize (
                              Value                =>Ada_Lib.Strings.Trim (Source),
                              Seperators              => " ",
                              Ignore_Multiple_Seperators => False);
      Month             : Ada.Calendar.Month_Number;
      Now                  : constant Ada.Calendar.Time := Time.Now;
      Time              : Duration;
      Year              : Ada.Calendar.Year_Number;

   begin
      declare
         Field       : constant String := Ada_Lib.Parser.Get_Value (Iterator);

      begin
         Ada_Lib.Parser.Next (Iterator);

         -- Field is a date
         if not Ada_Lib.Parser.At_End (Iterator) then
            declare
               Date_Iterator        : Ada_Lib.Parser.Iterator_Type :=
                                    Ada_Lib.Parser.Initialize (
                                       Value                =>Ada_Lib.Strings.Trim (Field),
                                       Seperators              => "/",
                                       Ignore_Multiple_Seperators => False);

            begin
               declare
                  Year_Value        : constant Natural :=
                                    Natural'value (
                                       Ada_Lib.Parser.Get_Value (Date_Iterator));

               begin
                  if Year_Value < 100 then
                     Year := Year_Value + 2000;
                  else
                     Year := Year_Value;
                  end if;
               end;

               Ada_Lib.Parser.Next (Date_Iterator);

               Month := Ada.Calendar.Month_Number'value (Ada_Lib.Parser.Get_Value (Date_Iterator));
               Ada_Lib.Parser.Next (Date_Iterator);

               Day := Ada.Calendar.Day_Number'value (Ada_Lib.Parser.Get_Value (Date_Iterator));
               Ada_Lib.Parser.Next (Date_Iterator);

               if not Ada_Lib.Parser.At_End (Date_Iterator) then
                  Raise_Bad_Time ("date");
               end if;
            end;

            Time := Parse_Time (Ada_Lib.Parser.Get_Value (Iterator));

         else
            Year := Ada.Calendar.Year (Now);
            Month := Ada.Calendar.Month (Now);
            Day := Ada.Calendar.Day (Now);

            Time := Parse_Time (Field);
         end if;
      end;

      return Ada.Calendar.Time_Of (Year, Month, Day, Time);

   exception
         when  Constraint_Error |
               Ada_Lib.Parser.Underflow =>
         Raise_Bad_Time ("date");

   end Parse_Date_Time;

   -------------------------------------------------------------------
   function Parse_Duration (
      Source               : in   String
   ) return Duration is
   -------------------------------------------------------------------

      ---------------------------------------------------------------
      procedure Raise_Bad_Time is
      ---------------------------------------------------------------

      begin
         Ada.Exceptions.Raise_Exception (Bad_Time'identity,
            "Invalid parameter for Ada_Lib.Time.Parse_Duration source parameter '" &
            Source & "'");
      end Raise_Bad_Time;
      ---------------------------------------------------------------

      Iterator          : Ada_Lib.Parser.Iterator_Type :=
                           Ada_Lib.Parser.Initialize (
                              Value                => Ada_Lib.Strings.Trim (Source),
                              Seperators              => ":",
                              Ignore_Multiple_Seperators => False);
      Time_Fields          : array (1 .. 4) of Natural;
      Time_Fields_Count    : Natural := 0;

   begin
      while not Ada_Lib.Parser.At_End (Iterator) loop
         Time_Fields_Count := Time_Fields_Count + 1;

         if Time_Fields_Count > 4 then
            Raise_Bad_Time;
         end if;

         Time_Fields (Time_Fields_Count) := Natural'value (Ada_Lib.Parser.Get_Value (Iterator));

         Ada_Lib.Parser.Next (Iterator);
      end loop;


      declare
         type Seconds_Type is range 0 .. 16#7FFF_FFFF_FFFF_FFFF#;

         Result            : Seconds_Type := 0;

      begin

         for Index in 1 .. Time_Fields_Count loop
            if Time_Fields_Count - Index < 2 then
               Result := Result * 60 + Seconds_Type (Time_Fields (Index));
            else
               Result := Result * 24 + Seconds_Type (Time_Fields (Index));
            end if;
         end loop;

         return Duration (Result);
      end;

   exception
      when Constraint_Error =>
         Raise_Bad_Time;
         return 0.0;

   end Parse_Duration;

   -------------------------------------------------------------------
   function To_Duration (
      Seconds              : in   Float;
      Minutes              : in   Natural := 0;
      Hours             : in   Natural := 0
   ) return Duration is
   -------------------------------------------------------------------

   begin
      return Duration ((Hours * 60 + Minutes) * 60) + Duration (Seconds);
   end To_Duration;

end Ada_Lib.Time;
