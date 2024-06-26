with Ada_Lib.Trace; use Ada_Lib.Trace;
with Debug_Options;

package body Ada_Lib.Runstring_Options is

   use type Ada_Lib.Options_Interface.Option_Type;
   use type Ada_Lib.Strings.Unlimited.String_Type;

   function Find_Registration (
      Registrations              : in     Registrations_Type;
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
   ) return Element_Constant_Access;

   -------------------------------------------------------------------
   overriding
   function "=" (
      Left, Right             : in     Element_Type
   ) return Boolean is
   -------------------------------------------------------------------

   begin
      return Left.Option = Right.Option and then
             Left.Kind = Right.Kind;
   end "=";

   -------------------------------------------------------------------
   function Find_Registration (
      Registrations              : in     Registrations_Type;
      Option                     : in     Ada_Lib.Options_Interface.Option_Type
   ) return Element_Constant_Access is
   -------------------------------------------------------------------

   begin
      Log_In (Debug, Option.Image & " registrations" & Registrations.Length'img);
      for Registration of Registrations loop
         Log_Here (Debug, Registration.Option.Image);
         if Registration.Option = Option then
            Log_Out (Debug, Registration.Option.Image);
            return Registration'unchecked_access;
         end if;
      end Loop;

      Log_Out (Debug);
      return Null;
   end Find_Registration;

   protected body Registration_Type is

      -------------------------------------------------------------------
      function All_Options (
         Quote                   : in     Boolean := True
      ) return String is
      -------------------------------------------------------------------

         Result                  : Ada_Lib.Strings.Unlimited.String_Type;

      begin
         Log_In (Debug, "registrations" & Registrations.Length'img);
         for Registration of Registrations loop
            Result := Result & Registration.Option.Image (Quote);
         end Loop;
         Log_Out (Debug, Result.Coerce);
         return Result.Coerce;
      end All_Options;

      -------------------------------------------------------------------
      function All_Registered (
         Quote                   : in     Boolean := True
      ) return String is
      -------------------------------------------------------------------

         Result                  : Ada_Lib.Strings.Unlimited.String_Type;

      begin
         Log_In (Debug, "registrations" & Registrations.Length'img);
         for Registration of Registrations loop
            Result := Result & Registration.Option.Image (Quote ) & ": " &
               Registration.From & " ";
         end Loop;
         Log_Out (Debug, Result.Coerce);
         return Result.Coerce;
      end All_Registered;

      -------------------------------------------------------------------
      function Has_Parameter (
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return Boolean is
      -------------------------------------------------------------------

         Element                 : constant Element_Constant_Access :=
                                    Find_Registration (Registrations, Option);
      begin
         if Element = Null then
            raise Failed with Option.Image & " not defined";
         else
            return Element.Kind = With_Parameters;
         end if;
      end Has_Parameter;

      -------------------------------------------------------------------
      function Is_Registered (
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return Boolean is
      -------------------------------------------------------------------

      begin
         Log_In (Debug, Option.Image);
         return Log_Out (Find_Registration (Registrations, Option) /= Null,
            Debug);
      end Is_Registered;

      -------------------------------------------------------------------
      procedure Register (
         Kind                    : in     Kind_Type;
         Options                 : in     Ada_Lib.Options_Interface.
                                             Options_Type;
         From                    : in     String:= Ada_Lib.Trace.Here) is
      -------------------------------------------------------------------

         ------------------------------------------------------------
         procedure Check_Duplicates is
         ------------------------------------------------------------

         begin
            Log_In (Debug, "registrations" & Registrations.Length'img);
            for Option of Options loop
               Log_Here (Debug, Option.Image);
               if Is_Registered (Option) then
                  Log_Exception (Debug);
                  raise Duplicate_Options with Option.Image &
                     " a parameter defined at " &
                     Registration (Option) &
                     " called from " & From;
               end if;
            end loop;
            Log_Out (Debug);
         end Check_Duplicates;
         ------------------------------------------------------------

      begin
         Log_In (Debug, "Kind " & Kind'img &
            " registrations" & Registrations.Length'img &
--          " address " & Image (Registrations'address) &
            " called from " & From);

         Check_Duplicates;
         for Option of Options loop
            Log_Here (Debug, Option.Image & " kind " & Kind'img);
            declare
               Element           : Element_Type;

            begin
               Element.From.Construct (From);
               Element.Kind := Kind;
               Element.Option := Option;
               Registrations.Append (Element);
            end;
         end loop;

         Log_Out (Debug, "registrations" & Registrations.Length'img);
      end Register;

      -------------------------------------------------------------------
      function Registration (
         Option                  : in     Ada_Lib.Options_Interface.
                                             Option_Type
      ) return String is
      -------------------------------------------------------------------

      begin
         return Find_Registration (Registrations, Option).From.Coerce;

      exception
         when Fault: others =>
            Trace_Exception (Debug, Fault, Option.Image &
               " not defined");
            raise;

      end Registration;

      -------------------------------------------------------------------
      procedure Reset is         -- clears sets if need different iterator sets
      -------------------------------------------------------------------

      begin
         Log_In (Debug, "length" & Registrations.Length'img);
         Registrations.Clear;
         Log_Out (Debug, "length" & Registrations.Length'img);
      end Reset;

   end Registration_Type;

begin
     Debug := Debug_Options.Debug_All;
--debug := true;
   Log_Here (Elaborate or Trace_Options);
end Ada_Lib.Runstring_Options;
