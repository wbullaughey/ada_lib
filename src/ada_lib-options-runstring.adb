
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Debug_Options;

package body Ada_Lib.Options.Runstring is

-- use type Ada_Lib.Options.Option_Type;
   use type Ada_Lib.Strings.Unlimited.String_Type;

   function Find_Registration (
      Registrations           : in     Registrations_Type;
      Option                  : in     Ada_Lib.Options.Option_Type
   ) return Constant_Reference_Type;

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
      Option                     : in     Ada_Lib.Options.Option_Type
   ) return Constant_Reference_Type is
   -------------------------------------------------------------------

      use Registrations_Package;

      Cursor          : Registrations_Package.Cursor := First (Registrations);

   begin
      Log_In (Debug, Option.Image & " registrations" & Registrations.Length'img);
      while Has_Element (Cursor) loop
         declare
            Element  : constant Constant_Reference_Type :=
                        Constant_Reference (Registrations, Cursor);
         begin
            Log_Here (Debug, Element.Option.Image);
            if Element.Option = Option then
               Log_Out (Debug, Element.Option.Image);
               return Element;
            end if;
         end;

         Next (Cursor);
      end Loop;

      Log_Out (Debug);
      raise Failed with Quote ("options", Option.Option) & " not defined";
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
         Option                  : in     Ada_Lib.Options.Option_Type
      ) return Boolean is
      -------------------------------------------------------------------

      begin
         if Is_Registered (Option) then
            declare
               Parameter          : constant Constant_Reference_Type :=
                                    Find_Registration (Registrations, Option);
            begin
               return Parameter.Kind = With_Parameters;
            end;
         end if;

         return False;
      end Has_Parameter;

      -------------------------------------------------------------------
      function Is_Registered (
         Option                  : in     Ada_Lib.Options.Option_Type
      ) return Boolean is
      -------------------------------------------------------------------

      use Registrations_Package;

         Cursor          : Registrations_Package.Cursor := First (Registrations);

      begin
         Log_In (Debug or trace_options, Option.Image &
            " registrations" & Registrations.Length'img);
         while Has_Element (Cursor) loop
            declare
               Element  : constant Constant_Reference_Type :=
                           Constant_Reference (Registrations, Cursor);
            begin
               if Element.Option = Option then
                  return Log_Out (True, Debug or trace_options, Element.Option.Image);
               end if;
            end;
            Next (Cursor);
         end Loop;
         return Log_Out (False, Debug or trace_options, Quote ("options", Option.Option));
      end Is_Registered;

      -------------------------------------------------------------------
      procedure Register (
         Kind                    : in     Kind_Type;
         Options                 : in     Ada_Lib.Options.Options_Type;
         From                    : in     String:= Ada_Lib.Trace.Here) is
      -------------------------------------------------------------------

         ------------------------------------------------------------
         procedure Check_Duplicates is
         ------------------------------------------------------------

         begin
            Log_In (Debug or Trace_Options,
               "registrations" & Registrations.Length'img);
            for Option of Options loop
               Log_Here (Debug or trace_options, Option.Image);
               if Is_Registered (Option) then
                  Log_Exception (Debug or trace_options);
                  raise Duplicate_Options with Option.Image &
                     " a parameter defined at " &
                     Registration (Option) &
                     " called from " & From;
               end if;
            end loop;
            Log_Out (Debug or Trace_Options);
         end Check_Duplicates;
         ------------------------------------------------------------

      begin
         Log_In (Debug or Trace_Options, "Kind " & Kind'img &
            " registrations" & Registrations.Length'img &
--          " address " & Image (Registrations'address) &
            " called from " & From);

         Check_Duplicates;
         for Option of Options loop
            Log_Here (Debug or Trace_Options, Option.Image & " kind " & Kind'img);
            declare
               Element           : Element_Type;

            begin
               Element.From.Construct (From);
               Element.Kind := Kind;
               Element.Option := Option;
               Registrations.Append (Element);
            end;
         end loop;

         Log_Out (Debug or Trace_Options,
            "registrations" & Registrations.Length'img);
      end Register;

      -------------------------------------------------------------------
      function Registration (
         Option                  : in     Ada_Lib.Options.Option_Type
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
     Debug := Debug or Debug_Options.Debug_All;
--Debug := True;
--Trace_Options := True;
--Elaborate := True;
   Log_Here (Debug or Elaborate or Trace_Options);
end Ada_Lib.Options.Runstring;
