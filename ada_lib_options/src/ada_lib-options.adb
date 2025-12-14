package body Ada_Lib.Options is

   Parameter_Parsing_Failed      : Boolean := False;

   ----------------------------------------------------------------------------
   function "&" (
      Left, Right                : in        Flag_List_Type
   ) return Flag_List_Type is
   ----------------------------------------------------------------------------

   begin
pragma Assert (false, here);
return Null_Flag_List;
   end "&";

   ----------------------------------------------------------------------------
   procedure Create_Options (
      Flag                    :    out Flag_List_Type;
      Options                 : in     Base_Options_Array;
      From                    : in     String := Here) is
   ----------------------------------------------------------------------------

      Index                   : Natural := 0;

   begin
      Flag.Options := new Base_Options_Array (1 .. Options'last);
      for Option of Options loop
         Index := Index + 1;
         Flag.Options (Index) := Option;
      end loop;
   end Create_Options;

   ----------------------------------------------------------------------------
   procedure Create_Option (
      Flag                       :    out Base_Flag_Option_Type;
      Option                     : in     Character;
      Modifier                   : in     Character;
      From                       : in     String := Here) is
   ----------------------------------------------------------------------------

   begin
      Flag.Kind := (if Modifier = Unmodified_flag then
                           Plain
                        else
                           Modified);
      Flag.Modifier := Modifier;
      Flag.Option   := Option;
   end Create_Option;

   ----------------------------------------------------------------------------
   function Image (
      Flat                    : in        Flag_List_Type
   ) return String is
   ----------------------------------------------------------------------------

   begin
not_implemented;
return "";
   end Image;

   ----------------------------------------------------------------------------
   procedure Iterate (
      Flags                      : in     Flag_List_Type;
      Callback                   : access procedure (
         Option                  : in     Base_Flag_Option_Type'class)) is
   ----------------------------------------------------------------------------

   begin
      for Option of Flags.Options.all loop
         Callback (Option.all);
      end loop;
   end Iterate;

   ----------------------------------------------------------------------------
   function Length (
      Flags                      : in     Flag_List_Type
   ) return Natural is
   ----------------------------------------------------------------------------

   begin
      return Flags.Options.all'length;
   end Length;

   -------------------------------------------------------------------
   -- raises assert
   procedure Not_Implemented (
      Why                        : in     String := "";
      Here                       : in     String := GNAT.Source_Info.Source_Location;
      Who                        : in     String := GNAT.Source_Info.Enclosing_Entity) is
   -------------------------------------------------------------------

   begin
      pragma Assert (False, "not implemented at " & Here & " by " & Who);
   end Not_Implemented;

   ----------------------------------------------------------------------------
   procedure Parsing_Failed is
   ----------------------------------------------------------------------------

   begin
--    Log_Here (Debug or Trace_Options);
      Parameter_Parsing_Failed := True;
   end Parsing_Failed;

   ----------------------------------------------------------------------------
   function Parsing_Failed return Boolean is
   ----------------------------------------------------------------------------

   begin
--    Log_Here (Debug or Trace_Options, "Parameter_Parsing_Failed " & Parameter_Parsing_Failed'img);
      return Parameter_Parsing_Failed;
   end Parsing_Failed;

end Ada_Lib.Options;

