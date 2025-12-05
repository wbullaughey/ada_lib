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
with Command_Line_Iterator;
with Ada_Lib.Template.Bounded_String_Array_Parameter;
with Ada_Lib.Template.Templates;
with Strings.Bounded;	-- needed for elaborate_all

pragma Elaborate_All (
	Ada_Lib.Template,
	Ada_Lib.Template.Bounded_String_Array_Parameter,
	Strings.Bounded
);

package body Template_Test_package is

	type String_Access			is access String;

	package BSA					is new Ada_Lib.Template.Bounded_String_Array_Parameter (25);
	subtype BSAPT				is BSA.Bounded_String_Array_Parameter_Type;

	Boolean_Parameter			: aliased Ada_Lib.Template.Boolean_Parameter_Type := 
									Ada_Lib.Template.Create ("b1", True);
	Bounded_String_Array		: aliased BSAPT := BSA.Create ("bsa", 3);
	Integer_Array				: aliased Ada_Lib.Template.Integer_Array_Parameter_Type :=
									Ada_Lib.Template.Create ("ia", 3);
	Iterator					: Command_Line_Iterator.Iterator_Type;
	Test_Parameter				: aliased Ada_Lib.Template.String_Parameter_Type :=
									Ada_Lib.Template.Create ("test", "abc");
								
	Vector_Parameters			: constant Ada_Lib.Template.Parameter_Array := (
		Bounded_String_Array'unchecked_access,
		Integer_Array'unchecked_access
	);
	Vector_Parameter			: aliased Ada_Lib.Template.Vector_Parameter_Type := 
									Ada_Lib.Template.Create ("va", Vector_Parameters);
	Parameters					: constant Ada_Lib.Template.Parameter_Array := (
		Boolean_Parameter'unchecked_access,
		Bounded_String_Array'unchecked_access,
		Integer_Array'unchecked_access,
		Test_Parameter'unchecked_access,
		Vector_Parameter'unchecked_access);
	Path						: String_Access := Null;
	Template					: String_Access := Null;

	--------------------------------------------------------------------------
	procedure Test is
	--------------------------------------------------------------------------

	begin
		Iterator := Command_Line_Iterator.Initialize (True, True, "p");

		while not Command_Line_Iterator.At_End (Iterator) loop

			if Command_Line_Iterator.Is_Option (Iterator) then
				declare
					Option		: constant Character := Command_Line_Iterator.Get_Option (Iterator);

				begin
					case Option is

						when 'p' =>
							if Path = Null then
								Path := new String'(Command_Line_Iterator.Get_Argument (Iterator));
							else
								Put_Line ("only one path allowed");
							end if;

						when others =>
							Put_Line ("Invalid " & Option & " option");
					end case;
				end;
			else
				if Template = Null then
					Template := new String'(Command_Line_Iterator.Get_Argument (Iterator));
				else
					Put_Line ("only one Template allowed");
				end if;
			end if;

			Command_Line_Iterator.Advance (Iterator);
		end loop;

		if Path = Null then
			Path := new String'(".");
		end if;

		if Template = Null then
			Template := new String'("source.template");
		end if;

		Ada_Lib.Template.Templates.Load (Path.all, ".template");

		for Index in 1 .. 3 loop
			Ada_Lib.Template.Set (Integer_Array, Index, Index);
			BSA.Set (Bounded_String_Array, Index, Index'img);
		end loop;
			
		Put_Line ("expanded '" & Ada_Lib.Template.Templates.Expand ("./" & Template.all,
			Parameters) & "'");
		
	exception
		when Ada_Lib.Template.Failed =>
			Put_Line ("template processor failed");

		when Ada_Lib.Template.Not_Found =>
			Put_Line ("template not found");
	end Test;
end Template_Test_package;