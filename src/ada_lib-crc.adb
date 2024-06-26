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

with Hex_IO;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Ada_Lib.CRC is

	package Conversion			is new System.Address_To_Access_Conversions (
		Character);

	use type System.Address;
	use type System.Storage_Elements.Storage_Offset;

	-------------------------------------------------------------------
	function CRC (
		Address					: in	 System.Address;
		Size					: in	 Positive		-- in bits
	) return CRC_Type is
	-------------------------------------------------------------------

		State					: State_Type;
	
	begin
		Initialize (State);
		Calculate (State, Address, Size);
		return Value (State);
	end CRC;

	-------------------------------------------------------------------
	function Image (
		CRC						: in	 CRC_Type
	) return String is
	-------------------------------------------------------------------

	begin
		return Hex_IO.Hex (Interfaces.Unsigned_32 (CRC), 8);
	end Image;

	-------------------------------------------------------------------
	procedure Initialize (
		State					:	 out State_Type) is
	-------------------------------------------------------------------

	begin
		GNAT.CRC32.Initialize (State);
	end Initialize;

	-------------------------------------------------------------------
	procedure Calculate (
		State					: in out State_Type;
		Address					: in	 System.Address;
		Size					: in	 Positive) is	-- in bits
	-------------------------------------------------------------------

		Bytes					: constant Positive := Size / System.Storage_Unit;
		Increment				: constant System.Storage_Elements.Storage_Offset := 1;
		Pointer					: System.Address := Address;

	begin
		pragma Assert (Bytes*System.Storage_Unit = Size);
		for Counter in 1 .. Bytes loop
			GNAT.CRC32.Update (State, Conversion.To_Pointer (Pointer).all);
			Pointer := Pointer + Increment;
		end loop;
	end Calculate;

	-------------------------------------------------------------------
	function Value (
		State					: in	 State_Type
	) return CRC_Type is
	-------------------------------------------------------------------

	begin
		return CRC_Type (GNAT.CRC32.Get_Value (State));
	end Value;
end Ada_Lib.CRC;
