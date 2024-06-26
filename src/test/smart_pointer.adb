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

with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Smart_Heap_Object;
with Smart_Heap_Pointer;

procedure Smart_Pointer is

	type Record_Type				is record
		Pointer						: Smart_Heap_Pointer.Pointer_Type;
	end record;

	type Record_Access				is access Record_Type;

	procedure Free is new Ada.Unchecked_Deallocation (
		Object	=> Record_Type,
		Name	=> Record_Access);

	Pointer_1						: Smart_Heap_Pointer.Pointer_Type;
	Pointer_2						: Smart_Heap_Pointer.Pointer_Type;
	Record_1						: Record_Access := new Record_Type;


begin
	Put_Line ("initialize pointer 1");
	Smart_Heap_Pointer.Initialize (Pointer_1, Smart_Heap_Object.Create (100));

	Put_Line ("print pointer 1, should be 100");
	Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Pointer_1).all);

	Put_Line ("copy pointer 1 to heap object");
	Record_1.Pointer := Pointer_1;

	Put_Line ("print pointer 1 from heap object, should be 100");
	Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Record_1.Pointer).all);

	Put_Line ("free heap object with pointer 1");
	Free (Record_1);

	Put_Line ("initialize pointer 2");
	Smart_Heap_Pointer.Initialize (Pointer_2, Smart_Heap_Object.Create (200));

	Put_Line ("print pointer 2, should be 200");
	Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Pointer_2).all);

	Put_Line ("copy pointer 1 to stack pointer");
	declare
		Pointer_Copy			: Smart_Heap_Pointer.Pointer_Type := Pointer_1;

	begin
		Put_Line ("print pointer 1 from stack copy, should be 100");
		Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Pointer_Copy).all);
		Put_Line ("replace pointer 1 with pointer 2");
		Pointer_1 := Pointer_2;
		Put_Line ("exit stack frame, first object should get freed");
	end;

	Put_Line ("print pointer 1, should be 200");
	Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Pointer_1).all);
	Put_Line ("print pointer 2, should be 200");
	Smart_Heap_Object.Print (Smart_Heap_Pointer.Get (Pointer_2).all);
end Smart_Pointer;
