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

with GNAT.Sockets;

package Ada_Lib.Sockets is

   Connection_Closed       : exception;
   Error                : exception;

   subtype Port_Type       is GNAT.Sockets.Port_Type;
   subtype Socket_Type        is GNAT.Sockets.Socket_Type;

   procedure Accept_Socket (
      Server               : in out Socket_Type;
      Client               :   out Socket_Type);

   procedure Bind (
      Handle               : in out Socket_Type;
      Port              : in   Port_Type);

   procedure Connect (
      Handle               : in out Socket_Type;
      Host              : in   String;
      Port              : in   Port_Type);

   function Get_Line (
      Socket               : in   Socket_Type
   ) return String;

   procedure Listen (
      Handle               : in   Socket_Type;
      Length               : in   Positive := 4)
   renames GNAT.Sockets.Listen_Socket;

   procedure Put_Line (
      Socket               : in   Socket_Type;
      Line              : in   String);

   procedure Shutdown (
      Handle               : in   Socket_Type;
      How               : in   GNAT.Sockets.Shutmode_Type :=
                           GNAT.Sockets.Shut_Read_Write)
   renames GNAT.Sockets.Shutdown_Socket;

   procedure Socket (
      Handle               :   out Socket_Type);

end Ada_Lib.Sockets;
