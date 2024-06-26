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

with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Text_IO;

package body Ada_Lib.Sockets is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   -------------------------------------------------------------------
   procedure Accept_Socket (
      Server               : in out Socket_Type;
      Client               :   out Socket_Type) is
   -------------------------------------------------------------------

      Address           : GNAT.Sockets.Sock_Addr_Type;

   begin
      GNAT.Sockets.Accept_Socket (Server, Client, Address);
   end Accept_Socket;

   -------------------------------------------------------------------
   procedure Bind (
      Handle               : in out Socket_Type;
      Port              : in   Port_Type) is
   -------------------------------------------------------------------

      Address           : GNAT.Sockets.Sock_Addr_Type;

   begin
      Address.Addr := GNAT.Sockets.Any_Inet_Addr;
      Address.Port := Port;
      GNAT.Sockets.Bind_Socket (Handle, Address);
   end Bind;

   -------------------------------------------------------------------
   procedure Connect (
      Handle               : in out Socket_Type;
      Host              : in   String;
      Port              : in   Port_Type) is
   -------------------------------------------------------------------

      Address              : GNAT.Sockets.Sock_Addr_Type;

   begin
Ada.Text_IO.put_line ("inet addr");
      Address.Addr := GNAT.Sockets.Inet_Addr (Host);
      Address.Port := Port;

Ada.Text_IO.put_line ("connect");
      GNAT.Sockets.Connect_Socket (Handle, Address);
Ada.Text_IO.put_line ("connected");
   end Connect;

   -------------------------------------------------------------------
   function Get_Line (
      Socket               : in   Socket_Type
   ) return String is
   -------------------------------------------------------------------

      Buffer               : String (1 .. 500);
      Item                 : Ada.Streams.Stream_Element_Array (1 .. 1);
         Last                 : Ada.Streams.Stream_Element_Offset;
      Length               : Natural := 0;

   begin
      while Length < Buffer'last loop
         GNAT.Sockets.Receive_Socket (Socket, Item, Last);

         if Last = 0 then
            if Length = 0 then
               raise Connection_Closed;
            end if;

            return Buffer (Buffer'first .. Length);
         end if;

         case Character'val (Item (Item'first)) is

            when Ada.Characters.Latin_1.LF =>
               return Buffer (Buffer'first .. Length);

            when Ada.Characters.Latin_1.CR =>
               null;

            when others =>
               Length := Length + 1;
               Buffer (Length) := Character'val (Item (Item'first));

         end case;

      end loop;

      begin
         return Buffer & Get_Line (Socket);

      exception

         when Connection_Closed =>
            return Buffer;

      end;

   exception
      when GNAT.Sockets.Socket_Error =>
         raise Connection_Closed;

   end Get_Line;

   -------------------------------------------------------------------
   procedure Put_Line (
      Socket               : in   Socket_Type;
      Line              : in   String) is
   -------------------------------------------------------------------

      Item                 : Ada.Streams.Stream_Element_Array (
                           Ada.Streams.Stream_Element_Offset (
                              Line'first) ..
                           Ada.Streams.Stream_Element_Offset (
                              Line'last + 1));
         Last                 : Ada.Streams.Stream_Element_Offset;

   begin
      for Index in Line'range loop
         Item (Ada.Streams.Stream_Element_Offset (Index)) :=
            Character'pos (Line (Index));
      end loop;

      Item (Item'last) := Character'pos (Ada.Characters.Latin_1.LF);

      GNAT.Sockets.Send_Socket (Socket, Item, Last);

      if Last = Item'first then
         raise Connection_Closed;
      elsif Last < Item'last then
         raise Error;
      end if;

   end Put_Line;

   -------------------------------------------------------------------
   procedure Socket (
      Handle               :   out Socket_Type) is
   -------------------------------------------------------------------

   begin
      GNAT.Sockets.Create_Socket (Handle,
         GNAT.Sockets.Family_Inet, GNAT.Sockets.Socket_Stream);
   end Socket;

begin
   GNAT.Sockets.Initialize (False);
end Ada_Lib.Sockets;
