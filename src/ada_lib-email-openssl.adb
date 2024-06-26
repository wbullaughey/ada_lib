-- with Interfaces.C;
with System;

package body Ada_Lib.EMail.OpenSSL is

-- type C_Socket_Type            is new Interfaces.C.Int;
   type SSL_Context_Type         is null record;
   type SSL_Context_Access       is access all SSL_Context_Type;
   type SSL_Type                 is null record;
   type SSL_Access               is access all SSL_Type;

   procedure SSL_Library_Initialize with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_library_init";

   procedure OpenSSL_Add_All_Algorithms with
       Import        => True,
       Convention    => C,
       External_Name => "OpenSSL_add_all_algorithms";

   procedure SSL_Load_Error_Strings with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_load_error_strings";

   function SSL_New (
      SSL_Context                :     SSL_Context_Access
   ) return SSL_Access with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_new";

   function SSL_CTX_New (
      Client_Method              :     System.Address
   ) return SSL_Context_Access with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_CTX_new";

   function TLSv1_2_Client_Method return System.Address with
       Import        => True,
       Convention    => C,
       External_Name => "TLSv1_2_client_method";

-- procedure C_Socket_Function with
--     Import        => True,
--     Convention    => C,
--     External_Name => "socket";

   procedure SSL_Set_FD (
      SSL                        : in     SSL_Access;
      C_Socket                   : in     GNAT.Sockets.Socket_Type
   ) with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_set_fd";

   procedure SSL_Connect (
      SSL                        : in     SSL_Access
   ) with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_connect";

   procedure SSL_Shutdown (
      SSL                        : in     SSL_Access
   ) with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_shutdown";

   procedure SSL_Free (
      SSL                        : in     SSL_Access
   ) with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_free";

   procedure SSL_CTX_Free (
      SSL                        : in     SSL_Context_Access
   ) with
       Import        => True,
       Convention    => C,
       External_Name => "SSL_CTX_free";

   -----------------------------------------------------------------------
   procedure Connect (
      ADA_LIB_Socket             : in out Ada_Lib.Socket_IO.Client.Client_Socket_Type;
      GNAT_Socket                : in     GNAT.Sockets.Socket_Type) is
   -----------------------------------------------------------------------

--    C_Socket                   : C_Socket_Type;
      SSL                        : SSL_Access;
      SSL_Context                : SSL_Context_Access;

   begin
      SSL_library_Initialize;
      OpenSSL_Add_All_Algorithms;
      SSL_Load_Error_Strings;

      -- Create a new SSL context
      SSL_Context := SSL_CTX_New (TLSv1_2_Client_Method);

      -- Attach the socket to the SSL context
      SSL := SSL_New(SSL_Context);
      SSL_Set_FD(SSL, GNAT_Socket);

      -- Perform the SSL/TLS handshake
      SSL_Connect(SSL);

      -- Authentication and email composition code goes here

      -- Close the SSL connection and cleanup
      SSL_Shutdown(SSL);
      SSL_Free(SSL);
      SSL_CTX_Free(SSL_Context);

      ADA_LIB_Socket.Set_Socket (GNAT_Socket);
   end Connect;
end Ada_Lib.EMail.OpenSSL;
