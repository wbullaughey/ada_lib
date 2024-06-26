with Ada_Lib.Socket_IO.Client;
with GNAT.Sockets;

package Ada_Lib.EMail.OpenSSL is

   procedure Connect (
      ADA_LIB_Socket             : in out Ada_Lib.Socket_IO.
                                       Client.Client_Socket_Type;
      GNAT_Socket                : in     GNAT.Sockets.Socket_Type);

end Ada_Lib.EMail.OpenSSL;
