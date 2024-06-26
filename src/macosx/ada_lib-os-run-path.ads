with Ada_Lib.OS;     -- needed for Elaborate_All

pragma Elaborate_All (Ada_Lib.OS);

package Ada_Lib.OS.Run.PATH is

      User                       : constant String := Ada_Lib.OS.Get_Environment ("USER");
      Local_Home_Directory       : constant String := "/Users/" & User;
      Log_File                   : constant String := Local_Home_Directory & "/tmp/run_remote.txt";
      Remote_Home_Directory      : constant String := "/home/" & User;

end Ada_Lib.OS.Run.PATH;
