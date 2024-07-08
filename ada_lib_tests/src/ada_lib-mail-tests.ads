with AUnit.Test_Suites;

package Ada_Lib.Mail.Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;
   Include_SMTP_Mail             : Boolean := False;

end Ada_Lib.Mail.Tests;
