with Ada.Exceptions;

package Test_Exit is

	procedure OK;
	procedure Failed;
	procedure Fault (
		Fault					: in	 Ada.Exceptions.Exception_Occurrence;
		Here					: in	 String);

end Test_Exit;