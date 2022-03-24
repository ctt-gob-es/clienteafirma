Imports Microsoft.Win32

Module Program
    Sub Main(args As String())
        Console.WriteLine("Se van a eliminar los registros del plugin hash")
        Registry.ClassesRoot.DeleteSubKey("*\shell\afirma.hashFile\command")
        Registry.ClassesRoot.DeleteSubKey("*\shell\afirma.hashFile")
        Registry.ClassesRoot.DeleteSubKey("Directory\shell\afirma.hashDirectory\command")
        Registry.ClassesRoot.DeleteSubKey("Directory\shell\afirma.hashDirectory")
        Registry.ClassesRoot.DeleteSubKey(".hash\shell\afirma.hash\command")
        Registry.ClassesRoot.DeleteSubKey(".hash\shell\afirma.hash")
        Registry.ClassesRoot.DeleteSubKey(".hashb64\shell\afirma.hasbh64\command")
        Registry.ClassesRoot.DeleteSubKey(".hashb64\shell\afirma.hasbh64")
        Registry.ClassesRoot.DeleteSubKey(".hashfiles\shell\afirma.hashfiles\command")
        Registry.ClassesRoot.DeleteSubKey(".hashfiles\shell\afirma.hashfiles")
		Registry.ClassesRoot.DeleteSubKey(".hexhash\shell\afirma.hexhash\command")
        Registry.ClassesRoot.DeleteSubKey(".hexhash\shell\afirma.hexhash")
        Console.WriteLine("Registros eliminados correctamente")
    End Sub
End Module
