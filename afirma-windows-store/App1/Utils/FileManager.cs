using es.gob.afirma.core;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Storage;
using Windows.Storage.Pickers;

namespace AfirmaWMetro.Utils
{
    class FileManager
    {
        public async static Task<byte[]> GetDataFromDisk(string commitText)
        {
            FileOpenPicker openPicker = new FileOpenPicker();
            openPicker.FileTypeFilter.Add("*");
            openPicker.SuggestedStartLocation = PickerLocationId.DocumentsLibrary;
            if (commitText != null)
            {
                openPicker.CommitButtonText = commitText;
            }
            StorageFile selectedFile2 = await openPicker.PickSingleFileAsync();
            if (selectedFile2 == null)
            {
                throw new AOCancelledOperationException();
            }
            byte[] bytes = null;
            using (Stream reader = await selectedFile2.OpenStreamForReadAsync())
            {
                // Leemos en el buffer
                bytes = new byte[reader.Length];
                int numBytesToRead = (int)reader.Length;
                int numBytesRead = 0;
                while (numBytesToRead > 0)
                {
                    int n;
                    if (numBytesToRead > 10)
                    {
                        // La lectura devuelve entre 0 y 10. 
                        n = reader.Read(bytes, numBytesRead, 10);
                    }
                    else
                    {
                        // La lectura devuelve entre 0 y 10. 
                        n = reader.Read(bytes, numBytesRead, numBytesToRead);
                    }

                    // Se ha alcanzado el fin de fichero
                    if (n == 0)
                    {
                        break;
                    }
                    numBytesRead += n;
                    numBytesToRead -= n;
                }
                reader.Dispose();
            }
            return bytes;
        }
    }
}
