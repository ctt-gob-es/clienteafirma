using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace es.gob.afirma.core.csutils
{
    class UrlReader
    {
        
        public static byte[] ReadUri(Uri uri)
        {
            if (uri == null)
            {
                throw new ArgumentNullException("La Uri no puede ser nula");
            }

            Worker w = new Worker();
            w.DoWorkAsync(uri);
            return w.resultado;
        }
               
    }
    //TODO: COMENTADO. Clase generada para hacer un hilo en windows metro-style. 
    class Worker
    {
        public byte[] resultado = null;
        public async void DoWorkAsync(Uri uri)
        {
            HttpClient http = new System.Net.Http.HttpClient();
            HttpResponseMessage response = await http.GetAsync(uri);
            resultado = await response.Content.ReadAsByteArrayAsync();
        }
    }
}
