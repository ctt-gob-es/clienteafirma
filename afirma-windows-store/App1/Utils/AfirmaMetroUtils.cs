using App1;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;
using VisuallyLocated.UI.Popups;
using Windows.ApplicationModel.Resources;
using Windows.Foundation;
using Windows.Storage;
using Windows.UI.Popups;
using Windows.UI.Xaml;

namespace AfirmaWMetro.Utils
{
    class AfirmaMetroUtils
    {
        //Acceso a los literales de la aplicacion.
        private static ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();

        /// <summary>
        /// Método que envía la firma al servidor para que la almacene.
        /// </summary>
        /// <param name="servlet">Url del servlet</param>
        /// <param name="idUser">Id usuario de la operacion</param>
        /// <param name="firma">firma en binario a enviar.</param>
        /// <returns></returns>
        public static async Task<string> SendDataServlet(string servlet, string idUser, byte[] firma, int padding)
        {
            //Procesamos el envío de los datos.
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(servlet);

            //Se compone la cadena a enviar al servlet:
            String finalData = ConstantsAfirmaMetro.OP + "=" + ConstantsAfirmaMetro.OP_DEFAULT_SEND;
            finalData = finalData + "&" + ConstantsAfirmaMetro.VERSION + "=" + ConstantsAfirmaMetro.VERSION_DEFAULT;
            finalData = finalData + "&" + ConstantsAfirmaMetro.ID + "=" + idUser;
            finalData = finalData + "&" + ConstantsAfirmaMetro.DAT + "=" + padding + "." +Base64Encoder.ToBase64(firma);
            byte[] dataStream = Encoding.UTF8.GetBytes(finalData);
            webRequest.Method = "POST";
            webRequest.ContentType = "application/x-www-form-urlencoded";

            using (Stream requestStream = await webRequest.GetRequestStreamAsync())
            {
                await requestStream.WriteAsync(dataStream, 0, dataStream.Length);
            }

            //Se espera la respuesta.
            string respuesta = "";
            using (HttpWebResponse response = (HttpWebResponse)await webRequest.GetResponseAsync())
            using (Stream responseStream = response.GetResponseStream())
            using (StreamReader readStream = new StreamReader(responseStream, Encoding.UTF8))
            {
                respuesta = await readStream.ReadToEndAsync();
            }

            return respuesta;

        }


        /// <summary>
        /// Método que envía la firma al servidor para que la almacene.
        /// </summary>
        /// <param name="servlet">Url del servlet</param>
        /// <param name="idUser">Id usuario de la operacion</param>
        /// <param name="firma">firma en binario a enviar.</param>
        /// <returns></returns>
        public static async Task<string> getDataServlet(string servlet, string idUser)
        {
            //Procesamos el envío de los datos.
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(servlet);

            //Se compone la cadena a enviar al servlet:
            String finalData = ConstantsAfirmaMetro.OP + "=" + ConstantsAfirmaMetro.OP_DEFAULT_GET;
            finalData = finalData + "&" + ConstantsAfirmaMetro.VERSION + "=" + ConstantsAfirmaMetro.VERSION_DEFAULT;
            finalData = finalData + "&" + ConstantsAfirmaMetro.ID + "=" + idUser;
            byte[] dataStream = Encoding.UTF8.GetBytes(finalData);
            webRequest.Method = "POST";
            webRequest.ContentType = "application/x-www-form-urlencoded";

            using (Stream requestStream = await webRequest.GetRequestStreamAsync())
            {
                await requestStream.WriteAsync(dataStream, 0, dataStream.Length);
            }

            //Se espera la respuesta.
            string respuesta = "";
            using (HttpWebResponse response = (HttpWebResponse)await webRequest.GetResponseAsync())
            using (Stream responseStream = response.GetResponseStream())
            using (StreamReader readStream = new StreamReader(responseStream, Encoding.UTF8))
            {
                respuesta = await readStream.ReadToEndAsync();
            }

            return respuesta;

        }


        /// <summary>
        /// Método que envía la firma al servidor para que la almacene.
        /// </summary>
        /// <param name="servlet">Url del servlet</param>
        /// <param name="idUser">Id usuario de la operacion</param>
        /// <param name="firma">firma en binario a enviar.</param>
        /// <returns></returns>
        public static async Task<string> SendErrorServlet(string servlet, string idUser, string error)
        {
            //Procesamos el envío de los datos.
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(servlet);

            //Se compone la cadena a enviar al servlet:
            String finalData = ConstantsAfirmaMetro.OP + "=" + ConstantsAfirmaMetro.OP_DEFAULT_SEND;
            finalData = finalData + "&" + ConstantsAfirmaMetro.VERSION + "=" + ConstantsAfirmaMetro.VERSION_DEFAULT;
            finalData = finalData + "&" + ConstantsAfirmaMetro.ID + "=" + idUser;
            finalData = finalData + "&" + ConstantsAfirmaMetro.DAT + "=" + error;
            byte[] dataStream = Encoding.UTF8.GetBytes(finalData);
            webRequest.Method = "POST";
            webRequest.ContentType = "application/x-www-form-urlencoded";
            using (Stream requestStream = await webRequest.GetRequestStreamAsync())
            {
                await requestStream.WriteAsync(dataStream, 0, dataStream.Length);
            }

            //Se espera la respuesta.
            string respuesta = "";
            using (HttpWebResponse response = (HttpWebResponse)await webRequest.GetResponseAsync())
            using (Stream responseStream = response.GetResponseStream())
            using (StreamReader readStream = new StreamReader(responseStream, Encoding.UTF8))
            {
                respuesta = await readStream.ReadToEndAsync();
            }

            return respuesta;

        }

        /// <summary>
        /// Muestra el error correspondiente y cierra la aplicación.
        /// </summary>
        public static async void showMessageAndClose(string content, string title)
        {
            //Mostramos un error al usuario antes de cerrar.
            await new Windows.UI.Popups.MessageDialog(
                content, 
                title
            ).ShowAsync();
            Application.Current.Exit();
        }

        
        /// <summary>
        /// Muestra el error correspondiente y cierra la aplicación.
        /// </summary>
        public static async Task showMessageAndCloseSync(string content, string title)
        {
            //Mostramos un error al usuario antes de cerrar.
            await new Windows.UI.Popups.MessageDialog(
                content + "\nCuando se cierre la aplicación vuelva a su navegador para continuar.",
                title
            ).ShowAsync();
            Application.Current.Exit();
        }
        

        /// <summary>
        /// Muestra un mensaje al usuario.
        /// </summary>
        /// <param name="message">Mensaje a mostrar al usuario.</param>
        public static async void showMessage(string content, string title)
        {
            await new MessageDialog(content, title).ShowAsync();
        }
 
    }
}
