using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.ApplicationModel.Resources;
using Windows.Foundation;
using Windows.Storage.Pickers;
using Windows.UI.Xaml.Controls;

namespace es.gob.afirma.windows.parameters
{
    public class SaveParameters
    {

        // Parámetros en URI de entrada para guardado
        private const string FILENAME = "filename";         // Nombre propuesto para el fichero en disco
        private const string EXTENSION = "extension";       // Extensión propuesta para el fichero en disco
        private const string EXTENSION_DEFAULT = ".csig";
        private const string DESCRIPTION = "description";   // Descripción del fichero (o del tipo de fichero) a guardar
        private const string DATA = "dat";                  // Datos a guardar
        private const string FROM = "metro";                // Entorno gráfico de origen (Metro o Escritorio clásico)

        // Parámetros de la URL
        private bool metro;
        private byte[] data;
        private string fileName;
        private string extension;
        private string description;

        public string getFileName()
        {
            return this.fileName;
        }

        public string getFileExtension()
        {
            return this.extension;
        }

        public string getFileDescription()
        {
            return this.description;
        }

        public bool IsWindowsNewUi()
        {
            return this.metro;
        }

        public byte[] getData()
        {
            return this.data;
        }

        public SaveParameters(string uri)
        {

            // Restor de recursos
            ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();

            // Analizador de parametros en URL
            WwwFormUrlDecoder decoder;
            try
            {
                decoder = new WwwFormUrlDecoder(uri.Substring(uri.IndexOf("?"), uri.Length - uri.IndexOf("?")));
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W00)");
            }

            // Comprobamos los parámetros obligatorios uno a uno

            // Nombre del fichero (opcional)
            try
            {
                this.fileName = decoder.GetFirstValueByName(FILENAME);
            }
            catch (Exception)
            {
                fileName = null;
            }

            // Extensión del fichero (opcional)
            try
            {
                this.extension = decoder.GetFirstValueByName(EXTENSION);
                if (!this.extension.StartsWith("."))
                {
                    this.extension = "." + this.extension;
                }
            }
            catch (Exception)
            {
                this.extension = EXTENSION_DEFAULT;
            }

            // Descripción del fichero (opcional)
            try
            {
                this.description = decoder.GetFirstValueByName(DESCRIPTION);
            }
            catch (Exception)
            {
                this.description = null;
            }

            // Entorno GUI de origen (opcional, en caso de error o ausencia se toma el escritorio clásico como origen)
            try
            {
                Boolean.TryParse(decoder.GetFirstValueByName(FROM), out this.metro);
            }
            catch (Exception)
            {
                metro = false;
            }

            // ***********************
            // *** Datos a guardar ***
            // ***********************
            string dataBase64 = null;
            try
            {
                dataBase64 = decoder.GetFirstValueByName(DATA);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W02)");
            }

            // Deshacemos la codificación URL si existiese
            dataBase64 = dataBase64.Replace('_', '/').Replace('-', '+');
            try
            {
                // Guardamos los datos descodificados
                this.data = Convert.FromBase64String(dataBase64);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W10)");
            }
        }
    }

    class ParameterException : Exception
    {
        private string msg;

        public ParameterException(string msg)
        {
            this.msg = msg;
        }

        public string GetMessage()
        {
            return this.msg;
        }
    }

    class UnsupportedSignatureFormat : ParameterException
    {
        public UnsupportedSignatureFormat(string format) : base(format)
        {
        }
    }

    class UnsupportedSignatureAlgorithm : ParameterException
    {
        public UnsupportedSignatureAlgorithm(string algorithm) : base(algorithm)
        {
        }
    }

    public class SignParameters
    {
        // Parámetros en URI de entrada para firma
        private const string SERVLET_STORAGE = "stservlet";  // URL del Servlet de deposición de datos
        private const string ID = "id";                      // Identificador de los datos a firmar
        private const string KEY = "key";                    // Clave DES de cifrado para los datos firmados
        private const string FORMAT = "format";              // Formato de firma (CAdES, XAdES, PAdES)
        private const string ALGORITHM = "algorithm";        // Algoritmo de firma (SHA1withRSA, SHA512withRSA, etc.)
        private const string PROPERTIES = "properties";      // Propiedades adicionales de la firma (opcional)
        private const string FROM = "metro";                 // Entorno gráfico de origen (Metro o Escritorio clásico)
        private const string DATA = "dat";                   // Datos a firmar

        // Formatos soportados, siempre todo en minúsculas
        private HashSet<string> supportedSignatureFormats = new HashSet<string> { "cades" };
        
        // Algoritmos soportados, se distingue entre mayúsculas y minúsculas
        private HashSet<string> supportedSignatureAlgorithms = new HashSet<string> { {"SHA1withRSA"}, {"SHA256withRSA"}, {"SHA384withRSA"}, {"SHA512withRSA"} };

        // Parámetros de la URL
        private Uri servletUri;
        private string id;
        private string format;
        private string algorithm;
        private Dictionary<string, string> properties;
        private bool metro;
        private string desKey;
        private byte[] data;

        public SignParameters(string uri)
        {

            // Restor de recursos
            ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();

            // Analizador de parametros en URL
            WwwFormUrlDecoder decoder;
            try
            {
                decoder = new WwwFormUrlDecoder(uri.Substring(uri.IndexOf("?"), uri.Length - uri.IndexOf("?")));
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W00)");
            }

            // Comprobamos los parámetros obligatorios uno a uno

            // Direccion del servlet de almacenamiento, comprobamos que exista y que sea una URI
            // valida con http o https
            string servlet;
            try
            {
                servlet = decoder.GetFirstValueByName(SERVLET_STORAGE);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W01)");
            }
            try
            {
                this.servletUri = new Uri(servlet);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W02)");
            }
            if (servletUri.Scheme != "http" && servletUri.Scheme != "https")
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W03)");
            }

            // Id del fichero a firmar.
            try
            {
                this.id = decoder.GetFirstValueByName(ID);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W04)");
            }

            // Formato de firma (CAdES, XAdES, PAdES...)
            try
            {
                this.format = decoder.GetFirstValueByName(FORMAT);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W05)");
            }
            if (!supportedSignatureFormats.Contains(this.format.ToLower()))
            {
                throw new UnsupportedSignatureFormat(this.format);
            }

            // Algoritmo de firma (SHA1withRSA, SHA512withRSA, etc.)
            try
            {
                this.algorithm = decoder.GetFirstValueByName(ALGORITHM);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W06)");
            }
            if (!supportedSignatureAlgorithms.Contains(this.algorithm))
            {
                throw new UnsupportedSignatureAlgorithm(this.algorithm);
            }

            // Propiedades adicionales
            string base64Properties = null;
            try
            {
                base64Properties = decoder.GetFirstValueByName(PROPERTIES);
            }
            catch(Exception)
            {
                this.properties = new Dictionary<string,string>();
            }
            if (base64Properties != null)
            {
                // Deshacemos la codificacion URL si existiese
                base64Properties = base64Properties.Replace('_', '/').Replace('-', '+');
                // Deshacemos el Base64
                try
                {
                    byte[] binaryProperties = Convert.FromBase64String(base64Properties);
                    this.properties = Properties2Dictionary(System.Text.Encoding.UTF8.GetString(binaryProperties, 0, binaryProperties.Length));
                }
                catch (Exception)
                {
                    throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W07)");
                }
            }

            // Entorno GUI de origen (opcional, en caso de error o ausencia se toma el escritorio clásico como origen)
            try
            {
                Boolean.TryParse(decoder.GetFirstValueByName(FROM), out this.metro);
            }
            catch (Exception)
            {
                metro = false;
            }

            // Clave DES de cifrado
            string key;
            try
            {
                key = decoder.GetFirstValueByName(KEY);
            }
            catch (Exception)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W08)");
            }
            this.desKey = key;
            if (this.desKey.Length != 8)
            {
                throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W09)");
            }

            // Datos a firmar
            string dataBase64;
            try
            {
                dataBase64 = decoder.GetFirstValueByName(DATA);
            }
            catch (Exception)
            {
                dataBase64 = null;
            }
            // Nos pasan los datos en la URL
            if (dataBase64 != null)
            {
                // Deshacemos la codificación URL si existiese
                dataBase64 = dataBase64.Replace('_', '/').Replace('-', '+');
                try
                {
                    // Guardamos los datos descodificados
                    this.data = Convert.FromBase64String(dataBase64);
                }
                catch (Exception)
                {
                    throw new ParameterException(labels.GetString("Error_parametros") + " (ERR:W10)");
                }
            }
            //no tiene sentido esto aqui.
            //this.data = null;


        }

        private Dictionary<string, string> Properties2Dictionary(string properties)
        {
            Dictionary<string, string> dictionary = new Dictionary<string, string>();
            properties = properties.Replace("\r", "");
            foreach (var line in properties.Split(new string[] { "\n" }, StringSplitOptions.RemoveEmptyEntries))
            {
                if ((!string.IsNullOrEmpty(line)) && (!line.StartsWith(";")) && (!line.StartsWith("#")) && (!line.StartsWith("'")) && (line.Contains("=")))
                {
                    int index = line.IndexOf('=');
                    string key = line.Substring(0, index).Trim();
                    string value = line.Substring(index + 1).Trim();

                    if ((value.StartsWith("\"") && value.EndsWith("\"")) || (value.StartsWith("'") && value.EndsWith("'")))
                    {
                        value = value.Substring(1, value.Length - 2);
                    }
                    dictionary.Add(key, value);
                }
            }
            return dictionary;
        }

        public Uri GetStorageServletUrl()
        {
            return this.servletUri;
        }

        public string GetContentId()
        {
            return this.id;
        }

        public string GetSignatureFormat()
        {
            return this.format;
        }

        public string GetSignatureAlgorithm()
        {
            return this.algorithm;
        }

        public Dictionary<String, String> GetExtraParams()
        {
            return this.properties;
        }

        public bool IsWindowsNewUi()
        {
            return this.metro;
        }

        public string GetCipherKey()
        {
            return this.desKey;
        }

        public byte[] GetData()
        {
            return this.data;
        }

        public void SetData(byte[] d)
        {
            this.data = d;
        }
    }
}
