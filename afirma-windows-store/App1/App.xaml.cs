using AfirmaWMetro;
using AfirmaWMetro.Utils;
using es.gob.afirma.windows.parameters;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using VisuallyLocated.UI.Popups;
using Windows.ApplicationModel;
using Windows.ApplicationModel.Activation;
using Windows.ApplicationModel.Resources;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.Storage;
using Windows.Storage.Pickers;
using Windows.Storage.Provider;
using Windows.UI.Popups;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Media.Imaging;
using Windows.UI.Xaml.Navigation;

// La plantilla Aplicación vacía está documentada en http://go.microsoft.com/fwlink/?LinkId=234227

namespace App1
{
    /// <summary>
    /// Proporciona un comportamiento específico de la aplicación para complementar la clase Application predeterminada.
    /// </summary>
    sealed partial class App : Application
    {

        //Acceso a los literales
        ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();

        /// <summary>
        /// Inicializa el objeto de aplicación Singleton. Esta es la primera línea de código creado
        /// ejecutado y, como tal, es el equivalente lógico de main() o WinMain().
        /// </summary>
        public App()
        {
            this.InitializeComponent();
            this.Suspending += OnSuspending;
        }

        /// <summary>
        /// Se invoca cuando la aplicación la inicia normalmente el usuario final. Se usarán otros puntos
        /// de entrada cuando la aplicación se inicie para abrir un archivo específico, para mostrar
        /// resultados de la búsqueda, etc.
        /// </summary>
        /// <param name="args">Información detallada acerca de la solicitud y el proceso de inicio.</param>
        protected override void OnLaunched(LaunchActivatedEventArgs args)
        {
            Frame rootFrame = Window.Current.Content as Frame;

            // No repetir la inicialización de la aplicación si la ventana tiene contenido todavía,
            // solo asegurarse de que la ventana está activa.
            if (rootFrame == null)
            {
                // Crear un marco para que actúe como contexto de navegación y navegar a la primera página.
                rootFrame = new Frame();

                if (args.PreviousExecutionState == ApplicationExecutionState.Terminated)
                {
                    //TODO: Cargar el estado de la aplicación suspendida previamente
                }

                // Poner el marco en la ventana actual.
                Window.Current.Content = rootFrame;
            }

            if (rootFrame.Content == null)
            {
                // Cuando no se restaura la pila de navegación para navegar a la primera página,
                // configurar la nueva página al pasar la información requerida como parámetro
                // parámetro
                if (!rootFrame.Navigate(typeof(AboutPage), args.Arguments))
                {
                    throw new Exception("Failed to create initial page");
                }
            }
            
            // Asegurarse de que la ventana actual está activa.
            Window.Current.Activate();
        }

        /// <summary>
        /// Se invoca al suspender la ejecución de la aplicación. El estado de la aplicación se guarda
        /// sin saber si la aplicación se terminará o se reanudará con el contenido
        /// de la memoria aún intacto.
        /// </summary>
        /// <param name="sender">Origen de la solicitud de suspensión.</param>
        /// <param name="e">Detalles sobre la solicitud de suspensión.</param>
        private void OnSuspending(object sender, SuspendingEventArgs e)
        {
            var deferral = e.SuspendingOperation.GetDeferral();
            //TODO: Guardar el estado de la aplicación y detener toda actividad en segundo plano
            deferral.Complete();
        }

        /// <summary>
        /// Este método se invoca cuando se arranca la aplicación por URI. Es decir, cuando se invoca por
        /// url "afirma://parametros".
        /// </summary>
        /// <param name="args">Información detallada acerca de la solicitud y el proceso de inicio.</param>
        protected async override void OnActivated(IActivatedEventArgs args)
        {
            if (args.Kind == ActivationKind.Protocol)
            {
                ProtocolActivatedEventArgs protocolArgs = args as ProtocolActivatedEventArgs;

                // Preparamos la ventana actual
                Frame mainFrame = new Frame();
                Window.Current.Content = mainFrame;
                // Asegurarse de que la ventana actual está activa.
                Window.Current.Activate();

                // Comprobamos que la entrada es una URI con parámetros
                WwwFormUrlDecoder decoder;
                try
                {
                    String uri = protocolArgs.Uri.AbsoluteUri;
                    decoder = new WwwFormUrlDecoder(uri.Substring(uri.IndexOf("?"), uri.Length - uri.IndexOf("?")));
                }
                catch (Exception)
                {
                    decoder = null;
                }
                if (decoder == null)
                {
                    await new Windows.UI.Popups.MessageDialog(labels.GetString("Error_parametros") + " (ERR:W00)").ShowAsync();
                    Application.Current.Exit();
                }

                // Si el protocolo es de tipo "afirma://sign?"
                if (protocolArgs.Uri.Authority.Equals(ConstantsAfirmaMetro.SIGN_OPERATION))
                {
                    mainFrame.Navigate(typeof(MainPage));

                    // Obtenemos el objeto de parámetros de la URL
                    string errorMessage = "Error indefinido";
                    try
                    {
                        MainPage.signParameters = new SignParameters(protocolArgs.Uri.AbsoluteUri);
                    }
                    catch (UnsupportedSignatureFormat e)
                    {
                        MainPage.signParameters = null;
                        errorMessage = "El formato de firma no está soportado en este entorno operativo: " + e.GetMessage();
                    }
                    catch (UnsupportedSignatureAlgorithm e)
                    {
                        MainPage.signParameters = null;
                        errorMessage = "El algoritmo de firma no está soportado en este entorno operativo: " + e.GetMessage();
                    }
                    catch (ParameterException e)
                    {
                        MainPage.signParameters = null;
                        errorMessage = e.GetMessage();
                    }
                    if (MainPage.signParameters == null)
                    {
                        AfirmaMetroUtils.showMessageAndClose(
                            errorMessage, 
                            "Error en la página Web de invocación"
                        );
                        return;
                    }

                    // Comprobamos si hay datos, y si no los hay los leemos desde disco
                    if (MainPage.signParameters.GetData() == null)
                    {
                        mainFrame.Navigate(typeof(FileOpenPicker));
                        string fileOpenErrorMessage = null;
                        try
                        {
                            MainPage.signParameters.SetData(await AfirmaWMetro.Utils.FileManager.GetDataFromDisk("Firmar fichero"));
                        }
                        catch (Exception e)
                        {
                            fileOpenErrorMessage = e.Message;
                        }
                        // Usamos el mensaje de error para determinar si se leyeron los datos
                        if (fileOpenErrorMessage != null)
                        {
                            AfirmaMetroUtils.showMessageAndClose("No se han seleccionado datos a firmar, se cancelará la operación en curso.\n" + fileOpenErrorMessage, "Error en la apertura de datos");
                            return;
                        }
                    }
                } // FIN SIGN

                // EL PROTOCOLO ES DE TIPO "afirma://save?"
                else if (protocolArgs.Uri.Authority.Equals(ConstantsAfirmaMetro.SAVE_OPERATION))
                {
                    SaveParameters saveParameters;
                    try
                    {
                        saveParameters = new SaveParameters(protocolArgs.Uri.AbsoluteUri);
                    }
                    catch(ParameterException e)
                    {
                        // La excepcion contiene el mensaje de error apropiado
                        AfirmaMetroUtils.showMessageAndClose(e.GetMessage(), "Error en la página Web de invocación");
                        return;
                    }

                    // Mostramos el diálogo de guardado de datos
                    FileSavePicker savePicker = new FileSavePicker();
                    savePicker.SuggestedStartLocation = PickerLocationId.DocumentsLibrary;
                    if (saveParameters.getFileDescription() != null && saveParameters.getFileExtension() != null)
                    {
                        savePicker.FileTypeChoices.Add(saveParameters.getFileDescription(), new List<string>() { saveParameters.getFileExtension() });
                    }
                    if (saveParameters.getFileName() != null)
                    {
                        savePicker.SuggestedFileName = saveParameters.getFileName();
                    }

                    var rootFrame = new Frame();
                    rootFrame.Navigate(typeof(FileSavePicker));
                    Window.Current.Content = rootFrame;
                    Window.Current.Activate();

                    StorageFile file = await savePicker.PickSaveFileAsync();
                    if (file != null)
                    {
                        CachedFileManager.DeferUpdates(file);
                        await FileIO.WriteBytesAsync(file, saveParameters.getData());
                        FileUpdateStatus status = await CachedFileManager.CompleteUpdatesAsync(file);
                        if (status != FileUpdateStatus.Complete)
                        {
                            AfirmaMetroUtils.showMessageAndClose("Ocurrió un error y no se pudo guardar el fichero, pruebe a reintentarlo más tarde o en un directorio diferente.", "Error en el salvado de fichero");
                            return;
                        }
                    }
                    else
                    {
                        AfirmaMetroUtils.showMessageAndClose("Se ha cancelado el guardado de fichero.", "Operación cancelada por el usuario");
                        return;
                    }
                    AfirmaMetroUtils.showMessageAndClose("El fichero se ha guardado correctamente.", "Operación finalizada correctamente");
                    return;
                }
            }
        }

        private async Task<bool> sendError(Dictionary<string, string> parameters,string idUser, string error)
        {
            bool resultado = false;
            try
            {
                //cargamos el servlet de storage para almacenar la operacion de cancelacion
                string rsServlet = null;
                bool rsServletLoaded = parameters.TryGetValue(ConstantsAfirmaMetro.SERVLET_STORAGE, out rsServlet);
                if (!rsServletLoaded)
                {
                    AfirmaMetroUtils.showMessageAndClose(labels.GetString("Iniciar_aplicacion_adecuado"), "TITULO");
                }

                // Asegurarse de que la ventana actual está activa.
                string result = await AfirmaMetroUtils.SendErrorServlet(rsServlet, idUser, error);
                //Se trata la respuesta del servidor.
                if (result.Equals("OK"))
                {
                    AfirmaMetroUtils.showMessageAndClose(labels.GetString("Error_cancelled_opperation"), "TITULO");
                    resultado = true;
                }
                else
                {
                    AfirmaMetroUtils.showMessageAndClose(labels.GetString("Error_communicating_with_web") + ".\n" + labels.GetString("Info_regreso_navegador"), "TITULO");
                    
                }
            }
            catch (WebException)
            {
                AfirmaMetroUtils.showMessageAndClose(labels.GetString("Error_communicating_with_web") + ".\n" + labels.GetString("Info_regreso_navegador"), "TITULO");
                
            }
            return resultado;
        }

    }
}
