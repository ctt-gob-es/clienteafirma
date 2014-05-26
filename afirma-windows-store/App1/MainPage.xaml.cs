using AfirmaWMetro;
using AfirmaWMetro.Utils;
using es.gob.afirma.windows.parameters;
using Org.BouncyCastle.Crypto.Parameters;
using Org.BouncyCastle.Pkcs;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using VisuallyLocated.UI.Popups;
using Windows.ApplicationModel.Activation;
using Windows.ApplicationModel.Resources;
using Windows.Storage;
using Windows.Storage.Pickers;
using Windows.UI.Popups;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Navigation;

namespace App1
{
    /// <summary>
    /// Página vacía que se puede usar de forma independiente o a la que se puede navegar dentro de un objeto Frame.
    /// </summary>
    public sealed partial class MainPage : Page
    {

        public static SignParameters signParameters;

        ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();
        Windows.Storage.StorageFolder localFolder = Windows.Storage.ApplicationData.Current.LocalFolder;
        RsaKeyParameters rsaKeyParameter = null;
        Pkcs12Store store = null;
        public byte[] ExternalData = null;

        private ProtocolActivatedEventArgs _protocolEventArgs = null;
        public ProtocolActivatedEventArgs ProtocolEvent
        {
            get { return _protocolEventArgs; }
            set { _protocolEventArgs = value; }
        }

        /// <summary>
        /// Método principal de la página.
        /// </summary>
        public MainPage()
        {
            this.InitializeComponent();
            initializeComponents();
            loadStorage();
        }

        /// <summary>
        /// Inicializa los elementos de la página.
        /// </summary>
        private void initializeComponents()
        {
            aliasList.Items.Clear();
            aliasList.Items.Add(labels.GetString("Etiqueta_seleccion_alias"));
            aliasList.SelectedIndex = 0;
            aliasList.IsEnabled = false;
            buttonSign.IsEnabled = false;
        }

        /// <summary>
        /// Carga los almacenes que hay en la carpeta de la aplicación en la lista de almacenes disponibles. 
        /// </summary>
        private async void loadStorage()
        {
            pfxList.Items.Clear();
            // Cargamos los almacenes previamente importados (los existentes)
            IReadOnlyList<StorageFile> files = await ApplicationData.Current.LocalFolder.GetFilesAsync();

            // Cargamos primero los P12 en una lista, esto nos permite conocer el número de elementos antes de empezar
            // a popular el desplegable
            List<String> stores = new List<String>();
            foreach (StorageFile file in files)
            {
                if (file.FileType.ToUpper().Equals(".PFX") || file.FileType.ToUpper().Equals(".P12"))
                {
                    stores.Add(file.Name);
                }
            }

            // Si no hay almacenes añadimos un "dummy" indicándolo y deshabilitamos el desplegable
            if (stores.Count < 1)
            {
                pfxList.Items.Add("No hay ningún almacén importado");
                pfxList.IsEnabled = false;
                // Pasamos el foco al botón de importar, lo único que se puede hacer en este caso
                botonImportar.Focus(Windows.UI.Xaml.FocusState.Programmatic);
                // Salimos
                return;
            }

            // Solo si hay más de un almacén añadimos el elemento "dummy"
            else if (stores.Count > 1)
            {
                pfxList.Items.Add(labels.GetString("Etiqueta_seleccion_almacen"));
            }
           
            foreach (String file in stores)
            {
                pfxList.Items.Add(file);
            }
            if (pfxList.Items.Count > 0)
            {
                pfxList.SelectedIndex = 0;
            }

            // Si estaba deshabilitado (por haber pasado antes por la situación de no tener ningún almacén), lo habilitamos
            if (!pfxList.IsEnabled)
            {
                pfxList.IsEnabled = true;
            }
        }

        /// <summary>
        /// Se invoca cuando esta página se va a mostrar en un objeto Frame.
        /// </summary>
        /// <param name="e">Datos de evento que describen cómo se llegó a esta página. La propiedad Parameter
        /// se usa normalmente para configurar la página.</param>
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {   
        }

        /// <summary>
        /// Método que importa un almacén a la aplicación. Solicita la clave del almacén y verifica que es un almacén,
        /// en cuyo caso, almacenará dicho almacén en la carpeta de la aplicación para poder ser utilizado más tarde.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void Button_Import_Click(object sender, RoutedEventArgs e)
        {
            StringBuilder contents = new StringBuilder();

            FileOpenPicker openPicker = new FileOpenPicker();
            openPicker.SuggestedStartLocation = PickerLocationId.DocumentsLibrary;
            openPicker.FileTypeFilter.Add(".pfx");
            openPicker.FileTypeFilter.Add(".p12");
            StorageFile selectedFile2 = await openPicker.PickSingleFileAsync();
            
            if (selectedFile2 != null)
            {
                CredentialPanel cp = new CredentialPanel();
                cp.Focus(Windows.UI.Xaml.FocusState.Programmatic);
                CustomDialog customDialog = new CustomDialog(cp, labels.GetString("Etiqueta_peticion_pass"));
                customDialog.Commands.Add(new UICommand(labels.GetString("Boton_aceptar")));
                customDialog.Commands.Add(new UICommand(labels.GetString("Boton_cancelar")));
                customDialog.DefaultCommandIndex = 0;
                customDialog.CancelCommandIndex = 1;
                IUICommand com = await customDialog.ShowAsync();

                if (com.Label.Equals(labels.GetString("Boton_aceptar")))
                    if (cp.getPassword() != null)
                        using (StreamReader reader = new StreamReader(await selectedFile2.OpenStreamForReadAsync()))
                        {
                            char[] password = cp.getPassword().ToCharArray();
                            try
                            {
                                store = new Pkcs12Store(reader.BaseStream, password);
                                if (store != null)
                                {
                                    await selectedFile2.CopyAsync(localFolder, selectedFile2.Name, NameCollisionOption.ReplaceExisting);
                                    AfirmaMetroUtils.showMessage(labels.GetString("Info_almacen_importado") + selectedFile2.Name + "\"", "Importación de almacén correcta");
                                    // Recargamos los almacenes
                                    loadStorage();
                                }
                            }
                            catch
                            {
                                AfirmaMetroUtils.showMessage(labels.GetString("Error_carga_almacen"), "TITULO");
                            }
                        }
                }
             
        }

        /// <summary>
        /// Método que carga un almacén. Solicita la contraseña del almacén seleccionado y carga los alias de dicho 
        /// alamacén en la lista de alias si la contraseña proporcionada fuera correcta.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void ComboBox_Stores_Selection_Changed(object sender, SelectionChangedEventArgs e)
        {
            if (pfxList.SelectedItem != null)
            {
                // Si no se ha seleccionado el elemento "dummy"...
                if (!pfxList.SelectedItem.Equals(labels.GetString("Etiqueta_seleccion_almacen")))
                {
                    IReadOnlyList<StorageFile> files = await ApplicationData.Current.LocalFolder.GetFilesAsync();
                    StorageFile fileSelected = null;
                    foreach (StorageFile file in files)
                    {
                        if (file.Name.Equals((String)pfxList.SelectedItem))
                        {
                            fileSelected = file;
                            break;
                        }
                    }
                    if (fileSelected != null)
                    {
                        using (StreamReader reader = new StreamReader(await fileSelected.OpenStreamForReadAsync()))
                        {
                            // Pedimos el PIN del almacén al usuario
                            CredentialPanel2 cp2 = new CredentialPanel2(pfxList.SelectedItem.ToString());
                            cp2.Focus(Windows.UI.Xaml.FocusState.Programmatic);
                            CustomDialog customDialog = new CustomDialog(cp2, labels.GetString("Etiqueta_peticion_pass"));
                            customDialog.Commands.Add(new UICommand(labels.GetString("Boton_aceptar")));
                            customDialog.Commands.Add(new UICommand(labels.GetString("Boton_cancelar")));
                            customDialog.DefaultCommandIndex = 0;
                            customDialog.CancelCommandIndex = 1;
                            IUICommand com = await customDialog.ShowAsync();
                            // El usuario ha cancelado, si solo no estaba el elemento "dummy" 
                            // hay que añadirlo en la posición cero y seleccionarlo, para permitirle reintentar
                            if (com.Label.Equals(labels.GetString("Boton_cancelar")))
                            {
                                if (!pfxList.Items.Contains(labels.GetString("Etiqueta_seleccion_almacen")))
                                {
                                    pfxList.Items.Insert(0, labels.GetString("Etiqueta_seleccion_almacen"));
                                }
                                pfxList.SelectedIndex = 0;
                            }
                            // El usuario ha aceptado en el diálogo de PIN
                            if (com.Label.Equals(labels.GetString("Boton_aceptar")))
                            {
                                aliasList.Items.Clear();
                                try
                                {
                                    store = new Pkcs12Store(reader.BaseStream, cp2.getPassword().ToCharArray());
                                    foreach (string n in store.Aliases)
                                    {
                                        if (store.IsKeyEntry(n))
                                        {
                                            AsymmetricKeyEntry key = store.GetKey(n);

                                            if (key.Key.IsPrivate)
                                            {
                                                aliasList.Items.Clear();
                                                aliasList.IsEnabled = true;
                                                RsaPrivateCrtKeyParameters parameters = key.Key as RsaPrivateCrtKeyParameters;
                                                rsaKeyParameter = (RsaKeyParameters)key.Key;
                                                foreach (object s in store.Aliases)
                                                {
                                                    aliasList.Items.Add((string)s);
                                                }

                                            }
                                        }
                                    }
                                }
                                catch
                                {
                                    AfirmaMetroUtils.showMessage(labels.GetString("Error_carga_almacen"), "Error en el almacén de claves" + " (" + pfxList.SelectedItem + ")");
                                    // Para permitirle reintentar insertamos el elemento "dummy" como primer elemento
                                    // y lo seleccionamos
                                    if (!pfxList.Items.Contains(labels.GetString("Etiqueta_seleccion_almacen")))
                                    {
                                        pfxList.Items.Insert(0, labels.GetString("Etiqueta_seleccion_almacen"));
                                    }
                                    pfxList.SelectedIndex = 0;
                                    return;
                                }

                                // Se ha seleccionado correctamente un almacén, eliminamos el componente "dummy"
                                if (pfxList.Items.Contains(labels.GetString("Etiqueta_seleccion_almacen")))
                                {
                                    pfxList.Items.Remove(labels.GetString("Etiqueta_seleccion_almacen"));
                                }

                                aliasList.SelectedIndex = 0;
                            }
                        }
                    }

                }
                else
                {
                    disableComboAlias();
                }
            }
        }

        /// <summary>
        /// Este método deshabilita tanto el combo de alias de un certificado como el botón de firmar
        /// </summary>
        private void disableComboAlias()
        {
            aliasList.Items.Clear();
            aliasList.Items.Add(labels.GetString("Etiqueta_seleccion_alias"));
            aliasList.SelectedIndex = 0;
            aliasList.IsEnabled = false;
            buttonSign.IsEnabled = false;
        }

        /// <summary>
        /// Método que se ejecuta cuando se pulsa el botón de firma. Genera la propia firma
        /// y la manda al servidor.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void Button_Sign_Click(object sender, RoutedEventArgs e)
        {
            DisableComponents();
            if (!aliasList.SelectedItem.Equals(labels.GetString("Etiqueta_seleccion_alias")))
            {
                    //Obtenemos la firma de los datos. 
                    byte[] firma = null;
                    bool isError = false;
                    try
                    {
                        firma = CriptoManager.getCades(
                            signParameters.GetData(),
                            this.rsaKeyParameter,
                            store.GetCertificate((string)aliasList.SelectedItem).Certificate.GetEncoded(),
                            signParameters.GetSignatureAlgorithm(),
                            signParameters.GetSignatureFormat()
                        );
                    }
                    catch
                    {
                        // Se produce un error en la operación de firma.
                        isError = true;
                    }
                    // Se informa sobre el error de la firma.
                    if (isError)
                    {
                        try
                        {
                            await AfirmaMetroUtils.SendErrorServlet(
                                signParameters.GetStorageServletUrl().ToString(), 
                                signParameters.GetContentId(), 
                                ConstantsAfirmaMetro.ERROR_SIGNING + ConstantsAfirmaMetro.ERROR_SEPARATOR + ConstantsAfirmaMetro.DESC_ERROR_SIGNING
                            );
                        }
                        finally
                        {
                            AfirmaMetroUtils.showMessageAndClose(labels.GetString("Info_operacion_finalizada"), "Error en la firma electrónica");
                        }
                    }

                    // Se cifran los datos
                    bool isErrorCiphering = false;
                    int firmaSize = firma.Length;
                    try
                    {
                        firma = CriptoManager.getDES(firma, signParameters.GetCipherKey());
                    }
                    catch
                    {
                        isErrorCiphering = true;                            
                    }
                    if (isErrorCiphering)
                    {
                        try
                        {
                            await AfirmaMetroUtils.SendErrorServlet(
                                signParameters.GetStorageServletUrl().ToString(), 
                                signParameters.GetContentId(), 
                                ConstantsAfirmaMetro.ERROR_INVALID_CIPHER_KEY + ConstantsAfirmaMetro.ERROR_SEPARATOR + ConstantsAfirmaMetro.DESC_ERROR_INVALID_CIPHER_KEY
                            );
                        }
                        finally
                        {
                            AfirmaMetroUtils.showMessageAndClose(labels.GetString("Info_operacion_finalizada"), "Operación finalizada correctamente (1)");
                        }
                    }

                    //Se envian los datos al servidor y se espera su respuesta.
                    try
                    {
                        int padding = (8 - firmaSize);
                        string response = AsyncHelpers.RunSync<string>(() => AfirmaMetroUtils.SendDataServlet(
                            signParameters.GetStorageServletUrl().ToString(),
                            signParameters.GetContentId(),
                            firma,
                            padding
                        ));
                        //Se trata la respuesta del servidor.
                        if (response.Equals("OK"))
                        {
                            if (signParameters.IsWindowsNewUi())
                            {
                                try
                                {
                                    AfirmaMetroUtils.showMessageAndClose("La operación ha finalizado correctamente.\nPara continuar, vuelva al navegador desde el que inició la operación (interfaz 'moderno' de Windows).", "Operación finalizada correctamente");
                                }
                                catch (Exception)
                                {
                                    Application.Current.Exit();
                                }
                            }
                            else
                            {
                                AfirmaMetroUtils.showMessageAndClose("La operación ha finalizado correctamente.\nPara continuar, vuelva al navegador desde el que inició la operación (escritorio clásico de Windows).", "Operación finalizada correctamente");
                            }
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
            }
            EnableComponents();
        }
                

        /// <summary>
        /// Acción que se ejecuta cuando el usuario selecciona un alias de certificado. En concreto, muestra el botón firmar y
        /// le da el foco.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ComboBox_alias_Selection_Changed(object sender, SelectionChangedEventArgs e)
        {

            // Si no se ha seleccionado el elemento "dummy"...
            if (aliasList.SelectedItem != null && !aliasList.SelectedItem.Equals(labels.GetString("Etiqueta_seleccion_alias")))
            {
                buttonSign.IsEnabled = true;
                buttonSign.Focus(Windows.UI.Xaml.FocusState.Programmatic);
            }
            else
            {
                buttonSign.IsEnabled = false;
            }
        }

        /// <summary>
        /// Deshabilita los componentes de la pantalla
        /// </summary>
        private void DisableComponents()
        {
            pfxList.IsEnabled = false;
            botonImportar.IsEnabled = false;
            aliasList.IsEnabled = false;
            buttonSign.IsEnabled = false;
            progressRing.IsActive = true;
            textoProcesando.Visibility = Windows.UI.Xaml.Visibility.Visible;
        }

        /// <summary>
        /// Habilita los componentes de la pantalla
        /// </summary>
        private void EnableComponents()
        {
            pfxList.IsEnabled = true;
            botonImportar.IsEnabled = true;
            aliasList.IsEnabled = true;
            buttonSign.IsEnabled = true;
            progressRing.IsActive = false;
            textoProcesando.Visibility = Windows.UI.Xaml.Visibility.Collapsed;
        }

    }

}