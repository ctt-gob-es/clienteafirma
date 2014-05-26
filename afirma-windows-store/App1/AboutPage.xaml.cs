using AfirmaWMetro;
using AfirmaWMetro.Utils;
using Org.BouncyCastle.Crypto.Parameters;
using Org.BouncyCastle.Pkcs;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Net;
using System.Text;
using VisuallyLocated.UI.Popups;
using Windows.ApplicationModel.Activation;
using Windows.ApplicationModel.Resources;
using Windows.Storage;
using Windows.Storage.Pickers;
using Windows.UI.Popups;
using Windows.UI.ViewManagement;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Navigation;


// La plantilla de elemento Página en blanco está documentada en http://go.microsoft.com/fwlink/?LinkId=234238

namespace App1
{
    /// <summary>
    /// Página vacía que se puede usar de forma independiente o a la que se puede navegar dentro de un objeto Frame.
    /// </summary>
    public sealed partial class AboutPage : Page
    {

        ResourceLoader labels = new Windows.ApplicationModel.Resources.ResourceLoader();
        Windows.Storage.StorageFolder localFolder = Windows.Storage.ApplicationData.Current.LocalFolder;
        Pkcs12Store store = null;
        StoreData storeData = null;
        /// <summary>
        /// The data source for the grouped grid view.
        /// </summary>
        private static ObservableCollection<KeyStoreFile> _source;

        /// <summary>
        /// Método principal de la página.
        /// </summary>
        public AboutPage()
        {
            this.InitializeComponent();
            AboutPageInit();
            storeData = new StoreData();
            storeData.LoadData();
            _source = storeData.GetGroupsByCategory();
            ItemGridView2.ItemsSource = storeData.Collection;

            // Añadimos el listener únicamente para deshabilitar el click izquierdo.
            // Cambiar por algo más apropiado
            ItemGridView2.IsItemClickEnabled = true;
            ItemGridView2.ItemClick += ItemGridView2_ItemClick;

            // Controlamos el evento de selección para deshabilitar el botón de borrado si no hay ningún almacén importado
            ItemGridView2.SelectionChanged += ItemGridView2_SelectionChange;
            EvaluateDeleteButton(storeData);

        }

        private void ItemGridView2_SelectionChange(object sender, SelectionChangedEventArgs e)
        {
            EvaluateDeleteButton(storeData);
        }

        private void ItemGridView2_ItemClick(object sender, ItemClickEventArgs e)
        {
        }
            
        /// <summary>
        /// Se invoca cuando esta página se va a mostrar en un objeto Frame.
        /// </summary>
        /// <param name="e">Datos de evento que describen cómo se llegó a esta página. La propiedad Parameter
        /// se usa normalmente para configurar la página.</param>
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {   
        }

        private async void Button_Import_Click(object sender, RoutedEventArgs e)
        {

            StringBuilder contents = new StringBuilder();
            StorageFile selectedFile2 = null;
            FileOpenPicker openPicker = new FileOpenPicker();
            openPicker.SuggestedStartLocation = PickerLocationId.DocumentsLibrary;
            openPicker.FileTypeFilter.Add(".pfx");
            openPicker.FileTypeFilter.Add(".p12");

            if (ApplicationView.Value == ApplicationViewState.Snapped)
            {
                if (ApplicationView.TryUnsnap())
                {
                    selectedFile2 = await openPicker.PickSingleFileAsync();
                }
            }
            else
            {                
                selectedFile2 = await openPicker.PickSingleFileAsync();
            }

            if (selectedFile2 != null)
            {
                CredentialPanel cp = new CredentialPanel();
                bool foco = cp.Focus(Windows.UI.Xaml.FocusState.Programmatic);
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
                                    storeData = new StoreData();
                                    storeData.LoadData();
                                    _source = storeData.GetGroupsByCategory();
                                    ItemGridView2.ItemsSource = storeData.Collection;
                                    EvaluateDeleteButton(storeData);
                                    // Le quitamos la extensión al nombre del almacén por coherencia con el borrado, que al pillarlo de la lista no tiene extensión.
                                    AfirmaMetroUtils.showMessage(labels.GetString("Info_almacen_importado") + selectedFile2.Name.Replace(selectedFile2.FileType, "") + "\".", "Importación de almacén correcta");

                                    // Lanzamos:
                                    var options = new Windows.System.LauncherOptions();
                                    options.DisplayApplicationPicker = true;
                                    var file2 = await localFolder.GetFileAsync(selectedFile2.Name);
                                    bool success = await Windows.System.Launcher.LaunchFileAsync(file2, options);

                                }
                            }
                            catch
                            {
                                AfirmaMetroUtils.showMessage(labels.GetString("Error_carga_almacen"), "Error en la importación de almacén");
                            }
                        }
            }
        }

        private async void PreDeleteStore()
        {
            // Pedimos confirmación para el borrado.
            var messageDialog = new MessageDialog("Los almacenes borrados, con sus certificados y claves privadas, no pueden ser recuperados, \n¿Desea realmente eliminar el almacén '" + ((KeyStoreFile)ItemGridView2.SelectedItem).Name + "'?", "Confirmación de borrado");
            messageDialog.Commands.Add(new UICommand("Borrar almacén", new UICommandInvokedHandler(this.DeleteStoreCommandInvokedHandler)));
            messageDialog.Commands.Add(new UICommand("Cancelar"));
            // La opción por defecto es NO borrar.
            messageDialog.DefaultCommandIndex = 1;
            messageDialog.CancelCommandIndex = 1;
            await messageDialog.ShowAsync();
        }

        private void DeleteStoreCommandInvokedHandler(IUICommand command)
        {
            DeleteStore();
        }

        private void DeleteStore()
        {
            KeyStoreFile seleccionado = (KeyStoreFile)ItemGridView2.SelectedItem;
            if (seleccionado != null)
            {
                try
                {
                    storeData.DeleteData(seleccionado.Name);
                    storeData.LoadData();
                    _source = storeData.GetGroupsByCategory();
                    ItemGridView2.ItemsSource = storeData.Collection;
                    EvaluateDeleteButton(storeData);
                }
                catch (FileNotFoundException)
                {
                    // Se ignora
                }
            }
        }

        private void Button_Borrar_Click(object sender, RoutedEventArgs e)
        {
            KeyStoreFile seleccionado = (KeyStoreFile)ItemGridView2.SelectedItem;
            if (seleccionado != null)
            {
                PreDeleteStore();
            }
        }

        private void EvaluateDeleteButton(StoreData storeData)
        {
            // Solo se habilita el botón de borrado si hay algún almacén seleccionado en la lista
            if (ItemGridView2.SelectedItem != null)
            {
                botonBorrar.IsEnabled = true;
            }
            else
            {
                botonBorrar.IsEnabled = false;
            }
        }

        #region ApplicationView event handling logic
        void ShowCurrentViewState()
        {

            // Query for the current view state
            ApplicationViewState currentState = Windows.UI.ViewManagement.ApplicationView.Value;

        }

        public void OnSizeChanged(object sender, Windows.UI.Core.WindowSizeChangedEventArgs args)
        {
            switch (Windows.UI.ViewManagement.ApplicationView.Value)
            {
                case Windows.UI.ViewManagement.ApplicationViewState.Snapped:
                    VisualStateManager.GoToState(this, "Snapped", false);
                    break;
                default:
                    VisualStateManager.GoToState(this, "Portrait", false);
                    break;
            }

            this.ShowCurrentViewState();
        }
        #endregion

        #region Adding and removing a SizeChanged event handler
        void AboutPageInit()
        {
            // Register for the window resize handler
            Window.Current.SizeChanged += OnSizeChanged;

        }

        void AboutPageReset()
        {
            Window.Current.SizeChanged -= OnSizeChanged;
        }
        #endregion

    }

}