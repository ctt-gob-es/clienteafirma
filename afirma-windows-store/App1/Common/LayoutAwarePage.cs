using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.System;
using Windows.UI.Core;
using Windows.UI.ViewManagement;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Navigation;

namespace App1.Common
{
    /// <summary>
    /// Implementación típica de Page que proporciona varias facilidades importantes:
    /// <list type="bullet">
    /// <item>
    /// <description>Asignación del estado de vista de la aplicación a un estado visual</description>
    /// </item>
    /// <item>
    /// <description>Controladores de eventos GoBack, GoForward y GoHome event</description>
    /// </item>
    /// <item>
    /// <description>Accesos directos de mouse y teclado para la navegación</description>
    /// </item>
    /// <item>
    /// <description>Administración del estado para la administración de la navegación y la duración de los procesos</description>
    /// </item>
    /// <item>
    /// <description>Modelo de vista predeterminada</description>
    /// </item>
    /// </list>
    /// </summary>
    [Windows.Foundation.Metadata.WebHostHidden]
    public class LayoutAwarePage : Page
    {
        /// <summary>
        /// Identifica la propiedad de dependencia <see cref="DefaultViewModel"/>.
        /// </summary>
        public static readonly DependencyProperty DefaultViewModelProperty =
            DependencyProperty.Register("DefaultViewModel", typeof(IObservableMap<String, Object>),
            typeof(LayoutAwarePage), null);

        private List<Control> _layoutAwareControls;

        /// <summary>
        /// Inicializa una nueva instancia de la clase <see cref="LayoutAwarePage"/>.
        /// </summary>
        public LayoutAwarePage()
        {
            if (Windows.ApplicationModel.DesignMode.DesignModeEnabled) return;

            // Crea un modelo de vista predeterminada vacía
            this.DefaultViewModel = new ObservableDictionary<String, Object>();

            // Cuando esta página forma parte del árbol visual, realizar dos cambios:
            // 1) Asignar el estado de vista de la aplicación a un estado visual para la página
            // 2) Controlar las solicitudes de navegación con el teclado y el mouse
            this.Loaded += (sender, e) =>
            {
                this.StartLayoutUpdates(sender, e);

                // La navegación con el teclado y el mouse se aplica únicamente cuando se ocupa toda la ventana
                if (this.ActualHeight == Window.Current.Bounds.Height &&
                    this.ActualWidth == Window.Current.Bounds.Width)
                {
                    // Escuchar a la ventana directamente de forma que el foco no es necesario
                    Window.Current.CoreWindow.Dispatcher.AcceleratorKeyActivated +=
                        CoreDispatcher_AcceleratorKeyActivated;
                    Window.Current.CoreWindow.PointerPressed +=
                        this.CoreWindow_PointerPressed;
                }
            };

            // Deshacer los cambios cuando la página ya no está visible
            this.Unloaded += (sender, e) =>
            {
                this.StopLayoutUpdates(sender, e);
                Window.Current.CoreWindow.Dispatcher.AcceleratorKeyActivated -=
                    CoreDispatcher_AcceleratorKeyActivated;
                Window.Current.CoreWindow.PointerPressed -=
                    this.CoreWindow_PointerPressed;
            };
        }

        /// <summary>
        /// Implementación de <see cref="IObservableMap&lt;String, Object&gt;"/> diseñada para
        /// usarla como modelo de vista trivial.
        /// </summary>
        protected IObservableMap<String, Object> DefaultViewModel
        {
            get
            {
                return this.GetValue(DefaultViewModelProperty) as IObservableMap<String, Object>;
            }

            set
            {
                this.SetValue(DefaultViewModelProperty, value);
            }
        }

        #region Admitir navegación

        /// <summary>
        /// Se invoca como controlador de eventos para navegar hacia atrás en el
        /// <see cref="Frame"/> asociado de la página hasta alcanzar el principio de la pila de navegación.
        /// </summary>
        /// <param name="sender">Instancia que desencadena el evento.</param>
        /// <param name="e">Datos de evento que describen las condiciones que dan lugar al evento.</param>
        protected virtual void GoHome(object sender, RoutedEventArgs e)
        {
            // Usar el entorno de exploración para volver a la página superior
            if (this.Frame != null)
            {
                while (this.Frame.CanGoBack) this.Frame.GoBack();
            }
        }

        /// <summary>
        /// Se invoca como controlador de eventos para navegar hacia atrás en la pila de navegación
        /// asociada con el <see cref="Frame"/> de esta página.
        /// </summary>
        /// <param name="sender">Instancia que desencadena el evento.</param>
        /// <param name="e">Datos de evento que describen las condiciones que dan lugar al
        /// evento.</param>
        protected virtual void GoBack(object sender, RoutedEventArgs e)
        {
            // Usar el entorno de exploración para volver a la página anterior
            if (this.Frame != null && this.Frame.CanGoBack) this.Frame.GoBack();
        }

        /// <summary>
        /// Se invoca como controlador de eventos para navegar hacia delante en la pila de navegación
        /// asociada con el <see cref="Frame"/> de esta página.
        /// </summary>
        /// <param name="sender">Instancia que desencadena el evento.</param>
        /// <param name="e">Datos de evento que describen las condiciones que dan lugar al
        /// evento.</param>
        protected virtual void GoForward(object sender, RoutedEventArgs e)
        {
            // Usar el marco de navegación para ir a la siguiente página
            if (this.Frame != null && this.Frame.CanGoForward) this.Frame.GoForward();
        }

        /// <summary>
        /// Se invoca en cada pulsación de tecla, incluidas las teclas del sistema como las combinaciones con la tecla Alt, cuando
        /// esta página está activa y ocupa toda la ventana. Se usa para detectar la navegación con el teclado
        /// entre páginas incluso cuando la página no tiene el foco.
        /// </summary>
        /// <param name="sender">Instancia que desencadena el evento.</param>
        /// <param name="args">Datos de evento que describen las condiciones que dan lugar al evento.</param>
        private void CoreDispatcher_AcceleratorKeyActivated(CoreDispatcher sender,
            AcceleratorKeyEventArgs args)
        {
            var virtualKey = args.VirtualKey;

            // Investigar más solo cuando se presionan las teclas Izquierda, Derecha o las teclas
            // dedicadas Repág o Avpág
            if ((args.EventType == CoreAcceleratorKeyEventType.SystemKeyDown ||
                args.EventType == CoreAcceleratorKeyEventType.KeyDown) &&
                (virtualKey == VirtualKey.Left || virtualKey == VirtualKey.Right ||
                (int)virtualKey == 166 || (int)virtualKey == 167))
            {
                var coreWindow = Window.Current.CoreWindow;
                var downState = CoreVirtualKeyStates.Down;
                bool menuKey = (coreWindow.GetKeyState(VirtualKey.Menu) & downState) == downState;
                bool controlKey = (coreWindow.GetKeyState(VirtualKey.Control) & downState) == downState;
                bool shiftKey = (coreWindow.GetKeyState(VirtualKey.Shift) & downState) == downState;
                bool noModifiers = !menuKey && !controlKey && !shiftKey;
                bool onlyAlt = menuKey && !controlKey && !shiftKey;

                if (((int)virtualKey == 166 && noModifiers) ||
                    (virtualKey == VirtualKey.Left && onlyAlt))
                {
                    // Cuando se presionan las teclas Repág o Alt+Izquierda, navegar hacia atrás
                    args.Handled = true;
                    this.GoBack(this, new RoutedEventArgs());
                }
                else if (((int)virtualKey == 167 && noModifiers) ||
                    (virtualKey == VirtualKey.Right && onlyAlt))
                {
                    // Cuando se presionan las teclas Avpág o Alt+Derecha, navegar hacia delante
                    args.Handled = true;
                    this.GoForward(this, new RoutedEventArgs());
                }
            }
        }

        /// <summary>
        /// Se invoca en cada clic del mouse, punteo en la pantalla táctil o una interacción equivalente cuando esta
        /// página está activa y ocupa toda la ventana. Se usa para detectar los clics de botón del mouse
        /// siguiente y anterior del estilo del explorador para navegar entre páginas.
        /// </summary>
        /// <param name="sender">Instancia que desencadena el evento.</param>
        /// <param name="args">Datos de evento que describen las condiciones que dan lugar al evento.</param>
        private void CoreWindow_PointerPressed(CoreWindow sender,
            PointerEventArgs args)
        {
            var properties = args.CurrentPoint.Properties;

            // Omitir la presión simultánea de botones con los botones Izquierda, Derecha y Medio
            if (properties.IsLeftButtonPressed || properties.IsRightButtonPressed ||
                properties.IsMiddleButtonPressed) return;

            // Si se presiona Repág o Avpág (pero no ambos), navegar adecuadamente
            bool backPressed = properties.IsXButton1Pressed;
            bool forwardPressed = properties.IsXButton2Pressed;
            if (backPressed ^ forwardPressed)
            {
                args.Handled = true;
                if (backPressed) this.GoBack(this, new RoutedEventArgs());
                if (forwardPressed) this.GoForward(this, new RoutedEventArgs());
            }
        }

        #endregion

        #region Conmutación de estado visual

        /// <summary>
        /// Se invoca como controlador de eventos, normalmente en el evento <see cref="FrameworkElement.Loaded"/>
        /// de un <see cref="Control"/> dentro de la página, para indicar que el remitente debe
        /// comenzar a recibir cambios de administración del estado visual que corresponden a cambios de estado
        /// de vista de la aplicación.
        /// </summary>
        /// <param name="sender">Instancia de <see cref="Control"/> que admite administración
        /// del estado visual correspondiente a estados de vista.</param>
        /// <param name="e">Datos de evento que describen cómo se realizó la solicitud.</param>
        /// <remarks>El estado de vista actual se usará inmediatamente para establecer el correspondiente
        /// estado visual cuando se soliciten actualizaciones de diseño. Se recomienda encarecidamente
        /// un controlador de eventos <see cref="FrameworkElement.Unloaded"/> correspondiente conectado a
        /// <see cref="StopLayoutUpdates"/>. Las instancias de
        /// <see cref="LayoutAwarePage"/> invocan automáticamente a estos controladores en sus eventos Loaded y
        /// Unloaded.</remarks>
        /// <seealso cref="DetermineVisualState"/>
        /// <seealso cref="InvalidateVisualState"/>
        public void StartLayoutUpdates(object sender, RoutedEventArgs e)
        {
            var control = sender as Control;
            if (control == null) return;
            if (this._layoutAwareControls == null)
            {
                // Comenzar a escuchar para ver cambios de estado cuando hay controles interesados en actualizaciones
                Window.Current.SizeChanged += this.WindowSizeChanged;
                this._layoutAwareControls = new List<Control>();
            }
            this._layoutAwareControls.Add(control);

            // Establecer el estado visual inicial del control
            VisualStateManager.GoToState(control, DetermineVisualState(ApplicationView.Value), false);
        }

        private void WindowSizeChanged(object sender, WindowSizeChangedEventArgs e)
        {
            this.InvalidateVisualState();
        }

        /// <summary>
        /// Se invoca como controlador de eventos, normalmente en el evento <see cref="FrameworkElement.Unloaded"/>
        /// de un <see cref="Control"/>, para indicar que el remitente debe comenzar a recibir
        /// cambios de administración del estado visual que corresponden a los cambios de estado de vista de la aplicación.
        /// </summary>
        /// <param name="sender">Instancia de <see cref="Control"/> que admite administración
        /// del estado visual correspondiente a estados de vista.</param>
        /// <param name="e">Datos de evento que describen cómo se realizó la solicitud.</param>
        /// <remarks>El estado de vista actual se usará inmediatamente para establecer el correspondiente
        /// estado visual cuando se solicitan actualizaciones.</remarks>
        /// <seealso cref="StartLayoutUpdates"/>
        public void StopLayoutUpdates(object sender, RoutedEventArgs e)
        {
            var control = sender as Control;
            if (control == null || this._layoutAwareControls == null) return;
            this._layoutAwareControls.Remove(control);
            if (this._layoutAwareControls.Count == 0)
            {
                // Dejar de escuchar cambios de estado de vista cuando no hay controles interesados en actualizaciones
                this._layoutAwareControls = null;
                Window.Current.SizeChanged -= this.WindowSizeChanged;
            }
        }

        /// <summary>
        /// Convierte valores <see cref="ApplicationViewState"/> en cadenas para la administración
        /// del estado visual dentro de la página. La implementación predeterminada usa los nombres de valores enum.
        /// Las subclases pueden invalidar este método para controlar la combinación de asignación usada.
        /// </summary>
        /// <param name="viewState">Ver el estado para el que se desea un estado visual.</param>
        /// <returns>Nombre de estado visual usado para controlar el
        /// <see cref="VisualStateManager"/></returns>
        /// <seealso cref="InvalidateVisualState"/>
        protected virtual string DetermineVisualState(ApplicationViewState viewState)
        {
            return viewState.ToString();
        }

        /// <summary>
        /// Actualiza todos los controles que escuchan cambios de estado visual con el
        /// estado visual correcto.
        /// </summary>
        /// <remarks>
        /// Se usa normalmente junto con la invalidación de <see cref="DetermineVisualState"/> para
        /// indicar que se puede devolver un valor diferente incluso si el estado de vista
        /// no ha cambiado.
        /// </remarks>
        public void InvalidateVisualState()
        {
            if (this._layoutAwareControls != null)
            {
                string visualState = DetermineVisualState(ApplicationView.Value);
                foreach (var layoutAwareControl in this._layoutAwareControls)
                {
                    VisualStateManager.GoToState(layoutAwareControl, visualState, false);
                }
            }
        }

        #endregion

        #region Administración de la duración de los procesos

        private String _pageKey;

        /// <summary>
        /// Se invoca cuando esta página se va a mostrar en un objeto Frame.
        /// </summary>
        /// <param name="e">Datos de evento que describen cómo se llegó a esta página. La propiedad Parameter
        /// proporciona el grupo que se va a mostrar.</param>
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
            // La devolución a una página almacenada en caché mediante navegación no debe desencadenar una carga de estado
            if (this._pageKey != null) return;

            var frameState = SuspensionManager.SessionStateForFrame(this.Frame);
            this._pageKey = "Page-" + this.Frame.BackStackDepth;

            if (e.NavigationMode == NavigationMode.New)
            {
                // Borrar el estado existente para la navegación hacia delante cuando se agregue una nueva página a la
                // pila de navegación
                var nextPageKey = this._pageKey;
                int nextPageIndex = this.Frame.BackStackDepth;
                while (frameState.Remove(nextPageKey))
                {
                    nextPageIndex++;
                    nextPageKey = "Page-" + nextPageIndex;
                }

                // Pasar el parámetro de navegación a la nueva página
                this.LoadState(e.Parameter, null);
            }
            else
            {
                // Pasar el parámetro de navegación y el estado de página mantenido a la página usando
                // la misma estrategia para cargar el estado suspendido y volver a crear las páginas descartadas
                // a partir de la memoria caché
                this.LoadState(e.Parameter, (Dictionary<String, Object>)frameState[this._pageKey]);
            }
        }

        /// <summary>
        /// Se invoca cuando esta página deja de estar visible en un marco.
        /// </summary>
        /// <param name="e">Datos de evento que describen cómo se llegó a esta página. La propiedad Parameter
        /// proporciona el grupo que se va a mostrar.</param>
        protected override void OnNavigatedFrom(NavigationEventArgs e)
        {
            var frameState = SuspensionManager.SessionStateForFrame(this.Frame);
            var pageState = new Dictionary<String, Object>();
            this.SaveState(pageState);
            frameState[_pageKey] = pageState;
        }

        /// <summary>
        /// Rellena la página con el contenido pasado durante la navegación. Cualquier estado guardado se
        /// proporciona también al crear de nuevo una página a partir de una sesión anterior.
        /// </summary>
        /// <param name="navigationParameter">Valor de parámetro pasado a
        /// <see cref="Frame.Navigate(Type, Object)"/> cuando se solicitó inicialmente esta página.
        /// </param>
        /// <param name="pageState">Diccionario del estado mantenido por esta página durante una sesión
        /// anterior. Será null la primera vez que se visite una página.</param>
        protected virtual void LoadState(Object navigationParameter, Dictionary<String, Object> pageState)
        {
        }

        /// <summary>
        /// Mantiene el estado asociado con esta página en caso de que se suspenda la aplicación o
        /// se descarte la página de la memoria caché de navegación. Los valores deben cumplir los requisitos
        /// de serialización de <see cref="SuspensionManager.SessionState"/>.
        /// </summary>
        /// <param name="pageState">Diccionario vacío para rellenar con un estado serializable.</param>
        protected virtual void SaveState(Dictionary<String, Object> pageState)
        {
        }

        #endregion

        /// <summary>
        /// Implementación de IObservableMap que admite reentrada para usar como
        /// modelo de vista predeterminada.
        /// </summary>
        private class ObservableDictionary<K, V> : IObservableMap<K, V>
        {
            private class ObservableDictionaryChangedEventArgs : IMapChangedEventArgs<K>
            {
                public ObservableDictionaryChangedEventArgs(CollectionChange change, K key)
                {
                    this.CollectionChange = change;
                    this.Key = key;
                }

                public CollectionChange CollectionChange { get; private set; }
                public K Key { get; private set; }
            }

            private Dictionary<K, V> _dictionary = new Dictionary<K, V>();
            public event MapChangedEventHandler<K, V> MapChanged;

            private void InvokeMapChanged(CollectionChange change, K key)
            {
                var eventHandler = MapChanged;
                if (eventHandler != null)
                {
                    eventHandler(this, new ObservableDictionaryChangedEventArgs(change, key));
                }
            }

            public void Add(K key, V value)
            {
                this._dictionary.Add(key, value);
                this.InvokeMapChanged(CollectionChange.ItemInserted, key);
            }

            public void Add(KeyValuePair<K, V> item)
            {
                this.Add(item.Key, item.Value);
            }

            public bool Remove(K key)
            {
                if (this._dictionary.Remove(key))
                {
                    this.InvokeMapChanged(CollectionChange.ItemRemoved, key);
                    return true;
                }
                return false;
            }

            public bool Remove(KeyValuePair<K, V> item)
            {
                V currentValue;
                if (this._dictionary.TryGetValue(item.Key, out currentValue) &&
                    Object.Equals(item.Value, currentValue) && this._dictionary.Remove(item.Key))
                {
                    this.InvokeMapChanged(CollectionChange.ItemRemoved, item.Key);
                    return true;
                }
                return false;
            }

            public V this[K key]
            {
                get
                {
                    return this._dictionary[key];
                }
                set
                {
                    this._dictionary[key] = value;
                    this.InvokeMapChanged(CollectionChange.ItemChanged, key);
                }
            }

            public void Clear()
            {
                var priorKeys = this._dictionary.Keys.ToArray();
                this._dictionary.Clear();
                foreach (var key in priorKeys)
                {
                    this.InvokeMapChanged(CollectionChange.ItemRemoved, key);
                }
            }

            public ICollection<K> Keys
            {
                get { return this._dictionary.Keys; }
            }

            public bool ContainsKey(K key)
            {
                return this._dictionary.ContainsKey(key);
            }

            public bool TryGetValue(K key, out V value)
            {
                return this._dictionary.TryGetValue(key, out value);
            }

            public ICollection<V> Values
            {
                get { return this._dictionary.Values; }
            }

            public bool Contains(KeyValuePair<K, V> item)
            {
                return this._dictionary.Contains(item);
            }

            public int Count
            {
                get { return this._dictionary.Count; }
            }

            public bool IsReadOnly
            {
                get { return false; }
            }

            public IEnumerator<KeyValuePair<K, V>> GetEnumerator()
            {
                return this._dictionary.GetEnumerator();
            }

            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            {
                return this._dictionary.GetEnumerator();
            }

            public void CopyTo(KeyValuePair<K, V>[] array, int arrayIndex)
            {
                int arraySize = array.Length;
                foreach (var pair in this._dictionary)
                {
                    if (arrayIndex >= arraySize) break;
                    array[arrayIndex++] = pair;
                }
            }
        }
    }
}
