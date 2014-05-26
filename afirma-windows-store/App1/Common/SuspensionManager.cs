using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Text;
using System.Threading.Tasks;
using Windows.ApplicationModel;
using Windows.Storage;
using Windows.Storage.Streams;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace App1.Common
{
    /// <summary>
    /// SuspensionManager captura el estado de sesión global para simplificar la administración de la duración de los procesos
    /// de una aplicación. Tenga en cuenta que el estado de la sesión se borrará automáticamente bajo diversas
    /// condiciones y solamente debería usarse para almacenar información que sería conveniente
    /// para el tema, pero se debe descartar cuando la aplicación se bloquea o se
    /// actualiza.
    /// </summary>
    internal sealed class SuspensionManager
    {
        private static Dictionary<string, object> _sessionState = new Dictionary<string, object>();
        private static List<Type> _knownTypes = new List<Type>();
        private const string sessionStateFilename = "_sessionState.xml";

        /// <summary>
        /// Proporciona acceso al estado de sesión global para la sesión actual. Este estado
        /// se serializa mediante <see cref="SaveAsync"/> y se restaura con
        /// <see cref="RestoreAsync"/>, por lo que los valores deben poder serializarse mediante
        /// <see cref="DataContractSerializer"/> y ser tan compactos como sea posible. Se recomienda el uso
        /// de cadenas y otros tipos de datos independientes.
        /// </summary>
        public static Dictionary<string, object> SessionState
        {
            get { return _sessionState; }
        }

        /// <summary>
        /// Lista de tipos personalizados proporcionados a <see cref="DataContractSerializer"/> al
        /// leer y escribir el estado de la sesión. Pueden agregarse tipos adicionales, inicialmente
        /// vacíos, para personalizar el proceso de serialización.
        /// </summary>
        public static List<Type> KnownTypes
        {
            get { return _knownTypes; }
        }

        /// <summary>
        /// Guardar el <see cref="SessionState"/> actual. Toda instancia de <see cref="Frame"/>
        /// registrada en <see cref="RegisterFrame"/> también conservará la
        /// pila de navegación actual que, a su vez, ofrece a la <see cref="Page"/> activa la oportunidad
        /// de guardar su estado.
        /// </summary>
        /// <returns>Tarea asincrónica que refleja cuándo se ha guardado el estado de la sesión.</returns>
        public static async Task SaveAsync()
        {
            try
            {
                // Guardar el estado de navegación para todos los marcos registrados
                foreach (var weakFrameReference in _registeredFrames)
                {
                    Frame frame;
                    if (weakFrameReference.TryGetTarget(out frame))
                    {
                        SaveFrameNavigationState(frame);
                    }
                }

                // Serializar el estado de la sesión de forma sincrónica para impedir el acceso asincrónico al estado
                // compartido
                MemoryStream sessionData = new MemoryStream();
                DataContractSerializer serializer = new DataContractSerializer(typeof(Dictionary<string, object>), _knownTypes);
                serializer.WriteObject(sessionData, _sessionState);

                // Obtener un flujo de salida para el archivo SessionState y escribir el estado de forma asincrónica
                StorageFile file = await ApplicationData.Current.LocalFolder.CreateFileAsync(sessionStateFilename, CreationCollisionOption.ReplaceExisting);
                using (Stream fileStream = await file.OpenStreamForWriteAsync())
                {
                    sessionData.Seek(0, SeekOrigin.Begin);
                    await sessionData.CopyToAsync(fileStream);
                    await fileStream.FlushAsync();
                }
            }
            catch (Exception e)
            {
                throw new SuspensionManagerException(e);
            }
        }

        /// <summary>
        /// Restaura el <see cref="SessionState"/> previamente guardado. Toda instancia de <see cref="Frame"/>
        /// registrada en <see cref="RegisterFrame"/> también restaurará su estado de navegación
        /// anterior lo que, a su vez, ofrece a su <see cref="Page"/> activa la oportunidad de restaurar su
        /// estado.
        /// </summary>
        /// <returns>Tarea asincrónica que refleja cuándo se ha leído el estado de la sesión. No hay
        /// que confiar en el contenido de <see cref="SessionState"/>hasta que no se complete
        /// esta tarea.</returns>
        public static async Task RestoreAsync()
        {
            _sessionState = new Dictionary<String, Object>();

            try
            {
                // Obtener el flujo de entrada para el archivo SessionState
                StorageFile file = await ApplicationData.Current.LocalFolder.GetFileAsync(sessionStateFilename);
                using (IInputStream inStream = await file.OpenSequentialReadAsync())
                {
                    // Deserializar el estado de sesión
                    DataContractSerializer serializer = new DataContractSerializer(typeof(Dictionary<string, object>), _knownTypes);
                    _sessionState = (Dictionary<string, object>)serializer.ReadObject(inStream.AsStreamForRead());
                }

                // Restaurar los marcos registrados a su estado guardado
                foreach (var weakFrameReference in _registeredFrames)
                {
                    Frame frame;
                    if (weakFrameReference.TryGetTarget(out frame))
                    {
                        frame.ClearValue(FrameSessionStateProperty);
                        RestoreFrameNavigationState(frame);
                    }
                }
            }
            catch (Exception e)
            {
                throw new SuspensionManagerException(e);
            }
        }

        private static DependencyProperty FrameSessionStateKeyProperty =
            DependencyProperty.RegisterAttached("_FrameSessionStateKey", typeof(String), typeof(SuspensionManager), null);
        private static DependencyProperty FrameSessionStateProperty =
            DependencyProperty.RegisterAttached("_FrameSessionState", typeof(Dictionary<String, Object>), typeof(SuspensionManager), null);
        private static List<WeakReference<Frame>> _registeredFrames = new List<WeakReference<Frame>>();

        /// <summary>
        /// Registra una instancia de <see cref="Frame"/> para permitir que su historial de navegación se guarde
        /// y restaure desde <see cref="SessionState"/>. Los marcos deben registrarse una vez
        /// inmediatamente tras su creación si van a participar en la administración del estado de sesión. Tras el
        /// registro, si el estado ya se ha restaurado para la clave especificada,
        /// se restaurará inmediatamente el historial de navegación. Las invocaciones subsiguientes de
        /// <see cref="RestoreAsync"/> también restaurarán el historial de navegación.
        /// </summary>
        /// <param name="frame">Instancia cuyo historial de navegación debería administrarse mediante
        /// <see cref="SuspensionManager"/></param>
        /// <param name="sessionStateKey">Clave única de <see cref="SessionState"/> que se usa para
        /// almacenar información relacionada con la navegación.</param>
        public static void RegisterFrame(Frame frame, String sessionStateKey)
        {
            if (frame.GetValue(FrameSessionStateKeyProperty) != null)
            {
                throw new InvalidOperationException("Frames can only be registered to one session state key");
            }

            if (frame.GetValue(FrameSessionStateProperty) != null)
            {
                throw new InvalidOperationException("Frames must be either be registered before accessing frame session state, or not registered at all");
            }

            // Usar una propiedad de dependencia para asociar la clave de sesión a un marco y mantener una lista de marcos cuyo
            // estado de navegación deba administrarse
            frame.SetValue(FrameSessionStateKeyProperty, sessionStateKey);
            _registeredFrames.Add(new WeakReference<Frame>(frame));

            // Comprobar si el estado de navegación puede restaurarse
            RestoreFrameNavigationState(frame);
        }

        /// <summary>
        /// Desasocia un <see cref="Frame"/> previamente registrado mediante <see cref="RegisterFrame"/>
        /// de <see cref="SessionState"/>. Todo estado de navegación previamente capturado se
        /// quitará.
        /// </summary>
        /// <param name="frame">Instancia cuyo historial de navegación debería dejar de ser
        /// administrado.</param>
        public static void UnregisterFrame(Frame frame)
        {
            // Quitar estado de la sesión y quitar el marco de la lista de marcos cuyo estado
            // de navegación se guardará (junto con cualquier referencia débil que haya dejado de estar accesible)
            SessionState.Remove((String)frame.GetValue(FrameSessionStateKeyProperty));
            _registeredFrames.RemoveAll((weakFrameReference) =>
            {
                Frame testFrame;
                return !weakFrameReference.TryGetTarget(out testFrame) || testFrame == frame;
            });
        }

        /// <summary>
        /// Proporciona almacenamiento para el estado de sesión asociado al <see cref="Frame"/> especificado.
        /// El estado de sesión de los marcos registrados previamente en <see cref="RegisterFrame"/>
        /// se guardó y restauró automáticamente como parte del
        /// <see cref="SessionState"/> global. Los marcos no registrados tienen un estado transitorio
        /// que puede ser útil al restaurar páginas descartadas de la
        /// memoria caché de navegación.
        /// </summary>
        /// <remarks>Las aplicaciones pueden elegir basarse en <see cref="LayoutAwarePage"/> para administrar
        /// el estado específico de página en lugar de trabajar directamente con el estado de sesión del marco.</remarks>
        /// <param name="frame">Instancia para la que se desea obtener el estado de sesión.</param>
        /// <returns>Colección de estados sujeta al mismo mecanismo de serialización que
        /// <see cref="SessionState"/>.</returns>
        public static Dictionary<String, Object> SessionStateForFrame(Frame frame)
        {
            var frameState = (Dictionary<String, Object>)frame.GetValue(FrameSessionStateProperty);

            if (frameState == null)
            {
                var frameSessionKey = (String)frame.GetValue(FrameSessionStateKeyProperty);
                if (frameSessionKey != null)
                {
                    // Los marcos registrados reflejan el estado de sesión correspondiente
                    if (!_sessionState.ContainsKey(frameSessionKey))
                    {
                        _sessionState[frameSessionKey] = new Dictionary<String, Object>();
                    }
                    frameState = (Dictionary<String, Object>)_sessionState[frameSessionKey];
                }
                else
                {
                    // Los marcos no registrados tienen un estado transitorio
                    frameState = new Dictionary<String, Object>();
                }
                frame.SetValue(FrameSessionStateProperty, frameState);
            }
            return frameState;
        }

        private static void RestoreFrameNavigationState(Frame frame)
        {
            var frameState = SessionStateForFrame(frame);
            if (frameState.ContainsKey("Navigation"))
            {
                frame.SetNavigationState((String)frameState["Navigation"]);
            }
        }

        private static void SaveFrameNavigationState(Frame frame)
        {
            var frameState = SessionStateForFrame(frame);
            frameState["Navigation"] = frame.GetNavigationState();
        }
    }
    public class SuspensionManagerException : Exception
    {
        public SuspensionManagerException()
        {
        }

        public SuspensionManagerException(Exception e)
            : base("SuspensionManager failed", e)
        {

        }
    }
}
