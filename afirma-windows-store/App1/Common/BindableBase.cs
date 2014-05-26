using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using Windows.UI.Xaml.Data;

namespace App1.Common
{
    /// <summary>
    /// Implementación de <see cref="INotifyPropertyChanged"/> para simplificar los modelos.
    /// </summary>
    [Windows.Foundation.Metadata.WebHostHidden]
    public abstract class BindableBase : INotifyPropertyChanged
    {
        /// <summary>
        /// Evento de multidifusión para notificaciones de cambio de propiedad.
        /// </summary>
        public event PropertyChangedEventHandler PropertyChanged;

        /// <summary>
        /// Comprueba si una propiedad coincide ya con el valor deseado. Establece la propiedad y
        /// notifica a los agentes de escucha solo si es necesario.
        /// </summary>
        /// <typeparam name="T">Tipo de la propiedad.</typeparam>
        /// <param name="storage">Referencia a una propiedad con captador y establecedor.</param>
        /// <param name="value">Valor deseado para la propiedad.</param>
        /// <param name="propertyName">Nombre de la propiedad usada para notificar a los agentes de escucha. Este
        /// valor es opcional y se puede proporcionar automáticamente cuando se invoca desde compiladores que
        /// admiten CallerMemberName.</param>
        /// <returns>True si se cambió el valor, false si el valor existente coincidía con el
        /// valor deseado.</returns>
        protected bool SetProperty<T>(ref T storage, T value, [CallerMemberName] String propertyName = null)
        {
            if (object.Equals(storage, value)) return false;

            storage = value;
            this.OnPropertyChanged(propertyName);
            return true;
        }

        /// <summary>
        /// Notifica a los agentes de escucha que ha cambiado un valor de propiedad.
        /// </summary>
        /// <param name="propertyName">Nombre de la propiedad usada para notificar a los agentes de escucha. Este
        /// valor es opcional y se puede proporcionar automáticamente cuando se invoca desde compiladores
        /// que admiten <see cref="CallerMemberNameAttribute"/>.</param>
        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            var eventHandler = this.PropertyChanged;
            if (eventHandler != null)
            {
                eventHandler(this, new PropertyChangedEventArgs(propertyName));
            }
        }
    }
}
