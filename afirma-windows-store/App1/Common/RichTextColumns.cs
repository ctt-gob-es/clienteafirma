using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Windows.Foundation;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Documents;

namespace App1.Common
{
    /// <summary>
    /// Contenedor para <see cref="RichTextBlock"/> que crea tantas columnas de desbordamiento
    /// adicionales como sea necesario para acomodar el contenido disponible.
    /// </summary>
    /// <example>
    /// El siguiente código crea una colección de columnas de 400 píxeles de ancho y una separación de 50 píxeles
    /// para acomodar contenido arbitrario enlazado a datos:
    /// <code>
    /// <RichTextColumns>
    ///     <RichTextColumns.ColumnTemplate>
    ///         <DataTemplate>
    ///             <RichTextBlockOverflow Width="400" Margin="50,0,0,0"/>
    ///         </DataTemplate>
    ///     </RichTextColumns.ColumnTemplate>
    ///     
    ///     <RichTextBlock Width="400">
    ///         <Paragraph>
    ///             <Run Text="{Binding Content}"/>
    ///         </Paragraph>
    ///     </RichTextBlock>
    /// </RichTextColumns>
    /// </code>
    /// </example>
    /// <remarks>Se usa normalmente en una región de desplazamiento horizontal donde una cantidad no enlazada de espacio permite
    /// crear todas las columnas necesarias. Cuando se usa en un espacio de desplazamiento vertical
    /// no habrá nunca columnas adicionales.</remarks>
    [Windows.UI.Xaml.Markup.ContentProperty(Name = "RichTextContent")]
    public sealed class RichTextColumns : Panel
    {
        /// <summary>
        /// Identifica la propiedad de dependencia <see cref="RichTextContent"/>.
        /// </summary>
        public static readonly DependencyProperty RichTextContentProperty =
            DependencyProperty.Register("RichTextContent", typeof(RichTextBlock),
            typeof(RichTextColumns), new PropertyMetadata(null, ResetOverflowLayout));

        /// <summary>
        /// Identifica la propiedad de dependencia <see cref="ColumnTemplate"/>.
        /// </summary>
        public static readonly DependencyProperty ColumnTemplateProperty =
            DependencyProperty.Register("ColumnTemplate", typeof(DataTemplate),
            typeof(RichTextColumns), new PropertyMetadata(null, ResetOverflowLayout));

        /// <summary>
        /// Inicializa una nueva instancia de la clase <see cref="RichTextColumns"/>.
        /// </summary>
        public RichTextColumns()
        {
            this.HorizontalAlignment = HorizontalAlignment.Left;
        }

        /// <summary>
        /// Obtiene o establece el contenido de texto enriquecido inicial para usarlo como primera columna.
        /// </summary>
        public RichTextBlock RichTextContent
        {
            get { return (RichTextBlock)GetValue(RichTextContentProperty); }
            set { SetValue(RichTextContentProperty, value); }
        }

        /// <summary>
        /// Obtiene o establece la plantilla usada para crear
        /// instancias de <see cref="RichTextBlockOverflow"/> adicionales.
        /// </summary>
        public DataTemplate ColumnTemplate
        {
            get { return (DataTemplate)GetValue(ColumnTemplateProperty); }
            set { SetValue(ColumnTemplateProperty, value); }
        }

        /// <summary>
        /// Se invoca cuando se cambia la plantilla de contenido o desbordamiento para volver a crear el diseño de columna.
        /// </summary>
        /// <param name="d">Instancia de <see cref="RichTextColumns"/> donde se
        /// produjo el cambio.</param>
        /// <param name="e">Datos de evento que describen el cambio específico.</param>
        private static void ResetOverflowLayout(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            // Cuando se realizan cambios drásticos, recompilar el diseño de columnas desde cero
            var target = d as RichTextColumns;
            if (target != null)
            {
                target._overflowColumns = null;
                target.Children.Clear();
                target.InvalidateMeasure();
            }
        }

        /// <summary>
        /// Enumera las columnas de desbordamiento ya creadas. Debe mantener una relación de 1:1 con
        /// las instancias de la colección <see cref="Panel.Children"/> que siguen al
        /// secundario de RichTextBlock inicial.
        /// </summary>
        private List<RichTextBlockOverflow> _overflowColumns = null;

        /// <summary>
        /// Determina si se necesitan columnas de desbordamiento adicionales y si se pueden
        /// quitar columnas existentes.
        /// </summary>
        /// <param name="availableSize">Tamaño del espacio disponible, usado para limitar el
        /// número de columnas adicionales que se pueden crear.</param>
        /// <returns>Tamaño resultante del contenido original más las columnas adicionales.</returns>
        protected override Size MeasureOverride(Size availableSize)
        {
            if (this.RichTextContent == null) return new Size(0, 0);

            // Asegurarse de que RichTextBlock es un secundario, por la ausencia de
            // una lista de columnas adicionales como signo de que esto no se ha
            // hecho aún
            if (this._overflowColumns == null)
            {
                Children.Add(this.RichTextContent);
                this._overflowColumns = new List<RichTextBlockOverflow>();
            }

            // Comenzar midiendo el contenido de RichTextBlock original
            this.RichTextContent.Measure(availableSize);
            var maxWidth = this.RichTextContent.DesiredSize.Width;
            var maxHeight = this.RichTextContent.DesiredSize.Height;
            var hasOverflow = this.RichTextContent.HasOverflowContent;

            // Asegurarse de que hay suficientes columnas de desbordamiento
            int overflowIndex = 0;
            while (hasOverflow && maxWidth < availableSize.Width && this.ColumnTemplate != null)
            {
                // Usar las columnas de desbordamiento existentes hasta que se agoten. Después, crear
                // más a partir de la plantilla proporcionada
                RichTextBlockOverflow overflow;
                if (this._overflowColumns.Count > overflowIndex)
                {
                    overflow = this._overflowColumns[overflowIndex];
                }
                else
                {
                    overflow = (RichTextBlockOverflow)this.ColumnTemplate.LoadContent();
                    this._overflowColumns.Add(overflow);
                    this.Children.Add(overflow);
                    if (overflowIndex == 0)
                    {
                        this.RichTextContent.OverflowContentTarget = overflow;
                    }
                    else
                    {
                        this._overflowColumns[overflowIndex - 1].OverflowContentTarget = overflow;
                    }
                }

                // Medir la nueva columna y preparar para repetir según sea necesario
                overflow.Measure(new Size(availableSize.Width - maxWidth, availableSize.Height));
                maxWidth += overflow.DesiredSize.Width;
                maxHeight = Math.Max(maxHeight, overflow.DesiredSize.Height);
                hasOverflow = overflow.HasOverflowContent;
                overflowIndex++;
            }

            // Desconectar las columnas adicionales de la cadena de desbordamiento, quitarlas de nuestra lista privada
            // de columnas y quitarlas como secundarios
            if (this._overflowColumns.Count > overflowIndex)
            {
                if (overflowIndex == 0)
                {
                    this.RichTextContent.OverflowContentTarget = null;
                }
                else
                {
                    this._overflowColumns[overflowIndex - 1].OverflowContentTarget = null;
                }
                while (this._overflowColumns.Count > overflowIndex)
                {
                    this._overflowColumns.RemoveAt(overflowIndex);
                    this.Children.RemoveAt(overflowIndex + 1);
                }
            }

            // Notificar el tamaño final determinado
            return new Size(maxWidth, maxHeight);
        }

        /// <summary>
        /// Organiza el contenido original y todas las columnas adicionales.
        /// </summary>
        /// <param name="finalSize">Define el tamaño del área en la que deben organizarse
        /// los secundarios.</param>
        /// <returns>Tamaño del área que realmente requieren los secundarios.</returns>
        protected override Size ArrangeOverride(Size finalSize)
        {
            double maxWidth = 0;
            double maxHeight = 0;
            foreach (var child in Children)
            {
                child.Arrange(new Rect(maxWidth, 0, child.DesiredSize.Width, finalSize.Height));
                maxWidth += child.DesiredSize.Width;
                maxHeight = Math.Max(maxHeight, child.DesiredSize.Height);
            }
            return new Size(maxWidth, maxHeight);
        }
    }
}
