package es.gob.afirma.ui.utils;

import javax.swing.Icon;
import javax.swing.JLabel;

/** Componente etiqueta que contiene un icono.
 * @author inteco */
public class IconLabel extends JLabel {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Icono. */
    private Icon icon = null;

    /** Icono original. */
    private Icon originalIcon = null;

    /** Constructor. */
    public IconLabel() {

    }

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return this.icon;
    }

    /** Obtener el icono original.
     * @return icono original. */
    public Icon getOriginalIcon() {
        return this.originalIcon;
    }

    /** Asigna el icono.
     * @param icon */
    @Override
    public void setIcon(final Icon icon) {
        this.icon = icon;
    }

    /** Asignar el icono original.
     * @param originalIcon */
    public void setOriginalIcon(final Icon originalIcon) {
        this.originalIcon = originalIcon;
    }

}
