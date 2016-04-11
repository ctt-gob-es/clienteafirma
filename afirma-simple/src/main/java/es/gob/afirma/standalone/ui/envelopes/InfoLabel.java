package es.gob.afirma.standalone.ui.envelopes;

import javax.swing.JLabel;

/** Etiquetas de presentaci&oacute;n de texto.
 * @author lmerayo */
public class InfoLabel extends JLabel {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /** Constructor sencillo.
     * Se utilizara para etiquetas con un componente asociado.
     * @param text texto de la etiqueta */
    public InfoLabel(final String text) {
        super("<HTML>" + text + "</HTML>");  //$NON-NLS-1$//$NON-NLS-2$
        this.setOpaque(false);
    }

    /** Constructor de la clase
     * @param text
     *        Texto a mostrar
     * @param opaque
     *        Indica si el componente ser&aacute; opaco */
    public InfoLabel(final String text, final boolean opaque) {
        super("<HTML>" + text + "</HTML>"); //$NON-NLS-1$//$NON-NLS-2$
        this.setFocusable(true); // Focusable
        this.setOpaque(opaque);
    }
}
