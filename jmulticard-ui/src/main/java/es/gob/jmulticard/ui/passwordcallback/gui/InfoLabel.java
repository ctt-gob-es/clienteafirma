/*
 * Controlador Java de la Secretaría de Estado de Administraciones Públicas
 * para el DNI electrónico.
 *
 * El Controlador Java para el DNI electrónico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electrónico en aplicaciones Java de terceros 
 * para la realización de procesos de autenticación, firma electrónica y validación 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electrónico, así como la realización 
 * de operaciones criptográficas de firma con el DNI electrónico. El Controlador ha 
 * sido diseñado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Dirección General de Modernización Administrativa, Procedimientos 
 * e Impulso de la Administración Electrónica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrán elegir bajo cual de las
 * licencias desean utilizar el código fuente. Su elección deberá reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinará
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la versión 2.1 de la Licencia, o en una versión posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * European Union Public License publicada por la Comisión Europea, 
 * tanto en la versión 1.1 de la Licencia, o en una versión posterior.
 * 
 * Debería recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://www.gnu.org/licenses/>.
 * 
 * Debería recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea útil, pero
 * SIN NINGUNA GARANTÍA; incluso sin la garantía implícita de comercialización
 * o idoneidad para un propósito particular.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import javax.swing.JLabel;

/** Etiquetas de presentaci&oacute;n de texto.
 * @author lmerayo */
final class InfoLabel extends JLabel {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /** Constructor sencillo.
     * Se utilizar&aacute; para etiquetas con un componente asociado.
     * @param text texto de la etiqueta */
    InfoLabel(final String text) {
        super("<HTML>" + text + "</HTML>"); //$NON-NLS-1$ //$NON-NLS-2$
        this.setOpaque(false);
        Utils.remarcar(this);
        Utils.setContrastColor(this);
        Utils.setFontBold(this);

    }

    /** Constructor de la clase
     * @param text
     *        Texto a mostrar
     * @param opaque
     *        Indica si el componente ser&aacute; opaco */
    InfoLabel(final String text, final boolean opaque) {
        super("<HTML>" + text + "</HTML>"); //$NON-NLS-1$ //$NON-NLS-2$
        this.setFocusable(true); // Focusable
        this.setOpaque(opaque);
        Utils.remarcar(this);
        Utils.setContrastColor(this);
        Utils.setFontBold(this);
    }
}
