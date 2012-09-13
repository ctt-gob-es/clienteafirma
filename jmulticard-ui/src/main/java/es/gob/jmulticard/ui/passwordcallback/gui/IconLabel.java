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

import java.awt.Dimension;

import javax.swing.Icon;
import javax.swing.JLabel;

/** Componente etiqueta que contiene un icono.
 * @author Inteco */
final class IconLabel extends JLabel {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Icono. */
    private Icon icon = null;

    /** Icono original. */
    private Icon originalIcon = null;
    
    /** Dimension original */
    private Dimension originalDimension;

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return this.icon;
    }

    /** Obtener el icono original.
     * @return icono original. */
    Icon getOriginalIcon() {
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
    void setOriginalIcon(final Icon originalIcon) {
        this.originalIcon = originalIcon;
    }

    /**
     * Retorna la dimension inicial del icono
     * @return dimension inicial del icono
     */
    public Dimension getOriginalDimension(){
    	return this.originalDimension;
    }
    
    /**
     * Establece la dimension inicial del icono
     * @param d Dimension inicial del icono
     */
    public void setOriginalDimension(Dimension d){
    	this.originalDimension = d;
    }
    
}
