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
/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import javax.swing.ImageIcon;

/**
 *	Constantes para contrase&ntilde;as
 */
final class Constants {

	private Constants() {
		// No permitimos la instanciacion
	}

    /** Width inicial de los JAccessibilityFrame de la aplicacion. */
    static final int WINDOW_INITIAL_WIDTH = 650;

    /**
     * Height inicial de los JAccessibilityFrame de la aplicacion.
     */
    static final int WINDOW_INITIAL_HEIGHT = 370;

    /**
     * Width inicial de los JAccessibilityDialog de la aplicacion.
     */
    static final int OPTION_INITIAL_WIDTH = 525;

    /**
     * Height inicial de los JAccessibilityDialog de la aplicacion.
     */
    static final int OPTION_INITIAL_HEIGHT = 600;

    /**
     * Width inicial de los JAccessibilityDialog de la aplicacion con variacion de fuente.
     */
    static final int OPTION_FONT_INITIAL_WIDTH = 650;

    /**
     * Height inicial de los JAccessibilityDialog de la aplicacion con variacion de fuente.
     */
    static final int OPTION_FONT_INITIAL_HEIGHT = 655;

    /**
     * Width inicial de los JAccessibilityDialog de la aplicacion con variacion de fuente en LINUX.
     */
    static final int OPTION_FONT_INITIAL_WIDTH_LINUX = 635;

    /**
     * Height inicial de los JAccessibilityDialog de la aplicacion con variacion de fuente en LINUX.
     */
    static final int OPTION_FONT_INITIAL_HEIGHT_LINUX = 700;

    /**
     * Width inicial de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_INITIAL_WIDTH = 530;

    /**
     * Height inicial de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_INITIAL_HEIGHT = 210;

    /**
     * Width inicial de los JAccessibilityCustomDialog de Confirmacion de Firma de la aplicacion.
     */
    static final int CUSTOMCONFIRMATION_INITIAL_WIDTH = 460;

    /**
     * Height inicial de los JAccessibilityCustomDialog de Confirmacion de Firma de la aplicacion.
     */
    static final int CUSTOMCONFIRMATION_INITIAL_HEIGHT = 190;
    
    /**
     * Width inicial de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_FONT_INITIAL_WIDTH = 555;

    /**
     * Height inicial de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_FONT_INITIAL_HEIGHT = 250;

    /**
     * Width de maximizado de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_MAX_WIDTH = 770;

    /**
     * Height de maximizado de los JAccessibilityCustomDialog de la aplicacion.
     */
    static final int CUSTOMDIALOG_MAX_HEIGHT = 299;
    
    /**
     * Width de maximizado de los JAccessibilityCustomDialog de confirmacion de firma de la aplicacion.
     */
    static final int CUSTOMCONFIRMATION_MAX_WIDTH = 700;

    /**
     * Height de maximizado de los JAccessibilityCustomDialog de confirmacion de firma de la aplicacion.
     */
    static final int CUSTOMCONFIRMATION_MAX_HEIGHT = 305;

    /**
     * Factor de redimensionado para las imagenes
     */
    static final double RESIZING_IMAGES_FACTOR = 0.0015;

    /**
     * Margen a restar para calcular el tamano de una ventana maximizada para linux.
     */
    static final int MAXIMIZE_VERTICAL_MARGIN_LINUX = 52;

	 /**
	 * Variable que almacena el icono original del boton de maximizar.
	 */
	static final ImageIcon IMAGEICON_MAXIMIZE = new ImageIcon(Constants.class.getResource("/images/maximize.png")); //$NON-NLS-1$

	 /**
	 * Variable que almacena el icono original del boton de restaurar.
	 */
	static final ImageIcon IMAGEICONRESTORE = new ImageIcon(Constants.class.getResource("/images/restore.png")); //$NON-NLS-1$

	/**
	 * C&oacute;digo HTML para el salto de l&iacute;nea
	 */
	static final String HTML_SALTO_LINEA = "<br>"; //$NON-NLS-1$

}

