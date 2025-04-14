/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
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
package es.gob.afirma.keystores.jmulticard.ui;

import javax.swing.ImageIcon;

/** Constantes para contrase&ntilde;as. */
final class AccesiblityConstants {

	private AccesiblityConstants() {
		// No permitimos la instanciacion
	}

    /** Ancho inicial de los <code>JAccessibilityFrame</code> de la aplicaci&oacute;n. */
    static final int WINDOW_INITIAL_WIDTH = 650;

    /** Alto inicial de los <code>JAccessibilityFrame</code> de la aplicaci&oacute;n. */
    static final int WINDOW_INITIAL_HEIGHT = 370;

    /** Ancho inicial de los <code>JAccessibilityDialog</code> de la aplicaci&oacute;n. */
    static final int OPTION_INITIAL_WIDTH = 525;

    /** Alto inicial de los <code>JAccessibilityDialog</code> de la aplicaci&oacute;n. */
    static final int OPTION_INITIAL_HEIGHT = 600;

    /** Ancho inicial de los <code>JAccessibilityDialog</code> de la
     * aplicaci&oacute;n con variaci&oacute;n de fuente. */
    static final int OPTION_FONT_INITIAL_WIDTH = 650;

    /** Alto inicial de los <code>JAccessibilityDialog</code> de la aplicaci&oacute;n con
     * variaci&oacute;n de fuente. */
    static final int OPTION_FONT_INITIAL_HEIGHT = 655;

    /** Ancho inicial de los <code>JAccessibilityDialog</code> de la aplicaci&oacute;n con
     * variaci&oacute;n de fuente en Linux. */
    static final int OPTION_FONT_INITIAL_WIDTH_LINUX = 635;

    /** Ancho inicial de los <code>JAccessibilityDialog</code> de la aplicaci&oacute;n con
     * variaci&oacute;n de fuente en Linux. */
    static final int OPTION_FONT_INITIAL_HEIGHT_LINUX = 700;

    /** Ancho inicial de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_INITIAL_WIDTH = 530;

    /** Alto inicial de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_INITIAL_HEIGHT = 210;

    /** Ancho inicial de los <code>AbstractJAccessibilityCustomDialog</code> de
     * confirmaci&oacute;n de firma de la aplicaci&oacute;n. */
    static final int CUSTOMCONFIRMATION_INITIAL_WIDTH = 460;

    /** Alto inicial de los <code>AbstractJAccessibilityCustomDialog</code> de
     * confirmaci&oacute;n de firma de la aplicaci&oacute;n. */
    static final int CUSTOMCONFIRMATION_INITIAL_HEIGHT = 190;

    /** Ancho inicial de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_FONT_INITIAL_WIDTH = 555;

    /** Alto inicial de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_FONT_INITIAL_HEIGHT = 250;

    /** Ancho de maximizado de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_MAX_WIDTH = 770;

    /** Alto de maximizado de los <code>AbstractJAccessibilityCustomDialog</code> de la aplicaci&oacute;n. */
    static final int CUSTOMDIALOG_MAX_HEIGHT = 299;

    /** Ancho de maximizado de los <code>AbstractJAccessibilityCustomDialog</code> de
     * confirmaci&oacute;n de firma de la aplicaci&oacute;n. */
    static final int CUSTOMCONFIRMATION_MAX_WIDTH = 700;

    /** Alto de maximizado de los <code>AbstractJAccessibilityCustomDialog</code> de
     * confirmaci&oacute;n de firma de la aplicaci&oacute;n. */
    static final int CUSTOMCONFIRMATION_MAX_HEIGHT = 305;

    /** Factor de redimensionado para las im&aacute;genes. */
    static final double RESIZING_IMAGES_FACTOR = 0.0015;

    /** Margen a restar para calcular el tama&ntilde;o de una ventana maximizada para Linux. */
    static final int MAXIMIZE_VERTICAL_MARGIN_LINUX = 52;

	 /** Variable que almacena el icono original del bot&oacute;n de maximizar. */
	static final ImageIcon IMAGEICON_MAXIMIZE = new ImageIcon(
		AccesiblityConstants.class.getResource("/images/maximize.png") //$NON-NLS-1$
	);

	 /** Icono original del bot&oacute;n de restaurar. */
	static final ImageIcon IMAGEICONRESTORE = new ImageIcon(
		AccesiblityConstants.class.getResource("/images/restore.png") //$NON-NLS-1$
	);

	/** C&oacute;digo HTML para el salto de l&iacute;nea. */
	static final String HTML_SALTO_LINEA = "<br>"; //$NON-NLS-1$
}

