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

/** Configuraci&oacute;n global de la aplicaci&oacute;n. */
final class GeneralConfig {

    /** Indica si el ususario ha activado o desactivado la opci&oacute;n de tama&ntilde;o
     * de fuente grande.
     * @return <code>true</code> si el ususario ha activado la opci&oacute;n de
     *         tama&ntilde;o de fuente grande. */
    static boolean isBigFontSize() {
        return false;
    }

    /** Indica si el ususario ha activado o desactivado la opcion de fuente en negrita
     * @return boolean Indicando el estado de la opcion */
    static boolean isFontBold() {
        return false;
    }

    /** Indica si el ususario ha activado o desactivado la opcion de alto contraste
     * @return boolean Indicando el estado de la opcion */
    static boolean isHighContrast() {
        return false;
    }

    /** Indica si el ususario ha activado o desactivado la opcion de maximizar todas las ventanas
     * @return boolean Indicando el estado de la opcion */
    static boolean isMaximized() {
        return false;
    }

    /** Indica si el ususario ha activado o desactivado la opcion de remarcar elementos con foco
     * @return boolean Indicando el estado de la opcion */
    static boolean isRemarked() {
        return false;
    }

    private GeneralConfig() {
        // No permitimos la instanciacion
    }
}
