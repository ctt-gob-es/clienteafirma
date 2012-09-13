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
package es.gob.jmulticard.card.iso7816four;

import java.util.Hashtable;

import es.gob.jmulticard.apdu.StatusWord;

/** Excepci&oacute;n gen&eacute;rica en tarjetas ISO 7816-4.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class Iso7816FourCardException extends Exception {

    private static final Hashtable ERRORS = new Hashtable();

    static {
        ERRORS.put(new StatusWord((byte) 0x62, (byte) 0x83), "El fichero seleccionado esta invalidado"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x65, (byte) 0x81), "Fallo en la memoria"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x67, (byte) 0x00), "Longitud incorrecta"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x68, (byte) 0x82), "Securizacion de mensajes no soportada"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x69, (byte) 0x82), "Condiciones de seguridad no satisfechas"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x69, (byte) 0x83), "Metodo de autenticacion bloqueado"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x69, (byte) 0x84), "Dato referenciado invalido"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x69, (byte) 0x85), "Condiciones de uso no satisfechas"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x69, (byte) 0x86), "Comando no permitido (no existe ningun EF seleccionado)"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x80), "Parametros incorrectos en el campo de datos"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x81), "Funcion no soportada."); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x82), "No se encuentra el fichero"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x83), "Registro no encontrado"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x84), "No hay suficiente espacio de memoria en el fichero"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x85), "La longitud de datos (Lc) es incompatible con la estructura TLV"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x86), "parametros incorrectos en P1 P2"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x87), "La longitud de los datos es inconsistente con P1-P2"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x88), "Datos referenciados no encontrados"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x89), "El fichero ya existe"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6A, (byte) 0x8A), "El nombre del DF ya existe"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6B, (byte) 0x00), "Parametro(s) incorrecto(s) P1-P2"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6E, (byte) 0x00), "Clase no soportada"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6D, (byte) 0x00), "Comando no permitido en la fase de vida actual"); //$NON-NLS-1$
        ERRORS.put(new StatusWord((byte) 0x6F, (byte) 0x00), "Diagnostico no preciso"); //$NON-NLS-1$
    }

    private final StatusWord returnCode;

    Iso7816FourCardException(final String desc, final StatusWord retCode) {
        super(desc);
        this.returnCode = retCode;
    }

    Iso7816FourCardException(final StatusWord retCode) {
        super((String) ERRORS.get(retCode));
        this.returnCode = retCode;
    }

    private static final long serialVersionUID = 5935577997660561619L;

    /** Obtiene el c&oacute;digo de finalizaci&oacute;n (en modo de palabra de estado) que caus&oacute; la
     * excepci&oacute;n.
     * @return C&oacute;digo de finalizaci&oacute;n que caus&oacute; la excepci&oacute;n */
    public StatusWord getStatusWord() {
        return this.returnCode;
    }
}
