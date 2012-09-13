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

import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.StatusWord;

/** Excepci&oacute;n que representa un fichero no encontrado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class FileNotFoundException extends Iso7816FourCardException {

    private static final long serialVersionUID = -1114043381519603316L;

    private final byte[] id;

    private static final StatusWord FILE_NOT_FOUND_RETURN_CODE = new StatusWord((byte) 0x6A, (byte) 0x82);

    /** Construye una excepci&oacute;n de fichero no encontrado. */
    public FileNotFoundException() {
        super("Fichero no encontrado", //$NON-NLS-1$
              FileNotFoundException.FILE_NOT_FOUND_RETURN_CODE);
        this.id = null;
    }

    /** Construye una excepci&oacute;n de fichero no encontrado.
     * @param fileId Identificador del fichero no encontrado */
    public FileNotFoundException(final byte[] fileId) {
        super("Fichero no encontrado: " + //$NON-NLS-1$
              HexUtils.hexify(fileId, false),
              FileNotFoundException.FILE_NOT_FOUND_RETURN_CODE);
        this.id = new byte[fileId.length];
        System.arraycopy(fileId, 0, this.id, 0, fileId.length);
    }
    
    /** Construye una excepci&oacute;n de fichero no encontrado.
     * @param filename Nombre del fichero no encontrado */
    public FileNotFoundException(final String filename) {
        super(
    		"Fichero no encontrado: " + //$NON-NLS-1$
            filename,
            FileNotFoundException.FILE_NOT_FOUND_RETURN_CODE
        );
        this.id = filename.getBytes();
    }

    /** Obtiene el identificador del fichero no encontrado.
     * @return Identificador del fichero no encontrado */
    public byte[] getFileId() {
        final byte[] out = new byte[this.id.length];
        System.arraycopy(this.id, 0, out, 0, this.id.length);
        return out;
    }
}
