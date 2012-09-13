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
package es.gob.jmulticard.jse.provider;

import java.io.IOException;
import java.io.NotSerializableException;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.security.interfaces.RSAPrivateKey;

import es.gob.jmulticard.card.CryptoCard;
import es.gob.jmulticard.card.Location;
import es.gob.jmulticard.card.dnie.Dnie;
import es.gob.jmulticard.card.dnie.DniePrivateKeyReference;

/** Clave privada de un DNIe. La clase no contiene la clave privada en si, sino una referencia a ella
 * y una referencia al propio DNIe, con el canal seguro establecido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DniePrivateKey implements RSAPrivateKey {

    private static final long serialVersionUID = 4403051294889801855L;

    private final Dnie dnie;

    /** Identificador de la clave. */
    private final String id;

    /** Ruta de la clave. */
    private final Location path;

    /** Etiqueta de la clave. */
    private final String name;

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return this.name;
    }

    /** Crea una clave privada de DNIe.
     * @param keyReference Referencia a la clave privada del DNIe. */
    DniePrivateKey(final DniePrivateKeyReference keyReference) {
    	this.dnie = keyReference.getDnieCard();
        this.id = keyReference.getIdentifier();
        this.path = keyReference.getKeyPath();
        this.name = keyReference.getLabel();
    }

    /** Obtiene la tarjeta capaz de operar con esta clave.
     * @return Tarjeta capaz de operar con esta clave */
    CryptoCard getCryptoCard() {
        return this.dnie;
    }

    /** {@inheritDoc} */
    @Override
    public String getAlgorithm() {
        return "RSA"; //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public byte[] getEncoded() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getFormat() {
        return null;
    }

    /** M&eacute;todo no soportado. */
    @Override
    public BigInteger getModulus() {
        throw new UnsupportedOperationException();
    }

    /** M&eacute;todo no soportado. */
    @Override
    public BigInteger getPrivateExponent() {
        throw new UnsupportedOperationException();
    }

    /** Recupera el identificador de la clave.
     * @return Identificador de la clave */
    String getId() {
        return this.id;
    }

    /** Recupera la ruta de la clave.
     * @return Ruta de la clave */
    Location getPath() {
        return this.path;
    }

    @SuppressWarnings("static-method")
    private void writeObject(@SuppressWarnings("unused") final ObjectOutputStream out) throws IOException {
        throw new NotSerializableException();
    }
}