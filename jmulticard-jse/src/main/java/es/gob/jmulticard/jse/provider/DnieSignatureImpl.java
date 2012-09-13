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
package es.gob.jmulticard.jse.provider;

import java.io.ByteArrayOutputStream;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.ProviderException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.SignatureSpi;

import es.gob.jmulticard.card.CryptoCardException;
import es.gob.jmulticard.card.dnie.Dnie;
import es.gob.jmulticard.card.dnie.DniePrivateKeyReference;

/** Implementaci&oacute;n del SPI Signature para DNIe.
 * Realiza firmas RSA con relleno PKCS#1 v1.5. Se soportan los siguientes algoritmos de firma:
 * <ul>
 *  <li>SHA1withRSA</li>
 *  <li>SHA256withRSA</li>
 *  <li>SHA384withRSA</li>
 *  <li>SHA512withRSA</li>
 * </ul>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
abstract class DnieSignatureImpl extends SignatureSpi {

    private final ByteArrayOutputStream data = new ByteArrayOutputStream();

    private Signature signatureVerifier = null;

    private DniePrivateKey privateKey = null;

    private final String signatureAlgo;

    DnieSignatureImpl(final String signatureAlgorithm) {
        super();
        this.signatureAlgo = signatureAlgorithm;
    }

    /** {@inheritDoc} */
    @Override
    protected Object engineGetParameter(final String param) {
        throw new InvalidParameterException("Parametro no soportado"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    protected void engineInitSign(final PrivateKey prKey) throws InvalidKeyException {
        if (prKey == null) {
            throw new InvalidKeyException("La clave proporcionada es nula"); //$NON-NLS-1$
        }
        if (!(prKey instanceof DniePrivateKey)) {
            throw new InvalidKeyException("La clave proporcionada no es de un DNIe: " + prKey.getClass().getName()); //$NON-NLS-1$
        }
        this.privateKey = (DniePrivateKey) prKey;
        this.data.reset();
    }

    /** {@inheritDoc} */
    @Override
    protected void engineInitVerify(final PublicKey publicKey) throws InvalidKeyException {
        this.data.reset();
        try {
        	this.signatureVerifier = Signature.getInstance(this.signatureAlgo);
            try {
            	if (this.signatureVerifier.getProvider() instanceof DnieProvider) {
            		this.signatureVerifier = Signature.getInstance(this.signatureAlgo, "SunRsaSign"); //$NON-NLS-1$
            	}
            }
            catch (final NoSuchProviderException e) {
                throw new IllegalStateException(
            		"No esta instalado el proveedor SunRsaSign", e //$NON-NLS-1$
                );
            }
        }
        catch (final NoSuchAlgorithmException e) {
            throw new IllegalStateException(
                "No existe un proveedor para validar firmas con el algoritmo " + this.signatureAlgo, e //$NON-NLS-1$
            );
        }
        this.signatureVerifier.initVerify(publicKey);
    }

    /** {@inheritDoc} */
    @Override
    protected void engineSetParameter(final String param, final Object value) {
        throw new InvalidParameterException("Parametro no soportado"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    protected byte[] engineSign() throws SignatureException {

    	if (!(this.privateKey.getCryptoCard() instanceof Dnie)) {
    		throw new ProviderException("La clave proporcionada no se corresponde con la de un DNIe"); //$NON-NLS-1$
    	}

    	final DniePrivateKeyReference dniePkRef = new DniePrivateKeyReference(
    			(Dnie) this.privateKey.getCryptoCard(), this.privateKey.getId(),
    			this.privateKey.getPath(), this.privateKey.toString());

    	try {
            return this.privateKey.getCryptoCard().sign(
            		this.data.toByteArray(), this.signatureAlgo, dniePkRef);
        }
        catch (final CryptoCardException e) {
            throw new SignatureException(e);
        }
    }

    /** {@inheritDoc} */
    @Override
    protected void engineUpdate(final byte b) throws SignatureException {
        this.data.write(b);
    }

    /** {@inheritDoc} */
    @Override
    protected void engineUpdate(final byte[] b, final int off, final int len) throws SignatureException {
        this.data.write(b, off, len);
    }

    /** {@inheritDoc} */
    @Override
    protected boolean engineVerify(final byte[] sigBytes) throws SignatureException {
        if (this.signatureVerifier == null) {
            throw new SignatureException("La verificacion no esta inicializada"); //$NON-NLS-1$
        }
        this.signatureVerifier.update(this.data.toByteArray());
        this.data.reset();
        return this.signatureVerifier.verify(sigBytes);
    }

    /** Firma SHA1withRSA. */
    public static final class Sha1 extends DnieSignatureImpl {
        /** Constructor */
        public Sha1() {
            super("SHA1withRSA"); //$NON-NLS-1$
        }
    }

    /** Firma SHA256withRSA. */
    public static final class Sha256 extends DnieSignatureImpl {
        /** Constructor */
        public Sha256() {
            super("SHA256withRSA"); //$NON-NLS-1$
        }
    }

    /** Firma SHA384withRSA. */
    public static final class Sha384 extends DnieSignatureImpl {
        /** Constructor */
        public Sha384() {
            super("SHA384withRSA"); //$NON-NLS-1$
        }
    }

    /** Firma SHA512withRSA. */
    public static final class Sha512 extends DnieSignatureImpl {
        /** Constructor */
        public Sha512() {
            super("SHA512withRSA"); //$NON-NLS-1$
        }
    }
}