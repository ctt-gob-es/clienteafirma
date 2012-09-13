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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.security.Key;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStore.ProtectionParameter;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.ProviderException;
import java.security.UnrecoverableEntryException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import es.gob.jmulticard.card.AuthenticationModeLockedException;
import es.gob.jmulticard.card.CryptoCard;
import es.gob.jmulticard.card.CryptoCardException;
import es.gob.jmulticard.card.PrivateKeyReference;
import es.gob.jmulticard.card.dnie.Dnie;
import es.gob.jmulticard.card.dnie.DniePrivateKeyReference;
import es.gob.jmulticard.jse.smartcardio.SmartcardIoConnection;

/** Implementaci&oacute;n del SPI KeyStore para DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DnieKeyStoreImpl extends KeyStoreSpi {

	private static final String INTERMEDIATE_CA_CERT_ALIAS = "CertCAIntermediaDGP"; //$NON-NLS-1$

    private static final List<String> USERS_CERTS_ALIASES = new ArrayList<String>(2);
    static {
        USERS_CERTS_ALIASES.add("CertAutenticacion"); //$NON-NLS-1$
        USERS_CERTS_ALIASES.add("CertFirmaDigital"); //$NON-NLS-1$
    }

    private CryptoCard cryptoCard = null;

    /** {@inheritDoc} */
    @Override
    public Enumeration<String> engineAliases() {
        return Collections.enumeration(USERS_CERTS_ALIASES);
    }

    /** {@inheritDoc} */
    @Override
    public boolean engineContainsAlias(final String alias) {
        return USERS_CERTS_ALIASES.contains(alias);
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public void engineDeleteEntry(final String alias) throws KeyStoreException {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public Certificate engineGetCertificate(final String alias) {
    	if (!engineContainsAlias(alias)) {
    		return null;
    	}
        try {
			return this.cryptoCard.getCertificate(alias);
		}
        catch (final CryptoCardException e) {
			throw new ProviderException(e);
		}
    }

    /** {@inheritDoc} */
    @Override
    public String engineGetCertificateAlias(final Certificate cert) {
        if (!(cert instanceof X509Certificate)) {
            return null;
        }
        final BigInteger serial = ((X509Certificate) cert).getSerialNumber();
        for (final String alias : USERS_CERTS_ALIASES) {
            if (((X509Certificate) engineGetCertificate(alias)).getSerialNumber() == serial) {
                return alias;
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Certificate[] engineGetCertificateChain(final String alias) {
    	if (!engineContainsAlias(alias)) {
    		return null;
    	}

    	// La cadena disponible del certificado la componen el propio certificado y el
    	// certificado de la CA intermedia. Si no se puede recuperar esta ultima, se obvia
    	X509Certificate intermediateCaCert;
    	try {
    		intermediateCaCert = this.cryptoCard.getCertificate(INTERMEDIATE_CA_CERT_ALIAS);
    	}
    	catch(final AuthenticationModeLockedException e) {
    		throw e;
    	}
    	catch (final Exception e) {
    		Logger.getLogger("es.gob.jmulticard").warning("No se ha podido cargar el certificado de la CA intermedia"); //$NON-NLS-1$ //$NON-NLS-2$
    		intermediateCaCert = null;
		}

    	if (intermediateCaCert == null) {
    		return new X509Certificate[] {
    				(X509Certificate) engineGetCertificate(alias)
    		};
    	}
    	return new X509Certificate[] {
    			(X509Certificate) engineGetCertificate(alias),
    			intermediateCaCert
    	};
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public Date engineGetCreationDate(final String alias) {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public Key engineGetKey(final String alias, final char[] password) throws NoSuchAlgorithmException, UnrecoverableKeyException {
    	if (password != null) {
    		Logger.getLogger("es.gob.jmulticard").warning("Se ha proporcionado una contrasena, pero esta se ignorara, ya que el PIN se gestiona internamente"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	if (!engineContainsAlias(alias)) {
    		return null;
    	}
        try {
        	final PrivateKeyReference pkRef = this.cryptoCard.getPrivateKey(alias);
        	if (!(pkRef instanceof DniePrivateKeyReference)) {
        		throw new ProviderException("La clave obtenida de la tarjeta no es del tipo esperado, se ha obtenido: " + pkRef.getClass().getName()); //$NON-NLS-1$
        	}
        	return new DniePrivateKey((DniePrivateKeyReference) pkRef);
		}
        catch (final CryptoCardException e) {
			throw new ProviderException(e);
		}
    }

    /** {@inheritDoc} */
    @Override
    public KeyStore.Entry engineGetEntry(final String alias,
    		                             final ProtectionParameter protParam) throws KeyStoreException,
    		                                                                                  NoSuchAlgorithmException,
    		                                                                                  UnrecoverableEntryException {
    	if (protParam != null) {
    		Logger.getLogger("es.gob.jmulticard").warning("Se ha proporcionado un ProtectionParameter, pero este se ignorara, ya que el PIN se gestiona internamente"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	if (!engineContainsAlias(alias)) {
    		return null;
    	}
    	final PrivateKey key = (PrivateKey) engineGetKey(alias, null);
    	return new PrivateKeyEntry(key, engineGetCertificateChain(alias));
    }

    /** {@inheritDoc} */
    @Override
    public boolean engineIsCertificateEntry(final String alias) {
        return USERS_CERTS_ALIASES.contains(alias);
    }

    /** {@inheritDoc} */
    @Override
    public boolean engineIsKeyEntry(final String alias) {
        return USERS_CERTS_ALIASES.contains(alias);
    }

    /** {@inheritDoc} */
    @Override
    public void engineLoad(final KeyStore.LoadStoreParameter param) throws IOException, NoSuchAlgorithmException, CertificateException {
    	if (param != null) {
       		throw new IllegalArgumentException("El LoadStoreParameter siempre debe ser null, la contrasena se gestiona internamente"); //$NON-NLS-1$
    	}
    	this.cryptoCard = new Dnie(new SmartcardIoConnection(), null, new JseCryptoHelper());
    }

    /** {@inheritDoc} */
    @Override
    public void engineLoad(final InputStream stream, final char[] password) throws IOException, NoSuchAlgorithmException, CertificateException {
    	if (password != null) {
    		throw new IllegalArgumentException("La contrasena siempre debe ser null, esta se gestiona internamente"); //$NON-NLS-1$
    	}
        // Aqui se realiza el acceso e inicializacion del DNIe
        this.cryptoCard = new Dnie(new SmartcardIoConnection(), null, new JseCryptoHelper());
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public void engineSetCertificateEntry(final String alias, final Certificate cert) throws KeyStoreException {
        throw new UnsupportedOperationException();
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public void engineSetKeyEntry(final String alias, final byte[] key, final Certificate[] chain) throws KeyStoreException {
        throw new UnsupportedOperationException();
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public void engineSetKeyEntry(final String alias, final Key key, final char[] pass, final Certificate[] chain) throws KeyStoreException {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public int engineSize() {
        return USERS_CERTS_ALIASES.size();
    }

    /** Operaci&oacute;n no soportada. */
    @Override
    public void engineStore(final OutputStream os, final char[] pass) throws IOException, NoSuchAlgorithmException, CertificateException {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public boolean engineEntryInstanceOf(final String alias, final Class<? extends KeyStore.Entry> entryClass) {
        if (!engineContainsAlias(alias)) {
            return false;
        }
        return (entryClass.equals(PrivateKeyEntry.class));
    }
}