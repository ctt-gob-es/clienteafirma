package es.gob.afirma.keystores.dnie;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.x500.X500Principal;

import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de DNIe mediante controlador
 * 100% Java m&aacute;s un segundo almac&eacute;n en el que los certificados de ambos se tratan de forma unificada
 * y homogenea.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class DnieUnifiedKeyStoreManager extends AOKeyStoreManager {

	private static X509Certificate dnieRootCertificate;

	static {
		try {
			dnieRootCertificate = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(ClassLoader.getSystemResourceAsStream("ACRAIZ-SHA2.crt")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final CertificateException e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No se ha podido cargal el certificado raiz del DNIe, la cadena de confianza puede estar incompleta: " + e //$NON-NLS-1$
			);
			dnieRootCertificate = null;
		}
	}

	/** Longitud de la cadena de confianza del DNIe. */
	private static final int DNIE_CERTCHAIN_LENGTH = 2;

	private static final List<String> DNIE_ALIASES = new ArrayList<String>(2);

	private final String[] aliases;

	static {
		DNIE_ALIASES.add("CertAutenticacion"); //$NON-NLS-1$
		DNIE_ALIASES.add("CertFirmaDigital"); //$NON-NLS-1$
	}

	private static final X500Principal DNIE_ISSUER = new X500Principal("CN=AC DNIE 001, OU=DNIE, O=DIRECCION GENERAL DE LA POLICIA, C=ES"); //$NON-NLS-1$

	private final AOKeyStoreManager originalKsm;
	private AOKeyStoreManager dnieKsm = null;

	/** Crea un almc&eacute;n de claves en base a un agregado del DNIe con controlador 100% Java y un segundo almac&eacute;n
	 * indicado como par&aacute;metro.
	 * @param originalKsm ALmac&eacute;n de claves original
	 * @param parent Componente padre para la modalidad */
	public DnieUnifiedKeyStoreManager(final AOKeyStoreManager originalKsm, final Object parent) {
		if (originalKsm == null) {
			throw new IllegalArgumentException("Es necesario un almacen al que anadir los certificados de DNIe, no puede ser nulo"); //$NON-NLS-1$
		}

		this.originalKsm = originalKsm;

		boolean dnieNeeded = true;

		for (final String alias : originalKsm.getAliases()) {
			if (originalKsm.getCertificate(alias).getIssuerX500Principal().equals(DNIE_ISSUER)) {
				dnieNeeded = false;
				break;
			}
		}
		if (dnieNeeded) {
			try {
				this.dnieKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.DNIEJAVA,
					null, // Lib
					originalKsm.getType() + "_PLUS_DNIE", // Description //$NON-NLS-1$
					null, // PasswordCallback
					parent
				);
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").info("No se puede usar DNIe con controlador 100% Java: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		// Unificamos los alias
		final String[] originalAliases = originalKsm.getAliases();
		this.aliases = new String[originalAliases.length + ((this.dnieKsm != null) ? 2 : 0)];
		System.arraycopy(originalAliases, 0, this.aliases, 0, originalAliases.length);
		if (this.dnieKsm != null) {
			this.aliases[this.aliases.length-1] = DNIE_ALIASES.get(0);
			this.aliases[this.aliases.length-2] = DNIE_ALIASES.get(1);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String[] getAliases() {
		return this.aliases;
	}

	/** {@inheritDoc} */
	@Override
	public X509Certificate getCertificate(final String alias) {
		if (!DNIE_ALIASES.contains(alias) || this.dnieKsm == null) {
			return this.originalKsm.getCertificate(alias);
		}
		return this.dnieKsm.getCertificate(alias);
	}

	/** {@inheritDoc} */
	@Override
	public X509Certificate[] getCertificateChain(final String alias) {
		if (!DNIE_ALIASES.contains(alias) || this.dnieKsm == null) {
			return this.originalKsm.getCertificateChain(alias);
		}
		final X509Certificate[] chain = new X509Certificate[DNIE_CERTCHAIN_LENGTH];
		final X509Certificate[] originalDnieChain = this.dnieKsm.getCertificateChain(alias);
		chain[0] = originalDnieChain[0];
		chain[1] = originalDnieChain[1];
		chain[2] = dnieRootCertificate;
		return chain;
	}

	/** {@inheritDoc} */
	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
                                                final PasswordCallback pssCallback) throws KeyStoreException,
                                                       									   NoSuchAlgorithmException,
                                                       									   UnrecoverableEntryException {
		if (!DNIE_ALIASES.contains(alias) || this.dnieKsm == null) {
			return this.originalKsm.getKeyEntry(alias, pssCallback);
		}
		return new PrivateKeyEntry(this.dnieKsm.getKeyEntry(alias, null).getPrivateKey(), this.getCertificateChain(alias));
	}

	/** {@inheritDoc} */
    @Override
	public List<KeyStore> getKeyStores() {
    	if (this.dnieKsm == null) {
    		return this.originalKsm.getKeyStores();
    	}
    	final List<KeyStore> ksms = new ArrayList<KeyStore>(this.originalKsm.getKeyStores().size() + 1);
    	ksms.addAll(this.originalKsm.getKeyStores());
    	ksms.addAll(this.dnieKsm.getKeyStores());
    	return ksms;
    }

    /** {@inheritDoc} */
    @Override
	public AOKeyStore getType() {
        return this.originalKsm.getType();
    }

    /** {@inheritDoc} */
    @Override
    public List<KeyStore> init(final AOKeyStore type,
            final InputStream store,
            final PasswordCallback pssCallBack,
            final Object[] params) throws AOKeyStoreManagerException,
                                          IOException {
    	throw new UnsupportedOperationException();
    }
}