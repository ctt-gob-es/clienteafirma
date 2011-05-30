package sun.security.mscapi;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.SecurityPermission;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.logging.Logger;

import sun.security.action.GetPropertyAction;

/**
 * <code>sun.security.mscapi.KeyStore</code> modificada para acceder a los
 * almacenes de CAPI <i>ADDRESSBOOK</i> y <i>CA</i>.
 * 
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public abstract class KeyStoreAddressBook extends KeyStoreSpi {

	/** KeyStore <i>CA</i> de CAPI. */
	public static final class CA extends KeyStoreAddressBook {

		/** Construye el <code>SPI</code> del KeyStore <i>CA</i> de CAPI. */
		public CA() {
			super("CA");
		}
	}

	/** KeyStore <i>ADDRESSBOOK</i> de CAPI. */
	public static final class ADDRESSBOOK extends KeyStoreAddressBook {
		/**
		 * Construye el <code>SPI</code> del KeyStore <i>ADDRESSBOOK</i> de
		 * CAPI.
		 */
		public ADDRESSBOOK() {
			super("ADDRESSBOOK");
		}
	}

	static class KeyEntry {
		// private Key privateKey;
		private X509Certificate certChain[];
		private String alias;

		KeyEntry(Key key, X509Certificate[] chain) {
			this(null, key, chain);
		}

		KeyEntry(String alias, Key key, X509Certificate[] chain) {

			// this.privateKey = key;
			this.certChain = chain;
			/*
			 * The default alias for both entry types is derived from a hash
			 * value intrinsic to the first certificate in the chain.
			 */
			if (alias == null) {
				this.alias = Integer.toString(chain[0].hashCode());
			} else {
				this.alias = alias;
			}
		}

		/**
		 * Gets the alias for the keystore entry.
		 */
		String getAlias() {
			return alias;
		}

		/**
		 * Sets the alias for the keystore entry.
		 */
		void setAlias(String alias) {
			this.alias = alias;
		}

		/** Gets the certificate chain for the keystore entry. */
		X509Certificate[] getCertificateChain() {
			return certChain;
		}

	};

	/*
	 * Compatibility mode: for applications that assume keystores are
	 * stream-based this mode tolerates (but ignores) a non-null stream or
	 * password parameter when passed to the load or store methods. The mode is
	 * enabled by default.
	 */
	private static final String KEYSTORE_COMPATIBILITY_MODE_PROP = "sun.security.mscapi.keyStoreCompatibilityMode";
	private final boolean keyStoreCompatibilityMode;

	/* The keystore entries. */
	private Collection<KeyEntry> entries = new ArrayList<KeyEntry>();

	/*
	 * The keystore name. Case is not significant.
	 */
	private final String storeName;

	private java.lang.reflect.Method loadKeysOrCertificateChains;
	private KeyStore.MY nativeWrapper;

	KeyStoreAddressBook(String storeName) {

		nativeWrapper = new KeyStore.MY();

		try {
			nativeWrapper.getClass();
			for (java.lang.reflect.Method m : nativeWrapper.getClass()
					.getDeclaredMethods()) {
				m.setAccessible(true);
			}
			for (java.lang.reflect.Method m : nativeWrapper.getClass()
					.getSuperclass().getDeclaredMethods()) {
				m.setAccessible(true);
				if (m.getName().equals("loadKeysOrCertificateChains"))
					loadKeysOrCertificateChains = m;
			}
		} catch (final Exception e) {
			Logger.getLogger("es.atosorigin").severe(
					"No se han podido obtener los metodos de acceso a sunmscapi.dll: "
							+ e);
		}

		// Get the compatibility mode
		String prop = (String) AccessController
				.doPrivileged(new GetPropertyAction(
						KEYSTORE_COMPATIBILITY_MODE_PROP));

		if ("false".equalsIgnoreCase(prop))
			keyStoreCompatibilityMode = false;
		else
			keyStoreCompatibilityMode = true;

		this.storeName = storeName;
	}

	/**
	 * Returns the key associated with the given alias.
	 * <p>
	 * A compatibility mode is supported for applications that assume a password
	 * must be supplied. It permits (but ignores) a non-null
	 * <code>password</code>. The mode is enabled by default. Set the
	 * <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system
	 * property to <code>false</code> to disable compatibility mode and reject a
	 * non-null <code>password</code>.
	 * 
	 * @param alias
	 *            the alias name
	 * @param password
	 *            the password, which should be <code>null</code>
	 * 
	 * @return the requested key, or null if the given alias does not exist or
	 *         does not identify a <i>key entry</i>.
	 * 
	 * @exception NoSuchAlgorithmException
	 *                if the algorithm for recovering the key cannot be found,
	 *                or if compatibility mode is disabled and
	 *                <code>password</code> is non-null.
	 * @exception UnrecoverableKeyException
	 *                if the key cannot be recovered.
	 */
	@Override
	public java.security.Key engineGetKey(String alias, char[] password)
			throws NoSuchAlgorithmException, UnrecoverableKeyException {
		throw new UnsupportedOperationException();
	}

	/**
	 * Returns the certificate chain associated with the given alias.
	 * 
	 * @param alias
	 *            the alias name
	 * 
	 * @return the certificate chain (ordered with the user's certificate first
	 *         and the root certificate authority last), or null if the given
	 *         alias does not exist or does not contain a certificate chain
	 *         (i.e., the given alias identifies either a <i>trusted certificate
	 *         entry</i> or a <i>key entry</i> without a certificate chain).
	 */
	@Override
	public Certificate[] engineGetCertificateChain(String alias) {
		if (alias == null)
			return null;

		for (KeyEntry entry : entries) {
			if (alias.equals(entry.getAlias())) {
				X509Certificate[] certChain = entry.getCertificateChain();
				return certChain.clone();
			}
		}

		return null;
	}

	/**
	 * Returns the certificate associated with the given alias.
	 * 
	 * <p>
	 * If the given alias name identifies a <i>trusted certificate entry</i>,
	 * the certificate associated with that entry is returned. If the given
	 * alias name identifies a <i>key entry</i>, the first element of the
	 * certificate chain of that entry is returned, or null if that entry does
	 * not have a certificate chain.
	 * 
	 * @param alias
	 *            the alias name
	 * 
	 * @return the certificate, or null if the given alias does not exist or
	 *         does not contain a certificate.
	 */
	@Override
	public Certificate engineGetCertificate(String alias) {
		if (alias == null)
			return null;
		java.lang.reflect.Method getAlias = null;
		java.lang.reflect.Method getCertificateChain = null;
		for (Object o : entries) {
			for (java.lang.reflect.Method m : o.getClass().getDeclaredMethods()) {
				if (m.getName().equals("getAlias")) {
					m.setAccessible(true);
					getAlias = m;
				} else if (m.getName().equals("getCertificateChain")) {
					m.setAccessible(true);
					getCertificateChain = m;
				}
				if (getAlias != null) {
					try {
						if (alias.equals(getAlias.invoke(o, new Object[0]))) {
							if (getCertificateChain != null) {
								X509Certificate[] certChain = (X509Certificate[]) getCertificateChain
										.invoke(o, new Object[0]);
								return certChain[0];
							}
						}
					} catch (final Exception e) {
					}
				}
			}
		}

		return null;
	}

	/**
	 * Returns the creation date of the entry identified by the given alias.
	 * 
	 * @param alias
	 *            the alias name
	 * 
	 * @return the creation date of this entry, or null if the given alias does
	 *         not exist
	 */
	@Override
	public Date engineGetCreationDate(String alias) {
		if (alias == null)
			return null;
		return new Date();
	}

	/**
	 * Stores the given private key and associated certificate chain in the
	 * keystore.
	 * 
	 * <p>
	 * The given java.security.PrivateKey <code>key</code> must be accompanied
	 * by a certificate chain certifying the corresponding public key.
	 * 
	 * <p>
	 * If the given alias already exists, the keystore information associated
	 * with it is overridden by the given key and certificate chain. Otherwise,
	 * a new entry is created.
	 * 
	 * <p>
	 * A compatibility mode is supported for applications that assume a password
	 * must be supplied. It permits (but ignores) a non-null
	 * <code>password</code>. The mode is enabled by default. Set the
	 * <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system
	 * property to <code>false</code> to disable compatibility mode and reject a
	 * non-null <code>password</code>.
	 * 
	 * @param alias
	 *            the alias name
	 * @param key
	 *            the private key to be associated with the alias
	 * @param password
	 *            the password, which should be <code>null</code>
	 * @param chain
	 *            the certificate chain for the corresponding public key (only
	 *            required if the given key is of type
	 *            <code>java.security.PrivateKey</code>).
	 * 
	 * @exception KeyStoreException
	 *                if the given key is not a private key, cannot be
	 *                protected, or if compatibility mode is disabled and
	 *                <code>password</code> is non-null, or if this operation
	 *                fails for some other reason.
	 */
	@Override
	public void engineSetKeyEntry(String alias, java.security.Key key,
			char[] password, Certificate[] chain) throws KeyStoreException {
		throw new UnsupportedOperationException();
	}

	/**
	 * Assigns the given key (that has already been protected) to the given
	 * alias.
	 * 
	 * <p>
	 * If the protected key is of type <code>java.security.PrivateKey</code>, it
	 * must be accompanied by a certificate chain certifying the corresponding
	 * public key. If the underlying keystore implementation is of type
	 * <code>jks</code>, <code>key</code> must be encoded as an
	 * <code>EncryptedPrivateKeyInfo</code> as defined in the PKCS #8 standard.
	 * 
	 * <p>
	 * If the given alias already exists, the keystore information associated
	 * with it is overridden by the given key (and possibly certificate chain).
	 * 
	 * @param alias
	 *            the alias name
	 * @param key
	 *            the key (in protected format) to be associated with the alias
	 * @param chain
	 *            the certificate chain for the corresponding public key (only
	 *            useful if the protected key is of type
	 *            <code>java.security.PrivateKey</code>).
	 * 
	 * @exception KeyStoreException
	 *                if this operation fails.
	 */
	@Override
	public void engineSetKeyEntry(String alias, byte[] key, Certificate[] chain)
			throws KeyStoreException {
		throw new UnsupportedOperationException(
				"Cannot assign the encoded key to the given alias.");
	}

	/**
	 * Assigns the given certificate to the given alias.
	 * 
	 * <p>
	 * If the given alias already exists in this keystore and identifies a
	 * <i>trusted certificate entry</i>, the certificate associated with it is
	 * overridden by the given certificate.
	 * 
	 * @param alias
	 *            the alias name
	 * @param cert
	 *            the certificate
	 * 
	 * @exception KeyStoreException
	 *                if the given alias already exists and does not identify a
	 *                <i>trusted certificate entry</i>, or this operation fails
	 *                for some other reason.
	 */
	@Override
	public void engineSetCertificateEntry(String alias, Certificate cert)
			throws KeyStoreException {
		throw new UnsupportedOperationException();
	}

	/**
	 * Deletes the entry identified by the given alias from this keystore.
	 * 
	 * @param alias
	 *            the alias name
	 * 
	 * @exception KeyStoreException
	 *                if the entry cannot be removed.
	 */
	@Override
	public void engineDeleteEntry(String alias) throws KeyStoreException {
		throw new UnsupportedOperationException();
	}

	/**
	 * Lists all the alias names of this keystore.
	 * 
	 * @return enumeration of the alias names
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Enumeration engineAliases() {

		final Iterator iter = entries.iterator();

		return new Enumeration() {
			public boolean hasMoreElements() {
				return iter.hasNext();
			}

			public Object nextElement() {
				Object o = iter.next();
				for (java.lang.reflect.Method m : o.getClass()
						.getDeclaredMethods()) {
					if (m.getName().equals("getAlias")) {
						m.setAccessible(true);
						try {
							return m.invoke(o, new Object[0]);
						} catch (Exception e) {
							Logger.getLogger("es.atosorigin").severe(
									"No se ha podido invocar a sunmscapi.dll para obtener los alias: "
											+ e);
							return null;
						}
					}
				}
				return null;
			}
		};
	}

	/**
	 * Checks if the given alias exists in this keystore.
	 * 
	 * @param alias
	 *            the alias name
	 * 
	 * @return true if the alias exists, false otherwise
	 */
	@Override
	public boolean engineContainsAlias(String alias) {
		for (Enumeration<?> enumerator = engineAliases(); enumerator
				.hasMoreElements();) {
			String a = (String) enumerator.nextElement();
			if (a.equals(alias))
				return true;
		}
		return false;
	}

	/**
	 * Retrieves the number of entries in this keystore.
	 * 
	 * @return the number of entries in this keystore
	 */
	@Override
	public int engineSize() {
		return entries.size();
	}

	/**
	 * Returns true if the entry identified by the given alias is a <i>key
	 * entry</i>, and false otherwise.
	 * 
	 * @return true if the entry identified by the given alias is a <i>key
	 *         entry</i>, false otherwise.
	 */
	@Override
	public boolean engineIsKeyEntry(String alias) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Returns true if the entry identified by the given alias is a <i>trusted
	 * certificate entry</i>, and false otherwise.
	 * 
	 * @return true if the entry identified by the given alias is a <i>trusted
	 *         certificate entry</i>, false otherwise.
	 */
	@Override
	public boolean engineIsCertificateEntry(String alias) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Returns the (alias) name of the first keystore entry whose certificate
	 * matches the given certificate.
	 * 
	 * <p>
	 * This method attempts to match the given certificate with each keystore
	 * entry. If the entry being considered is a <i>trusted certificate
	 * entry</i>, the given certificate is compared to that entry's certificate.
	 * If the entry being considered is a <i>key entry</i>, the given
	 * certificate is compared to the first element of that entry's certificate
	 * chain (if a chain exists).
	 * 
	 * @param cert
	 *            the certificate to match with.
	 * 
	 * @return the (alias) name of the first entry with matching certificate, or
	 *         null if no such entry exists in this keystore.
	 */
	@Override
	public String engineGetCertificateAlias(Certificate cert) {
		for (KeyEntry entry : entries) {
			if (entry.certChain != null && entry.certChain[0].equals(cert)) {
				return entry.getAlias();
			}
		}
		return null;
	}

	/**
	 * engineStore is currently a no-op. Entries are stored during
	 * engineSetEntry.
	 * 
	 * A compatibility mode is supported for applications that assume keystores
	 * are stream-based. It permits (but ignores) a non-null <code>stream</code>
	 * or <code>password</code>. The mode is enabled by default. Set the
	 * <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system
	 * property to <code>false</code> to disable compatibility mode and reject a
	 * non-null <code>stream</code> or <code>password</code>.
	 * 
	 * @param stream
	 *            the output stream, which should be <code>null</code>
	 * @param password
	 *            the password, which should be <code>null</code>
	 * 
	 * @exception IOException
	 *                if compatibility mode is disabled and either parameter is
	 *                non-null.
	 */
	@Override
	public void engineStore(OutputStream stream, char[] password)
			throws IOException, NoSuchAlgorithmException, CertificateException {
		if (stream != null && !keyStoreCompatibilityMode)
			throw new IOException("Keystore output stream must be null");
		if (password != null && !keyStoreCompatibilityMode)
			throw new IOException("Keystore password must be null");
	}

	/**
	 * Loads the keystore.
	 * 
	 * A compatibility mode is supported for applications that assume keystores
	 * are stream-based. It permits (but ignores) a non-null <code>stream</code>
	 * or <code>password</code>. The mode is enabled by default. Set the
	 * <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system
	 * property to <code>false</code> to disable compatibility mode and reject a
	 * non-null <code>stream</code> or <code>password</code>.
	 * 
	 * @param stream
	 *            the input stream, which should be <code>null</code>.
	 * @param password
	 *            the password, which should be <code>null</code>.
	 * 
	 * @exception IOException
	 *                if there is an I/O or format problem with the keystore
	 *                data. Or if compatibility mode is disabled and either
	 *                parameter is non-null.
	 * @exception NoSuchAlgorithmException
	 *                if the algorithm used to check the integrity of the
	 *                keystore cannot be found
	 * @exception CertificateException
	 *                if any of the certificates in the keystore could not be
	 *                loaded
	 * @exception SecurityException
	 *                if the security check for
	 *                <code>SecurityPermission("authProvider.<i>name</i>")</code>
	 *                does not pass, where <i>name</i> is the value returned by
	 *                this provider's <code>getName</code> method.
	 */
	@Override
	public void engineLoad(InputStream stream, char[] password)
			throws IOException, NoSuchAlgorithmException, CertificateException {
		if (stream != null && !keyStoreCompatibilityMode) {
			throw new IOException("Keystore input stream must be null");
		}

		if (password != null && !keyStoreCompatibilityMode) {
			throw new IOException("Keystore password must be null");
		}

		/*
		 * Use the same security check as AuthProvider.login
		 */
		SecurityManager sm = System.getSecurityManager();
		if (sm != null) {
			sm.checkPermission(new SecurityPermission("authProvider.SunMSCAPI"));
		}

		// Clear all key entries
		entries.clear();

		try {
			// Load keys and/or certificate chains
			loadKeysOrCertificateChains(getName(), entries);
		} catch (KeyStoreException kse) {
			// Wrap the JNI exception in an IOException
			throw new IOException(kse.toString());
		}
	}

	/**
	 * Returns the name of the keystore.
	 */
	private String getName() {
		return storeName;
	}

	/**
	 * Load keys and/or certificates from keystore into Collection.
	 * 
	 * @param name
	 *            Name of keystore.
	 * @param entries
	 *            Collection of key/certificate.
	 */
	private void loadKeysOrCertificateChains(final String name,
			final Collection<KeyEntry> ntries) throws KeyStoreException {
		try {
			loadKeysOrCertificateChains.invoke(nativeWrapper, name, ntries);
		} catch (final Exception e) {
			throw new KeyStoreException(e);
		}
	}

}
