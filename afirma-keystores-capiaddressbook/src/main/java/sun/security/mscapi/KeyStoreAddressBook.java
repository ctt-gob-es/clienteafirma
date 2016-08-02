/*
 * Copyright (c) 2005, 2006, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package sun.security.mscapi;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.SecurityPermission;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.logging.Logger;

/** <code>sun.security.mscapi.KeyStore</code> modificada para acceder a los
 * almacenes de CAPI <i>ADDRESSBOOK</i> y <i>CA</i>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class KeyStoreAddressBook extends KeyStoreSpi {

    /** KeyStore <i>CA</i> de CAPI. */
    public static final class CA extends KeyStoreAddressBook {

        /** Construye el <code>SPI</code> del KeyStore <i>CA</i> de CAPI. */
        public CA() {
            super("CA"); //$NON-NLS-1$
        }
    }

    /** KeyStore <i>ADDRESSBOOK</i> de CAPI. */
    public static final class ADDRESSBOOK extends KeyStoreAddressBook {
        /** Construye el <code>SPI</code> del KeyStore <i>ADDRESSBOOK</i> de
         * CAPI. */
        public ADDRESSBOOK() {
            super("ADDRESSBOOK"); //$NON-NLS-1$
        }
    }

    final class KeyEntry {

        private final X509Certificate certChain[];
        private String alias;

        X509Certificate[] getCertChain() {
            return this.certChain.clone();
        }

        KeyEntry(final X509Certificate[] chain) {
            this((String) null, chain);
        }

        KeyEntry(final String alias, final X509Certificate[] chain) {

            this.certChain = chain.clone();
            /*
             * The default alias for both entry types is derived from a hash
             * value intrinsic to the first certificate in the chain.
             */
            if (alias == null) {
                this.alias = Integer.toString(this.certChain[0].hashCode());
            }
            else {
                this.alias = alias;
            }
        }

        /** Gets the alias for the keystore entry.
         * @return Alias para la entrada de almac&eacute;n. */
        String getAlias() {
            return this.alias;
        }

        /** Gets the certificate chain for the keystore entry.
         * @return cadena de certificados para la entrada de almac&eacute;n. */
        X509Certificate[] getCertificateChain() {
            return this.certChain;
        }

    }

    /** The keystore entries. */
    private final Collection<KeyEntry> entries = new ArrayList<KeyEntry>();

    /** The keystore name. Case is not significant. */
    private final String storeName;

    private java.lang.reflect.Method loadKeysOrCertificateChains;

    private final KeyStore.MY nativeWrapper;

    KeyStoreAddressBook(final String storeName) {

        this.nativeWrapper = new KeyStore.MY();

        try {
            this.nativeWrapper.getClass();
            for (final java.lang.reflect.Method m : this.nativeWrapper.getClass().getDeclaredMethods()) {
                m.setAccessible(true);
            }
            for (final java.lang.reflect.Method m : this.nativeWrapper.getClass().getSuperclass().getDeclaredMethods()) {
                m.setAccessible(true);
                if (m.getName().equals("loadKeysOrCertificateChains")) { //$NON-NLS-1$
                    this.loadKeysOrCertificateChains = m;
                }
            }
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se han podido obtener los metodos de acceso a sunmscapi.dll: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        this.storeName = storeName;
    }

    /** Returns the key associated with the given alias.
     * <p>
     * A compatibility mode is supported for applications that assume a password must be supplied. It permits (but ignores) a non-null
     * <code>password</code>. The mode is enabled by default. Set the <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system property to
     * <code>false</code> to disable compatibility mode and reject a non-null <code>password</code>.
     * @param alias
     *        the alias name
     * @param password
     *        the password, which should be <code>null</code>
     * @return the requested key, or null if the given alias does not exist or
     *         does not identify a <i>key entry</i>. */
    @Override
    public final java.security.Key engineGetKey(final String alias, final char[] password) {
        throw new UnsupportedOperationException();
    }

    /** Returns the certificate chain associated with the given alias.
     * @param alias
     *        the alias name
     * @return the certificate chain (ordered with the user's certificate first
     *         and the root certificate authority last), or null if the given
     *         alias does not exist or does not contain a certificate chain
     *         (i.e., the given alias identifies either a <i>trusted certificate
     *         entry</i> or a <i>key entry</i> without a certificate chain). */
    @Override
    public final Certificate[] engineGetCertificateChain(final String alias) {
        if (alias == null) {
            return null;
        }

        // Se usan los KeyEntry por reflexion porque se han detectado casos en los que son del
        // tipo del almacen de windows en lugar de la libreta de direcciones
        try {
        	for (final Object entry : this.entries.toArray()) {
        		final Method getAliasMethod = entry.getClass().getDeclaredMethod("getAlias"); //$NON-NLS-1$
        		getAliasMethod.setAccessible(true);
        		if (alias.equals(getAliasMethod.invoke(entry))) {
        			final Method getCertificateChainMethod = entry.getClass().getDeclaredMethod("getCertificateChain"); //$NON-NLS-1$
        			getCertificateChainMethod.setAccessible(true);
        			return (Certificate[]) getCertificateChainMethod.invoke(entry);
        		}
        	}
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").warning("Error tratando de obtener la cadena de certificacion: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return null;
    }

    /** Returns the certificate associated with the given alias.
     * <p>
     * If the given alias name identifies a <i>trusted certificate entry</i>, the certificate associated with that entry is returned. If the given
     * alias name identifies a <i>key entry</i>, the first element of the certificate chain of that entry is returned, or null if that entry does not
     * have a certificate chain.
     * @param alias
     *        the alias name
     * @return the certificate, or null if the given alias does not exist or
     *         does not contain a certificate. */
    @Override
    public final Certificate engineGetCertificate(final String alias) {
        if (alias == null) {
            return null;
        }
        java.lang.reflect.Method getAlias = null;
        java.lang.reflect.Method getCertificateChain = null;
        for (final Object o : this.entries) {
            for (final java.lang.reflect.Method m : o.getClass().getDeclaredMethods()) {
                if (m.getName().equals("getAlias")) { //$NON-NLS-1$
                    m.setAccessible(true);
                    getAlias = m;
                }
                else if (m.getName().equals("getCertificateChain")) { //$NON-NLS-1$
                    m.setAccessible(true);
                    getCertificateChain = m;
                }
                if (getAlias != null) {
                    try {
                        if (alias.equals(getAlias.invoke(o, new Object[0])) && getCertificateChain != null) {
                            final X509Certificate[] certChain = (X509Certificate[]) getCertificateChain.invoke(o, new Object[0]);
                            return certChain[0];
                        }
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").warning("Error obteniendo el certificado para el alias '" + alias + "', se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }
                }
            }
        }

        return null;
    }

    /** Returns the creation date of the entry identified by the given alias.
     * @param alias
     *        the alias name
     * @return the creation date of this entry, or null if the given alias does
     *         not exist */
    @Override
    public final Date engineGetCreationDate(final String alias) {
        if (alias == null) {
            return null;
        }
        return new Date();
    }

    /** Stores the given private key and associated certificate chain in the
     * keystore.
     * <p>
     * The given java.security.PrivateKey <code>key</code> must be accompanied by a certificate chain certifying the corresponding public key.
     * <p>
     * If the given alias already exists, the keystore information associated with it is overridden by the given key and certificate chain. Otherwise,
     * a new entry is created.
     * <p>
     * A compatibility mode is supported for applications that assume a password must be supplied. It permits (but ignores) a non-null
     * <code>password</code>. The mode is enabled by default. Set the <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system property to
     * <code>false</code> to disable compatibility mode and reject a non-null <code>password</code>.
     * @param alias
     *        the alias name
     * @param key
     *        the private key to be associated with the alias
     * @param password
     *        the password, which should be <code>null</code>
     * @param chain
     *        the certificate chain for the corresponding public key (only
     *        required if the given key is of type <code>java.security.PrivateKey</code>). */
    @Override
    public final void engineSetKeyEntry(final String alias, final java.security.Key key, final char[] password, final Certificate[] chain) {
        throw new UnsupportedOperationException();
    }

    /** Assigns the given key (that has already been protected) to the given
     * alias.
     * <p>
     * If the protected key is of type <code>java.security.PrivateKey</code>, it must be accompanied by a certificate chain certifying the
     * corresponding public key. If the underlying keystore implementation is of type <code>jks</code>, <code>key</code> must be encoded as an
     * <code>EncryptedPrivateKeyInfo</code> as defined in the PKCS #8 standard.
     * <p>
     * If the given alias already exists, the keystore information associated with it is overridden by the given key (and possibly certificate chain).
     * @param alias
     *        the alias name
     * @param key
     *        the key (in protected format) to be associated with the alias
     * @param chain
     *        the certificate chain for the corresponding public key (only
     *        useful if the protected key is of type <code>java.security.PrivateKey</code>). */
    @Override
    public final void engineSetKeyEntry(final String alias, final byte[] key, final Certificate[] chain) {
        throw new UnsupportedOperationException("Cannot assign the encoded key to the given alias."); //$NON-NLS-1$
    }

    /** Assigns the given certificate to the given alias.
     * <p>
     * If the given alias already exists in this keystore and identifies a <i>trusted certificate entry</i>, the certificate associated with it is
     * overridden by the given certificate.
     * @param alias
     *        the alias name
     * @param cert
     *        the certificate. */
    @Override
    public final void engineSetCertificateEntry(final String alias, final Certificate cert) {
        throw new UnsupportedOperationException();
    }

    /** Deletes the entry identified by the given alias from this keystore.
     * @param alias
     *        the alias name. */
    @Override
    public final void engineDeleteEntry(final String alias) {
        throw new UnsupportedOperationException();
    }

    /** Lists all the alias names of this keystore.
     * @return enumeration of the alias names */
    @Override
    public final Enumeration<String> engineAliases() {

        final Iterator<KeyEntry> iter = this.entries.iterator();

        return new Enumeration<String>() {

        	/** {@inheritDoc} */
            @Override
			public boolean hasMoreElements() {
                return iter.hasNext();
            }

            /** {@inheritDoc} */
            @Override
			public String nextElement() {
                final Object o = iter.next();
                for (final java.lang.reflect.Method m : o.getClass().getDeclaredMethods()) {
                    if (m.getName().equals("getAlias")) { //$NON-NLS-1$
                        m.setAccessible(true);
                        try {
                            return m.invoke(o, new Object[0]).toString();
                        }
                        catch (final Exception e) {
                            Logger.getLogger("es.gob.afirma").severe("No se ha podido invocar a sunmscapi.dll para obtener los alias: " + e); //$NON-NLS-1$//$NON-NLS-2$
                            return null;
                        }
                    }
                }
                return null;
            }
        };
    }

    /** Checks if the given alias exists in this keystore.
     * @param alias
     *        the alias name
     * @return true if the alias exists, false otherwise */
    @Override
    public final boolean engineContainsAlias(final String alias) {
        for (final Enumeration<?> enumerator = engineAliases(); enumerator.hasMoreElements();) {
            final String a = (String) enumerator.nextElement();
            if (a.equals(alias)) {
                return true;
            }
        }
        return false;
    }

    /** Retrieves the number of entries in this keystore.
     * @return the number of entries in this keystore */
    @Override
    public final int engineSize() {
        return this.entries.size();
    }

    /** Returns true if the entry identified by the given alias is a <i>key
     * entry</i>, and false otherwise.
     * @return true if the entry identified by the given alias is a <i>key
     *         entry</i>, false otherwise. */
    @Override
    public final boolean engineIsKeyEntry(final String alias) {
        throw new UnsupportedOperationException();
    }

    /** Returns true if the entry identified by the given alias is a <i>trusted
     * certificate entry</i>, and false otherwise.
     * @return true if the entry identified by the given alias is a <i>trusted
     *         certificate entry</i>, false otherwise. */
    @Override
    public final boolean engineIsCertificateEntry(final String alias) {
        throw new UnsupportedOperationException();
    }

    /** Returns the (alias) name of the first keystore entry whose certificate
     * matches the given certificate.
     * <p>
     * This method attempts to match the given certificate with each keystore entry. If the entry being considered is a <i>trusted certificate
     * entry</i>, the given certificate is compared to that entry's certificate. If the entry being considered is a <i>key entry</i>, the given
     * certificate is compared to the first element of that entry's certificate chain (if a chain exists).
     * @param cert
     *        the certificate to match with.
     * @return the (alias) name of the first entry with matching certificate, or
     *         null if no such entry exists in this keystore. */
    @Override
    public final String engineGetCertificateAlias(final Certificate cert) {
        for (final KeyEntry entry : this.entries) {
            if (entry.getCertChain() != null && entry.getCertChain()[0].equals(cert)) {
                return entry.getAlias();
            }
        }
        return null;
    }

    /** engineStore is currently a no-op. Entries are stored during
     * engineSetEntry.
     * @param stream
     *        the output stream, which should be <code>null</code>
     * @param password
     *        the password, which should be <code>null</code>. */
    @Override
    public final void engineStore(final OutputStream stream, final char[] password) {
        // No es necesario hacer nada, se almacena en engineSetEntry()
    }

    /** Loads the keystore.
     * A compatibility mode is supported for applications that assume keystores
     * are stream-based. It permits (but ignores) a non-null <code>stream</code> or <code>password</code>. The mode is enabled by default. Set the
     * <code>sun.security.mscapi.keyStoreCompatibilityMode</code> system
     * property to <code>false</code> to disable compatibility mode and reject a
     * non-null <code>stream</code> or <code>password</code>.
     * @param stream
     *        the input stream, which should be <code>null</code>.
     * @param password
     *        the password, which should be <code>null</code>.
     * @exception IOException
     *            if there is an I/O or format problem with the keystore
     *            data. Or if compatibility mode is disabled and either
     *            parameter is non-null.
     * @exception SecurityException
     *            if the security check for <code>SecurityPermission("authProvider.<i>name</i>")</code> does not pass, where <i>name</i> is the value
     *            returned by
     *            this provider's <code>getName</code> method. */
    @Override
    public final void engineLoad(final InputStream stream, final char[] password) throws IOException {

        /*
         * Use the same security check as AuthProvider.login
         */
        final SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new SecurityPermission("authProvider.SunMSCAPI")); //$NON-NLS-1$
        }

        // Clear all key entries
        this.entries.clear();

        try {
            // Load keys and/or certificate chains
            loadKeysOrCertificateChains(getName(), this.entries);
        }
        catch (final KeyStoreException kse) {
            // Wrap the JNI exception in an IOException
            throw new IOException(kse.toString(), kse);
        }
    }

    /** Devuelve el nombre del almac&eacute;n.
     * @return Nombre del almac&eacute;n. */
    private String getName() {
        return this.storeName;
    }

    /** Load keys and/or certificates from keystore into Collection.
     * @param name Name of keystore.
     * @param ntries Collection of key/certificate.
     * @throws KeyStoreException Si hay problemas tratando el almac&eacute;n. */
    private void loadKeysOrCertificateChains(final String name, final Collection<KeyEntry> ntries) throws KeyStoreException {
        try {
            this.loadKeysOrCertificateChains.invoke(this.nativeWrapper, name, ntries);
        }
        catch (final Exception e) {
            throw new KeyStoreException(e);
        }
    }
}