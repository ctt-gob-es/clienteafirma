/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JOptionPane;

import es.gob.afirma.core.MissingLibraryException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.filters.CertificateFilter;

/** Almacena una configuracui&oacute;n para el almac&eacute;bn establecido del
 * Cliente @firma. Gestiona su inicializaci&oacute;n y la selecci&oacute;n de
 * certificados por parte del usuario. */
final class KeyStoreConfigurationManager {

    /** Almac&eacute;n de claves configurado. */
    private AOKeyStore ks = null;

    /** Almac&eacute;n por defecto para el sistema en cuesti&oacute;n. */
    private AOKeyStore defaultKeyStore = null;

    /** Alias de certificado seleccionado. */
    private String selectedAlias = null;

    /** Manejador para los almacenes de claves. */
    private AOKeyStoreManager ksManager = null;

    /** Referencia al certificado con clave privada seleccionado. */
    private PrivateKeyEntry ke = null;

    /** Ruta del almac&eacute;n de claves seleccionado (para alamcenes P12/PFX,
     * JKS, P11...). */
    private String ksPath = null;

    /** Contrase&ntilde;a del almacen de claves seleccionado. */
    private String ksPassword = null;

    /** Mensaje de error establecido. */
    private String errorMessage = null;

    /** Componente padre sobre el que mostrar los di&aacute;logos modales. */
    private Component parent = null;

    /** Indica si debe advertirse al usuario de que inserte los dispositivos
     * criptogr&aacute;ficos externos antes de inicializar el almac&eacute;n de
     * certificados. */
    private boolean showLoadingWarning = false;

    private boolean showExpiratedCertificates = false;
    private boolean checkPrivateKey = true;
    private boolean mandatoryCert = false;
    private final List<CertificateFilter> certFilters = new ArrayList<CertificateFilter>();

    /** Construye la configuraci&oacute;n por defecto para el Cliente, pudiendo
     * variar el almac&eacute;n seg&uacute;n el sistema operativo:
     * <ul>
     * <li><b>Windows:</b> Almac&eacute;n de Windows/Internet Explorer.</li>
     * <li><b>Sistemas UNIX:</b> Almac&eacute;n Mozilla.</li>
     * <li><b>Mac OS X:</b> Almac&eacute;n de Apple Mac OS X.</li>
     * <li><b>Otros:</b> Almac&eacute;n PKCS#12/PFX.</li>
     * </ul>
     * @param keyStore
     *        Almac&eacute;n de claves por defecto.
     * @param parent
     *        Componente padre sobre el que se mostrar&aacute;n los
     *        di&aacute;logos modales necesarios. */
    KeyStoreConfigurationManager(final AOKeyStore keyStore, final Component parent) {
        this(keyStore);
        this.parent = parent;
    }

    /** Construye la configuraci&oacute;n por defecto para el Cliente, pudiendo
     * variar el almac&eacute;n seg&uacute;n el sistema operativo:
     * <ul>
     * <li><b>Windows:</b> Almac&eacute;n de Windows/Internet Explorer.</li>
     * <li><b>Sistemas Unix:</b> Almac&eacute;n Mozilla.</li>
     * <li><b>Mac OS X:</b> Almac&eacute;n de Apple Mac OS X.</li>
     * <li><b>Otros:</b> Almac&eacute;n PKCS#12/PFX.</li>
     * </ul>
     * @param keyStore
     *        Almac&eacute;n de claves por defecto. */
    private KeyStoreConfigurationManager(final AOKeyStore keyStore) {
        this.ks = this.defaultKeyStore = keyStore != null ? keyStore : getDefaultKeyStore();
    }

    /** Recupera el almac&eacute;n de claves por defecto para el sistema
     * operativo actual.
     * @return Almac&eacute;n de claves por defecto. */
    private static AOKeyStore getDefaultKeyStore() {
        if (Platform.getOS().equals(Platform.OS.WINDOWS))
         {
            return AOKeyStore.WINDOWS; // Sistemas Windows
        }
        if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
            return AOKeyStore.MOZ_UNI;
        }
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            return AOKeyStore.APPLE;
        }

        // Otros sistemas
        return AOKeyStore.PKCS12;
    }

    void resetFilters() {
        this.certFilters.clear();
    }

    void addCertFilter(final CertificateFilter certFilter) {
        if (certFilter != null) {
            this.certFilters.add(certFilter);
        }
    }

    /** Reestablece el almac&eacute;n de certificados por defecto y reinicia su
     * configuraci&oacute;n. */
    void initialize() {
        this.ks = this.defaultKeyStore;
        this.selectedAlias = null;
        this.ksManager = null;
        this.errorMessage = null;
        this.ke = null;
        this.showLoadingWarning = false;
        this.resetFilters();
    }

    /** Inicializa el repositorio de certificados establecido.
     * @throws AOKeystoreAlternativeException
     *         Cuando ocurre un error durante la inicializaci&oacute;n
     * @throws IOException
     * 		   Cuando la contrase&ntilde;a del almac&eacute;n es incorrecta. */
    private void initKeyStore() throws AOKeystoreAlternativeException, IOException {

        if (this.showLoadingWarning) {
        	AOUIFactory.showMessageDialog(
    			this.parent,
                AppletMessages.getString("KeyStoreConfigurationManager.0"), //$NON-NLS-1$
                AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE
            );
        }
        this.ksManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(this.ks, this.ksPath, null, this.getKeystorePasswordCallback(), this.parent);
    }

    /** Cambia el tipo de almac&eacute;n de claves establecido. Si existe una
     * configuraci&oacute;n previa (contrase&ntilde;a del almacen, alias
     * seleccionado,...) es recomendable utilizar previamente {@link #initialize()}.
     * @param keyStore
     *        Nuevo almac&eacute;n de claves. */
    void changeKeyStore(final AOKeyStore keyStore) {

        if (keyStore == null) {
            throw new IllegalArgumentException("Es obligatorio asignar un almacen de claves"); //$NON-NLS-1$
        }

        this.ks = keyStore;

        // Inicializamos la configuracion interna, la externa se respeta porque
        // puede haberse
        // establecido, teniendo en cuenta el cambio
        this.ke = null;
        this.ksManager = null;
    }

    /** Indica si hay un certificado seleccionado.
     * @return Devuelve {@code true} si hay un certificado seleccionado. */
    boolean isSelectedCertificate() {
        return this.ke != null;
    }

    /** Selecciona un certificado con clave privada del almac&eacute;n, ya sea a
     * trav&eacute;s de la configuraci&oacute;n proporcionada o
     * solicit&aacute;ndoselo al usuario.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando el usuario cancela la operaci&oacute;n.
     * @throws AOKeyStoreManagerException
     *         Cuando no se pueda inicializar el almac&eacute;n de
     *         certificados.
     * @throws AOCertificatesNotFoundException
     *         Cuando no se encuentran certificados v&aacute;lido en el
     *         almac&eacute;n.
     * @throws AOKeystoreAlternativeException
     *         Cuando no se pueda inicializar el almac&eacute;n de
     *         certificados pero existe un almac&eacute;n alternativo.
     * @throws CertificateException
     *         Cuando no se pueda seleccionar un certificado.
     * @throws KeyStoreException
     * 		   Cuando ocurren errores en el tratamiento del almac&eacute;n de claves.
     * @throws NoSuchAlgorithmException
     * 		   Cuando no se puede identificar el algoritmo para la recuperacion de la clave.
     * @throws UnrecoverableEntryException
     * 		   Cuando no se puede extraer la clave privada de un certificado.
     */
    void selectCertificate() throws AOKeyStoreManagerException,
                            AOCertificatesNotFoundException,
                            AOKeystoreAlternativeException,
                            CertificateException,
                            UnrecoverableEntryException,
                            NoSuchAlgorithmException,
                            KeyStoreException {
        if (this.selectedAlias == null) {
        	// Obtenemos el KeyStoreManager para asegurarnos de que esta inicializado
        	// el listado de alias
        	this.getKeyStoreManager();

        	final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
        			this.ksManager,                 // KeyStoreManager
                    this.parent,                    // Panel sobre el que mostrar el dialogo
                    this.checkPrivateKey,           // Comprobar accesibilidad de claves privadas
                    this.showExpiratedCertificates, // Muestra certificados caducados
                    true,                           // Advierte si el certificado esta caducado
                    this.certFilters,               // Filtros para los certificados
                    this.isMandatoryCert()          // Solo se admite un certificado
    				);
    		dialog.show();

    		this.selectedAlias = dialog.getSelectedAlias();
        }

        // En caso de ser todos certificados con clave privada, obtenemos la
        // referencia a esta
        if (this.checkPrivateKey) {
        	this.getKeyStoreManager().setEntryPasswordCallBack(this.getCertificatePasswordCallback());
            this.ke = this.getKeyStoreManager().getKeyEntry(this.selectedAlias);
        }
    }

    /** Recupera el certificado con el alias indicado del repositorio actual. Si
     * no existe ning&uacute;n certificado con ese alias, se devolver&aacute; {@code null}.
     * @param alias
     *        Alias del certificado que deseamos recuperar.
     * @return Certificado con el alias seleccionado.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando se cancela la operaci&oacute;n.
     * @throws AOKeyStoreManagerException
     *         Cuando no se ha podido inicializar el almac&eacute;n de
     *         certificados.
     * @throws AOKeystoreAlternativeException Cuando no se ha podido inicializar el almac&eacute;n de
     *         certificados pero hay una alternativa de uso. */
    Certificate getCertificate(final String alias) throws AOKeyStoreManagerException, AOKeystoreAlternativeException {
        return this.getKeyStoreManager().getCertificate(alias);
    }

    /** Recupera los alias del almacen seleccionado. Si ocurre alg&uacute;n error
     * durante la operaci&oacute;n se devuelve un array vac&iacute;o.
     * @return Certificado con el alias seleccionado.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando se cancela la operaci&oacute;n.
     * @throws AOKeyStoreManagerException
     *         Cuando no se ha podido inicializar el almac&eacute;n de
     *         certificados.
     * @throws AOKeystoreAlternativeException Cuando no se ha podido inicializar el almac&eacute;n de
     *         certificados pero hay una alternativa de uso. */
    String[] getArrayCertificateAlias() throws AOKeyStoreManagerException, AOKeystoreAlternativeException {

    	// Recuperamos el listado filtrado de alias. usamos los reales (las claves) y
    	// no los nombre legibles (los valores)
    	final Set<String> aliases = KeyStoreUtilities.getAliasesByFriendlyName(
				this.getKeyStoreManager().getAliases(),
				this.getKeyStoreManager(),
				this.checkPrivateKey,
				this.showExpiratedCertificates,
				this.certFilters
			).keySet();
    	return aliases.toArray(new String[aliases.size()]);
    }

    /** Recupera el gestor del almac&eacute;n de certificados actual ya
     * inicializado.
     * @return Gestor del almac&eacute;n de certificados.
     * @throws es.gob.afirma.core.AOCancelledOperationException
     *         Cuando se cancela la operaci&oacute;n.
     * @throws AOKeyStoreManagerException
     *         Cuando no se ha podido inicializar el almac&eacute;n.
     * @throws AOKeystoreAlternativeException Cuando no se ha podido inicializar el almac&eacute;n de
     *         certificados pero hay una alternativa de uso. */
    AOKeyStoreManager getKeyStoreManager() throws AOKeyStoreManagerException, AOKeystoreAlternativeException {
        if (this.ksManager == null) {
            try {
                this.initKeyStore();
            }
            catch (final MissingLibraryException e) {
            	throw new AOKeystoreAlternativeException(
            			getAlternateKeyStoreType(this.ks),
            			"No es posible cargar el almacen por falta de una biblioteca necesaria, se cargara el siguiente almacen disponible", //$NON-NLS-1$
            			e);
			}
            catch (final IOException e) {
            	throw new AOKeyStoreManagerException("No se ha podido inicializar el almacen de certificados", e); //$NON-NLS-1$
			}
        }
        return this.ksManager;
    }

    /** Devuelve el almac&eacute;n de certificados configurado.
     * @return Almac&eacute;n de certificados. */
    AOKeyStore getKeyStore() {
        return this.ks;
    }

    /** Recupera la referencia a la clave privada del certificado.
     * @return Clave privada del certificado. */
    PrivateKeyEntry getCertificateKeyEntry() {
        return this.ke;
    }

    /** Recupera el PasswordCallback apropiado para los certificados del
     * almac&eacute;n concreto o, si se especific&oacute; una contrase&ntilde;a,
     * uno con ella prefijada.
     * @return PasswordCallback para la obtenci&oacute;n de la clave del
     *         certificado. */
    private PasswordCallback getCertificatePasswordCallback() {
        return this.ksPassword == null
          ? this.ks.getCertificatePasswordCallback(this.parent)
    		  : new CachePasswordCallback(this.ksPassword.toCharArray());
    }

    /** Recupera el PasswordCallback apropiado para el almacen de claves
     * configurado o, si se especific&oacute; una contrase&ntilde;a, uno con
     * ella prefijada.
     * @return PasswordCallback para el acceso al almac&eacute;n. */
    private PasswordCallback getKeystorePasswordCallback() {
        return this.ksPassword == null
	      ? this.ks.getStorePasswordCallback(this.parent)
    		  : new CachePasswordCallback(this.ksPassword.toCharArray());
    }

    /** Recupera el certificado seleccionado.
     * @return Certificado seleccionado o nulo si no hab&iacute;a ninguno.
     * @throws AOKeyStoreManagerException
     * 		   Cuando no est&aacute; inicializado el almac&eacute;n de claves.
     * @throws AOKeystoreAlternativeException
     * 		   Cuando ocurre cualquier otro problema al acceder al almac&eacute;n.
     * */
    X509Certificate getSelectedCertificate() throws AOKeyStoreManagerException, AOKeystoreAlternativeException {
        if (this.ke != null) {
            final Certificate cert = this.ke.getCertificate();
            if (cert instanceof X509Certificate) {
                return (X509Certificate) cert;
            }
        }
        if (this.selectedAlias != null) {
			return this.getKeyStoreManager().getCertificate(this.selectedAlias);
        }
        throw new UnsupportedOperationException("No se puede recuperar el Certificado X509"); //$NON-NLS-1$
    }

    /** Recupera el alias del certificado seleccionado.
     * @return Alias del certificado. */
    String getSelectedAlias() {
        return this.selectedAlias;
    }

    /** Establece el alias del certificado que debe utilizarse.
     * @param selectedAlias
     *        Alias del certificado. */
    void setSelectedAlias(final String selectedAlias) {
        if (this.selectedAlias != null && !this.selectedAlias.equals(selectedAlias)) {
            this.ke = null;
        }
        this.selectedAlias = selectedAlias;
    }

    /** Establece la contrase&ntilde; para el almac&eacute;n de claves.
     * @param password
     *        Contrase&ntilde;a para el almac&eacute;n */
    void setKsPassword(final String password) {
        this.ksPassword = password;
    }

    /** Establece la ruta del almac&eacute;n de claves.
     * @param path
     *        Ruta absoluta del almac&eacute;n. */
    void setKsPath(final String path) {
        this.ksPath = path == null || path.trim().equals("") ? null : path; //$NON-NLS-1$
    }

    /** Indica si se seleccionar&aacute; autom&aacute;ticamente el certificado de
     * firma.
     * @return Devuelve {@code true} si autoselecciona el certificado. */
    private boolean isMandatoryCert() {
        return this.mandatoryCert;
    }

    /** Establece si deben mostrarse los certificados caducados.
     * @param showExpiratedCerts
     *        {@code true} para mostrar los certificados caducados. */
    void setShowExpiratedCertificates(final boolean showExpiratedCerts) {
        this.showExpiratedCertificates = showExpiratedCerts;
    }

    /** Establece si s&oacute;lo deben mostrarse los certificados de firma.
     * @param checkPrivateKey
     *        {@code true} para mostrar s&oacute;lo los certificados de firma. */
    void setShowOnlySignatureCertificates(final boolean checkPrivateKey) {
    	this.checkPrivateKey = checkPrivateKey;
    }

    /** Establece que se seleccione autom&aacute;ticamente el certificado cuando
     * s&oacute;lo quede uno despu&eacute;s de pasar los distintos filtros. Si
     * quedase m&aacute;s de un certificado se lanzar&iacute;a una
     * excepci&oacute;n.
     * @param mCert
     *        {@code true} para indicar que se seleccione
     *        autom&aacute;ticamente el certificado. */
    void setMandatoryCert(final boolean mCert) {
        this.mandatoryCert = mCert;
    }

    /** Establece que se muestre o no, antes de cargar un almac&eacute;n de
     * certificados, una advertencia del tipo
     * "Inserte su tarjeta inteligente o cualquier otro dispositivo antes de continuar"
     * .
     * @param showWarning
     *        Si es {@code true} se mostrar&aacute; la advertencia. */
    void setLoadingWarning(final boolean showWarning) {
        this.showLoadingWarning = showWarning;
    }

    /** Recupera el mensaje de error identificado en la configurac&oacute;n
     * actual para el almac&eacute;n. Si no se ha identificado ning&uacute;n
     * error se devolver&aacute; {@code null}.
     * @return Mensaje de error. */
    String getErrorMessage() {
        return this.errorMessage;
    }

    /** Obtiene un almac&eacute;n de claves alternativo.
     * @param currentStore Almac&acute;n actual, para el cual se busca una alternativa.
     * @return <code>AOKeyStore</code> alternativo o <code>null</code> si no hay alternativo */
    private static AOKeyStore getAlternateKeyStoreType(final AOKeyStore currentStore) {
        if (AOKeyStore.PKCS12.equals(currentStore)) {
            return null;
        }
        if (Platform.OS.WINDOWS.equals(Platform.getOS()) && !AOKeyStore.WINDOWS.equals(currentStore)) {
            return AOKeyStore.WINDOWS;
        }
        if (Platform.OS.MACOSX.equals(Platform.getOS()) && !AOKeyStore.APPLE.equals(currentStore)) {
            return AOKeyStore.APPLE;
        }
        return AOKeyStore.PKCS12;
    }
}
