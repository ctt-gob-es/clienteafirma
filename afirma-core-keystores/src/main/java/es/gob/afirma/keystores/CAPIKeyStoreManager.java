package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.InvalidOSException;
import es.gob.afirma.core.misc.Platform;

/** Clase gestora de claves y certificados en los almacenes <i>ROOT</i> y <i>MY</i> de CAPI.
 * @version 0.1 */
public final class CAPIKeyStoreManager extends AOKeyStoreManager {

	private static KeyStore capiKsMy = null;

    /** Obtiene la clave privada de un certificado.
     * @param alias
     *        Alias del certificado
     * @param pssCallback
     *        <i>CallBback</i> para obtener la contrase&ntilde;a del
     *        certificado que contiene la clave
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException
     * 		   Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException
     * 		   Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException
     * 		   Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException
     * 		   Cuando el usuario cancela el proceso antes de que finalice
     */
    @Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias,
    		                                    final PasswordCallback pssCallback) throws KeyStoreException,
    		                                                                               NoSuchAlgorithmException,
    		                                                                               UnrecoverableEntryException {
        if (capiKsMy == null) {
            throw new IllegalStateException("Se han pedido claves a un almacen no inicializado"); //$NON-NLS-1$
        }
        return (KeyStore.PrivateKeyEntry) capiKsMy.getEntry(alias, new KeyStore.PasswordProtection("dummy".toCharArray())); //$NON-NLS-1$
    }

	@Override
	public List<KeyStore> init(final AOKeyStore type,
			                   final InputStream store,
			                   final PasswordCallback pssCallBack,
			                   final Object[] params) throws AOKeyStoreManagerException, IOException {
		if (AOKeyStore.WINDOWS.equals(type)) {
			setKeyStoreType(AOKeyStore.WINDOWS);
        	return initCAPI();
        }
		throw new AOKeyStoreManagerException("Tipo de almacen no soportado: " + type.getName()); //$NON-NLS-1$
	}

    private static List<KeyStore> initCAPI() throws AOKeyStoreManagerException, IOException {

    	if (capiKsMy == null) {

	        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
	            throw new InvalidOSException("Microsoft Windows"); //$NON-NLS-1$
	        }

	        // Si no se ha agregado el proveedor CAPI de Sun, lo anadimos
	        // En java 6 viene instalado de serie, pero no pasa nada por reinstalarlo
	        if (Security.getProvider("SunMSCAPI") == null) { //$NON-NLS-1$
	            try {
	                Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance()); //$NON-NLS-1$
	            }
	            catch(final Exception e) {
	            	LOGGER.severe("No se ha podido instanciar 'sun.security.mscapi.SunMSCAPI': " + e); //$NON-NLS-1$
	            	throw new MissingSunMSCAPIException(e);
	            }
	        }

	        // Inicializamos
	        try {
	        	capiKsMy = KeyStore.getInstance(AOKeyStore.WINDOWS.getProviderName());
	        }
	        catch (final Exception e) {
	            throw new AOKeyStoreManagerException("No se ha podido obtener el almacen Windows.MY: " + e, e); //$NON-NLS-1$
	        }

	        LOGGER.info("Cargando KeyStore de Windows"); //$NON-NLS-1$
	        try {
	        	capiKsMy.load(null, null);
	        }
	        catch (final CertificateException e) {
	            throw new AOKeyStoreManagerException("No se han podido cargar los certificados del almacen Windows.MY: " + e, e); //$NON-NLS-1$
	        }
	        catch (final NoSuchAlgorithmException e) {
	        	throw new AOKeyStoreManagerException("No se ha podido verificar la integridad del almacen Windows.MY: " + e, e); //$NON-NLS-1$
			}

	        // Tratamos los alias repetidos, situacion problematica afectada por el bug
	        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6483657
	        // Este solo se da con SunMSCAPI
	        try {
	            cleanCAPIDuplicateAliases(capiKsMy);
	        }
	        catch (final Exception e) {
	            LOGGER.warning("No se han podido tratar los alias duplicados: " + e); //$NON-NLS-1$
	        }

    	}

        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(capiKsMy);
        return ret;
    }

    /** Obtiene un certificado del almac&eacute;n activo a partir de su alias.
     * @param alias Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    @Override
	public X509Certificate getCertificate(final String alias) {
        if (alias == null) {
            LOGGER.warning("El alias del certificado es nulo, se devolvera null"); //$NON-NLS-1$
            return null;
        }

        if (capiKsMy == null) {
            LOGGER.warning(
        		"No se ha podido recuperar el certificado con alias '" + alias + "' porque el KeyStore no estaba inicializado, se devolvera null" //$NON-NLS-1$ //$NON-NLS-2$
    		);
            return null;
        }

        X509Certificate cert = null;
        try {
            cert = (X509Certificate) capiKsMy.getCertificate(alias);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        if (cert == null) {
            LOGGER.warning("No se ha podido recuperar el certificado con alias '" + alias + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        return  cert;

    }

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore
     * activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
    @Override
	public X509Certificate[] getCertificateChain(final String alias) {
        if (capiKsMy == null) {
            LOGGER.warning("El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias '" + alias + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        try {
            return (X509Certificate[]) capiKsMy.getCertificateChain(alias);
        }
        catch (final Exception e) {
            LOGGER.severe(
              "Error al obtener la cadena de certificados para el alias '" + alias //$NON-NLS-1$
                 + "', se devolvera una cadena vacia: " + e //$NON-NLS-1$
            );
            return new X509Certificate[0];
        }
    }

    /** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
    @Override
	public String[] getAliases() {

        if (capiKsMy == null) {
            throw new IllegalStateException("Se han pedido los alias de un almacen no inicializado"); //$NON-NLS-1$
        }

        LOGGER.info("Solicitando los alias al KeyStore (" + capiKsMy.getProvider() + ")"); //$NON-NLS-1$ //$NON-NLS-2$

        final Enumeration<String> aliases;
        try {
            aliases = capiKsMy.aliases();
        }
        catch (final Exception e) {
            LOGGER.severe("Error intentando obtener los alias del almacen de claves, se devolvera una enumeracion vacia: " + e); //$NON-NLS-1$
            return new String[0];
        }

        String currAlias;
        final List<String> v = new ArrayList<String>();

        LOGGER.info("Componiendo el vector de alias"); //$NON-NLS-1$

        while (aliases.hasMoreElements()) {
            currAlias = aliases.nextElement().toString();
            v.add(currAlias);
        }

        return v.toArray(new String[0]);
    }

    /** Devuelve el <code>keyStore</code> en uso.
     * @return Almac&eacute;n de claves (<code>KeyStore</code>) actual */
    @Override
	public List<KeyStore> getKeyStores() {
        final List<KeyStore> ret = new ArrayList<KeyStore>(1);
        ret.add(capiKsMy);
        return ret;
    }

    @Override
    public String toString() {
    	return "Gestor del almacen Windows.MY de CAPI via SunMSCAPI"; //$NON-NLS-1$
    }

    private static void cleanCAPIDuplicateAliases(final KeyStore keyStore) throws NoSuchFieldException,
                                                                                  IllegalAccessException {

    	Field field = keyStore.getClass().getDeclaredField("keyStoreSpi"); //$NON-NLS-1$
    	field.setAccessible(true);
    	final KeyStoreSpi keyStoreVeritable = (KeyStoreSpi) field.get(keyStore);

    	if ("sun.security.mscapi.KeyStore$MY".equals(keyStoreVeritable.getClass().getName())) { //$NON-NLS-1$
    		String alias, hashCode;
    		X509Certificate[] certificates;

    		field = keyStoreVeritable.getClass().getEnclosingClass().getDeclaredField("entries"); //$NON-NLS-1$
    		field.setAccessible(true);
    		final Collection<?> entries = (Collection<?>) field.get(keyStoreVeritable);

    		for (final Object entry : entries) {
    			field = entry.getClass().getDeclaredField("certChain"); //$NON-NLS-1$
    			field.setAccessible(true);
    			certificates = (X509Certificate[]) field.get(entry);

    			hashCode = Integer.toString(certificates[0].hashCode());

    			field = entry.getClass().getDeclaredField("alias"); //$NON-NLS-1$
    			field.setAccessible(true);
    			alias = (String) field.get(entry);

    			if (!alias.equals(hashCode)) {
    				field.set(entry, alias.concat(" - ").concat(hashCode)); //$NON-NLS-1$
    			}
    		} // for
    	} // if
    }

}
