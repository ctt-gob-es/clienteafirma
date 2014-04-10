package es.gob.afirma.keystores.mozilla;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.Provider;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

final class NssKeyStoreManager extends AOKeyStoreManager {

	private static Provider nssProvider = null;

	/** Componente padre sobre el que montar los di&aacute;logos modales. */
	private final Object parentComponent;

	/** Construye un gestor de almac&eacute;n de claves y certificados Mozilla (NSS).
	 * @param parent Elemento padre para la modalidad */
	NssKeyStoreManager(final Object parent) {
		setKeyStoreType(AOKeyStore.MOZ_UNI);
		this.parentComponent = parent;
	}

	/** Inicializa la clase gestora de almacenes de claves.
	 * @throws AOKeyStoreManagerException
	 *         Si no puede inicializarse el almac&eacute;n NSS interno */
	@Override
	public void init(final AOKeyStore type,
			         final InputStream store,
			         final PasswordCallback pssCallBack,
			         final Object[] params,
			         final boolean forceReset) throws AOKeyStoreManagerException {

		// Se ha detectado que en algunas versiones de Java/OpenJDK, al solicitar un proveedor
		// de seguridad comprobar su existencia, puede afectar negativamente a que este proveedor
		// se cargue en un futuro, asi que guardamos una copia local del proveedor para hacer
		// estas comprobaciones
		// getNssProvider() hace toda la inicializacion de NSS como PKCS#11 especial en Java
		final Provider p = getNssProvider();

		KeyStore keyStore = null;
		if (p != null) {
			try {
				keyStore = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido obtener el KeyStore PKCS#11 NSS del proveedor SunPKCS11: " + e); //$NON-NLS-1$
				keyStore = null;
			}
		}

		if (keyStore != null) {
			try {
				keyStore.load(null, new char[0]);
			}
			catch (final Exception e) {
				try {
					keyStore.load(null, pssCallBack != null
						? pssCallBack.getPassword()
							: new UIPasswordCallback(FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.0"), //$NON-NLS-1$
								this.parentComponent).getPassword());
				}
				catch (final AOCancelledOperationException e1) {
					keyStore = null;
					throw e1;
				}
				catch (final Exception e2) {
					LOGGER.warning(
						"No se ha podido abrir el almacen PKCS#11 NSS del proveedor SunPKCS11: " + e2 //$NON-NLS-1$
					);
					keyStore = null;
				}
			}
		}

		if (keyStore != null) {
			setKeyStore(keyStore);
		}

	}

	/** Carga e instala el proveedor de seguridad para el acceso al almac&eacute;n de NSS. Si
	 * ya estaba cargado, lo recupera directamente.
	 * @return Proveedor para el acceso a NSS. */
	private static Provider getNssProvider() {
		if (nssProvider != null) {
			return nssProvider;
		}
		try {
			nssProvider = MozillaKeyStoreUtilities.loadNSS();
		}
		catch (final Exception e) {
			LOGGER.severe("Error inicializando el proveedor NSS: " + e); //$NON-NLS-1$
			nssProvider = null;
		}
		return nssProvider;
	}
}
