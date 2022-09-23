/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AggregatedKeyStoreManager;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos. */
public class MozillaUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

    private static final String ONLY_PKCS11 = "es.gob.afirma.keystores.mozilla.LoadSscdOnly"; //$NON-NLS-1$
    private static final String ONLY_PKCS11_ENV = "AFIRMA_NSS_LOAD_SSCD_ONLY"; //$NON-NLS-1$

    /** Propiedad de sistema que indica que hay que a&ntilde;adir el PKCS#11 nativo de DNIe aunque no
     * est&eacute; declarado como m&oacute;dulo externo en Mozilla. */
    public static final String INCLUDE_NATIVE_DNIE_P11 = "es.gob.afirma.keystores.mozilla.IncludeNativeDniePkcs11"; //$NON-NLS-1$

    /** Variable de entorno que indica que hay que a&ntilde;adir el PKCS#11 nativo de DNIe aunque no
     * est&eacute; declarado como m&oacute;dulo externo en Mozilla. */
    protected static final String INCLUDE_NATIVE_DNIE_P11_ENV = "AFIRMA_NSS_INCLUDE_NATIVE_DNIE_PKCS11"; //$NON-NLS-1$

	private PasswordCallback passwordCallback = null;
	private Object[] configParams = null;

	/** Indica si el almacen se cargo previamente. */
	private boolean initialized = false;
	private boolean preferredKsAdded = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public MozillaUnifiedKeyStoreManager() {
		setKeyStoreType(AOKeyStore.MOZ_UNI);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados ya que esta llamada puede realizarse como
		// parte de una operacion de refresco del almacen
		removeAll();

		final Object parentComponent = this.configParams != null && this.configParams.length > 0 ? this.configParams[0] : null;

		if (!Boolean.getBoolean(ONLY_PKCS11) && !Boolean.parseBoolean(System.getenv(ONLY_PKCS11_ENV))) {
			// Primero anadimos el almacen principal NSS
			final AOKeyStoreManager ksm = getNssKeyStoreManager();
			try {
				ksm.init(type, store, pssCallBack, this.configParams, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
					"No se ha podido cargar NSS, se continuara con los almacenes externos: " + e //$NON-NLS-1$
				);
			}
			setKeyStore(ksm.getKeyStore());
			addKeyStoreManager(ksm);
		}

		// Intentamos ahora agregar los almacenes externos preferentes ajenos a los
		// dispositivos de seguridad configurados en Firefox haciendo uso del controlador Java
		boolean excludePreferredKeyStores = false;
		if (forceReset || !this.initialized) {
			try {
				this.preferredKsAdded = KeyStoreUtilities.addPreferredKeyStoreManagers(this, parentComponent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				// En caso de haber detectado una tarjeta preferida pero haberse cancelado su uso,
				// permitiremos utilizar el resto de modulos a excepcion de los PKCS#11 que tambien
				// controlen las tarjetas preferidas, ya que se supone que no se desean utilizar
				this.preferredKsAdded = false;
				excludePreferredKeyStores = true;
			}
		}

		// Si se pudo agregar algun almacen preferente entendemos que se desean usar y no cargamos los
		// configurados en Firefox. Si no, iniciamos los almacenes externos. DNIe nunca se cargara como
		// almacen externo. En el caso de las tarjetas CERES, si no se pudo cargar a traves del
		// controlador JAVA, se intentara cargar a traves del PKCS#11 si estaba configurado en Firefox
		if (!this.preferredKsAdded) {
			final Map<String, String> externalStores = getExternalStores(excludePreferredKeyStores);

			if (externalStores.size() > 0) {
				final StringBuilder logStr = new StringBuilder(
					"Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: " //$NON-NLS-1$
				);
				for (final String key : externalStores.keySet()) {
					logStr.append("'"); //$NON-NLS-1$
					logStr.append(externalStores.get(key));
					logStr.append("' "); //$NON-NLS-1$
				}
				LOGGER.info(logStr.toString());
			}
			else {
				LOGGER.info("No se han encontrado modulos PKCS#11 externos instalados en Firefox"); //$NON-NLS-1$
			}

			for (final String descr : externalStores.keySet()) {
				final AOKeyStoreManager tmpKsm = new AOKeyStoreManager();
				try {
					internalInitStore(tmpKsm, descr, parentComponent, forceReset, externalStores.get(descr));
				}
				catch (final AOCancelledOperationException ex) {
					LOGGER.warning(
						"Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex //$NON-NLS-1$ //$NON-NLS-2$
					);
					continue;
				}
				catch (final Exception ex) {
					// En ciertos sistemas Linux fallan las inicializaciones la primera vez por culpa de PC/SC, reintentamos
					if (Platform.OS.LINUX.equals(Platform.getOS())) {
						try {
							internalInitStore(tmpKsm, descr, parentComponent, forceReset, externalStores.get(descr));
						}
						catch (final AOCancelledOperationException exc) {
							LOGGER.warning("Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + exc); //$NON-NLS-1$ //$NON-NLS-2$
							continue;
						}
						catch(final Exception e) {
							LOGGER.warning(
								"No se ha podido inicializar el PKCS#11 '" + descr + "' tras haberlo intentado dos veces: " + ex + ", " + e //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							);
							continue;
						}
					}
					else {
						LOGGER.warning("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
				}
				addKeyStoreManager(tmpKsm);

				LOGGER.info(
					"El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas y se detiene la carga del resto de almacenes" //$NON-NLS-1$ //$NON-NLS-2$
				);
				break;
			}
		}

		if (lacksKeyStores()) {
			LOGGER.warning(
				"No se ha podido inicializar ningun almacen, interno o externo, de Mozilla, ni los almacenes preferentes" //$NON-NLS-1$
			);
		}

		setKeyStoreType(type);

		this.initialized = true;
	}


	/** Inicializa un almac&eacute;n externo PKCS#11, mostrando un di&aacute;logo de inserci&oacute;n de PIN al usuario
	 * si es necesario.
	 * @param tmpKsm Gestor del almac&eacute;n.
	 * @param descr Nombre descriptivo del almac&eacute;n.
	 * @param parentComponent Componente padre sobre el que mostrar componentes gr&aacute;ficos.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @param libName Nombre del m&oacute;dulo PKCS#11 del almac&eacute;n.
     * @throws AOKeyStoreManagerException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Si se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *                     almac&eacute;n de certificados.
     * @throws AOCancelledOperationException Cuando se cancela el di&aacute;logo de inserci&oacute;n de PIN. */
	private static void internalInitStore(final AOKeyStoreManager tmpKsm,
			                              final String descr,
			                              final Object parentComponent,
			                              final boolean forceReset,
			                              final String libName) throws AOKeyStoreManagerException, IOException {
		tmpKsm.init(
			AOKeyStore.PKCS11,
			null,
			new UIPasswordCallback(
				FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + descr, //$NON-NLS-1$ //$NON-NLS-2$
				parentComponent
			),
			new String[] {
				libName, descr.toString()
			},
			forceReset
		);
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.MOZ_UNI, null, this.passwordCallback, this.configParams, true);
	}

	@SuppressWarnings("static-method")
	protected Map<String, String> getExternalStores() {
		return MozillaKeyStoreUtilities.getMozillaPKCS11Modules(
			// Si no es Linux o NO nos han indicado que incluyamos controlador nativo DNIe, lo excluimos
			!Platform.OS.LINUX.equals(Platform.getOS()) ||
				Boolean.getBoolean(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER) ||
					Boolean.parseBoolean(System.getenv(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER_ENV)),
			true  // Incluir los PKCS#11 que esten instalados en el sistema pero no en Mozilla
		);
	}

	@SuppressWarnings("static-method")
	protected Map<String, String> getExternalStores(final boolean excludePreferredKeyStores) {
		return MozillaKeyStoreUtilities.getMozillaPKCS11Modules(excludePreferredKeyStores, true);
	}

	/** Obtiene el gestor interno exclusivo del almac&acute;n NSS.
	 * @return Gestor interno exclusivo del almac&acute;n NSS. */
	protected AOKeyStoreManager getNssKeyStoreManager() {
		return new NssKeyStoreManager(getParentComponent(), false);
	}
}
