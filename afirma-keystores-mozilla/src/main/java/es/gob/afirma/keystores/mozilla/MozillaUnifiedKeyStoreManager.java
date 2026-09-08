/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.*;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.jmulticard.ui.DialogBuilder;

import javax.security.auth.callback.PasswordCallback;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.logging.Level;

/** Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox accedidos
 *  v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y externos. */
public class MozillaUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

    protected static final String ONLY_PKCS11 = "es.gob.afirma.keystores.mozilla.LoadSscdOnly"; //$NON-NLS-1$
    protected static final String ONLY_PKCS11_ENV = "AFIRMA_NSS_LOAD_SSCD_ONLY"; //$NON-NLS-1$

    /** Propiedad de sistema que indica que hay que a&ntilde;adir el PKCS#11 nativo de DNIe aunque no
     * est&eacute; declarado como m&oacute;dulo externo en Mozilla. */
    protected static final String INCLUDE_NATIVE_DNIE_P11 = "es.gob.afirma.keystores.mozilla.IncludeNativeDniePkcs11"; //$NON-NLS-1$

    /** Variable de entorno que indica que hay que a&ntilde;adir el PKCS#11 nativo de DNIe aunque no
     * est&eacute; declarado como m&oacute;dulo externo en Mozilla. */
    protected static final String INCLUDE_NATIVE_DNIE_P11_ENV = "AFIRMA_NSS_INCLUDE_NATIVE_DNIE_PKCS11"; //$NON-NLS-1$

    protected PasswordCallback passwordCallback = null;
    protected Object[] configParams = null;

	/** Indica si el almacen se cargo previamente. */
    protected boolean initialized = false;
    protected boolean smartcardLoadedByJMulticard = false;
	protected boolean dnieLoadedBypkcs11 = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Firefox. */
	public MozillaUnifiedKeyStoreManager() {
		setType(AOKeyStore.MOZ_UNI);
	}

	@Override
	public void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		LOGGER.info("Inicializamos el almacen de tipo: " + type); //$NON-NLS-1$

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		// Vaciamos el listado de almacenes agregados, ya que esta llamada puede realizarse como
		// parte de una operacion de refresco del almacen
		removeAll();

		Object parentComponent = null;

		if (this.configParams != null && this.configParams.length > 0) {
			parentComponent = this.configParams[0];
		}

		// Salvo que se indique que solo se carguen los PKCS#11, primero agregamos el almacen interno de Mozilla
		if (!Boolean.getBoolean(ONLY_PKCS11) && !Boolean.parseBoolean(System.getenv(ONLY_PKCS11_ENV))) {
			// Primero anadimos el almacen principal NSS
			final AOKeyStoreManager ksm = getNssKeyStoreManager();
			try {
				ksm.init(type, store, this.passwordCallback, this.configParams, forceReset);
			}
			catch(final Exception e) {
				LOGGER.severe(
					"No se ha podido cargar NSS, se continuara con los almacenes externos: " + e //$NON-NLS-1$
				);
			}
			setKeyStore(ksm.getKeyStore());
			addKeyStoreManager(ksm);
		}

		// Intentamos ahora agregar los almacenes de JMulticard. Estos almacenes siempre se agregan con maxima prioridad
		boolean excludeJMulticardKeyStores = false;
		if (forceReset || !this.initialized) {
			try {
				this.smartcardLoadedByJMulticard = KeyStoreUtilities.addJMulticardKeyStoreManagers(this, parentComponent, forceReset);
				setSmartCardAdded(this.smartcardLoadedByJMulticard);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				this.smartcardLoadedByJMulticard = false;
				// En caso de haya cancelado expresamente el uso de una tarjeta que se haya encontrado insertada,
				// lo marcamos para que, al cargar el resto de modulos mediante PKCS#11 se omitan aquellos que se
				// controlen desde JMulticard y que el usuario ha indicado que no desea utilizar
				excludeJMulticardKeyStores = true;
			}
		}

		// Si se cargo alguna tarjeta con JMulticard entendemos que se desean usar y no cargamos ningun otro
		// almacen externo. Si no, trataremos de cargar el DNI a traves de su PKCS#11.
		if (!this.smartcardLoadedByJMulticard) {
			try {
				AOKeyStoreManager pkcs11DnieKsm = getDNIePKCS11KeyStoreManager(parentComponent, forceReset);
				addKeyStoreManager(0, pkcs11DnieKsm);
				setSmartCardAdded(true);
				this.dnieLoadedBypkcs11 = true;
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del PKCS#11 del DNIe: " + e); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
					"No se ha podido cargar el DNIe a traves de su PKCS#11, se continuara con el resto de almacenes externos", e); //$NON-NLS-1$
			}

			// Si no se ha cargado el DNIe, cargamos los PKCS#11 externos declarados en el propio Firefox
			if (!this.dnieLoadedBypkcs11) {
				final Map<String, String> externalStores = getExternalStores(excludeJMulticardKeyStores);
				if (!externalStores.isEmpty()) {
					final StringBuilder logStr = new StringBuilder(
							"Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: " //$NON-NLS-1$
					);
					for (final String key : externalStores.keySet()) {
						logStr.append("'"); //$NON-NLS-1$
						logStr.append(externalStores.get(key));
						logStr.append("' "); //$NON-NLS-1$
					}
					LOGGER.info(logStr.toString());
				} else {
					LOGGER.info("No se han encontrado modulos PKCS#11 externos instalados en Firefox"); //$NON-NLS-1$
				}

				for (final String descr : externalStores.keySet()) {
					final AOKeyStoreManager tmpKsm;
					try {
						tmpKsm = initExternalStore(externalStores.get(descr), descr, parentComponent, forceReset);
					}
					catch (final AOCancelledOperationException ex) {
						LOGGER.warning(
								"Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex //$NON-NLS-1$ //$NON-NLS-2$
						);
						continue;
					}
					catch (final Exception ex) {
						LOGGER.warning("No se ha podido inicializar el PKCS#11 '" + descr + "': " + ex); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
					// Agregamos el almacen con maxima prioridad para que, dado un mismo certificado, se utilice el de
					// tarjeta en lugar del certificado software. Esto no afecta al uso de JMulticard, ya que si se ha
					// cargado un almacen de este tipo, no se cargan los PKCS#11 externos
					addKeyStoreManager(0, tmpKsm);

					LOGGER.info(
							"El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas y se detiene la carga del resto de almacenes" //$NON-NLS-1$ //$NON-NLS-2$
					);
					break;
				}
			}
		}

		if (lacksKeyStores()) {
			LOGGER.warning(
				"No se ha podido inicializar ningun almacen, interno o externo, de Mozilla, ni los almacenes preferentes" //$NON-NLS-1$
			);
		}

		setType(type);

		this.initialized = true;
	}

	/**
	 * Obtiene el almacen del DNIe mediante su PKCS#11
	 * @param parent Componente padre sobre el que mostrar componentes gr&aacute;ficos.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute.
	 * @return Almac&eacute;n de DNIe.
	 * @throws AOKeyStoreManagerException si ocurre un error al acceder o validar el keystore alternativo.
	 * @throws IOException si se produce un error de entrada/salida durante la lectura o escritura de datos.
	 */
	protected AOKeyStoreManager getDNIePKCS11KeyStoreManager(Object parent, boolean forceReset)
			throws AOKeyStoreManagerException, IOException {

		Component parentComponent = null;
		if (parent instanceof Component) {
			parentComponent = (Component) parent;
		}


		final PasswordCallback psc = DialogBuilder.getDefaultDniePasswordCallback(parentComponent);

		final AOKeyStoreManager dniKsm;
		try {
			dniKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.PKCS11_DNIE, null, null, psc, parent, forceReset);
		}
		catch (KeystoreAlternativeException e) {
			throw new AOKeyStoreManagerException("No se pudo cargar el DNIe a traves de su PKCS#11",
					e, KeyStoreErrorCode.Internal.LOADING_PKCS11_DNIE_ERROR);
		}

		return dniKsm;
	}

	/**
	 * Inicializa un almac&eacute;n externo PKCS#11, mostrando un di&aacute;logo de inserci&oacute;n de PIN al usuario
	 * si es necesario.
	 * @param libName Nombre del m&oacute;dulo PKCS#11 del almac&eacute;n.
	 * @param descr Nombre descriptivo del almac&eacute;n.
	 * @param parentComponent Componente padre sobre el que mostrar componentes gr&aacute;ficos.
	 * @param forceReset Indica si se debe forzar al reinicio del almac&eacute;n si ya estaba iniciado.
	 * @throws KeystoreAlternativeException Cuando ocurre cualquier problema durante la inicializaci&oacute;n
     * @throws IOException Si se ha insertado una contrase&ntilde;a incorrecta para la apertura del
     *                     almac&eacute;n de certificados.
     * @throws AOCancelledOperationException Cuando se cancela el di&aacute;logo de inserci&oacute;n de PIN.
	 */
	protected AOKeyStoreManager initExternalStore(
			final String libName,
			final String descr,
			final Object parentComponent,
			final boolean forceReset) throws IOException, KeystoreAlternativeException {

		PasswordCallback psc = new UIPasswordCallback(
				FirefoxKeyStoreMessages.getString("MozillaUnifiedKeyStoreManager.1") + " " + descr, //$NON-NLS-1$ //$NON-NLS-2$
				parentComponent);

		return AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.PKCS11, libName, descr, psc, parentComponent, forceReset);
	}

	@Override
	public void refresh() throws IOException {
        if (getParentComponent() != null) {
            if (this.passwordCallback != null && this.passwordCallback instanceof UIPasswordCallback) {
                ((UIPasswordCallback) this.passwordCallback).setParent(getParentComponent());
            }
            if (this.configParams == null || this.configParams.length == 0) {
                this.configParams = new Object[1];
            }
            this.configParams[0] = getParentComponent();
        }
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
