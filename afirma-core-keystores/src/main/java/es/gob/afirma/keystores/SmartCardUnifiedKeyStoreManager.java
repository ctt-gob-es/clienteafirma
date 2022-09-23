/* Copyright (C) 2017 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.IOException;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.callbacks.UIPasswordCallback;

/** Representa a un <i>AOKeyStoreManager</i> que presenta los certificados de las
 * tarjetas detectadas en encontrado como parte de un &uacute;nico almacen de claves.
 * El uso que se plantea para este almac&eacute;n es el de servir para sustituir al
 * almac&eacute;n de Windows cuando se detecte que no se puede acceder al mismo. Este
 * caso se da en cuentas con un perfil temporal.<br>
 * Este almac&eacute;n har&aacute; uso de una serie de PKCS#11 conocidos para acceder
 * a las tarjetas. Tambi&eacute;n se har&aacute; uso del controlador 100% Java de DNIe
 * y se dar&aacute; prioridad al DNIe sobre el resto de tarjetas. Debido a que la carga
 * de este controlador puede interferir con el uso de los PKCS#11, si se detecta un DNIe
 * se ignorar&aacute;n el resto de tarjetas. */
public class SmartCardUnifiedKeyStoreManager extends AggregatedKeyStoreManager {

	private PasswordCallback passwordCallback = null;
	private Object[] configParams = null;

	/** Indica si una tarjeta preferente (DNIe) ya se carg&oacute;. */
	private boolean preferredKsAdded = false;

	/** Indica si el almac&eacute;n se carg&oacute; previamente. */
	private boolean initialized = false;

	/** Crea un <i>AOKeyStoreManager</i> para acceso a las tarjetas inteligentes
	 * conocidas que se encuentren en el sistema. */
	public SmartCardUnifiedKeyStoreManager() {
		setKeyStoreType(AOKeyStore.KNOWN_SMARTCARDS);
	}

	/** Inicializa la clase gestora de almacenes de claves. */
	@Override
	public final void init(final AOKeyStore type,
			               final InputStream store,
			               final PasswordCallback pssCallBack,
			               final Object[] params,
			               final boolean forceReset) {

		this.passwordCallback = pssCallBack;
		this.configParams = params != null ? params.clone() : null;

		final Object parentComponent = params != null && params.length > 0 ? params[0] : null;
		boolean excludePreferredKeyStores = false;

		// Vaciamos el listado de almacenes agregados
		removeAll();

		// Intentamos ahora agregar los almacenes externos preferentes ajenos a los
		// dispositivos de seguridad configurados en Firefox haciendo uso del controlador Java
		if (forceReset || !this.initialized) {
			try {
				this.preferredKsAdded = KeyStoreUtilities.addPreferredKeyStoreManagers(this, parentComponent);
			}
			catch (final AOCancelledOperationException e) {
				LOGGER.info("Se cancelo el uso del driver Java: " + e); //$NON-NLS-1$
				// En caso de haber detectado una tarjeta preferente pero haberse cancelado su uso,
				// permitiremos utilizar el resto de modulos a excepcion de las preferentes
				this.preferredKsAdded = false;
				excludePreferredKeyStores = true;
			}
		}

		// Si se pudo agregar algun almacen preferente entendemos que se desean usar y no cargamos
		// el resto de tarjetas. Si no, iniciamos el proceso de todos los PKCS#11.
		// DNIe nunca se cargara como almacen externo. En el caso de las tarjetas CERES, si no se
		// pudo cargar a traves del controlador JAVA, se intentara cargar a traves del PKCS#11.
		// Si se tienen registrados varios PKCS#11 para una misma tarjeta, en cuanto funcione
		// la carga mediante uno de ellos, se ignora el resto
		if (!this.preferredKsAdded) {

			boolean cardInitialized = false;

			for (final KnownSmartCardsPkcs11 sc : KnownSmartCardsPkcs11.values()) {

				LOGGER.warning("Intentamos cargar: " + sc.getDescription()); //$NON-NLS-1$

				// Si no se quieren cargar los PKCS#11 de los almacenes preferidos y
				// estamos ante uno de ellos, nos lo saltamos
				if (excludePreferredKeyStores && sc.isPreferred()) {
					continue;
				}

				for (final String pkcs11Library : sc.getPkcs11Names()) {

					LOGGER.warning("Probamos con la biblioteca: " + pkcs11Library); //$NON-NLS-1$

					AOKeyStoreManager ksm;
					try {
						ksm = initPkcs11(sc.getDescription(), parentComponent, forceReset, pkcs11Library);
					}
					catch (final AOCancelledOperationException ex) {
						LOGGER.warning("Se cancelo el acceso al almacen externo  '" + sc.getDescription() + "', se continuara con el siguiente: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
						continue;
					}
					catch (final Exception ex) {

						LOGGER.warning(
							"Fallo el acceso al almacen externo  '" + sc.getDescription() + "', se reintentara: " + ex //$NON-NLS-1$ //$NON-NLS-2$
						);

						// En ciertos sistemas Linux fallan las inicializaciones la primera vez por culpa de PC/SC, reintentamos
						if (Platform.OS.LINUX.equals(Platform.getOS())) {
							try {
								ksm = initPkcs11(sc.getDescription(), parentComponent, forceReset, pkcs11Library);
							}
							catch (final AOCancelledOperationException exc) {
								LOGGER.warning("Se cancelo el acceso al almacen externo  '" + sc.getDescription() + "', se continuara con el siguiente: " + exc); //$NON-NLS-1$ //$NON-NLS-2$
								continue;
							}
							catch(final Exception e) {
								LOGGER.warning(
									"Fallo el acceso al almacen externo  '" + sc.getDescription() + "', se continuara con el siguiente: " + e //$NON-NLS-1$ //$NON-NLS-2$
								);
								continue;
							}
						}
						continue;
					}
					cardInitialized = true;
					addKeyStoreManager(ksm);

					LOGGER.info("El almacen externo '" + sc.getDescription() + "' ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$ //$NON-NLS-2$
					break;
				}

				// Solo cargaremos una tarjeta
				if (cardInitialized) {
					break;
				}
			}
		}

		this.initialized = true;
	}

	private static AOKeyStoreManager initPkcs11(
			final String descr,
			final Object parent,
			final boolean forceReset,
			final String libName) throws AOKeyStoreManagerException, IOException {

		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		ksm.init(
				AOKeyStore.PKCS11,
				null,
				new UIPasswordCallback(KeyStoreMessages.getString("SmartCardUnifiedKeyStoreManager.0", descr.toString()), parent), //$NON-NLS-1$
				new String[] { libName, descr.toString() },
				forceReset
				);

		return ksm;
	}

	@Override
	public void refresh() throws IOException {
		init(AOKeyStore.KNOWN_SMARTCARDS, null, this.passwordCallback, this.configParams, true);
	}

	/**
	 * Bibliotecas PKCS#11 de las tarjetas inteligentes conocidas.
	 */
	private enum KnownSmartCardsPkcs11 {
		CERES("FNMT-CERES", new String[] { //$NON-NLS-1$
				"%SYSTEMROOT%/System32/FNMT_P11_x64.dll",  //$NON-NLS-1$
				"%SYSTEMROOT%/System32/FNMT_P11.dll", //$NON-NLS-1$
				"%SYSTEMROOT%/SysWOW64/FNMT_P11.dll" //$NON-NLS-1$
		}, true),
		CARDOS("CardOS", new String[] { //$NON-NLS-1$
				"%SYSTEMROOT%/SysWOW64/siecap11.dll",  //$NON-NLS-1$
				"%SYSTEMROOT%/System32/siecap11.dll" //$NON-NLS-1$
		}, false),
		TUI("TUI", new String[] { //$NON-NLS-1$
				"%PROGRAMFILES%/umu/Third-Party/Gemalto/Classic Client/BIN/gclib.dll", //$NON-NLS-1$
				"%PROGRAMFILES(x86)%/umu/Third-Party/Gemalto/Classic Client/BIN/gclib.dll", //$NON-NLS-1$
				"%PROGRAMFILES%/umu/UMU-Crypto/lib/libumupkcs11.dll", //$NON-NLS-1$
				"%PROGRAMFILES(x86)%/umu/UMU-Crypto/lib/libumupkcs11.dll", //$NON-NLS-1$
				"%PROGRAMFILES%/Gemalto/IDGo 800 PKCS#11/IDPrimePKCS1164.dll", //$NON-NLS-1$
				"%PROGRAMFILES%/Gemalto/IDGo 800 PKCS#11/IDPrimePKCS11.dll", //$NON-NLS-1$
				"%PROGRAMFILES(x86)%/Gemalto/IDGo 800 PKCS#11/IDPrimePKCS1164.dll", //$NON-NLS-1$
				"%PROGRAMFILES(x86)%/Gemalto/IDGo 800 PKCS#11/IDPrimePKCS11.dll" //$NON-NLS-1$
		}, false),
		SAFESIGN("SafeSign", new String[] { //$NON-NLS-1$
				"%SYSTEMROOT%/SysWOW64/aetpkss1.dll",  //$NON-NLS-1$
				"%SYSTEMROOT%/System32/aetpkss1.dll" //$NON-NLS-1$
		}, false);

		private static final String[] ENV_PROPERTIES = new String[] {
				"%PROGRAMFILES%", "%PROGRAMFILES(x86)%", "%SYSTEMROOT%" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		private String description;
		private String[] pkcs11Names;
		private boolean preferredKs;

		KnownSmartCardsPkcs11(final String description, final String[] pkcs11Names, final boolean preferredKs) {
			this.description = description;
			this.pkcs11Names = pkcs11Names != null ? pkcs11Names.clone() : null;
			this.preferredKs = preferredKs;
		}

		public String getDescription() {
			return this.description;
		}

		public String[] getPkcs11Names() {
			final String[] paths = new String[this.pkcs11Names.length];
			for (int i = 0; i < this.pkcs11Names.length; i++) {
				paths[i] = replaceEnvProperties(this.pkcs11Names[i]);
			}
			return paths;
		}

		private static String replaceEnvProperties(final String absolutePath) {
			String path = absolutePath;
			for (final String envProperty : ENV_PROPERTIES) {
				if (path.contains(envProperty)) {
					path = path.replace(envProperty, System.getenv(envProperty.substring(1, envProperty.length() - 1)));
				}
			}
			return path;
		}

		public boolean isPreferred() {
			return this.preferredKs;
		}
	}
}
