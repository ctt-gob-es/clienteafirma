/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.Provider;
import java.security.cert.X509Certificate;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.TextOutputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.auth.login.LoginException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidades para le manejo de almacenes de claves y certificados. */
public final class KeyStoreUtilities {

	/** Nombre de la variable de entorno (a nivel de sistema operativo) que debe establecerse a <code>true</code>
	 * si se desea <b>no</b> usar el controlador de DNIe Java interno del programa. */
	public static final String DISABLE_DNIE_NATIVE_DRIVER_ENV = "AFIRMA_NSS_DISABLE_DNIE_NATIVE_DRIVER"; //$NON-NLS-1$

	/** Nombre de la propiedad Java (a nivel de JVM) que debe establecerse a <code>true</code>
	 * si se desea <b>no</b> usar el controlador de DNIe Java interno del programa. */
	public static final String DISABLE_DNIE_NATIVE_DRIVER = "es.gob.afirma.keystores.mozilla.disableDnieNativeDriver"; //$NON-NLS-1$

	/** Nombre de la variable de entorno a nivel de sistema operation que debe establecerse a <code>true</code>
	 * si se desea <b>no</b> usar el controlador Java de tarjetas FNMT CERES interno del programa. */
	public static final String DISABLE_CERES_NATIVE_DRIVER_ENV = "AFIRMA_NSS_DISABLE_CERES_NATIVE_DRIVER"; //$NON-NLS-1$

	/** Nombre de la propiedad Java (a nivel de JVM) que debe establecerse a <code>true</code>
	 * si se desea <b>no</b> usar el controlador Java de tarjetas FNMT CERES interno del programa. */
	public static final String DISABLE_CERES_NATIVE_DRIVER = "es.gob.afirma.keystores.mozilla.disableCeresNativeDriver"; //$NON-NLS-1$

	/** Nombre de la variable de entorno a nivel de sistema operativo que debe establecerse a <code>true</code>
	* si se desea <b>no</b> usar el controlador Java de tarjetas G&amp;D SmartCafe interno del programa. */
	public static final String ENABLE_GYDSC_NATIVE_DRIVER_ENV = "AFIRMA_NSS_ENABLE_GYDSC_NATIVE_DRIVER"; //$NON-NLS-1$

	/** Nombre de la propiedad Java (a nivel de JVM) que debe establecerse a <code>true</code>
	 * si se desea <b>no</b> usar el controlador Java de tarjetas G&amp;D SmartCafe interno del programa. */
	public static final String ENABLE_GYDSC_NATIVE_DRIVER = "es.gob.afirma.keystores.mozilla.enableGYDSCNativeDriver"; //$NON-NLS-1$

	/** Longitud maxima que permitimos para un alias. Si se excede, se considerara que no se trata de tal,
	 * sino de un PrincipalName. */
	private static final int ALIAS_MAX_LENGTH = 120;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private KeyStoreUtilities() {
        // No permitimos la instanciacion
    }

    /** Nombre de los ficheros de biblioteca de los controladores de la FNMT para DNIe, CERES y TIF
     * que no implementan la firma con algoritmo de huella SHA1. */
    private static final String[] FNMT_PKCS11_LIBS_WITHOUT_SHA1 = {
    	"DNIe_P11_priv.dll", //$NON-NLS-1$
    	"DNIe_P11_pub.dll", //$NON-NLS-1$
    	"FNMT_P11.dll", //$NON-NLS-1$
    	"FNMT_P11_x64.dll", //$NON-NLS-1$
    	"UsrPkcs11.dll", //$NON-NLS-1$
    	"UsrPubPkcs11.dll", //$NON-NLS-1$
    	"TIF_P11.dll" //$NON-NLS-1$
    };

	private static final String PIN_ERROR_LOCKED = "CKR_PIN_LOCKED"; //$NON-NLS-1$
	private static final String PIN_ERROR_WRONG_LENGTH = "CKR_PIN_LEN_RANGE"; //$NON-NLS-1$
	private static final String PIN_ERROR_INCORRECT = "CKR_PIN_INCORRECT"; //$NON-NLS-1$

    /** Crea las l&iacute;neas de configuraci&oacute;n para el proveedor PKCS#11
     * de Sun.
     * @param lib Nombre (con o sin ruta) de la biblioteca PKCS#11
     * @param name Nombre que queremos tenga el proveedor. CUIDADO: SunPKCS11
     *             a&ntilde;ade el prefijo <i>SunPKCS11-</i>.
     * @param slot Lector de tarjetas en el que buscar la biblioteca.
     * @return Fichero con las propiedades de configuracion del proveedor
     *         PKCS#11 de Sun para acceder al KeyStore de un token gen&eacute;rico */
    public static String createPKCS11ConfigFile(final String lib, final String name, final Integer slot) {

        final StringBuilder buffer = new StringBuilder("library="); //$NON-NLS-1$

        buffer.append(getLibraryPath(lib));
		buffer.append("\r\n") //$NON-NLS-1$

	        // Ignoramos la descripcion que se nos proporciona, ya que el
	        // proveedor PKCS#11 de Sun falla si llegan espacios o caracteres raros
            .append("name=") //$NON-NLS-1$

            .append(name != null ? name : "AFIRMA-PKCS11") //$NON-NLS-1$

            // El showInfo debe ser 'true' por el error 8039912 de Java
            //       https://bugs.openjdk.java.net/browse/JDK-8039912
            .append("\r\nshowInfo=true\r\n"); //$NON-NLS-1$

        if (slot != null) {
            buffer.append("slot=").append(slot).append("\r\n"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Por un problema con la version 10 del driver de la FNMT para el DNIe y tarjetas CERES
        // debemos deshabilitar el mecanismo del algorimto de firma con SHA1 para que lo emule
        for (final String problematicLib : FNMT_PKCS11_LIBS_WITHOUT_SHA1) {
        	if (problematicLib.equalsIgnoreCase(new java.io.File(lib).getName())) {
        		buffer.append("disabledMechanisms={ CKM_SHA1_RSA_PKCS }\r\n"); //$NON-NLS-1$
        		break;
        	}
        }

        LOGGER.info("Creada configuracion PKCS#11:\r\n" + buffer.toString()); //$NON-NLS-1$
        return buffer.toString();
    }


    /** Obtiene la ruta de la biblioteca de tal forma que es apta para su
     * uso carga mediante NSS.
     * @param lib Biblioteca del que obtener la ruta..
     * @return Ruta de la biblioteca.
     */
    private static String getLibraryPath(final String lib) {

    	File resultFile = new File(lib);

    	// Si no encontramos el fichero y es una ruta relativa, trataremos de componer la ruta completa
    	if (!resultFile.isFile() && !resultFile.isAbsolute()) {
    		boolean found = false;
    		File libraryFile;
    		final String[] libraryPaths = System.getProperty("java.library.path").split(File.pathSeparator); //$NON-NLS-1$
    		for (int i = 0; i < libraryPaths.length && !found; i++) {
    			libraryFile = new File(libraryPaths[i], lib);
    			if (libraryFile.isFile()) {
    				resultFile = libraryFile;
    				found = true;
    			}
    		}
    	}

    	// Calculamos el nombre corto de la ruta si contiene parentesis (caso comun cuando las bibliotcas
    	// estan en "Program files (x86)"), ya que la carga del PKCS#11 falla cuando estos caracteres
    	// se encuentran en la ruta de la biblioteca
    	String libPath = resultFile.getAbsolutePath();
    	if (libPath.contains(")") || libPath.contains("(")) { //$NON-NLS-1$ //$NON-NLS-2$
        	libPath = getWindowsShortName(libPath);
        }
    	LOGGER.info(LoggerUtil.getCleanUserHomePath(String.format("La ruta de la libreria '%s' se interpretara como '%s'", lib, libPath))); //$NON-NLS-1$
		return libPath;
	}

    /** Obtiene una mapa con las descripciones usuales de los alias de
     * certificados (como claves de estas &uacute;ltimas). Se aplicar&aacute;n los
     * filtros de certificados sobre todos ellos y se devolver&aacute;n aquellos
     * certificados que cumplan con los filtros definidos.
     * @param aliases Alias de los certificados entre los que el usuario debe
     *                seleccionar uno
     * @param ksm Gestor de los almac&eacute;nes de certificados a los que pertenecen los alias.
     *            Debe ser {@code null} si se quiere usar el m&eacute;todo para seleccionar
     *            otra cosa que no sean certificados X.509 (como claves de cifrado)
     * @param checkPrivateKeys Indica si se debe comprobar que el certificado tiene clave
     *                         privada o no, para no mostrar aquellos que carezcan de ella
     * @param showExpiredCertificates Indica si se deben o no mostrar los certificados caducados o
     *                                a&uacute;n no v&aacute;lidos
     * @param certFilters Filtros a aplicar sobre los certificados.
     * @return Mapa que asocia los alias reales de los certificados que han pasados los
     *         filtros con un nombre mas amistoso. */
    public static Map<String, String> getAliasesByFriendlyName(final String[] aliases,
                                                               final KeyStoreManager ksm,
                                                               final boolean checkPrivateKeys,
                                                               final boolean showExpiredCertificates,
                                                               final List<? extends CertificateFilter> certFilters) {

        // Creamos un mapa con la relacion Alias-Nombre_a_mostrar de los
        // certificados
    	final String[] trimmedAliases = aliases.clone();
        final Map<String, String> aliassesByFriendlyName = new Hashtable<>(trimmedAliases.length);
        for (final String trimmedAlias : trimmedAliases) {
            aliassesByFriendlyName.put(trimmedAlias, trimmedAlias);
        }

        String tmpCN;
        if (ksm != null) {

        	X509Certificate tmpCert;
            for (final String al : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
                tmpCert = null;

                try {
                    tmpCert = ksm.getCertificate(al);
                }
                catch (
            		final AOCancelledOperationException e
        		) {
                	throw e;
                }
                catch (final RuntimeException e) {
                	// Comprobaciones especifica para la compatibilidad con el proveedor de DNIe
                	if (
            			e instanceof es.gob.jmulticard.CancelledOperationException ||
                		e instanceof es.gob.jmulticard.jse.provider.BadPasswordProviderException
            		) {
            			throw e;
                	}
                    LOGGER.warning("No se ha inicializado el KeyStore indicado: " + e); //$NON-NLS-1$
                    continue;
                }

                if (tmpCert == null) {
                    LOGGER.warning("El KeyStore no permite extraer el certificado publico para el alias especificado"); //$NON-NLS-1$
                    continue;
                }

                if (!showExpiredCertificates) {
                    try {
                        tmpCert.checkValidity();
                    }
                    catch (final Exception e) {
                        LOGGER.info(
                            "Se ocultara el certificado por no estar vigente: " + e //$NON-NLS-1$
                        );
                        aliassesByFriendlyName.remove(al);
                        continue;
                    }
                }

                if (checkPrivateKeys) {
                    try {
                    	if (!ksm.isKeyEntry(al)) {
                    		aliassesByFriendlyName.remove(al);
                            LOGGER.info(String.format(
                            		"Se ha ocultado el certificado con numero de serie '%s' (emitido por '%s') por no soportar operaciones de clave privada", //$NON-NLS-1$
                            		AOUtil.hexify(tmpCert.getSerialNumber().toByteArray(), false),
                            		AOUtil.getCN(tmpCert.getIssuerX500Principal().toString())));
                    	}
                    }
                    catch (final Exception e) {
                    	aliassesByFriendlyName.remove(al);
                    	LOGGER.info(String.format(
                    			"Se ha ocultado el certificado con numero de serie '%s' (emitido por '%s') por no poderse comprobar su clave privada: "  + e, //$NON-NLS-1$
                    			AOUtil.hexify(tmpCert.getSerialNumber().toByteArray(), false),
                    			AOUtil.getCN(tmpCert.getIssuerX500Principal().toString())));
                    }
                }
            }

            // Aplicamos los filtros de certificados
            if (certFilters != null && certFilters.size() > 0) {
            	// Tabla para los certificados que si hay que mostrar
            	final Map<String, String> filteredAliases = new Hashtable<>();
                for (final CertificateFilter cf : certFilters) {
                	final String[] certAliases = aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()]);
                	for (final String filteredAlias : cf.matches(certAliases, ksm)) {
                		filteredAliases.put(filteredAlias, aliassesByFriendlyName.get(filteredAlias));
                		aliassesByFriendlyName.remove(filteredAlias);
                	}
                }

                // Indicamos en el log que certficados no han superado los filtros
                if (LOGGER.isLoggable(Level.INFO)) {
                	for (final String alias : aliassesByFriendlyName.keySet().toArray(new String[0])) {
                		try {
                			tmpCert = ksm.getCertificate(alias);
                			LOGGER.info(String.format(
                					"Se ha ocultado el certificado con numero de serie '%s' (emitido por '%s') por no ajustarse a los filtros proporcionados", //$NON-NLS-1$
                					AOUtil.hexify(tmpCert.getSerialNumber().toByteArray(), false),
                					AOUtil.getCN(tmpCert.getIssuerX500Principal().toString())));
                		}
                		catch (final Exception e) {
                			LOGGER.info(String.format(
                					"Se ha ocultado el certificado con alias '%s' por no ajustarse a los filtros proporcionados: " + e, //$NON-NLS-1$
                					alias));
                		}
                	}
                }

            	aliassesByFriendlyName.clear();
            	aliassesByFriendlyName.putAll(filteredAliases);
            }

            for (final String alias : aliassesByFriendlyName.keySet().toArray(new String[0])) {
            	tmpCN = AOUtil.getCN(ksm.getCertificate(alias));

            	if (tmpCN != null) {
            		aliassesByFriendlyName.put(alias, tmpCN);
            	}
            	else {
            		// Hacemos un trim() antes de insertar, porque los alias de los
            		// certificados de las tarjetas CERES terminan con un '\r', que se
            		// ve como un caracter extrano
            		aliassesByFriendlyName.put(alias, alias.trim());
            	}
            }

        }

        else {

            // Vamos a ver si en vez de un alias nos llega un Principal X.500 completo,
            // en cuyo caso es muy largo como para mostrase y mostrariamos solo el
            // CN o una version truncada si no nos cuela como X.500.
            // En este bucle usamos la clave tanto como clave como valor porque
            // asi se ha inicializado el mapa.
            for (final String al : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
                final String value = aliassesByFriendlyName.get(al);
                if (value.length() > ALIAS_MAX_LENGTH) {
                    tmpCN = AOUtil.getCN(value);
                    if (tmpCN != null) {
                        aliassesByFriendlyName.put(al, tmpCN);
                    }
                    else {
                        aliassesByFriendlyName.put(al, value.substring(0, ALIAS_MAX_LENGTH - "...".length()) + "..."); //$NON-NLS-1$ //$NON-NLS-2$
                    }
                }
                else {
                    aliassesByFriendlyName.put(al, value.trim());
                }
            }
        }

        return aliassesByFriendlyName;
    }

	/**
	 * Obtiene el nombre corto (8+3) de un fichero o directorio indicado (con ruta).
	 * @param originalPath Ruta completa hacia el fichero o directorio que queremos pasar a nombre corto.
	 * @return Nombre corto del fichero o directorio con su ruta completa, la cadena originalmente indicada si no puede
	 *         obtenerse la versi&oacute;n corta o {@code null} si se proporcion&oacute; {@code null} en la entrada.
	 */
	public static String getWindowsShortName(final String originalPath) {
		if (originalPath == null || !Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return originalPath;
		}

		// Realizamos comprobaciones y modificaciones de seguridad para evitar inyeccion de
		// codigo
		final String path = originalPath.replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		File dir;
		try {
			dir = new File(path).getCanonicalFile();
		}catch (final Exception e) {
			LOGGER.log(Level.WARNING, "La ruta de fichero a acortar no es valida", e); //$NON-NLS-1$
			dir = null;
		}
		if (dir == null || !dir.exists()) {
			return originalPath;
		}

		try {
			final Process p = new ProcessBuilder(
				"cmd.exe", "/c", "for %f in (\"" + dir.getAbsolutePath() + "\") do @echo %~sf" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			).start();
			try (
				final InputStream is = p.getInputStream()
			) {
				return new String(AOUtil.getDataFromInputStream(is)).trim();
			}
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido obtener el nombre corto de " + LoggerUtil.getCleanUserHomePath(originalPath) + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return originalPath;
	}

	/** A&ntilde;ade los almacenes preferentes (por ahora DNIe 100% Java y CERES 100% Java) a un almac&eacute;n agredado.
	 * @param aksm Almac&eacute;n agredado al que se desea a&ntilde;adir los almacenes preferentes.
	 * @param parentComponent Componente padre para los di&aacute;logos de los almacenes preferentes
	 *                        (solicitud de PIN, confirmaci&oacute;n de firma, etc.).
	 * @return Devuelve {@code true} cuando se ha detectado alguno de los almacenes preferentes,
	 *         {@code false} en caso contrario.
	 * @throws AOCancelledOperationException Cuando se cancela la carga del almac&eacute;n. */
	public static boolean addPreferredKeyStoreManagers(final AggregatedKeyStoreManager aksm,
			                                           final Object parentComponent) {
		// Anadimos el controlador Java del DNIe SIEMPRE excepto que se indique lo contrario
		// mediante una variable de entorno de sistema operativo o una propiedad Java
		if (
			!Boolean.getBoolean(DISABLE_DNIE_NATIVE_DRIVER) &&
			!Boolean.parseBoolean(System.getenv(DISABLE_DNIE_NATIVE_DRIVER_ENV))
		) {
			try {
				aksm.addKeyStoreManager(getDnieKeyStoreManager(parentComponent));
				return true; // Si instancia esta tarjeta, no pruebo el resto. No deberia haber varias tarjetas insertadas
			}
			catch (final NoClassDefFoundError e) {
				LOGGER.info("No se puede inicializar el almacen de tipo DNIe: " + e); //$NON-NLS-1$
			}
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch (final Exception e) {
				LOGGER.info("No se ha encontrado un DNIe: " + e); //$NON-NLS-1$
			}
		}

		// Anadimos el controlador Java de CERES SIEMPRE a menos que se indique lo contrario
		// mediante una variable de entorno de sistema operativo o una propiedad Java
		if (
			!Boolean.getBoolean(DISABLE_CERES_NATIVE_DRIVER) &&
			!Boolean.parseBoolean(System.getenv(DISABLE_CERES_NATIVE_DRIVER_ENV))
		) {
			// Tarjeta CERES 430
			try {
				aksm.addKeyStoreManager(getCeres430KeyStoreManager(parentComponent));
				return true; // Si instancia esta tarjeta, no pruebo el resto. No deberia haber varias tarjetas insertadas
			}
            catch (final NoClassDefFoundError e) {
                LOGGER.info("No se puede inicializar el almacen de tipo CERES 4.30 o superior: " + e); //$NON-NLS-1$
            }
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch (final Exception e) {
				LOGGER.info("No se ha encontrado una tarjeta CERES 4.30 o superior: " + e); //$NON-NLS-1$
			}

			// Otras tarjetas CERES
			try {
				aksm.addKeyStoreManager(getCeresKeyStoreManager(parentComponent));
				return true; // Si instancia esta tarjeta, no pruebo el resto. No deberia haber varias tarjetas insertadas
			}
            catch (final NoClassDefFoundError e) {
                LOGGER.info("No se puede inicializar el almacen de tipo CERES (otras): " + e); //$NON-NLS-1$
            }
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch (final Exception ex) {
				LOGGER.info("No se ha encontrado una tarjeta CERES: " + ex); //$NON-NLS-1$
			}
		}

		// Anadimos el controlador Java de G&D SmartCafe SOLO si se ha indicado asi
		// mediante una variable de entorno de sistema operativo o
		// una propiedad Java
		if (
			Boolean.getBoolean(ENABLE_GYDSC_NATIVE_DRIVER) ||
			Boolean.parseBoolean(System.getenv(ENABLE_GYDSC_NATIVE_DRIVER_ENV))
		) {
			try {
				aksm.addKeyStoreManager(getSmartCafeKeyStoreManager(parentComponent));
				return true; // Si instancia SmartCafe no pruebo otras tarjetas, no deberia haber varias tarjetas instaladas
			}
            catch (final NoClassDefFoundError e) {
                LOGGER.info("No se puede inicializar el almacen de tipo G&D SmartCafe: " + e); //$NON-NLS-1$
            }
			catch (final AOCancelledOperationException e) {
				throw e;
			}
			catch (final Exception ex) {
				LOGGER.info("No se ha encontrado una tarjeta G&D SmartCafe: " + ex); //$NON-NLS-1$
			}
		}

		return false;
	}

	private static AOKeyStoreManager getDnieKeyStoreManager(final Object parentComponent) throws AOKeystoreAlternativeException, IOException {
		final AOKeyStoreManager tmpKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.DNIEJAVA,
			null,
			null,
			null,
			parentComponent
		);
		LOGGER.info("El DNIe 100% Java ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
		tmpKsm.setPreferred(true);
		return tmpKsm;
	}

	private static AOKeyStoreManager getCeres430KeyStoreManager(final Object parentComponent) throws AOKeystoreAlternativeException, IOException {
		final AOKeyStoreManager tmpKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.CERES_430, // Store
			null,             // Lib (null)
			null,             // Description (null)
			null,             // PasswordCallback (no hay en la carga, hay en la firma)
			parentComponent   // Parent
		);
		LOGGER.info("La tarjeta CERES 430 ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
		tmpKsm.setPreferred(true);
		return tmpKsm;
	}

	private static AOKeyStoreManager getCeresKeyStoreManager(final Object parentComponent) throws AOKeystoreAlternativeException, IOException {
		final AOKeyStoreManager tmpKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.CERES, // Store
			null,             // Lib (null)
			null,             // Description (null)
			null,             // PasswordCallback (no hay en la carga, hay en la firma)
			parentComponent   // Parent
		);
		LOGGER.info("La tarjeta CERES ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
		tmpKsm.setPreferred(true);
		return tmpKsm;
	}

	private static AOKeyStoreManager getSmartCafeKeyStoreManager(final Object parentComponent) throws AOKeystoreAlternativeException, IOException {
		final AOKeyStoreManager tmpKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.SMARTCAFE, // Store
			null,                 // Lib (null)
			null,                 // Description (null)
			null,                 // PasswordCallback (no hay en la carga, hay en la firma)
			parentComponent       // Parent
		);
		LOGGER.info("La tarjeta SmartCafe ha podido inicializarse, se anadiran sus entradas"); //$NON-NLS-1$
		tmpKsm.setPreferred(true);
		return tmpKsm;
	}

    /** Obtiene un almac&eacute;n de claves agregando un gestor de <i>callbacks</i> gen&eacute;rico.
     * @param ks Tipo de almac&eacute;n a obtener.
     * @param pssCallBack <i>PasswordCallback</i> para solilcitar la contrase&ntilde;a al usuario.
     * @param provider Proveedor de <code>KeyStore</code>.
     * @param parentComponent Componente padre para la modalidad.
     * @return Almac&eacute;n de claves con un gestor de <i>callbacks</i> gen&eacute;rico instalado.
     * @throws KeyStoreException Si no se puede obtener el almac&eacute;n de claves. */
    public static KeyStore getKeyStoreWithPasswordCallbackHandler(final AOKeyStore ks,
    		                                                      final PasswordCallback pssCallBack,
                                                                  final Provider provider,
                                                                  final Object parentComponent) throws KeyStoreException {

    	final PasswordCallbackHandler handler = new PasswordCallbackHandler(parentComponent, pssCallBack);
    	final KeyStore.CallbackHandlerProtection chp = new KeyStore.CallbackHandlerProtection(handler);

    	final KeyStore.Builder builder = KeyStore.Builder.newInstance(
			ks.getProviderName(),
			provider,
			chp
		);

    	try {
    		return builder.getKeyStore();
    	}
    	catch(final KeyStoreException e) {
    		if (handler.isCancelled()) {
    			LOGGER.warning("Se ha detectado la cancelacion del dialogo de PIN"); //$NON-NLS-1$
    			throw new AOCancelledOperationException("Se cancelo el dialogo de insercion de PIN"); //$NON-NLS-1$
    		}

    		// Si identificamos un PIN incorrecto, lo notificamos al usuario y volvemos a
    		// intentar cargar el almacen
    		if (e.getCause() != null && e.getCause().getCause() != null &&
    				e.getCause().getCause() instanceof LoginException) {
    			final Throwable pkcs11Exception = e.getCause().getCause().getCause();
    			boolean ksLocked = false;
    			String msg = KeyStoreMessages.getString("KeyStoreUtilities.5"); //$NON-NLS-1$
    			if (pkcs11Exception != null) {
        			if (PIN_ERROR_LOCKED.equals(pkcs11Exception.getMessage())) {
        				msg = KeyStoreMessages.getString("KeyStoreUtilities.7"); //$NON-NLS-1$
        				ksLocked = true;
        			}
        			else if (PIN_ERROR_WRONG_LENGTH.equals(pkcs11Exception.getMessage())) {
        				msg = KeyStoreMessages.getString("KeyStoreUtilities.8"); //$NON-NLS-1$
        			}
        			else if (PIN_ERROR_INCORRECT.equals(pkcs11Exception.getMessage())) {
        				msg = KeyStoreMessages.getString("KeyStoreUtilities.5"); //$NON-NLS-1$
        			}
    			}
    			AOUIFactory.showMessageDialog(
    					parentComponent,
    					msg,
    					KeyStoreMessages.getString("KeyStoreUtilities.6"), //$NON-NLS-1$
    					AOUIFactory.ERROR_MESSAGE);

    			// Si el almacen no se encuentra bloqueado, lo seguimos intentando
    			if (!ksLocked) {
					return getKeyStoreWithPasswordCallbackHandler(ks, pssCallBack, provider, parentComponent);
    			}
    		}
    		throw e;
    	}
    }

    /** Busca un fichero (o una serie de ficheros) en el <i>LIBRARY PATH</i> del sistema. Deja
     * de buscar en la primera ocurrencia.
     * @param files Ficheros a buscar en el <i>LIBRARY PATH</i>.
     * @return Ruta completa del fichero encontrado en el <i>LIBRARY PATH</i> o <code>null</code> si no se encontr&oacute; nada */
    public static String searchPathForFile(final String[] files) {
        if (files == null || files.length < 1) {
            return null;
        }

        // Si existe el primero con el PATH completo lo devolvemos sin mas
        if (new File(files[0]).exists()) {
            return files[0];
        }

        final StringTokenizer st = new StringTokenizer(Platform.getJavaLibraryPath(), File.pathSeparator);
        String libPath;
        while (st.hasMoreTokens()) {
            libPath = st.nextToken();
            if (!libPath.endsWith(File.separator)) {
                libPath = libPath + File.separator;
            }
            File tmpFile;
            for (final String f : files) {
                tmpFile = new File(libPath, f);
                if (tmpFile.exists() && !tmpFile.isDirectory()) {
                    return libPath + f;
                }
            }
        }
        return null;
    }

    /** Manejador para la gesti&oacute;n de la contrase&ntilde;a (y otros di&aacute;logos)
     * de un almac&eacute;n de claves. */
    public static class PasswordCallbackHandler implements CallbackHandler {

    	private final Object parentComponent;
    	private final PasswordCallback pssCallBack;
    	private boolean cancelled = false;

    	/** Construye un manejador para la gesti&oacute;n de la contrase&ntilde;a (y otros di&aacute;logos)
         * de un almac&eacute;n de claves.
    	 * @param parentComponent Componente padre para la modalidad.
    	 * @param pssCallBack <code>PasswordCallback</code> para solicitar la contrase&ntilde;a. */
    	public PasswordCallbackHandler(final Object parentComponent, final PasswordCallback pssCallBack) {
    		this.parentComponent = parentComponent;
    		this.pssCallBack = pssCallBack;
		}

    	@Override
    	public void handle(final Callback[] cbs) throws UnsupportedCallbackException {
    		for (final Callback callback : cbs) {
    			if (callback instanceof PasswordCallback) {
    				try {
    					((PasswordCallback) callback).setPassword(this.pssCallBack.getPassword());
    				}
    				catch (final AOCancelledOperationException e) {
    					// Al no establecer una contrasena, la carga del almacen lanzara
    					// un error generico. Cuando lo capturemos, comprobaremos si esta
    					// marcado que se cancelo el dialogo para distinguir si este es
    					// el motivo de ese error.
    					this.cancelled = true;
    				}
    			}
    			else if (callback instanceof TextOutputCallback) {
    				final TextOutputCallback toc = (TextOutputCallback)callback;
    				switch (toc.getMessageType()) {
    				case TextOutputCallback.INFORMATION:
    					LOGGER.info("Informacion del dispositivo criptografico: " + toc.getMessage()); //$NON-NLS-1$
    					AOUIFactory.showMessageDialog(
							this.parentComponent,
							toc.getMessage(),
							KeyStoreMessages.getString("KeyStoreUtilities.0"), //$NON-NLS-1$
							AOUIFactory.INFORMATION_MESSAGE
						);
    					break;
    				case TextOutputCallback.ERROR:
    					LOGGER.severe("Informacion del dispositivo criptografico: " + toc.getMessage()); //$NON-NLS-1$
    					AOUIFactory.showMessageDialog(
							this.parentComponent,
							toc.getMessage(),
							KeyStoreMessages.getString("KeyStoreUtilities.1"), //$NON-NLS-1$
							AOUIFactory.ERROR_MESSAGE
						);
    					break;
    				case TextOutputCallback.WARNING:
    					LOGGER.warning("Informacion del dispositivo criptografico: " + toc.getMessage()); //$NON-NLS-1$
    					AOUIFactory.showMessageDialog(
							this.parentComponent,
							toc.getMessage(),
							KeyStoreMessages.getString("KeyStoreUtilities.2"), //$NON-NLS-1$
							AOUIFactory.WARNING_MESSAGE
						);
    					break;
    				default:
    					LOGGER.warning(
							"Recibida informacion del dispositivo criptografico en un formato desconocido: " + toc.getMessageType() //$NON-NLS-1$
						);
    				}
    			}
    			else if (callback instanceof NameCallback) {
    				final Object name = AOUIFactory.showInputDialog(
						this.parentComponent,
						KeyStoreMessages.getString("KeyStoreUtilities.3"), //$NON-NLS-1$
						KeyStoreMessages.getString("KeyStoreUtilities.4"), //$NON-NLS-1$
						AOUIFactory.WARNING_MESSAGE,
						null,
						null,
    						null
					);
    				if (name != null) {
    					((NameCallback)callback).setName(name.toString());
    				}
    				throw new UnsupportedCallbackException(
						callback,
						"No se soporta la solicitud de nombre de usuario para dispositivos criptograficos" //$NON-NLS-1$
					);
    			}
    			else {
    				throw new UnsupportedCallbackException(
						callback,
						"Recibido tipo de callback desconocido: " + callback.getClass().getName() //$NON-NLS-1$
					);
    			}
    		}
    	}

    	/** Indica si se cancel&oacute; el di&aacute;logo de inserci&oacute;n de PIN.
    	 * @return {@code true} si se cancel&oacute; el di&aacute;logo de
    	 *         inserci&oacute;n de PIN, {@code false} en caso contrario. */
		public boolean isCancelled() {
			return this.cancelled;
		}
    }
}
