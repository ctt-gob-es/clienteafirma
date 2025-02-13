/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de la aplicaci&oacute;n. */
final class ConfiguratorLinux implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String ALTERNATIVE_APP_SUBDIR = ".afirma/Autofirma"; //$NON-NLS-1$

	private static final String UNINSTALL_SCRIPT_NAME = "uninstall.sh"; //$NON-NLS-1$
	private static final String INSTALL_SCRIPT_NAME = "script.sh"; //$NON-NLS-1$

    private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
    private static final String FILE_AUTOFIRMA_CERTIFICATE = "Autofirma_ROOT.cer"; //$NON-NLS-1$
    private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	private final boolean jnlpInstance;

    public ConfiguratorLinux(final boolean jnlpInstance) {
		this.jnlpInstance = jnlpInstance;
	}

    @Override
    public void configure(final Console window) throws IOException, GeneralSecurityException, HeadlessException {

        LOGGER.info(Messages.getString("ConfiguratorLinux.2")); //$NON-NLS-1$

        final File appDir = getApplicationDirectory(this.jnlpInstance);

        LOGGER.info(Messages.getString("ConfiguratorLinux.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

        // Obtenemos los directorios de los usuarios
		final String[] usersDirs = getSystemUsersHomes();

        if (!checkSSLKeyStoreGenerated(appDir)) {
            LOGGER.info(Messages.getString("ConfiguratorLinux.5")); //$NON-NLS-1$

            final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
                ConfiguratorUtil.CERT_ALIAS,
                KS_PASSWORD
            );

            LOGGER.info(Messages.getString("ConfiguratorLinux.11")); //$NON-NLS-1$

           //Generacion del certificado pfx
            ConfiguratorUtil.installFile(
        		certPack.getPkcs12(),
        		new File(appDir, KS_FILENAME)
    		);

            //Generacion del certificado raiz .cer
            ConfiguratorUtil.installFile(
        		certPack.getCaCertificate().getEncoded(),
        		new File(appDir, FILE_AUTOFIRMA_CERTIFICATE)
    		);

            try {
                LOGGER.info(Messages.getString("ConfiguratorLinux.13")); //$NON-NLS-1$
                ConfiguratorFirefoxLinux.createScriptsToSystemKeyStore(
            		appDir,
            		usersDirs,
            		new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
            		new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME)
        		);

                ConfiguratorFirefoxLinux.createScriptsToMozillaKeyStore(
            		appDir,
            		usersDirs,
                	new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
                	new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME)
            	);
              }
            catch(final MozillaProfileNotFoundException e) {
                LOGGER.warning(Messages.getString("ConfiguratorLinux.12")); //$NON-NLS-1$
            }
        }
        else {
            LOGGER.info(Messages.getString("ConfiguratorLinux.14")); //$NON-NLS-1$
        }

        LOGGER.info(Messages.getString("ConfiguratorLinux.8")); //$NON-NLS-1$

        // Si se necesita interfaz grafica, comprobamos que la JRE lo soporte
        if (isHeadlessJre()) {
        	LOGGER.warning("La JVM con la que se ha ejecutado el instalador no soporta interfaces graficas (headless). Esta JVM no es compatible con la interfaz grafica de Autofirma. Instale una JRE completa para poder ejecutarla."); //$NON-NLS-1$
        }
    }

    /** Comprueba si ya existe un almac&eacute;n de certificados generado.
     * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
     * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
    private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
        return new File(appConfigDir, KS_FILENAME).exists();
    }

	private static File getApplicationDirectory(final boolean jnlpDeployment) {

		// Devolver un directorio que utilizar como directorio de instalacion cuando
		// se realice un despliegue JNLP
		if (jnlpDeployment) {
			try {
				return getIntApplicationDirectory();
			} catch (final IOException e) {
				LOGGER.severe("No se encuentra ni ha podido generarse el directorio de aplicacion: " + e); //$NON-NLS-1$
			}
		}

		return ConfiguratorUtil.getApplicationDirectory();
	}

	/** Obtiene un directorio en el que almacenar los ficheros de la aplicaci&oacute;n.
	 * @return Directorio de aplicaci&oacute;n.
	 * @throws IOException EN cualquier error. */
	private static File getIntApplicationDirectory() throws IOException {
		final String userHome;
		try {
			userHome = System.getProperty("user.home"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido identificar el directorio del usuario para almacenar los ficheros de instalacion", e); //$NON-NLS-1$
		}
		if (userHome == null) {
			throw new IOException("No se encuentra definido el directorio del usuario"); //$NON-NLS-1$
		}
		final File appDir = new File(userHome, ALTERNATIVE_APP_SUBDIR);
		if (!appDir.isDirectory() && !appDir.mkdirs()) {
			throw new IOException("No ha podido crearse el directorio para los ficheros de aplicacion"); //$NON-NLS-1$
		}
		return appDir;
	}

	@Override
	public File getAplicationDirectory() {
		return getApplicationDirectory(false);
	}

	@Override
	public File getAlternativeApplicationDirectory() {
		final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
		return new File(userHome, ".afirma/Autofirma"); //$NON-NLS-1$
	}

    /** Obtiene los directorios de usuarios del sistema.
	 * @return Listado con todos directorios de los usuarios del sistema.
     * @throws IOException Cuando no se puede obtener el listado de directorios. */
	private static String[] getSystemUsersHomes() throws IOException {

        // Comando para sacar los usuarios del sistema
        final String[] command = new String[] {
				"cut", //$NON-NLS-1$
				"-d:", //$NON-NLS-1$
				"-f6", //$NON-NLS-1$
				"/etc/passwd" //$NON-NLS-1$
				};

		try {
			final Process process = new ProcessBuilder(command).start();

			String line;
			// arraylist con todos los directorios de usuario
			final List<String> usersDir = new ArrayList<>();
			try (
					final InputStream resIs = process.getInputStream();
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 256 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				while ((line = resReader.readLine()) != null) {
					if(line.toLowerCase().contains("home/") && !usersDir.contains(line)) { //$NON-NLS-1$
						usersDir.add(line);
					}
				}
			}
			return usersDir.toArray(new String[usersDir.size()]);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al obtener el listado de directorios de usuarios del sistema: " + e); //$NON-NLS-1$
			throw new IOException("No se pudo obtener el listado de directorios de usuarios del sistema", e); //$NON-NLS-1$
		}
	}

	@Override
    public void uninstall(final Console console, final PluginsManager pluginsManager) {
        // No es necesario hacer nada mas  alla de eliminar los elementos creados por la aplicacion
    	// porque el  proceso de desinstalacion de linux eliminara el directorio de aplicacion con
    	// todo su contenido.

    	// Listamos los plugins instalados
    	List<AfirmaPlugin> plugins = null;
    	try {
    		plugins = pluginsManager.getPluginsLoadedList();
    	}
    	catch (final Exception e) {
    		LOGGER.log(Level.WARNING, "No se pudo obtener el listado de plugins de Autofirma", e); //$NON-NLS-1$
    	}

    	// Desinstalamos los plugins instalados si los hubiese
    	if (plugins != null && !plugins.isEmpty()) {
    		LOGGER.info("Desinstalamos los plugins instalados"); //$NON-NLS-1$
    		for (final AfirmaPlugin plugin : plugins) {
    			try {
    				pluginsManager.uninstallPlugin(plugin);
    			} catch (final Exception e) {
    				LOGGER.log(Level.WARNING, "No se pudo desinstalar el plugin: " + plugin.getInfo().getName(), e); //$NON-NLS-1$
    			}
    		}
    	}

    	// Eliminamos si existe el directorio alternativo usado para el guardado de certificados
    	// SSL durante el proceso de instalacion
    	final File alternativeDir = getAlternativeApplicationDirectory();
    	if (alternativeDir.isDirectory()) {
    		try {
    			Files.walkFileTree(
    					alternativeDir.toPath(),
    					new HashSet<>(),
    					Integer.MAX_VALUE,
    					new SimpleFileVisitor<Path>() {
    						@Override
    						public FileVisitResult visitFile(final Path file, final BasicFileAttributes attr) {
    							try {
    								Files.delete(file);
    							}
    							catch (final Exception e) {
    								LOGGER.warning("No se pudo eliminar el fichero: " + LoggerUtil.getCleanUserHomePath(file.toAbsolutePath().toString())); //$NON-NLS-1$
    							}
    							return FileVisitResult.CONTINUE;
    						}
    						@Override
    						public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) {
    							try {
    								Files.delete(dir);
    							} catch (final IOException e) {
    								LOGGER.warning("No se pudo eliminar el directorio: " + LoggerUtil.getCleanUserHomePath(dir.toAbsolutePath().toString())); //$NON-NLS-1$
    							}
    							return FileVisitResult.CONTINUE;
    						}
    					});
    		}
    		catch (final Exception e) {
    			LOGGER.log(Level.WARNING, "No se ha podido eliminar por completo el directorio alternativo para el certificado SSL", e); //$NON-NLS-1$
    		}
    	}
    }

	/**
	 * Identifica si la JRE carece de las bibliotecas necesarias para la ejecuci&oacute;n de las
	 * interfaces gr&aacute;ficas de Autofirma.
	 * @return {@code true} si la JRE es headless, {@code false} en caso contrario.
	 */
	private static boolean isHeadlessJre() {
		boolean headless = false;
		try {
			GraphicsEnvironment.getLocalGraphicsEnvironment();
		}
		catch (final UnsatisfiedLinkError e) {
			headless = true;
		}
		return headless;
	}
}
