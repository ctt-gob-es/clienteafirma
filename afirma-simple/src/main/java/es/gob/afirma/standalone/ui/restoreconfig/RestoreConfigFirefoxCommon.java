/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.mozilla.MozillaProfile;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.logging.Logger;


/**
 * Contiene la l&oacute;gica com&uacute;n para todos los sistemas que permite realizar las tareas de restauraci&oacute;n
 * asociadas al navegador Firefox.
 */
final class RestoreConfigFirefoxCommon {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final long NSS_PASSWORD_CHECK_TIMEOUT_MILLIS = 5000L;
	private static final long NSS_PASSWORD_CHECK_POLL_MILLIS = 100L;
	private static final int NSS_PASSWORD_CHECK_READ_BUFFER_SIZE = 256;
	private static final int NSS_PASSWORD_CHECK_MAX_OUTPUT_SIZE = 8192;

	private RestoreConfigFirefoxCommon() {
		// No instanciable
	}

	static boolean isNssDbPasswordProtected(final String certUtilPath, final File profileDir) {
		Process process = null;
		Thread outputReader = null;
		try {
			final boolean sqlDb = new File(profileDir, "pkcs11.txt").exists(); //$NON-NLS-1$
			final String profileReference = (sqlDb ? "sql:" : "") //$NON-NLS-1$ //$NON-NLS-2$
					+ profileDir.getAbsolutePath();
			final Process startedProcess = new ProcessBuilder(certUtilPath, "-K", "-d", profileReference) //$NON-NLS-1$ //$NON-NLS-2$
					.redirectErrorStream(true)
					.start();
			process = startedProcess;

			final StringBuilder processOutput = new StringBuilder();
			outputReader = new Thread(() -> {
                try (final Reader reader = new InputStreamReader(startedProcess.getInputStream())) {
                    final char[] buffer = new char[NSS_PASSWORD_CHECK_READ_BUFFER_SIZE];
                    int charsRead;
                    while ((charsRead = reader.read(buffer)) != -1) {
                        synchronized (processOutput) {
                            processOutput.append(buffer, 0, charsRead);
                            if (containsPasswordPrompt(processOutput)) {
                                return;
                            }
                            if (processOutput.length() > NSS_PASSWORD_CHECK_MAX_OUTPUT_SIZE) {
                                processOutput.delete(0, processOutput.length() - NSS_PASSWORD_CHECK_MAX_OUTPUT_SIZE);
                            }
                        }
                    }
                }
                catch (final IOException e) {
                    // Puede ocurrir cuando se cierra el flujo al finalizar el tiempo limite
                    LOGGER.fine("No se pudo leer la salida de CertUtil"); //$NON-NLS-1$
                }
            }, "certutil-output-reader"); //$NON-NLS-1$
			outputReader.setDaemon(true);
			outputReader.start();

			final long deadline = System.nanoTime() + NSS_PASSWORD_CHECK_TIMEOUT_MILLIS * 1000000L;
			boolean passwordProtected;
			while (outputReader.isAlive()) {
				synchronized (processOutput) {
					passwordProtected = containsPasswordPrompt(processOutput);
				}
				if (passwordProtected) {
					break;
				}

				final long remainingNanos = deadline - System.nanoTime();
				if (remainingNanos <= 0) {
					LOGGER.warning("Se agoto el tiempo de espera al comprobar la contrasena del almacen NSS"); //$NON-NLS-1$
					break;
				}
				try {
					final long remainingMillis = (remainingNanos + 999999L) / 1000000L;
					outputReader.join(Math.min(NSS_PASSWORD_CHECK_POLL_MILLIS, Math.max(1L, remainingMillis)));
				}
				catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					LOGGER.warning("Se interrumpio la comprobacion de la contrasena del almacen NSS"); //$NON-NLS-1$
					break;
				}
			}

			synchronized (processOutput) {
				passwordProtected = containsPasswordPrompt(processOutput);
			}
			return passwordProtected;
		} catch (final Exception e) {
			LOGGER.warning("No se pudo determinar si el almacen NSS esta protegido por contrasena: " + e); //$NON-NLS-1$
		}
		finally {
			if (process != null) {
				process.destroyForcibly();
				try {
					process.getInputStream().close();
				}
				catch (final IOException e) {
					LOGGER.fine("No se pudo cerrar la salida de CertUtil"); //$NON-NLS-1$
				}
			}
			if (outputReader != null && outputReader.isAlive()) {
				try {
					outputReader.join(NSS_PASSWORD_CHECK_POLL_MILLIS);
				}
				catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}
		return false;
	}

	private static boolean containsPasswordPrompt(final StringBuilder processOutput) {
		final String output = processOutput.toString();
		return output.contains("Enter Password") || output.toLowerCase(Locale.ENGLISH).contains("password"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static File writePasswordToTempFile(final File workingDir, final char[] password) throws IOException {

		// Creamos un fichero temporal con un nombre que no coincida con ningun otro
		int i = 0;
		File outFile;
		do {
			outFile = new File(workingDir, "temp-" + i++);
		} while (outFile.exists()); //$NON-NLS-1$

		// Creamos el fichero con la contrasena
		try (final RandomAccessFile raf = new RandomAccessFile(outFile, "rws")) { //$NON-NLS-1$
			for (char c : password) {
				raf.write(c);
			}
		} catch (final Exception e) {
			if (outFile.exists()) {
				outFile.delete();
			}
			throw new IOException("No se pudo preparar la contrasena maestra para pasarsela a CertUtil", e); //$NON-NLS-1$
		}

		return outFile;
	}

	static char[] showPasswordDialog(final String profileName, final boolean passwordFailed, final Component parent) throws AOCancelledOperationException {

		String message =
				(passwordFailed ? SimpleAfirmaMessages.getString("SimpleKeyStoreManager.14")  + " " : "") // Si la contrasena es incorrecta, se antepone el texto que lo notifica
				+ SimpleAfirmaMessages.getString("SimpleKeyStoreManager.0", profileName); //$NON-NLS-1$	// Texto que solicita la contrasena

		return es.gob.afirma.core.ui.AOUIFactory.getPassword(
				message,
				null,
				null,
				false,
				parent);
	}

	/**
	 * Comprueba que el directorio {@code rootDir} es un directorio de la
	 * ruta de {@code childFile}.
	 * @param rootDir Directorio ra&iacute;z.
	 * @param childFile Fichero/directorio hijo.
	 * @return {@code true} cuando el directorio forma parte de la ruta del fichero/directorio hijo,
	 * {@code false} en caso contrario.
	 * @throws IOException Cuando no se pueda canonizar alguno de los ficheros.
	 */
	static boolean isAncestorDir(final File rootDir, final File childFile) throws IOException {

		final File parent = rootDir.getCanonicalFile();
		File intermediateDir = childFile.getCanonicalFile();
		while (intermediateDir != null && !intermediateDir.equals(parent)) {
			intermediateDir = intermediateDir.getParentFile();
		}
		return intermediateDir != null;
	}

	/**
	 * Inicializa un perfil de Firefox para su uso con certutil.
	 * @param workingDir Directorio de trabajo en el que poder guardar ficheros.
	 * @param profile Perfil de Firefox a inicializar.
	 * @param certUtilPath Ruta de certutil.
	 * @param supportPassword Indica si el entorno es compatible con el uso de contrase&ntilde;a maestra para el perfil de Firefox.
	 * @param passwordFailed Indica si ya se ha intentado introducir la contrase&ntilde;a maestra y ha fallado.
	 * @param parentComponent Componente padre para los cuadros de di&aacute;logo.
	 */
	static void initProfile(final File workingDir, final MozillaProfile profile, String certUtilPath, boolean supportPassword, boolean passwordFailed, Component parentComponent) {

		boolean hasPassword = false;
		File passwordFile = null;
		if (isNssDbPasswordProtected(certUtilPath, profile.getProfileDir())) {
			hasPassword = true;
			if (supportPassword) {
				char[] password;
				try {
					password = showPasswordDialog(profile.getName(), passwordFailed, parentComponent);
				} catch (final AOCancelledOperationException e) {
					// Continua sin contrasena
					LOGGER.warning("El usuario cancelo la introduccion de la contrasena maestra para el perfil de Firefox " + profile.getName()); //$NON-NLS-1$
					password = null;
				} catch (Exception e) {
					LOGGER.warning("No se pudo obtener la contrasena del usuario para el perfil " + profile.getName()); //$NON-NLS-1$
					password = null;
				}

				// Si se indico una contrasena, la escribimos en un fichero temporal para que certutil pueda usarla (es el
				// mecanismo definido por el propio certutil)
				if (password != null) {
					try {
						passwordFile = writePasswordToTempFile(workingDir, password);
					} catch (IOException e) {
						LOGGER.severe("No se podra usar la contrasena maestra insertada: " + e); //$NON-NLS-1$
					}
				}
			}
		}
		profile.prepare(hasPassword, passwordFile);
	}
}
