/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;

/** Utilidades generales y de control del autoarranque de Autofirma en el inicio de Windows.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class DesktopUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String REG_CMD = "reg"; //$NON-NLS-1$
	private static final String REG_KEY = "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"; //$NON-NLS-1$
	private static final String REG_VALUE = "Autofirma"; //$NON-NLS-1$
	private static final String REG_VALUE_OPT = "/v"; //$NON-NLS-1$

	private static final Image ICON = Toolkit.getDefaultToolkit().getImage(
		DesktopUtil.class.getResource("/resources/logo_cliente_24.png") //$NON-NLS-1$
	);

	private static List<Image> ICONS = null;



	/** Obtiene el icono por defecto para los di&aacute;logos gr&aacute;ficos.
	 * @return Icono por defecto para los di&aacute;logos gr&aacute;ficos. */
	public static Image getDefaultDialogsIcon() {
		return ICON;
	}

	/** Obtiene el icono por defecto para los di&aacute;logos gr&aacute;ficos.
	 * @return Icono por defecto para los di&aacute;logos gr&aacute;ficos. */
	public static List<Image> getIconImages() {

		if (ICONS == null) {
			ICONS = new ArrayList<>();
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_16.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_24.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_32.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_48.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_128.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_256.png"))); //$NON-NLS-1$
			ICONS.add(Toolkit.getDefaultToolkit().getImage(
					DesktopUtil.class.getResource("/resources/logo_cliente_512.png"))); //$NON-NLS-1$
		}
		return ICONS;
	}

	/** Indica si Autofirma est&aacute; instalado para ejecutarse al inicio de Windows o no.
	 * @return <code>true</code> si Autofirma est&aacute; instalado para ejecutarse al inicio de Windows,
	 *         <code>false</code> en caso contrario, si el sistema no es Windows o si no se puede determinar. */
	public static boolean getAutoStartEnabled() {
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return false;
		}
		try {
			final Process p = new ProcessBuilder(
				REG_CMD, "QUERY", REG_KEY, REG_VALUE_OPT, REG_VALUE //$NON-NLS-1$
			).start();
			final String res = new String(AOUtil.getDataFromInputStream(p.getInputStream())).trim();
			final String[] tokens = res.split(" "); //$NON-NLS-1$
			for (final String token : tokens) {
				if ("Autofirma".equals(token)) { //$NON-NLS-1$
					return true;
				}
			}
		}
		catch(final IOException e) {
			LOGGER.severe(
				"No se ha podido leer el registro de Windows para determinar el autoarranque de Autofirma: " + e //$NON-NLS-1$
			);
		}
		return false;
	}

	/** Establece si Autofirma debe instalarse para ejecutarse al inicio de Windows o no.
	 * No tiene efecto en sistemas no Windows.
	 * @param enable <code>true</code> para instalar Autofirma para ejecutarse al inicio de Windows,
	 *         <code>false</code> para desinstalarlo (no tiene efecto si no lo estaba).
	 * @throws IOException En caso de fallo en el proceso. */
	public static void setAutoStartEnabled(final boolean enable) throws IOException {
		if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return;
		}
		if (enable) {
			enableAutoStart();
		}
		else {
			disableAutoStart();
		}
	}


	private static void enableAutoStart() throws IOException {
		if (getAutoStartEnabled()) {
			return;
		}
		final Process p = new ProcessBuilder(
			REG_CMD, "ADD", REG_KEY, REG_VALUE_OPT, REG_VALUE, "/t", "REG_SZ", "/d", "c:\\windows\\system32\\winver.exe" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		).start();
		if (!getAutoStartEnabled()) {
			throw new IOException(
				"No se ha podido habilitar el autoarranque de Autofirma: " + new String(AOUtil.getDataFromInputStream(p.getErrorStream())) //$NON-NLS-1$
			);
		}
	}

	private static void disableAutoStart() throws IOException {
		if (!getAutoStartEnabled()) {
			return;
		}
		final Process p = new ProcessBuilder(
			REG_CMD, "DELETE", REG_KEY, REG_VALUE_OPT, REG_VALUE, "/f" //$NON-NLS-1$ //$NON-NLS-2$
		).start();
		if (getAutoStartEnabled()) {
			throw new IOException(
				"No se ha podido deshabilitar el autoarranque de Autofirma: " + new String(AOUtil.getDataFromInputStream(p.getErrorStream())) //$NON-NLS-1$
			);
		}
	}

	/**
	 * Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n.
	 */
	public static File getApplicationDirectory() {

		if (isJnlpDeployment()) {
			return getJNLPApplicationDirectory();
		}

		// Identificamos el directorio de instalacion
		try {
			return new File(
				DesktopUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
			).getParentFile();
		}
		catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}

		return null;
	}


	/**
	 * Obtiene el nombre del ejecutable de la operaci&oacute;n.
	 * @return Nombre del ejecutable de la aplicaci&oacute;n siempre y cuando no se haya ejecutado directamente
	 * la clase java.
	 */
	public static String getApplicationFilename() {
		String filename;
		final String path = CommandLineLauncher.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		if (path != null && !path.isEmpty()) {
			String decodedPath;
			try {
				decodedPath = URLDecoder.decode(path, StandardCharsets.UTF_8.name());
			} catch (final UnsupportedEncodingException e) {
				decodedPath = path;
			}
			filename = new File(decodedPath).getName();
		}
		else {
			filename = "Autofirma"; //$NON-NLS-1$
		}
		return filename;
	}

	/**
	 * Obtiene el directorio de aplicaci&oacute;n que corresponde cuando se
	 * ejecuta la aplicaci&oacute;n mediante un despliegue es JNLP.
	 * @return Directorio de aplicaci&oacute;n.
	 */
	public static File getJNLPApplicationDirectory() {
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			final File appDir = getWindowsAlternativeAppDir();
			if (appDir.isDirectory() || appDir.mkdirs()) {
				return appDir;
			}
		}
		else if (Platform.getOS() == Platform.OS.MACOSX) {
			final File appDir = getMacOsXAlternativeAppDir();
			if (appDir.isDirectory() || appDir.mkdirs()) {
				return appDir;
			}
		}
		return new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Windows.
	 * @return Directorio de instalaci&oacute;n.
	 */
	public static File getWindowsAlternativeAppDir() {
		final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
		return new File (commonDir, "Autofirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Linux.
	 * @return Directorio de instalaci&oacute;n.
	 */
	public static File getLinuxAlternativeAppDir() {
		final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
		return new File(userHome, ".afirma/Autofirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas macOS.
	 * @return Directorio de instalaci&oacute;n.
	 */
	public static File getMacOsXAlternativeAppDir() {
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		return new File (userDir, "Library/Application Support/Autofirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio alternativo de la aplicaci&oacute;n, en el que
	 * se pueden almacenar los logs y recursos externos.
	 * @return Directorio alternativo de la aplicaci&oacute;n o {@code null} si
	 * no ha podido determinarse.
	 */
	public static File getAlternativeDirectory() {

		File appDir = null;
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			appDir = getWindowsAlternativeAppDir();
		}
		else if (Platform.getOS() == Platform.OS.LINUX) {
			appDir = getLinuxAlternativeAppDir();
		}
		else if (Platform.getOS() == Platform.OS.MACOSX) {
			appDir = getMacOsXAlternativeAppDir();
		}
		return appDir;
	}

	/**
	 * Comprueba si estamos en un despliegue JNLP de la aplicaci&oacute;n.
	 * @return {@code true} si estamos en un despliegue JNLP, {@code false}
	 * en caso contrario.
	 */
	private static boolean isJnlpDeployment() {

		// Para comprobar si estamos en un despliegue JNLP sin crear una dependencia
		// con javaws, hacemos una llamada equivalente a:
		//     javax.jnlp.ServiceManager.lookup("javax.jnlp.ExtendedService");
		// Si falla la llamda, no estamos en un despliegue JNLP
		try {
			final Class<?> serviceManagerClass = Class.forName("javax.jnlp.ServiceManager"); //$NON-NLS-1$
			final Method lookupMethod = serviceManagerClass.getMethod("lookup", String.class); //$NON-NLS-1$
			lookupMethod.invoke(null, "javax.jnlp.ExtendedService"); //$NON-NLS-1$
		}
		catch (final Throwable e) {
			return false;
		}
		return true;
	}

	/** Recupera el DPI de la pantalla principal.
	 * @return DPI de la pantalla principal. */
	public static int getDPI() {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			final String[] cmd = {"wmic", "desktopmonitor", "get", "PixelsPerXLogicalInch"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			final ProcessBuilder builder = new ProcessBuilder(cmd);
			try {
				final Process process = builder.start();
				process.waitFor();
				try (
					final BufferedReader bufferedReader = new BoundedBufferedReader(
						new InputStreamReader(process.getInputStream())
					);
				) {
					String line;
					int dpi = 0;
					while ((line = bufferedReader.readLine()) != null) {
						try {
							dpi = Integer.parseInt(line.trim());
							break;
						}
						catch (final Exception e) {
							continue;
						}
		            }
		            return dpi;
				}
			}
			catch (final Exception e) {
				LOGGER.log(	Level.SEVERE, "Error obteniendo DPI: " + e); //$NON-NLS-1$
				return 0;
			}
		}
		return 0;
	}

	/** Recupera el n&uacute;mero de pantallas que tiene habilitadas el usuario.
	 * @return N&uacute;mero de pantallas.
	 * @throws HeadlessException Si el equipo no tiene pantalla. */
	public static int getDisplaysNumber() throws HeadlessException {
		return GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices().length;
	}

	/** Devuelve el fichero en su forma can&oacute;nica.
	 * @param file Fichero del cual obtener su forma can&oacute;nica.
	 * @return Fichero en su forma can&oacute;nica o el fichero de entrada si hay error.*/
	public static File getCanonicalFile(final File file) {
		try {
			return file.getCanonicalFile();
		}
		catch(final IOException e) {
			LOGGER.severe(
				"No se ha podido obtener el fichero canonico: " + e //$NON-NLS-1$
			);
			return file;
		}
	}
}
