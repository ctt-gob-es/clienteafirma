/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.so.macos.MacUtils;

/** Utilidades generales y de control del autoarranque de Autofirma en el inicio de Windows.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class DesktopUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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

	/**
	 * Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n.
	 */
	public static File getApplicationDirectory() {

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

	/** Recupera el DPI de la pantalla principal.
	 * @return DPI de la pantalla principal. */
	public static int getDPI() {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			// Intentamos primero la forma mas eficiente: consultar el registro de Windows directamente
			int dpi = getDpiFromRegistry();
			if (dpi > 0) {
				return dpi;
			}

			// Como segunda opcion, probamos con wmic
			dpi = getDpiFromWmic();
			if (dpi > 0) {
				return dpi;
			}

			// Como tercera opcion, probamos con PowerShell
			dpi = getDpiFromPowerShell();
			if (dpi > 0) {
				return dpi;
			}
		}
		return 0;
	}

	private static int getDpiFromRegistry() {
		int dpi = getDpiFromRegistryKey("HKCU\\Control Panel\\Desktop\\WindowMetrics", "AppliedDPI"); //$NON-NLS-1$ //$NON-NLS-2$
		if (dpi == 0) {
			dpi = getDpiFromRegistryKey("HKLM\\SYSTEM\\CurrentControlSet\\Hardware Profiles\\Current\\Software\\Fonts", "LogPixels"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return dpi;
	}

	private static int getDpiFromRegistryKey(final String key, final String value) {
		try {
			final Process p = new ProcessBuilder(
				"reg", "query", key, "/v", value //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			).start();
			try (
				final BufferedReader bufferedReader = new BoundedBufferedReader(
					new InputStreamReader(p.getInputStream())
				)
            ) {
				String line;
				while ((line = bufferedReader.readLine()) != null) {
					if (line.contains(value)) {
						final String[] tokens = line.trim().split("\\s+"); //$NON-NLS-1$
						if (tokens.length >= 3) {
							try {
								return Integer.decode(tokens[tokens.length - 1]).intValue();
							}
							catch (final Exception e) {
								// Ignoramos
							}
						}
					}
				}
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.FINE, "No se pudo obtener el DPI del registro de Windows (" + key + "\\" + value + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		return 0;
	}


	private static int getDpiFromWmic() {
		final String[] cmd = {"wmic", "desktopmonitor", "get", "PixelsPerXLogicalInch"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		final ProcessBuilder builder = new ProcessBuilder(cmd);
		try {
			final Process process = builder.start();
			process.waitFor();
			try (
					final BufferedReader bufferedReader = new BoundedBufferedReader(
							new InputStreamReader(process.getInputStream())
							)
            ) {
				String line;
				while ((line = bufferedReader.readLine()) != null) {
					try {
						final int dpi = Integer.parseInt(line.trim());
						if (dpi > 0) {
							return dpi;
						}
					}
					catch (final Exception e) {
						// La linea no se corresponde con un DPI, la ignoramos
					}
				}
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error obteniendo DPI con wmic: " + e); //$NON-NLS-1$
		}

		return 0;
	}

	private static int getDpiFromPowerShell() {
		try {
			final Process p = new ProcessBuilder(
				"powershell", "-NoProfile", "-Command", "Get-CimInstance Win32_DesktopMonitor | Select-Object -ExpandProperty PixelsPerXLogicalInch" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			).start();
			try (
				final BufferedReader bufferedReader = new BoundedBufferedReader(
					new InputStreamReader(p.getInputStream())
				)
            ) {
				String line;
				while ((line = bufferedReader.readLine()) != null) {
					try {
						final int dpi = Integer.parseInt(line.trim());
						if (dpi > 0) {
							return dpi;
						}
					}
					catch (final Exception e) {
						// Ignoramos
					}
				}
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.FINE, "No se pudo obtener el DPI con PowerShell: " + e); //$NON-NLS-1$
		}
		return 0;
	}

	/**
	 * Si es posible, devuelve el comando a ejecutar para arrancar la aplicaci&oacute;n.
	 * Si no se encuentra un modo de arrancar la nueva instancia de la aplicaci&oacute;n,
	 * no se hace nada.
	 * @return Comando de arranque.
	 */
	public static List<String> getResetApplicationCommand() {

		File currentFile;
		try {
			currentFile = new File(DesktopUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido identificar el fichero ejecutable", e); //$NON-NLS-1$
			return null;
		}

		return getCommand(currentFile);
	}

	/**
	 * Devuelve el comando necesario para ejecutar la aplicaci&oacute;n o {@code null}
	 * si no hay una forma efectiva de ejecutarla
	 * @param currentFile Fichero o directorio con la aplicaci&oacute;n.
	 * @return Par&aacute;meros para la ejecuci&oacute;n de la aplicaci&oacute;n.
	 */
	public static List<String> getCommand(final File currentFile) {

		// La aplicacion se ejecutan las clases Java. No va a poder ejecutarse sin las
		// dependencias, por lo que se omite
		if (currentFile.isDirectory()) {
			return null;
		}

		// La aplicacion se ejecuta desde un JAR
		List<String> command;
		if (currentFile.getName().toLowerCase().endsWith(".jar")) { //$NON-NLS-1$

			// Si ese JAR forma parte de un ejecutable macOS, usamos el ejecutable
			final File appMac = MacUtils.getMacApp(currentFile);
			if (appMac != null && appMac.isFile()) {
				command = new ArrayList<>();
				command.add(appMac.getAbsolutePath());
			}
			else {
				final String java = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				command = new ArrayList<>();
				command.add(java);
				command.add("-jar"); //$NON-NLS-1$
				command.add(currentFile.getPath());
			}
		}
		// La aplicacion es un ejecutable de Windows
		else if (currentFile.getName().toLowerCase().endsWith(".exe")) { //$NON-NLS-1$
			command = new ArrayList<>();
			command.add(currentFile.getPath());
		}
		// En cualquier otro caso, no reiniciamos
		else {
			command = null;
		}

		return command;
	}

}