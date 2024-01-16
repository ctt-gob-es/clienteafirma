/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse;

import java.awt.Component;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.core.ui.KeyStoreDialogManager;

/** Gestor de componentes de interfaz gr&aacute;fico (tanto para Applet como para
 * aplicaci&oacute;n de escritorio) de la aplicaci&oacute;n.
 * Esta clase usa AWT para los di&aacute;logos de apertura y guardado de ficheros.
 * Su principal uso es en macOS, en donde permite usar los di&aacute;logos del sistema.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AWTUIManager extends JSEUIManager {

	/** {@inheritDoc} */
    @Override
	public File saveDataToFile(final byte[] data,
							   final String dialogTitle,
							   final String currentDir,
							   final String selectedFile,
			                   final List<GenericFileFilter> filters,
			                   final Object parent) throws IOException {

    	return saveDataToFile(data, dialogTitle, currentDir, selectedFile, filters, null, parent);
    }

	/** {@inheritDoc} */
    @Override
	public File saveDataToFile(final byte[] data,
							   final String dialogTitle,
							   final String currentDir,
							   final String selectedFile,
			                   final List<GenericFileFilter> filters,
			                   final GenericFileFilter defaultFilter,
			                   final Object parent) throws IOException {

        final FileDialog fd;
        if (parent instanceof Frame) {
        	fd = new FileDialog(
                (Frame) parent, // Padre
                dialogTitle, // Titulo
                FileDialog.SAVE // Tipo
           );
        }
        else if (parent instanceof Dialog) {
        	fd = new FileDialog(
                (Dialog) parent, // Padre
                dialogTitle, // Titulo
                FileDialog.SAVE // Tipo
           );
        }
        else {
        	fd = new FileDialog(
                (Frame) null, // Padre
                dialogTitle, // Titulo
                FileDialog.SAVE // Tipo
           );
        }


        // En Windows, el metodo setSelectedFile determina tambien el directorio actual, asi que lo usamos cuando
        // se indica el nombre de fichero
        if (selectedFile != null && currentDir != null && Platform.OS.WINDOWS.equals(Platform.getOS())) {
        	fd.setFile(new File(currentDir, selectedFile).getAbsolutePath());
        }
        else {
            if (selectedFile != null) {
                fd.setFile(selectedFile);
            }
            if (currentDir == null) {
            	fd.setDirectory(get(PREFERENCE_DIRECTORY, currentDir));
            }
        }

        // En las versiones actuales de macOS, el dialogo de guardado no soporta filtros
        // y en las antiguas solo soporta uno, asi que por compatibilidad, lo establecemos.
        // Para hacerlo, juntamos las extensiones de todos para crear uno solo
    	final List<String> extensions = new ArrayList<>();
        if (filters != null) {
        	for (final GenericFileFilter gff : filters) {
        		final String[] fe = gff.getExtensions();
        		if (fe != null) {
        			extensions.addAll(Arrays.asList(fe));
        		}
        	}
        }

        final String[] exts = extensions.toArray(new String[extensions.size()]);
    	if (exts != null) {
            fd.setFilenameFilter(
        		new FilenameFilter() {
	                @Override
	                public boolean accept(final File dir, final String name) {
	                    for (final String ext : exts) {
	                        if (name.endsWith(ext)) {
	                            return true;
	                        }
	                    }
	                    return false;
	                }
	            }
    		);
        }

        fd.setVisible(true);

        if (fd.getFile() == null) {
        	throw new AOCancelledOperationException();
        }

		final File file = new File(fd.getDirectory(), fd.getFile());

        // Si se proporcionan datos, se guardan y se devuelve el fichero donde se ha hecho.
        // Si no se proporcionan datos, se devuelve el fichero seleccionado, permitiendo que
        // el guardado se haga externamente.
        if (data != null) {
	        try (
	    		final OutputStream fos = new FileOutputStream(file);
			) {
		        fos.write(data);
		        fos.flush();
	        }
        }

        return file;
    }

    /** {@inheritDoc} */
    @Override
	public File[] getLoadFiles(final String dialogTitle,
			                  final String currentDir,
			                  final String defaultFilename,
			                  final String[] extensions,
			                  final String description,
			                  final boolean selectDirectory,
			                  final boolean multiSelect,
			                  final Object icon,
			                  final Object parent) {
        final FileDialog fd = new FileDialog(parent instanceof Frame ? (Frame) parent : null, dialogTitle);
        fd.setMode(FileDialog.LOAD);

        if (currentDir == null) {
        	fd.setDirectory(get(PREFERENCE_DIRECTORY, currentDir));
        }

        if (multiSelect) {
        	fd.setMultipleMode(multiSelect);
       	}

        if (defaultFilename != null) {
        	fd.setFile(defaultFilename);
        }

        if (extensions != null && extensions.length > 0) {
        	// Los filtros de extension no funcionan en Windows
        	if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
        		String ext = extensions[0];
        		if (!ext.startsWith("*.")) { //$NON-NLS-1$
        			if (ext.startsWith(".")) { //$NON-NLS-1$
        				ext = "*" + ext; //$NON-NLS-1$
        			}
        			else {
        				ext = "*." + ext; //$NON-NLS-1$
        			}
        		}
        		fd.setFile(ext);
        	}
        	else {
	            fd.setFilenameFilter(new FilenameFilter() {
	                @Override
	                public boolean accept(final File dir, final String name) {
	                    for (final String ext : extensions) {
	                        if (name.endsWith(ext)) {
	                            return true;
	                        }
	                    }
	                    return false;
	                }
	            });
        	}
        }

        if (selectDirectory) {
        	fd.setFile("*."); //$NON-NLS-1$
        	System.setProperty("apple.awt.fileDialogForDirectories", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        fd.setVisible(true);
        if (fd.getFile() == null) {
            throw new AOCancelledOperationException();
        }

		File[] files;
		if (multiSelect) {
        	files = fd.getFiles();
        	if (files == null) {
        		files = new File[0];
        	}
        }
		else {
			final String filename = fd.getFile();
			files = filename != null
				? new File[] { new File(fd.getDirectory(), filename) }
				: new File[0];
		}

        System.setProperty("apple.awt.fileDialogForDirectories", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        put(PREFERENCE_DIRECTORY, fd.getDirectory());

        return files;
    }

    /** {@inheritDoc} */
    @Override
	public String showCertificateSelectionDialog(final Object parentComponent,
    												   final KeyStoreDialogManager ksdm) {

    	final Component parent = parentComponent instanceof Component ? (Component) parentComponent : null;

    	try {
    		final Class<?> csdClass = Class.forName(
				"es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog" //$NON-NLS-1$
    		);
    		final Constructor<?> csdConstructor = csdClass.getConstructor(
				Component.class,
				KeyStoreDialogManager.class
			);
    		final Object csd =  csdConstructor.newInstance(parent, ksdm);
    		final Method showDialogMethod = csdClass.getMethod("showDialog", Boolean.TYPE); //$NON-NLS-1$

    		// Si tenemos un componente padre sobre el que mostrarnos, no es necesario forzar que
    		// se muestre arriba el dialogo
    		final Object result = showDialogMethod.invoke(csd, Boolean.FALSE);
    		focusApplication();


    		return result instanceof String ? (String) result : null;
    	}
    	catch (final InvocationTargetException e) {
    		LOGGER.severe("Se genero un error en el dialogo de seleccion de certificados: " + e); //$NON-NLS-1$
    		if (e.getCause() instanceof RuntimeException) {
    			throw (RuntimeException) e.getCause();
    		}
    		throw new IllegalStateException(
				"No se encontraron certificados en el almacen o se produjo un error durante la extraccion del certificado seleccionado: " + e, e.getCause() //$NON-NLS-1$
			);
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se encuentra disponible el proyecto del interfaz grafico del dialogo de seleccion: " + e); //$NON-NLS-1$
    		throw new IllegalStateException(
				"No se encuentra disponible el proyecto del interfaz grafico del dialogo de seleccion u ocurrio un error durante su ejecucion: " + e, e //$NON-NLS-1$
			);
    	}
    }

	/** Coge el foco del sistema en macOS. En el resto de sistemas no hace nada. */
	private static void focusApplication() {
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final String scriptCode = "tell me to activate"; //$NON-NLS-1$
			try {
				runAppleScript(scriptCode);
			}
			catch (final Exception e) {
				LOGGER.warning("Fallo cogiendo el foco en macOS: " + e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Ejecuta el script.
	 * @return Texto devuelto por el script o {@code null} si no se devolvi&oacute; nada.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del script.
	 * @throws InterruptedException Cuando finaliza inesperadamente la ejecuci&oacute;n del script.
	 */
	private static String runAppleScript(final String script) throws IOException, InterruptedException {

		final List<String> params = new ArrayList<>();
		params.add("/usr/bin/osascript"); //$NON-NLS-1$
		params.add("-e"); //$NON-NLS-1$
		params.add(script);

		LOGGER.fine("Ejecutando apple script: " + script); //$NON-NLS-1$

		final ProcessBuilder processBuilder = new ProcessBuilder(params);
		final Process process = processBuilder.start();
		final int exitValue = process.waitFor();

		if (exitValue != 0) {
			byte[] errorOutput;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			LOGGER.warning("Salida de error: " + (errorOutput != null ? new String(errorOutput) : "")); //$NON-NLS-1$ //$NON-NLS-2$
			throw new IOException("La ejecucion del script devolvio el codigo de finalizacion: " + exitValue); //$NON-NLS-1$
		}

		byte[] output;
		try (final InputStream inputStream = process.getInputStream()) {
			output = AOUtil.getDataFromInputStream(inputStream);
		}
		return output != null ? new String(output, "utf-8") : null; //$NON-NLS-1$
	}
}
