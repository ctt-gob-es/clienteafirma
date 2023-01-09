/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse;

import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.GenericFileFilter;

/** Gestor de componentes de interfaz gr&aacute;fico (tanto para Applet como para
 * aplicaci&oacute;n de escritorio) de la aplicaci&oacute;n.
 * Esta clase usa AWT para los di&aacute;logos de apertura y guardado de ficheros.
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

        // Este dialogo no soporta multiples filtros, asi que juntamos
        // las extensiones de todos para crear un filtro unico
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

        final File file;
        if (fd.getFile() != null) {
            file = new File(fd.getDirectory() + fd.getFile());
        }
        else {
        	throw new AOCancelledOperationException();
        }

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
			                  final String filename,
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

        // Habilitamos si corresponde el modo de seleccion multiple. Ya que solo esta disponible
        // en Java 7, lo hacemos por reflexion para evitar problemas de compilacion. Esto equivale
        // a la sentencia: "fd.setMultipleMode(multiSelect)"
        if (multiSelect) {
        	try {
        		final Method setMultipleModeMethod = FileDialog.class.getMethod("setMultipleMode", Boolean.TYPE); //$NON-NLS-1$
        		setMultipleModeMethod.invoke(fd, Boolean.valueOf(multiSelect));
        	}
        	catch (final Exception e) {
        		LOGGER.warning("No es posible utilizar la seleccion multiple de ficheros con los dialogos del sistema con versiones anteriores de Java 7, se utilizara Swing: " + e); //$NON-NLS-1$
        		return super.getLoadFiles(dialogTitle, currentDir, filename, extensions, description, selectDirectory, multiSelect, icon, parent);
        	}
        }

        if (filename != null) {
        	fd.setFile(filename);
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

        if (multiSelect) {
        	// getFiles() solo esta disponible en Java 7
        	File[] files;
        	try {
        		final Method getFilesMethod = FileDialog.class.getMethod("getFiles"); //$NON-NLS-1$
        		files = (File[]) getFilesMethod.invoke(fd);
        	}
        	catch (final Exception e) {
        		LOGGER.warning("Error de reflexion al recuperar la seleccion multiple del dialogo de carga, se devolvera un unico fichero: " + e); //$NON-NLS-1$
        		files = new File[] { new File(fd.getFile()) };
        	}
        	if (files == null) {
        		files = new File[0];
        	}

        	return files;
        }

        final File ret = new File(fd.getDirectory(), fd.getFile());

        System.setProperty("apple.awt.fileDialogForDirectories", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        put(PREFERENCE_DIRECTORY, fd.getDirectory());

        return new File[] { ret };
    }
}
