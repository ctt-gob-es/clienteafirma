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
        if (fd.getFile() == null) {
        	throw new AOCancelledOperationException();
        }
		file = new File(fd.getDirectory() + fd.getFile());

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

        if (multiSelect) {
        	fd.setMultipleMode(multiSelect);
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
        	File[] files = fd.getFiles();
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
