/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.core.jse;

import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.Method;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;

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
			                   final String[] exts,
			                   final String description,
			                   final Object parent) throws IOException {

        final FileDialog fd = new FileDialog(
             parent instanceof Frame ? (Frame) parent : null, // Padre
             dialogTitle, // Titulo
             FileDialog.SAVE // Tipo
        );

        // En Windows, el metodo setSelectedFile determina tambien el directorio actual, asi que lo usamos cuando
        // se indica el nombre de fichero
        if (selectedFile != null && currentDir != null && Platform.OS.WINDOWS.equals(Platform.getOS())) {
        	fd.setFile(new File(currentDir, selectedFile).getAbsolutePath());
        }
        else {
            if (selectedFile != null) {
                fd.setFile(selectedFile);
            }
            if (currentDir != null) {
                fd.setDirectory(currentDir);
            }
        }

    	if (exts != null) {
            fd.setFilenameFilter(new FilenameFilter() {
                @Override
                public boolean accept(final File dir, final String name) {
                    for (final String ext : exts) {
                        if (name.endsWith(ext)) {
                            return true;
                        }
                    }
                    return false;
                }
            });
        }

        fd.setVisible(true);

        final File file;

        if (fd.getFile() != null) {
            file = new File(fd.getDirectory() + fd.getFile());
        }
        else {
        	throw new AOCancelledOperationException();
        }

    	final FileOutputStream fos = new FileOutputStream(file);
        fos.write(data);
        fos.flush();
        fos.close();

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

        if (currentDir != null) {
        	fd.setDirectory(currentDir);
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
        		LOGGER.warning("No es posible utilizar la seleccion multiple de ficheros con los dialogos del sistema con versiones anteriores de Java 7, se utilizara Swing"); //$NON-NLS-1$
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
        fd.setVisible(true);
        if (fd.getFile() == null) {
            throw new AOCancelledOperationException();
        }

        if (multiSelect && isJava7()) {
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
        return new File[] { new File(fd.getDirectory(), fd.getFile()) };
    }

	private static boolean isJava7() {
		return System.getProperty("java.version").startsWith("1.7"); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
