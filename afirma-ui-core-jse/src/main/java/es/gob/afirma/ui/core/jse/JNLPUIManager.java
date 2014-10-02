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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import javax.jnlp.FileContents;
import javax.jnlp.FileSaveService;
import javax.jnlp.ServiceManager;
import javax.jnlp.UnavailableServiceException;

import es.gob.afirma.core.AOCancelledOperationException;

/** Gestor de componentes de interfaz gr&aacute;fico (tanto para Applet como para
 * aplicaci&oacute;n de escritorio) de la aplicaci&oacute;n.
 * Esta clase usa AWT para los di&aacute;logos de apertura y guardado de ficheros.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class JNLPUIManager extends JSEUIManager {

	/** {@inheritDoc} */
    @Override
	public File saveDataToFile(final byte[] data,
							   final String dialogTitle,
							   final String currentDir,
							   final String selectedFile,
			                   final String[] exts,
			                   final String description,
			                   final Object parent) throws IOException {
    	final FileSaveService fos;
        try {
            fos = (FileSaveService)ServiceManager.lookup("javax.jnlp.FileSaveService"); //$NON-NLS-1$
        }
        catch (final UnavailableServiceException e) {
        	throw new IOException(
    			"No se ha podido mostrar el dialogo de apertura de ficheros: " + e, e //$NON-NLS-1$
			);
        }
        if (fos == null) {
        	throw new IOException(
    			"No se ha podido mostrar el dialogo de apertura de ficheros" //$NON-NLS-1$
			);
        }
        final FileContents fc = fos.saveFileDialog(
    		currentDir,
    		exts,
    		new ByteArrayInputStream(data),
    		selectedFile
		);
        if (fc == null) {
        	throw new AOCancelledOperationException();
        }

    	return null;
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
			                  final Object parent) {
    	throw new UnsupportedOperationException();
    }

}
