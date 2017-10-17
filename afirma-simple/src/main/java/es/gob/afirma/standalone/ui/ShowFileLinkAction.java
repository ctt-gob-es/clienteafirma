/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Enlace para la apetura/guardado de un fichero.
 * @author Carlos Gamuci
 */
final class ShowFileLinkAction {

    private final String text;
    private final byte[] data;

    ShowFileLinkAction(final String text, final byte[] data) {
        this.text = text;
        this.data = data.clone();
    }

    void action() {

        if (this.data == null) {
            return;
        }

        final String ext = ShowFileLinkAction.getCommonDataExtension(this.data);

        // Si conocemos la extension, intentamos abrir el fichero. Si no, permitimos
        // guardarlo con la extension que se desee.
        if (ext != null) {
            try {
                final File tmp = File.createTempFile("afirma", "." + ext);   //$NON-NLS-1$//$NON-NLS-2$
                tmp.deleteOnExit();
                try (
            		final OutputStream bos = new BufferedOutputStream(new FileOutputStream(tmp));
        		) {
                	bos.write(this.data);
                }
                Desktop.getDesktop().open(tmp);
            }
            catch(final Exception e) {
            	AOUIFactory.showErrorMessage(
                    null,
                    SimpleAfirmaMessages.getString("ShowFileLinkAction.2") + " '" + ext + "'",  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
            }
        }
        else {
            try {
            	AOUIFactory.getSaveDataToFile(
        			this.data,
			        SimpleAfirmaMessages.getString("ShowFileLinkAction.1"), //$NON-NLS-1$
			        null,
			        null,
			        null,
			        null,
			        null
				);
			}
            catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido guardar el fichero: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				AOUIFactory.showErrorMessage(
					null,
					SimpleAfirmaMessages.getString("ShowFileLinkAction.3"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("ShowFileLinkAction.4"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			}
            catch (final AOCancelledOperationException e) {
				Logger.getLogger("es.gob.afirma").warning("Operacion cancelada por el usuario"); //$NON-NLS-1$ //$NON-NLS-2$
			}
        }
    }

    private static String getCommonDataExtension(final byte[] dat) {
    	final String ext = new MimeHelper(dat).getExtension();
        return ext == null || ext.length() == 0 ? null : ext;
    }

    @Override
    public String toString() {
        return this.text;
    }

}
