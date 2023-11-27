/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.DataFileInfoDialog.Options;

/**
 * Enlace para la apetura/guardado de un fichero.
 * @author Carlos Gamuci
 */
public final class ShowFileLinkAction {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final String text;
    private final byte[] data;
    private final Component parent;

   public ShowFileLinkAction(final String text, final byte[] data, final Component parent) {
        this.text = text;
        this.data = data;
        this.parent = parent;
    }

    void action() {

        if (this.data == null) {
            return;
        }

        // Extraemos informacion del fichero
        final DataFileInfo info = DataFileAnalizer.analize(this.data);

        // Mostramos un dialogo con la informacion del fichero. Si se conoce el tipo de
        // fichero, se permitira abrirlo. Si no, se permitira almacenarlo.
        final DataFileInfoDialog dialog = new DataFileInfoDialog(info, this.parent);
        final Options op = dialog.show();
        if (op == Options.SAVE) {
            saveDataFile(info, this.parent);
        }
        else if (op == Options.OPEN) {
        	openDataFile(info, this.parent);
        }
    }

    private static void openDataFile(final DataFileInfo info, final Component parent) {
    	File tmp;
		try {
			tmp = File.createTempFile("afirma", "." + info.getExtension()); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final IOException e) {
			LOGGER.info("No se pudo crear el temporal para abrir el fichero: " + e); //$NON-NLS-1$
			saveDataFile(info, parent);
			return;
		}
        try (
    		final OutputStream bos = new BufferedOutputStream(new FileOutputStream(tmp));
		) {
        	bos.write(info.getData());
        	Desktop.getDesktop().open(tmp);
            tmp.deleteOnExit();
        }
        catch(final Exception e) {
        	LOGGER.warning("Error intentado abrir el fichero: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                SimpleAfirmaMessages.getString("ShowFileLinkAction.2", info.getExtension()),  //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE,
                e
            );
        }
    }

    private static void saveDataFile(final DataFileInfo info, final Component parent) {
    	try {
    		AOUIFactory.getSaveDataToFile(
				info.getData(),
				SimpleAfirmaMessages.getString("ShowFileLinkAction.1"), //$NON-NLS-1$
				null,
				info.getExtension() != null ? SimpleAfirmaMessages.getString("ShowFileLinkAction.5") + "." + info.getExtension() : null, //$NON-NLS-1$ //$NON-NLS-2$
				Collections.singletonList(
						new GenericFileFilter(
								info.getExtension() != null ? new String[] { info.getExtension() } : null,
								info.getDescription()
						)
				),
				parent
				);
		}
        catch (final IOException e) {
			LOGGER.severe("No se ha podido guardar el fichero: " + e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				SimpleAfirmaMessages.getString("ShowFileLinkAction.3"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("ShowFileLinkAction.4"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
			);
		}
        catch (final AOCancelledOperationException e) {
			LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
		}
    }

    @Override
    public String toString() {
        return this.text;
    }

}
