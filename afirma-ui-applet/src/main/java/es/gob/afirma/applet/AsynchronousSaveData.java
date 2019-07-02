/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.util.Arrays;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;

/** Invocar preferentemente de la siguiente manera:<br>
 * <code>SwingUtilities.invokeLater(new AsynchronousSaveData(data, file, desc, exts, parent, true));</code> */
final class AsynchronousSaveData implements Runnable {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final byte[] dataToSave;

	byte[] getDataToSave() {
		return this.dataToSave;
	}

	private String dialogTitle = AppletMessages.getString("AsynchronousSaveData.2"); //$NON-NLS-1$

	String getDialogTitle() {
		return this.dialogTitle;
	}

	private String savingTarget;

	void setSavingTarget(final String target) {
		this.savingTarget = target;
	}

	String getSavingTarget() {
		return this.savingTarget;
	}

	private final String[] extensions;

	String[] getExtensions() {
		return this.extensions;
	}

	private String description = AppletMessages.getString("AsynchronousSaveData.0"); //$NON-NLS-1$

	String getDescription() {
		return this.description;
	}

	private final Frame parent;

	Frame getParent() {
		return this.parent;
	}

	private final boolean showDialogIfError;

	boolean getShowDialogIfError() {
		return this.showDialogIfError;
	}

    /** Crea una clase para el guardado as&iacute;ncrono de datos en disco.
     * @param data Datos a guardar
     * @param dialogTitle T&iacute;tulo de la ventana de di&aacute;logo.
     * @param fileName Nombre del fichero destino
     * @param desc Descripci&oacute;n del fichero destino
     * @param exts Posibles extensiones para el fichero destino
     * @param p Componente padre para la modalidad
     * @param errorDialog <code>true</code> si queremos mostrar un di&aacute;logo
     *                    gr&aacute;fico en caso de error, <code>false</code> en caso
     *                    contrario */
    AsynchronousSaveData(final byte[] data,
    							final String dialogTitle,
                                final String fileName,
                                final String desc,
                                final String[] exts,
                                final Frame p,
                                final boolean errorDialog) {
        if (data == null || data.length == 0) {
            throw new IllegalArgumentException("Los datos a guardar no pueden ser nulos"); //$NON-NLS-1$
        }
        this.dataToSave = data.clone();
        this.dialogTitle = dialogTitle;
        this.savingTarget = fileName;
        if (desc != null && !"".equals(desc)) { //$NON-NLS-1$
            this.description = desc;
        }
        this.extensions = exts == null ? null : exts.clone();
        this.parent = p;
        this.showDialogIfError = errorDialog;
    }

    @Override
	public void run() {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            @Override
			public Void run() {
                if (AsynchronousSaveData.this.getSavingTarget() == null || "".equals(AsynchronousSaveData.this.getSavingTarget())) { //$NON-NLS-1$
                    try {
                    	final String[] exts = AsynchronousSaveData.this.getExtensions();
                    	final File outputFile = AOUIFactory.getSaveDataToFile(
                    			AsynchronousSaveData.this.getDataToSave(),
                    			AsynchronousSaveData.this.getDialogTitle(),
                    			null,
                    			"*" + (exts == null || exts.length == 0 ? "" : "." + exts[0]), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    			Arrays.asList(new GenericFileFilter(
                    					exts,
                    					AsynchronousSaveData.this.getDescription())),
                    			AsynchronousSaveData.this.getParent()
            			);
                    	if (outputFile == null) {
                    		LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            return null;
                    	}
                    	AsynchronousSaveData.this.setSavingTarget(outputFile.getAbsolutePath());
                    }
                    catch (final AOCancelledOperationException e) {
                    	LOGGER.warning("El usuario cancelo la operacion de guardado: " + e); //$NON-NLS-1$
                        return null;
                    }
                    catch (final Exception e) {
                        LOGGER.severe("El nombre de fichero para guardar los datos no es valido: " + e); //$NON-NLS-1$
                        return null;
                    }
                    if (AsynchronousSaveData.this.getSavingTarget() == null) {
                    	LOGGER.severe("No se establecio un nombre de fichero de salida"); //$NON-NLS-1$
                        return null;
                    }
                }

                // Aqui ya tenemos un nombre de salida
                try {
                	final OutputStream fos = new FileOutputStream(AsynchronousSaveData.this.getSavingTarget());
                    fos.write(AsynchronousSaveData.this.getDataToSave());
                    fos.flush();
                    fos.close();
                }
                catch (final Exception e) {
                	LOGGER.severe("No se pudieron almacenar los datos en disco: " + e);  //$NON-NLS-1$
                    if (AsynchronousSaveData.this.getShowDialogIfError()) {
                    	AOUIFactory.showMessageDialog(
                			AsynchronousSaveData.this.getParent(),
                            AppletMessages.getString("AsynchronousSaveData.1"), //$NON-NLS-1$
                            AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
                            JOptionPane.ERROR_MESSAGE
                        );
                    }
                }
                return null;
            }
        });
    }

}
