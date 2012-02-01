/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Frame;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

/** Invocar preferentemente de la siguiente manera:<br>
 * <code>SwingUtilities.invokeLater(new AsynchronousSaveData(data, file, desc, exts, parent, true));</code> */
public final class AsynchronousSaveData implements Runnable {

	private final byte[] dataToSave;

	byte[] getDataToSave() {
		return this.dataToSave;
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
     * @param data
     *        Datos a guardar
     * @param fileName
     *        Nombre del fichero destino
     * @param desc
     *        Descripci&oacute;n del fichero destino
     * @param exts
     *        Posibles extensiones para el fichero destino
     * @param p
     *        Componente padre para la modalidad
     * @param errorDialog
     *        <code>true</code> si queremos mostrar un di&aacute;logo
     *        gr&aacute;fico en caso de error, <code>false</code> en caso
     *        contrario */
    public AsynchronousSaveData(final byte[] data,
                                final String fileName,
                                final String desc,
                                final String[] exts,
                                final Frame p,
                                final boolean errorDialog) {
        if (data == null || data.length == 0) {
            throw new IllegalArgumentException("Los datos a guardar no pueden ser nulos"); //$NON-NLS-1$
        }
        this.dataToSave = data.clone();
        this.savingTarget = fileName;
        if (desc != null && !"".equals(desc)) { //$NON-NLS-1$
            this.description = desc;
        }
        this.extensions = exts == null ? null : exts.clone();
        this.parent = p;
        this.showDialogIfError = errorDialog;
    }

    public void run() {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                if (AsynchronousSaveData.this.getSavingTarget() == null || "".equals(AsynchronousSaveData.this.getSavingTarget())) { //$NON-NLS-1$
                    try {
                        AsynchronousSaveData.this.setSavingTarget(UIDialogs.getSaveFileName(
                    		AsynchronousSaveData.this.getExtensions(),
                    		AsynchronousSaveData.this.getDescription(),
                    		AsynchronousSaveData.this.getParent()
                		));
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").severe("El nombre de fichero para guardar los datos no es valido: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        return null;
                    }
                    if (AsynchronousSaveData.this.getSavingTarget() == null) {
                        Logger.getLogger("es.gob.afirma").severe("No se establecio un nombre de fichero de salida"); //$NON-NLS-1$ //$NON-NLS-2$
                        return null;
                    }
                }

                // Aqui ya tenemos un nombre de salida
                OutputStream fos = null;
                try {
                    fos = new FileOutputStream(AsynchronousSaveData.this.getSavingTarget());
                    fos.write(AsynchronousSaveData.this.getDataToSave());
                    fos.flush();
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe("No se pudieron almacenar los datos en disco: " + e);  //$NON-NLS-1$//$NON-NLS-2$
                    if (AsynchronousSaveData.this.getShowDialogIfError()) {
                        JOptionPane.showMessageDialog(AsynchronousSaveData.this.getParent(),
                                                      AppletMessages.getString("AsynchronousSaveData.1"), //$NON-NLS-1$
                                                      AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                }
                finally {
                    if (fos != null) {
                        try {
                            fos.close();
                        }
                        catch (final Exception e) {
                            Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
                                  .warning("No se ha podido cerrar el fichero de salida, es posible que se no se pueda abrir hasta cerrar la aplicacion"); //$NON-NLS-1$
                        }
                    }
                }
                return null;
            }
        });
    }

}
