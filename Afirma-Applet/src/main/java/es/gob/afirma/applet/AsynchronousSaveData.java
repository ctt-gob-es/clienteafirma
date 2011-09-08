/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.applet;

import java.awt.Frame;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.applet.ui.UIDialogs;

/** Invocar preferentemente de la siguiente manera:<br>
 * <code>SwingUtilities.invokeLater(new AsynchronousSaveData(data, file, desc, exts, parent, true));</code> */
public final class AsynchronousSaveData implements Runnable {

    private final byte[] dataToSave;
    private String savingTarget;
    private final String[] extensions;
    private String description = "Firma digital";
    private final Frame parent;
    private final boolean showDialogIfError;

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
        this.extensions = exts;
        this.parent = p;
        this.showDialogIfError = errorDialog;
    }

    public void run() {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                if (AsynchronousSaveData.this.savingTarget == null || "".equals(AsynchronousSaveData.this.savingTarget)) { //$NON-NLS-1$
                    try {
                        AsynchronousSaveData.this.savingTarget = UIDialogs.getSaveFileName(extensions, description, parent);
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").severe("El nombre de fichero para guardar los datos no es valido: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        return null;
                    }
                    if (AsynchronousSaveData.this.savingTarget == null) {
                        Logger.getLogger("es.gob.afirma").severe("No se establecio un nombre de fichero de salida"); //$NON-NLS-1$ //$NON-NLS-2$
                        return null;
                    }
                }

                // Aqui ya tenemos un nombre de salida
                OutputStream fos = null;
                try {
                    fos = new FileOutputStream(AsynchronousSaveData.this.savingTarget);
                    fos.write(AsynchronousSaveData.this.dataToSave);
                    fos.flush();
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe("No se pudieron almacenar los datos en disco: " + e);  //$NON-NLS-1$//$NON-NLS-2$
                    if (AsynchronousSaveData.this.showDialogIfError) {
                        JOptionPane.showMessageDialog(AsynchronousSaveData.this.parent,
                                                      "Error al almacenar los datos en disco,\r\nlos datos no se han guardado.",
                                                      "Error",
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
