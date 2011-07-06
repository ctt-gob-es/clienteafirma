/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.awt.Frame;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.AccessController;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.ui.AOUIManager;

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
            throw new NullPointerException("Los datos a guardar no pueden ser nulos");
        }
        dataToSave = data.clone();
        savingTarget = fileName;
        if (desc != null && !"".equals(desc)) {
            description = desc;
        }
        extensions = exts;
        parent = p;
        showDialogIfError = errorDialog;
    }

    public void run() {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                if (savingTarget == null || "".equals(savingTarget)) {
                    try {
                        savingTarget = AOUIManager.getSaveFileName(extensions, description, parent);
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").severe("El nombre de fichero para guardar los datos no es valido: " + e);
                        return null;
                    }
                    if (savingTarget == null) {
                        Logger.getLogger("es.gob.afirma").severe("No se establecio un nombre de fichero de salida");
                        return null;
                    }
                }

                // Aqui ya tenemos un nombre de salida
                OutputStream fos = null;
                try {
                    fos = new FileOutputStream(savingTarget);
                    fos.write(dataToSave);
                    fos.flush();
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe("No se pudieron almacenar los datos en disco: " + e);
                    if (showDialogIfError) {
                        JOptionPane.showMessageDialog(parent,
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
                            Logger.getLogger("es.gob.afirma")
                                  .warning("No se ha podido cerrar el fichero de salida, es posible que se no se pueda abrir hasta cerrar la aplicacion");
                        }
                    }
                }
                return null;
            }
        });
    }

}
