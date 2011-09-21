package es.gob.afirma.standalone.ui;

import java.awt.FileDialog;
import java.awt.Frame;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.OutputStream;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.Messages;

/**
 * Utilidades para el tratamiento de ficheros (apertura, guardado, etc.).
 */
public final class FileUIManager {

    /**
     * Muestra un di&aacute;logo para el guardado de datos y los almacena en el
     * fichero seleccionado.
     * @param data Datos que se desean guardar.
     * @param exts Posibles extensiones que asignar al fichero.
     * @param currentDir Directorio actual.
     * @return Fichero guardado. Si no se almacen&oacute; se devuelve {@code null}.
     */
    static File saveFile(final Frame parent, final byte[] data, final File currentDir, final String[] exts, final String description, final String title) {

        String newFileName = null;
        if (Platform.OS.MACOSX.equals(Platform.getOS()) || Platform.OS.WINDOWS.equals(Platform.getOS())) {
            final FileDialog fd = new FileDialog(parent, title, FileDialog.SAVE);
            if (currentDir != null) {
                fd.setDirectory(currentDir.getAbsolutePath());
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
            if (fd.getFile() != null) {
                newFileName = fd.getDirectory() + fd.getFile();
            }
        }
        else {
            final JFileChooser fc = new JFileChooser(currentDir);
            if (exts != null) {
                fc.setFileFilter(new FileFilter() {
                    @Override
                    public boolean accept(final File file) {
                        for (final String ext : exts) {
                            if (file.getName().endsWith(ext)) {
                                return true;
                            }
                        }
                        return false;
                    }

                    @Override
                    public String getDescription() {
                        return description;
                    }

                });
            }
            if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(parent)) {
                newFileName = fc.getSelectedFile().getAbsolutePath();
            }
        }

        if (newFileName == null) {
            return null;
        }

        // Anadimos la extension si es necesario
        if (exts != null) {
            boolean nameMissingExtension = true;
            for (final String ext : exts) {
                if (newFileName.toLowerCase().endsWith(ext)) {
                    nameMissingExtension = false;
                }
            }
            newFileName = newFileName + (nameMissingExtension ? exts[0] : ""); //$NON-NLS-1$
        }

        // Cuando se usa un FileDialog la confirmacion de sobreescritura la gestiona
        // el sistema operativo, pero en Mac hay comportamiento extrano con la extension

        final File outputFile = new File(newFileName);

        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            if (newFileName.toLowerCase().endsWith(".pdf") && outputFile.exists()) { //$NON-NLS-1$
                if (JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(
                        parent,
                        Messages.getString("SignPanel.84"), //$NON-NLS-1$
                        Messages.getString("SignPanel.19"), //$NON-NLS-1$
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.WARNING_MESSAGE)) {
                    return null;
                }
            }
        }

        OutputStream fos = null;
        OutputStream bos = null;
        try {
            fos = new FileOutputStream(outputFile);
            bos = new BufferedOutputStream(fos);
            bos.write(data);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                    "No se han podido almacenar los datos indicados: " + e //$NON-NLS-1$
            );
            UIUtils.showErrorMessage(
                    parent,
                    Messages.getString("SignPanel.89"), //$NON-NLS-1$
                    Messages.getString("SignPanel.25"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
        }
        finally {
            try { if (bos != null) {
                bos.flush();
            } } catch (final Exception e) {}
            try { if (fos != null) {
                fos.flush();
            } } catch (final Exception e) {}
            try { if (bos != null) {
                bos.close();
            } } catch (final Exception e) {}
            try { if (fos != null) {
                fos.close();
            } } catch (final Exception e) {}
        }

        return outputFile;
    }

    
    /**
     * Muestra un di&aacute;logo para la apertura de un fichero.
     * @param parent Componente padre, para la modalidad
     * @param exts Posibles extensiones que asignar al fichero.
     * @param currentDir Directorio actual.
     * @param title T&iacute;tulo del di&aacute;logo de apertura
     * @return Fichero seleccionado desde el di&aacute;logo
     */
    public static File openFile(final Frame parent, File currentDir, final String[] exts, final String title) {

        if (currentDir == null) {
            currentDir = new File("."); //$NON-NLS-1$
        }

        FilenameFilter filter = null;
        if (exts != null) {
            filter = new FilenameFilter() {
                @Override
                public boolean accept(final File dir, final String name) {
                    for (final String ext : exts) {
                        if (name.endsWith(ext)) {
                            return true;
                        }
                    }
                    return false;
                }
            };
        }

        if (Platform.OS.MACOSX.equals(Platform.getOS()) || Platform.OS.WINDOWS.equals(Platform.getOS())) {
            final FileDialog fd = new FileDialog(parent, title);
            fd.setDirectory(currentDir.getAbsolutePath());

            if (filter != null) {
                fd.setFilenameFilter(new FilenameFilter() {
                    @Override
                    public boolean accept(final File dir, final String name) {
                    	if (exts == null) {
                            return true;
                        }
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
            if (fd.getFile() == null) {
                return null;
            }
            return new File(fd.getDirectory(), fd.getFile());
        }

        final JFileChooser fc = new JFileChooser(currentDir);
        if (filter != null) {
            fc.setFileFilter(new FileFilter() {
                @Override public String getDescription() {
                    return null;
                }

                @Override public boolean accept(final File f) {
                	if (exts == null) {
                        return true;
                    }
                    for (final String ext : exts) {
                        if (f.getName().endsWith(ext)) {
                            return true;
                        }
                    }
                    return false;
                }
            });
        }
        if (JFileChooser.APPROVE_OPTION != fc.showOpenDialog(parent)) {
            return null;
        }
        return new File(fc.getSelectedFile().getAbsolutePath());

    }
}
