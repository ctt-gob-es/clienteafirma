package es.gob.afirma.ui.core.jse;

import java.awt.Component;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;

/**
 * Funciones de utilidad del di&aacute;logo de selecci&oacute;n de certificados.
 */
final class CertificateUtils {

    private CertificateUtils() {
        // No permitimos la instanciacion
    }

    private static final String CERTIFICATE_DEFAULT_EXTENSION = ".cer"; //$NON-NLS-1$

	/** Abre un certificado con la aplicaci&oacute;n por defecto del sistema. Si no
	 * puede hacerlo, permite que el usuario lo almacene en la ruta que desee.
	 * @param parent Componente padre sobre el que se muestran los di&aacute;logos.
	 * @param certificate Certificado que deseamos abrir. */
	static void openCert(final Component parent, final X509Certificate certificate) {

		// Tratamos de abrir el certificado en Java 6
		Class<?> desktopClass;
		try {
			desktopClass = AOUtil.classForName("java.awt.Desktop"); //$NON-NLS-1$
		}
		catch (final ClassNotFoundException e) {
			desktopClass = null;
		}

		if (desktopClass != null) {
			try {
				final File certFile = saveTemp(certificate.getEncoded(), CERTIFICATE_DEFAULT_EXTENSION);
				final Method getDesktopMethod = desktopClass.getDeclaredMethod("getDesktop()", (Class[]) null); //$NON-NLS-1$
				final Object desktopObject = getDesktopMethod.invoke(null, (Object[]) null);
				final Method openMethod = desktopClass.getDeclaredMethod("open", File.class); //$NON-NLS-1$
				openMethod.invoke(desktopObject, certFile);
				return;
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning("No ha sido posible abrir el certificado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		// En entornos Java 5 intentamos abrirlo manualmente en Windows
		if (Platform.getOS() == OS.WINDOWS) {
			try {
				final File certFile = saveTemp(certificate.getEncoded(), CERTIFICATE_DEFAULT_EXTENSION);
				new ProcessBuilder(
						new String[] {
								"cmd", "/C", "start", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								"\"" + JSEUIMessages.getString("CertificateUtils.0") + "\"", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								"\"" + certFile.getAbsolutePath() + "\""} //$NON-NLS-1$ //$NON-NLS-2$
				).start();
				return;
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning("No ha sido posible abrir el certificado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		// Si no podemos abrirlo, lo guardamos en disco
		try {
			final File savedfile = selectFileToSave(parent, JSEUIMessages.getString("CertificateUtils.1")); //$NON-NLS-1$
			saveFile(savedfile, certificate.getEncoded());
		}
		catch (final IOException e) {
			new JSEUIManager().showConfirmDialog(
				parent,
				JSEUIMessages.getString("CertificateUtils.2"), //$NON-NLS-1$
				JSEUIMessages.getString("CertificateUtils.3"), //$NON-NLS-1$
				JOptionPane.CLOSED_OPTION,
				JOptionPane.ERROR_MESSAGE
			);
		}
		catch (final CertificateEncodingException e) {
			new JSEUIManager().showConfirmDialog(
					parent,
					JSEUIMessages.getString(JSEUIMessages.getString("CertificateUtils.4")), //$NON-NLS-1$
					JSEUIMessages.getString("CertificateUtils.3"), //$NON-NLS-1$
					JOptionPane.CLOSED_OPTION,
					JOptionPane.ERROR_MESSAGE
				);
		}
	}

	/** Pregunta al usuario por un nombre de fichero para salvar datos en disco.
     * @param parent Componente padre sobre el que se muestran los di&aacute;logos.
     * @param title T&iacute;tulo del di&aacute;logo de guardado.
     * @return Nombre de fichero (con ruta) seleccionado por el usuario
     * @throws IOException Cuando se produzca un error durante la selecci&oacute;n del fichero.
     * @throws AOCancelledOperationException Cuando el usuario cancele la operaci&oacute;n. */
    private static File selectFileToSave(final Component parent, final String title) throws IOException {

    	final JFileChooser fc = new JFileChooser();
    	fc.setDialogTitle(title);
    	fc.setFileFilter(new FileFilter() {
    	    /** {@inheritDoc} */
			@Override
			public String getDescription() {
				return JSEUIMessages.getString("CertificateUtils.3"); //$NON-NLS-1$
			}

			/** {@inheritDoc} */
			@Override
			public boolean accept(final File f) {
				if (f.isDirectory()) {
					return true;
				}
				if (f.getName().toLowerCase().endsWith(CERTIFICATE_DEFAULT_EXTENSION)) {
					return true;
				}
				return false;
			}
		});
   		fc.setSelectedFile(new File(JSEUIMessages.getString("CertificateUtils.5") + CERTIFICATE_DEFAULT_EXTENSION));  //$NON-NLS-1$

    	boolean selectedFile = false;
        File finalFile = null;
        do {
            final int ret = fc.showSaveDialog(parent);
            if (ret == JFileChooser.CANCEL_OPTION) {
            	throw new AOCancelledOperationException();
            }
            if (ret == JFileChooser.ERROR_OPTION) {
            	throw new IOException();
            }
            final File tempFile = fc.getSelectedFile();
            if (tempFile.exists()) {
            	if (tempFile.isDirectory() || !tempFile.canWrite()) {
            		JOptionPane.showMessageDialog(parent,
            				JSEUIMessages.getString("CertificateUtils.6")  + tempFile.getAbsolutePath(), //$NON-NLS-1$
            				JSEUIMessages.getString("CertificateUtils.7"), //$NON-NLS-1$
            				JOptionPane.WARNING_MESSAGE);
            		continue;
            	}
            	final int resp =
            		JOptionPane.showConfirmDialog(parent,
            				JSEUIMessages.getString("CertificateUtils.8"), //tempFile.getAbsolutePath()) //$NON-NLS-1$
            				JSEUIMessages.getString("CertificateUtils.7"), //$NON-NLS-1$
            				JOptionPane.YES_NO_CANCEL_OPTION,
            				JOptionPane.QUESTION_MESSAGE);
            	if (resp == JOptionPane.YES_OPTION) { // Sobreescribir fichero
            		finalFile = tempFile;
            		selectedFile = true;
            	}
            	else if (resp == JOptionPane.NO_OPTION) { // Seleccionar fichero
            		continue;
            	}
            	else { // Cancelar operacion de guardado
            		throw new AOCancelledOperationException();
            	}
            }
            else {
            	finalFile = fc.getSelectedFile();
            	selectedFile = true;
            }
        } while (!selectedFile);

        return finalFile;
    }

    private static boolean saveFile(final File file, final byte[] dataToSave) throws IOException {
    	final FileOutputStream fos = new FileOutputStream(file);
    	fos.write(dataToSave);
    	try {
    		fos.close();
    	}
    	catch (final Exception e) {
    		/* No hacemos nada */
		}
    	return true;
    }

    private static File saveTemp(final byte[] data, final String suffix) throws IOException {
    	final File tempFile = File.createTempFile("afirma", suffix); //$NON-NLS-1$
    	tempFile.deleteOnExit();
    	if (saveFile(tempFile, data)) {
    		return tempFile;
    	}
    	return null;
    }
}
