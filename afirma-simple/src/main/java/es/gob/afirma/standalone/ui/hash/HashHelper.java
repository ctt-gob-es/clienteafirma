package es.gob.afirma.standalone.ui.hash;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;

/** Funciones para el acceso a las capacidades de creaci&oacute;n y verificaci&oacute;n de
 * huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HashHelper {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String DEFAULT_HASH_ALGORITHM = "SHA-256"; //$NON-NLS-1$
	private static final boolean DEFAULT_RECURSIVE = true;
	private static final boolean DEFAULT_USE_BASE64 = false;
	private static final boolean DEFAULT_COPY_TO_CLIPBOARD = true;
	private static final int SIZE_WAIT = 50000000; //Tamano en bytes

	/** Comprueba las huellas digitales del fichero de huella proporcionados mediante un
	 * interfaz gr&aacute;fico.
	 * @param fileName Nombre del fichero de huella. */
	public static void checkHashUI(final String fileName) {
		if (fileName == null) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.4"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		final File file = new File(fileName);
		if (!file.exists() || file.isDirectory()) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.5"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		if (!file.canRead()) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.3"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}

		// Leemos el fichero
		final byte[] inputBytes;
		try (
			InputStream fis = new FileInputStream(file);
			InputStream bis = new BufferedInputStream(fis);
		) {
			inputBytes = AOUtil.getDataFromInputStream(bis);
		}
		catch(final Exception e) {
			LOGGER.log(Level.SEVERE, "No se ha podido leer el fichero de huellas indicado", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		// Si es un XML debe ser un informe de huellas
		if (DataAnalizerUtil.isXML(inputBytes)) {
			// Preguntamos por el directorio de origen
			final File dataDir;
			try {
				dataDir = AOUIFactory.getLoadFiles(
					SimpleAfirmaMessages.getString("HashHelper.10"), //$NON-NLS-1$
					file.getParent(),
					null, // Filename
					null,
					null,
					true, // Seleccion de directorio
					false,
					AutoFirmaUtil.getDefaultDialogsIcon(),
					null
				)[0];
			}
			catch(final AOCancelledOperationException e) {
				// Operacion cancelada
				return;
			}
			try {
				// Se crea la ventana de espera.
				final CommonWaitDialog dialog = new CommonWaitDialog(
					null,
					SimpleAfirmaMessages.getString("CreateHashFiles.21"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CreateHashFiles.22") //$NON-NLS-1$
				);

				// Arrancamos el proceso en un hilo aparte
				final SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
					protected Void doInBackground() throws Exception {
						CheckHashFiles.setReportXML(new HashMap<String, List<String>>());
						CheckHashFiles.checkHashXML(
							Paths.get(dataDir.toURI()),
							file.getAbsolutePath()
						);
						return null;
					}

					@Override
					protected void done() {
						super.done();
						dialog.dispose();
					}
				};
				worker.execute();

				if (CreateHashFiles.getSize(dataDir) > SIZE_WAIT) {
					// Se muestra la ventana de espera
					dialog.setVisible(true);
				}

				worker.get();
				if (!(CheckHashFiles.getReportXML().containsKey("CheckHashDialog.5") || //$NON-NLS-1$
					CheckHashFiles.getReportXML().containsKey("CheckHashFiles.1") || //$NON-NLS-1$
					CheckHashFiles.getReportXML().containsKey("CheckHashFiles.10"))) { //$NON-NLS-1$
							AOUIFactory.showMessageDialog(
								null,
								SimpleAfirmaMessages.getString("CheckHashDialog.2"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("CheckHashDialog.3"), //$NON-NLS-1$
								JOptionPane.INFORMATION_MESSAGE
							);
				}
				else {
					AOUIFactory.showMessageDialog(
						null,
						SimpleAfirmaMessages.getString("CheckHashFiles.18"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CheckHashFiles.17"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE
					);
				}
				final String ext = SimpleAfirmaMessages.getString("CheckHashFiles.20"); //$NON-NLS-1$
				AOUIFactory.getSaveDataToFile(
					CheckHashFiles.generateXMLReport(
						CheckHashFiles.getReportXML(),
						CheckHashFiles.getAlgorithm(),
						CheckHashFiles.getIsRecursive()
					).getBytes(),
					SimpleAfirmaMessages.getString("CheckHashFiles.15"), //$NON-NLS-1$ ,,,
					null,
					new java.io.File(SimpleAfirmaMessages.getString("CheckHashFiles.16")).getName() + ext, //$NON-NLS-1$
					new String[] { ext },
					SimpleAfirmaMessages.getString("CheckHashFiles.11") + " (*" + ext + ")", //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
					null
				);
			}
			catch (final OutOfMemoryError ooe) {
				AOUIFactory.showErrorMessage(
					null,
					SimpleAfirmaMessages.getString("CreateHashFiles.2"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				LOGGER.log(
					Level.SEVERE,
					"Fichero demasiado grande: " + ooe //$NON-NLS-1$
				);
				return;
			}
			catch (final AOCancelledOperationException ex) {
				// Operacion cancelada por el usuario
				return;
			}
			catch (final Exception ex) {
				LOGGER.log(
					Level.SEVERE,
					"Error comprobando las huellas digitales del directorio '" +  //$NON-NLS-1$
					dataDir.getAbsolutePath()  + " con el fichero: " +  //$NON-NLS-1$
					file.getAbsolutePath() + ex
				);
				AOUIFactory.showErrorMessage(
					null,
					SimpleAfirmaMessages.getString("CheckHashDialog.6"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CheckHashDialog.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return;
			}
		}
		// Si no debe ser un fichero de huella simple
		else {
			// Preguntamos por el fichero de datos
			final File dataFile;
			try {
				dataFile = AOUIFactory.getLoadFiles(
					SimpleAfirmaMessages.getString("HashHelper.7"), //$NON-NLS-1$
					file.getParent(),
					null, // Filename
					null,
					null,
					false,
					false,
					AutoFirmaUtil.getDefaultDialogsIcon(),
					null
				)[0];
			}
			catch(final AOCancelledOperationException e) {
				// Operacion cancelada
				return;
			}
			try {
				if (CheckHashDialog.checkHash(file.getAbsolutePath(), dataFile.getAbsolutePath())) {
					AOUIFactory.showMessageDialog(
						null,
						SimpleAfirmaMessages.getString("HashHelper.11"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("HashHelper.12"), //$NON-NLS-1$
						JOptionPane.INFORMATION_MESSAGE
					);
				}
				else {
					AOUIFactory.showMessageDialog(
						null,
						SimpleAfirmaMessages.getString("HashHelper.13"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("HashHelper.14"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE
					);
				}
			}
			catch (final Exception e) {
				LOGGER.log(
					Level.SEVERE,
					"Error comprobando la huella digital de fichero '" + dataFile.getAbsolutePath() + "' con la huella: " + file.getAbsolutePath(), //$NON-NLS-1$ //$NON-NLS-2$
					e
				);
				AOUIFactory.showErrorMessage(
					null,
					SimpleAfirmaMessages.getString("HashHelper.8"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return;
			}

		}
	}

	/** Crea las huellas digitales del fichero o directorio proporcionados mediante un
	 * interfaz gr&aacute;fico.
	 * @param fileName Nombre del fichero o del directorio. */
	public static void createHashUI(final String fileName) {
		if (fileName == null) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.0"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		final File file = new File(fileName);
		if (!file.exists()) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.2"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		if (!file.canRead()) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("HashHelper.3"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		if (file.isDirectory()) {
			CreateHashFiles.doHashProcess(
				null,
				file.getAbsolutePath(),
				DEFAULT_HASH_ALGORITHM,
				DEFAULT_RECURSIVE
			);
		}
		// Es un fichero
		else {
			CreateHashDialog.doHashProcess(
				null,
				file.getAbsolutePath(),
				DEFAULT_HASH_ALGORITHM,
				DEFAULT_USE_BASE64,
				DEFAULT_COPY_TO_CLIPBOARD,
				null
			);
		}

	}

}
