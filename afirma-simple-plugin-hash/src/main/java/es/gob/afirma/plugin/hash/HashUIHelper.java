/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.hash;

import java.awt.Component;
import java.awt.Image;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.plugin.hash.CreateHashFileDialog.HashFormat;
import es.gob.afirma.standalone.plugins.UIFactory;

/** Funciones para el acceso a las capacidades de creaci&oacute;n y verificaci&oacute;n de
 * huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HashUIHelper {

	private static final Logger LOGGER = Logger.getLogger(HashUIHelper.class.getName());

	private static final String DEFAULT_HASH_ALGORITHM = "SHA-256"; //$NON-NLS-1$
	private static final HashFormat DEFAULT_HASH_FORMAT = HashFormat.HEX;
	private static final boolean DEFAULT_COPY_TO_CLIPBOARD = true;

	private static final String REPORT_EXT = "hashreport"; //$NON-NLS-1$

	/**
	 * Comprueba las huellas digitales del fichero de huella proporcionados mediante un
	 * interfaz gr&aacute;fico.
	 * @param dataFile Fichero o directorio del que comprobar las huellas.
	 * @param hashFile Fichero con las huellas del fichero de datos o directorio.
	 */
	public static void checkHashUI(final File dataFile, final File hashFile) {
		if (dataFile == null) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.4"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}
		if (!dataFile.exists()) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.5"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}
		if (!dataFile.canRead()) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.3"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}

		// Comprobacion de las huellas de un directorio
		if (dataFile.isDirectory()) {
			File selectedHashFile;
			if (hashFile != null && hashFile.isFile()) {
				selectedHashFile = hashFile;
			}
			else {
				try {
					selectedHashFile = AOUIFactory.getLoadFiles(
							Messages.getString("HashHelper.10"), //$NON-NLS-1$
							hashFile != null ? hashFile.getParent() : dataFile.getParent(),
									hashFile != null ? hashFile.getName() : null,
											new String[] {"hashfiles", "txthashfiles"}, //$NON-NLS-1$ //$NON-NLS-2$
											Messages.getString("HashHelper.15"), //$NON-NLS-1$
											false,
											false,
											getDialogIcon(),
											null)[0];
				}
				catch(final AOCancelledOperationException e) {
					// Operacion cancelada
					return;
				}
			}

			// Comprobamos los hashes
			HashReport report;
			try {
				// Se crea la ventana de espera
				JDialog dialog;
				try {
					dialog = UIFactory.getWaitingDialog(
							null,
							Messages.getString("CreateHashFiles.21"), //$NON-NLS-1$
							Messages.getString("CreateHashFiles.22") //$NON-NLS-1$
							);
				}
				catch (final Exception e) {
					// No se puede mostrar el dialogo de espera
					dialog = null;
				}

				// Arrancamos el proceso en un hilo aparte
				final SwingWorker<HashReport, Void> worker = new CheckHashWorker(dataFile, selectedHashFile, dialog);
				worker.execute();

				// Se muestra la ventana de espera
				if (dialog != null) {
					dialog.setVisible(true);
				}

				report = worker.get();

				if (!report.hasErrors()) {
					AOUIFactory.showMessageDialog(
							null,
							Messages.getString("CheckHashDialog.2"), //$NON-NLS-1$
							Messages.getString("CheckHashDialog.3"), //$NON-NLS-1$
							JOptionPane.INFORMATION_MESSAGE
							);
				}
				else {
					AOUIFactory.showMessageDialog(
							null,
							Messages.getString("CheckHashFiles.18"), //$NON-NLS-1$
							Messages.getString("CheckHashFiles.17"), //$NON-NLS-1$
							JOptionPane.WARNING_MESSAGE
							);
				}
			}
			catch (final OutOfMemoryError ooe) {
				AOUIFactory.showErrorMessage(
						Messages.getString("CreateHashFiles.2"), //$NON-NLS-1$
						Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ooe
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
								dataFile.getAbsolutePath()  + "' con el fichero: " +  //$NON-NLS-1$
								selectedHashFile.getAbsolutePath(), ex
						);
				AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.6"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ex
						);
				return;
			}

			// Generamos el informe de validacion con los resultados
			byte[] reportContent;
			try {
				reportContent = CheckHashDirDialog.generateXMLReport(report).getBytes(report.getCharset());
			}
			catch (final Exception e) {
				AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.20"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e
						);
				return;
			}

			try {
				showSaveReportDialog(reportContent, dataFile.getParent(), null);
			}
			catch(final AOCancelledOperationException e) {
				// Operacion cancelada
				return;
			}
			catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Error guardado el informe de comprobacion de hashes", e); //$NON-NLS-1$
				AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.19"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						e
						);
				return;
			}
		}
		// Comprobacion de la huella de un fichero
		else {
			// Cargamos el fichero de huellas
			final File selectedHashFile;
			if (hashFile != null && hashFile.isFile()) {
				selectedHashFile = hashFile;
			}
			else {
				try {
					selectedHashFile = AOUIFactory.getLoadFiles(
							Messages.getString("HashHelper.7"), //$NON-NLS-1$
							hashFile != null ? hashFile.getParent() : dataFile.getParent(),
									hashFile != null ? hashFile.getName() : null,
											new String[] {"hash", "hashb64", "hexhash"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
											Messages.getString("HashHelper.16"), //$NON-NLS-1$
											false,
											false,
											getDialogIcon(),
											null)[0];
				}
				catch(final AOCancelledOperationException e) {
					// Operacion cancelada
					return;
				}
			}

			try {
				if (CheckHashFileDialog.checkHash(selectedHashFile.getAbsolutePath(), dataFile.getAbsolutePath())) {
					AOUIFactory.showMessageDialog(
						null,
						Messages.getString("HashHelper.11"), //$NON-NLS-1$
						Messages.getString("HashHelper.12"), //$NON-NLS-1$
						JOptionPane.INFORMATION_MESSAGE
					);
				}
				else {
					AOUIFactory.showMessageDialog(
						null,
						Messages.getString("HashHelper.13"), //$NON-NLS-1$
						Messages.getString("HashHelper.14"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE
					);
				}
			}
			catch (final Exception e) {
				LOGGER.log(
					Level.SEVERE,
					"Error comprobando la huella digital de fichero '" + LoggerUtil.getCleanUserHomePath(dataFile.getAbsolutePath()) + "' con la huella guardada en " + selectedHashFile.getAbsolutePath(), //$NON-NLS-1$ //$NON-NLS-2$
					e
				);
				AOUIFactory.showErrorMessage(
					Messages.getString("HashHelper.8"), //$NON-NLS-1$
					Messages.getString("HashHelper.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e
				);
				return;
			}

		}
	}

	/** Crea las huellas digitales del fichero o directorio proporcionados mediante un
	 * interfaz gr&aacute;fico.
	 * @param inputFile Fichero o directorio del que se quiere calcular las huellas digitales.
	 * @param outputFile Fichero de salida por defecto con las huellas huellas digitales.
	 * @param hashAlgorithm Algoritmo de huella digital.
	 * @param hashFormat Formato en el que se almacenaran las huellas digitales.
	 * @param recursive Indica si se deben procesar los ficheros de los subdirectorios si la
	 * entrada era un directorio.
	 */
	public static void createHashUI(final File inputFile, final File outputFile, final String hashAlgorithm, final String hashFormat, final boolean recursive) {
		if (inputFile == null) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.0"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}
		if (!inputFile.exists()) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.2"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}
		if (!inputFile.canRead()) {
			AOUIFactory.showErrorMessage(
				Messages.getString("HashHelper.3"), //$NON-NLS-1$
				Messages.getString("HashHelper.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				null
			);
			return;
		}
		if (inputFile.isDirectory()) {
			CreateHashDirDialog.doHashProcess(
				null,
				inputFile,
				outputFile,
				hashAlgorithm != null ? hashAlgorithm : DEFAULT_HASH_ALGORITHM,
				hashFormat,
				recursive
			);
		}
		// Es un fichero
		else {
			CreateHashFileDialog.doHashProcess(
				null,
				inputFile,
				outputFile,
				hashAlgorithm != null ? hashAlgorithm : DEFAULT_HASH_ALGORITHM,
				hashFormat != null ? HashFormat.fromString(hashFormat) : DEFAULT_HASH_FORMAT,
				DEFAULT_COPY_TO_CLIPBOARD
			);
		}

	}

	static void showSaveReportDialog(final byte[] reportContent, final String parentDir, final Component parent) throws IOException {

		AOUIFactory.getSaveDataToFile(
				reportContent,
				Messages.getString("CheckHashFiles.15"), //$NON-NLS-1$
				parentDir,
				new java.io.File(Messages.getString("CheckHashFiles.16")).getName() + "." + REPORT_EXT, //$NON-NLS-1$ //$NON-NLS-2$
				Collections.singletonList(new GenericFileFilter(
						new String[] { REPORT_EXT },
						Messages.getString("CheckHashFiles.11") + " (*." +  REPORT_EXT + ")" //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
					)),
				parent
			);
	}

	public static File loadDirToCheck() {

		return AOUIFactory.getLoadFiles(
				Messages.getString("CheckHashFiles.24"), // Titulo //$NON-NLS-1$
				null,	// Directorio actual
				null,	// Nombre de directorio por defecto
				null,	// Extensiones de fichero
				Messages.getString("CheckHashFiles.13"), // Descripcion //$NON-NLS-1$
				true,	// Seleccion de directorio
				false,	// Multiseleccion
				getDialogIcon(),
				null // Componente padre
				)[0];

	}

	public static File loadFileToCheck() {

		return AOUIFactory.getLoadFiles(
				Messages.getString("CheckHashDialog.13"), // Titulo //$NON-NLS-1$
				null,	// Directorio actual
				null,	// Nombre de directorio por defecto
				null,	// Extensiones de fichero
				Messages.getString("CheckHashDialog.21"), // Descripcion //$NON-NLS-1$
				false,	// Seleccion de fichero
				false,	// Multiseleccion
				getDialogIcon(),
				null // Componente padre
				)[0];

	}

	private static Image getDialogIcon() {
		Image icon;
		try {
			icon = UIFactory.getDefaultDialogIcon();
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el icono del dialogo: " + e); //$NON-NLS-1$
			icon = null;
		}
		return icon;
	}

	private static class CheckHashWorker extends SwingWorker<HashReport, Void> {

		private final File dataFile;
		private final File hashFile;
		private final JDialog waitingDialog;

		public CheckHashWorker(final File dataFile, final File hashFile, final JDialog dialog) {
			this.dataFile = dataFile;
			this.hashFile = hashFile;
			this.waitingDialog = dialog;
		}

		@Override
		protected HashReport doInBackground() throws Exception {
			final HashReport hashReport =  new HashReport();
			CheckHashDirDialog.checkHash(
					Paths.get(this.dataFile.toURI()),
					this.hashFile,
					hashReport
					);
			return hashReport;
		}

		@Override
		protected void done() {
			super.done();
			if (this.waitingDialog != null) {
				this.waitingDialog.dispose();
			}
		}
	}
}
