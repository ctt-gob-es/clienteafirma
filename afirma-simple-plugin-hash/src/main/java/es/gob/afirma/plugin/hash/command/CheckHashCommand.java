package es.gob.afirma.plugin.hash.command;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ExecutionException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.plugin.hash.CheckHashDirDialog;
import es.gob.afirma.plugin.hash.CheckHashFileDialog;
import es.gob.afirma.plugin.hash.CorruptedDocumentException;
import es.gob.afirma.plugin.hash.DocumentException;
import es.gob.afirma.plugin.hash.HashReport;
import es.gob.afirma.plugin.hash.HashUIHelper;
import es.gob.afirma.plugin.hash.Messages;
import es.gob.afirma.standalone.plugins.PluginCommandAction;
import es.gob.afirma.standalone.plugins.PluginControlledException;

public class CheckHashCommand extends PluginCommandAction {

	static String COMMAND = HashCommands.CHECKHASH.getOp();

	public CheckHashCommand() {
		super(COMMAND);
	}

	public CheckHashCommand(final String command) {
		super(command);
	}

	@Override
	protected String process(final String[] args) throws PluginControlledException {

		// Comprobamos si debemos mostrar la ayuda del comando
		if (args == null || args.length == 0
				|| HashParameters.PARAM_HELP.equalsIgnoreCase(args[0])) {
			return getHelpText();
		}

		String response = null;
		final HashParameters params = new HashParameters(HashCommands.CHECKHASH, args);

		if (params.isGui()) {
			checkHashByGui(params);
			Runtime.getRuntime().halt(0);
		}
		else {
			boolean valid;
			try {
				valid = checkHashByCommandLine(params);
			} catch (final IllegalArgumentException e) {
				throw e;
			} catch (final Exception e) {
				throw new PluginControlledException(e.getMessage(), e);
			}
			response = valid ? Messages.getString("CommandLine.123") //$NON-NLS-1$
							 : Messages.getString("CommandLine.124"); //$NON-NLS-1$
		}

		return response;
	}

	/**
	 * Realizamos la operaci&oacute;n de comprobaci&oacute;n de huellas digitales a trav&eacute;s de consola.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @return {@code true} si la operaci&oacute;n termino correctamente, {@code false} en caso contrario.
	 * @throws IllegalArgumentException Cuando falta algun par&aacute;metro necesario.
	 * @throws IOException Cuando No se pueden leer los datos de entrada.
	 * @throws AOException Cuando falla el c&aacute;lculo del hash.
	 */
	private static boolean checkHashByCommandLine(final HashParameters params)
			throws IllegalArgumentException, IOException, AOException {

		final File dataFile = params.getMainFile();
		if (dataFile == null) {
			throw new IllegalArgumentException(Messages.getString("CommandLine.5")); //$NON-NLS-1$
		}

		if (!dataFile.exists()) {
			throw new IOException(Messages.getString("CommandLine.87", dataFile.getAbsolutePath())); //$NON-NLS-1$
		}
		if (!dataFile.canRead()) {
			throw new IOException(Messages.getString("CommandLine.88", dataFile.getAbsolutePath())); //$NON-NLS-1$
		}

		final File hashFile = params.getInputFile();
		if (hashFile == null) {
			throw new IllegalArgumentException(Messages.getString("CommandLine.6")); //$NON-NLS-1$
		}
		if (!hashFile.exists()) {
			throw new IOException(Messages.getString("CommandLine.98", hashFile.getAbsolutePath())); //$NON-NLS-1$
		}
		if (!hashFile.canRead()) {
			throw new IOException(Messages.getString("CommandLine.99", hashFile.getAbsolutePath())); //$NON-NLS-1$
		}

		boolean result;
		if (dataFile.isDirectory()) {

			final HashReport report = new HashReport();
			try {
				CheckHashDirDialog.checkHash(dataFile.toPath(), hashFile, report);
			} catch (final IOException e) {
				throw new IOException(Messages.getString("CommandLine.101"), e); //$NON-NLS-1$
			} catch (final DocumentException e) {
				throw new AOException(Messages.getString("CommandLine.102"), e); //$NON-NLS-1$
			} catch (final CorruptedDocumentException e) {
				throw new AOException(Messages.getString("CommandLine.103"), e); //$NON-NLS-1$
			}
			result = !report.hasErrors();

			if (params.getOutputFile() != null)  {
				byte[] reportData;
				try {
					reportData = CheckHashDirDialog.generateXMLReport(report).getBytes(report.getCharset());
				} catch (final Exception e) {
					throw new AOException(Messages.getString("CommandLine.104"), e); //$NON-NLS-1$
				}
				try (final OutputStream fos = new FileOutputStream(params.getOutputFile());) {
					fos.write(reportData);
				}
				catch(final Exception e) {
					throw new IOException(Messages.getString(
							"CommandLine.21", //$NON-NLS-1$
							params.getOutputFile().getAbsolutePath())
							, e);
				}
			}
		}
		else {
			try {
				result = CheckHashFileDialog.checkHash(hashFile, dataFile, null);
			}
			catch (final InterruptedException | ExecutionException e) {
				throw new AOException(Messages.getString("CommandLine.94"), e); //$NON-NLS-1$
			}
		}


		return result;
	}

	/** Realizamos la operaci&oacute;n de comprobaci&oacute;n de huellas digitales mostrando los di&aacute;logos
	 * que fuesen necesarios.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @throws IllegalArgumentException Cuando falta algun par&aacute;metro necesario. */
	private static void checkHashByGui(final HashParameters params)
			throws IllegalArgumentException {

		// Tomamos el fichero/directorio que se desea comprobar
		File dataFile = params.getMainFile();

		// Si no se indico el fichero/directorio, pediremos cargarlo.
		if (dataFile == null) {
			try {
				// Si se nos ha indicado validar un directorio, permitimos seleccionarlo
				if (params.isDirectory()) {
					dataFile = HashUIHelper.loadDirToCheck();
				}
				// Si no se indico, permitiremos seleccionar un fichero
				else {
					dataFile = HashUIHelper.loadFileToCheck();
				}
			}
			catch (final AOCancelledOperationException e) {
				// El usuario cancelo la operacion
				return;
			}
		}

		// Tomamos el fichero de hash (que podria no haberse indicado)
		final File hashFile = params.getInputFile();

		HashUIHelper.checkHashUI(dataFile, hashFile);
	}

	@Override
	public String getHelpText() {
		return HashParameters.buildSyntaxError(HashCommands.CHECKHASH, null);
	}
}
