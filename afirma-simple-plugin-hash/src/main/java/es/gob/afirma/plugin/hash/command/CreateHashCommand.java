package es.gob.afirma.plugin.hash.command;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.plugin.hash.CreateHashDirDialog;
import es.gob.afirma.plugin.hash.CreateHashFileDialog;
import es.gob.afirma.plugin.hash.DocumentException;
import es.gob.afirma.plugin.hash.HashDocument;
import es.gob.afirma.plugin.hash.HashDocumentFactory;
import es.gob.afirma.plugin.hash.HashUIHelper;
import es.gob.afirma.plugin.hash.Messages;
import es.gob.afirma.standalone.plugins.PluginCommandAction;
import es.gob.afirma.standalone.plugins.PluginControlledException;

public class CreateHashCommand extends PluginCommandAction {

	static String COMMAND = HashCommands.CREATEHASH.getOp();

	public CreateHashCommand() {
		super(COMMAND);
	}

	public CreateHashCommand(final String command) {
		super(command);
	}

	@Override
	protected String process(final String[] args)
			throws IllegalArgumentException, PluginControlledException {

		// Comprobamos si debemos mostrar la ayuda del comando
		if (args != null && args.length > 0
				&& HashParameters.PARAM_HELP.equalsIgnoreCase(args[0])) {
			return getHelpText();
		}

		String response = null;
		final HashParameters params = new HashParameters(HashCommands.CREATEHASH, args);

		if (params.isGui()) {
			createHashByGui(params);
			Runtime.getRuntime().halt(0);
		}
		else {
			try {
				response = createHashByCommandLine(params);
			} catch (final IllegalArgumentException e) {
				throw e;
			} catch (final Exception e) {
				throw new PluginControlledException(e.getMessage(), e);
			}
		}

		return response;
	}

	/**
	 * Realizamos la operaci&oacute;n de creaci&oacute;n de huellas digitales mostrando los di&aacute;logos
	 * que fuesen necesarios.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @throws IllegalArgumentException Cuando falta algun par&aacute;metro necesario.
	 */
	private static void createHashByGui(final HashParameters params)
			throws IllegalArgumentException {

		HashUIHelper.createHashUI(
				params.getMainFile(),
				params.getOutputFile(),
				params.getHashAlgorithm(),
				params.getHashFormat(),
				params.isRecursive());
	}

	/** Realizamos la operaci&oacute;n de creaci&oacute;n de huellas digitales a trav&eacute;s de consola.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @return Si se ha indicado el fichero de salida, se devuelve la cadena texto con le mensaje del resultado;
	 * si no, se devuelve el hash con la codificaci&oacute;n que se haya indicado.
	 * @throws IllegalArgumentException Cuando falta algun par&aacute;metro necesario.
	 * @throws IOException Cuando ocurre algun error en la lectura o guardado de ficheros.
	 * @throws AOException Cuando ocurre algun error al procesar la petici&oacute;n. */
	private static String createHashByCommandLine(final HashParameters params)
			throws IllegalArgumentException, IOException, AOException {

		final File inputFile = params.getMainFile();
		if (inputFile == null) {
			throw new IllegalArgumentException(Messages.getString("CommandLine.5")); //$NON-NLS-1$
		}

		if (!inputFile.exists()) {
			throw new IOException(Messages.getString("CommandLine.87", inputFile.getAbsolutePath())); //$NON-NLS-1$
		}
		if (!inputFile.canRead()) {
			throw new IOException(Messages.getString("CommandLine.88", inputFile.getAbsolutePath())); //$NON-NLS-1$
		}

		// Operamos segun la entrada sea un directorio o fichero
		byte[] hashesDocumentData;
		try {
			if (inputFile.isDirectory()) {

				// Se obtiene el formato de salida para los hashes del directorio
				final String outputFormat = params.getHashDirectoryFormat();

				// Hacemos el calculo
				final Map<String, byte[]> hashes = CreateHashDirDialog.calculateHashes(inputFile, params.isRecursive(), params.getHashAlgorithm(), null);

				// Generamos el informe
				final HashDocument hashDocument = HashDocumentFactory.getHashDocument(outputFormat);
				hashDocument.setHashes(hashes);
				hashDocument.setRecursive(params.isRecursive());
				hashDocument.setAlgorithm(params.getHashAlgorithm());
				hashDocument.setCharset(StandardCharsets.UTF_8);

				try {
					hashesDocumentData = hashDocument.generate();
				} catch (final DocumentException e) {
					throw new AOException(Messages.getString("CommandLine.93"), e); //$NON-NLS-1$
				}
			}
			// Si es un fichero
			else {

				// Se obtiene el formato de salida de hash del fichero
				final String outputFormat = params.getHashFileFormat();

				final byte[] calculatedHash = CreateHashFileDialog.calculateHash(inputFile, params.getHashAlgorithm(), null);

				// Codificamos como sea necesario segun el formato de guardado
				if (outputFormat.equals(HashParameters.FORMAT_HASH_FILE_BASE64)) {
					hashesDocumentData = Base64.encode(calculatedHash).getBytes();
				}
				else if (outputFormat.equals(HashParameters.FORMAT_HASH_FILE_BIN)) {
					if (params.getOutputFile() != null) {
						hashesDocumentData = calculatedHash;
					}
					else {
						throw new IllegalArgumentException(Messages.getString("CommandLine.119")); //$NON-NLS-1$
					}
				}
				else {
					hashesDocumentData = (AOUtil.hexify(calculatedHash, false) + "h").getBytes(); //$NON-NLS-1$
				}
			}
		}
		catch (final InterruptedException | ExecutionException e) {
			throw new AOException(Messages.getString("CommandLine.94"), e); //$NON-NLS-1$
		}

		// Si se ha proporcionado un fichero de salida, se guarda el resultado de la firma en el.
		// Si no, se imprime en consola
		String result;
		if (params.getOutputFile() != null) {
			try (final OutputStream fos = new FileOutputStream(params.getOutputFile());) {
				fos.write(hashesDocumentData);
			}
			catch(final Exception e) {
				throw new IOException(Messages.getString(
						"CommandLine.21", //$NON-NLS-1$
						params.getOutputFile().getAbsolutePath())
						, e);
			}
			result = Messages.getString("CommandLine.22"); //$NON-NLS-1$
		}
		else {
			result = new String(hashesDocumentData);
		}
		return result;
	}

	@Override
	public String getHelpText() {
		return HashParameters.buildSyntaxError(HashCommands.CREATEHASH, null);
	}
}
