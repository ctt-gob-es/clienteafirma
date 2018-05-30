package es.gob.afirma.standalone.ui.pdf;

import java.awt.Frame;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.standalone.ui.SignOperationConfig;
import es.gob.afirma.standalone.ui.SignatureExecutor;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;

/**
 * Manejador para la firma visible PDF.
 */
public class VisiblePdfSignatureManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Obtiene la configuracion necesaria para realizar una firma PDF visible.
	 * @param signConfig Configuraci&oacute;n de firma.
	 * @param signExecutor Objeto para la ejecuci&oacute;n de las firmas.
	 * @param parent Componente padre.
	 * @throws IOException Cuando se produce un error al procesar el PDF.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n.
	 */
	public static void getVisibleSignatureParams(
			final SignOperationConfig signConfig,
			final SignatureExecutor signExecutor,
			final Frame parent) throws IOException, AOCancelledOperationException {

		final byte[] data = loadData(signConfig.getDataFile());
		final List<SignatureField> emptySignatureFields = PdfUtil.getPdfEmptySignatureFields(data);

		// Si hay campos visibles de firma vacios, usaremos uno de entre ellos
		if (!emptySignatureFields.isEmpty()) {
			selectEmptySignatureField(data, signConfig, emptySignatureFields, signExecutor, parent);
		}
		// Si no los hay, permitiremos crear uno
		else {
			createNewSignatureField(data, signConfig, signExecutor, parent);
		}
	}

	/**
	 * Permite indicar el campo en el que realizar un firma del PDF. Este campo
	 * puede ser uno nuevo que se cree al momento o un campo preexistente.
	 * @param data Documento PDF.
	 * @param signConfig Configuraci&oacute;n de firma a aplicar.
	 * @param emptySignatureFields Listado de campos vaci&oacute;s del PDF.
	 * @param executor Componente encargado de ejecutar la firma.
	 * @param parent Componente padre.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n.
	 * @throws IOException Cuando se produce un error durante la firma.
	 */
	private static void selectEmptySignatureField(final byte[] data, final SignOperationConfig signConfig,
			List<SignatureField> emptySignatureFields, final SignatureExecutor executor, Frame parent)
					throws AOCancelledOperationException, IOException {

		// Selecionamos uno de los campos de firma vacios del PDF
		final SignatureField field = PdfEmptySignatureFieldsChooserDialog.selectField(emptySignatureFields);

		// Si no se selecciono un campo de firma, se permitira crearlo
		if (field == null) {
			createNewSignatureField(data, signConfig, executor, parent);
		}
	}

    private static void createNewSignatureField(final byte[] data, final SignOperationConfig signConfig,
    		SignatureExecutor executor, final Frame parent) throws IOException {
    	try {
    		SignPdfDialog.getVisibleSignatureExtraParams(
    				signConfig.getSigner().isSign(data),
    				data,
    				parent,
    				new SignPdfListener(signConfig, executor));
    	} catch (final Exception e) {
    		LOGGER.severe("No se han podido cargar los parametros de la firma visible: " + e); //$NON-NLS-1$
    		throw new IOException("No se han podido cargar los parametros de la firma visible"); //$NON-NLS-1$
    	}
    }

	/**
	 * Carga en memoria el contenido de un fichero.
	 * @param file Fichero a cargar.
	 * @return Contenido del fichero.
	 * @throws IOException Cuando ocurre un error durante la carga.
	 */
	private static byte[] loadData(File file) throws IOException {
		int n = 0;
		final byte[] buffer = new byte[4096];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try (FileInputStream fis = new FileInputStream(file)) {
			while ((n = fis.read(buffer)) > 0) {
				baos.write(buffer, 0, n);
			}
		}
		return baos.toByteArray();
	}

	/**
	 * Clase que procesa la firma de un PDF una vez se han obtenido
	 * los par&aacute;metros de firma visible.
	 */
	public static class SignPdfListener implements SignPdfDialogListener {

		private final SignOperationConfig signConfig;
		private final SignatureExecutor executor;

		/**
		 * Construye un objeto para ejecutar la firma de un PDF una vez se
		 * han obtenido sus par&aacute;metros para la firma visible.
		 * @param signConfig Operaci&oacute;n de firma a ejecutar.
		 * @param executor Objeto para la ejecuci&oacute;n de la firma.
		 */
		public SignPdfListener(SignOperationConfig signConfig, SignatureExecutor executor) {
			this.signConfig = signConfig;
			this.executor = executor;
		}

		@Override
		public void propertiesCreated(final Properties extraParams) {

			// Agregamos las propiedades de la firma visible al resto de propiedades
			// de la primera firma
			if (extraParams != null && !extraParams.isEmpty()) {
				final Properties currentExtraParams = this.signConfig.getExtraParams();
				final Enumeration<Object> keys = extraParams.keys();
				while (keys.hasMoreElements()) {
					final String key = (String)keys.nextElement();
					currentExtraParams.setProperty(key, extraParams.getProperty(key));
				}
			}
			final List<SignOperationConfig> configs = new ArrayList<>(1);
			configs.add(this.signConfig);
			this.executor.initSignTask(configs);
		}

	}
}
