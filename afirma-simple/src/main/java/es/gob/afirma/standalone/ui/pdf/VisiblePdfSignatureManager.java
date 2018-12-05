package es.gob.afirma.standalone.ui.pdf;

import java.awt.Frame;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.PdfExtraParams;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.standalone.ui.SignOperationConfig;
import es.gob.afirma.standalone.ui.SignatureExecutor;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;

/** Manejador para la firma visible PDF. */
public final class VisiblePdfSignatureManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Obtiene la configuracion necesaria para realizar una firma PDF visible.
	 * @param signConfig Configuraci&oacute;n de firma.
	 * @param signExecutor Objeto para la ejecuci&oacute;n de las firmas.
	 * @param signatureVisible Indica si se va a insertar una firma
	 * @param stampVisible Indica si se va a insertar una marca
	 * @param parent Componente padre.
	 * @throws IOException Cuando se produce un error al procesar el PDF.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n. */
	public static void getVisibleSignatureParams(final SignOperationConfig signConfig,
												 final SignatureExecutor signExecutor,
												 final boolean signatureVisible,
												 final boolean stampVisible, 
												 final Frame parent) throws IOException,
	                                                                        AOCancelledOperationException {
		final byte[] data = loadData(signConfig.getDataFile());
		final List<SignatureField> emptySignatureFields = PdfUtil.getPdfEmptySignatureFields(data);

		// Si hay campos visibles de firma vacios, usaremos uno de entre ellos
		if (!emptySignatureFields.isEmpty()) {
			selectEmptySignatureField(data, signConfig, emptySignatureFields, signExecutor, signatureVisible, stampVisible, parent);
		}
		// Si no los hay, permitiremos crear uno
		else {
			createNewSignatureField(data, signConfig, signExecutor, signatureVisible, stampVisible, parent);
		}
	}

	/** Permite indicar el campo en el que realizar un firma del PDF. Este campo
	 * puede ser uno nuevo que se cree al momento o un campo preexistente.
	 * @param data Documento PDF.
	 * @param signConfig Configuraci&oacute;n de firma a aplicar.
	 * @param emptySignatureFields Listado de campos vaci&oacute;s del PDF.
	 * @param executor Componente encargado de ejecutar la firma.
	 * @param signatureVisible Indica si se va a insertar una firma
	 * @param stampVisible Indica si se va a insertar una marca
	 * @param parent Componente padre.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n.
	 * @throws IOException Cuando se produce un error durante la firma. */
	private static void selectEmptySignatureField(final byte[] data,
			                                      final SignOperationConfig signConfig,
			                                      final List<SignatureField> emptySignatureFields,
			                                      final SignatureExecutor executor,
												  final boolean signatureVisible,
												  final boolean stampVisible, 
			                                      final Frame parent) throws AOCancelledOperationException,
	                                                                         IOException {
		// Selecionamos uno de los campos de firma vacios del PDF
		final SignatureField field = PdfEmptySignatureFieldsChooserDialog.selectField(
			emptySignatureFields
		);

		// Si no se selecciono un campo de firma, se permitira crearlo
		if (field == null) {
			createNewSignatureField(data, signConfig, executor, signatureVisible, stampVisible, parent);
		}
		else {
			signConfig.addExtraParam(PdfExtraParams.SIGNATURE_FIELD, field.getName());
			executor.initSignTask(Collections.singletonList(signConfig));
		}
	}

    private static void createNewSignatureField(final byte[] data,
    		                                    final SignOperationConfig signConfig,
    		                                    final SignatureExecutor executor,
    		                                    final boolean signatureVisible,
    		                                    final boolean stampVisible, 
    		                                    final Frame parent) throws IOException {
    	try {
    		SignPdfDialog.getVisibleSignatureExtraParams(
				signConfig.getSigner().isSign(data),
				data,
				parent,
				signatureVisible,
				stampVisible,
				new SignPdfListener(signConfig, executor)
			);
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se han podido cargar los parametros de la firma visible: " + e); //$NON-NLS-1$
    		throw new IOException("No se han podido cargar los parametros de la firma visible", e); //$NON-NLS-1$
    	}
    }

	/** Carga en memoria el contenido de un fichero.
	 * @param file Fichero a cargar.
	 * @return Contenido del fichero.
	 * @throws IOException Cuando ocurre un error durante la carga. */
	private static byte[] loadData(final File file) throws IOException {
		try (InputStream fis = new FileInputStream(file)) {
			return AOUtil.getDataFromInputStream(fis);
		}
	}

	/** Procesa la firma de un PDF una vez se han obtenido
	 * los par&aacute;metros de firma visible. */
	public static final class SignPdfListener implements SignPdfDialogListener {

		private final SignOperationConfig signConfig;
		private final SignatureExecutor executor;

		/** Construye un objeto para ejecutar la firma de un PDF una vez se
		 * han obtenido sus par&aacute;metros para la firma visible.
		 * @param signConfig Operaci&oacute;n de firma a ejecutar.
		 * @param executor Objeto para la ejecuci&oacute;n de la firma. */
		public SignPdfListener(final SignOperationConfig signConfig, final SignatureExecutor executor) {
			this.signConfig = signConfig;
			this.executor = executor;
		}

		@Override
		public void propertiesCreated(final Properties extraParams) {
			// Agregamos las propiedades de la firma visible al resto de propiedades
			// de la primera firma
			if (extraParams != null && !extraParams.isEmpty()) {
				this.signConfig.addExtraParams(extraParams);
			}
			this.executor.initSignTask(Collections.singletonList(this.signConfig));
		}

	}
}
