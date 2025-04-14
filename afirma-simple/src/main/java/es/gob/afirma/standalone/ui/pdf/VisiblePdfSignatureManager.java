package es.gob.afirma.standalone.ui.pdf;

import java.awt.Frame;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.standalone.ui.SignOperationConfig;
import es.gob.afirma.standalone.ui.SignatureExecutor;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;

/** Manejador para la firma visible PDF. */
public final class VisiblePdfSignatureManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Obtiene la configuracion necesaria para realizar una firma PDF visible.
	 * @param signConfigs Configuraci&oacute;n de firma.
	 * @param signExecutor Objeto para la ejecuci&oacute;n de las firmas.
	 * @param signatureVisible Indica si se va a insertar una firma
	 * @param stampVisible Indica si se va a insertar una marca
	 * @param parent Componente padre.
	 * @throws IOException Cuando se produce un error al procesar el PDF.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n. */
	public static void getVisibleSignatureParams(final List<SignOperationConfig> signConfigs,
												 final SignatureExecutor signExecutor,
												 final boolean signatureVisible,
												 final boolean stampVisible,
												 final Frame parent) throws IOException,
	                                                                        AOCancelledOperationException {

		// Obtenemos y actuamos sobre el primer PDF que se encuentre en las configuraciones
		SignOperationConfig pdfSignConf = null;
		for (final SignOperationConfig signConfig : signConfigs) {
			if (signConfig.getSigner() instanceof AOPDFSigner) {
				pdfSignConf = signConfig;
				break;
			}
		}
		byte[] data = null;

		if (pdfSignConf != null) {
			data = loadData(pdfSignConf.getDataFile());
		}

		final List<SignatureField> emptySignatureFields = PdfUtil.getPdfEmptySignatureFields(data);

		// Si hay campos de firma vacios, usaremos uno de entre ellos
		if (!emptySignatureFields.isEmpty()) {
			selectEmptySignatureField(data, signConfigs, emptySignatureFields, signExecutor, signatureVisible, stampVisible, parent);
		}
		// Si no los hay, pero se ha pedido agregar una firma visible o una imagen al PDF, permitiremos configurarlos
		else if (signatureVisible || stampVisible) {
			showVisibleSignatureDialog(data, signConfigs, signExecutor, signatureVisible, stampVisible, parent);
		}
		// Si no, procedemos inmediatamente con la firma
		else {

			LOGGER.info(" =========----- Iniciamos el proceso de firma visible del PDF, pero sin haber iniciado el dialogo");

			signExecutor.initSignTask(signConfigs);
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
			                                      final List<SignOperationConfig> signConfigs,
			                                      final List<SignatureField> emptySignatureFields,
			                                      final SignatureExecutor executor,
												  final boolean signatureVisible,
												  final boolean stampVisible,
			                                      final Frame parent) throws AOCancelledOperationException,
	                                                                         IOException {
		// Selecionamos uno de los campos de firma vacios del PDF
		final SignatureField field = PdfEmptySignatureFieldsChooserDialog.selectField(emptySignatureFields);

		// Si no se selecciono un campo de firma, se permitira crearlo
		if (field == null) {
			// Si se debe agregar una firma visible o una marca en el PDF, se muestra el dialogo para ello
			if (signatureVisible || stampVisible) {
				showVisibleSignatureDialog(data, signConfigs, executor, signatureVisible, stampVisible, parent);
			}
			// Si no es una firma visible, se firma directamente
			else {

				LOGGER.info(" =========----- Iniciamos el proceso de firma en un campo de firma prexistente");

				executor.initSignTask(signConfigs);
			}
		}
		// Si se selecciono el campo de firma que se debe usar, se firmar ese campo
		else {
			for (final SignOperationConfig signConfig : signConfigs) {
				signConfig.addExtraParam(PdfExtraParams.SIGNATURE_FIELD, field.getName());
			}

			// Si se ha pedido una marca visible, se muestra el dialogo correspondiente.
			// En este caso, nunca se permitira seleccionar el area de firma, ya que se
			// usara la del campo seleccionado.
			if (stampVisible) {
				showVisibleSignatureDialog(data, signConfigs, executor, false, stampVisible, parent);
			}
			// Si no, se firma directamente
			else {
				executor.initSignTask(signConfigs);
			}
		}
	}

    private static void showVisibleSignatureDialog(final byte[] data,
    		                                    final List<SignOperationConfig> signConfigs,
    		                                    final SignatureExecutor executor,
    		                                    final boolean signatureVisible,
    		                                    final boolean stampVisible,
    		                                    final Frame parent) throws IOException {
    	try {
    		SignPdfDialog.getVisibleSignatureExtraParams(
    			signConfigs.get(0).getSigner().isSign(data),
    			signConfigs.size() > 1,
				data,
				parent,
				signatureVisible,
				true,
				stampVisible,
				new SignPdfListener(signConfigs, executor)
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

	/** Procesa la firma de uno o varios PDF una vez se han obtenido
	 * los par&aacute;metros de firma visible. */
	public static final class SignPdfListener implements SignPdfDialogListener {

		private final List<SignOperationConfig> signConfigs;
		private final SignatureExecutor executor;

		/** Construye un objeto para ejecutar la firma de uno o varios PDF una vez se
		 * han obtenido sus par&aacute;metros para la firma visible.
		 * @param signConfigs Operaci&oacute;nes de firma a ejecutar.
		 * @param executor Objeto para la ejecuci&oacute;n de la/s firma/s. */
		public SignPdfListener(final List<SignOperationConfig> signConfigs, final SignatureExecutor executor) {
			this.signConfigs = signConfigs;
			this.executor = executor;
		}

		@Override
		public void propertiesCreated(final Properties extraParams) {
			// Agregamos las propiedades de la firma visible al resto de archivos PDF
			if (extraParams != null && !extraParams.isEmpty()) {
				for (final SignOperationConfig signConfig : this.signConfigs) {
					if (signConfig.getSigner() instanceof AOPDFSigner) {
						signConfig.addExtraParams(extraParams);
					}
				}
			}

			LOGGER.info(" =========----- Iniciamos el proceso de firma visible PDF con los parametros ya establecidos");

			this.executor.initSignTask(this.signConfigs);
		}

	}
}
