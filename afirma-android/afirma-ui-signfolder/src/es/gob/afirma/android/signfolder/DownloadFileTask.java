package es.gob.afirma.android.signfolder;

import java.io.File;

import android.app.Activity;
import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.SaveFileTask.SaveFileListener;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.DocumentData;

/** Tarea as&iacute;ncrona para la previsualizaci&oacute;n de documentos. */
final class DownloadFileTask extends AsyncTask<Void, Void, DocumentData> implements SaveFileListener {

	private static final String DEFAULT_TEMP_DOCUMENT_PREFIX = "temp";  //$NON-NLS-1$

	private static final String PDF_MIMETYPE = "application/pdf"; //$NON-NLS-1$

	private final String documentId;
	private final int type;
	private final boolean extDir;
	private final String proposedName;
	private final String mimetype;
	private final String certB64;
	private final CommManager commManager;
	private final DownloadDocumentListener listener;
	private final Activity activity;

	/** Documento de datos. */
	static final int DOCUMENT_TYPE_DATA = 1;

	/** Documento de firma. */
	static final int DOCUMENT_TYPE_SIGN = 2;

	/** Informe de firma. */
	static final int DOCUMENT_TYPE_REPORT = 3;

	/**
	 * Listener utilizado para detectar el resultado de una peticion de descarga de fichero para
	 * visualizaci&oacute;n.
	 */
	interface DownloadDocumentListener {

		/** Cuando el documento se ha descargado correctamente.
		 * @param documentFile Documento que hay que visualizar.
		 * @param filename Nombre del documento.
		 * @param mimetype MimeType del documento.
		 * @param docType Tipo de documento (datos, firma o informe). */
		void downloadDocumentSuccess(File documentFile, String filename, String mimetype, int docType);

		/** Cuando ocurri&oacute; un error al descargar el documento. */
		void downloadDocumentError();
	}

	/**
	 * Crea una tarea as&iacute;ncrona para la descarga de un documento. Al construir la tarea
	 * se indica si queremos almacenar el documento en el almacenamiento interno de la
	 * aplicaci&oacute;n o en el externo (directorio de descargas del dispositivo). El nombre
	 * propuesto del documento solo se atender&aacute;a cuando se almacene en el directorio externo.
	 * @param documentId Identificador del documento que se desea previsualizar.
	 * @param type Tipo de documento (datos, firma o informe).
	 * @param proposedName Nombre propuesto para el fichero.
	 * @param extDir Descargar en directorio de descargas externo ({@code true}) o en el directorio de datos interno ({@code false}).
	 * @param certB64 Certificado para la autenticaci&oacute;n de la operaci&oacute;n.
	 * @param commManager Manejador de los servicios de comunicaci&oacute;n con el portafirmas.
	 * @param listener Listener que procesa las notificaciones con el resultado de la operaci&oacute;n.
	 * @param activity Actividad sobre la que mostrar las notificaciones.
	 */
	DownloadFileTask(final String documentId, final int type, final String proposedName, final String mimetype, final boolean extDir, final String certB64, final CommManager commManager, final DownloadDocumentListener listener, final Activity activity) {
		this.documentId = documentId;
		this.type = type;
		this.proposedName = proposedName;
		this.mimetype = mimetype;
		this.extDir = extDir;
		this.certB64 = certB64;
		this.commManager = commManager;
		this.listener = listener;
		this.activity = activity;
	}

	Activity getActivity() {
		return this.activity;
	}

	@Override
	protected DocumentData doInBackground(final Void... args) {

		DocumentData documentData;
		try {
			switch (this.type) {
			case DOCUMENT_TYPE_SIGN:
				documentData = this.commManager.getPreviewSign(this.documentId,
						this.proposedName, null, this.certB64);
				break;
			case DOCUMENT_TYPE_REPORT:
				documentData = this.commManager.getPreviewReport(this.documentId,
						this.proposedName, PDF_MIMETYPE, this.certB64);
				break;
			default:
				documentData = this.commManager.getPreviewDocument(this.documentId,
						this.proposedName, this.mimetype, this.certB64);
			}

		} catch (final Exception e) {
    		Log.w(SFConstants.LOG_TAG, "No se pudo descargar el documento para su previsualizacion: " + e); //$NON-NLS-1$
    		return null;
    	}

		return documentData;
	}

	@Override
	protected void onPostExecute(final DocumentData documentData) {

		if (isCancelled()) {
			return;
		}

		if (documentData == null) {
			this.listener.downloadDocumentError();
			return;
		}

		// Una vez tenemos la respuesta del servicio, guardamos el fichero
		String suffix = null;
		if (documentData.getFilename() != null && documentData.getFilename().indexOf('.') != -1) {
			suffix = documentData.getFilename().substring(documentData.getFilename().lastIndexOf('.'));
		}

		String filename = this.proposedName;
		if (filename == null) {
			filename = DEFAULT_TEMP_DOCUMENT_PREFIX;
			if (suffix != null) {
				filename += suffix;
			}
		}

		new SaveFileTask(
				documentData.getDataIs(), filename, this.extDir, this, this.activity
				).execute();
	}

	@Override
	public void saveFileSuccess(File outputFile) {
		this.listener.downloadDocumentSuccess(outputFile, outputFile.getName(), this.mimetype, this.type);
	}

	@Override
	public void saveFileError(String filename) {
		this.listener.downloadDocumentError();
	}
}
