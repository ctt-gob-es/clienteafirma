package es.gob.afirma.android.signfolder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import android.app.Activity;
import android.content.Context;
import android.os.AsyncTask;
import android.os.Environment;
import android.util.Log;
import android.widget.Toast;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.DocumentData;
import es.gob.afirma.core.misc.Base64;

/**
 * Tarea as&iacute;ncrona para la previsualizaci&oacute;n de documentos.
 */
public class DownloadFileTask extends AsyncTask<Void, Void, DocumentData> {

	private static final String DEFAULT_TEMP_DOCUMENT_PREFIX = "temp";  //$NON-NLS-1$

	private final String documentId;
	private final int type;
	private final boolean extDir;
	private final String proposedName;
	private final String certB64;
	private final CommManager commManager;
	private final DownloadDocumentListener listener;
	private final Activity activity;

	/** Documento de datos. */
	public static final int DOCUMENT_TYPE_DATA = 1;
	
	/** Documento de firma. */
	public static final int DOCUMENT_TYPE_SIGN = 2;
	
	/** Informe de firma. */
	public static final int DOCUMENT_TYPE_REPORT = 3;
	
	/**
	 * Listener utilizado para detectar el resultado de una peticion de descarga de fichero para
	 * visualizaci&oacute;n.
	 */
	public interface DownloadDocumentListener {
		
		/**
		 * Cuando el document se ha descargado correctamente.
		 * @param documentFile Documento que hay que visualizar.
		 * @param filename Nombre del documento.
		 * @param mimetype MimeType del documento.
		 * @param docType Tipo de documento (datos, firma o informe).
		 */
		public void downloadDocumentSuccess(File documentFile, String filename, String mimetype, int docType);
		
		/**
		 * Cuando ocurri&oacute; un error al descargar el documento.
		 */
		public void downloadDocumentError();
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
	public DownloadFileTask(final String documentId, final int type, final String proposedName, final boolean extDir, final String certB64, final CommManager commManager, final DownloadDocumentListener listener, final Activity activity) {
		this.documentId = documentId;
		this.type = type;
		this.proposedName = proposedName;
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
				documentData = this.commManager.getPreviewSign(this.documentId, this.certB64);				
				break;
			case DOCUMENT_TYPE_REPORT:
				documentData = this.commManager.getPreviewReport(this.documentId, this.certB64);		
				break;
			default:
				documentData = this.commManager.getPreviewDocument(this.documentId, this.certB64);
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

		// Una vez tenemos la respuesta del servicio, guardamos el fichero y lo abrirmos
		String suffix = null;
		if (documentData.getFilename() != null && documentData.getFilename().indexOf('.') != -1) {
			suffix = documentData.getFilename().substring(documentData.getFilename().lastIndexOf('.'));
		}
		
		File documentFile;
		try {
			documentFile = saveFile(documentData, suffix);
		} catch (final Exception e) {
			documentFile = null;
			Toast.makeText(this.activity, R.string.toast_error_previewing, Toast.LENGTH_SHORT).show();
			Log.i(SFConstants.LOG_TAG, "Error durante el guardado del fichero: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}
		
		if (documentFile != null) {
			this.listener.downloadDocumentSuccess(documentFile, documentData.getFilename(), documentData.getMimetype(), this.type);
		}
	}
	
	/**
	 * Guarda un documento en el espacio de cache de la aplicaci&oacute;n.
	 * @param documentData Documento que se desea guardar.
	 * @param suffix Sufijo para el final del nombre del documento (com&uacute;nmente la extensi&oacute;n).
	 * @return Fichero generado.
	 * @throws IOException Cuando ocurre un error al guardar el fichero.
	 */
	private File saveFile(final DocumentData documentData, final String suffix) throws IOException {
		
		String filename = DEFAULT_TEMP_DOCUMENT_PREFIX;
		if (suffix != null) {
			filename += suffix;
		}
		
		File outFile;
		if (this.extDir) {
			int i = 0;
			do {
				outFile = new File(
						Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS),
						generateFileName(this.proposedName, i++));
			} while (outFile.exists());

			final FileOutputStream fos = new FileOutputStream(outFile);
			fos.write(Base64.decode(documentData.getDataB64()));
			fos.close();
			
		} else {
			final FileOutputStream fos = this.activity.openFileOutput(filename, Context.MODE_WORLD_READABLE);
			fos.write(Base64.decode(documentData.getDataB64()));
			fos.close();

			outFile = new File(this.activity.getFilesDir(), filename);			
		}

		return outFile;
	}
	
	/**
	 * Genera un nombre de fichero agregando un indice al final del nombre propuesto. Si el
	 * &iacute;ndice es menor o igual a 0, se devuelve el nombre indicado.
	 * @param docName Nombre inicial del fichero.
	 * @param index &Iacute;ndice que agregar.
	 * @return Nombre generado.
	 */
	private static String generateFileName(final String docName, final int index) {
		if (index <= 0) {
			return docName;
		}
		
		final int lastDocPos = docName.lastIndexOf('.');
		if (lastDocPos == -1) {
			return docName + '(' + index + ')';
		}
		
		return docName.substring(0, lastDocPos) + '(' + index + ')' + docName.substring(lastDocPos);
	}
}
