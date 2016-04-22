package es.gob.afirma.android.signfolder;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;

import org.xml.sax.SAXException;

import android.content.ActivityNotFoundException;
import android.os.AsyncTask;
import android.util.Log;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignRequest;
import es.gob.afirma.android.util.AOException;

/** Tarea que ejecuta una firma electr&oacute;nica a trav&eacute;s de un AOSigner.
 * La operaci&oacute;n puede ser firma simple, cofirma o contrafirma (de nodos hoja
 * o todo el &aacute;rbol) seg&uacute;n se indique.
 * La firma es una operaci&oacute;n que necesariamente debe ejecutarse en segundo
 * plano ya que las firmas trif&aacute;sicas hacen conexiones de red.
 * @author Carlos Gamuci
 */
final class SignRequestTask extends AsyncTask<Void, Void, RequestResult>{

	private final SignRequest signRequest;
	private final CommManager comm;
	private final PrivateKey pk;
	private final X509Certificate[] certificateChain;
	private final OperationRequestListener operationListener;
	private Throwable t;

	/** Construye la tarea encargada de realizar la operaci&oacute;n criptogr&aacute;fica.
	 * @param comm Objeto para la comunicaci&oacute;n con el servidor proxy.
	 * @param pk Clave privada para la firma.
	 * @param certificateChain Cadena de certificaci&oacute;n.
	 * @param operationListener Manejador para el tratamiento del resultado de la operaci&oacute;n. */
	SignRequestTask(final SignRequest signRequest,
			final PrivateKey pk,
			final X509Certificate[] certificateChain,
			final CommManager comm,
			final OperationRequestListener operationListener) {
		this.signRequest = signRequest;
		this.pk = pk;
		this.certificateChain = certificateChain;
		this.comm = comm;
		this.operationListener = operationListener;
		this.t = null;
	}

	@Override
	protected RequestResult doInBackground(final Void...params) {

		this.t = null;
		RequestResult result = null;
		try {
			result = TriSigner.sign(this.signRequest, this.pk, this.certificateChain, this.comm);
		}
		catch (final CertificateEncodingException e) {
			Log.e(SFConstants.LOG_TAG, "Error al codificar el certificado de firma: " + e); //$NON-NLS-1$
			this.t = e;
			e.printStackTrace();
		}
		catch (final IOException e) {
			Log.e(SFConstants.LOG_TAG, "Error en la comunicacion con el servidor: " + e); //$NON-NLS-1$
			this.t = e;
			e.printStackTrace();
		}
		catch (final SAXException e) {
			Log.e(SFConstants.LOG_TAG, "Error en las respuesta devuelta por el servicio de firma: " + e); //$NON-NLS-1$
			this.t = e;
			e.printStackTrace();
		}
		catch (final Exception e) {

			e.printStackTrace();

			// Solo se dara este error (hasta la fecha) cuando se intente cargar el dialogo de PIN de
			// una tarjeta criptografica
			if (e.getCause() != null && e.getCause() instanceof AOException && e.getCause().getCause() instanceof ActivityNotFoundException) {
				Log.e(SFConstants.LOG_TAG, "Error al intentar cargar el dialogo de PIN de una tarjeta criptografica: " + e); //$NON-NLS-1$
				this.t = e;
			}
			else {
				Log.e(SFConstants.LOG_TAG, "Error durante la operacion de firma: " + e); //$NON-NLS-1$
				this.t = e;
			}
		}
		catch (final Throwable e) {
			Log.e(SFConstants.LOG_TAG, "Error grave durante la operacion de firma: " + e); //$NON-NLS-1$
			this.t = e;

			e.printStackTrace();
		}

		return result;
	}

	@Override
	protected void onPostExecute(final RequestResult result) {

		if (this.t == null && result != null && result.isStatusOk()) {
			this.operationListener.requestOperationFinished(OperationRequestListener.SIGN_OPERATION, result);
		}
		else {
			this.operationListener.requestOperationFailed(OperationRequestListener.SIGN_OPERATION, result, this.t);
		}
	}
}
