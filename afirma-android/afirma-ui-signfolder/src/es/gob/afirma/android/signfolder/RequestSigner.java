package es.gob.afirma.android.signfolder;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

import android.content.Context;
import android.os.Build;
import android.security.KeyChainException;
import android.util.Log;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestResult;
import es.gob.afirma.android.signfolder.proxy.SignRequest;
import es.gob.afirma.android.util.AOException;

/** Clase para la firma de peticiones. El resultado del proceso, se gestiona, a nivel individual para
 * cada petici&oacute;n desde un listener.
 * @author Carlos Gamuci Mill&aacute;n */
final class RequestSigner implements PrivateKeySelectionListener {

	private final String certAlias;
	private SignRequest[] requests;
	private final OperationRequestListener opListener;
	private final Context c;

	/**
	 * Construye un objeto con la configuraci&oacute;n necesaria para firmar.
	 * @param certAlias Alias del certificado de firma.
	 * @param operationListener Listener que procesar&aacute; el resultado de las firmas.
	 * @param context Contexto desde el que se invoca a la operaci&oacute;n.
	 */
	public RequestSigner(final String certAlias, final OperationRequestListener operationListener, final Context context) {
		this.certAlias = certAlias;

		this.opListener = operationListener;
		this.c = context;
	}

	/**
	 * Firma un listado de peticiones.
	 * @param signRequests Peticiones a firmar.
	 */
	public void sign(final SignRequest[] signRequests) {

		this.requests = signRequests;

		new LoadSelectedPrivateKeyTask(this.certAlias, this, this.c).execute();
	}

	@Override
	public synchronized void keySelected(final KeySelectedEvent kse) {

		Throwable t = null;
		PrivateKeyEntry pke = null;
		try {
			pke = kse.getPrivateKeyEntry();
			if (pke == null) {
				throw new Exception("Se ha recuperado una clave privada nula"); //$NON-NLS-1$
			}
		}
		catch (final KeyChainException e) {
			Log.e(SFConstants.LOG_TAG, e.toString());
			if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				t = new AOException(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE_ANDROID_4_1), e);
			}
			else {
				t = new AOException(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE), e);
			}
		}
		catch (final KeyStoreException e) {
			Log.e(SFConstants.LOG_TAG, "El usuario no selecciono un certificado: " + e); //$NON-NLS-1$
			t = new AOException(ErrorManager.getErrorMessage(ErrorManager.ERROR_CANCELLED_OPERATION), e);
		}
		// Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
		// en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
		catch (final Throwable e) {
			Log.e(SFConstants.LOG_TAG, "Error desconocido en la seleccion del certificado: " + e); //$NON-NLS-1$
			t = new AOException(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE), e);
		}

		if (t != null) {
			for (final SignRequest req : this.requests) {
				this.opListener.requestOperationFailed(OperationRequestListener.SIGN_OPERATION,
						new RequestResult(req.getId(), false), t);
			}
			return;
		}

		doSign(this.requests, pke.getPrivateKey(), (X509Certificate[]) pke.getCertificateChain());
	}

	private void doSign(final SignRequest[] reqs, final PrivateKey pk, final X509Certificate[] certChain) {
		for (final SignRequest req : reqs) {
			new SignRequestTask(req, pk, certChain, CommManager.getInstance(), this.opListener).execute();
		}
	}

}
