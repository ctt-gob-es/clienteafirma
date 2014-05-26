package es.gob.afirma.android.crypto;

import android.app.Activity;
import android.os.AsyncTask;

/** Tarea de carga e inicializaci&oacute;n del gestor de claves y certificados en Android. */
public final class LoadKeyStoreManagerTask extends AsyncTask<Void, Void, Void> {

	/** Interfaz para la notificaci&oacute;n de finalizaci&oacute;n de la
	 * carga e inicializaci&oacute;n del gestor de claves y certificados. */
	public interface KeystoreManagerListener {

		/** Establece un gestor de claves y certificados ya inicializado.
		 * @param msm Gestor de claves y certificados */
		public void setKeyStoreManager(MobileKeyStoreManager msm);

		/** Establece el error que hizo fallar la carga del almac&eacute;n de certificados.
		 * @param msg Texto con la descripci&oacute;n del error.
		 * @param t Error capturado. */
		public void onErrorLoadingKeystore(String msg, Throwable t);
	}

	private final KeystoreManagerListener kmListener;
	private final Activity activity;

	/** Crea una tarea de carga e inicializaci&oacute;n del gestor de claves y certificados en Android.
	 * @param kml Clase a la que hay que notificar cuando se finaliza la
	 * carga e inicializaci&oacute;n del gestor de claves y certificados
	 * @param act Actividad padre */
	public LoadKeyStoreManagerTask(final KeystoreManagerListener kml, final Activity act) {
		this.kmListener = kml;
		this.activity = act;
	}


	@Override
	protected Void doInBackground(final Void... params) {
		KeyStoreManagerFactory.initKeyStoreManager(this.activity, this, this.kmListener, null, null);
		return null;
	}

	/** <i>Callback</i> para el establecimiento de almac&eacute;n.
	 * @param ks Almac&eacute;n de claves y certificados */
	public void setKeyStore(final MobileKeyStoreManager ks) {
		this.kmListener.setKeyStoreManager(ks);
	}

	/** <i>Callback</i> para el error en el establecimiento de almac&eacute;n.
	 * @param msg Mensaje de error
	 * @param t Error */
	public void onErrorLoadingKeyStore(final String msg, final Throwable t) {
		this.kmListener.onErrorLoadingKeystore(msg, t);
	}
}
