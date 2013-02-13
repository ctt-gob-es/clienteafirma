package es.gob.afirma.android.crypto;

import android.app.Activity;
import android.os.AsyncTask;

/** Tarea de carga e inicialización del gestor de claves y certificados en Android. */
public final class LoadKeyStoreManagerTask extends AsyncTask<Void, Void, Void> {

	/** Interfaz para la notificaci&oacute;n de finalizaci&oacute;n de la
	 * carga e inicialización del gestor de claves y certificados. */
	public interface KeystoreManagerListener {
		/** Establece un gestor de claves y certificados ya inicializado.
		 * @param msm Gestor de claves y certificados */
		public void setKeyStoreManager(MobileKeyStoreManager msm);
	}

	private final KeystoreManagerListener kmListener;
	private final Activity activity;

	/** Crea una tarea de carga e inicialización del gestor de claves y certificados en Android.
	 * @param kml Clase a la que hay que notificar cuando se finaliza la
	 * carga e inicialización del gestor de claves y certificados
	 * @param act Actividad padre */
	public LoadKeyStoreManagerTask(final KeystoreManagerListener kml, final Activity act) {
		this.kmListener = kml;
		this.activity = act;
	}

	@Override
	protected Void doInBackground(final Void... params) {
		this.kmListener.setKeyStoreManager(KeyStoreManagerFactory.getKeyStoreManager(this.activity));
		return null;
	}

}
