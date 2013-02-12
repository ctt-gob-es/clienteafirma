package es.gob.afirma.android.crypto;

import android.app.Activity;
import android.os.AsyncTask;

public class LoadKeyStoreManagerTask extends AsyncTask<Void, Void, Void> {

	public interface KeystoreManagerListener {
		public void setKeyStoreManager(MobileKeyStoreManager msm);
	}

	private final KeystoreManagerListener kmListener;
	private final Activity activity;

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
