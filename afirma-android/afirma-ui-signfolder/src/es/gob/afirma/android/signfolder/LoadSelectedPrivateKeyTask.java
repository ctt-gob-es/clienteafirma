package es.gob.afirma.android.signfolder;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

import android.content.Context;
import android.os.AsyncTask;
import android.security.KeyChain;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;

public class LoadSelectedPrivateKeyTask extends AsyncTask<Void, Void, PrivateKey> {

	private final String selectedAlias;
	private final Context context;
	private final PrivateKeySelectionListener listener;
	private X509Certificate[] certChain;
	private Throwable t;
	
	public LoadSelectedPrivateKeyTask(final String certAlias, final PrivateKeySelectionListener listener, final Context context) {
		this.selectedAlias = certAlias;
		this.listener = listener;
		this.context = context;
	}
	
	@Override
	protected PrivateKey doInBackground(Void... params) {
		
		final PrivateKey pk;
		try {
			pk = KeyChain.getPrivateKey(this.context, this.selectedAlias);
			this.certChain = KeyChain.getCertificateChain(this.context, this.selectedAlias);
		} catch (Exception e) {
			e.printStackTrace();
			this.t = e;
			return null;
		}
		
		return pk;
	}
	
	@Override
	protected void onPostExecute(PrivateKey privateKey) {
		
		final KeySelectedEvent ksEvent;
		if (privateKey != null) {
			ksEvent = new KeySelectedEvent(new PrivateKeyEntry(privateKey, this.certChain), this.selectedAlias);	
		}
		else {
			ksEvent = new KeySelectedEvent(this.t);
		}
		
		this.listener.keySelected(ksEvent);
	}
}
