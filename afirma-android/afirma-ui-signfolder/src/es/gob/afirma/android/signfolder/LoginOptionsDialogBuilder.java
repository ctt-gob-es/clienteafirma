package es.gob.afirma.android.signfolder;

import java.net.URL;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import es.gob.afirma.android.signfolder.proxy.AppPreferences;

public class LoginOptionsDialogBuilder {
	
	private final LoginOptionsListener listener;
	
	private final AlertDialog.Builder builder;
	
	private final View v;
	
	public LoginOptionsDialogBuilder(final Activity activity, final LoginOptionsListener listener) {

		this.listener = listener;
		this.builder = new AlertDialog.Builder(activity);
		
		// Establecemos el layout del dialogo		
		LayoutInflater inflater = activity.getLayoutInflater();
		this.v = inflater.inflate(R.layout.dialog_login_options, null);
		
		final EditText etUrl = (EditText) this.v.findViewById(R.id.etProxyUrl);
		loadSavedUrl(etUrl);
		
		this.builder.setView(this.v).setTitle(R.string.dialog_title_login_options);
		
		this.builder.setPositiveButton(R.string.ok,
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						
						try {
							new URL(etUrl.getEditableText().toString());
						} catch (Exception e) {
							getListener().onErrorLoginOptions(etUrl.getEditableText().toString());
							return;
						}
						
						saveProxyConfig(etUrl.getEditableText().toString());
					}
				});
		
		this.builder.setNegativeButton(R.string.cancel, null);
	}
	
	LoginOptionsListener getListener() {
		return this.listener;
	}
	
	/**
	 * Carga la configuraci&oacute;n anteriormente cargada con la URL del proxy del Portafirmas.
	 * @param etUrl Caja de texto en donde mostrar la URL.
	 */
	private static void loadSavedUrl(final EditText etUrl) {
		etUrl.setText(AppPreferences.getUrlProxy());
	}
	
	/**
	 * Guarda la URL configurada.
	 * @param proxyUrl URL.
	 */
	static void saveProxyConfig(final String proxyUrl) {
		AppPreferences.setUrlProxy(proxyUrl);
	}
	
	public void show() {
		this.builder.show();
	}
	
	/**
	 * Interfaz a la que se notifica cuando ocurre un error en la configuraci&oacute;n de la
	 * aplicaci&oacute;n.
	 */
	public interface LoginOptionsListener {
		
		public void onErrorLoginOptions(final String url);
	}
}
