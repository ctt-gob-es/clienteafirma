package es.gob.afirma.android.signfolder;

import java.net.URL;
import java.util.Collections;
import java.util.List;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import es.gob.afirma.android.signfolder.proxy.AppPreferences;

final class LoginOptionsDialogBuilder {

	private final LoginOptionsListener listener;

	private final AlertDialog.Builder builder;
	
	private AlertDialog alertDialog;

	private final View v;
		
	private CharSequence[] items;
	
	private int selectedServer;

	LoginOptionsDialogBuilder(final Activity activity, final LoginOptionsListener listener) {
		
		this.listener = listener;
		this.builder = new AlertDialog.Builder(activity);
		this.selectedServer = 0;

		// Establecemos el layout del dialogo
		final LayoutInflater inflater = activity.getLayoutInflater();

		this.v = inflater.inflate(R.layout.dialog_server_new, null);
		final List<String> servers = AppPreferences.getServersList();
		Collections.sort(servers);
		if (servers.size() > 0 ) {
			this.items = servers.toArray(new CharSequence[servers.size()]);
			if (servers.indexOf(AppPreferences.getAliasProxy()) != -1) {
				this.selectedServer = servers.indexOf(AppPreferences.getAliasProxy());
			}
			else {
				saveProxyConfig(
					items[this.selectedServer].toString(), 
					AppPreferences.getServerUrl(items[this.selectedServer].toString())
				);
			}
		}
		
		this.builder.setSingleChoiceItems(items, selectedServer, new OnClickListener() {
	        @Override
	        public void onClick(DialogInterface d, int n) {
	        	selectedServer = n;
	        }
		});
		
		final Button newButton = (Button) this.v.findViewById(R.id.dialog_server_new_button);
		newButton.setOnClickListener(
			new View.OnClickListener() {
				@Override
				public void onClick(final View v) {
					addServer(activity, inflater);
					
				}
			}
		);
		
		final Button editButton = (Button) this.v.findViewById(R.id.dialog_server_edit_button);
		editButton.setOnClickListener(
			new View.OnClickListener() {
				@Override
				public void onClick(final View v) {
					editServer(
						activity, 
						inflater, 
						items[selectedServer].toString(), 
						AppPreferences.getServerUrl(items[selectedServer].toString())
					);
				}
			}
		);

		if (servers.size() < 2) {
			((View) this.v.findViewById(R.id.dialog_server_line)).setVisibility(View.GONE);
		}
		
		this.builder.setView(this.v);
		this.builder.setCustomTitle((View) inflater.inflate(R.layout.dialog_server_title, null));
		
		this.builder.setPositiveButton(R.string.ok,
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int which) {						
					saveProxyConfig(
						items[selectedServer].toString(), 
						AppPreferences.getServerUrl(items[selectedServer].toString())
					);
				}
			}
		);

		this.builder.setNegativeButton(R.string.cancel, null);
	}

	private void addServer(final Activity act, final LayoutInflater inflater) {
		final AlertDialog.Builder builder = new AlertDialog.Builder(act);
		final View view = inflater.inflate(R.layout.dialog_add_server, null);
		
		final EditText alias = (EditText) view.findViewById(R.id.alias);
		final EditText url = (EditText) view.findViewById(R.id.url);
		alias.requestFocus();
		
		builder.setView(view);
		builder.setTitle(R.string.dialog_add_server_title);
		builder.setPositiveButton(R.string.ok,
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int which) {
					if (alias != null && !alias.getText().toString().trim().isEmpty()
						&& url != null && !url.getEditableText().toString().trim().isEmpty()) {
						try {
							new URL(url.getEditableText().toString()).toString();
						}
						catch (final Exception e) {
							getListener().onErrorLoginOptions(act.getString(R.string.invalid_url));
							return;
						}
						AppPreferences.setServer(alias.getText().toString(), url.getEditableText().toString());
						alertDialog.dismiss();
						final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, listener);
						dialogBuilder.show();
					}
					else{
						getListener().onErrorLoginOptions(act.getString(R.string.dialog_server_empty_fields));
					}
					
				}
			}
		);

		builder.setNegativeButton(R.string.cancel, null);
		builder.show();
	}
	
	private void editServer(final Activity act, final LayoutInflater inflater, final String alias, final String url) {
		final AlertDialog.Builder builder = new AlertDialog.Builder(act);
		final View view = inflater.inflate(R.layout.dialog_add_server, null);
		
		final EditText newAlias = (EditText) view.findViewById(R.id.alias);
		final EditText newUrl = (EditText) view.findViewById(R.id.url);
		newAlias.setText(alias);
		newAlias.requestFocus();
		newUrl.setText(url);
		
		builder.setView(view);
		builder.setTitle(R.string.dialog_edit_server_title);
		builder.setPositiveButton(R.string.ok,
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int which) {
					if (newAlias != null && !newAlias.getText().toString().trim().isEmpty()
						&& newUrl != null && !newUrl.getEditableText().toString().trim().isEmpty()) {
						try {
							new URL(newUrl.getEditableText().toString()).toString();
						}
						catch (final Exception e) {
							getListener().onErrorLoginOptions(act.getString(R.string.invalid_url));
							return;
						}
						AppPreferences.setServer(newAlias.getText().toString(), newUrl.getEditableText().toString());
						if (AppPreferences.getAliasProxy().equals(alias)) {
							saveProxyConfig(
								newAlias.getText().toString(), 
								newUrl.getText().toString()
							);
						}
						if (!newAlias.getText().toString().equals(alias)) {
							AppPreferences.removeServer(alias);
						}
						alertDialog.dismiss();
						final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, listener);
						dialogBuilder.show();
					}
					else {
						getListener().onErrorLoginOptions(act.getString(R.string.dialog_server_empty_fields));
					}
				}
			}
		);

		builder.setNegativeButton(R.string.cancel, null);
		builder.setNeutralButton(R.string.dialog_server_delete_button, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(final DialogInterface dialog, final int which) {
				if (AppPreferences.getAliasProxy().equals(alias)) {
					AppPreferences.removeProxyConfig();
				}
				
				AppPreferences.removeServer(alias);
				alertDialog.dismiss();
				final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, listener);
				dialogBuilder.show();
			}
		});
		builder.show();
	}
	
	LoginOptionsListener getListener() {
		return this.listener;
	}

	/**
	 * Guarda la URL configurada.
	 * @param proxyUrl URL.
	 */
	static void saveProxyConfig(final String alias, final String proxyUrl) {
		AppPreferences.setUrlProxy(alias, proxyUrl);
	}

	public void show() {
		this.alertDialog = this.builder.show();
	}

	/**
	 * Interfaz a la que se notifica cuando ocurre un error en la configuraci&oacute;n de la
	 * aplicaci&oacute;n.
	 */
	public interface LoginOptionsListener {

		public void onErrorLoginOptions(final String url);
	}
}
