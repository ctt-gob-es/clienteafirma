package es.gob.afirma.android.signfolder;

import java.net.URL;
import java.util.Collections;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.DialogInterface.OnShowListener;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.EditText;
import android.widget.ListView;
import es.gob.afirma.android.signfolder.proxy.AppPreferences;

final class LoginOptionsDialogBuilder {

	private final LoginOptionsListener listener;

	private final AlertDialog.Builder builder;

	private final AlertDialog alertDialog;
	AlertDialog getAlertDialog() {
		return this.alertDialog;
	}

	private CharSequence[] items;
	CharSequence[] getItems() {
		return this.items;
	}

	private int selectedServer;
	int getSelectedServer() {
		return this.selectedServer;
	}
	void setSelectedServer(final int s) {
		this.selectedServer = s;
	}

	LoginOptionsDialogBuilder(final Activity activity, final LoginOptionsListener listener) {

		this.listener = listener;
		this.builder = new AlertDialog.Builder(activity);
		this.selectedServer = 0;
		final LayoutInflater inflater = activity.getLayoutInflater();

		final List<String> servers = AppPreferences.getServersList();
		Collections.sort(servers);
		if (!servers.isEmpty()) {
			this.items = servers.toArray(new CharSequence[servers.size()]);
			if (servers.indexOf(AppPreferences.getAliasProxy()) != -1) {
				this.selectedServer = servers.indexOf(AppPreferences.getAliasProxy());
			}
			else {
				saveProxyConfig(
					this.items[this.selectedServer].toString(),
					AppPreferences.getServerUrl(this.items[this.selectedServer].toString())
				);
			}
			this.builder.setCustomTitle(inflater.inflate(R.layout.dialog_server_title, null));
			this.builder.setSingleChoiceItems(this.items, this.selectedServer, new OnClickListener() {
		        @Override
		        public void onClick(final DialogInterface d, final int n) {
		        	setSelectedServer(n);
		        }
			});
		}
		else {
			this.builder.setTitle(R.string.server_select);
			this.builder.setMessage(R.string.dialog_server_new_info);
		}

		this.builder.setPositiveButton(R.string.ok,
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int which) {
					saveProxyConfig(
							getItems()[getSelectedServer()].toString(),
						AppPreferences.getServerUrl(getItems()[getSelectedServer()].toString())
					);
				}
			}
		);

		this.builder.setNeutralButton(R.string.dialog_server_new_button,
				new DialogInterface.OnClickListener() {
					@Override
					public void onClick(final DialogInterface dialog, final int which) {
						addServer(activity, inflater);
					}
			}
		);
		this.builder.setNegativeButton(R.string.cancel, null);
		this.alertDialog = this.builder.create();

		if (!servers.isEmpty()) {
			this.alertDialog.setOnShowListener(new OnShowListener()
			{
			    @Override
			public void onShow(final DialogInterface dialog)
			{
			        final ListView lv = getAlertDialog().getListView();
			        lv.setOnItemLongClickListener(new OnItemLongClickListener()
			    {
			    @Override
			    public boolean onItemLongClick(final AdapterView<?> parent, final View view, final int position, final long id)
			    {
			    	editServer(
		    			activity,
		    			inflater,
		    			getItems()[position].toString(),
		    			AppPreferences.getServerUrl(getItems()[position].toString())
			    	);
					return true;
			    }
			    });
			}
			});
		}
	}

	void addServer(final Activity act, final LayoutInflater inflater) {
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
					if (!alias.getText().toString().trim().isEmpty()
						&& url != null && !url.getEditableText().toString().trim().isEmpty()) {
						try {
							new URL(url.getEditableText().toString()).toString();
						}
						catch (final Exception e) {
							getListener().onErrorLoginOptions(act.getString(R.string.invalid_url));
							return;
						}
						AppPreferences.setServer(alias.getText().toString(), url.getEditableText().toString());
						getAlertDialog().dismiss();
						final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, getListener());
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

	void editServer(final Activity act, final LayoutInflater inflater, final String alias, final String url) {
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
					if (!newAlias.getText().toString().trim().isEmpty()
						&& !newUrl.getEditableText().toString().trim().isEmpty()) {
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
						getAlertDialog().dismiss();
						final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, getListener());
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
				getAlertDialog().dismiss();
				final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(act, getListener());
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
		this.alertDialog.show();
	}

	/**
	 * Interfaz a la que se notifica cuando ocurre un error en la configuraci&oacute;n de la
	 * aplicaci&oacute;n.
	 */
	public interface LoginOptionsListener {

		public void onErrorLoginOptions(final String url);
	}
}
