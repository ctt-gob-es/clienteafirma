package es.gob.afirma.android.gui;

import java.io.Serializable;
import java.security.KeyStore;
import java.util.ArrayList;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.ListView;
import android.widget.RadioButton;
import es.gob.afirma.android.crypto.AndroidJcaKeyStoreManager;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.android.signfolder.R;

/**Dia&acute;logo para elegir un alias
 * @author Astrid Idoate */
public final class SelectAliasDialog extends DialogFragment {

	/** Obtiene una nueva instancia del di&aacute;logo de selecci&oacute;n de alias de certificado.
	 * @param aliases lista de alias disponibles en el dispositivo
	 * @param ksmListener Clase a la que se establece el gestor de almacenes de claves y certificados
	 * @return devuelve la instanciaci&oacute;n del di&acute;logo de selecci&oacute;n del alias	 */
	public static SelectAliasDialog newInstance(final ArrayList<CertificateInfoForAliasSelect> aliases, final KeystoreManagerListener ksmListener) {
		final SelectAliasDialog scd = new SelectAliasDialog();
		final Bundle args = new Bundle();
		args.putSerializable("aliases", aliases); //$NON-NLS-1$
		scd.setArguments(args);
		scd.setKeyStoreListener(ksmListener);

		return scd;
	}

	String alias;
	void setAlias(final String alias){
		this.alias = alias;
	}
	String getAlias(){
		return this.alias;
	}

	char[] pin;
	void setPin(final char[] pin){
		this.pin = pin;
	}
	char[] getPin(){
		return this.pin;
	}

	private KeyStore ks = null;
	KeyStore getKs() {
		return this.ks;
	}

	/** Establece el almac&eacute;n de claves.
	 * @param ks KeyStore origen, debe estar previamente inicializado y cargado */
	void setKeyStore(final KeyStore ks){
		this.ks=ks;
	}

	private KeystoreManagerListener ksmListener = null;
	KeystoreManagerListener getKsmListener() {
		return this.ksmListener;
	}

	/**Clase para crear cada fila de la tabla.
	 * Cada fila consta de: nombre
	 * @author Astrid Idoate*/
	public final class AppAdapter extends ArrayAdapter<CertificateInfoForAliasSelect>{

		private final ArrayList<CertificateInfoForAliasSelect> items;
		/**
		 * @param context
		 * @param textViewResourceId
		 * @param items
		 */
		public AppAdapter(final Context context, final int textViewResourceId,  final ArrayList<CertificateInfoForAliasSelect> items) {
			super(context, textViewResourceId, items);
			this.items = items;
		}

		@Override
		public View getView(final int position, final View convertView, final ViewGroup parent) {
			View v;
			final LayoutInflater vi = (LayoutInflater) getActivity().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			v = vi.inflate(R.layout.dialog_aliases, null);

			final CertificateInfoForAliasSelect certInfo = this.items.get(position);

			final RadioButton rd = (RadioButton) v.findViewById(R.id.rdAlias);

			rd.setText(getString(R.string.dialog_alias_cert_text,
					certInfo.getCommonName(),
					certInfo.getNotBeforeDate(),
					certInfo.getNotAfterDate(),
					certInfo.getIssuer()));

			//la primera opcion por defecto siempre esta seleccionada
			if(position == 0){
				rd.setChecked(true);
				setAlias(certInfo.getAlias());
			}else{
				rd.setChecked(false);
			}

			rd.setOnCheckedChangeListener(new  OnCheckedChangeListener(){

				@Override
				public void onCheckedChanged(final CompoundButton button, final boolean isChecked) {
					if (isChecked) {
						setAlias(certInfo.getAlias());
					}
				}
			});
			return v;
		}
	}


	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState){

		final Serializable aliasesSerializable = getArguments().getSerializable("aliases"); //$NON-NLS-1$
		if (aliasesSerializable == null || !(aliasesSerializable instanceof ArrayList<?>) ||
				((ArrayList<?>) aliasesSerializable).size() < 1) {
			final AlertDialog.Builder noCertificatesBuilder = new AlertDialog.Builder(getActivity());
			noCertificatesBuilder.setTitle(getString(R.string.error_title_keystore_empty));
			noCertificatesBuilder.setMessage(getString(R.string.error_no_certs));
			noCertificatesBuilder.setPositiveButton(getString(R.string.ok), new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int id) {
					if (SelectAliasDialog.this.getKsmListener() != null) {
						SelectAliasDialog.this.getKsmListener().onErrorLoadingKeystore("No se encontraron certificados en el almacen", null); //$NON-NLS-1$
					}
					dialog.dismiss();
				}
			});

			return noCertificatesBuilder.create();
		}

		@SuppressWarnings("unchecked")
		final ArrayList<CertificateInfoForAliasSelect> aliases = (ArrayList<CertificateInfoForAliasSelect>) aliasesSerializable;

		final AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(getActivity());
		alertDialogBuilder.setTitle(getString(R.string.dialog_title_select_cert));
		alertDialogBuilder.setPositiveButton(getString(R.string.allow), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(final DialogInterface dialog, final int id) {
				if (SelectAliasDialog.this.getKsmListener() != null) {
					SelectAliasDialog.this.getKsmListener().setKeyStoreManager(
						new AndroidJcaKeyStoreManager(
							getAlias(),
							SelectAliasDialog.this.getKs(),
							getPin()
						)
					);
				}
				dialog.dismiss();
			}
		});
		alertDialogBuilder.setNegativeButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(final DialogInterface dialog, final int id) {
				//Cancelamos el proceso
				if (SelectAliasDialog.this.getKsmListener() != null) {
					SelectAliasDialog.this.getKsmListener().setKeyStoreManager(null);
				}
				dialog.dismiss();
			}
		});
		final LayoutInflater inflater = (LayoutInflater) getActivity().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		final View view = inflater.inflate(R.layout.dialog_list, null);
		final ListView listView = (ListView) view.findViewById(R.id.listViewListadoApp);

		final AppAdapter listaAliasAdapter = new AppAdapter(getActivity(), R.layout.dialog_aliases, aliases);
		listView.setAdapter(listaAliasAdapter);
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setOnKeyListener(new DialogInterface.OnKeyListener() {
			@Override
			public boolean onKey(final DialogInterface dialog, final int keyCode, final KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_BACK) {
					//Cancelamos el proceso
					if (SelectAliasDialog.this.getKsmListener() != null) {
						SelectAliasDialog.this.getKsmListener().setKeyStoreManager(null);
					}
					dialog.dismiss();
					return true;
				}
				return false;
			}
		});
		return alertDialogBuilder.create();
	}

	private void setKeyStoreListener(final KeystoreManagerListener ksmListener) {
		this.ksmListener = ksmListener;
	}

}
