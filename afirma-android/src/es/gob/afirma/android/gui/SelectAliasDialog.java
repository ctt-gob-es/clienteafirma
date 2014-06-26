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
import android.view.View.OnClickListener;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import es.gob.afirma.R;
import es.gob.afirma.android.crypto.AndroidJcaKeyStoreManager;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;

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

	ArrayList<CertificateInfoForAliasSelect> aliases;
	ArrayList<CertificateInfoForAliasSelect> getAlises(){
		return this.aliases;
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

	@SuppressWarnings("unchecked")
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {

		//Datos de los alias recibidos
		final Serializable aliasesSerializable = getArguments().getSerializable("aliases"); //$NON-NLS-1$

		//Comprobamos si los datos son nulos
		if (aliasesSerializable == null || !(aliasesSerializable instanceof ArrayList<?>) ||
				((ArrayList<?>) aliasesSerializable).size() < 1) {

			final AlertDialog.Builder noCertificatesBuilder = new AlertDialog.Builder(getActivity());

			noCertificatesBuilder.setTitle(getString(R.string.error_title_keystore_empty));
			noCertificatesBuilder.setMessage(getString(R.string.error_no_certs));
			noCertificatesBuilder.setPositiveButton(getString(R.string.ok), new DialogInterface.OnClickListener() {

				@Override
				public void onClick(final DialogInterface dialog, final int id) {
					if (SelectAliasDialog.this.getKsmListener() != null) {
						SelectAliasDialog.this.getKsmListener().onErrorLoadingKeystore(
								getString(R.string.error_no_certs), null
						);
					}
					dialog.dismiss();
				}

			});

			return noCertificatesBuilder.create();
		}

		this.aliases = (ArrayList<CertificateInfoForAliasSelect>) aliasesSerializable;

		//Creamos el dialogo de selecion de alias
		final AlertDialog.Builder dialog = new AlertDialog.Builder(getActivity());

		dialog.setTitle(getString(R.string.dialog_title_select_cert));

		dialog.setPositiveButton(getString(R.string.allow), new DialogInterface.OnClickListener() {
			@Override
			public void onClick(final DialogInterface dlg, final int id) {

				if (SelectAliasDialog.this.getKsmListener() != null) {
					SelectAliasDialog.this.getKsmListener().setKeyStoreManager(
						new AndroidJcaKeyStoreManager(
							getAlias(),
							SelectAliasDialog.this.getKs(),
							getPin()
						)
					);
				}

				dlg.dismiss();
			}
		});

		dialog.setNegativeButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {

			@Override
			public void onClick(final DialogInterface dlg, final int id) {
				//Cancelamos el proceso
				if (SelectAliasDialog.this.getKsmListener() != null) {
					SelectAliasDialog.this.getKsmListener().setKeyStoreManager(null);
				}
				dlg.dismiss();
			}
		});

		final LayoutInflater inflater = (LayoutInflater) getActivity().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		final View view = inflater.inflate(R.layout.dialog_aliases, null);
		final RadioGroup rGroup = (RadioGroup) view.findViewById(R.id.radioGroup);
		for(int i = 0; i < getAlises().size(); i++) {
			final RadioButton rb = new RadioButton(getActivity().getApplicationContext());
			final CertificateInfoForAliasSelect certInfo = getAlises().get(i);
			rb.setText(getString(R.string.dialog_alias_cert_text,
					certInfo.getCommonName(),
					certInfo.getNotBeforeDate(),
					certInfo.getNotAfterDate(),
					certInfo.getIssuer()));
		    rb.setOnClickListener(new OnClickListener() {

		    	@Override
					public void onClick(final View v) {
						setAlias(certInfo.getAlias());
					}

		    });
		    rb.setTextColor(getResources().getColor(R.color.black));
		    rGroup.addView(rb);
		  }
		dialog.setView(view);

		dialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
			@Override
			public boolean onKey(final DialogInterface dlg, final int keyCode, final KeyEvent event) {

				if (keyCode == KeyEvent.KEYCODE_BACK) {

					//Cancelamos el proceso
					if (SelectAliasDialog.this.getKsmListener() != null) {
						SelectAliasDialog.this.getKsmListener().setKeyStoreManager(null);
					}
					dlg.dismiss();
					return true;
				}
				return false;

			}
		});
		return dialog.create();
	}

	private void setKeyStoreListener(final KeystoreManagerListener ksmListener) {
		this.ksmListener = ksmListener;
	}

}
