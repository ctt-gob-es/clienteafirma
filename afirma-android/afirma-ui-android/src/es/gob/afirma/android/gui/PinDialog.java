package es.gob.afirma.android.gui;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;

import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import es.gob.afirma.R;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.core.misc.AOUtil;

/** Di&acute;logo para introducir el PIN.
 * Se usa en almacenes distintos al del propio sistema operativo Android.
 * @author Astrid Idoate */

public class PinDialog extends DialogFragment {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private String provider;
	String getProviderName() {
		return this.provider;
	}

	private String keyStoreName;
	String getKeyStoreName() {
		return this.keyStoreName;
	}

	private KeystoreManagerListener ksmListener;
	KeystoreManagerListener getKsmListener() {
		return this.ksmListener;
	}

	private LoadKeyStoreManagerTask ksmTask = null;
	LoadKeyStoreManagerTask getKsmTask() {
		return this.ksmTask;
	}

	/** Construye un di&acute;logo para introducir el PIN. */
	public PinDialog() {
		this.ksmListener = null;
	}

	/** Obtiene una nueva instancia de un di&acute;logo para introducir el PIN.
	 * @param provider proveedor Proveedor de seguridad para obtener el almac&eacute;n de claves
	 * @param keyStoreName Nombre del almac&eacute;n de claves
	 * @param ksml Clase a la que se establece el gestor de almacenes de claves y certificados
	 * @return pinDialog el di&acute;logo creado
	 * @throws KeyStoreException Cuando el di&aacute;logo de PIN es invocado desde una
	 */
	public static PinDialog newInstance(final String provider, final String keyStoreName, final KeystoreManagerListener ksml) {

		final PinDialog pinDialog = new PinDialog();
		pinDialog.setKeyStoreManagerListener(ksml);
		final Bundle args = new Bundle();
		args.putString("provider", provider); //$NON-NLS-1$
		args.putString("keyStoreName", keyStoreName); //$NON-NLS-1$
		pinDialog.setArguments(args);
		return pinDialog;

	}

	/** Establece la clase que manejara el resultado de la carga del almacen de claves del dispositivo.
	 * @param ksml Manejador de la carga. */
	public void setKeyStoreManagerListener(final KeystoreManagerListener ksml) {
		this.ksmListener = ksml;
	}

	/** Establece la clase a la que hay que establecer el almac&eacute;n sobre el cual se pide el PIN.
	 * @param lksmt Clase en la que hay que establecer el almac&eacute;n */
	public void setLoadKeyStoreManagerTask(final LoadKeyStoreManagerTask lksmt) {
		this.ksmTask = lksmt;
	}

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState){

		this.provider = getArguments().getString("provider"); //$NON-NLS-1$
		this.keyStoreName = getArguments().getString("keyStoreName"); //$NON-NLS-1$

		Log.i(ES_GOB_AFIRMA,"PinDialog recibe los argumentos provider: " + this.provider + " y keyStoreName: " + this.keyStoreName);   //$NON-NLS-1$//$NON-NLS-2$

		final Builder alertDialogBuilder = new AlertDialog.Builder(getActivity());
		alertDialogBuilder.setTitle(getString(R.string.security_code) + " " + this.keyStoreName); //$NON-NLS-1$

		final LayoutInflater layoutInflater = LayoutInflater.from(getActivity());
		final View view = layoutInflater.inflate(R.layout.dialog_pin, null);

		final EditText editTextPin = (EditText) view.findViewById(R.id.etPin);
		alertDialogBuilder.setView(view);
		alertDialogBuilder.setNegativeButton(
			getActivity().getString(R.string.cancel),
			new DialogInterface.OnClickListener() {
				@Override
				public void onClick(final DialogInterface dialog, final int id) {
					dialog.dismiss();
					//Cancelamos el proceso
					if (PinDialog.this.getKsmListener() != null) {
						PinDialog.this.getKsmListener().setKeyStoreManager(null);
					}
				}
			}
		);
		alertDialogBuilder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(final DialogInterface dialog, final int which) {

				//TODO: El PIN no puede ser cadena vacia?
				if(editTextPin.getText() != null && !"".equals(editTextPin.getText().toString())) { //$NON-NLS-1$

					// Aqui tenemos el PIN, empezamos con la inicializacion del almacen
					final KeyStore ks;
					try {
						ks = KeyStore.getInstance(PinDialog.this.getKeyStoreName(), PinDialog.this.getProviderName());
						ks.load(null, editTextPin.getText().toString().toCharArray());
					}
					catch(final Exception e) {
						Log.e(ES_GOB_AFIRMA, "Error al cargar el almacen de claves: " + e); //$NON-NLS-1$
						dialog.dismiss();
						if (PinDialog.this.getKsmListener() != null) {
							PinDialog.this.getKsmListener().onErrorLoadingKeystore(
								getActivity().getString(R.string.error_loading_keystore), e
							);
						}
						return;
					}

					// Obtenemos los elementos para el dialogo de seleccion
					final Enumeration<String> aliases;
					try {
						aliases = ks.aliases();
					}
					catch(final Exception e) {
						Log.e(ES_GOB_AFIRMA, "Error extrayendo los alias de los certificados del almacen: " + e); //$NON-NLS-1$
						dialog.dismiss();
						if (PinDialog.this.getKsmListener() != null) {
							PinDialog.this.getKsmListener().onErrorLoadingKeystore(
								getActivity().getString(R.string.error_loading_certificate_alias), e
							);
						}
						return;
					}

					final ArrayList<CertificateInfoForAliasSelect> arrayListCertificate = new ArrayList<CertificateInfoForAliasSelect>();

					while(aliases.hasMoreElements()) {
						final String alias = aliases.nextElement();
						X509Certificate cert = null;
						try {
							cert = (X509Certificate) ks.getCertificate(alias);
						}
						catch (final KeyStoreException e) {
							Log.w(ES_GOB_AFIRMA, "No se ha podido extraer el certificado '" + alias + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
							continue;
						}
						catch(final Exception e) {
							// Gestion a medida de un DNIe bloqueado (usando JMultiCard)
							if ("es.gob.jmulticard.card.AuthenticationModeLockedException".equals(e.getClass().getName())) { //$NON-NLS-1$
								manageLockedDnie(e);
								return;
							}
							Log.e(ES_GOB_AFIRMA,"Error obteniendo el certificado con alias '" + alias + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
							continue;
						}

						// Comprobamos si tiene clave privada o no
						try {
							ks.getEntry(
								alias,
								new KeyStore.PasswordProtection(editTextPin.getText().toString().toCharArray())
							);
						}
						catch(final Exception e) {
							Log.w(ES_GOB_AFIRMA, "Se omite el certificado '" + AOUtil.getCN(cert) + "' por no tener clave privada: " + e); //$NON-NLS-1$ //$NON-NLS-2$
							continue;
						}
						arrayListCertificate.add(
							new CertificateInfoForAliasSelect(
								AOUtil.getCN(cert),
								cert.getNotBefore(), cert.getNotAfter(),
								alias,
								AOUtil.getCN(cert.getIssuerX500Principal().toString())
							)
						);

					}

					if(PinDialog.this.getKsmTask() == null){
						Log.e(ES_GOB_AFIRMA, "No se ha establecido la tarea para la obtencion del almacen de certificados con setLoadKeyStoreManagerTask()");  //$NON-NLS-1$
						dialog.dismiss();
						if (PinDialog.this.getKsmListener() != null) {
							PinDialog.this.getKsmListener().onErrorLoadingKeystore(
								getActivity().getString(R.string.error_loading_keystore), null
							);
						}
						return;
					}

					final SelectAliasDialog selectAlias = SelectAliasDialog.newInstance(
						arrayListCertificate,
						PinDialog.this.getKsmListener()
					);
					selectAlias.setKeyStore(ks);
					selectAlias.setPin(editTextPin.getText().toString().toCharArray());

					selectAlias.show(getActivity().getFragmentManager(), "SelectAliasDialog"); //$NON-NLS-1$
					dialog.dismiss();
				}

				else {
					//TODO: Gestionar este caso
					Log.e(ES_GOB_AFIRMA, "El pin no puede ser vacio o nulo"); //$NON-NLS-1$
					if (PinDialog.this.getKsmListener() != null) {
						PinDialog.this.getKsmListener().onErrorLoadingKeystore(
							getActivity().getString(R.string.error_pin_nulo), null
						);
					}
					return;
				}
			}

			private void manageLockedDnie(final Throwable e) {

				Log.e(ES_GOB_AFIRMA, "El DNIe esta bloqueado: " + e); //$NON-NLS-1$

				final AlertDialog.Builder dniBloqueado = new AlertDialog.Builder(getActivity());

				dniBloqueado.setTitle(getString(R.string.error_title_dni_blocked));
				dniBloqueado.setMessage(getString(R.string.error_dni_blocked_dlg));
				dniBloqueado.setPositiveButton(
					getString(R.string.ok),
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(final DialogInterface d, final int id) {
							d.dismiss();
						}
					}
				);
				dniBloqueado.create();
				dniBloqueado.show();

				if (PinDialog.this.getKsmListener() != null) {
					PinDialog.this.getKsmListener().onErrorLoadingKeystore(
						getActivity().getString(R.string.error_dni_blocked), e
					);
				}

			}
		});
		alertDialogBuilder.setOnKeyListener(new DialogInterface.OnKeyListener() {
			@Override
			public boolean onKey(final DialogInterface dialog, final int keyCode, final KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_BACK) {
					dialog.dismiss();
					//Cancelamos el proceso
					if (PinDialog.this.getKsmListener() != null) {
						PinDialog.this.getKsmListener().setKeyStoreManager(null);
					}
					return true;
				}
				return false;
			}
		});

		return alertDialogBuilder.create();
	}
}