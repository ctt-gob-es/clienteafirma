package es.gob.afirma.android.signfolder;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStoreException;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnKeyListener;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.os.Build;
import android.os.Bundle;
import android.security.KeyChain;
import android.security.KeyChainException;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.android.crypto.MobileKeyStoreManager;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;
import es.gob.afirma.android.signfolder.LoadConfigurationDataTask.LoadConfigurationListener;
import es.gob.afirma.android.signfolder.LoginOptionsDialogBuilder.LoginOptionsListener;
import es.gob.afirma.android.signfolder.proxy.AppPreferences;
import es.gob.afirma.android.signfolder.proxy.CommManager;
import es.gob.afirma.android.signfolder.proxy.RequestAppConfiguration;
import es.gob.afirma.core.misc.Base64;

/** Actividad para entrada con usuario y contrase&ntilde;a al servicio de ortafirmas. */
public final class LoginActivity extends FragmentActivity implements KeystoreManagerListener,
                                                                     PrivateKeySelectionListener,
                                                                     LoadConfigurationListener,
                                                                     LoginOptionsListener {

	private final static String EXTRA_RESOURCE_TITLE = "es.gob.afirma.signfolder.title"; //$NON-NLS-1$
	private final static String EXTRA_RESOURCE_EXT = "es.gob.afirma.signfolder.exts"; //$NON-NLS-1$

	private final static String CERTIFICATE_EXTS = ".p12,.pfx"; //$NON-NLS-1$

	private final static int SELECT_CERT_REQUEST_CODE = 1;

	/** Dialogo para mostrar mensajes al usuario */
	private MessageDialog messageDialog = null;

	MessageDialog getMessageDialog() {
		return this.messageDialog;
	}

	private ProgressDialog progressDialog = null;
	ProgressDialog getProgressDialog() {
		return this.progressDialog;
	}
	void setProgressDialog(final ProgressDialog pd) {
		this.progressDialog = pd;
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_login);

		//Comprobamos si la conectividad a la red es correcta
		final ConnectivityManager conMgr = (ConnectivityManager) getSystemService (Context.CONNECTIVITY_SERVICE);
		if (conMgr.getActiveNetworkInfo() == null ||
				!conMgr.getActiveNetworkInfo().isAvailable() ||
				!conMgr.getActiveNetworkInfo().isConnected()) {
			//Error en la conexion
			showErrorDialog(getString(R.string.error_msg_check_connection));
		}

		// Cargamos la configuracion general de la aplicacion
		AppPreferences.init(this);
	}

	/** @param v Vista sobre la que se hace clic. */
	public void onClick(final View v) {

		//Boton Acceder
		if(v.getId() == R.id.button_acceder){
			CommManager.resetConfig();
			if (!CommManager.getInstance().verifyProxyUrl()) {
				showErrorDialog(getString(R.string.error_msg_proxy_no_config));
				return;
			}

			loadKeyStore();
		}
		// Boton importar certificados
		else {
			browseKeyStore();
		}
	}

	/** Carga el almacen de certificados del sistema. Se configura desde el layout para su ejecucion. */
	public void loadKeyStore() {

		final LoadKeyStoreManagerTask lksmt = new LoadKeyStoreManagerTask(this, this);
		showProgressDialog(getString(R.string.dialog_msg_accessing_keystore),lksmt,null);
		lksmt.execute();
		}

	/** Abre un activity para la seleccion de un fichero PKCS#12 local. */
	public void browseKeyStore() {
		final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
		intent.setClass(this, FileChooserActivity.class);
		intent.putExtra(EXTRA_RESOURCE_TITLE, getString(R.string.title_activity_cert_chooser));
		intent.putExtra(EXTRA_RESOURCE_EXT, CERTIFICATE_EXTS);
		startActivityForResult(intent, SELECT_CERT_REQUEST_CODE);
	}

	@Override
	public void setKeyStoreManager(final MobileKeyStoreManager msm) {

		dismissProgressDialog();

		if (msm == null){
			Log.w(SFConstants.LOG_TAG, "Error al establecer el almacen de certificados. Es posible que el usuario cancelase la operacion."); //$NON-NLS-1$
			showErrorDialog(ErrorManager.getErrorMessage(ErrorManager.ERROR_ESTABLISHING_KEYSTORE));
		} else {
			msm.getPrivateKeyEntryAsynchronously(this);
		}
	}

	@Override
	public void onErrorLoadingKeystore(final String msg, final Throwable t) {
		showErrorDialog(ErrorManager.getErrorMessage(ErrorManager.ERROR_ESTABLISHING_KEYSTORE));
	}

	@Override
	public synchronized void keySelected(final KeySelectedEvent kse) {

		showProgressDialog(getString(R.string.dialog_msg_authenticating),null,null);

		final String alias;
		final byte[] certEncoded;
		try {
			alias = kse.getCertificateAlias();
			certEncoded = kse.getCertificateEncoded();
		}
		catch (final KeyChainException e) {
			if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				Log.e(SFConstants.LOG_TAG, "Error al obtener la clave privada del certificado en Android 4.1.X (asegurese de que no contiene caracteres no validos en el alias): " + e); //$NON-NLS-1$
				showErrorDialog(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE_ANDROID_4_1));
			}
			else {
				Log.e(SFConstants.LOG_TAG, "Error al obtener la clave privada del certificado: " + e); //$NON-NLS-1$
				showErrorDialog(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE));
			}
			return;
		}
		catch (final KeyStoreException e) {
			// Este caso se da cuando el usuario cancela el acceso al almacen o la seleccion de
			// un certificado. En el primer caso es posible que la activity se considere cerrada
			// asi que no se puede mostrar un dialogo de error. Nos limitamos a quitar el de espera.
			Log.e(SFConstants.LOG_TAG, "El usuario no selecciono un certificado: " + e); //$NON-NLS-1$
			dismissProgressDialog();
			return;
		}
		// Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
		// en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
		catch (final Throwable e) {
			Log.e(SFConstants.LOG_TAG, "Error al obtener el certificado para la autenticacion: " + e); //$NON-NLS-1$
			e.printStackTrace();
			showErrorDialog(ErrorManager.getErrorMessage(ErrorManager.ERROR_PKE));
			return;
		}

		dismissProgressDialog();

		final LoadConfigurationDataTask lcdt = new LoadConfigurationDataTask(Base64.encode(certEncoded), alias,
				CommManager.getInstance(), this, this);
		showProgressDialog(getString(R.string.dialog_msg_configurating),null,lcdt);
		lcdt.execute();

	}

	// Definimos el menu de opciones de la aplicacion, cuyas opciones estan definidas
    // en el fichero activity_petition_list_options_menu.xml
    @Override
    public boolean onCreateOptionsMenu(final Menu menu) {
    	getMenuInflater().inflate(R.menu.activity_login_options_menu, menu);
        return true;
    }

    // Definimos que hacer cuando se pulsa una opcion del menu de opciones de la aplicacion
    @Override
    public boolean onOptionsItemSelected(final MenuItem item) {
    	if (item.getItemId() == R.id.configuration) {

    		final LoginOptionsDialogBuilder dialogBuilder = new LoginOptionsDialogBuilder(this, this);
    		dialogBuilder.show();
    	}

    	return true;
    }

	/**
	 * Muestra un mensaje de advertencia al usuario.
	 * @param message Mensaje que se desea mostrar.
	 */
	private void showErrorDialog(final String message) {

		dismissProgressDialog();

		if (this.messageDialog == null) {
			this.messageDialog = new MessageDialog(message, null, this);
		}
		this.messageDialog.setMessage(message);
		this.messageDialog.setTitle(getString(R.string.error));
		try {
			runOnUiThread(new Runnable() {
				@Override
				public void run() {
					getMessageDialog().show(getSupportFragmentManager(), "ErrorDialog"); //$NON-NLS-1$;
				}
			});
		} catch (final Exception e) {
			Log.e(SFConstants.LOG_TAG, "No se ha podido mostrar el mensaje de error: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}
	}



	/** Muestra un di&aacute;logo de espera con un mensaje. */
	private void showProgressDialog(final String message, final LoadKeyStoreManagerTask lksmt,
								final LoadConfigurationDataTask lcdt) {
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				try {
					//TODO: Descomentar si se quiere un titulo en el dialogo de espera
					//setProgressDialog(ProgressDialog.show(LoginActivity.this, LoginActivity.this.getString(R.string.loading), message, true));
					setProgressDialog(ProgressDialog.show(LoginActivity.this, null, message, true));
					getProgressDialog().setOnKeyListener(new OnKeyListener() {
						@Override
						public boolean onKey(final DialogInterface dialog, final int keyCode, final KeyEvent event) {
							if (keyCode == KeyEvent.KEYCODE_BACK) {
								if(lksmt!=null){
									lksmt.cancel(true);
								}else if(lcdt!=null){
									lcdt.cancel(true);
								}
								dismissProgressDialog();
								return true;
							}
							return false;
						}
					});

				}catch (final Exception e) {
					Log.e(SFConstants.LOG_TAG, "No se ha podido mostrar el dialogo de progreso: " + e); //$NON-NLS-1$
				}
			}
		});
	}


	/** Cierra el di&aacute;logo de espera en caso de estar abierto. */
	void dismissProgressDialog() {
		if (getProgressDialog() != null) {
			runOnUiThread(new Runnable() {
				@Override
				public void run() {
					getProgressDialog().dismiss();
				}
			});
		}
	}

	@Override
	protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
		super.onActivityResult(requestCode, resultCode, data);

		if (requestCode == SELECT_CERT_REQUEST_CODE && resultCode == RESULT_OK) {

			final String filename = data.getStringExtra(FileChooserActivity.RESULT_DATA_STRING_FILENAME);

			int n = 0;
			final byte[] buffer = new byte[1024];
			final ByteArrayOutputStream baos;
			try {
				baos = new ByteArrayOutputStream();
				final InputStream is = new FileInputStream(filename);
				while ((n = is.read(buffer)) > 0) {
					baos.write(buffer, 0, n);
				}
				is.close();
			} catch (final IOException e) {
				showErrorDialog(getString(R.string.error_loading_selected_file, filename));
				Log.e(SFConstants.LOG_TAG, "Error al cargar el fichero: " + e.toString()); //$NON-NLS-1$
				e.printStackTrace();
				return;
			}

			final Intent intent = KeyChain.createInstallIntent();
			intent.putExtra(KeyChain.EXTRA_PKCS12, baos.toByteArray());
			startActivity(intent);
		}
	}
	@Override
	public void configurationLoadSuccess(final RequestAppConfiguration appConfig, final String certEncoded, final String certAlias) {
		access(certEncoded, certAlias, appConfig);
	}
	@Override
	public void configurationLoadError(final Throwable t) {

		dismissProgressDialog();

		showErrorDialog(getString(R.string.error_loading_app_configuration));
	}

	/**
	 * Valida la identidad del usuario y da acceso al portafirmas.
	 * @param certEncodedB64 Certificado de autenticaci&oacute;n codificado en Base64 .
	 */
	private void access(final String certEncodedB64, final String alias, final RequestAppConfiguration appConfig) {

		dismissProgressDialog();

		final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
		intent.setClass(this, PetitionListActivity.class);
		intent.putExtra(PetitionListActivity.EXTRA_RESOURCE_CERT_B64, certEncodedB64);
		intent.putExtra(PetitionListActivity.EXTRA_RESOURCE_CERT_ALIAS, alias);
		intent.putExtra(PetitionListActivity.EXTRA_RESOURCE_APP_IDS, appConfig.getAppIdsList().toArray(new String[appConfig.getAppIdsList().size()]));
		intent.putExtra(PetitionListActivity.EXTRA_RESOURCE_APP_NAMES, appConfig.getAppNamesList().toArray(new String[appConfig.getAppNamesList().size()]));
		startActivity(intent);
	}

	//metodo vacio para evitar bugs en versiones superiores al api11
	@Override
	protected void onSaveInstanceState(final Bundle outState) {
	    //No call for super(). Bug on API Level > 11.
	}

	@Override
	public void onErrorLoginOptions(final String url) {
		try {
			Toast.makeText(this, getString(R.string.invalid_url), Toast.LENGTH_LONG).show();
		} catch (final Exception e) {
			Log.e(SFConstants.LOG_TAG, "No se ha podido mostrar el mensaje de error por configuracion incorrecta: " + e); //$NON-NLS-1$
			e.printStackTrace();
		}
	}
}
