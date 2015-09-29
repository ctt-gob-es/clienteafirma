package es.gob.afirma.android;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.GeneralSecurityException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.HashMap;

import android.app.PendingIntent;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.hardware.usb.UsbConstants;
import android.hardware.usb.UsbDevice;
import android.hardware.usb.UsbManager;
import android.os.Build;
import android.os.Bundle;
import android.security.KeyChainException;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.view.KeyEvent;
import android.widget.Toast;
import es.gob.afirma.R;
import es.gob.afirma.android.crypto.CipherDataManager;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.android.crypto.MSCBadPinException;
import es.gob.afirma.android.crypto.MobileKeyStoreManager;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;
import es.gob.afirma.android.crypto.SignTask;
import es.gob.afirma.android.crypto.SignTask.SignListener;
import es.gob.afirma.android.gui.DownloadFileTask;
import es.gob.afirma.android.gui.DownloadFileTask.DownloadDataListener;
import es.gob.afirma.android.gui.SendDataTask;
import es.gob.afirma.android.gui.SendDataTask.SendDataListener;
import es.gob.afirma.android.network.AndroidUrlHttpManager;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.signers.AOSignConstants;

/** Actividad dedicada a la firma de los datos recibidos en la entrada mediante un certificado
 * del almac&eacute;n central seleccionado por el usuario. */
public final class SignDataActivity extends FragmentActivity implements KeystoreManagerListener,
                                                                        PrivateKeySelectionListener,
                                                                        DownloadDataListener,
                                                                        SendDataListener,
                                                                        SignListener {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private static final String OK_SERVER_RESULT = "OK"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	private final static String EXTRA_RESOURCE_TITLE = "es.gob.afirma.android.title"; //$NON-NLS-1$
	private final static String EXTRA_RESOURCE_EXT = "es.gob.afirma.android.exts"; //$NON-NLS-1$
	private final static String EXTRA_RESOURCE_EXCLUDE_DIRS = "es.gob.afirma.android.excludedDirs"; //$NON-NLS-1$

	/** Codigo de petici\u00F3n usado para invocar a la actividad que selecciona el fichero para firmar. */
	private static final int SELECT_FILE_REQUEST_CODE = 1;

	private boolean fileChooserOpenned;

	private UrlParametersToSign parameters;

	private DownloadFileTask downloadFileTask = null;

	private MessageDialog messageDialog;
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

	private UsbManager usbManager = null;
	UsbManager getUsbManager() {
		return this.usbManager;
	}
	void setUsbManager(final UsbManager usbMgr) {
		this.usbManager = usbMgr;
	}

	private UsbDevice usbDevice = null;
	UsbDevice getUsbDevice() {
		return this.usbDevice;
	}
	void setUsbDevice(final UsbDevice usbDev) {
		this.usbDevice = usbDev;
	}

	private static final String ACTION_USB_PERMISSION = "es.gob.afirma.android.USB_PERMISSION"; //$NON-NLS-1$

	private final BroadcastReceiver mUsbReceiver = new BroadcastReceiver() {
		@Override
		public void onReceive(final Context context, final Intent intent) {
			final String action = intent.getAction();
			if (ACTION_USB_PERMISSION.equals(action)) {
				synchronized (this) {

					Log.d(ES_GOB_AFIRMA, "Comprobamos el permiso de acceso al lector USB"); //$NON-NLS-1$

					// Si no se concedio el permiso
					if (!intent.getBooleanExtra(UsbManager.EXTRA_PERMISSION_GRANTED, false)) {
						SignDataActivity.this.setUsbManager(null);
						SignDataActivity.this.setUsbDevice(null);
						Log.d(ES_GOB_AFIRMA, "Permiso denegado para el acceso a USB"); //$NON-NLS-1$
					}
					// Ya sea con dispositivo con sin el, se continua la ejecucion cargando el almacen
					new LoadKeyStoreManagerTask(
						SignDataActivity.this,
						SignDataActivity.this,
						SignDataActivity.this.getUsbDevice(),
						SignDataActivity.this.getUsbManager()
					).execute();
				}
			}
		}
	};

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (getIntent() == null || getIntent().getData() == null) {
			Log.w(ES_GOB_AFIRMA, "No se han indicado parametros de entrada para la actividad");  //$NON-NLS-1$
			closeActivity();
			return;
		}

		// Establecemos el layout con la interfaz
		setContentView(R.layout.activity_sign_data);

		// Aseguramos que se muestra el panel con el mensaje de espera
		this.progressDialog = ProgressDialog.show(
			this, "", getString(R.string.dialog_msg_loading_data), true //$NON-NLS-1$
		);

		Log.d(ES_GOB_AFIRMA, "URI de invocacion: " + getIntent().getDataString()); //$NON-NLS-1$

		try {
			this.parameters = ProtocolInvocationUriParser.getParametersToSign(getIntent().getDataString());
		}
		catch (final ParameterException e) {
			Log.e(ES_GOB_AFIRMA, "Error en los parametros de firma: " + e.toString(), e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
		}
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, "Error grave en el onCreate de SignDataActivity: " + e.toString(), e); //$NON-NLS-1$
			e.printStackTrace();
			showErrorMessage(getString(R.string.error_bad_params));
		}

		UrlHttpManagerFactory.install(new AndroidUrlHttpManager());
	}

	@Override
	public void onStart() {
		super.onStart();

		//EasyTracker.getInstance().activityStart(this);

		if (this.parameters != null) {

			// Si no tenemos datos ni un fichero de descargar, cargaremos un fichero del dispositivo
			if (this.parameters.getData() == null && this.parameters.getFileId() == null) {
				Log.i(ES_GOB_AFIRMA, "Se va a cargar un fichero local para la firma"); //$NON-NLS-1$
				// Comprobamos que no este ya abierta la pantalla de seleccion, ya que puede ser un caso
				// de cancelacion de la seleccion de fichero, en cuyo caso no deseamos que se vuelva a abrir
				if (!this.fileChooserOpenned) {
					openSelectFileActivity();
				}
				else {
					this.fileChooserOpenned = false;
				}
			}
			// Si no se han indicado datos y si el identificador de un fichero remoto, lo recuperamos para firmarlos
			else if (this.parameters.getData() == null && this.parameters.getFileId() != null) {
				Log.i(ES_GOB_AFIRMA, "Se van a descargar los datos desde servidor con el identificador: " + this.parameters.getFileId()); //$NON-NLS-1$
				this.downloadFileTask = new DownloadFileTask(
					this.parameters.getFileId(),
					this.parameters.getRetrieveServletUrl(),
					this
				);
				this.downloadFileTask.execute();
			}
			// Si tenemos los datos, cargamos un certificado para firmarlos
			else {
				Log.i(ES_GOB_AFIRMA, "Se inicia la firma de los datos obtenidos por parametro"); //$NON-NLS-1$
				loadKeyStore();
			}
		}
	}

	private void loadKeyStore() {

		// Si ya tenemos los datos para la firma (no antes), buscamos si hay dispositivos CCID USB conectados
		if (this.parameters.getData() != null || this.parameters.getFileId() != null) {
			final UsbManager usbMgr = (UsbManager) getSystemService(Context.USB_SERVICE);
			final HashMap<String, UsbDevice> devices = usbMgr.getDeviceList();
			for (final UsbDevice dev : devices.values()) {
				if (dev.getDeviceClass() == 0 && dev.getInterface(0).getInterfaceClass() == UsbConstants.USB_CLASS_CSCID) {
					this.usbManager = usbMgr;
					this.usbDevice = dev;
					break;
				}
			}
		}

		// Si es igual a null es que no hay un CCID conectado
		if (this.usbManager == null) {
			// Cargamos el almacen de certificados normalmente
			new LoadKeyStoreManagerTask(this, this).execute();
		}

		//Si no, pedimos acceso al dispositivo
		else {
			askForUsbPermission();
		}
	}

	private void askForUsbPermission(){
		final PendingIntent mPermissionIntent = PendingIntent.getBroadcast(this, 0, new Intent(ACTION_USB_PERMISSION), 0);
		final IntentFilter filter = new IntentFilter(ACTION_USB_PERMISSION);
		registerReceiver(this.mUsbReceiver, filter);

		this.usbManager.requestPermission(this.usbDevice, mPermissionIntent);
	}

	/** Identifica las extensiones de los ficheros que se pueden firmar con un formato de firma.
	 * @param signatureFormat Formato de firma.
	 * @return Extensiones. */
	private static String identifyExts(final String signatureFormat) {

		if (AOSignConstants.SIGN_FORMAT_PADES.equals(signatureFormat) ||
			AOSignConstants.SIGN_FORMAT_PADES_TRI.equals(signatureFormat)) {
				return ".pdf"; //$NON-NLS-1$
		}
		return null;
	}

	private void doSign(final PrivateKeyEntry pke) {

		if (pke == null) {
			throw new IllegalArgumentException("La entrada a la clave privada no puede ser nula"); //$NON-NLS-1$
		}

		new SignTask(
			this.parameters.getOperation(),
			this.parameters.getData(),
			this.parameters.getSignatureFormat(),
			this.parameters.getSignatureAlgorithm(),
			pke,
			this.parameters.getExtraParams(),
			this
		).execute();
	}

	/** Env&iacute;a los datos indicado a un servlet. En caso de error, cierra la aplicaci&oacute;n.
	 * @param data Datos que se desean enviar.
	 * @param needCloseApp <code>true</code> si la aplicaci&oacute;n debe cerrarse el finalizar la operaci&oacute;n,
	 *                     <code>false</code> en caso contrario. */
	private void sendData(final String data, final boolean critical, final boolean needCloseApp) {

		Log.i(ES_GOB_AFIRMA, "Se almacena el resultado en el servidor con el Id: " + this.parameters.getId()); //$NON-NLS-1$

		new SendDataTask(
			this.parameters.getId(),
			this.parameters.getStorageServletUrl().toExternalForm(),
			data,
			this,
			critical,
			needCloseApp
		).execute();
	}

	/** Muestra un mensaje de error y lo env&iacute;a al servidor para que la p&aacute;gina Web
	 * tenga constancia de &eacute;l.
	 * @param errorId Identificador del error.
	 * @param needCloseApp <code>true</code> si el error implica que la aplicaci&oacute;n deba cerrarse,
	 *                     <code>false</code> en caso contrario. */
	private void launchError(final String errorId, final boolean critical, final boolean needCloseApp) {
		try {
			sendData(URLEncoder.encode(ErrorManager.genError(errorId, null), DEFAULT_URL_ENCODING), critical, needCloseApp);
		}
		catch (final UnsupportedEncodingException e) {
			// No puede darse, el soporte de UTF-8 es obligatorio
			Log.e(ES_GOB_AFIRMA,
				"No se ha podido enviar la respuesta al servidor por error en la codificacion " + DEFAULT_URL_ENCODING, e //$NON-NLS-1$
			);
		}
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA,
				"Error desconocido al enviar el error obtenido al servidor: " + e, e //$NON-NLS-1$
			);
		}
	}

	/** Muestra un mensaje de advertencia al usuario.
	 * @param message Mensaje que se desea mostrar. */
	private void showErrorMessage(final String message) {

		dismissProgressDialog();

		if (this.messageDialog == null) {
			this.messageDialog = new MessageDialog(message, new CloseActivityDialogAction(), this);
		}
		this.messageDialog.setMessage(message);

		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				try {
					SignDataActivity.this.getMessageDialog().show(getSupportFragmentManager(), "ErrorDialog"); //$NON-NLS-1$;
				}
				catch (final Exception e) {
					// Si falla el mostrar el error (posiblemente por no disponer de un contexto grafico para mostrarlo)
					// se mostrara en un
					Toast.makeText(SignDataActivity.this, message, Toast.LENGTH_LONG).show();
				}

			}
		});
	}

	/** Muestra un mensaje de advertencia al usuario.
	 * @param message Mensaje que se desea mostrar. */
	private void showErrorMessageOnToast(final String message) {

		dismissProgressDialog();
		dismissMessageDialog();

		runOnUiThread(
			new Runnable() {
				@Override
				public void run() {
					Toast.makeText(SignDataActivity.this, message, Toast.LENGTH_LONG).show();
				}
			}
		);
	}

	@Override
	public synchronized void setKeyStoreManager(final MobileKeyStoreManager msm) {

		dismissProgressDialog();

		if(msm == null){
			Log.w(ES_GOB_AFIRMA,"Error al establecer el almacen de certificados. Es posible que el usuario cancelase la operacion."); //$NON-NLS-1$
			launchError(ErrorManager.ERROR_ESTABLISHING_KEYSTORE, true, true);
		}
		else {
			msm.getPrivateKeyEntryAsynchronously(this);
		}
	}

	@Override
	public void onErrorLoadingKeystore(final String msg, final Throwable t) {
		launchError(ErrorManager.ERROR_ESTABLISHING_KEYSTORE, true, true);
	}

	@Override
	public synchronized void keySelected(final KeySelectedEvent kse) {

		showProgressDialog(getString(R.string.dialog_msg_signning));

		final PrivateKeyEntry pke;
		try {
			pke = kse.getPrivateKeyEntry();
		}
		catch (final KeyChainException e) {
			Log.e(ES_GOB_AFIRMA, "Error en la recuperacion de la clave del almacen: " + e, e); //$NON-NLS-1$
			if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				launchError(ErrorManager.ERROR_PKE_ANDROID_4_1, true, true);
			}
			else {
				launchError(ErrorManager.ERROR_PKE, true, true);
			}
			return;
		}
		catch (final AOCancelledOperationException e) {
			Log.e(ES_GOB_AFIRMA, "El usuario no selecciono un certificado", e); //$NON-NLS-1$
			launchError(ErrorManager.ERROR_CANCELLED_OPERATION, false, true);
			return;
		}
		catch (final Exception e) {
			Log.e(ES_GOB_AFIRMA, "Error al recuperar la clave del certificado de firma", e); //$NON-NLS-1$
			e.printStackTrace();
			launchError(ErrorManager.ERROR_PKE, true, true);
			return;
		}
		// Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
		// en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, "Error al importar la clave. Si es un AssertionError puede tratarse de un problema de Android", e); //$NON-NLS-1$
			e.printStackTrace();
			return;
		}

		doSign(pke);
	}

	private void showProgressDialog(final String message) {
		runOnUiThread(
			new Runnable() {
				@Override
				public void run() {
					try {
						setProgressDialog(ProgressDialog.show(SignDataActivity.this, "", message, true)); //$NON-NLS-1$
					}
					catch (final Throwable e) {
						Log.e(ES_GOB_AFIRMA, "No se ha podido mostrar el dialogo de progreso", e); //$NON-NLS-1$
					}
				}
			}
		);
	}

	@Override
	public synchronized void processData(final byte[] data) {

		Log.i(ES_GOB_AFIRMA, "Se ha descargado correctamente la configuracion de firma almacenada en servidor"); //$NON-NLS-1$
		Log.i(ES_GOB_AFIRMA, "Cantidad de datos descargada: " + (data == null ? -1 : data.length)); //$NON-NLS-1$

		// Si hemos tenido que descargar los datos desde el servidor, los desciframos y llamamos
		// al dialogo de seleccion de certificados para la firma
		byte[] decipheredData;
		try {
			decipheredData = CipherDataManager.decipherData(data, this.parameters.getDesKey());
		}
		catch (final IOException e) {
			Log.e(ES_GOB_AFIRMA, "Los datos proporcionados no est&aacute;n correctamente codificados en base 64", e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}
		catch (final GeneralSecurityException e) {
			Log.e(ES_GOB_AFIRMA, "Error al descifrar los datos recuperados del servidor para la firma", e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}
		catch (final IllegalArgumentException e) {
			Log.e(ES_GOB_AFIRMA, "Los datos recuperados no son un base64 valido", e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, "Error desconocido durante el descifrado de los datos", e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}

		Log.i(ES_GOB_AFIRMA, "Se han descifrado los datos y se inicia su analisis"); //$NON-NLS-1$

		try {
			this.parameters = ProtocolInvocationUriParser.getParametersToSign(decipheredData);
		}
		catch (final ParameterException e) {
			Log.e(ES_GOB_AFIRMA, "Error en los parametros XML de configuracion de firma: " + e.toString(), e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, "Error desconocido al analizar los datos descargados desde el servidor", e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_bad_params));
			return;
		}

		Log.i(ES_GOB_AFIRMA, "Se inicia la firma de los datos descargados desde el servidor"); //$NON-NLS-1$
		loadKeyStore();
	}

	@Override
	public synchronized void onErrorDownloadingData(final String msg, final Throwable t) {
		Log.e(ES_GOB_AFIRMA, "Error durante la descarga de la configuracion de firma guardada en servidor:" + msg + (t != null ? ": " + t.toString() : ""), t); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		showErrorMessage(getString(R.string.error_server_connect));
	}

	@Override
	public void onSignSuccess(final byte[] signature) {

		// Ciframos si nos dieron clave privada, si no subimos los datos sin cifrar
		final String data;
		try {
			data = CipherDataManager.cipherData(signature, this.parameters.getDesKey());
		}
		catch (final IOException e) {
			Log.e(ES_GOB_AFIRMA, "Error al codificar los datos cifrados", e); //$NON-NLS-1$
			launchError(ErrorManager.ERROR_CODING_BASE64, true, true);
			return;
		}
		catch (final GeneralSecurityException e) {
			Log.e(ES_GOB_AFIRMA, "Error en el cifrado de la firma", e); //$NON-NLS-1$
			launchError(ErrorManager.ERROR_CIPHERING, true, true);
			return;
		}
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, "Error desconocido al cifrar el resultado de la firma", e); //$NON-NLS-1$
			launchError(ErrorManager.ERROR_CIPHERING, true, true);
			return;
		}

		sendData(data, true, true);
	}

	@Override
	public void onSignError(final Throwable t) {
		if (t instanceof MSCBadPinException) {
			showErrorMessage(getString(R.string.error_msc_pin));
			launchError(ErrorManager.ERROR_MSC_PIN, false, false);
		}
		else if (t instanceof AOUnsupportedSignFormatException) {
			showErrorMessage(getString(R.string.error_format_not_supported));
			launchError(ErrorManager.ERROR_NOT_SUPPORTED_FORMAT, true, true);
		}
		else {
			launchError(ErrorManager.ERROR_SIGNING, true, true);
		}
	}

	@Override
	public void onSuccessSendingData(final byte[] result, final boolean critical, final boolean needCloseApp) {
		Log.i(ES_GOB_AFIRMA, "Resultado del deposito de la firma: " + (result == null ? null : new String(result))); //$NON-NLS-1$

		if (result == null || !new String(result).trim().equals(OK_SERVER_RESULT)) {
			Log.e(ES_GOB_AFIRMA, "No se pudo entregar la firma al servlet: " + (result == null ? null : new String(result))); //$NON-NLS-1$
			if (critical) {
				showErrorMessage(getString(R.string.error_sending_data));
				return;
			}
		} else {
			Log.i(ES_GOB_AFIRMA, "Resultado entregado satisfactoriamente."); //$NON-NLS-1$
		}
		if (needCloseApp) {
			closeActivity();
		}
	}

	@Override
	public void onErrorSendingData(final Throwable error, final boolean critical, final boolean needCloseApp) {

		Log.e(ES_GOB_AFIRMA, "Se ejecuta la funcion de error en el envio de datos", error); //$NON-NLS-1$
		error.printStackTrace();

		if (critical) {
			dismissProgressDialog();

			showErrorMessage(getString(R.string.error_sending_data));
			return;
		}

		if (needCloseApp) {
			closeActivity();
		}
	}

	/** Comprueba si esta abierto el di&aacute;logo de espera y lo cierra en dicho caso. */
	private void dismissProgressDialog() {
		if (this.progressDialog != null) {
			this.progressDialog.dismiss();
		}
	}

	/** Comprueba si esta abierto el di&aacute;logo de mensajes y lo cierra en dicho caso. */
	private void dismissMessageDialog() {
		if (this.messageDialog != null && this.messageDialog.isVisible()) {
					this.messageDialog.dismiss();
		}
	}

	@Override
	protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
		super.onActivityResult(requestCode, resultCode, data);

		if (requestCode == SELECT_FILE_REQUEST_CODE) {
			// Si el usuario cancelo la seleccion del fichero a firmar
			if (resultCode == RESULT_CANCELED) {
				launchError(ErrorManager.ERROR_CANCELLED_OPERATION, false, true);
				return;
			}
			else if (resultCode == RESULT_OK) {

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
				}
				catch (final IOException e) {
					Log.e(ES_GOB_AFIRMA, "Error al cargar el fichero, se dara al usuario la posibilidad de reintentar", e); //$NON-NLS-1$
					showErrorMessageOnToast(getString(R.string.error_loading_selected_file, filename));
					openSelectFileActivity();
					return;
				}
				catch (final Throwable e) {
						Log.e(ES_GOB_AFIRMA, "Error desconocido al cargar el fichero", e); //$NON-NLS-1$
						showErrorMessageOnToast(getString(R.string.error_loading_selected_file, filename));
						e.printStackTrace();
						return;
					}

				this.parameters.setData(baos.toByteArray());

				// Tras esto vuelve a ejecutarse el onStart() de la actividad pero ya con datos para firmar
			}
		}
	}

	private void openSelectFileActivity() {

		final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
		intent.setClass(this, FileChooserActivity.class);
		intent.putExtra(EXTRA_RESOURCE_TITLE, getString(R.string.title_activity_choose_sign_file));
		intent.putExtra(EXTRA_RESOURCE_EXCLUDE_DIRS, MainActivity.COMMON_EXCLUDED_DIRS);
		final String exts = identifyExts(this.parameters.getSignatureFormat());
		if (exts != null) {
			intent.putExtra(EXTRA_RESOURCE_EXT, exts);
		}

		this.fileChooserOpenned = true;
		startActivityForResult(intent, SELECT_FILE_REQUEST_CODE);
	}

	/** Accion para el cierre de la actividad. */
	private class CloseActivityDialogAction implements DialogInterface.OnClickListener {

		CloseActivityDialogAction() {
			// Constructor vacio para evitar el sintetico
		}

		@Override
		public void onClick(final DialogInterface dialog, final int which) {
			closeActivity();
		}
	}

	static void closeActivity() {
//		finish();
		// Cerramos a la fuerza para, en siguientes ejecuciones, no se vuelvan a cargar los mismos datos
		System.exit(0);
	}

	@Override
	public boolean onKeyDown(final int keyCode, final KeyEvent event) {
		if(keyCode == KeyEvent.KEYCODE_HOME) {
			launchError(ErrorManager.ERROR_CANCELLED_OPERATION, false, true);
		}
		return super.onKeyDown(keyCode, event);
	}


	@Override
	public void onBackPressed() {
		launchError(ErrorManager.ERROR_CANCELLED_OPERATION, false, true);
		super.onBackPressed();
	}

	@Override
	protected void onStop() {
		dismissProgressDialog();
		dismissMessageDialog();
		//EasyTracker.getInstance().activityStop(this);
		super.onStop();
	}

	@Override
	protected void onDestroy() {
		if (this.downloadFileTask != null) {
			Log.d(ES_GOB_AFIRMA, "SignDataActivity onDestroy: Cancelamos la descarga"); //$NON-NLS-1$
			try {
				this.downloadFileTask.cancel(true);
			}
			catch(final Exception e) {
				Log.e(ES_GOB_AFIRMA, "No se ha podido cancelar el procedimiento de descarga de los datos", e); //$NON-NLS-1$
			}
		}
		super.onDestroy();
	}
}