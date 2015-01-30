package es.gob.afirma.android;

import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.HashMap;
import java.util.Locale;

import android.app.PendingIntent;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.hardware.usb.UsbConstants;
import android.hardware.usb.UsbDevice;
import android.hardware.usb.UsbManager;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.security.KeyChainException;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.google.analytics.tracking.android.EasyTracker;

import es.gob.afirma.R;
import es.gob.afirma.android.ReadLocalFileTask.ReadLocalFileListener;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.android.crypto.MSCBadPinException;
import es.gob.afirma.android.crypto.MobileKeyStoreManager;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.KeySelectedEvent;
import es.gob.afirma.android.crypto.MobileKeyStoreManager.PrivateKeySelectionListener;
import es.gob.afirma.android.crypto.SignTask;
import es.gob.afirma.android.crypto.SignTask.SignListener;
import es.gob.afirma.android.network.AndroidUrlHttpManager;
import es.gob.afirma.android.network.UriParser;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignerFactory;

/** Esta actividad permite firmar un fichero local. La firma se guarda en un fichero .csig.
 * Esta clase tiene mucho c&oacute;fdigo duplicado de la clase LocalSignResultActivity.
 * Hay crear una nueva clase con los m&ecute;todos duplicados.
 * @author Astrid Idoate Gil. */
public final class LocalSignResultActivity extends FragmentActivity implements KeystoreManagerListener,
		                                                                       PrivateKeySelectionListener,
		                                                                       SignListener {

	private final static String EXTRA_RESOURCE_TITLE = "es.gob.afirma.android.title"; //$NON-NLS-1$

	private final static int SELECT_FILE_REQUEST_CODE = 1;

	private static final String DEFAULT_SIGNATURE_ALGORITHM = "SHA1withRSA"; //$NON-NLS-1$

	private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$

	private final static String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private final static String SAVE_INSTANCE_KEY_TITLE_VISIBILITY = "titleVisibility"; //$NON-NLS-1$
	private final static String SAVE_INSTANCE_KEY_OK_RESULT_VISIBILITY = "okVisibility"; //$NON-NLS-1$
	private final static String SAVE_INSTANCE_KEY_ERROR_RESULT_VISIBILITY ="errorVisibility"; //$NON-NLS-1$
	private final static String SAVE_INSTANCE_KEY_OK_TEXT = "okMessage"; //$NON-NLS-1$
	private final static String SAVE_INSTANCE_KEY_ERROR_TEXT = "errorMessage"; //$NON-NLS-1$

	String fileName; //Nombre del fichero seleccionado

	private byte[] dataToSign;
	private PrivateKeyEntry pke;

	private String format = null;

	private ProgressDialog progressDialog;
	void setProgressDialog(final ProgressDialog pd) {
		this.progressDialog = pd;
	}
	ProgressDialog getProgressDialog() {
		return this.progressDialog;
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
						LocalSignResultActivity.this.setUsbManager(null);
						LocalSignResultActivity.this.setUsbDevice(null);
						Log.d(ES_GOB_AFIRMA, "Permiso denegado para el acceso a USB"); //$NON-NLS-1$
					}
					// Ya sea con dispositivo con sin el, se continua la ejecucion cargando el almacen
					new LoadKeyStoreManagerTask(
							LocalSignResultActivity.this,
							LocalSignResultActivity.this,
							LocalSignResultActivity.this.getUsbDevice(),
							LocalSignResultActivity.this.getUsbManager()
							).execute();
				}
			}
		}
	};

	private void askForUsbPermission(){
		final PendingIntent mPermissionIntent = PendingIntent.getBroadcast(this, 0, new Intent(ACTION_USB_PERMISSION), 0);
		final IntentFilter filter = new IntentFilter(ACTION_USB_PERMISSION);
		registerReceiver(this.mUsbReceiver, filter);
		this.usbManager.requestPermission(this.usbDevice, mPermissionIntent);
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_signed_file);

		if (savedInstanceState != null && savedInstanceState.containsKey(SAVE_INSTANCE_KEY_TITLE_VISIBILITY) && savedInstanceState.getBoolean(SAVE_INSTANCE_KEY_TITLE_VISIBILITY)) {
			findViewById(R.id.signedfile_title).setVisibility(View.VISIBLE);

			((TextView) findViewById(R.id.tv_signedfile_ko)).setText(savedInstanceState.getString(SAVE_INSTANCE_KEY_ERROR_TEXT));

			findViewById(R.id.signedfile_error).setVisibility(
					savedInstanceState.getBoolean(SAVE_INSTANCE_KEY_ERROR_RESULT_VISIBILITY) ? View.VISIBLE : View.INVISIBLE);

			((TextView) findViewById(R.id.filestorage_path)).setText(savedInstanceState.getString(SAVE_INSTANCE_KEY_OK_TEXT));

			findViewById(R.id.signedfile_correct).setVisibility(
					savedInstanceState.getBoolean(SAVE_INSTANCE_KEY_OK_RESULT_VISIBILITY) ? View.VISIBLE : View.INVISIBLE);
		}
		else {
			// Elegimos un fichero del directorio
			final Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
			intent.setClass(this, FileChooserActivity.class);
			intent.putExtra(EXTRA_RESOURCE_TITLE, getString(R.string.title_activity_choose_sign_file));
			intent.putExtra("es.gob.afirma.android.excludedDirs", MainActivity.COMMON_EXCLUDED_DIRS); //$NON-NLS-1$
			startActivityForResult(intent, SELECT_FILE_REQUEST_CODE);
		}

		UrlHttpManagerFactory.install(new AndroidUrlHttpManager());
	}

	@Override
	  public void onStart() {
	    super.onStart();
	    EasyTracker.getInstance().activityStart(this); // Instanciacion de Google Analytics
	}

	@Override
	  public void onStop() {
	    super.onStop();

	    dismissProgressDialog();

	    EasyTracker.getInstance().activityStop(this); // Parada de Google Analytics
	}

	void finishLocalSign(final byte[] data) {
		if (data == null) {
			showErrorMessage(getString(R.string.error_loading_selected_file, this.fileName));
			return;
		}
		this.dataToSign = data;

		// Abrirmos el KeyStore y firmamos
		loadKeyStore();
	}

	@Override
	protected void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
		super.onActivityResult(requestCode, resultCode, data);

		if (requestCode == SELECT_FILE_REQUEST_CODE && resultCode == RESULT_OK) {
			this.fileName = data.getStringExtra(FileChooserActivity.RESULT_DATA_STRING_FILENAME);

			// Aseguramos que se muestra el panel con el mensaje de espera
			showProgressDialog(getString(R.string.dialog_msg_loading_data));

			new ReadLocalFileTask(
				new ReadLocalFileListener() {
					@Override
					public void setData(final Object readedData) {
						if (readedData instanceof OutOfMemoryError) {
							showErrorMessage(
								getString(R.string.file_read_out_of_memory)
							);
						}
						else if (readedData instanceof Exception) {
							showErrorMessage(
								getString(R.string.error_loading_selected_file, LocalSignResultActivity.this.fileName)
							);
						}
						else {
							finishLocalSign((byte[]) readedData);
						}
					}
				}
			).execute(this.fileName);
		}
		else {
			// Se ha cancelado la seleccion del fichero a firmar
			closeActivity();
			return;
		}
	}

	private void showProgressDialog(final String message) {
		runOnUiThread(
			new Runnable() {
				@Override
				public void run() {
					try {
						setProgressDialog(ProgressDialog.show(LocalSignResultActivity.this, "", message, true)); //$NON-NLS-1$
					}
					catch (final Exception e) {
						Log.e(ES_GOB_AFIRMA, "No se ha podido mostrar el dialogo de progreso: " + e); //$NON-NLS-1$
					}
				}
			}
		);
	}

	private void loadKeyStore() {

		// Buscamos si hay dispositivos CCID USB conectados
		final UsbManager usbMgr = (UsbManager) getSystemService(Context.USB_SERVICE);
		final HashMap<String, UsbDevice> devices = usbMgr.getDeviceList();
		for (final UsbDevice dev : devices.values()) {
			if (dev.getDeviceClass() == 0 && dev.getInterface(0).getInterfaceClass() == UsbConstants.USB_CLASS_CSCID) {
				this.usbManager = usbMgr;
				this.usbDevice = dev;
				break;
			}
		}

		// Si es igual a null es que no hay un CCID conectado
		if (this.usbManager == null) {
			Log.i(ES_GOB_AFIRMA, "No hay dispositivos CCID USB conectados"); //$NON-NLS-1$
			// Cargamos el almacen de certificados normalmente
			new LoadKeyStoreManagerTask(this, this, null, null).execute();
		}

		//Si no, pedimos acceso al dispositivo
		else {
			Log.i(ES_GOB_AFIRMA, "Se han detectado dispositivos CCID USB conectados"); //$NON-NLS-1$
			askForUsbPermission();
		}
	}

	@Override
	public synchronized void keySelected(final KeySelectedEvent kse) {

		showProgressDialog(getString(R.string.dialog_msg_signning));

		try {
			this.pke = kse.getPrivateKeyEntry();
		}
		catch (final KeyChainException e) {
			if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				Log.e(ES_GOB_AFIRMA, "Error al extraer la clave en Android " + Build.VERSION.RELEASE + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				showErrorMessage(getString(R.string.error_android_4_1));
			}
			else {
				Log.e(ES_GOB_AFIRMA, "No se pudo extraer la clave privada del certificado: " + e); //$NON-NLS-1$
				showErrorMessage(getString(R.string.error_signing_no_key));
			}
			return;
		}
		catch (final AOCancelledOperationException e) {
			Log.e(ES_GOB_AFIRMA, "El usuario no selecciono un certificado: " + e); //$NON-NLS-1$
			closeActivity();
			return;
		}
		catch (final Exception e) {
			Log.e(ES_GOB_AFIRMA, "Error al recuperar la clave del certificado de firma: " + e); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_signing_no_key));
			return;
		}
		// Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
		// en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
		catch (final Throwable e) {
			Log.e(ES_GOB_AFIRMA, e.toString());
			return;
		}

		try {
			doSign(this.pke);
		}
		catch (final Exception e) {
			showErrorMessage(getString(R.string.error_signing_config));
		}
	}

	@Override
	public synchronized void setKeyStoreManager(final MobileKeyStoreManager msm) {

		// Si el usuario cancelo la insercion de PIN o cualquier otro dialogo del almacen
		if(msm == null){
			Log.e(ES_GOB_AFIRMA,"El usuario cancelo el acceso al almacen o la seleccion de certificado"); //$NON-NLS-1$
			closeActivity();
			return;
		}

		dismissProgressDialog();
		msm.getPrivateKeyEntryAsynchronously(this);
	}

	@Override
	public void onErrorLoadingKeystore(final String msg, final Throwable t) {
		showErrorMessage(msg);
	}

	private void doSign(final PrivateKeyEntry keyEntry) {

		if (keyEntry == null) {
			throw new IllegalArgumentException("La entrada a la clave privada no puede ser nula"); //$NON-NLS-1$
		}

		this.format = this.fileName.toLowerCase(Locale.ENGLISH).endsWith(PDF_FILE_SUFFIX) ?
				AOSignConstants.SIGN_FORMAT_PADES : AOSignConstants.SIGN_FORMAT_CADES;

		new SignTask(
			UriParser.OP_SIGN,
			this.dataToSign,
			this.format,
			DEFAULT_SIGNATURE_ALGORITHM,
			keyEntry,
			null,
			this
		).execute();

	}

	//Guarda los datos en un directorio del dispositivo y muestra por pantalla al usuario la informacion indicando donse se ha almacenado el fichero
	private void saveData(final byte[] signature){

		// Comprobamos que tenemos permisos de lectura sobre el directorio en el que se encuentra el fichero origen
		boolean originalDirectory;
		String outDirectory = null;
		if (new File(this.fileName).getParentFile().canWrite()) {
			Log.d(ES_GOB_AFIRMA, "La firma se guardara en el directorio del fichero de entrada"); //$NON-NLS-1$
			outDirectory = new File(this.fileName).getParent();
			originalDirectory = true;
		}
		else if (Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).exists() && Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).canWrite()) {
			Log.d(ES_GOB_AFIRMA, "La firma se guardara en el directorio de descargas"); //$NON-NLS-1$
			outDirectory = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).getAbsolutePath();
			originalDirectory = false;
		}
		else {
			Log.w(ES_GOB_AFIRMA, "No se ha encontrado donde guardar la firma generada"); //$NON-NLS-1$
			showErrorMessage(getString(R.string.error_no_device_to_store));
			return;
		}

		String inText = null;
		if (AOSignConstants.SIGN_FORMAT_PADES.equals(this.format)) {
			inText = "_signed"; //$NON-NLS-1$
		}

		int i = 0;
		final String signatureFilename = AOSignerFactory.getSigner(this.format).getSignedName(new File(this.fileName).getName(), inText);
		String finalSignatureFilename = signatureFilename;
		while (new File(outDirectory, finalSignatureFilename).exists()) {
			finalSignatureFilename = buildName(signatureFilename, ++i);
		}

		try {
			final FileOutputStream fos = new FileOutputStream(new File(outDirectory, finalSignatureFilename));
			fos.write(signature);
			fos.flush();
			fos.close();
		}
		catch (final Exception e) {
			showErrorMessage(getString(R.string.error_saving_signature));
			e.printStackTrace();
			return;
		}

		showSuccessMessage(finalSignatureFilename, outDirectory, originalDirectory);

		//refresco del media scanner despues de guardar el ficheo porque esta dando problemas en la version 4.3
		sendBroadcast(new Intent(Intent.ACTION_MEDIA_MOUNTED,android.net.Uri.fromFile(new File(outDirectory))));
	}

	/** Muestra los elementos de pantalla informando de un error ocurrido durante la operaci&oacute;n de
	 * firma.
	 * @param message Mensaje que describe el error producido. */
	void showErrorMessage(final String message) {

		dismissProgressDialog();

		// Ya cerrados los dialogos modales, mostramos el titulo de la pantalla
		final TextView tvTitle = (TextView) findViewById(R.id.signedfile_title);
		tvTitle.setVisibility(View.VISIBLE);

		//activo los elementos de la interfaz que corresponden a la firma incorrecta de un fichero
		final TextView errorMessage = (TextView) findViewById(R.id.tv_signedfile_ko);
		errorMessage.setText(message);

		final RelativeLayout rl = (RelativeLayout) findViewById(R.id.signedfile_error);
		rl.setVisibility(View.VISIBLE);
	}

	/** Muestra los elementos de pantalla informando de que la firma se ha generado correctamente y
	 * donde se ha almacenado.
	 * @param filename Nombre del fichero almacenado.
	 * @param directory Directorio en el que se ha almacenado la firma. */
	private void showSuccessMessage(final String filename, final String directory, final boolean originalDirectory) {

		dismissProgressDialog();

		// Ya cerrados los dialogos modales, mostramos el titulo de la pantalla
		final TextView tvTitle = (TextView) findViewById(R.id.signedfile_title);
		tvTitle.setVisibility(View.VISIBLE);

		//activo los elementos de la interfaz que corresponden a la firma correcta de un fichero
		final TextView tv_sf= (TextView) findViewById(R.id.filestorage_path);
		tv_sf.setText(getString(originalDirectory ?
				R.string.signedfile_original_location :
					R.string.signedfile_downloads_location, filename));

		final RelativeLayout rl = (RelativeLayout) findViewById(R.id.signedfile_correct);
		rl.setVisibility(View.VISIBLE);
	}

	/** Cierra la actividad. */
	void closeActivity() {
		finish();
	}

	@Override
	public void onSignSuccess(final byte[] signature) {

		// Notificamos a Google Analytics la operacion realizada
		EasyTracker.getTracker().sendEvent(
			"Operacion local", //$NON-NLS-1$
			"Firma realizada", //$NON-NLS-1$
			"Operacion='" + UriParser.OP_SIGN + "', formato='" + this.format + "', algoritmo='" + DEFAULT_SIGNATURE_ALGORITHM + "'", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			Long.valueOf(0L)
		);

		saveData(signature);
	}

	@Override
	public void onSignError(final Throwable t) {

		// Notificamos a Google Analytics la operacion realizada
		EasyTracker.getTracker().sendEvent(
			"Operacion local", //$NON-NLS-1$
			"Firma error", //$NON-NLS-1$
			"Operacion='" + UriParser.OP_SIGN + "', formato='" + this.format + "', algoritmo='" + DEFAULT_SIGNATURE_ALGORITHM + "'", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			Long.valueOf(0L)
		);


		if (t instanceof MSCBadPinException) {
			showErrorMessage(getString(R.string.error_msc_pin));
		}
		else {
			showErrorMessage(getString(R.string.error_signing));
		}
		t.printStackTrace();
	}

	/** Comprueba si esta abierto el di&aacute;logo de espera y lo cierra en dicho caso. */
	private void dismissProgressDialog() {

		if (this.progressDialog != null) {
			this.progressDialog.dismiss();
		}
	}

	/** Construye un nombre apropiado para un fichero de firma en base a un nombre base
	 * y un &iacute;ndice.
	 * @param originalName Nombre base del fichero.
	 * @param index &Iacute;ndice.
	 * @return Nombre apropiado para el fichero de firma. */
	private static String buildName(final String originalName, final int index) {

		String indexSuffix = ""; //$NON-NLS-1$
		if (index > 0) {
			indexSuffix = "(" + index + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}

		final int dotPos = originalName.lastIndexOf('.');
		if (dotPos == -1) {
			return originalName + indexSuffix;
		}
		return originalName.substring(0, dotPos) + indexSuffix + originalName.substring(dotPos);
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		super.onSaveInstanceState(outState);

		outState.putBoolean(SAVE_INSTANCE_KEY_TITLE_VISIBILITY,
				findViewById(R.id.signedfile_title).getVisibility() == View.VISIBLE);

		outState.putString(SAVE_INSTANCE_KEY_OK_TEXT,
				((TextView) findViewById(R.id.filestorage_path)).getText().toString());

		outState.putBoolean(SAVE_INSTANCE_KEY_OK_RESULT_VISIBILITY,
				findViewById(R.id.signedfile_correct).getVisibility() == View.VISIBLE);

		outState.putString(SAVE_INSTANCE_KEY_ERROR_TEXT,
				((TextView) findViewById(R.id.tv_signedfile_ko)).getText().toString());

		outState.putBoolean(SAVE_INSTANCE_KEY_ERROR_RESULT_VISIBILITY,
				findViewById(R.id.signedfile_error).getVisibility() == View.VISIBLE);
	}
}

