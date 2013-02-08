package es.gob.afirma;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.os.Build;
import android.os.Bundle;
import android.security.KeyChain;
import android.security.KeyChainAliasCallback;
import android.security.KeyChainException;
import android.util.Log;
import android.widget.Toast;
import es.gob.afirma.android.crypto.DesCipher;
import es.gob.afirma.android.network.UriParser;
import es.gob.afirma.android.network.UrlHttpManager;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/**
 * Actividad dedicada a la firma de los datos recibidos en la entrada mediante un certificado
 * del almac&eacute;n central seleccionado por el usuario.
 */
public class SignDataActivity extends Activity implements KeyChainAliasCallback {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String UTF8 = "utf-8"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el formato de firma. */
	private static final String FORMAT_PARAM = "format"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String ALGORITHM_PARAM = "algorithm"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los datos a firmar. */
	private static final String DATA_PARAM = "dat"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String STORAGE_SERVLET_PARAM = "stservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la clave para el cifrado del documento. */
	private static final String KEY_PARAM = "key"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las opciones de configuraci&oacute;n de la firma. */
	private static final String PROPERTIES_PARAM = "properties"; //$NON-NLS-1$

	/** Formato de firma por defecto. */
	private static final String DEFAULT_SIGNATURE_FORMAT = AOSignConstants.SIGN_FORMAT_CADES;

	/** Algoritmo de firma por defecto. */
	private static final String DEFAULT_SIGNATURE_ALGORITHM = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador de sesi&oacute;n de la firma. */
	private static final int MAX_ID_LENGTH = 20;

	/** Longitud permitida para la clave de cifrado. */
	private static final int CIPHER_KEY_LENGTH = 8;

	private Map<String, String> parameters = null;

    private static final String METHOD_OP = "put"; //$NON-NLS-1$

    private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

    /** Car&aacute;cter utilizado para separar el padding agregado a los datos para cifrarlos y los propios datos
     * cifrados en base64. */
    private static final char PADDING_CHAR_SEPARATOR = '.';

    private Toast toast;

    @SuppressLint("ShowToast")
	@Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sign_data);

        this.toast = Toast.makeText(getApplicationContext(), "", Toast.LENGTH_LONG); //$NON-NLS-1$
    }

    @Override
    public void onStart() {
        super.onStart();

        if (getIntent() != null && getIntent().getData() != null) {

            this.parameters = UriParser.parser(getIntent().getData().toString());

            if (!checkParameters(this.parameters)) {
            	finish();
            	return;
            }

            KeyChain.choosePrivateKeyAlias(this, this, new String[] { "RSA" }, //$NON-NLS-1$ // KeyTypes
            		null, // Issuers
            		null, // Host
            		-1, // Port
            		null // Alias
            		);
        }
    }

    /** Comprueba que est&eacute;n disponibles todos los parametros disponibles en la entrada de datos.
     * @param params Par&aacute;metros de entrada.
     * @return {@code true} si existen todos los par&oacute;metros necesarios, {@code false} en caso
     *         contrario. */
	private boolean checkParameters(final Map<String, String> params) {

		// Comprobamos que se ha especificado el identificador para al firma
    	if (!params.containsKey(ID_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con el identificador del documento: " + ID_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

		// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
		final String signatureSessionId = params.get(ID_PARAM);
		if (signatureSessionId.length() > MAX_ID_LENGTH) {
			Log.e(ES_GOB_AFIRMA, "La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
    		showMessage(getString(R.string.error_bad_params));
            return false;
		}

		// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
		for (final char c : signatureSessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
			if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
				Log.e(ES_GOB_AFIRMA, "El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
	    		showMessage(getString(R.string.error_bad_params));
	            return false;
			}
		}

		// Comprobamos que se ha especificado la clave de cifrado
    	if (!params.containsKey(KEY_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con la clave para el cifrado de la firma: " + KEY_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

		// Comprobamos que la clave de cifrado tenga la longitud correcta
		if (params.get(KEY_PARAM) == null || params.get(KEY_PARAM).length() != CIPHER_KEY_LENGTH) {
			Log.e(ES_GOB_AFIRMA, "La longitud de la clave de cifrado no es correcta."); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
		}

		// Comprobamos que se ha especificado el servlet
    	if (!params.containsKey(STORAGE_SERVLET_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con la direccion del servlet para el envio de la firma: " + STORAGE_SERVLET_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

		// Comprobamos que la URL sea valida
		final URL servletUrl;
    	try {
    		servletUrl = new URL(params.get(STORAGE_SERVLET_PARAM));
		}
    	catch (final MalformedURLException e) {
			Log.e(ES_GOB_AFIRMA, "La URL proporcionada del servlet no es valida: " + e); //$NON-NLS-1$
			showMessage(getString(R.string.error_bad_params));
			return false;
		}
    	// Comprobamos que el protocolo este soportado
    	if (servletUrl.getProtocol() != "http" &&  servletUrl.getProtocol() != "https") { //$NON-NLS-1$ //$NON-NLS-2$
			Log.e(ES_GOB_AFIRMA, "EL protocolo de la URL proporcionada para el servlet no esta soportado"); //$NON-NLS-1$
			showMessage(getString(R.string.error_bad_params));
			return false;
    	}

		// Comprobamos que se nos hayan indicado los datos
    	if (!params.containsKey(DATA_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con los datos para firmar: " + DATA_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

    	// Comprobamos que los datos se pueden tratar como base 64
    	try {
    		Base64.decode(URLDecoder.decode(params.get(DATA_PARAM), DEFAULT_URL_ENCODING));
    	}
    	catch (final Exception e) {
    		Log.e(ES_GOB_AFIRMA, "Los datos introducidos no se pueden tratar como base 64: " + e); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

    	// Comprobamos que se ha especificado el formato
    	if (!params.containsKey(FORMAT_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con el formato de firma: " + FORMAT_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

    	// Comprobamos que se ha especificado el algoritmo
    	if (!params.containsKey(ALGORITHM_PARAM)) {
    		Log.e(ES_GOB_AFIRMA, "No se ha recibido el parametro con el algoritmo de firma: " + ALGORITHM_PARAM); //$NON-NLS-1$
    		showMessage(getString(R.string.error_bad_params));
            return false;
    	}

    	return true;
    }

    @Override
    public void alias(final String alias) {

    	Log.i(ES_GOB_AFIRMA, "Se ha seleccionado el certificado: " + alias); //$NON-NLS-1$

    	// Si no se selecciono ningun certificado, se envia el error
    	if (alias == null) {
            Log.e(ES_GOB_AFIRMA, "No se selecciono ningun certificado de firma"); //$NON-NLS-1$
            launchError(ErrorManager.ERROR_NO_CERT_SELECTED);
            finish();
            return;
    	}

        // Reanudamos aqui la operacion de firma
        // Recuperacion de la clave privada de firma
    	final PrivateKeyEntry pke;
        try {
            pke = new PrivateKeyEntry(KeyChain.getPrivateKey(this, alias), KeyChain.getCertificateChain(this, alias));
        }
        catch (final KeyChainException e) {
            Log.e(ES_GOB_AFIRMA, e.toString());
            if ("4.1.1".equals(Build.VERSION.RELEASE) || "4.1.0".equals(Build.VERSION.RELEASE) || "4.1".equals(Build.VERSION.RELEASE)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				launchError(ErrorManager.ERROR_PKE_ANDROID_4_1);
			} else {
        		launchError(ErrorManager.ERROR_PKE);
        	}
            finish();
            return;
        }
        catch (final Exception e) {
            Log.e(ES_GOB_AFIRMA, e.toString());
            launchError(ErrorManager.ERROR_PKE);
            finish();
            return;
        }
        // Cuando se instala el certificado desde el dialogo de seleccion, Android da a elegir certificado
        // en 2 ocasiones y en la segunda se produce un "java.lang.AssertionError". Se ignorara este error.
        catch (final Error e) {
        	Log.e(ES_GOB_AFIRMA, e.toString());
            finish();
            return;
		}

        // Creacion del firmador
        final AOSigner signer;
        final String format = getParameter(FORMAT_PARAM, DEFAULT_SIGNATURE_FORMAT);
        if (AOSignConstants.SIGN_FORMAT_CADES.equals(format)) {
            signer = new AOCAdESSigner();
        }
        else {
            Log.e(ES_GOB_AFIRMA, "No existe el formato: " + format); //$NON-NLS-1$
            launchError(ErrorManager.ERROR_NOT_SUPPORTED_FORMAT);
            finish();
            return;
        }

        // Generacion de la firma
        final byte[] sign;
        try {
            sign = signer.sign(
        		Base64.decode(URLDecoder.decode(getParameter(DATA_PARAM), DEFAULT_URL_ENCODING)),
            	getParameter(ALGORITHM_PARAM, DEFAULT_SIGNATURE_ALGORITHM),
                pke,
                SignDataActivity.parseB64Properties(getParameter(PROPERTIES_PARAM))
            );
        }
        catch (final Exception e) {
            Log.e(ES_GOB_AFIRMA, e.getMessage());
            launchError(ErrorManager.ERROR_SIGNING);
            finish();
            return;
        }

    	// Ciframos si nos dieron clave privada, si no subimos los datos sin cifrar
        final String data;
        try {
        	data = generateCipherDataString(sign, getParameter(KEY_PARAM));
        }
        catch (final IOException e) {
        	Log.e(ES_GOB_AFIRMA, e.getMessage());
        	launchError(ErrorManager.ERROR_CODING_BASE64);
        	finish();
        	return;
        }
        catch (final GeneralSecurityException e) {
        	Log.e(ES_GOB_AFIRMA, e.getMessage());
        	launchError(ErrorManager.ERROR_CIPHERING);
        	finish();
        	return;
        }

        sendData(data);

        Log.i(ES_GOB_AFIRMA, "Firma entregada satisfactoriamente."); //$NON-NLS-1$
        finish();
    }

    /**
     * Genera una cadena con datos cifrados y codificados en base 64 antecedidos por el n&uacute;mero de
     * caracteres que se han tenido que agregar como padding y separados por un car&aacute;cter separador.
     * @param data Datos a cifrar.
     * @param cipherKey Clave de cifrado.
     * @return Cadena con el numero de caracteres agregados manualmente para cumplir la longitud requerida,
     * el caracter separador y los datos cifrados y en base 64.
     * @throws InvalidKeyException Cuando la clave no es v&aacute;lida.
     * @throws GeneralSecurityException Cuando falla el proceso de cifrado.
     * @throws IOException
     */
    private static String generateCipherDataString(final byte[] data, final String cipherKey) throws InvalidKeyException, GeneralSecurityException, IOException {
    	return Integer.toString(DesCipher.getPaddingLength() - data.length % DesCipher.getPaddingLength()) +
    			PADDING_CHAR_SEPARATOR + Base64.encodeBytes(DesCipher.cipher(data, cipherKey), Base64.URL_SAFE);
    }

    /**
     * Envia los datos indicado a un servlet. En caso de error, cierra la app.
     * @param data Datos que se desean enviar.
     */
    private void sendData(final String data) {

    	Log.i(ES_GOB_AFIRMA, "Invocando sendData"); //$NON-NLS-1$

    	final UrlHttpManager urlManager = new UrlHttpManager();
        try {

        	final StringBuilder url = new StringBuilder(getParameter(STORAGE_SERVLET_PARAM));
        	url.append("?op=").append(METHOD_OP); //$NON-NLS-1$
        	url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
        	url.append("&id=").append(getParameter(ID_PARAM)); //$NON-NLS-1$
        	url.append("&dat=").append(data); //$NON-NLS-1$

        	// Llamamos al servicio para guardar los datos
            final byte[] result = urlManager.readUrl(url.toString());

            Log.i(ES_GOB_AFIRMA, "Resultado del deposito de la firma: " + new String(result)); //$NON-NLS-1$


            if (!new String(result).trim().equals("OK")) { //$NON-NLS-1$
            	Log.e(ES_GOB_AFIRMA, "No se pudo entregar la firma al servlet: " + new String(result)); //$NON-NLS-1$
            	showToast(getString(R.string.error_server_error));
            	finish();
            }
        }
        catch (final IOException e) {
        	Log.e(ES_GOB_AFIRMA, "No se pudo conectar con el servidor intermedio: " + e); //$NON-NLS-1$
        	showToast(getString(R.string.error_server_connect));
        	finish();
        }
    }

    /**
     * Muestra un mensaje de error y lo env&iacute;a al servidor para que la p&aacute;gina Web
     * tenga constancia de &eacute;l.
     * @param errorId Identificador del error.
     */
    private void launchError(final String errorId) {
		try {
			sendData(URLEncoder.encode(ErrorManager.genError(errorId, null), DEFAULT_URL_ENCODING));
		}
		catch (final UnsupportedEncodingException e) {
			// No puede darse el soporte de UTF-8 es obligatorio
			Log.e(
				ES_GOB_AFIRMA,
				"No se ha posido enviar la respuesta al servidor por error en la codificacion " + DEFAULT_URL_ENCODING + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
    }

    /**
     * Muestra un mensaje como Toast.
     * @param message Mensaje que se desea mostrar.
     */
    private void showToast(final String message) {
    	this.toast.setText(message);
    	this.toast.show();
    }

    /**
     * Muestra un mensaje de advertencia al usuario.
     * @param message Mensaje que se desea mostrar.
     */
    private void showMessage(final String message) {
    	final ErrorDialog dialog = new ErrorDialog(message);
    	dialog.show(getFragmentManager(), ""); //$NON-NLS-1$
    	this.toast.setText(message);
    	this.toast.show();
    }

    /**
     * Recupera un p&aacute;rametro de la URL descodificado. Si no se encuentra el par&aacute;metro se devuelve {@code null}.
     * @param param Clave del par&aacute;metro.
     * @return Valor del par&aacute;metro.
     */
    private String getParameter(final String param) {
    	return getParameter(param, null);
    }

    /**
     * Recupera un p&aacute;rametro de la URL descodificado. Si no se encuentra el par&aacute;metro se devuelve {@code null}.
     * @param param Clave del par&aacute;metro.
     * @return Valor del par&aacute;metro.
     */
    private String getParameter(final String param, final String defaultValue) {
    	try {
    		return this.parameters.containsKey(param) ?
    				URLDecoder.decode(this.parameters.get(param), UTF8) :
    					defaultValue;
		} catch (final UnsupportedEncodingException e) {
			Log.w(ES_GOB_AFIRMA, "La codificacion utilizada para la URL no es valida: " + e.toString()); //$NON-NLS-1$
			return this.parameters.get(param);
		}
    }


    /**
     * Convierte una cadena en Base 64 de propiedades en un Properties.
     * @param prop Listado de propiedades en base 64.
     * @return Objeto de propiedades.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de la cadena.
     */
    private static Properties parseB64Properties(final String prop) throws IOException {
        final Properties properties = new Properties();

        if (prop != null) {
        	properties.load(new ByteArrayInputStream(Base64.decode(prop)));
        }

        return properties;
    }

    /**
     * Di&aacute;logo modal con el que mostrar al usuario los errores que deberer&iacute;a gestionar con
     * el integrador del servicio.
     */
    private class ErrorDialog extends DialogFragment {

    	private String message = null;

    	/**
    	 * Construye un di&aacute;logo de error.
    	 * @param message Mensaje que mostrar al usuario.
    	 */
    	public ErrorDialog(final String message) {
			this.message = message;
		}

    	@Override
    	public Dialog onCreateDialog(final Bundle savedInstanceState) {

    		final AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(getActivity());
    		dialogBuilder.setMessage(this.message)
    			.setPositiveButton(R.string.ok, null);

    		return dialogBuilder.create();
    	}
    }
}