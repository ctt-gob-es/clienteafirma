package es.gob.afirma;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Build;
import android.os.Bundle;
import android.security.KeyChain;
import android.security.KeyChainAliasCallback;
import android.security.KeyChainException;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.widget.Toast;
import es.gob.afirma.android.crypto.DesCipher;
import es.gob.afirma.android.network.UriParser;
import es.gob.afirma.android.network.UriParser.ParameterException;
import es.gob.afirma.android.network.UriParser.UrlParameters;
import es.gob.afirma.android.network.UrlHttpManager;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Actividad dedicada a la firma de los datos recibidos en la entrada mediante un certificado
 * del almac&eacute;n central seleccionado por el usuario. */
public final class SignDataActivity extends FragmentActivity implements KeyChainAliasCallback {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

    private static final String METHOD_OP = "put"; //$NON-NLS-1$

    private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

    /** Car&aacute;cter utilizado para separar el padding agregado a los datos para cifrarlos y los propios datos
     * cifrados en base64. */
    private static final char PADDING_CHAR_SEPARATOR = '.';

    private Toast toast;

    private UrlParameters parameters;

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
            try {
				this.parameters = UriParser.getParameters(getIntent().getData().toString());
			}
            catch (final ParameterException e) {
            	showErrorMessage(getString(R.string.error_bad_params));
            	Log.e(ES_GOB_AFIRMA, "La longitud de la clave de cifrado no es correcta."); //$NON-NLS-1$
            	return;
			}

            KeyChain.choosePrivateKeyAlias(
        		this,
        		this,
        		new String[] { "RSA" }, //$NON-NLS-1$ // KeyTypes
        		null, // Issuers
        		null, // Host
        		-1, // Port
        		null // Alias
    		);
        }
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
        // Por ahora instanciamos directamente el CAdES
        final AOSigner signer = new AOCAdESSigner();

        // Generacion de la firma
        final byte[] sign;
        try {
            sign = signer.sign(
        		this.parameters.getData(),
        		this.parameters.getSignatureAlgorithm(),
                pke,
                this.parameters.getExtraParams()
            );
        }
        catch (final Exception e) {
            Log.e(ES_GOB_AFIRMA, "Error en la firma: " + e); //$NON-NLS-1$
            launchError(ErrorManager.ERROR_SIGNING);
            finish();
            return;
        }

    	// Ciframos si nos dieron clave privada, si no subimos los datos sin cifrar
        final String data;
        try {
        	data = generateCipherDataString(sign, this.parameters.getDesKey());
        }
        catch (final IOException e) {
        	Log.e(ES_GOB_AFIRMA, "Error en el cifrado de la firma: " + e); //$NON-NLS-1$
        	launchError(ErrorManager.ERROR_CODING_BASE64);
        	finish();
        	return;
        }
        catch (final GeneralSecurityException e) {
        	Log.e(ES_GOB_AFIRMA, "Error en el cifrado de la firma: " + e); //$NON-NLS-1$
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
     * @throws IOException */
    private static String generateCipherDataString(final byte[] data, final byte[] cipherKey) throws InvalidKeyException, GeneralSecurityException, IOException {
    	return Integer.toString(DesCipher.getPaddingLength() - data.length % DesCipher.getPaddingLength()) +
    			PADDING_CHAR_SEPARATOR + Base64.encodeBytes(DesCipher.cipher(data, cipherKey), Base64.URL_SAFE);
    }

    /**
     * Envia los datos indicado a un servlet. En caso de error, cierra la app.
     * @param data Datos que se desean enviar.
     */
    private void sendData(final String data) {

    	Log.i(ES_GOB_AFIRMA, "Invocando sendData"); //$NON-NLS-1$

        try {

        	final StringBuilder url = new StringBuilder(this.parameters.getStorageServletUrl().toExternalForm());
        	url.append("?op=").append(METHOD_OP); //$NON-NLS-1$
        	url.append("&v=").append(SYNTAX_VERSION); //$NON-NLS-1$
        	url.append("&id=").append(this.parameters.getId()); //$NON-NLS-1$
        	url.append("&dat=").append(data); //$NON-NLS-1$

        	// Llamamos al servicio para guardar los datos
            final byte[] result = UrlHttpManager.readUrl(url.toString());

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
    private void showErrorMessage(final String message) {
    	final ErrorDialog dialog = new ErrorDialog(message);
    	dialog.show(getSupportFragmentManager(), "ErrorDialog"); //$NON-NLS-1$
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
    	ErrorDialog(final String message) {
			this.message = message;
		}

    	@Override
    	public Dialog onCreateDialog(final Bundle savedInstanceState) {

    		final AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(getActivity());
    		dialogBuilder.setMessage(this.message)
    			.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(final DialogInterface dialog, final int which) {
						finish();
					}
				});

    		return dialogBuilder.create();
    	}
    }
}