package es.gob.afirma.signers.xades;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

public class XAdESMessages {

    private static final String BUNDLE_NAME = "xadesmessages"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

    private XAdESMessages() {
    	// No se permite la instanciacion
	}

    /** Recupera el texto identificado con la clave proporcionada.
     * @param key Clave del texto.
     * @return Recueso textual. */
    static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
        	LOGGER.warning("No se ha encontrado el recurso textual '" + key + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return '!' + key + '!';
        }
    }

    /** Recupera el texto identificado con la clave proporcionada y sustituye la
     * subcadenas "%0" por el texto proporcionado.
     * @param key Clave del texto.
     * @param text Texto que se desea insertar.
     * @return Recuerso textual con la subcadena sustituida. */
    static String getString(final String key, final String text) {
        try {
            return RESOURCE_BUNDLE.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
        	LOGGER.warning("No se ha encontrado el recurso textual '" + key + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return '!' + key + '!';
        }
    }

    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto en la posici&oacute;n 'i' del array
     * proporcionado.
     * @param key Clave del texto.
     * @param params Par&aacute;metros que se desean insertar.
     * @return Recuerso textual con las subcadenas sustituidas. */
    static String getString(final String key, final String[] params) {
        String text;
        try {
            text = RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
        	LOGGER.warning("No se ha encontrado el recurso textual '" + key + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return '!' + key + '!';
        }

        if (params != null) {
            for (int i = 0; i < params.length; i++) {
                text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            }
        }

        return text;
    }

}
