package es.gob.afirma.utils;

import java.util.HashMap;
import java.util.Map;

import android.util.Log;

/** Clase de utilidad para el an&aacute;lisis sint&aacute;ctico de Urls
 * @author Alberto Mart&iacute;nez */
public final class UriParser {

    private UriParser() {
        // Constructor privado. No se permite instancias
    }

    /** Analiza la Url de entrada para obtener la lista de par&aacute;metros asociados
     * @param uri Url de llamada
     * @return Devuelve una tabla hash con cada par6aacute;metro asociado a un valor */
    public static Map<String, String> parser(final String uri) {
        final Map<String, String> params = new HashMap<String, String>();
        final String[] parameters = uri.substring(uri.indexOf('?') + 1).split("&"); //$NON-NLS-1$
        for (final String param : parameters) {

        	Log.i("es.gob.afirma", param);

			if (param.indexOf('=') > 0) {
				params.put(
						param.substring(0, param.indexOf('=')),
						(param.indexOf('=') == (param.length() - 1) ? "" : param.substring(param.indexOf('=') + 1))); //$NON-NLS-1$
			}
		}

        return params;
    }
}