package es.gob.afirma.miniapplet;

import java.util.Properties;

/** Clase de utilidad para el proceso de propiedades enviadas desde JavaScript
 * y recogidas desde java en formato <code>Properties<code>. */
public final class ExtraParamsProcessor {

	/**
	 * Transforma los par&aacute;metros indicados en entradas de un properties.
	 * Las entradas debe tener la forma {@code CLAVE=VALOR} en donde CLAVE es
	 * el identificador del par&aacute;metro y VALOR el valor asignado a este.
	 * La CLAVE no puede contener ning&uacute;n signo igual ('=') ni empezar por
	 * almohadilla ('#') y se ignorar&aacute;n aquellas entradas que no contengan
	 * el signo igual en una posici&oacute;n la cadena distinta a la primera.
	 * Si se introduce null se devuelve un Properties vac&iacute;o.
	 * @param entries Listado de pares CLAVE - VALOR.
	 * @return Properties con las claves indicadas cargadas como par&aacute;metro.
	 */
	public static Properties convertToProperties(String[] entries) {

		final Properties params = new Properties();
		if (entries == null) {
			return params;
		}

		int n;
		for (final String entry : entries) {
			if (entry != null && !entry.startsWith("#") &&  //$NON-NLS-1$
					((n = entry.indexOf('=')) != -1) && (n != 0)) {
				params.setProperty(entry.substring(0, n).toLowerCase(), (n < entry.length() - 1) ?
						entry.substring(n + 1).trim() : ""); //$NON-NLS-1$
			}
		}
		
		return params;
	}
}
