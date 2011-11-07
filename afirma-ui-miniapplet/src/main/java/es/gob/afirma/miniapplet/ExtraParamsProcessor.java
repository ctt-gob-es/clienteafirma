package es.gob.afirma.miniapplet;

import java.io.ByteArrayInputStream;
import java.util.Properties;
import java.util.logging.Logger;

/** Clase de utilidad para el proceso de propiedades enviadas desde JavaScript
 * y recogidas desde java en formato <code>Properties<code>. */
public final class ExtraParamsProcessor {

	/**
	 * Transforma la entrada introducida en un properties.
	 * Las entradas deben estar separadas por salto de l&iacute;nea y tener la forma
	 * {@code CLAVE=VALOR} en donde CLAVE es el identificador del par&aacute;metro y
	 * VALOR el valor asignado a este.
	 * La CLAVE no puede contener ning&uacute;n signo igual ('=') ni empezar por
	 * almohadilla ('#') y se ignorar&aacute;n aquellas entradas que no contengan
	 * el signo igual en una posici&oacute;n la cadena distinta a la primera.
	 * Si se introduce null se devuelve un Properties vac&iacute;o.
	 * @param entries Listado de pares CLAVE - VALOR.
	 * @return Properties con las claves indicadas cargadas como par&aacute;metro.
	 */
	public static Properties convertToProperties(String entries) {

		final Properties params = new Properties();
		if (entries == null) {
			return params;
		}

		try {
			params.load(new ByteArrayInputStream(entries.getBytes()));
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"Se han encontrado entradas no validas en la configuracion de la operacion: " //$NON-NLS-1$
					+ e);
			return params;
		}
		
		return params;
	}
}
