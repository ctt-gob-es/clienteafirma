package es.gob.afirma.core.signers;

import java.util.Properties;

/**
 * Define la posibilidad de que la entrada de datos de un objeto sea opcional.
 */
public interface OptionalDataInterface {

	/**
	 * Indica si el objeto necesita o no los datos de entrada.
	 * @param cryptoOperation Operaci&oacute;n de firma que se desea realizar (SIGN, COSIGN, COUNTERSIGN).
	 * @param config Configuraci&oacute;n en base a la que identificar si la entrada de datos es obligatoria.
	 * @return {@code true} si los datos son necesarios, {@code false} si no lo son.
	 */
	boolean needData(String cryptoOperation, Properties config);
}
