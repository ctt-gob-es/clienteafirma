package es.gob.afirma.signers.padestri.server;

import java.io.IOException;
import java.util.Properties;

/**
 * Interfaz que define los m&eacute;todos necesarios para la mejora de firmas (agregado de
 * sellos de tiempo, firma longeva,...).
 */
public interface SignEnhancer {

	/**
	 * Completa una firma electr&oacute;nica agreg&aacute;ndole opciones adicionales
	 * postfirma.
	 * @param signature Firma que se desea mejorar.
	 * @param options Opciones y configuraci&oacute;n necesarias para la mejora de la firma.
	 * @return Firma mejorada.
	 * @throws IOException Cuando ocurre algun error al generar la firma.
	 */
	public byte[] enhance(byte[] signature, Properties options) throws IOException;
}
