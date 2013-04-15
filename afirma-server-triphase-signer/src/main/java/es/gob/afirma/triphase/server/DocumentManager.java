package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.util.Properties;

/** Interfaz para la recuperaci&oacute;n de documentos desde un servidor o repositorio documental.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
public interface DocumentManager {

	/** Obtiene un documento en base a su identificador.
	 * @param id Identificador del documento
	 * @param config Par&aacute;metros para la configuraci&oacute;n de la recuperaci&oacute;n del documento.
	 * @return Documento (en binario)
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n */
	byte[] getDocument(final String id, Properties config) throws IOException;

	/** Almacena un documento firmado.
	 * @param id Identificador del documento original no firmado.
	 * @param data Datos firmados.
	 * @param config Par&aacute;metros para la configuraci&oacute;n del guardado del documento.
	 * @return Identificador del nuevo documento codificado en base 64.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n */
	String storeDocument(final String id, final byte[] data, final Properties config) throws IOException;
}
