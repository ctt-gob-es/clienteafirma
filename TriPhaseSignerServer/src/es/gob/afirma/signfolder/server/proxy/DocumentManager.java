package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;

/** Interfaz para la recuperaci&oacute;n de documentos desde un servidor o repositorio documental.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
public interface DocumentManager {

	/** Obtiene un documento en base a su identificador.
	 * @param id Identificador del documento
	 * @return Documento (en binario)
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n */
	byte[] getDocument(final String id) throws IOException;

	/** Almacena un documento firmado.
	 * @param id Identificador del documento original no firmado.
	 * @param data Datos firmados.
	 * @return Identificador del nuevo documento.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n */
	String storeDocument(final String id, final byte[] data) throws IOException;
}
