package es.gob.afirma.triphase.server.document;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

/** Interfaz para el uso de m&eacute;todos especificos en la firma de lotes */
public interface BatchDocumentManager extends DocumentManager {

	/**
	 * Deshace un guardado previo (para los modos transaccionales).
	 * @param dataRef Referencia al documento firmado.
	 * @param certChain Cadena de certificaci&oacute;n usada para el guardado de los datos.
	 * @param prop Configuraci&oacute;n de firma aplicada.
	 * @throws IOException Cuando falla el deshacer el guardado de la firma.
	 */
	void rollback(String dataRef, X509Certificate[] certChain, Properties prop) throws IOException;

	/**
	 * Configura c&oacute;mo ha de guardarse la firma electr&oacute;nica.
	 * cada implementaci&oacute;n requerir&aacute; unas propiedades distintas dentro del
	 * objeto de propiedades.
	 * @param config Propiedades de configuraci&oacute;n del gestor.
	 */
	void init (Properties config);

}
