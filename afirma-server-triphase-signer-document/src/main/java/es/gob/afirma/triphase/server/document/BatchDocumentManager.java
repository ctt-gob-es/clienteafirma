package es.gob.afirma.triphase.server.document;

import java.util.Properties;

/** Interfaz para el uso de m&eacutetodos especificos en la firma de lotes */
public interface BatchDocumentManager extends DocumentManager {

	/** Deshace un guardado previo (para los modos transaccionales).
	 * @param sign Identificador de la firma a deshacer. */
	void rollback(final String singleSignId);

	/** Configura c&oacute;mo ha de guardarse la firma electr&oacute;nica.
	 * cada implementaci&oacute;n requerir&aacute; unas propiedades distintas dentro del
	 * objeto de propiedades.
	 * @param config Propiedades de configuraci&oacute;n. */
	void init(final Properties config);

}
