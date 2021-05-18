package es.gob.afirma.triphase.server.document;

import java.util.Properties;


public abstract class DocumentManagerBase implements DocumentManager{

	/** Deshace un guardado previo (para los modos transaccionales).
	 * @param sign Identificador de la firma a deshacer. */
	public void rollback() {
		//NADA
	}

	/** Configura c&oacute;mo ha de guardarse la firma electr&oacute;nica.
	 * cada implementaci&oacute;n requerir&aacute; unas propiedades distintas dentro del
	 * objeto de propiedades.
	 * @param config Propiedades de configuraci&oacute;n. */
	public void init(final Properties config) {

	}

}
