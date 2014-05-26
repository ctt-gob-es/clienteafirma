package es.gob.afirma.signfolder.server.proxy;

import java.util.ArrayList;
import java.util.List;

/** Petici&oacute;n de fase de firma de documentos.
 * @author Carlos Gamuci Mill&aacute;n */
public class TriphaseRequest extends ArrayList<TriphaseSignDocumentRequest> {

	/** Serial id. */
	private static final long serialVersionUID = 1L;

	/** Referencia de la petici&oacute;n. */
	private final String ref;

	/** Resultado de la petici&oacute;n de la petici&oacute;n. */
	private boolean statusOk = true;

	/**
	 * Construye un objeto de petici&oacute;n de prefirma o postfirma de documentos.
	 * @param reference Referencia &uacute;nica de la petici&oacute;n.
	 * @param documementsRequest Listado de documentos para los que se solicita la operaci&oacute;n.
	 */
	TriphaseRequest(final String reference, final List<TriphaseSignDocumentRequest> documementsRequest) {
		this.ref = reference;
		if (documementsRequest != null) {
			this.addAll(documementsRequest);
		}
	}

	/** Construye un objeto de petici&oacute;n de firma de documentos.
	 * @param reference Referencia &uacute;nica de la petici&oacute;n.
	 * @param statusOk Estado de la petici&oacute;n.
	 * @param documementsRequest Listado de documentos para los que se solicita la firma.
	 */
	TriphaseRequest(final String reference, final boolean statusOk, final List<TriphaseSignDocumentRequest> documementsRequest) {
		this.ref = reference;
		this.statusOk = statusOk;
		if (documementsRequest != null) {
			this.addAll(documementsRequest);
		}
	}

	/** Recupera la referencia de la petici&oacute;n firma de documentos.
	 * @return Referencia de la petici&oacute;n. */
	public String getRef() {
		return this.ref;
	}

	/** Indica si el estado de la petici&oacute;n es OK.
	 * @return Indicador del estado de la petici&oacute;n. */
	public boolean isStatusOk() {
		return this.statusOk;
	}
	
	/** Estable si el estado de la petici&oacute;n es OK.
	 * @param statusOk Es {@code true} en casode que la petici&oacute;n de firma progrese
	 * correctamente, {@code false} en caso contrario. */
	public void setStatusOk(final boolean statusOk) {
		this.statusOk = statusOk;
	}
}
