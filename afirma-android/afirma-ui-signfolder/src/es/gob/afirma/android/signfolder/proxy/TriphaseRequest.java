package es.gob.afirma.android.signfolder.proxy;

/**
 * Petici&oacute;n de fase de firma de documentos.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class TriphaseRequest {

	/** Referencia de la petici&oacute;n. */
	private final String ref;

	/** Resultado de la petici&oacute;n de la petici&oacute;n. */
	private boolean statusOk = true;

	/** Listado de documentos de la petici&oacute;n que se desean firmar. */
	private final TriphaseSignDocumentRequest[] documementsRequest;

	/** Resultado de la petici&oacute;n de la petici&oacute;n. */
	private String exception = null;
	
	/**
	 * Construye un objeto de petici&oacute;n de prefirma o postfirma de documentos.
	 * @param reference Referencia &uacute;nica de la petici&oacute;n.
	 * @param documementsRequest Listado de documentos para los que se solicita la operaci&oacute;n.
	 */
	public TriphaseRequest(final String reference, final TriphaseSignDocumentRequest[] documementsRequest) {
		this.ref = reference;
		this.documementsRequest = documementsRequest;
	}

	/**
	 * Construye un objeto de petici&oacute;n de firma de documentos.
	 * @param reference Referencia &uacute;nica de la petici&oacute;n.
	 * @param statusOk Estado de la petici&oacute;n.
	 * @param documementsRequest Listado de documentos para los que se solicita la firma.
	 */
	public TriphaseRequest(final String reference, final boolean statusOk, final TriphaseSignDocumentRequest[] documementsRequest) {
		this.ref = reference;
		this.statusOk = statusOk;
		this.documementsRequest = documementsRequest;
	}

	/**
	 * Construye un objeto de petici&oacute;n de firma de documentos.
	 * @param reference Referencia &uacute;nica de la petici&oacute;n.
	 * @param statusOk Estado de la petici&oacute;n.
	 * @param exception Traza de la excepci&oacute;n que provoc&oacute; el error.
	 */
	public TriphaseRequest(final String reference, final boolean statusOk, final String exception) {
		this.ref = reference;
		this.statusOk = statusOk;
		this.documementsRequest = null;
		this.exception = exception;
	}
	
	/**
	 * Recupera la referencia de la petici&oacute;n firma de documentos.
	 * @return Referencia de la petici&oacute;n.
	 */
	public String getRef() {
		return this.ref;
	}

	/** Indica si el estado de la petici&oacute;n es correcto.
	 * @return Indicador del estado de la petici&oacute;n. */
	public boolean isStatusOk() {
		return this.statusOk;
	}

	/** Establece el estado actual de la petici&oacute;n.
	 * @param ok <code>true</code> si la petici&oacute;n es correcta, <code>false</code> en caso contrario */
	public void setStatusOk(final boolean ok) {
		this.statusOk = ok;
	}

	/**
	 * Listado de peticiones de documentos para los que se desea la firma en multiples fases.
	 * @return Listado de peticiones o null si ha ocurrido alg&uacute;n problema al
	 * procesar la petici&oacute;n ({@code isStatusOk() == false}).
	 */
	public TriphaseSignDocumentRequest[] getDocumentsRequests() {
		return this.documementsRequest;
	}
	
	/**
	 * En caso de error, recupera la traza de la excepci&oacute;n que lo provoc&oacute;
	 * @return Traza de la excepci&oacute;n.
	 */
	public String getException() {
		return this.exception;
	}
}
