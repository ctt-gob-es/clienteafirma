package es.gob.afirma.signfolder.server.proxy;

import java.util.Map;
import java.util.logging.Logger;

/**
 * Petici&oacute;n de solicitudes de firma.
 * @author Carlos Gamuci
 */
public class ListRequest {

	/** Estado de solicitud de firma pendiente de firmar. */
	public static final String STATE_UNRESOLVED = "unresolved"; //$NON-NLS-1$

	/** Estado de solicitud de firma ya firmada. */
	public static final String STATE_SIGNED = "signed"; //$NON-NLS-1$

	/** Estado de solicitud de firma rechazada. */
	public static final String STATE_REJECTED = "rejected"; //$NON-NLS-1$

	private final byte[] certEncoded;

	private String state;

	private final String[] formats;

	private final Map<String, String> filters;
	
	private final int numPage;
	
	private final int pageSize;

	/**
	 * Construye una petici&oacute;n para obtener parte del listado de solicitudes de firma.
	 * @param certEncoded Certificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @param state Estado de las solicitudes requeridas.
	 * @param formats Formatos de firma compatibles.
	 * @param filters Filtros de solicitudes de firma.
	 * @param numPage N&uacute;mero de pagina del listado.
	 * @param pageSize Tama&ntilde;o de p&aacute;gina definido.
	 */
	public ListRequest(final byte[] certEncoded, final String state, final String[] formats, final Map<String, String> filters, final int numPage, final int pageSize) {
		if (state == null || !STATE_UNRESOLVED.equalsIgnoreCase(state) &&
				!STATE_SIGNED.equalsIgnoreCase(state) && !STATE_REJECTED.equalsIgnoreCase(state)) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"La peticion de solicitudes de firma requirio aquellas con el estado '" + //$NON-NLS-1$
					state + "'. Se devolveran las del estado " + STATE_UNRESOLVED); //$NON-NLS-1$
			this.state = STATE_UNRESOLVED;
		}
		else {
			this.state = state;
		}

		this.certEncoded = certEncoded;
		this.formats = formats;
		this.filters = filters;
		this.numPage = numPage;
		this.pageSize = pageSize;
	}

	/**
	 * Recupera el certificado codificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @return Certificado codificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}

	/**
	 * Recupera el estado solicitado de las peticiones de firma.
	 * @return Estado solicitado.
	 */
	public String getState() {
		return this.state;
	}

	/**
	 * Recupera el listado de formatos de firma compatibles.
	 * @return Listado de formatos de firma.
	 */
	public String[] getFormats() {
		return this.formats;
	}

	/**
	 * Recupera el conjunto de filtros establecidos con las duplas clave-valor.
	 * @return Filtros establecidos.
	 */
	public Map<String, String> getFilters() {
		return this.filters;
	}

	/**
	 * Recupera el numero de p&aacute;gina de peticiones solicitado.
	 * @return N&uacute;mero de p&aacute;gina.
	 */
	public int getNumPage() {
		return this.numPage;
	}
	
	/**
	 * Recupera el tama&ntilde;o de p&aacute;gina configurado.
	 * @return Tama&ntilde;o de p&aacute;gina.
	 */
	public int getPageSize() {
		return this.pageSize;
	}
}
