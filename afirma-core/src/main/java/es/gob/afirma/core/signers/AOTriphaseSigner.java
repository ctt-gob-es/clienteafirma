package es.gob.afirma.core.signers;

import es.gob.afirma.core.misc.http.UrlHttpManager;

public abstract class AOTriphaseSigner implements AOSigner {

	protected UrlHttpManager httpConnection = null;

	/**
	 * Establece la conexi&oacute;n que debe usar para conectar
	 * con el servicio de firma trif&aacute;sica.
	 * @param httpConnection Conexi&oacute;n.
	 */
	public void setHttpConnection(final UrlHttpManager httpConnection) {
		this.httpConnection = httpConnection;
	}
}
