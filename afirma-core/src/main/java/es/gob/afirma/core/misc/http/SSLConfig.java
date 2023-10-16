package es.gob.afirma.core.misc.http;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSocketFactory;

/**
 * Configuracion para las conexiones SSL.
 */
public class SSLConfig {

	private HostnameVerifier hostnameVerifier = null;

	private SSLSocketFactory socketFactory = null;

	/**
	 * Obtiene el verificador de nombres a utilizar en caso de error en la validaci&oacute;n de
	 * dominio identificado en el certificado SSL.
	 * @return Verificador de nombres.
	 */
	public HostnameVerifier getHostnameVerifier() {
		return this.hostnameVerifier;
	}

	/**
	 * Establece el verificador de nombres a utilizar en caso de error en la validaci&oacute;n de
	 * dominio identificado en el certificado SSL.
	 * @param hostnameVerifier Verificador de nombres.
	 */
	public void setHostnameVerifier(final HostnameVerifier hostnameVerifier) {
		this.hostnameVerifier = hostnameVerifier;
	}

	/**
	 * Obtiene la factor&iacute;a de sockets a utilizar para la conexi&oacute;n SSL.
	 * @return Factor&iacute;a de sockets.
	 */
	public SSLSocketFactory getSSLSocketFactory() {
		return this.socketFactory;
	}

	/**
	 * Establece la factor&iacute;a de sockets a utilizar para la conexi&oacute;n SSL.
	 * @param socketFactory Factor&iacute;a de sockets.
	 */
	public void setSSLSocketFactory(final SSLSocketFactory socketFactory) {
		this.socketFactory = socketFactory;
	}
}
