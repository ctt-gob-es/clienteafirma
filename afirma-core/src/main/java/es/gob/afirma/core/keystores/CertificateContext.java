package es.gob.afirma.core.keystores;

/**
 * Contexto de un certificado consistente en el almacen al que pertenece
 * y el alias con el cual obtenerlo.
 */
public class CertificateContext {

	private final KeyStoreManager ksm;

	private final String alias;

	/**
	 * Construye el contexto del certificado.
	 * @param ksm Almac&eacute;n al que pertenece el certificado.
	 * @param alias Alias del certificados en el almac&eacute;n.
	 */
	public CertificateContext(final KeyStoreManager ksm, final String alias) {
		this.ksm = ksm;
		this.alias = alias;
	}

	/**
	 * Recupera el almac&eacute;n al que pertenece el certificado.
	 * @return Almac&eacute;n de certificados/claves.
	 */
	public KeyStoreManager getKeyStoreManager() {
		return this.ksm;
	}

	/**
	 * Alias del certificado demntro del almac&eacute;n.
	 * @return Alias del certificado.
	 */
	public String getAlias() {
		return this.alias;
	}
}
