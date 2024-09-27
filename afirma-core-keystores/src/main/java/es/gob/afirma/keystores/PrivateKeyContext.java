package es.gob.afirma.keystores;

import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.keystores.KeyStoreManager;

/**
 * Conjunto de elementos que definen el contexto de una clave privada.
 */
public class PrivateKeyContext {
	private final KeyStoreManager keyStoreManager;
	private final PrivateKeyEntry privateKeyEntry;

	/**
	 * Construye el contexto de la clave privada.
	 * @param ksm Almacen del que se extrajo la clave.
	 * @param pke Entrada de la clave privada.
	 */
	public PrivateKeyContext(final KeyStoreManager ksm, final PrivateKeyEntry pke) {
		this.keyStoreManager = ksm;
		this.privateKeyEntry = pke;
	}

	/**
	 * Obtiene el almac&eacute;n de la clave privada.
	 * @return Almac&eacute;n de la clave privada.
	 */
	public KeyStoreManager getKeyStoreManager() {
		return this.keyStoreManager;
	}

	/**
	 * Obtiene la entrada de la clave privada.
	 * @return Entrada de la clave privada.
	 */
	public PrivateKeyEntry getPrivateKeyEntry() {
		return this.privateKeyEntry;
	}
}
