package es.gob.afirma.keystores;

import es.gob.afirma.misc.AOConstants;

/**
 * Indica que ocurri&oacute; un error intentando obtener un <code>KeyStore</code>, pero que
 * se obtuvo uno de un tipo alternativo.
 * @version 0.1
 */
public final class AOKeystoreAlternativeException extends Exception {

	private static final long serialVersionUID = -1536411480952188376L;
	
	private final AOConstants.AOKeyStore alternativeKs;
	
	public AOKeystoreAlternativeException(final AOConstants.AOKeyStore ks, 
			                              final String desc, 
			                              final Throwable e) {
		super(desc, e);
		if (ks == null) throw new NullPointerException(
			"Es necesario proporcionar un AOConstants.AOKeyStore alternativo"
		);
		alternativeKs = ks;
	}
	
	public AOKeystoreAlternativeException(final AOConstants.AOKeyStore ks, final String desc) {
		super(desc);
		if (ks == null) throw new NullPointerException(
			"Es necesario proporcionar un AOConstants.AOKeyStore alternativo"
		);
		alternativeKs = ks;
	}
	
	public AOConstants.AOKeyStore getAlternativeKsm() {
		return alternativeKs;
	}

}
