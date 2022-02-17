package es.gob.afirma.standalone.configurator;

/**
 * Excepci&oacute;n que se&ntilde;ala un problema con el Llavero de macOS.
 */
public class KeyChainException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -9024740474433228626L;

	/**
	 * Crea la excepci&oacute;n con una descripci&oacute;n del error.
	 * @param msg Descripci&oacute;n del error. 
	 */
	public KeyChainException(String msg) {
		super(msg);
	}
	
	/**
	 * Crea la excepci&oacute;n con una descripci&oacute;n y el motivo del
	 * error.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Motivo que origin&oacute; el error. 
	 */
	public KeyChainException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
