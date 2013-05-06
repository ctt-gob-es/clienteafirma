package es.gob.afirma.signers.pkcs7;

import es.gob.afirma.core.InvalidLibraryException;

/** Indica que hay un BouncyCastle inv&aacute;lido en el CLASSPATH o en el BOOTCLASSPATH, a menudo
 * porque se ha instalado el JAR inapropiadamente como extensi&oacute;n del JRE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class InvalidBouncyCastleException extends InvalidLibraryException {

	private static final long serialVersionUID = -322997692480101275L;

	private final String exp;
	private final String fnd;

	/** Crea una instancia de la excepci&oacute;n.
	 * @param expected Versi&oacute;n esperada de BouncyCastle
	 * @param found Versi&oacute;n encontrada (actual) de BouncyCastle
	 * @param e Excepci&oacute;n original */
	public InvalidBouncyCastleException(final String expected, final String found, final Throwable e) {
		super("Se necesitaba BouncyCastle version " + expected + ", pero se encontro la version " + found, e); //$NON-NLS-1$ //$NON-NLS-2$
		this.exp = expected;
		this.fnd = found;
	}

	/** Obtiene la versi&oacute;n esperada de BouncyCastle.
	 * @return Versi&oacute;n esperada de BouncyCastle */
	public String getExpectedVersion() {
		return this.exp;
	}

	/** Obtiene la versi&oacute;n encontrada (actual) de BouncyCastle.
	 * @return Versi&oacute;n encontrada (actual) de BouncyCastle */
	public String getFoundVersion() {
		return this.fnd;
	}

}
