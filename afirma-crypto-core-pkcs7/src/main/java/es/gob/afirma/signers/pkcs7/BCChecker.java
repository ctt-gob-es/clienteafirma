package es.gob.afirma.signers.pkcs7;



/** Comprobador de la versi&oacute;n de Bouncycastle del sistema.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BCChecker {

    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.47 o superior). */
    private static final String BC_VERSION = "1.47"; //$NON-NLS-1$

	/** Comprueba que la versi&oacute;n de BouncyCastle existente sea v1.47 o superior.
	 * @throws InvalidBouncyCastleException Cuando no se puede detectar la versi&oacute;n de
	 * BouncyCastle disponible o no es compatible con afirma. */
	@SuppressWarnings("static-method")
	public void checkBouncyCastle() {
		try {
			Class.forName("org.bouncycastle.asn1.ASN1Primitive"); //$NON-NLS-1$
		}
		catch(final ClassNotFoundException e) {
			throw new InvalidBouncyCastleException(BC_VERSION, "1.46 o anterior", e); //$NON-NLS-1$
		}
	}

}
