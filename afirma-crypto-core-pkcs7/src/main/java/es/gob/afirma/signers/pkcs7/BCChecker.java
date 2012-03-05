package es.gob.afirma.signers.pkcs7;

import es.gob.afirma.core.misc.Platform;

/** Comprobador de la versi&oacute;n de Bouncycastle del sistema.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BCChecker {

    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.46 o superior). */
    private static final String BC_VERSION = "1.46"; //$NON-NLS-1$

	/** Comprueba que la versi&oacute;n de BouncyCastle existente sea v1.46 o superior.
	 * @throws InvalidBouncyCastleException Cuando no se puede detectar la versi&oacute;n de
	 * BouncyCastle disponible o no es compatible con afirma. */
	@SuppressWarnings("static-method")
	public void checkBouncyCastle() {
        final String bcVersion = Platform.getBouncyCastleVersion();
        if (bcVersion == null || BC_VERSION.compareTo(bcVersion) > 0) {
            throw new InvalidBouncyCastleException(
        		"igual o superior a " + BC_VERSION, //$NON-NLS-1$
        		bcVersion
    		);
        }
	}

}
