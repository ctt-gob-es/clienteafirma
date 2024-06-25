package es.gob.afirma.signers.xades;

import java.security.Key;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.interfaces.ECPrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.logging.Logger;

import org.spongycastle.jcajce.provider.asymmetric.ec.BCECPrivateKey;

/** Utilidades de claves criptogr&aacute;ficas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class KeyUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private KeyUtil() {
		// No instanciable
	}

	/** Convierte una clave privada de curva el&iacute;ptica en formato BouncyCastle a
	 * formato JSE.
	 * La implementaci&oacute;n de curva el&iacute;ptica de BouncyCastle es anterior a la
	 * de JSE e incompatible con esta &uacute;ltima, por lo que, dependiendo del orden de
	 * los proveedores, es posible que el proveedor de firma XML del JRE rechaze las claves
	 * de BouncyCastle, haciendo conveniente esta conversi&oacute;n.
	 * @param bcKey Clave en formato BouncyCastle.
	 * @return Clave en formato JSE. */
	public static ECPrivateKey ecBc2Jce(final BCECPrivateKey bcKey) {
		final PrivateKey ret;
		try {
			final KeyFactory keyFactory = KeyFactory.getInstance("EC"); //$NON-NLS-1$
			final PKCS8EncodedKeySpec kspec = new PKCS8EncodedKeySpec(bcKey.getEncoded());
			ret = keyFactory.generatePrivate(kspec);
		}
		catch(final NoSuchAlgorithmException | InvalidKeySpecException e) {
			LOGGER.info(
				"No se ha podido convertir la clave de BC a JRE, es posible que falle el proceso: " + e //$NON-NLS-1$
			);
			return bcKey;
		}
		if (ret instanceof ECPrivateKey) {
			return (ECPrivateKey) ret;
		}
		LOGGER.info(
			"No se ha podido convertir la clave de BC a JRE, es posible que falle el proceso" //$NON-NLS-1$
		);
		return bcKey;
	}

	public static boolean isMsCapiPrivateKey(final Key key) {
		try {
			final Class<?> msCapiKey = Class.forName("sun.security.mscapi.CPrivateKey", false, KeyUtil.class.getClassLoader()); //$NON-NLS-1$
			return msCapiKey.isAssignableFrom(key.getClass());
		}
		catch (final Exception e) {
			LOGGER.info("La clase \"sun.security.mscapi.CPrivateKey\" no esta disponible para comprobar el tipo de clave"); //$NON-NLS-1$
			return false;
		}
	}
}
