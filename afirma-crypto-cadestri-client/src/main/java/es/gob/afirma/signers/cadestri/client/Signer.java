package es.gob.afirma.signers.cadestri.client;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOPkcs1Signer;

final class Signer {

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	/** Prefijo para las propiedades que almacenan prefirmas. */
	private static final String PROPERTY_NAME_PRESIGN_PREFIX = "PRE."; //$NON-NLS-1$

	/** Nombre de la propiedad que contiene el n&uacute;mero de firmas proporcionadas. */
	private static final String PROPERTY_NAME_SIGN_COUNT = "SIGN_COUNT"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN_PREFIX = "PK1."; //$NON-NLS-1$

	private Signer() {
		// No instanciable
	}

	static Properties signProperties(final byte[] preSignResult,
			                         final String algorithm,
			                         final Certificate[] certChain,
			                         final PrivateKey key) throws IOException,
			                         							  AOException {

		// Convertimos la respuesta del servidor en un Properties
		final Properties preSignProperties = ProtocolConstants.base642Properties(new String(preSignResult));

		final String needPreProperty = preSignProperties.getProperty(PROPERTY_NAME_NEED_PRE);
		final boolean needPre = needPreProperty != null && "true".equalsIgnoreCase(needPreProperty); //$NON-NLS-1$

		// Es posible que se ejecute mas de una firma en el mismo lote
		int signCount = 1;
		if (preSignProperties.containsKey(PROPERTY_NAME_SIGN_COUNT)) {
			signCount = Integer.parseInt(preSignProperties.getProperty(PROPERTY_NAME_SIGN_COUNT));
		}

		for (int i = 0; i < signCount; i++) {
			final String base64PreSign = preSignProperties.getProperty(PROPERTY_NAME_PRESIGN_PREFIX + i);
			if (base64PreSign == null) {
				throw new AOException("El servidor no ha devuelto la prefirma numero " + i + ": " + new String(preSignResult)); //$NON-NLS-1$ //$NON-NLS-2$
			}

			final byte[] preSign = Base64.decode(base64PreSign);

			final byte[] pkcs1sign = new AOPkcs1Signer().sign(
				preSign,
				algorithm,
				key,
				certChain,
				null // No hay parametros en PKCS#1
			);

			// Configuramos la peticion de postfirma indicando las firmas PKCS#1 generadas
			preSignProperties.setProperty(PROPERTY_NAME_PKCS1_SIGN_PREFIX + i, Base64.encode(pkcs1sign));

			// Si no es necesaria la prefirma para completar la postfirma, la eliminamos
			if (!needPre) {
				preSignProperties.remove(PROPERTY_NAME_PRESIGN_PREFIX + i);
			}
		}

		return preSignProperties;

	}

}
