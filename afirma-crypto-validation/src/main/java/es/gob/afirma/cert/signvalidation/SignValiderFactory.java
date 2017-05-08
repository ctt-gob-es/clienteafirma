package es.gob.afirma.cert.signvalidation;

import java.util.logging.Logger;

/** Factoria para la creaci&oacute;n de los validadores de firma.
 * @author Sergio Mart&iacute;nez Rico. */
public final class SignValiderFactory {

	/* Listado de los validadores de firma soportados y los identificadores de formato de firma asociados. */
	private static final String SIGNER_VLIDER_CLASS_BINARY 	= "es.gob.afirma.cert.signvalidation.ValidateBinarySignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_PDF   	= "es.gob.afirma.cert.signvalidation.ValidatePdfSignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_XML	= "es.gob.afirma.cert.signvalidation.ValidateXMLSignature"; //$NON-NLS-1$

	private SignValiderFactory() {
		// No permitimos la instanciacion externa
	}

	/** Obtiene un validador de firmas para el tipo de dato proporcionado.
	 * @param data Firma a validar.
	 * @return Validador adecuado o <code>null</code> si no hay ninguno para ese tipo de dato.
	 * @throws IllegalArgumentException Fallo si la firma obtenida est&aacute; vac&iacute;a. */
	public static SignValider getSignValider(final byte[] data) throws IllegalArgumentException {
		if (data == null) {
			throw new IllegalArgumentException("No se han indicado datos de firma"); //$NON-NLS-1$
		}
		final String validerClassName;
		if (DataAnalizerUtil.isPDF(data)) {
			validerClassName = SIGNER_VALIDER_CLASS_PDF;
        }
        else if (DataAnalizerUtil.isFacturae(data)) { // Factura electronica
        	validerClassName = SIGNER_VALIDER_CLASS_XML;
        }
        else if (DataAnalizerUtil.isXML(data)) {
        	validerClassName = SIGNER_VALIDER_CLASS_XML;
        }
        else if(DataAnalizerUtil.isBinary(data)) {
        	validerClassName = SIGNER_VLIDER_CLASS_BINARY;
        }
        else {
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"No hay un validador para el tipo de dato proporcionado" //$NON-NLS-1$
			);
        	return null;
        }
		try {
        	return (SignValider) Class.forName(validerClassName).getDeclaredConstructor().newInstance();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido instanciar el validador '" + validerClassName + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return null;
		}
	}
}
