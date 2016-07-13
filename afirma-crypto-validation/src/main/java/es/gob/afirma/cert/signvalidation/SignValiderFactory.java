package es.gob.afirma.cert.signvalidation;

/** Factoria para la creaci&oacute;n de los validadores de firma.
 * @author Sergio Mart&iacute;nez Rico. */
public class SignValiderFactory {

	/* Listado de los validadores de firma soportados y los identificadores de formato de firma asociados. */
	private static final String SIGNER_VLIDER_CLASS_BINARY 	= "es.gob.afirma.cert.signvalidation.ValidateBinarySignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_PDF   	= "es.gob.afirma.cert.signvalidation.ValidatePdfSignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_XML	= "es.gob.afirma.cert.signvalidation.ValidateXMLSignature"; //$NON-NLS-1$


	private SignValiderFactory() {
		// No permitimos la instanciacion externa
	}

	/** Obtiene el resultado al validar la firma.
	 * @param data Firma a validar.
	 * @return Resultado la validar la firma.
	 * @throws IllegalArgumentException Fallo si la firma obtenida est&aacute; vac&iacute;a. */
	public static SignValider getSignValider(byte[] data) throws IllegalArgumentException {
		if (data == null) {
			throw new IllegalArgumentException("No se han indicado datos de firma"); //$NON-NLS-1$
		}
		try {
			if (DataAnalizerUtil.isPDF(data)) {
	        	return (SignValider) Class.forName(SIGNER_VALIDER_CLASS_PDF).newInstance();
	        }
	        else if (DataAnalizerUtil.isFacturae(data)) { // Factura electronica
	        	return (SignValider) Class.forName(SIGNER_VALIDER_CLASS_XML).newInstance();
	        }
	        else if (DataAnalizerUtil.isXML(data)) {
	        	return (SignValider) Class.forName(SIGNER_VALIDER_CLASS_XML).newInstance();
	        }
	        else if(DataAnalizerUtil.isBinary(data)) {
	        	return (SignValider) Class.forName(SIGNER_VLIDER_CLASS_BINARY).newInstance();
	        }
	        return null;
		}
		catch (final Exception e) {
			return null;
		}
	}
}
