/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.cert.signvalidation;

import java.util.logging.Logger;

/** Factor&iacute;a para la creaci&oacute;n de los validadores de firma.
 * @author Sergio Mart&iacute;nez Rico. */
public final class SignValiderFactory {

	/* Listado de los validadores de firma soportados y los identificadores de formato de firma asociados. */
	private static final String SIGNER_VALIDER_CLASS_BINARY 	= "es.gob.afirma.cert.signvalidation.ValidateBinarySignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_PDF   	= "es.gob.afirma.cert.signvalidation.ValidatePdfSignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_XML	= "es.gob.afirma.cert.signvalidation.ValidateXMLSignature"; //$NON-NLS-1$

	private SignValiderFactory() {
		// No permitimos la instanciacion externa
	}

	/** Obtiene un validador de firmas para el tipo de dato proporcionado.
	 * @param data Firma a validar.
	 * @return Validador adecuado o <code>null</code> si no hay ninguno para ese tipo de dato.
	 * @throws IllegalArgumentException Si la firma proporcionada es nula o vac&iacute;a. */
	public static SignValider getSignValider(final byte[] data) throws IllegalArgumentException {
		if (data == null || data.length < 1) {
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
        else if(DataAnalizerUtil.isOOXML(data)) {
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"No hay un validador para OOXML" //$NON-NLS-1$
			);
        	return null;
        }
        else if(DataAnalizerUtil.isODF(data)) {
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"No hay un validador para ODF" //$NON-NLS-1$
			);
        	return null;
        }
        else  {
        	validerClassName = SIGNER_VALIDER_CLASS_BINARY;
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
