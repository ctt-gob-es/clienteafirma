/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import java.util.logging.Logger;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Factor&iacute;a para la creaci&oacute;n de los validadores de firma.
 * @author Sergio Mart&iacute;nez Rico. */
public final class SignValiderFactory {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/* Listado de los validadores de firma soportados y los identificadores de formato de firma asociados. */
	private static final String SIGNER_VALIDER_CLASS_BINARY 	= "es.gob.afirma.signvalidation.ValidateBinarySignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_PDF   	= "es.gob.afirma.signvalidation.ValidatePdfSignature"; //$NON-NLS-1$
	private static final String SIGNER_VALIDER_CLASS_XML	= "es.gob.afirma.signvalidation.ValidateXMLSignature"; //$NON-NLS-1$

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
        	LOGGER.warning("No hay un validador para OOXML"); //$NON-NLS-1$
        	return null;
        }
        else if(DataAnalizerUtil.isODF(data)) {
        	LOGGER.warning("No hay un validador para ODF"); //$NON-NLS-1$
        	return null;
        }
        else  {
        	validerClassName = SIGNER_VALIDER_CLASS_BINARY;
        }
		try {
        	return (SignValider) Class.forName(validerClassName).getDeclaredConstructor().newInstance();
		}
		catch (final Exception e) {
			LOGGER.severe("No se ha podido instanciar el validador '" + validerClassName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	/** Obtiene un validador de firmas compatible con un manejador de firma.
	 * @param signer Manejador de firma del que se quiere obtener un validador compatible.
	 * @return Validador adecuado o <code>null</code> si no hay ninguno compatible con ese manejador. */
	public static SignValider getSignValider(final AOSigner signer) throws IllegalArgumentException {
		String validerClassName = null;
		if (signer instanceof AOCAdESSigner) {
			validerClassName = SIGNER_VALIDER_CLASS_BINARY;
		}
		else if (signer instanceof AOXAdESSigner || signer instanceof AOFacturaESigner) {
			validerClassName = SIGNER_VALIDER_CLASS_XML;
		}
		else if (signer instanceof AOPDFSigner) {
			validerClassName = SIGNER_VALIDER_CLASS_PDF;
		}

		SignValider valider = null;
		if (validerClassName != null) {
			try {
				valider = (SignValider) Class.forName(validerClassName).getDeclaredConstructor().newInstance();
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido instanciar el validador '" + validerClassName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return valider;
	}
}
