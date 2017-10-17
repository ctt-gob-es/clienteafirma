/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.security.PrivilegedExceptionAction;
import java.util.logging.Logger;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;

/**
 * Acci&oacute;n para la seleccion de un manejador de firma.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class SelectSignerAction implements PrivilegedExceptionAction<AOSigner> {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final String format;
    private final byte[] data;

    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con el formato de firma indicado.
     * @param format Formato de firma.
     */
    SelectSignerAction(final String format) {
        this.format = format;
        this.data = null;
    }

    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con la firma indicada.
     * @param sign Firma electr&oacute;nica para la que se desea el manejador.
     */
    SelectSignerAction(final byte[] sign) {
        this.data = sign != null ? sign.clone() : null;
        this.format = null;
    }

    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con el formato de firma indicado. Si se indica el formato "AUTO", se determinar&aacute;
     * un formato seg&uacute; el tipo de datos indicados.
     * @param format Formato de firma.
     * @param data Datos que se van a firmar.
     */
    SelectSignerAction(final String format, final byte[] data) {
        this.format = format;
        this.data = data;
    }

    /**
     * Selecciona el manejador de firma adecuado para el formato o los datos indicados.
     * Si no se encuentra un manejador compatible, se devuelve {@code null}.
     * @return Manejador de firma.
     * @throws IOException Cuando se produce un error durante la lectura de los datos.
     */
	@Override
	public AOSigner run() throws IOException {
		AOSigner signer = null;
		if (this.format != null) {
			if (!AOSignConstants.SIGN_FORMAT_AUTO.equalsIgnoreCase(this.format)) {
				LOGGER.info("Seleccionamos el signer correspondiente al formato " + this.format); //$NON-NLS-1$
				signer = AOSignerFactory.getSigner(this.format);
			}
			else if (this.data != null) {
				LOGGER.info("Seleccion automatica de formato"); //$NON-NLS-1$
				if (DataAnalizerUtil.isPDF(this.data)) {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_PADES); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_PADES);
				}
				else if (DataAnalizerUtil.isFacturae(this.data)) {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_FACTURAE); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_FACTURAE);
				}
				else if (DataAnalizerUtil.isXML(this.data)) {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_XADES); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_XADES);
				}
				else if (DataAnalizerUtil.isODF(this.data)) {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_ODF); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_ODF);
				}
				else if (DataAnalizerUtil.isOOXML(this.data)) {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_OOXML); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_OOXML);
				}
				else {
					LOGGER.info("Se selecciona el formato " + AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
					signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_CADES);
				}
			}
			else {
				LOGGER.warning("No se han indicado datos a partir de los cuales determinar el formato"); //$NON-NLS-1$
			}
		}
		else if (this.data != null) {
			// Si no se ha indicado un formato, se interpreta que los datos son una firma
			signer = AOSignerFactory.getSigner(this.data);
		}

        return signer;
	}
}
