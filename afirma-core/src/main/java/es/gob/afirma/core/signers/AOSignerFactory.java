/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Factor&iacute;a que gestiona todos los formatos de firma disponibles en cada
 * momento en el cliente. */
public final class AOSignerFactory {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static AOSignerFactory signerFactory = null;

    private static final Map<String, AOSigner> SIGNERS = new HashMap<String, AOSigner>(7);

    /** Listado de formatos de firma soportados y el manejador de firma asociado. */
    private static final Map<String, String> SIGNERS_CLASSES = new HashMap<String, String>(7);
    
    static {
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_CADES,              "es.gob.afirma.signers.cades.AOCAdESSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_CMS,                "es.gob.afirma.signers.cms.AOCMSSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XADES_DETACHED,     "es.gob.afirma.signers.xades.AOXAdESSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,    "es.gob.afirma.signers.xades.AOXAdESSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,   "es.gob.afirma.signers.xades.AOXAdESSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED,   "es.gob.afirma.signers.xmldsig.AOXMLDSigSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED,  "es.gob.afirma.signers.xmldsig.AOXMLDSigSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "es.gob.afirma.signers.xmldsig.AOXMLDSigSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_PDF,                "es.gob.afirma.signers.pades.AOPDFSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_ODF,                "es.gob.afirma.signers.odf.AOODFSigner"); //$NON-NLS-1$
        SIGNERS_CLASSES.put(AOSignConstants.SIGN_FORMAT_OOXML,              "es.gob.afirma.signers.ooxml.AOOOXMLSigner"); //$NON-NLS-1$
    }
    
    private AOSignerFactory() {
        // No permitimos la instanciacion externa
    }

    /** Obtiene una instancia de la factor&iacute;a.
     * @return Instancia de la factor&iacute;a */
    public static AOSignerFactory getInstance() {
        if (signerFactory != null) {
            return signerFactory;
        }
        signerFactory = new AOSignerFactory();
        return signerFactory;
    }

    /** Recupera un manejador de firma capaz de tratar la firma indicada. En caso
     * de no tener ning&uacute;n manejador compatible se devolver&aacute; <code>null</code>.
     * @param signData Firma electr&oacute;nica
     * @return Manejador de firma */
    public static AOSigner getSigner(final byte[] signData) {
        if (signData == null) {
            throw new IllegalArgumentException("No se han indicado datos de firma"); //$NON-NLS-1$
        }
        for (final String format : SIGNERS_CLASSES.keySet()) {
            if (SIGNERS.get(format) == null) {
                try {
                    SIGNERS.put(format, (AOSigner) AOUtil.classForName(SIGNERS_CLASSES.get(format)).newInstance());
                }
                catch(final Exception e) {
                    LOGGER.severe("No se ha podido instanciar un manejador para el formato de firma '" + format + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    e.printStackTrace();
                    continue;
                }
                final AOSigner signer = SIGNERS.get(format);
                if (signer != null && signer.isSign(signData)) {
                    return signer;
                }
            }
        }
        return null;
    }

    /** Obtiene un manejador para un formato de firma dado. En caso de no
     * encontrar ninguno, se devuelve <code>null</code>.
     * @param signFormat Formato de firma para el cual solicitamos el manejador.
     * @return Manejador capaz de firmar en el formato indicado. */
    public static AOSigner getSigner(final String signFormat) {
        if (!SIGNERS_CLASSES.containsKey(signFormat)) {
            LOGGER.warning("El formato de firma '" + signFormat + "' no esta soportado, se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }
        if (SIGNERS.get(signFormat) == null) {
            try {
                SIGNERS.put(signFormat, (AOSigner) AOUtil.classForName(SIGNERS_CLASSES.get(signFormat)).newInstance());
            }
            catch(final Exception e) {
                LOGGER.severe("No se ha podido instanciar un manejador para el formato de firma '" + signFormat + "', se devolvera null: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return SIGNERS.get(signFormat);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Factoria de manejadores de firma. Formatos soportados:"); //$NON-NLS-1$
        for (final String format : SIGNERS_CLASSES.keySet()) {
            sb.append(" ").append(format); //$NON-NLS-1$
        }
        return sb.toString();
    }

}
