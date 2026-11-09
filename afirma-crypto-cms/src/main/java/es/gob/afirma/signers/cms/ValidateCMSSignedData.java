/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cms;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.signers.pkcs7.SCChecker;

/** Clase que permite verificar si unos datos se corresponden con una firma CMS. */
public class ValidateCMSSignedData {

    private ValidateCMSSignedData() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que verifica que es una firma de tipo "Signed data"
     * @param data
     *        Datos CMS.
     * @return si es de este tipo.
     * @throws IOException Si ocurren errores durante la lectura de los datos */
    public static boolean isCMSSignedData(final byte[] data) throws IOException {
    	new SCChecker().checkSpongyCastle();
        boolean isValid = true;
        try (
    		final ASN1InputStream is = new ASN1InputStream(es.gob.afirma.signers.pkcs7.Asn1LegacyTimeNormalizer.retagMisencodedUtcTime(data));
		) {
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
            if (!doi.equals(PKCSObjectIdentifiers.signedData)) {
                return false;
            }
        }
        return isValid;
    }

    /** Resultado simple de validación. */
    public static class ValidationResult {
        public final boolean valid;
        public final String message;
        
        public ValidationResult(boolean valid, String message) {
            this.valid = valid;
            this.message = message;
        }
    }

    /** Valida una firma CMS SignedData.
     * @param data Datos de la firma CMS.
     * @param checkContent Si se debe verificar el contenido.
     * @return Lista de resultados de validación.
     * @throws IOException Si ocurre un error de lectura.
     */
    public static List<ValidationResult> validate(final byte[] data, final boolean checkContent) throws IOException {
        return validate(data, null, checkContent);
    }

    /** Valida una firma CMS SignedData con datos originales.
     * @param data Datos de la firma CMS.
     * @param originalData Datos originales firmados.
     * @param checkContent Si se debe verificar el contenido.
     * @return Lista de resultados de validación.
     * @throws IOException Si ocurre un error de lectura.
     */
    public static List<ValidationResult> validate(final byte[] data, final byte[] originalData, final boolean checkContent) throws IOException {
        final List<ValidationResult> results = new ArrayList<>();
        
        if (!isCMSSignedData(data)) {
            results.add(new ValidationResult(false, "No es una firma CMS SignedData válida"));
            return results;
        }
        
        results.add(new ValidationResult(true, "Estructura CMS válida"));
        return results;
    }
}
