/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

import java.net.URL;
import java.security.MessageDigest;
import java.util.Properties;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;


/** Pol&iacute;tica de firma para AdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AdESPolicy {

	private static final String DEFAULT_HASH_ALGORITHM = "SHA-512"; //$NON-NLS-1$

    private String policyIdentifier = null;
    private String policyIdentifierHash = null;
    private String policyIdentifierHashAlgorithm = null;
    private URL policyQualifier = null;

    /** Crea una pol&iacute;tica para firma AdES.
     * @param identifier Identificador de la pol&iacute;tica de firma (URL u OID, directo o como URN)
     * @param identifierHash Huella digital de la pol&iacute;tica de firma en formato ASN.1 procesable identificado por
     *                             <code>policyIdentifier</code>. Puede ser nulo
     * @param identifierHashAlgorithm Algoritmo de huella digital usado para el c&aacute;lculo del valor indicado
     *                                      en <code>policyIdentifierHashAlgorithm</code>. Es obligatorio si el valor
     *                                      indicado en <code>policyIdentifierHashAlgorithm</code> no es nulo
     * @param qualifier URL que apunta a una descripci&oacute;n legible de la pol&iacute;tica (normalmente un PDF).
     *                  Puede ser <code>null</code>.
     */
    public AdESPolicy(final String identifier,
                      final String identifierHash,
                      final String identifierHashAlgorithm,
                      final String qualifier) {
        setValues(identifier, identifierHash, identifierHashAlgorithm, qualifier);
    }


    private void setValues(final String identifier,
                           final String identifierHash,
                           final String identifierHashAlgorithm,
                           final String qualifier) {

        if (identifier == null || identifier.isEmpty()) {
            throw new IllegalArgumentException("El identificador de politica no puede ser nulo ni vacio"); //$NON-NLS-1$
        }

        this.policyIdentifier = identifier;

        if (identifierHash == null || "0".equals(identifierHash)) { //$NON-NLS-1$

        	this.policyIdentifierHashAlgorithm = identifierHashAlgorithm == null ?
        			DEFAULT_HASH_ALGORITHM : AOSignConstants.getDigestAlgorithmName(identifierHashAlgorithm);

            try {
                this.policyIdentifierHash =  Base64.encode(
                		MessageDigest.getInstance(this.policyIdentifierHashAlgorithm).digest(
                				AOUtil.getDataFromInputStream(new URL(identifier).openStream())));
            }
            catch(final Exception e) {
                throw new IllegalArgumentException("Si no se especifica la huella digital de la politica es necesario que el identificador sea una URL accesible universalmente: " + e, e); //$NON-NLS-1$
            }
        }
        else {

        	if (identifierHashAlgorithm == null || identifierHashAlgorithm.isEmpty()) {
                throw new IllegalArgumentException("Si se indica la huella digital del identificador de politica es obligatorio indicar tambien el algoritmo"); //$NON-NLS-1$
            }

        	if (!Base64.isBase64(identifierHash.getBytes())) {
        		throw new IllegalArgumentException("La huella digital de la politica debe estar en formato Base64"); //$NON-NLS-1$
        	}

        	this.policyIdentifierHash = identifierHash;
        	this.policyIdentifierHashAlgorithm = AOSignConstants.getDigestAlgorithmName(identifierHashAlgorithm);
        }

        if (qualifier != null && !qualifier.isEmpty()) {
            try {
                this.policyQualifier = new URL(qualifier);
            }
            catch (final Exception e) {
                throw new IllegalArgumentException("El calificador de la politica debe ser una URL valida", e); //$NON-NLS-1$
            }
        }
    }

    /** Obtiene el identificador de la pol&iacute;tica de firma.
     * @return Identificador de la pol&iacute;tica de firma
     */
    public String getPolicyIdentifier() {
        return this.policyIdentifier;
    }

    /** Obtiene la huella digital del identificador de la pol&iacute;tica de firma.
     * @return Huella digital del identificador de la pol&iacute;tica de firma
     */
    public String getPolicyIdentifierHash() {
        return this.policyIdentifierHash;
    }

    /** Obtiene el algoritmo usado para el c&aacute;lculo de la huella digital del identificador de la pol&iacute;tica de firma.
     * @return Algoritmo usado para el c&aacute;lculo de la huella digital del identificador de la pol&iacute;tica de firma
     */
    public String getPolicyIdentifierHashAlgorithm() {
        return this.policyIdentifierHashAlgorithm;
    }

    /** Obtiene el calificador de la pol&iacute;tica de firma.
     * @return Calificador de la pol&iacute;tica de firma
     */
    public URL getPolicyQualifier() {
        return this.policyQualifier;
    }

    /** Crea una pol&iacute;tica AdES a partir de un fichero de propiedades.
     * Las propiedades aceptadas son:
     * <ul>
     *  <li><b>policyIdentifier</b>: Identificador de la pol&iacute;tica de firma (URL u OID
     *  directo o como URN que identifica un&iacute;vocamente el fichero de pol&iacute;tica
     *  computacionalmente procesable)
     *  <li><b>policyIdentifierHash</b>: Huella digital de la pol&iacute;tica de firma en formato
     *  ASN.1 procesable identificado por <code>policyIdentifier</code>. Puede ser nulo
     *  <li><b>policyIdentifierHashAlgorithm</b>: Algoritmo de huella digital usado para el
     *  c&aacute;lculo del valor indicado en <code>policyIdentifierHashAlgorithm</code>. Es
     *  obligatorio si el valor indicado en <code>policyIdentifierHashAlgorithm</code> no es nulo
     *  <li><b>policyQualifier</b>: URL que apunta a una descripci&oacute;n legible de la
     *  pol&iacute;tica (normalmente un PDF)
     * </ul>
     * @param extraParams Propiedades de la pol&iacute;tica
     * @return Configuraci&oacute;n de pol&iacute;tica de firma o {@code null} si no se encontro el
     * identificador de la politica en el fichero de propiedades.
     * @throws IllegalArgumentException Cuando se indique un identificador de pol&iacute;tica pero
     * el resto de datos sean incongruentes.
     */
    public static AdESPolicy buildAdESPolicy(final Properties extraParams) {
    	if (extraParams == null) {
    		throw new IllegalArgumentException("Es necesario proporciona las propiedades de la politica"); //$NON-NLS-1$
    	}
    	final String policyID = extraParams.getProperty("policyIdentifier"); //$NON-NLS-1$
    	if (policyID == null) {
    		return null;
    	}

    	return new AdESPolicy(
			policyID,
			extraParams.getProperty("policyIdentifierHash"), //$NON-NLS-1$
			extraParams.getProperty("policyIdentifierHashAlgorithm"), //$NON-NLS-1$
			extraParams.getProperty("policyQualifier") //$NON-NLS-1$
		);
    }

    /** Devuelve los datos de la pol&iacute;tica como un objeto de propiedades de par&aacute;metros
     * adicionales.
     * @return Datos de la pol&iacute;tica como un objeto de propiedades de par&aacute;metros
     *         adicionales. */
    public Properties asExtraParams() {
    	final Properties p = new Properties();
    	p.put("policyIdentifier", getPolicyIdentifier()); //$NON-NLS-1$
    	if (getPolicyIdentifierHash() != null) {
    		p.put("policyIdentifierHash", getPolicyIdentifierHash()); //$NON-NLS-1$
    	}
    	if (getPolicyIdentifierHashAlgorithm() != null) {
    		p.put("policyIdentifierHashAlgorithm", getPolicyIdentifierHashAlgorithm()); //$NON-NLS-1$
    	}
    	if (getPolicyQualifier() != null) {
    		p.put("policyQualifier", getPolicyQualifier()); //$NON-NLS-1$
    	}
    	return p;
    }

    @Override
	public String toString() {
    	return
			"Politica de firma: " + //$NON-NLS-1$
			"identificador='" + getPolicyIdentifier() + "', " + //$NON-NLS-1$ //$NON-NLS-2$
			"huella='" + getPolicyIdentifierHash() + "', " + //$NON-NLS-1$ //$NON-NLS-2$
			"algoritmo de huella='" + getPolicyIdentifierHashAlgorithm() + "', " + //$NON-NLS-1$ //$NON-NLS-2$
			"calificador='" + getPolicyQualifier(); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(final Object o) {

    	if (!(o instanceof AdESPolicy)) {
    		return false;
    	}
    	final AdESPolicy other = (AdESPolicy) o;
    	return other.getPolicyIdentifier().equals(getPolicyIdentifier()) &&
    		   other.getPolicyIdentifierHash().equals(getPolicyIdentifierHash()) &&
    		   other.getPolicyIdentifierHashAlgorithm().equals(getPolicyIdentifierHashAlgorithm());
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
    	return getPolicyIdentifier().hashCode();
    }
}
