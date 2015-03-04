/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Manejador de firmas XML XAdES Factura-E.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOFacturaESigner implements AOSigner {

    private static final AOSigner XADES_SIGNER = new AOXAdESSigner();

    private static final Set<String> ALLOWED_PARAMS = new HashSet<String>(5);
    static {
        ALLOWED_PARAMS.add("signatureProductionCity"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionProvince"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionPostalCode"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionCountry"); //$NON-NLS-1$
		ALLOWED_PARAMS.add("xadesNamespace"); //$NON-NLS-1$
		ALLOWED_PARAMS.add("signedPropertiesTypeUrl"); //$NON-NLS-1$
    }

    private static final Properties EXTRA_PARAMS = new Properties();
    static {
        EXTRA_PARAMS.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("policyIdentifier", "http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("policyIdentifierHash", "Ohixl6upD6av8N7pEvDABhEL6hM=");  //$NON-NLS-1$//$NON-NLS-2$
        EXTRA_PARAMS.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("policyDescription", "facturae31"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("signerClaimedRoles", "emisor"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("facturaeSign", "true"); //$NON-NLS-1$ //$NON-NLS-2$:P
    }

    /** Operaci&oacute;n no soportada. */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties extraParams) throws AOException {
    	throw new UnsupportedOperationException("No se soporta la cofirma de facturas"); //$NON-NLS-1$
    }

    /** Operaci&oacute;n no soportada. */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final Certificate[] certChain,
                         final Properties extraParams) throws AOException {
    	throw new UnsupportedOperationException("No se soporta la cofirma de facturas"); //$NON-NLS-1$
    }

    /** Operaci&oacute;n no soportada, se lanza una <code>UnsupportedOperationException</code>. */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final Certificate[] certChain,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No se soporta la contrafirma de facturas"); //$NON-NLS-1$
    }

    /** Firma Facturas en formato XAdES Factura-E.
     * @param data Factura electr&oacute;nica.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionProvince</i></b></dt>
     *   <dd>Provincia en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionPostalCode</i></b></dt>
     *   <dd>C&oacute;digo postal en el que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionCountry</i></b></dt>
     *   <dd>Pa&iacute;s en el que se realiza la firma</dd>
     * </dl>
     * @return Cofirma en formato XAdES
     * @throws InvalidEFacturaDataException Cuando se proporcionan datos que no son una factura electr&oacute;nica
     * @throws EFacturaAlreadySignedException Cuando se proporciona un factura ya firmada
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     * @throws IOException Cuando ocurren problemas relacionados con la lectura de los datos */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final Certificate[] certChain,
                       final Properties extraParams) throws AOException, IOException {
        if (!isValidDataFile(data)) {
            throw new InvalidEFacturaDataException();
        }
        if (isSign(data)) {
        	throw new EFacturaAlreadySignedException();
        }
        final Properties xParams = (Properties) EXTRA_PARAMS.clone();
        if (extraParams != null) {
            for (final Object k : extraParams.keySet()) {
                if (ALLOWED_PARAMS.contains(k)) {
                    xParams.put(k, extraParams.get(k));
                }
            }
        }
        return XADES_SIGNER.sign(data, algorithm, key, certChain, xParams);
    }

    /** {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException {
        return XADES_SIGNER.getSignersStructure(sign, asSimpleSignInfo);
    }

    /** {@inheritDoc} */
    @Override
	public boolean isSign(final byte[] is) throws IOException {
        return XADES_SIGNER.isSign(is) && isValidDataFile(is);
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * Importante: El que los datos sean una factura electr&oacute;nica no implica que puedan ser firmados, si esta
     * ya est&aacute; firmada el a&ntilde;adido de una firma adicional invalidar&iacute;a la factura
     * @param is Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    @Override
	public boolean isValidDataFile(final byte[] is) {
        if (is == null || is.length == 0) {
            return false;
        }
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        try {
            final Document doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(is));
            final Element rootNode = doc.getDocumentElement();
            final String rootNodePrefix = rootNode.getPrefix();

            if (!((rootNodePrefix != null ? rootNodePrefix + ":" : "") + "Facturae").equals(rootNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                return false;
            }

            final Set<String> childs = new HashSet<String>(3);
            childs.add("FileHeader"); //$NON-NLS-1$
            childs.add("Parties"); //$NON-NLS-1$
            childs.add("Invoices"); //$NON-NLS-1$

            final NodeList nl = rootNode.getChildNodes();
            for (int i=0;i<nl.getLength();i++) {
                final String nodeName = nl.item(i).getNodeName();
                if (childs.contains(nodeName)) {
                    childs.remove(nodeName);
                }
            }
            if (childs.size() > 0) {
                return false;
            }

        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        return XADES_SIGNER.getSignedName(originalName, inText);
    }

    /** {@inheritDoc} */
    @Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
        return XADES_SIGNER.getData(signData);
    }

    /** {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException, IOException {
    	final AOSignInfo facturaeSignInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_FACTURAE);
    	facturaeSignInfo.setVariant(null);
    	return facturaeSignInfo;
    }

}
