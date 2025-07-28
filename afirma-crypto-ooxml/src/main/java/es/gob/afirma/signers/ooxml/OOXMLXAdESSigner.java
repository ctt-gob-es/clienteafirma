/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.signers.xades.XAdESUtil;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
import es.uji.crypto.xades.jxades.security.xml.XAdES.CommitmentTypeIndication;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlace;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignatureProductionPlaceImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRole;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignerRoleImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES_EPES;
import es.uji.crypto.xades.jxades.util.DOMOutputImpl;

/** Firmador XAdES OOXML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class OOXMLXAdESSigner {

	private static final String ID_PACKAGE_OBJECT = "idPackageObject"; //$NON-NLS-1$
	private static final String ID_OFFICE_OBJECT = "idOfficeObject"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

    /** URI que define la versi&oacute;n por defecto de XAdES. */
    private static final String XADESNS = "http://uri.etsi.org/01903/v1.3.2#"; //$NON-NLS-1$

    private static final String XADES_SIGNATURE_PREFIX = "xd"; //$NON-NLS-1$
    private static final String XML_SIGNATURE_PREFIX = "ds"; //$NON-NLS-1$

    private OOXMLXAdESSigner() {
    	// No permitimos la instanciacion
    }

    /**
     * Obtiene el XML de firma XAdES <i>enveloping</i> OOXML.
     * @param ooXmlDocument Documento OOXML original.
     * @param algorithm Algoritmo de firma.
     * @param pk Clave privada para la firma.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales de la firma.
     * @return XML de firma.
     * @throws ParserConfigurationException Si hay problemas con el analizador XML por defecto.
     * @throws GeneralSecurityException Si ocurre alg&uacute;n problema de seguridad.
     * @throws SAXException Si hay problemas en XML SAX.
     * @throws IOException Si hay problemas gen&eacute;ricos en el tratamiento de datos.
     * @throws XMLSignatureException Si hay problemas con la firma XML.
     * @throws MarshalException Si hay problemas con la envoltura de la firma XML.
     * @throws AOException Cuando ocurre un error de firma ya identificado.
     */
    static byte[] getSignedXML(final byte[] ooXmlDocument,
    		                   final String algorithm,
    						   final PrivateKey pk,
    						   final X509Certificate[] certChain,
    						   final Properties xParams) throws ParserConfigurationException,
    								                            GeneralSecurityException,
    								                            IOException,
    								                            SAXException,
    								                            MarshalException,
    								                            XMLSignatureException, AOException {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new IllegalArgumentException(
				"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final Document docSignature = SecureXmlBuilder.getSecureDocumentBuilder().newDocument();

		// Instancia XADES_EPES
		final XAdES_EPES xades = (XAdES_EPES) XAdES.newInstance(
			XAdES.EPES,                       // XAdES-EPES
			XADESNS,                          // XAdES NameSpace
			XADES_SIGNATURE_PREFIX,           // XAdES Prefix
			XML_SIGNATURE_PREFIX,             // XMLDSig Prefix
			DigestMethod.SHA512,              // DigestMethod
			docSignature,                     // Document
			docSignature.getDocumentElement() // Element
		);

		//*******************************************************************
		//************* ATRIBUTOS XAdES *************************************

		// SigningCertificate
		xades.setSigningCertificate(certChain[0]);

		// SignatureProductionPlace
		final SignatureProductionPlace spp = getSignatureProductionPlace(
			extraParams.getProperty(OOXMLExtraParams.SIGNATURE_PRODUCTION_CITY),
			extraParams.getProperty(OOXMLExtraParams.SIGNATURE_PRODUCTION_PROVINCE),
			extraParams.getProperty(OOXMLExtraParams.SIGNATURE_PRODUCTION_POSTAL_CODE),
			extraParams.getProperty(OOXMLExtraParams.SIGNATURE_PRODUCTION_COUNTRY)
		);
		if (spp != null) {
			xades.setSignatureProductionPlace(spp);
		}

		// CommitmentTypeIndications:
		//  - http://www.w3.org/TR/XAdES/#Syntax_for_XAdES_The_CommitmentTypeIndication_element
		//  - http://uri.etsi.org/01903/v1.2.2/ts_101903v010202p.pdf
		final List<CommitmentTypeIndication> ctis = XAdESUtil.parseCommitmentTypeIndications(extraParams, null);
		if (ctis != null && ctis.size() > 0) {
			xades.setCommitmentTypeIndications(ctis);
		}

		// SignerRole
		final String signerRoleValue = extraParams.getProperty(OOXMLExtraParams.SIGNER_CLAIMED_ROLES);
		if (signerRoleValue != null) {
			final SignerRole signerRole = new SignerRoleImpl();
			signerRole.addClaimedRole(signerRoleValue);
			xades.setSignerRole(signerRole);
		}

		// SigningTime
		xades.setSigningTime(new Date());

		//************* FIN ATRIBUTOS XAdES *********************************
		//*******************************************************************

		// Creamos el objeto final de firma
		final OOXMLAdvancedSignature xmlSignature = OOXMLAdvancedSignature.newInstance(xades, ooXmlDocument);

		// Lista de referencias a firmar
		final List<Reference> referenceList = new ArrayList<>();

		// Identificador de primer nivel de la firma
		final String signatureId = "xmldsig-" + UUID.randomUUID().toString(); //$NON-NLS-1$

		// Creamos la factoria de firma XML
	    final XMLSignatureFactory fac = Utils.getDOMFactory();

	    // Huella digital para las referencias
	    final DigestMethod digestMethod = fac.newDigestMethod(DigestMethod.SHA512, null);

		// Anadimos los nodos especificos OOXML y las referencias a ellos

		// idPackageObject
		xmlSignature.addXMLObject(
			OOXMLPackageObjectHelper.getPackageObject(
				ID_PACKAGE_OBJECT,
				fac,
				ooXmlDocument,
				docSignature,
				signatureId
			)
		);
		referenceList.add(
			fac.newReference(
				"#" + ID_PACKAGE_OBJECT, //$NON-NLS-1$
				digestMethod,
				null,
				"http://www.w3.org/2000/09/xmldsig#Object", //$NON-NLS-1$
				null
			)
		);

		// idOfficeObject
		xmlSignature.addXMLObject(
			OOXMLOfficeObjectHelper.getOfficeObject(
				ID_OFFICE_OBJECT,
				fac,
				docSignature,
				signatureId,
				extraParams.getProperty(OOXMLExtraParams.SIGNATURE_COMMENTS),
				extraParams.getProperty(OOXMLExtraParams.SIGNATURE_ADDRESS1),
				extraParams.getProperty(OOXMLExtraParams.SIGNATURE_ADDRESS2),
				ctis != null && ctis.size() > 0 ? "1" : null //$NON-NLS-1$
			)
		);
		referenceList.add(
			fac.newReference(
				"#" + ID_OFFICE_OBJECT, //$NON-NLS-1$
				digestMethod,
				null,
				"http://www.w3.org/2000/09/xmldsig#Object", //$NON-NLS-1$
				null
			)
		);

		xmlSignature.sign(
			certChain,
			pk,
			XMLConstants.SIGN_ALGOS_URI.get(algorithm),
			referenceList,
			signatureId
		);

		return writeXML(
			docSignature.getDocumentElement()
		);
    }

    /** Escribe un XML como texto.
     * @param node Nodo XML que queremos pasar a texto
     * @return Cadena de texto con el XML en forma de array de octetos */
    private static byte[] writeXML(final Node node) {

        // La codificacion por defecto sera UTF-8
        final String xmlEncoding = StandardCharsets.UTF_8.name();

        // Primero creamos un writer
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Writer writer = null;
        try {
            writer = new OutputStreamWriter(baos, xmlEncoding);
        }
        catch (final UnsupportedEncodingException e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            writer = new OutputStreamWriter(baos);
        }

        // Ahora escribimos el XML usando XALAN
        writeXMLwithXALAN(writer, node, xmlEncoding);

        try {
        	SecureXmlBuilder.getSecureDocumentBuilder().parse(new ByteArrayInputStream(baos.toByteArray()));
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido recargar el XML para insertar los atributos de la cabecera, quizas la codificacion se vea afectada: " + e); //$NON-NLS-1$
            return baos.toByteArray();
        }

        // Y devolvemos el resultado como array de bytes, insertando antes la
        // cabecera de hoja de estilo
        try {
            return new String(baos.toByteArray(), xmlEncoding).getBytes(xmlEncoding);
        }
        catch (final Exception e) {
            LOGGER.warning("La codificacion '" + xmlEncoding + "' no es valida, se usara la por defecto del sistema: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return new String(baos.toByteArray()).getBytes();
        }
    }

	private static void writeXMLwithXALAN(final Writer writer, final Node node, final String xmlEncoding) {
        final LSSerializer serializer = ((DOMImplementationLS) node.getOwnerDocument().getImplementation()).createLSSerializer();
        serializer.getDomConfig().setParameter("namespaces", Boolean.FALSE); //$NON-NLS-1$
        final DOMOutputImpl output = new DOMOutputImpl();
        output.setCharacterStream(writer);
        if (xmlEncoding != null) {
            output.setEncoding(xmlEncoding);
        }
        serializer.write(node, output);
    }

    private static SignatureProductionPlace getSignatureProductionPlace(final String city,
                                                                        final String province,
                                                                        final String postalCode,
                                                                        final String country) {
    	if (city == null && province == null && postalCode == null && country == null) {
    		return null;
    	}
    	return new SignatureProductionPlaceImpl(city, province, postalCode, country);
    }

}
