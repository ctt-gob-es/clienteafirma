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

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.SignedInfo;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;
import javax.xml.parsers.ParserConfigurationException;

import net.java.xades.security.xml.WrappedKeyStorePlace;
import net.java.xades.security.xml.XmlWrappedKeyInfo;
import net.java.xades.security.xml.XAdES.XAdES_BES;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;

import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import es.gob.afirma.signers.xml.CustomDOMUriDereferencer;

/** Derivado de <code>net.java.xades.security.xml.XAdES.XMLAdvancedSignature</code> con los
 * siguientes cambios:
 * <ul>
 * <li>En el <i>KeyInfo</i> no se a&ntilde;aden los elementos <i>SubjectX500Principal</i> y <i>X509IssuerSerial</i></li>
 * <li>Se puede establecer el algoritmo de firma</li>
 * <li>Se puede establecer el algoritmo de canonicalizaci&oacute;n para la firma</li>
 * <li>Se puede establecer la URL del espacio de nombres de XAdES</li>
 * <li>Se puede a&ntilde;adir una hoja de estilo en modo <i>enveloping</i> dentro de la firma
 * </ul> */
final class AOXMLAdvancedSignature extends XMLAdvancedSignature {

    private AOXMLAdvancedSignature(final XAdES_BES xades) {
        super(xades);
    }

    private String canonicalizationMethod = CanonicalizationMethod.INCLUSIVE;
    private Element styleElement = null;
    private String styleType = "text/xsl"; //$NON-NLS-1$
    private String styleEncoding = null;
    private String styleId = null;

    /** A&ntilde;ade una hoja de estilo en modo <i>enveloping</i> dentro de la
     * firma. La referencia para firmarla debe construirse de forma externa,
     * esta clase no la construye ni a&ntilde;ade
     * @param s XML de la hoja de estilo (si se proporciona un nulo no se
     *          a&ntilde;ade la hoja de estilo)
     * @param sType Tipo (MimeType) de la hoja de estilo (puede ser nulo)
     * @param sEncoding Codificaci&oacute;n de la hoja de estilo (puede ser nula)
     * @param sId Identificador de la hoja de estilo (si se proporciona un nulo
     *            no se a&ntilde;ade la hoja de estilo) */
    void addStyleSheetEnvelopingOntoSignature(final Element s,
    		                                  final String sType,
    		                                  final String sEncoding,
    		                                  final String sId) {
        this.styleElement = s;
        if (sType != null) {
            this.styleType = sType;
        }
        this.styleId = sId;
        this.styleEncoding = sEncoding;
    }

    /** Establece el algoritmo de canonicalizaci&oacute;n.
     * @param canMethod
     *        URL del algoritmo de canonicalizaci&oacute;n. Debe estar
     *        soportado en XMLDSig 1.0 &oacute; 1.1 */
    void setCanonicalizationMethod(final String canMethod) {
        if (canMethod != null) {
            this.canonicalizationMethod = canMethod;
        }
    }

    private KeyInfo newKeyInfo(final List<X509Certificate> certificates,
    		                   final String keyInfoId) throws KeyException {
        final KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
        final List<X509Certificate> x509DataList = new ArrayList<X509Certificate>();
        if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo())) {
            for (final X509Certificate cert : certificates) {
                x509DataList.add(cert);
            }
        }
        final List<XMLStructure> newList = new ArrayList<XMLStructure>();
        newList.add(keyInfoFactory.newKeyValue(certificates.get(0).getPublicKey()));
        newList.add(keyInfoFactory.newX509Data(x509DataList));
        return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

    void sign(final List<X509Certificate> certificates,
              final PrivateKey privateKey,
              final String signatureMethod,
              final List<?> refsIdList,
              final String signatureIdPrefix) throws MarshalException,
                                                     GeneralSecurityException,
                                                     XMLSignatureException {

        final List<?> referencesIdList = new ArrayList<Object>(refsIdList);

        if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace()) && certificates != null && certificates.size() > 0) {
            this.xades.setSigningCertificate(certificates.get(0));
        }

        addXMLObject(
    		marshalXMLSignature(
				this.xadesNamespace,
				this.signedPropertiesTypeUrl,
				signatureIdPrefix,
				referencesIdList
			)
		);

        final XMLSignatureFactory fac = getXMLSignatureFactory();

        if (this.styleElement != null && this.styleId != null) {
            addXMLObject(
        		fac.newXMLObject(
    				Collections.singletonList(new DOMStructure(this.styleElement)),
    				this.styleId,
    				this.styleType,
    				this.styleEncoding
				)
    		);
        }

        final List<Reference> documentReferences = getReferences(referencesIdList);
        final String keyInfoId = getKeyInfoId(signatureIdPrefix);
        documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$

        this.signature =fac.newXMLSignature(
        		fac.newSignedInfo(
    				fac.newCanonicalizationMethod(
						this.canonicalizationMethod,
						(C14NMethodParameterSpec) null
					),
                    fac.newSignatureMethod(signatureMethod, null),
                    documentReferences
                ),
                newKeyInfo(certificates, keyInfoId),
                getXMLObjects(),
                getSignatureId(signatureIdPrefix),
                getSignatureValueId(signatureIdPrefix)
        );

        this.signContext = new DOMSignContext(privateKey, this.baseElement);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());
        if (needCustomUriDereferencer()) {
        	Logger.getLogger("es.gob.afirma").info("Se instala el derreferenciador a medida XML"); //$NON-NLS-1$ //$NON-NLS-2$
        	this.signContext.setURIDereferencer(new CustomDOMUriDereferencer());
        }

        this.signature.sign(this.signContext);

    }

    // Sobreescribimos este metodo de XMLAdvancedSignature unicamente para poder instalar el
    // derreferenciador a medida

    @Override
    public void sign(final X509Certificate certificate,
    		         final PrivateKey privateKey,
    		         final String signatureMethod,
    		         final List refsIdList,
    		         final String signatureIdPrefix) throws MarshalException,
                                                            XMLSignatureException,
                                                            GeneralSecurityException,
                                                            TransformException,
                                                            IOException,
                                                            ParserConfigurationException,
                                                            SAXException {

        final List<?> referencesIdList = new ArrayList(refsIdList);

        if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace())) {
            this.xades.setSigningCertificate(certificate);
        }

        final XMLObject xadesObject = marshalXMLSignature(
    		this.xadesNamespace,
            this.signedPropertiesTypeUrl,
            signatureIdPrefix,
            referencesIdList
        );
        addXMLObject(xadesObject);

        final String signatureId = getSignatureId(signatureIdPrefix);
        final String signatureValueId = getSignatureValueId(signatureIdPrefix);

        final XMLSignatureFactory fac = getXMLSignatureFactory();
        final CanonicalizationMethod cm = fac.newCanonicalizationMethod(
    		CanonicalizationMethod.INCLUSIVE,
            (C14NMethodParameterSpec) null
        );

        final List<Reference> documentReferences = getReferences(referencesIdList);
        final String keyInfoId = getKeyInfoId(signatureIdPrefix);
        documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$

        final SignatureMethod sm = fac.newSignatureMethod(signatureMethod, null);
        final SignedInfo si = fac.newSignedInfo(cm, sm, documentReferences);

        this.signature = fac.newXMLSignature(si, newKeyInfo(certificate, keyInfoId),
                getXMLObjects(), signatureId, signatureValueId);

        this.signContext = new DOMSignContext(privateKey, this.baseElement);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());
        if (needCustomUriDereferencer()) {
        	this.signContext.setURIDereferencer(new CustomDOMUriDereferencer());
        }

        this.signature.sign(this.signContext);
    }

    /** Obtiene una instancia de la clase.
     * @param xades Datos de la firma XAdES-BES
     * @return Instancia de la clase
     * @throws GeneralSecurityException Cuando se especifica una XAdES con un algoritmo de huella digital no soportado. */
    public static AOXMLAdvancedSignature newInstance(final XAdES_BES xades) throws GeneralSecurityException {
        final AOXMLAdvancedSignature result = new AOXMLAdvancedSignature(xades);
        result.setDigestMethod(xades.getDigestMethod());
        result.setXadesNamespace(xades.getXadesNamespace());
        return result;
    }

    /** Indica si es necesario instalar el derreferenciador XML a medida.
     * @return {@code true} si se tiene que instalar el derreferenciador, {@code false}
     *         en caso contrario. */
    public static boolean needCustomUriDereferencer() {
    	try {
	    	final Class<?> apacheNodeSetDataClass = Class.forName("org.jcp.xml.dsig.internal.dom.ApacheNodeSetData"); //$NON-NLS-1$
	    	final Class<?> xmlSignatureInputClass = Class.forName("com.sun.org.apache.xml.internal.security.signature.XMLSignatureInput"); //$NON-NLS-1$

	    	// El constructor ApacheNodeSetData(XMLSignatureInput) esta en XMLSec y no en la JRE. Si
	    	// se encuentra este metodo entonces XMLSec esta instalado y no es necesario utilizar
	    	// un derreferenciador a medida
	    	apacheNodeSetDataClass.getConstructor(xmlSignatureInputClass);
    	}
    	catch (final Exception e) {
    		return false;
    	}
    	return true;
    }
}
