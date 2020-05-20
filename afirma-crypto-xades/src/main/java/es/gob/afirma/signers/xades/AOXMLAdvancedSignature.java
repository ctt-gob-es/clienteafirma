/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;

import org.spongycastle.jcajce.provider.asymmetric.ec.BCECPrivateKey;
import org.w3c.dom.Element;

import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;
import es.gob.afirma.signers.xml.style.XmlStyle;
import es.uji.crypto.xades.jxades.security.xml.XmlWrappedKeyInfo;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdESBase;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XMLAdvancedSignature;

/** Derivado de <code>es.uji.crypto.xades.security.xml.XAdES.XMLAdvancedSignature</code> con los
 * siguientes cambios:
 * <ul>
 *  <li>En el <i>KeyInfo</i> no se a&ntilde;aden los elementos <i>SubjectX500Principal</i> y <i>X509IssuerSerial</i>.</li>
 *  <li>Se puede establecer el algoritmo de firma.</li>
 *  <li>Se puede establecer el algoritmo de canonicalizaci&oacute;n para la firma.</li>
 *  <li>Se puede establecer la URL del espacio de nombres de XAdES.</li>
 *  <li>Se puede a&ntilde;adir una hoja de estilo en modo <i>enveloping</i> dentro de la firma.</li>
 * </ul> */
final class AOXMLAdvancedSignature extends XMLAdvancedSignature {

	static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

    private AOXMLAdvancedSignature(final XAdESBase xades) {
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
     * @param xmlStyle Elemento de estilo XML
     * @param sId Identificador de la hoja de estilo (si se proporciona un nulo
     *            no se a&ntilde;ade la hoja de estilo) */
    void addStyleSheetEnvelopingOntoSignature(final XmlStyle xmlStyle,
    		                                  final String sId) {
        this.styleElement = xmlStyle.getStyleElement();
        if (xmlStyle.getStyleType() != null) {
            this.styleType = xmlStyle.getStyleType();
        }
        this.styleId = sId;
        this.styleEncoding = xmlStyle.getStyleEncoding();
    }

    /** Establece el algoritmo de canonicalizaci&oacute;n.
     * @param canMethod
     *        URL del algoritmo de canonicalizaci&oacute;n. Debe estar
     *        soportado en XMLDSig 1.0 o 1.1 */
    void setCanonicalizationMethod(final String canMethod) {
        if (canMethod != null) {
            this.canonicalizationMethod = canMethod;
        }
    }

    private KeyInfo newKeyInfo(final List<Certificate> certs,
    		                   final String keyInfoId,
    		                   final boolean addKeyValue,
    		                   final boolean addKeyName,
    		                   final boolean addIssuerSerial) throws KeyException {

    	final List<Certificate> certificates = EscapeHelper.getEscapedCertificates(certs);
        final KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
        final List<Certificate> x509DataList = new ArrayList<>();
        if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo())) {
            for (final Certificate cert : certificates) {
                x509DataList.add(cert);
            }
        }
        final List<XMLStructure> newList = new ArrayList<>();
        newList.add(keyInfoFactory.newX509Data(x509DataList));
        if (addKeyValue) {
	        newList.add(keyInfoFactory.newKeyValue(
	    		certificates.get(0).getPublicKey())
			);
        }
        if (addKeyName) {
	        newList.add(
	    		keyInfoFactory.newKeyName(
    				EscapeHelper.escapeLdapName(
						((X509Certificate) certificates.get(0)).getSubjectX500Principal().toString()
					)
				)
			);
        }
        if (addIssuerSerial) {
        	newList.add(
	    		keyInfoFactory.newX509IssuerSerial(
    				EscapeHelper.escapeLdapName(
						((X509Certificate) certificates.get(0)).getIssuerX500Principal().toString()
					),
					((X509Certificate) certificates.get(0)).getSerialNumber()
				)
			);
        }
        return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

    void sign(final List<Certificate> certificates,
              final PrivateKey privateKey,
              final String signatureMethod,
              final List<?> refsIdList,
              final String signatureIdPrefix,
              final boolean addKeyInfoKeyValue,
              final boolean addKeyInfoKeyName,
              final boolean addKeyInfoX509IssuerSerial,
              final boolean keepKeyInfoUnsigned,
              final boolean verifyPkcs1) throws MarshalException,
                                                        GeneralSecurityException,
                                                        XMLSignatureException {

        final List<?> referencesIdList = new ArrayList<>(refsIdList);

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
        if (!keepKeyInfoUnsigned) {
        	documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$
        }

        this.signature = fac.newXMLSignature(
    		fac.newSignedInfo(
				fac.newCanonicalizationMethod(
					this.canonicalizationMethod,
					(C14NMethodParameterSpec) null
				),
                fac.newSignatureMethod(signatureMethod, null),
                documentReferences
            ),
            newKeyInfo(
        		certificates,
        		keyInfoId,
        		addKeyInfoKeyValue,
        		addKeyInfoKeyName,
        		addKeyInfoX509IssuerSerial
    		),
            getXMLObjects(),
            getSignatureId(signatureIdPrefix),
            getSignatureValueId(signatureIdPrefix)
        );

        this.signContext = new DOMSignContext(
        	// Si llega una clave ECDSA de BC la convertimos a una EC de JSE para evitar problemas
    		privateKey instanceof BCECPrivateKey ?
				KeyUtil.ecBc2Jce((BCECPrivateKey)privateKey) :
					privateKey,
    		this.baseElement
		);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());

        try {
        	// Instalamos un dereferenciador nuevo que solo actua cuando falla el por defecto
        	this.signContext.setURIDereferencer(
    			new CustomUriDereferencer()
			);
        }
        catch (final Exception e) {
        	LOGGER.log(
    			Level.WARNING,
    			"No se ha podido instalar un dereferenciador a medida, es posible que fallen las firmas de nodos concretos: " + e, //$NON-NLS-1$
    			e
			);
        }

        // Generamos la firma
        this.signature.sign(this.signContext);

		// Siguiendo la recomendacion de la ETSI TS 119 102-1, verificamos que el dispositivo de
        // creacion de firma realmente ha generado el PKCS#1 usando la clave privada del
        // certificado proporcionado. Para ello, desciframos el PKCS#1 con la clave publica de
        // ese certificado
        if (certificates != null && certificates.size() > 0 && verifyPkcs1) {
        	verifyPkcs1(this.signature.getSignatureValue().getValue(),
        			certificates.get(0).getPublicKey(),
        			signatureMethod);
        }
    }

    /** Obtiene una instancia de la clase.
     * @param xades Datos de la firma XAdES
     * @return Instancia de la clase
     * @throws GeneralSecurityException Cuando se especifica una XAdES con un algoritmo de huella digital no soportado. */
    public static AOXMLAdvancedSignature newInstance(final XAdESBase xades) throws GeneralSecurityException {
        final AOXMLAdvancedSignature result = new AOXMLAdvancedSignature(xades);
        result.setDigestMethod(xades.getDigestMethod());
        result.setXadesNamespace(xades.getXadesNamespace());
        return result;
    }

    /**
     * Verifica que un PKCS#1 se haya generado con la clave privada correspondiente a una clave
     * p&uacute;blica dada.
     * @param signatureValue PKCS#1 de la firma.
     * @param publicKey Clave p&uacute;blica con la que validar la firma.
     * @param signatureMethod URI del algoritmo de firma.
     * @throws XMLSignatureException Cuando no se proporciona un par&aacute;metro v&aacute;lido o
     * el PKCS#1 se gener&oacute; con una clave privada distinta a la esperada.
     */
    private static void verifyPkcs1(final byte[] signatureValue, final PublicKey publicKey,
    		final String signatureMethod) throws XMLSignatureException {
    	try {
    		final int signAlgoPos = signatureMethod.lastIndexOf('#') + 1;
    		final int signAlgoLimit = signatureMethod.indexOf('-', signAlgoPos);
    		final String signAlgo = signatureMethod.substring(signAlgoPos, signAlgoLimit);

    		//TODO: Probar y soportar algoritmos de cifrado de curva eliptica
    		if (!signAlgo.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
    			LOGGER.warning("No se soporta la validacion del PKCS#1 con el algoritmo de cifrado asociado al algoritmo de firma seleccionado"); //$NON-NLS-1$
    			return;
    		}

    		final Cipher cipher = Cipher.getInstance(signAlgo.toUpperCase());
    		cipher.init(Cipher.DECRYPT_MODE, publicKey);
    		cipher.doFinal(signatureValue);
    	}
    	catch (final Exception e) {
    		throw new XMLSignatureException("El PKCS#1 de la firma no se ha generado con el certificado indicado", e); //$NON-NLS-1$
    	}
    }
}
