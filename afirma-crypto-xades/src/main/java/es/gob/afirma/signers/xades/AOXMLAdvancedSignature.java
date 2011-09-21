/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.xades;

import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.TransformException;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMSignContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyInfoFactory;
import javax.xml.crypto.dsig.spec.C14NMethodParameterSpec;

import net.java.xades.security.xml.WrappedKeyStorePlace;
import net.java.xades.security.xml.XmlWrappedKeyInfo;
import net.java.xades.security.xml.XAdES.XAdES_BES;
import net.java.xades.security.xml.XAdES.XMLAdvancedSignature;

import org.w3c.dom.Element;

/** Derivado de <code>net.java.xades.security.xml.XAdES.XMLAdvancedSignature</code> con los
 * siguientes cambios:
 * <ul>
 * <li>En el <i>KeyInfo</i> no se a&ntilde;aden los elementos <i>SubjectX500Principal</i> y <i>X509IssuerSerial</i></li>
 * <li>Se puede establecer el algoritmo de firma</li>
 * <li>Se puede establecer el algoritmo de canonicalizaci&oacute;n para la firma</li>
 * <li>Se puede establecer la URL del espacio de nombres de XAdES</li>
 * <li>Se puede a&ntilde;adir una hoja de estilo en modo <i>enveloping</i> dentro de la firma
 * </ul> */
@SuppressWarnings("restriction")
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
     * @param s
     *        XML de la hoja de estilo (si se proporciona un nulo no se
     *        a&ntilde;ade la hoja de estilo)
     * @param sType
     *        Tipo (MimeType) de la hoja de estilo (puede ser nulo)
     * @param sEncoding
     *        Codificaci&oacute;n de la hoja de estilo (puede ser nula)
     * @param sId
     *        Identificador de la hoja de estilo (si se proporciona un nulo
     *        no se a&ntilde;ade la hoja de estilo) */
    void addStyleSheetEnvelopingOntoSignature(final Element s, final String sType, final String sEncoding, final String sId) {
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

    @Override
    protected KeyInfo newKeyInfo(final X509Certificate certificate, final String keyInfoId) throws KeyException {
        final KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
        final List<X509Certificate> x509DataList = new ArrayList<X509Certificate>();
        if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo())) {
            x509DataList.add(certificate);
        }
        final List<XMLStructure> newList = new ArrayList<XMLStructure>();
        newList.add(keyInfoFactory.newKeyValue(certificate.getPublicKey()));
        newList.add(keyInfoFactory.newX509Data(x509DataList));
        return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public void sign(final X509Certificate certificate,
                     final PrivateKey privateKey,
                     final String signatureMethod,
                     final List refsIdList,
                     final String signatureIdPrefix,
                     final String tsaURL) throws MarshalException, XMLSignatureException, GeneralSecurityException, TransformException {

        final List<?> referencesIdList = new ArrayList(refsIdList);

        if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace())) {
            this.xades.setSigningCertificate(certificate);
        }

        addXMLObject(marshalXMLSignature(this.xadesNamespace, signatureIdPrefix, referencesIdList, tsaURL));

        final XMLSignatureFactory fac = getXMLSignatureFactory();

        if (this.styleElement != null && this.styleId != null) {
            addXMLObject(fac.newXMLObject(Collections.singletonList(new DOMStructure(this.styleElement)), this.styleId, this.styleType, this.styleEncoding));
        }

        final List<Reference> documentReferences = getReferences(referencesIdList);
        final String keyInfoId = getKeyInfoId(signatureIdPrefix);
        documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$

        this.signature =
                fac.newXMLSignature(fac.newSignedInfo(fac.newCanonicalizationMethod(this.canonicalizationMethod, (C14NMethodParameterSpec) null),
                                                      fac.newSignatureMethod(signatureMethod, null),
                                                      documentReferences),
                                    newKeyInfo(certificate, keyInfoId),
                                    getXMLObjects(),
                                    getSignatureId(signatureIdPrefix),
                                    getSignatureValueId(signatureIdPrefix));

        this.signContext = new DOMSignContext(privateKey, this.baseElement);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());

        this.signature.sign(this.signContext);
    }

    public static AOXMLAdvancedSignature newInstance(final XAdES_BES xades) throws GeneralSecurityException {
        final AOXMLAdvancedSignature result = new AOXMLAdvancedSignature(xades);
        result.setDigestMethod(xades.getDigestMethod());
        result.setXadesNamespace(xades.getXadesNamespace());
        return result;
    }

}
