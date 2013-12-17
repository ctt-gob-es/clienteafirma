package es.gob.afirma.signers.ooxml;

import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.Reference;
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

final class OOXMLAdvancedSignature extends XMLAdvancedSignature {

	private final byte[] ooXmlDocument;

    private OOXMLAdvancedSignature(final XAdES_BES xades, final byte[] ooXmlPackage) {
		super(xades);
		this.ooXmlDocument = ooXmlPackage.clone();
	}

    static OOXMLAdvancedSignature newInstance(final XAdES_BES xades, final byte[] ooXmlPackage) throws GeneralSecurityException {
        final OOXMLAdvancedSignature result = new OOXMLAdvancedSignature(xades, ooXmlPackage);
        result.setDigestMethod(xades.getDigestMethod());
        result.setXadesNamespace(xades.getXadesNamespace());
        return result;
    }

    void sign(final X509Certificate[] certChain,
              final PrivateKey privateKey,
              final String signatureMethod,
              final List<?> refsIdList,
              final String signatureIdPrefix) throws MarshalException,
                                                     GeneralSecurityException,
                                                     XMLSignatureException {

      final List<?> referencesIdList = new ArrayList<Object>(refsIdList);

      if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace()) && certChain != null && certChain.length > 0) {
          this.xades.setSigningCertificate(certChain[0]);
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

      final List<Reference> documentReferences = getReferences(referencesIdList);
      final String keyInfoId = getKeyInfoId(signatureIdPrefix);
      documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$

      this.signature =fac.newXMLSignature(
      		fac.newSignedInfo(
  				fac.newCanonicalizationMethod(
  						CanonicalizationMethod.INCLUSIVE,
						(C14NMethodParameterSpec) null
					),
                  fac.newSignatureMethod(signatureMethod, null),
                  documentReferences
              ),
              newKeyInfo(certChain, keyInfoId),
              getXMLObjects(),
              getSignatureId(signatureIdPrefix),
              getSignatureValueId(signatureIdPrefix)
      );

      this.signContext = new DOMSignContext(
		  privateKey,
		  this.baseElement != null ? this.baseElement : this.getBaseDocument()
	  );
      this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
      this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());
      this.signContext.setURIDereferencer(new OOXMLURIDereferencer(this.ooXmlDocument));

      this.signature.sign(this.signContext);
    }

    private KeyInfo newKeyInfo(final X509Certificate[] certChain, final String keyInfoId) throws KeyException {
    	final KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
    	final List<X509Certificate> x509DataList = new ArrayList<X509Certificate>();
    	if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo())) {
    		for (final X509Certificate cert : certChain) {
    			x509DataList.add(cert);
    		}
    	}
    	final List<XMLStructure> newList = new ArrayList<XMLStructure>();
    	newList.add(keyInfoFactory.newKeyValue(certChain[0].getPublicKey()));
    	newList.add(keyInfoFactory.newX509Data(x509DataList));
    	return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

}
