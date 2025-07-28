/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
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
import javax.xml.crypto.dsig.spec.TransformParameterSpec;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.AOCancelledSMOperationException;
import es.gob.afirma.core.keystores.AuthenticationException;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.signers.xml.Utils;
import es.uji.crypto.xades.jxades.security.xml.WrappedKeyStorePlace;
import es.uji.crypto.xades.jxades.security.xml.XmlWrappedKeyInfo;
import es.uji.crypto.xades.jxades.security.xml.XAdES.SignaturePolicyIdentifierImpl;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XAdES_EPES;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XMLAdvancedSignature;
import es.uji.crypto.xades.jxades.security.xml.XAdES.XadesWithBasicAttributes;

final class OOXMLAdvancedSignature extends XMLAdvancedSignature {

	private final byte[] ooXmlDocument;

    private OOXMLAdvancedSignature(final XAdES_EPES xades, final byte[] ooXmlPackage) {
		super(xades);
		this.ooXmlDocument = ooXmlPackage.clone();
	}

    static OOXMLAdvancedSignature newInstance(final XAdES_EPES xades, final byte[] ooXmlPackage) throws GeneralSecurityException {
    	xades.setSignaturePolicyIdentifier(new SignaturePolicyIdentifierImpl(true));
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
                                                     XMLSignatureException,
                                                     AOException {

      final List<?> referencesIdList = new ArrayList<>(refsIdList);

      if (WrappedKeyStorePlace.SIGNING_CERTIFICATE_PROPERTY.equals(getWrappedKeyStorePlace()) && certChain != null && certChain.length > 0 && this.xades instanceof XadesWithBasicAttributes) {
		  ((XadesWithBasicAttributes) this.xades).setSigningCertificate(certChain[0]);
	  }

      addXMLObject(
  		marshalXMLSignature(
			this.xadesNamespace,
			this.signedPropertiesTypeUrl,
			signatureIdPrefix,
			referencesIdList,
			Arrays.asList( // En OOXML las SignedProperties se canonicalizan
	    		Utils.getDOMFactory().newTransform(
					"http://www.w3.org/TR/2001/REC-xml-c14n-20010315", //$NON-NLS-1$
					(TransformParameterSpec) null
				)
			)
		)
	  );

      final XMLSignatureFactory fac = getXMLSignatureFactory();

      final List<Reference> documentReferences = getReferences(referencesIdList);
      final String keyInfoId = getKeyInfoId(signatureIdPrefix);
      documentReferences.add(fac.newReference("#" + keyInfoId, getDigestMethod())); //$NON-NLS-1$

      this.signature = fac.newXMLSignature(
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
		  this.baseElement != null ? this.baseElement : getBaseDocument()
	  );
      this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
      this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());
      this.signContext.setURIDereferencer(new OOXMLURIDereferencer(this.ooXmlDocument));

      try {
    	  this.signature.sign(this.signContext);
      }
      catch (final XMLSignatureException e) {
    	  final Throwable cause = e.getCause() != null ? e.getCause() : null;
    	  if (cause != null) {
    		  String causeName = cause.getClass().getName();
    		  // Si JMulticard informa de un problema de autenticacion durante la firma
    		  if ("es.gob.jmulticard.jse.provider.SignatureAuthException".equals(causeName)) { //$NON-NLS-1$
    			  causeName = cause.getCause() != null ? cause.getCause().getClass().getName() : null;
    			  // Si la tarjeta esta bloqueada
    			  if ("es.gob.jmulticard.card.AuthenticationModeLockedException".equals(causeName)) { //$NON-NLS-1$
    				  throw new LockedKeyStoreException("El almacen de claves esta bloqueado", e); //$NON-NLS-1$
    			  }
    			  // Si se ha insertado un PIN incorrecto
    			  if ("es.gob.jmulticard.card.BadPinException".equals(causeName)) { //$NON-NLS-1$
    				  throw new PinException("La contrasena del almacen o certificado es incorrecta", e); //$NON-NLS-1$
    			  }
    			  throw new AuthenticationException("Ocurrio un error de autenticacion al utilizar la clave de firma", cause); //$NON-NLS-1$
    		  }
    	  }
    	  throw e;
      }
      catch (final Exception e) {
    	  if ("es.gob.jmulticard.CancelledOperationException".equals(e.getClass().getName())) { //$NON-NLS-1$
    		  throw new AOCancelledSMOperationException("Cancelacion del dialogo de JMulticard"); //$NON-NLS-1$
    	  }
    	  throw e;
      }
    }

    private KeyInfo newKeyInfo(final X509Certificate[] certChain, final String keyInfoId) throws KeyException {
    	final KeyInfoFactory keyInfoFactory = getXMLSignatureFactory().getKeyInfoFactory();
    	final List<X509Certificate> x509DataList = new ArrayList<>();
    	if (!XmlWrappedKeyInfo.PUBLIC_KEY.equals(getXmlWrappedKeyInfo()) && certChain != null) {
    		for (final X509Certificate cert : certChain) {
    			x509DataList.add(cert);
    		}
    	}
    	final List<XMLStructure> newList = new ArrayList<>();
    	if (certChain != null) {
	    	newList.add(keyInfoFactory.newKeyValue(certChain[0].getPublicKey()));
	    	newList.add(keyInfoFactory.newX509Data(x509DataList));
    	}
    	return keyInfoFactory.newKeyInfo(newList, keyInfoId);
    }

}
