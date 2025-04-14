/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.PublicKey;
import java.security.Security;
import java.security.Signature;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.URIDereferencer;
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

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.AOCancelledSMOperationException;
import es.gob.afirma.core.keystores.AuthenticationException;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.Pkcs1Utils;
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

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String SYSTEM_PROPERTY_SIGNATURE_PROVIDER = "org.jcp.xml.dsig.internal.dom.SignatureProvider"; //$NON-NLS-1$

	private static final String PROVIDER_NAME_SUNMSCAPI = "SunMSCAPI"; //$NON-NLS-1$

    private URIDereferencer uriDereferencer = null;

    private String canonicalizationMethod = CanonicalizationMethod.INCLUSIVE;
    private Element styleElement = null;
    private String styleType = "text/xsl"; //$NON-NLS-1$
    private String styleEncoding = null;
    private String styleId = null;

    private AOXMLAdvancedSignature(final XAdESBase xades) {
        super(xades);
    }

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

        //FIXME: Cuando Afirma valide el elemento ECKeyValue generado por Apache Santuario
        // se deberia permitir agregar este elemento
        final PublicKey publicKey = certificates.get(0).getPublicKey();
        final boolean ecKey = "EC".equalsIgnoreCase(publicKey.getAlgorithm()); //$NON-NLS-1$

        final List<XMLStructure> newList = new ArrayList<>();
        newList.add(keyInfoFactory.newX509Data(x509DataList));
        if (addKeyValue && !ecKey) {
	        newList.add(keyInfoFactory.newKeyValue(publicKey));
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
              final boolean verify) throws MarshalException,
                                                        GeneralSecurityException,
                                                        XMLSignatureException,
                                                        AOException {

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

        Key normalizedPk;
        if (privateKey instanceof BCECPrivateKey) {
        	normalizedPk = KeyUtil.ecBc2Jce((BCECPrivateKey)privateKey);
        }
        else {
			normalizedPk = privateKey;
        }

        this.signContext = new DOMSignContext(normalizedPk, this.baseElement);
        this.signContext.putNamespacePrefix(XMLSignature.XMLNS, this.xades.getXmlSignaturePrefix());
        this.signContext.putNamespacePrefix(this.xadesNamespace, this.xades.getXadesPrefix());
//        this.signContext.putNamespacePrefix("http://www.w3.org/2009/xmldsig11#", "ds11");

        // En caso de ser un tipo de clave del almacen de Windows, forzamos su proveedor. Esto es
        // necesario cuando se usan certificados de curva eliptica con Java 9 y superior porque
        // Apache Santuario usara el proveedor SunEC para la firma, pero este no entiende las
        // claves del almacen de Windows. La diferencia con Java 8 es que en versiones superiores
        // Apache Santuario usa el algoritmo de firma "LOQUESEAwithECDSAinP1363Format" (que se
        // agrego en Java 9) y esto debe llevar a una eleccion distinta de proveedor.

        // TODO: Investigar por que viene prefijado el proveedor SunEC, ya que no deberia seleccionarse hasta indicar el tipo de clave.

        // Si la clave de ha obtenido con el almacen de Windows, forzamos que se use el mismo proveedor para la firma
    	if (KeyUtil.isMsCapiPrivateKey(normalizedPk)) {
    		final Provider prov = Security.getProvider(PROVIDER_NAME_SUNMSCAPI);
    		this.signContext.setProperty(SYSTEM_PROPERTY_SIGNATURE_PROVIDER, prov);
    	}

        // Instalamos un nuevo dereferenciador. Si se nos indica, usamos el indicado. Si no, usaremos
        // uno que funciona tal como el por defecto, pero realiza comprobaciones adicionales si falla

        try {
        	if (this.uriDereferencer != null) {
        		this.signContext.setURIDereferencer(this.uriDereferencer);
        	}
        	else {
        		this.signContext.setURIDereferencer(new CustomUriDereferencer());
        	}
        }
        catch (final Exception e) {
        	LOGGER.log(
        			Level.WARNING,
        			"No se ha podido instalar un dereferenciador a medida para la firma, es posible que fallen las firmas de nodos concretos: " + e, //$NON-NLS-1$
        			e
        			);
        }

        // Generamos la firma
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

		// Siguiendo la recomendacion de la ETSI TS 119 102-1, verificamos que el dispositivo de
        // creacion de firma realmente ha generado el PKCS#1 usando la clave privada del
        // certificado proporcionado. Para ello, desciframos el PKCS#1 con la clave publica de
        // ese certificado
        if (certificates != null && !certificates.isEmpty() && verify) {
        	verifySignature(certificates.get(0).getPublicKey(), signatureMethod);
        }
    }

    /**
     * Establece un derreferenciador de URI a medida para poder resolver el contenido de los datos
     * en firma con referencias especiales.
     * @param uriDereferencer Derreferenciador a medida.
     */
    public void setUriDereferencer(final URIDereferencer uriDereferencer) {
    	this.uriDereferencer = uriDereferencer;
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
     * Verifica que el PKCS#1 de la firma se haya generado con la clave privada correspondiente a una clave
     * p&uacute;blica dada.
     * @param publicKey Clave p&uacute;blica con la que validar la firma.
     * @param signatureMethod URI del algoritmo de firma.
     * @throws XMLSignatureException Cuando no se proporciona un par&aacute;metro v&aacute;lido o
     * el PKCS#1 se gener&oacute; con una clave privada distinta a la esperada.
     */
    private void verifySignature(final PublicKey publicKey,
    		final String signatureMethod) throws XMLSignatureException {

    	final byte[] signatureValue = this.signature.getSignatureValue().getValue();

    	final String signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(signatureMethod, publicKey.getAlgorithm());
    	byte[] normalizedSignatureValue = signatureValue;

    	boolean valid;
    	try {
    		Signature sig;

    		// Cuando se usa Java 9 o superior, Apache Santuario utiliza el formato normalizado
    		// de las firmas ECDSA y DSA, lo que hace que no haya problemas con la validacion.
			// En Java 8, sin embargo, tendremos que codificar correctamente la firma antes de
			// hacer la validacion
    		if (signatureAlgorithm.endsWith("withECDSA") //$NON-NLS-1$
    				|| signatureAlgorithm.endsWith("withDSA")) { //$NON-NLS-1$
    			try {
    				sig = Signature.getInstance(signatureAlgorithm + "inP1363Format"); //$NON-NLS-1$
    			}
    			catch (final Exception e) {
    				// Si el proveedor no soporta el formato P1363 (caso de Java 8 y anteriores), tendremos
    				// que decodificar la firma para obtener el resultado esperado
    				sig  = Signature.getInstance(signatureAlgorithm);
    				normalizedSignatureValue = Pkcs1Utils.encodeSignature(signatureValue);
    			}
    		}
    		else {
    			sig = Signature.getInstance(signatureAlgorithm);
    		}

    		sig.initVerify(publicKey);

        	byte[] signedInfoEncoded;
    		try (InputStream is = this.signature.getSignedInfo().getCanonicalizedData()) {
    			signedInfoEncoded = AOUtil.getDataFromInputStream(is);
    		}
    		sig.update(signedInfoEncoded);

    		valid = sig.verify(normalizedSignatureValue);
    	}
    	catch (final Exception e) {
    		throw new XMLSignatureException("Error al verificar el PKCS#1 de la firma", e); //$NON-NLS-1$
    	}

    	if (!valid) {
    		throw new XMLSignatureException("El PKCS#1 de la firma no se ha generado con la clave publica indicada"); //$NON-NLS-1$
    	}
    }
}
