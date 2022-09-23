/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Locale;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.DERTaggedObject;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.DERUTF8String;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.esf.SignerLocation;
import org.spongycastle.asn1.ess.ContentHints;
import org.spongycastle.asn1.ess.ESSCertID;
import org.spongycastle.asn1.ess.ESSCertIDv2;
import org.spongycastle.asn1.ess.SigningCertificate;
import org.spongycastle.asn1.ess.SigningCertificateV2;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.CertificatePolicies;
import org.spongycastle.asn1.x509.DigestInfo;
import org.spongycastle.asn1.x509.GeneralName;
import org.spongycastle.asn1.x509.GeneralNames;
import org.spongycastle.asn1.x509.IssuerSerial;
import org.spongycastle.asn1.x509.PolicyInformation;
import org.spongycastle.asn1.x509.X509AttributeIdentifiers;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Utilidades varias relacionadas con firmas electr&oacute;nicas CAdES.
 * Se declara como clase p&uacute;blica para permitir su uso en el m&oacute;dulo de multifirmas CAdES.
 * Las principales estructuras ASN.1 implementadas son:
 * <pre>
 * id-aa-signingCertificateV2 OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-aa(2) 47
 * }
 *
 * SigningCertificateV2 ::=  SEQUENCE {
 *      certs        SEQUENCE OF ESSCertIDv2,
 *      policies     SEQUENCE OF PolicyInformation OPTIONAL
 * }
 *
 * ESSCertIDv2 ::= SEQUENCE {
 *   hashAlgorithm AlgorithmIdentifier
 *   DEFAULT {
 *     algorithm id-sha256
 *   },
 *   certHash Hash,
 *   issuerSerial IssuerSerial OPTIONAL
 * }
 * Hash ::= OCTET STRING+
 *
 * IssuerSerial ::= SEQUENCE {
 *   issuer GeneralNames,
 *   serialNumber CertificateSerialNumber
 *   issuerUID UniqueIdentifier OPTIONAL
 * }
 *
 * PolicyInformation ::= SEQUENCE {
 *    policyIdentifier CertPolicyId,
 *    policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo OPTIONAL
 * }
 *
 * CertPolicyId ::= OBJECT IDENTIFIER
 * PolicyQualifierInfo ::= SEQUENCE {
 *   policyQualifierId PolicyQualifierId,
 *   qualifier ANY DEFINED BY policyQualifierId
 * }
 *
 * SigningCertificateV2 ::= SEQUENCE {
 *   certs SEQUENCE OF ESSCertIDv2,
 *   policies SEQUENCE OF PolicyInformation OPTIONAL
 * }
 *
 * id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-aa(2) 12
 * }
 *
 * SigningCertificate ::=  SEQUENCE {
 *      certs        SEQUENCE OF ESSCertID,
 *      policies     SEQUENCE OF PolicyInformation OPTIONAL
 * }
 *
 * IssuerSerial ::= SEQUENCE {
 *   issuer GeneralNames,
 *   serialNumber CertificateSerialNumber
 * }
 *
 * ESSCertID ::= SEQUENCE {
 *   certHash Hash,
 *   issuerSerial IssuerSerial OPTIONAL
 * }
 * Hash ::= OCTET STRING -- SHA1 hash of entire certificate
 *
 * PolicyInformation ::= SEQUENCE {
 *   policyIdentifier CertPolicyId,
 *   policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo OPTIONAL
 * }
 *
 * CertPolicyId ::= OBJECT IDENTIFIER
 *
 * PolicyQualifierInfo ::= SEQUENCE {
 *   policyQualifierId PolicyQualifierId,
 *   qualifier ANY DEFINED BY policyQualifierId
 * }
 *
 * SigningCertificateV2 ::= SEQUENCE {
 *    certs SEQUENCE OF ESSCertIDv2,
 *    policies SEQUENCE OF PolicyInformation OPTIONAL
 * }
 *
 * id-aa-signingCertificate OBJECT IDENTIFIER ::= {
 *    iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9) smime(16) id-aa(2) 12
 * }
 *
 * SigPolicyId ::= OBJECT IDENTIFIER (Politica de firma)
 *
 * OtherHashAlgAndValue ::= SEQUENCE {
 *     hashAlgorithm    AlgorithmIdentifier,
 *     hashValue        OCTET STRING
 * }
 *
 * AOSigPolicyQualifierInfo ::= SEQUENCE {
 *       SigPolicyQualifierId  SigPolicyQualifierId,
 *       SigQualifier          ANY DEFINED BY policyQualifierId
 * }
 *
 * SignaturePolicyId ::= SEQUENCE {
 *    sigPolicyId           SigPolicyId,
 *    sigPolicyHash         SigPolicyHash,
 *    sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF AOSigPolicyQualifierInfo OPTIONAL
 * }
 *
 * </pre>
 *
 *  */
public final class CAdESUtils {

	private CAdESUtils() {
        // No permitimos la instanciacion
    }

    /** Genera una estructura <i>SigningCertificateV2</i> seg&uacute;n RFC 5035.
     * Es importante rese&ntilde;ar que la estructura <code>ESSCertIDv2</code> tiene la siguiente estructura:
     * <pre>
     *  ESSCertIDv2 ::=  SEQUENCE {
     *   hashAlgorithm AlgorithmIdentifier DEFAULT {algorithm id-sha256},
     *   certHash Hash,
     *   issuerSerial IssuerSerial OPTIONAL
     *  }
     * </pre>
     * En <code>DER</code> un campo marcado como <code>DEFAULT</code> no debe codificarse, debe dejarse ausente:
     * <a href="http://www.oss.com/asn1/resources/asn1-faq.html#default">http://www.oss.com/asn1/resources/asn1-faq.html#default</a>.<br>
     * Dado que el <code>SignedData</code> debe codificarse en <code>DER</code>, si el algoritmo de huella es SHA-256, el campo de OID de
     * algoritmo debe estar ausente.<br>
     * Citando la especificaci&oacute;n "ETSI TS 102 778-3"  (PDF Advanced Electronic Signature Profiles;Part 3: PAdES Enhanced - PAdES-BES
     * and PAdES-EPES Profiles) en su apartado "4.2 b"):<br>
     * <i>
     *  b) A <b>DER-encoded</b> SignedData object as specified in CMS (RFC 3852 [4]) shall be included as the PDF signature in the entry with the
     *  key Content of the signature dictionary as described in ISO 32000-1 [1], clause 12.8.1.
     * </i>
     * @param cert Certificado del firmante.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar.
     * @param includePolicyOnSigningCertificate Si se establece a <code>false</code>, omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>true</code> se incluye siempre que el certificado
     *                                               la declare.
     * @param includeIssuerSerial Si se establece a {@code false}, se omite la inclusi&oacute;n del n&uacute;mero
     *                                               de serie del issuer en el atributo <i>SigningCertificate</i>;
     *                                               si se establece a {@code true}, se incluye.
     * @return Estructura <i>SigningCertificateV2</i> seg&uacute;n RFC 5035.
     * @throws CertificateEncodingException Si el certificado proporcionado no es v&aacute;lido.
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella indicado. */
    private static Attribute getSigningCertificateV2(final X509Certificate cert,
    		                                         final String digestAlgorithmName,
                                                     final boolean includePolicyOnSigningCertificate,
                                                     final boolean includeIssuerSerial) throws CertificateEncodingException,
    		                                                                                                      NoSuchAlgorithmException {
    	// ALGORITMO DE HUELLA DIGITAL
    	final String hashOid = AOAlgorithmID.getOID(digestAlgorithmName);
    	// Si es SHA-256 ponemos el OID a null para que no incluya el campo (y tome su
    	// valor por defecto).
        final AlgorithmIdentifier digestAlgorithmOID =
    		AOAlgorithmID.OID_SHA256.equals(hashOid) ?
				null :
					SigUtils.makeAlgId(hashOid);

        // INICIO SINGING CERTIFICATE-V2

        final GeneralNames gns = new GeneralNames(
    		new GeneralName(X500Name.getInstance(cert.getIssuerX500Principal().getEncoded()))
		);

        IssuerSerial issuerSerial = null;
        if (includeIssuerSerial) {
        	issuerSerial = new IssuerSerial(gns, cert.getSerialNumber());
        }

        final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
        final ESSCertIDv2[] essCertIDv2 = {
            new ESSCertIDv2(digestAlgorithmOID, certHash, issuerSerial)
        };

        final SigningCertificateV2 scv2;
        final PolicyInformation[] polInfo = includePolicyOnSigningCertificate ? getPolicyInformation(cert) : null;

        if (polInfo != null) {
            scv2 = new SigningCertificateV2(essCertIDv2, polInfo); // con politica
        }
        else {
            scv2 = new SigningCertificateV2(essCertIDv2); // Sin politica
        }

        return new Attribute(
			PKCSObjectIdentifiers.id_aa_signingCertificateV2,
			new DERSet(scv2)
		);

    }

    /** Genera una estructura <i>SigningCertificateV2</i> seg&uacute;n RFC 5035.
     * @param cert Certificado del firmante.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar.
     * @param includePolicyOnSigningCertificate Si se establece a {@code false}, se omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>;
     *                                               si se establece a {@code true}, se incluye siempre que el certificado
     *                                               la declare.
     * @param includeIssuerSerial Si se establece a {@code false}, se omite la inclusi&oacute;n del n&uacute;mero
     *                                               de serie del issuer en el atributo <i>SigningCertificate</i>;
     *                                               si se establece a {@code true}, se incluye.
     * @return Estructura <i>SigningCertificate</i> seg&uacute;n RFC 5035.
     * @throws CertificateEncodingException Si el certificado proporcionado no es v&aacute;lido.
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella indicado. */
    private static Attribute getSigningCertificateV1(final X509Certificate cert,
                                                     final String digestAlgorithmName,
                                                     final boolean includePolicyOnSigningCertificate,
                                                     final boolean includeIssuerSerial) throws CertificateEncodingException,
                                                                                                                  NoSuchAlgorithmException {

        // INICIO SINGNING CERTIFICATE

        final GeneralName gn = new GeneralName(X500Name.getInstance(cert.getIssuerX500Principal().getEncoded()));
        final GeneralNames gns = new GeneralNames(gn);

        IssuerSerial issuerSerial = null;
        if (includeIssuerSerial) {
        	issuerSerial = new IssuerSerial(gns, cert.getSerialNumber());
        }

        final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
        final ESSCertID essCertID = new ESSCertID(certHash, issuerSerial);

        final SigningCertificate scv;
        final PolicyInformation[] polInfo = includePolicyOnSigningCertificate ? getPolicyInformation(cert) : null;

        if (polInfo != null) {

             // HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
             // CASTLE NO TIENE DICHO CONSTRUCTOR.

            final ASN1EncodableVector v = new ASN1EncodableVector();
            v.add(new DERSequence(essCertID));
            v.add(new DERSequence(polInfo));
            scv = SigningCertificate.getInstance(new DERSequence(v)); // con politica
        }
        else {
            scv = new SigningCertificate(essCertID); // Sin politica
        }

        return new Attribute(
			PKCSObjectIdentifiers.id_aa_signingCertificate,
			new DERSet(scv)
		);
    }

    private static DERSet getSigPolicyAttribute(final String digestAlgorithmName,
    		                                final AdESPolicy policy) throws IOException {

        final ASN1ObjectIdentifier doiSigPolicyId = new ASN1ObjectIdentifier(
    		policy.getPolicyIdentifier().toLowerCase(Locale.US).replace("urn:oid:", "") //$NON-NLS-1$ //$NON-NLS-2$
		);

        // Algoritmo para el hash
        final AlgorithmIdentifier hashid;
        // Si tenemos algoritmo de calculo de hash, lo ponemos
        if(policy.getPolicyIdentifierHashAlgorithm()!=null){
            hashid = SigUtils.makeAlgId(
                AOAlgorithmID.getOID(
                    AOSignConstants.getDigestAlgorithmName(
                       policy.getPolicyIdentifierHashAlgorithm()
                   )
               )
           );
        }
        // Si no tenemos, ponemos el algoritmo de firma.
        else {
            hashid = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
        }

        // Huella del documento
        final byte[] hashed;
        if (policy.getPolicyIdentifierHash()!=null) {
            hashed = Base64.decode(policy.getPolicyIdentifierHash());
        }
        else {
            hashed = new byte[]{0};
        }

        final DigestInfo otherHashAlgAndValue = new DigestInfo(hashid, hashed);

        AOSigPolicyQualifierInfo spqInfo = null;
        if(policy.getPolicyQualifier() != null) {
            spqInfo = new AOSigPolicyQualifierInfo(policy.getPolicyQualifier().toString());
        }

        final ASN1EncodableVector v = new ASN1EncodableVector();
        // sigPolicyId
        v.add(doiSigPolicyId);
        // sigPolicyHash
        v.add(otherHashAlgAndValue.toASN1Primitive()); // como sequence
        // sigPolicyQualifiers
        if(spqInfo!=null) {
            v.add(new DERSequence(spqInfo.toASN1Primitive()));
        }

        final DERSequence ds = new DERSequence(v);

        return new DERSet(ds.toASN1Primitive());
    }

    /** Genera un vector con los atributos que deben firmarse dentro de la firma.
     * @param cert Certificado del firmante.
     * @param config Configraci&oacute;n con el detalle de firma.
     * @param isCountersign <code>true</code> si desea generarse el <code>SignerInfo</code> de una
     *                      contrafirma, <code>false</code> en caso contrario.
     * @return Los datos necesarios para generar la firma referente a los datos del usuario.
     * @throws java.security.NoSuchAlgorithmException Cuando se introduce un algoritmo no v&aacute;lido.
     * @throws java.io.IOException Cuando se produce un error de entrada/salida.
     * @throws CertificateEncodingException Error de codificaci&oacute;n en el certificado.
     * @throws IllegalArgumentException Cuando no se ha proporcionado ni los datos ni la huella digital a firmar. */
    public static ASN1EncodableVector generateSignedAttributes(
    		final Certificate cert,
    		final CAdESParameters config,
    		final boolean isCountersign)
    				throws NoSuchAlgorithmException,
    						IOException,
    						CertificateEncodingException {

        // Listado de atributos que se van a firmar
        final ASN1EncodableVector contextSpecific = new ASN1EncodableVector();

        // Algunos atributos basicos quedan listados en: http://tools.ietf.org/html/rfc3852#section-11

        // ContentType (https://tools.ietf.org/html/rfc3852#section-11.1) es obligatorio excepto en
        // contrafirmas (donde no debe aparecer nunca). Debe tener siempre el valor "id-data"
        if (!isCountersign) {
	        contextSpecific.add(
	    		new Attribute(
					CMSAttributes.contentType,
					new DERSet(PKCSObjectIdentifiers.data)
				)
			);
        }

        // MessageDigest (https://tools.ietf.org/html/rfc3852#section-11.2)
        if (config.getDataDigest() != null || config.getContentData() != null) {
        	contextSpecific.add(
       			new Attribute(
     				CMSAttributes.messageDigest,
    				new DERSet(
    					new DEROctetString(
    						config.getDataDigest() != null ?
    							config.getDataDigest() :
    							MessageDigest.getInstance(config.getDigestAlgorithm()).digest(config.getContentData())
    					)
    				)
    			)
    		);
        }
        else {
        	throw new IllegalArgumentException("Ni los datos a firmar ni la huella de los mismos se han configurado o encontrado "); //$NON-NLS-1$
        }

        // Informacion del certificado de firma  (en la forma original o la V2 segun sea necesario)
        if (config.isSigningCertificateV2()) {
            contextSpecific.add(
        		getSigningCertificateV2(
    				(X509Certificate) cert,
    				config.getDigestAlgorithm(),
    				config.isIncludedPolicyOnSigningCertificate(),
    				config.isIncludedIssuerSerial()
				)
    		);
        }
        else {
            contextSpecific.add(
        		getSigningCertificateV1(
    				(X509Certificate) cert,
    				config.getDigestAlgorithm(),
    				config.isIncludedPolicyOnSigningCertificate(),
    				config.isIncludedIssuerSerial()
				)
    		);
        }

        // Politica de firma
        if (config.getExternalPolicy() != null && config.getExternalPolicy().getPolicyIdentifier() != null) {
        	contextSpecific.add(
        		new Attribute(
        			PKCSObjectIdentifiers.id_aa_ets_sigPolicyId,
        			getSigPolicyAttribute(config.getDigestAlgorithm(), config.getExternalPolicy())
        		)
        	);
        }

        // ContentHints, que se crea en base al ContentType.
        // Este atributo no se creara en las contrafirmas cuando se use un perfil baseline
        if (!isCountersign || !AOSignConstants.SIGN_PROFILE_BASELINE.equals(config.getProfileSet()) ) {
        	// Tampoco se incluira si se ha definio externamente que no se haga (el contentTypeOid sera nulo)
        	if (config.getContentTypeOid() != null) {
        		final ContentHints contentHints;
        		if (config.getContentDescription() != null) {
        			contentHints = new ContentHints(
        					new ASN1ObjectIdentifier(config.getContentTypeOid()),
        					new DERUTF8String(config.getContentDescription())
        					);
        		}
        		else {
        			contentHints = new ContentHints(
        					new ASN1ObjectIdentifier(config.getContentTypeOid())
        					);
        		}
        		contextSpecific.add(
        				new Attribute(
        						PKCSObjectIdentifiers.id_aa_contentHint,
        						new DERSet(contentHints.toASN1Primitive())
        						)
        				);
        	}
        }

        // Atributos adicionales segun seccion 5.11 de RFC 5126

        // commitment-type-indication
        if (config.getCommitmentTypeIndications() != null && config.getCommitmentTypeIndications().size() > 0) {
        	for (final CommitmentTypeIndicationBean ctib : config.getCommitmentTypeIndications()) {
        		contextSpecific.add(
    				new Attribute(
						PKCSObjectIdentifiers.id_aa_ets_commitmentType,
						new DERSet(
							CommitmentTypeIndicationsHelper.generateCommitmentTypeIndication(ctib).toASN1Primitive()
						)
					)
    			);
        	}
        }

        // id-aa-ets-signerLocation (OID=1.2.840.113549.1.9.16.2.17)
        // Este atributo no se anade en PAdES:
        // 4.5.9 signer-location Attribute
        // For all profiles covered in the present document the signer-location attribute shall not be present.
        // NOTE: The location can be indicated by the value of the Location entry in the signature dictionary.
        if (config.getMetadata() != null) {
        	final SignerLocation location = CAdESSignerMetadataHelper.getSignerLocation(config.getMetadata().getSignerLocation());
        	if (location != null) {
        		contextSpecific.add(
        				new Attribute(
        						PKCSObjectIdentifiers.id_aa_ets_signerLocation,
        						new DERSet(location)
        				)
        		);
        	}
        }

        // Roles del firmante. De entre los distintos roles que pueden agregarse a una firma,
        // solo se soporta agregar los roles declarados. La estructua para la declaracion de
        // estos roles nellos, solo se soportan El como se declaran varia segun se utilicen los perfiles
        // avanzados tradicionales (BES, EPES, etc) o los baseline (B-Level, etc):
        //  - En el caso de los perfiles tradicionales, se utilizara signer-attributes segun el estandar ETSI TS 101 733 V2.1.1 (2012-03).
        //
        //		SignerAttribute ::= SEQUENCE OF CHOICE {
        //			claimedAttributes   [0] ClaimedAttributes, OPTIONAL
        //			certifiedAttributes [1] CertifiedAttributes	OPTIONAL (No lo implementamos)
        //		}
        //		ClaimedAttributes ::= SEQUENCE OF Attribute
        //
        //  - En el caso de los perfiles baseline, se utilizara signer-attributes-v2 segun el estandar ETSI EN 319 122-1 V1.1.1 (2016-04).
        //
        //		SignerAttributeV2 ::= SEQUENCE {
        //			claimedAttributes [0] ClaimedAttributes OPTIONAL,
        //			certifiedAttributesV2 [1] CertifiedAttributesV2 OPTIONAL, (No lo implementamos)
        //			signedAssertions [2] SignedAssertions OPTIONAL (No lo implementamos)
        //		}
        //		ClaimedAttributes ::= SEQUENCE OF Attribute
        if (config.getClaimedRoles() != null && config.getClaimedRoles().length > 0) {

        	final ASN1EncodableVector claimedRoles = getSignerClaimedRoles(config.getClaimedRoles());
        	if (claimedRoles != null) {
        		// Identificamos el OID con el que se van a declarar los roles, que dependera del perfil
        		ASN1ObjectIdentifier signerAttrOid = null;
        		if (AOSignConstants.SIGN_PROFILE_ADVANCED.equals(config.getProfileSet()) ) {
        			signerAttrOid = PKCSObjectIdentifiers.id_aa_ets_signerAttr;
        		}
        		else if (AOSignConstants.SIGN_PROFILE_BASELINE.equals(config.getProfileSet()) ) {
        			signerAttrOid = new ASN1ObjectIdentifier(CAdESAttributes.OID_id_aa_ets_signerAttrV2);
        		}
        		// Agregamos los roles
        		if (signerAttrOid != null) {
        			contextSpecific.add(
        					new Attribute(
        							signerAttrOid,
        							new DERSet(
        									new DERSequence(
        											new DERTaggedObject(0, new DERSequence(claimedRoles))
        											)
        									)
        							)
        					);
        		}
        	}
        }

        // La fecha de firma (https://tools.ietf.org/html/rfc3852#section-11.3), no se anade a
        // las firmas PAdES, pero es obligatoria en CAdES. La agregaremos cuando no estemos
        // generando una firma PAdES o cuando se fuerce a
        if (config.getSigningTime() != null) {
        	contextSpecific.add(
    			new Attribute(
    					CMSAttributes.signingTime,
					new DERSet(
						new DERUTCTime(config.getSigningTime())
					)
				)
			);
        }

        // El mimetype de los datos firmados. Solo se agrega si se indica expresamente y nunca
        // en las contrafirmas
        if (!isCountersign && config.getMimeType() != null) {
        	contextSpecific.add(
        			new Attribute(
        					new ASN1ObjectIdentifier(CAdESAttributes.OID_id_aa_ets_mimeType),
        					new DERSet(new DERUTF8String(config.getMimeType())))
			);
        }

        return contextSpecific;
    }

    /**
     * Crea un vector de atributos con los claimed roles declarados.
     * @param claimedRoles Roles declarados.
     * @return Vector de atributos con los roles para su uso en signer-attributes
     * o signer-attributes-v2.
     */
    private static ASN1EncodableVector getSignerClaimedRoles(final String[] claimedRoles) {

    	// Creamos un listado de roles con el ID de rol utilizado en los certificados
    	final ASN1EncodableVector roles = new ASN1EncodableVector();
    	for (final String role : claimedRoles) {
    		if (role != null && !role.isEmpty()) {
    			roles.add(
    					new Attribute(
    							X509AttributeIdentifiers.id_at_role,
    							new DERSet(new DERUTF8String(role))
    					)
    			);
    		}
    	}

		return roles.size() > 0 ? roles : null;
	}

	/** Obtiene un <i>PolicyInformation</i> a partir de los datos de la pol&iacute;tica de un certificado.
     * Sirve para los datos de SigningCertificate y SigningCertificateV2. Tiene que llevar algunos
     * datos de la pol&iacute;tica.
     *
     * <pre>
     * PolicyInformation ::= SEQUENCE {
     *       policyIdentifier   CertPolicyId,
     *       policyQualifiers   SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo OPTIONAL
     * }
     *
     * CertPolicyId ::= OBJECT IDENTIFIER
     *
     * PolicyQualifierInfo ::= SEQUENCE {
     *      policyQualifierId  PolicyQualifierId,
     *      qualifier          ANY DEFINED BY policyQualifierId
     * }
     *
     * -- policyQualifierIds for Internet policy qualifiers
     *
     * id-qt          OBJECT IDENTIFIER ::=  { id-pkix 2 }
     * id-qt-cps      OBJECT IDENTIFIER ::=  { id-qt 1 }
     * id-qt-unotice  OBJECT IDENTIFIER ::=  { id-qt 2 }
     *
     * PolicyQualifierId ::= OBJECT IDENTIFIER ( id-qt-cps | id-qt-unotice )
     *
     * Qualifier ::= CHOICE {
     *      cPSuri           CPSuri,
     *      userNotice       UserNotice
     * }
     *
     * CPSuri ::= IA5String
     *
     * UserNotice ::= SEQUENCE {
     *      noticeRef        NoticeReference OPTIONAL,
     *      explicitText     DisplayText OPTIONAL
     * }
     *
     * NoticeReference ::= SEQUENCE {
     *      organization     DisplayText,
     *      noticeNumbers    SEQUENCE OF INTEGER
     * }
     *
     * DisplayText ::= CHOICE {
     *      ia5String        IA5String      (SIZE (1..200)),
     *      visibleString    VisibleString  (SIZE (1..200)),
     *      bmpString        BMPString      (SIZE (1..200)),
     *      utf8String       UTF8String     (SIZE (1..200))
     * }
     *
     * PolicyQualifierInfo ::= SEQUENCE {
     *          policyQualifierId  PolicyQualifierId,
     *          qualifier          ANY DEFINED BY policyQualifierId
     * }
     *
     * PolicyInformation ::= SEQUENCE {
     *     policyIdentifier   CertPolicyId,
     *     policyQualifiers   SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo OPTIONAL
     * }
     *
     * </pre>
     * @param cert Certificado del cual queremos describir su pol&iacute;tica.
     * @return Estructura con la pol&iacute;tica preparada para insertarla en la firma o
     *         <code>null</code> si el certificado no tiene declarada una pol&iacute;tica. */
    private static PolicyInformation[] getPolicyInformation(final X509Certificate cert) {

        if (cert == null) {
            throw new IllegalArgumentException("El certificado no puede ser nulo"); //$NON-NLS-1$
        }

        final byte[] certificatePoliciesBytes = cert.getExtensionValue("2.5.29.32"); //$NON-NLS-1$
		if (certificatePoliciesBytes == null || certificatePoliciesBytes.length < 1) {
			return null;
		}

		return CertificatePolicies.getInstance(
			ASN1Sequence.getInstance(
				ASN1OctetString.getInstance(certificatePoliciesBytes).getOctets()
			)
		).getPolicyInformation();
    }
}
