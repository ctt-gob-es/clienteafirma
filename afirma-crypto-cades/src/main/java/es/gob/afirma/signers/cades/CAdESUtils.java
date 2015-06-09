/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1UTCTime;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTF8String;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.ess.ContentHints;
import org.bouncycastle.asn1.ess.ESSCertID;
import org.bouncycastle.asn1.ess.ESSCertIDv2;
import org.bouncycastle.asn1.ess.SigningCertificate;
import org.bouncycastle.asn1.ess.SigningCertificateV2;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierId;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;

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

    /** Genera una estructura <i>SigningCertificateV2</i> seg&uacute;n RFC 5035:
     * @param cert Certificado del firmante
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar
     * @param policy Pol&iacute;tica de firma
     * @return Estructura <i>SigningCertificateV2</i> seg&uacute;n RFC 5035
     * @throws CertificateEncodingException Si el certificado proporcionado no es v&aacute;lido
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella indicado
     * @throws IOException Si hay errores en el tratamiento de datos */
    private static Attribute getSigningCertificateV2(final X509Certificate cert,
    		                                         final String digestAlgorithmName,
    		                                         final AdESPolicy policy) throws CertificateEncodingException,
    		                                                                  NoSuchAlgorithmException,
    		                                                                  IOException {

    	// ALGORITMO DE HUELLA DIGITAL
        final AlgorithmIdentifier digestAlgorithmOID = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));

        // INICIO SINGING CERTIFICATE-V2

        final GeneralNames gns = new GeneralNames(
    		new GeneralName(X500Name.getInstance(cert.getIssuerX500Principal().getEncoded()))
		);

        final IssuerSerial isuerSerial = new IssuerSerial(gns, cert.getSerialNumber());

        final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
        final ESSCertIDv2[] essCertIDv2 = {
            new ESSCertIDv2(digestAlgorithmOID, certHash, isuerSerial)
        };

        final SigningCertificateV2 scv2;
        if (policy != null && policy.getPolicyIdentifier() != null) {
            scv2 = new SigningCertificateV2(essCertIDv2, getPolicyInformation(policy)); // con politica
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
     * @param cert Certificado del firmante
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar
     * @param policy Pol&iacute;tica de firma
     * @return Estructura <i>SigningCertificate</i> seg&uacute;n RFC 5035
     * @throws CertificateEncodingException Si el certificado proporcionado no es v&aacute;lido
     * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella indicado */
    private static Attribute getSigningCertificateV1(final X509Certificate cert,
                                                     final String digestAlgorithmName,
                                                     final AdESPolicy policy) throws CertificateEncodingException,
                                                                                     NoSuchAlgorithmException {

        // INICIO SINGNING CERTIFICATE

        final GeneralName gn = new GeneralName(X500Name.getInstance(cert.getIssuerX500Principal().getEncoded()));
        final GeneralNames gns = new GeneralNames(gn);

        final IssuerSerial isuerSerial = new IssuerSerial(gns, cert.getSerialNumber());

        final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
        final ESSCertID essCertID = new ESSCertID(certHash, isuerSerial);

        final SigningCertificate scv;
        if (policy != null && policy.getPolicyIdentifier() != null) {

             // HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
             // CASTLE NO TIENE DICHO CONSTRUCTOR.

            final ASN1EncodableVector v = new ASN1EncodableVector();
            v.add(new DERSequence(essCertID));
            v.add(new DERSequence(getPolicyInformation(policy)));
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

    private static Attribute getSigPolicyId(final String digestAlgorithmName, final AdESPolicy policy) throws IOException {

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
            hashid= SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
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

        return new Attribute(
			PKCSObjectIdentifiers.id_aa_ets_sigPolicyId,
			new DERSet(
				ds.toASN1Primitive()
			)
		);

    }

    /** Genera la parte que contiene la informaci&oacute;n del Usuario.
     * Se generan los atributos que se necesitan para generar la firma.
     *
     * <pre>
     * SignerInfo ::= SEQUENCE {
     *   version CMSVersion,
     *   sid SignerIdentifier,
     *   digestAlgorithm DigestAlgorithmIdentifier,
     *   signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
     *   signatureAlgorithm SignatureAlgorithmIdentifier,
     *   signature SignatureValue,
     *   unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL
     * }
     *
     * SignerIdentifier ::= CHOICE {
     *   issuerAndSerialNumber IssuerAndSerialNumber,
     *   subjectKeyIdentifier [0] SubjectKeyIdentifier
     * }
     *
     * SignedAttributes ::= SET SIZE (1..MAX) OF Attribute
     *
     * UnsignedAttributes ::= SET SIZE (1..MAX) OF Attribute
     *
     * Attribute ::= SEQUENCE {
     *   attrType OBJECT IDENTIFIER,
     *   attrValues SET OF AttributeValue
     * }
     *
     * AttributeValue ::= ANY
     *
     * SignatureValue ::= OCTET STRING
     *
     * ContentHints ::= SEQUENCE {  (esta secuencia con el tipo de contenido firmado. No se agrega en firmas PAdES)
     *	  contentDescription UTF8String (SIZE (1..MAX)) OPTIONAL,
     *	  contentType ContentType
     * }
     *
     * </pre>
     *
     * @param cert Certificado del firmante
     * @param digestAlgorithmName Nombre del algoritmo de huella digital a usar
     * @param data Datos firmados
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2 {@code true} para utilizar la versi&oacute;n 2 del campo
     *                             signingCertificate, {@code false} para utilizar la versi&oacute;n 1.
     * @param dataDigest Huella digital de los datos firmados
     * @param signDate Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trif&aacute;sica)
     * @param padesMode <code>true</code> para generar una firma CAdES compatible PAdES, <code>false</code> para generar una firma CAdES normal
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Lista de compromisos adquiridos con esta firma
     * @param csm Metadatos sobre el firmante
     * @param isCountersign <code>true</code> si desea generarse el <code>SignerInfo</code> de una
     *                      contrafirma, <code>false</code> en caso contrario.
     * @return Los datos necesarios para generar la firma referente a los datos del usuario.
     * @throws java.security.NoSuchAlgorithmException Cuando se introduce un algoritmo no v&aacute;lido.
     * @throws java.io.IOException Cuando se produce un error de entrada/salida.
     * @throws CertificateEncodingException Error de codificaci&oacute;n en el certificado. */
    public static ASN1EncodableVector generateSignerInfo(final Certificate cert,
                                                         final String digestAlgorithmName,
                                                         final byte[] data,
                                                         final AdESPolicy policy,
                                                         final boolean signingCertificateV2,
                                                         final byte[] dataDigest,
                                                         final Date signDate,
                                                         final boolean padesMode,
                                                         final String contentType,
                                                         final String contentDescription,
                                                         final List<CommitmentTypeIndicationBean> ctis,
                                                         final CAdESSignerMetadata csm,
                                                         final boolean isCountersign) throws NoSuchAlgorithmException,
                                                                                             IOException,
                                                                                             CertificateEncodingException {
        // // ATRIBUTOS

        // authenticatedAttributes (http://tools.ietf.org/html/rfc3852#section-11)
        final ASN1EncodableVector contexExpecific = initContexExpecific(
                digestAlgorithmName,
                data,
                dataDigest,
                signDate,
                isCountersign,
                padesMode
        );

        if (signingCertificateV2) {
            contexExpecific.add(
        		getSigningCertificateV2((X509Certificate) cert, digestAlgorithmName, policy)
    		);
        }
        else {
            contexExpecific.add(
        		getSigningCertificateV1((X509Certificate) cert, digestAlgorithmName, policy)
    		);
        }

        // SIGPOLICYID ATTRIBUTE

        if (policy != null && policy.getPolicyIdentifier() != null) {
            contexExpecific.add(
        		getSigPolicyId(digestAlgorithmName, policy)
    		);
        }

        // ContentHints, que se crea en base al ContentType
        if (contentType != null && !padesMode) {
        	final ContentHints contentHints;
        	if (contentDescription != null) {
        		contentHints = new ContentHints(
    				new ASN1ObjectIdentifier(contentType),
    				new DERUTF8String(contentDescription)
				);
        	}
        	else {
        		contentHints = new ContentHints(new ASN1ObjectIdentifier(contentType));
        	}
        	contexExpecific.add(
    			new Attribute(
        			PKCSObjectIdentifiers.id_aa_contentHint,
        			new DERSet(contentHints.toASN1Primitive())
    			)
			);
        }

        // Atributos adicionales segun seccion 5.11 de RFC 5126

        // commitment-type-indication
        if (ctis != null && ctis.size() > 0) {
        	for (final CommitmentTypeIndicationBean ctib : ctis) {
        		contexExpecific.add(
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
        if (!padesMode && csm != null && CAdESSignerMetadataHelper.getSignerLocation(csm.getSignerLocation()) != null) {
    		contexExpecific.add(
				new Attribute(
					PKCSObjectIdentifiers.id_aa_ets_signerLocation,
					new DERSet(
						CAdESSignerMetadataHelper.getSignerLocation(csm.getSignerLocation())
					)
				)
			);
        }

        return contexExpecific;
    }

    /** Obtiene un <i>PolicyInformation</i> a partir de los datos de la pol&iacute;tica.
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
     * @param policy Pol&iacute;tica de la firma.
     * @return Estructura con la pol&iacute;tica preparada para insertarla en la firma. */
    private static PolicyInformation[] getPolicyInformation(final AdESPolicy policy){

        if (policy == null) {
            throw new IllegalArgumentException("La politica de firma no puede ser nula en este punto"); //$NON-NLS-1$
        }

        final PolicyQualifierId pqid = PolicyQualifierId.id_qt_cps;
        DERIA5String uri = null;

        if (policy.getPolicyQualifier()!=null && !policy.getPolicyQualifier().equals("")){ //$NON-NLS-1$
            uri = new DERIA5String(policy.getPolicyQualifier().toString());
        }

        final ASN1EncodableVector v = new ASN1EncodableVector();
        PolicyQualifierInfo pqi = null;
        if(uri != null){
            v.add(pqid);
            v.add(uri);
            pqi = PolicyQualifierInfo.getInstance(new DERSequence(v));
        }

        if (policy.getPolicyQualifier()==null || pqi == null) {
            return new PolicyInformation[] {
                new PolicyInformation(
            		new ASN1ObjectIdentifier(
        				policy.getPolicyIdentifier().toLowerCase(Locale.US).replace("urn:oid:", "") //$NON-NLS-1$ //$NON-NLS-2$
    				)
        		)
            };
        }

        return new PolicyInformation[] {
            new PolicyInformation(
        		new ASN1ObjectIdentifier(
    				policy.getPolicyIdentifier().toLowerCase(Locale.US).replace("urn:oid:", "") //$NON-NLS-1$ //$NON-NLS-2$
				),
    			new DERSequence(pqi)
    		)
        };

    }

    private static ASN1EncodableVector initContexExpecific(final String dataDigestAlgorithmName,
                                                           final byte[] data,
                                                           final byte[] dataDigest,
                                                           final Date signDate,
                                                           final boolean isCountersign,
                                                           final boolean padesMode) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // ContentType es obligatorio excepto en contrafirmas (donde no debe aparecer nunca),
        // debe tener siempre el valor "id-data"
        if (!isCountersign) {
	        contexExpecific.add(
	    		new Attribute(
					CMSAttributes.contentType,
					new DERSet(PKCSObjectIdentifiers.data)
				)
			);
        }

        // fecha de firma, no se anade en modo PAdES, pero es obligatorio en CAdES
        if (!padesMode) {
            contexExpecific.add(
        		new Attribute(
    				CMSAttributes.signingTime,
    				new DERSet(new ASN1UTCTime(signDate))
				)
    		);
        }

        // MessageDigest
        contexExpecific.add(
    		new Attribute(
				CMSAttributes.messageDigest,
				new DERSet(
					new DEROctetString(
						dataDigest != null ? dataDigest : MessageDigest.getInstance(dataDigestAlgorithmName).digest(data)
					)
				)
			)
		);

        return contexExpecific;
    }

}
