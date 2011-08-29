/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.envelopers.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa firma digital CADES-EPES SignedAndEnvelopedData. basado
 * en las especificaciones de RFC-5126.
 * La Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 * SignedAndEnvelopedData ::= SEQUENCE {
 *    version Version,
 *    recipientInfos RecipientInfos,
 *    digestAlgorithms DigestAlgorithmIdentifiers,
 *    encryptedContentInfo EncryptedContentInfo,
 *    certificates
 *      [0] IMPLICIT ExtendedCertificatesAndCertificates
 *         OPTIONAL,
 *    crls
 *      [1] IMPLICIT CertificateRevocationLists OPTIONAL,
 *    signerInfos SignerInfos }
 *
 * </code>
 * </pre>
 *
 * Todos los datos son iguales que en SignedAndEnvelopedData de Pkcs#7 a
 * excepci&oacute;n de que en dentro de signerInfo:
 *
 * <pre>
 * <code>
 *  SignerInfo ::= SEQUENCE {
 *        version CMSVersion,
 *        sid SignerIdentifier,
 *        digestAlgorithm DigestAlgorithmIdentifier,
 *        signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
 *        signatureAlgorithm SignatureAlgorithmIdentifier,
 *        signature SignatureValue,
 *        unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL }
 * </code>
 * </pre>
 *
 * En los atributos de la firma (signedAttrs) va la pol&iacute;tica de la firma.
 * id-aa-ets-sigPolicyId OBJECT IDENTIFIER ::= { iso(1) member-body(2) us(840)
 * rsadsi(113549) pkcs(1) pkcs9(9) smime(16) id-aa(2) 15 }
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

final class CAdESEPESSignedAndEnvelopedData {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Clave de cifrado. La almacenamos internamente. */
    private SecretKey cipherKey;
    private ASN1Set signedAttr2;

    /** M&eacute;todo que genera la firma de tipo SignedAndEnvelopedData.
     * @param parameters
     *        Par&aacute;metros necesarios para la generaci&oacute;n de este
     *        tipo.
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar
     * @param policy
     *        Pol&iacute;tica del certificado.
     * @param qualifier
     *        OID de la pol&iacute;tica.
     * @param signingCertificateV2
     *        <code>true</code> si se desea usar la versi&oacute;n 2 del
     *        atributo <i>Signing Certificate</i> <code>false</code> para
     *        usar la versi&oacute;n 1
     * @param certDest
     *        Certificado del destino al cual va dirigido la firma.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Entrada a la clave de firma
     * @return Firma de tipo SignedAndEnvelopedData.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se encuentra un algoritmo v&aacute;lido. */
    byte[] genCADESEPESSignedAndEnvelopedData(final P7ContentSignerParameters parameters,
                                                     final AOCipherConfig config,
                                                     final String policy,
                                                     final String qualifier,
                                                     final boolean signingCertificateV2,
                                                     final X509Certificate[] certDest,
                                                     final String dataType,
                                                     final PrivateKeyEntry keyEntry) throws IOException,
                                                                              CertificateEncodingException,
                                                                              NoSuchAlgorithmException {

        this.cipherKey = CAdESUtils.initEnvelopedData(config, certDest);

        // 1. VERSION
        // la version se mete en el constructor del signedAndEnvelopedData y es
        // 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final String signatureAlgorithm;
        final String digestAlgorithm;
        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        final String keyAlgorithm;

        try {
            signatureAlgorithm = parameters.getSignatureAlgorithm();
            digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
            keyAlgorithm = CAdESUtils.getKeyAlgorithm(signatureAlgorithm);

            final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));
            digestAlgs.add(digAlgId);
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e); //$NON-NLS-1$
        }

        // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
        ASN1Set certificates = null;
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        certificates = CAdESUtils.fetchCertificatesList(signerCertificateChain);

        // 2. RECIPIENTINFOS
        final Info infos = CAdESUtils.getEnvelopeInfo(parameters.getContent(), config, certDest, this.cipherKey);

        // 4. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificateStructure tbs2 = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs2.getIssuer()), tbs2.getSerialNumber().getValue());

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // AlgorithmIdentifier
        final AlgorithmIdentifier digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(AOAlgorithmID.getOID(digestAlgorithm)), new DERNull());

        // // ATRIBUTOS
        final ASN1EncodableVector contextExpecific =
            CAdESUtils.generateSignerInfo(signerCertificateChain[0],
                                         digestAlgorithm,
                                         digAlgId,
                                         parameters.getContent(),
                                         policy,
                                         qualifier,
                                         signingCertificateV2,
                                         dataType,
                                         null);
        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
        final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId;
        try {
            encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(keyAlgorithm));
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e); //$NON-NLS-1$
        }

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        }
        catch (final AOException ex) {
            throw new IOException("Error en la firma electronica: " + ex); //$NON-NLS-1$
        }

        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null // unsignedAttr
        ));

        final ASN1Set certrevlist = null;

        // construimos el Signed And Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(new DERSet(infos.getRecipientInfos()),
                                                                                                        new DERSet(digestAlgs),
                                                                                                        infos.getEncInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos))).getDEREncoded();
    }

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @return Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException */
    private ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry) throws AOException {

        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        final byte[] tmp;
        try {
            tmp = this.signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            throw new AOException("Error obteniendo los atributos firmados", ex); //$NON-NLS-1$
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(keyEntry.getPrivateKey());
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e); //$NON-NLS-1$
        }

        // firmamos.
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        final ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;

    }

    /** M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre
     * de tipo AuthenticatedEnvelopedData.
     * @param data
     *        fichero que tiene la firma.
     * @param parameters
     * @param keyEntry
     * @param dataType
     * @param policy
     * @param qualifier
     * @param signingCertificateV2
     * @return La nueva firma AuthenticatedEnvelopedData con los remitentes que
     *         ten&iacute;a (si los tuviera) con la cadena de certificados
     *         nueva. */
    byte[] addOriginatorInfo(final InputStream data,
                                    final P7ContentSignerParameters parameters,
                                    final PrivateKeyEntry keyEntry,
                                    final String dataType,
                                    final String policy,
                                    final String qualifier,
                                    final boolean signingCertificateV2) {
        // boolean isValid = false;
        byte[] retorno = null;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)) {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

                final SignedAndEnvelopedData signEnv = new SignedAndEnvelopedData((ASN1Sequence) doj.getObject());

                // Obtenemos los originatorInfo
                final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
                final Enumeration<?> signers = signEnv.getSignerInfos().getObjects();
                while (signers.hasMoreElements()) {
                    signerInfos.add((ASN1Sequence) signers.nextElement());
                }

                // certificado del remitente
                final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

                ASN1EncodableVector signCerts = new ASN1EncodableVector();

                // Si no hay certificados, se deja como esta.
                if (signerCertificateChain.length != 0) {

                    // algoritmo
                    final String signatureAlgorithm;
                    final String digestAlgorithm;
                    final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
                    final String keyAlgorithm;

                    try {
                        signatureAlgorithm = parameters.getSignatureAlgorithm();
                        digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
                        keyAlgorithm = CAdESUtils.getKeyAlgorithm(signatureAlgorithm);

                        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));
                        digestAlgs.add(digAlgId);
                    }
                    catch (final Exception e2) {
                        throw new IOException("Error de codificacion: " + e2); //$NON-NLS-1$
                    }

                    final TBSCertificateStructure tbs2 =
                            TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

                    final IssuerAndSerialNumber encSid =
                            new IssuerAndSerialNumber(X500Name.getInstance(tbs2.getIssuer()), tbs2.getSerialNumber().getValue());

                    final SignerIdentifier identifier = new SignerIdentifier(encSid);

                    // AlgorithmIdentifier
                    final AlgorithmIdentifier digAlgId =
                            new AlgorithmIdentifier(new DERObjectIdentifier(AOAlgorithmID.getOID(digestAlgorithm)), new DERNull());

                    // // ATRIBUTOS
                    final ASN1EncodableVector contextExpecific =
                        CAdESUtils.generateSignerInfo(signerCertificateChain[0],
                                                     digestAlgorithm,
                                                     digAlgId,
                                                     parameters.getContent(),
                                                     policy,
                                                     qualifier,
                                                     signingCertificateV2,
                                                     dataType,
                                                     null);
                    this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
                    final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));

                    final ASN1Set unSignedAttr = null;

                    // digEncryptionAlgorithm
                    final SignerInfo nuevoSigner =
                            CAdESUtils.signAndEnvelope(keyEntry,
                                                  signatureAlgorithm,
                                                  digAlgId,
                                                  identifier,
                                                  signedAttr,
                                                  unSignedAttr,
                                                  sun.security.x509.AlgorithmId.get(keyAlgorithm),
                                                  this.signedAttr2);

                    // introducimos el nuevo Signer
                    signerInfos.add(nuevoSigner);

                    // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
                    signCerts = CAdESUtils.loadCertificatesList(signEnv, signerCertificateChain);
                }
                else {
                    LOGGER.warning("No se ha podido obtener el certificado del nuevo firmante "); //$NON-NLS-1$
                }

                final ASN1Set certrevlist = null;

                // Se crea un nuevo AuthenticatedEnvelopedData a partir de los
                // datos anteriores con los nuevos originantes.
                retorno =
                        new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(signEnv.getRecipientInfos(),// new
                                                                                                                                             // DERSet(recipientInfos),
                                                                                                                 signEnv.getDigestAlgorithms(),// new
                                                                                                                                               // DERSet(digestAlgs),
                                                                                                                 signEnv.getEncryptedContentInfo(),// encInfo,
                                                                                                                 new DERSet(signCerts),// certificates,
                                                                                                                 certrevlist,// certrevlist,
                                                                                                                 new DERSet(signerInfos))).getDEREncoded();
            }

        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de insercion: " + ex); //$NON-NLS-1$

        }
        return retorno;
    }
    
}
