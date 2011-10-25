/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Firmador CAdES en tres fases independientes, adecuado para su uso en un entorno mixto cliente-servidor.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class CAdESTriPhaseSigner {
    
    private CAdESTriPhaseSigner() {
     // No permitimos la instanciacion
    }
    
    /**
     * Genera los atributos firmados CAdES (prefirma).
     * @param digestAlgorithmName Algoritmo de huella digital
     * @param content Datos a firmar (usar <code>null</code> si no se desean a&ntilde;adir a la firma)
     * @param signerCertificateChain Cadena de certificados del firmante
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2 <code>true</code> para usar SigningCertificateV2, <code>false</code> para usar V1
     * @param messageDigest Valor de la huella digital del contenido (usar <code>null</code> si se estableci&oacute; <code>content</code>)
     * @param signDate Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trif&aacute;sica)
     * @return Atributos CAdES a firmar (prefirma) en formato ASN.1
     * @throws AOException
     */
    public static byte[] preSign(final String digestAlgorithmName, 
                          final byte[] content, 
                          final X509Certificate[] signerCertificateChain,
                          final AdESPolicy policy,
                          final boolean signingCertificateV2,
                          final byte[] messageDigest,
                          final Date signDate) throws AOException {
        
        if (signerCertificateChain == null || signerCertificateChain.length == 0) {
            throw new IllegalArgumentException("La cadena de certificados debe contener al menos una entrada"); //$NON-NLS-1$
        } 
        
        // Atributos firmados
        final ASN1Set signedAttributes;
        try {
            signedAttributes = SigUtils.getAttributeSet(
               new AttributeTable(
                  CAdESUtils.generateSignerInfo(
                     signerCertificateChain[0],
                     digestAlgorithmName,
                     content,
                     policy,
                     signingCertificateV2,
                     messageDigest,
                     signDate
                  )
               )
            );
        }
        catch(final Exception e) {
            throw new AOException("Error obteniendo los atributos a firmar", e); //$NON-NLS-1$
        }
        
        try {
            return signedAttributes.getEncoded(ASN1Encodable.DER);
        }
        catch (final Exception ex) {
            throw new AOException("Error al codificar los datos ASN.1 a firmar finalmente", ex); //$NON-NLS-1$
        }

    }
    
    /** Realiza una firma CAdES completa.
     * @param digestAlgorithmName Algoritmo de huella digital
     * @param content Datos a firmar (usar <code>null</code> si no se desean a&ntilde;adir a la firma)
     * @param signerCertificateChain Cadena de certificados del firmante
     * @param signature Firma PKCS#1 v1.5 de los atributos firmados
     * @param signedAttributes Atributos firmados (prefirma)
     * @return Firma CAdES completa
     * @throws AOException  
     */
    public static byte[] postSign(final String digestAlgorithmName,
                           final byte[] content,
                           final X509Certificate[] signerCertificateChain,
                           final byte[] signature,
                           final byte[] signedAttributes
               ) throws AOException {
        
        if (signerCertificateChain == null || signerCertificateChain.length == 0) {
            throw new IllegalArgumentException("La cadena de certificados debe contener al menos una entrada"); //$NON-NLS-1$
        }
                        
        final TBSCertificateStructure tbsCertificateStructure;
        try {
            tbsCertificateStructure = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
        }
        catch(final Exception e) {
            throw new AOException("No se ha podido crear la estructura de certificados", e); //$NON-NLS-1$
        }
        
        final SignerIdentifier signerIdentifier = new SignerIdentifier(
           new IssuerAndSerialNumber(X500Name.getInstance(tbsCertificateStructure.getIssuer()), tbsCertificateStructure.getSerialNumber().getValue())
        );
        
        // Algoritmo de huella digital
        final AlgorithmIdentifier digestAlgorithmOID;
        try {
            digestAlgorithmOID = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo el OID en ASN.1 del algoritmo de huella digital", e); //$NON-NLS-1$
        }
        
        // EncryptionAlgorithm
        final AlgorithmIdentifier keyAlgorithmIdentifier;
        try {
            keyAlgorithmIdentifier = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new AOException("Error al codificar el algoritmo de cifrado", e); //$NON-NLS-1$
        }
        
        // Firma PKCS#1 codificada
        final ASN1OctetString encodedPKCS1Signature = new DEROctetString(signature);

        // Atributos firmados
        final ASN1Set asn1SignedAttributes;
        try {
            asn1SignedAttributes = (ASN1Set) ASN1Object.fromByteArray(signedAttributes);
        } catch (IOException e) {
            throw new AOException("Error en la inclusion de la recuperacion de los SignedAttibutes", e); //$NON-NLS-1$
        }
        
        // SignerInfo
        final ASN1EncodableVector signerInfo = new ASN1EncodableVector();
        signerInfo.add(new SignerInfo(signerIdentifier, digestAlgorithmOID, asn1SignedAttributes, keyAlgorithmIdentifier, encodedPKCS1Signature, null));
        
        
        // ContentInfo
        final ContentInfo contentInfo;
        if (content != null) {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final CMSProcessable msg = new CMSProcessableByteArray(content);
            try {
                msg.write(baos);
            }
            catch (final Exception e) {
                throw new AOException("Error en la escritura del contenido implicito en el ContentInfo", e); //$NON-NLS-1$
            }
            contentInfo = new ContentInfo(new ASN1ObjectIdentifier(PKCSObjectIdentifiers.data.getId()), new BERConstructedOctetString(baos.toByteArray()));
        }
        else {
            contentInfo = new ContentInfo(new ASN1ObjectIdentifier(PKCSObjectIdentifiers.data.getId()), null);
        }

        // Certificados
        final List<DEREncodable> ce = new ArrayList<DEREncodable>();
        for (final X509Certificate cert : signerCertificateChain) {
            try {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getEncoded())));
            }
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("Error insertando el certificado '" + AOUtil.getCN(cert) + "' en la cadena de confianza"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
        }
        final ASN1Set certificates = SigUtils.createBerSetFromList(ce);
        
        // Algoritmos de huella digital
        final ASN1EncodableVector digestAlgorithms = new ASN1EncodableVector();
        digestAlgorithms.add(digestAlgorithmOID);

        return new ContentInfo(
           PKCSObjectIdentifiers.signedData, 
           new SignedData(
              new DERSet(digestAlgorithms),
              contentInfo,
              certificates,
              null,
              new DERSet(signerInfo)
           )
        ).getDEREncoded();
        
    }

}
