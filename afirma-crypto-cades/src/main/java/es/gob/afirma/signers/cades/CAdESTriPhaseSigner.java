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
 * <p>La firma electr&oacute;nica en tres fases est&aacute; pensada para entornos donde la clave privada reside
 * en un sistema con al menos alguna de las siguientes restricciones:</p>
 * <ul>
 *  <li>
 *   El sistema no es compatible con el Cliente @firma. En este caso, dado que el 95% del c&oacute;digo se
 *   ejecuta en un sistema externo, solo es necesario portar el 5% restante.
 *  </li>
 *  <li>
 *   El sistema tiene unas capacidades muy limitadas en cuanto a proceso computacional, memoria o comunicaciones por
 *   red. En este caso, el sistema solo realiza una operaci&oacute;n criptogr&aacute;fica, una firma PKCS#1,
 *   mucho menos demandante de potencia de proceso que una firma completa CAdES, y, adicionalmente, no trata el
 *   documento a firmar completo, sino &uacute;icamente una prque&ntilde;a cantidad de datos resultante de un
 *   pre-proceso (la pre-firma) realizado por el sistema externo, lo que resulta en un enorme decremento en las necesidades
 *   de memoria y transmisi&oacute;n de datos.
 *  </li>
 *  <li>
 *   Por motivos de seguridad, el documento a firmar no puede salir de un sistema externo. Como se ha descrito en el punto
 *   anterior, en este caso el documento jam&aacute;s sale del sistema externo, sino que se transfiere &uacute;nicamente
 *   el resultado de la pre-firma, desde la cual es imposible reconstruir el documento original.
 *  </li>
 * </ul>
 * <p>
 *  Estos condicionantes convierten la firma trif&aacute;sica en una opci&oacute;n perfectamente adaptada a los
 *  dispositivos m&oacute;viles, donde se dan tanto la heterogeneidad de sistemas operativos (Apple iOS, Google
 *  Android, RIM BlackBerry, Microsoft Windows Phone, etc.) y las limitaciones en potencia de proceso, memoria
 *  y comunicaciones; en estas &uacute;ltimas hay que tener en cuenta el coste, especialmente si estamos haciendo
 *  uso de una red de otro operador en itinerancia (<i>roaming</i>).
 * </p>
 * <p>
 *  El funcionamiento t&iacute;pico de una firma trif&aacute;sica en la que intervienen un disposotivo m&oacute;vil,
 *  un servidor Web (que hace la pre-firma y la post-firma) y un servidor documental podr&iacute;a ser el siguiente:
 * </p>
 * <p><b>Pre-firma:</b></p>
 * <p align="center"><img src="doc-files/CAdESTriPhaseSigner-1.png"></p>
 * <ul>
 *  <li>El dispositivo móvil solicita una pre-firma al servidor Web indicando un identificador de documento.</li>
 *  <li>El servidor Web solicita el documento a servidor documental.</li>
 *  <li>
 *   El servidor documental entrega el documento al servidor Web.<br>Es importante recalcar que el servidor
 *   documental no necesita almacenar ning&uacute;n dato de sesi&oacute;n y que este no est&aacute; expuesto a Internet
 *   de forma directa en ning&uacute;n momento.
 *  </li>
 *  <li>
 *   El servidor Web calcula la pre-firma, entregando el resultado (muy peque&ntilde;o en tama&ntilde;o) al dispositivo.<br>
 *   Es importante recalcar que el servidor Web no necesita almacenar ning&uacute;n dato de sesi&oacute;n ni
 *   exponer los documentos directamente al dispositivo.
 *  </li>
 * </ul>
 * <p><b>Firma:</b></p>
 * <p align="center"><img src="doc-files/CAdESTriPhaseSigner-2.png"></p>
 * <ul>
 *  <li>
 *   El dispositivo m&oacute;vil realiza, de forma completamente aislada una firma electr&oacute;nica 
 *   simple (computacionalmente ligera) de los datos de la pre-firma. La clave privada del usuario nunca sale
 *   del dispositivo y no se expone externamente en ningún momento.
 *  </li>
 * </ul>
 * <p><b>Post-firma:</b></p>
 * <p align="center"><img src="doc-files/CAdESTriPhaseSigner-3.png"></p>
 * <ul>
 *  <li>
 *   El dispositivo m&oacute;vil solicita una post-firma al servidor Web indicando un identificador de 
 *   documento y proporcionando el resultado de su pre-firma firmada.
 *  </li>
 *  <li>El servidor Web solicita el documento a servidor documental.</li>
 *  <li>El servidor documental entrega el documento al servidor Web.</li>
 *  <li>
 *   El servidor Web calcula la post-firma y compone el documento final firmado, entregando el resultado 
 *   al servidor documental para su almac&eacute;n.
 *  </li>
 *  <li>El servidor documental almacena el nuevo documento y devuelve un identificador al servidor Web.</li>
 *  <li>
 *   El servidor Web comunica al dispositivo el éxito de la operación y el identificador del fichero 
 *   ya firmado y almacenado.
 *  </li>
 * </ul>
 * <p>
 *  Es conveniente tener en cuenta al usar firmas trif&aacute;sicas que es necesario disponer de un mecanismo
 *  para que el usuario pueda ver en todo momento los documentos que est&aacute; firmando (una copia que refleje
 *  con fidelidad el contenido firmado puede ser suficiente) para evitar situaciones de repudio.
 * </p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESTriPhaseSigner {
    
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
