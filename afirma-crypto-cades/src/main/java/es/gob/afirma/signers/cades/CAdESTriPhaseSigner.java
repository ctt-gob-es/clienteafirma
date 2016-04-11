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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BEROctetString;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.TBSCertificateStructure;
import org.spongycastle.cms.CMSProcessable;
import org.spongycastle.cms.CMSProcessableByteArray;

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
 *   documento a firmar completo, sino &uacute;nicamente una peque&ntilde;a cantidad de datos resultante de un
 *   pre-proceso (la pre-firma) realizado por el sistema externo, lo que resulta en un enorme decremento en las necesidades
 *   de memoria y transmisi&oacute;n de datos (esto &uacute;ltimo si decide omitirse la transferencia del fichero a firmar).
 *  </li>
 *  <li>
 *   Por motivos de seguridad, el documento a firmar no puede salir de un sistema externo. Como se ha descrito en el punto
 *   anterior, en este caso es posible omitir por completo la salida del documento del sistema externo, y puede transferirse &uacute;nicamente
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
 * <p style="text-align: center;"><img src="doc-files/CAdESTriPhaseSigner-1.png" alt="Esquema de la fase 1: Pre-Firma"></p>
 * <ul>
 *  <li>El dispositivo movil solicita una pre-firma al servidor Web indicando un identificador de documento.</li>
 *  <li>El servidor Web solicita el documento a servidor documental.</li>
 *  <li>
 *   El servidor documental entrega el documento al servidor Web.<br> Es importante recalcar que el servidor
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
 * <p style="text-align: center;"><img src="doc-files/CAdESTriPhaseSigner-2.png" alt="Esquema de la fase 2: Firma"></p>
 * <ul>
 *  <li>
 *   El dispositivo m&oacute;vil realiza, de forma completamente aislada una firma electr&oacute;nica
 *   simple (computacionalmente ligera) de los datos de la pre-firma. La clave privada del usuario nunca sale
 *   del dispositivo y no se expone externamente en ningun momento.
 *  </li>
 * </ul>
 * <p><b>Post-firma:</b></p>
 * <p style="text-align: center;"><img src="doc-files/CAdESTriPhaseSigner-3.png" alt="Esquema de la fase 3: Post-Firma"></p>
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
 *   El servidor Web comunica al dispositivo el exito de la operacion y el identificador del fichero
 *   ya firmado y almacenado.
 *  </li>
 * </ul>
 * <p>
 *  El esquema podria ser igualmente implementado sin servidor documental, pudiendo obtener el Servidor Web el documento
 *  desde otro origen, incluyendo el propio dispositivo m&oacute;vil. Igualmente, una vez firmado el documento, su destino
 *  puede ser cualquiera, incluyendo de nuevo al propio dispositivo.
 * </p>
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

    /** Genera los atributos firmados CAdES (prefirma).
     * @param digestAlgorithmName Algoritmo de huella digital
     * @param content Datos a firmar (usar <code>null</code> si no se desean a&ntilde;adir a la firma)
     * @param signerCertificateChain Cadena de certificados del firmante
     * @param policy Pol&iacute;tica de firma
     * @param signingCertificateV2 <code>true</code> para usar SigningCertificateV2, <code>false</code> para usar V1
     * @param dataDigest Valor de la huella digital del contenido (usar <code>null</code> si se estableci&oacute; <code>content</code>)
     * @param signDate Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trif&aacute;sica)
     * @param includeSigningTimeAttribute <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *                                    <code>false</code> para no incluirlo. Este atributo nunca se incluye en el modo PAdES.
     * @param padesMode <code>true</code> para generar una firma CAdES compatible PAdES, <code>false</code> para generar una firma CAdES normal.
     * @param contentType Tipo de contenido definido por su OID.
     * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
     * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
     * @param csm Metadatos sobre el firmante.
     * @param doNotIncludePolicyOnSigningCertificate Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     *                                               pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     *                                               si se establece a <code>false</code> se incluye siempre que el certificado
     *                                               la declare.
     * @return Atributos CAdES a firmar (prefirma) en formato ASN.1.
     * @throws AOException Cuando se produce cualquier error durante el proceso. */
    public static byte[] preSign(final String digestAlgorithmName,
                          final byte[] content,
                          final Certificate[] signerCertificateChain,
                          final AdESPolicy policy,
                          final boolean signingCertificateV2,
                          final byte[] dataDigest,
                          final Date signDate,
                          final boolean includeSigningTimeAttribute,
                          final boolean padesMode,
                          final String contentType,
                          final String contentDescription,
                          final List<CommitmentTypeIndicationBean> ctis,
                          final CAdESSignerMetadata csm,
                          final boolean doNotIncludePolicyOnSigningCertificate) throws AOException {

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
                     dataDigest,
                     signDate,
                     includeSigningTimeAttribute,
                     padesMode,
                     contentType,
                     contentDescription,
                     ctis,
                     csm,
                     false,  // No es contrafirma
                     doNotIncludePolicyOnSigningCertificate
                  )
               )
            );
        }
        catch(final Exception e) {
            throw new AOException("Error obteniendo los atributos a firmar: " + e, e); //$NON-NLS-1$
        }

        try {
            return signedAttributes.getEncoded(ASN1Encoding.DER);
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
     * @throws AOException Cuando se produce cualquier error durante el proceso.
     */
    public static byte[] postSign(final String digestAlgorithmName,
                           final byte[] content,
                           final Certificate[] signerCertificateChain,
                           final byte[] signature,
                           final byte[] signedAttributes
               ) throws AOException {

        if (signerCertificateChain == null || signerCertificateChain.length == 0) {
            throw new IllegalArgumentException("La cadena de certificados debe contener al menos una entrada"); //$NON-NLS-1$
        }

        final TBSCertificateStructure tbsCertificateStructure;
        try {
            tbsCertificateStructure = TBSCertificateStructure.getInstance(
        		ASN1Primitive.fromByteArray(
    				((X509Certificate) signerCertificateChain[0]).getTBSCertificate()
				)
    		);
        }
        catch(final Exception e) {
            throw new AOException("No se ha podido crear la estructura de certificados", e); //$NON-NLS-1$
        }

        final SignerIdentifier signerIdentifier = new SignerIdentifier(
           new IssuerAndSerialNumber(
    		   X500Name.getInstance(tbsCertificateStructure.getIssuer()),
    		   tbsCertificateStructure.getSerialNumber().getValue()
		   )
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
            asn1SignedAttributes = (ASN1Set) ASN1Primitive.fromByteArray(signedAttributes);
        }
        catch (final IOException e) {
            throw new AOException("Error en la inclusion de la recuperacion de los SignedAttibutes", e); //$NON-NLS-1$
        }

        // SignerInfo
        final ASN1EncodableVector signerInfo = new ASN1EncodableVector();
        signerInfo.add(
    		new SignerInfo(
				signerIdentifier,
				digestAlgorithmOID,
				asn1SignedAttributes,
				keyAlgorithmIdentifier,
				encodedPKCS1Signature,
				null
			)
		);

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
            contentInfo = new ContentInfo(
        		new ASN1ObjectIdentifier(
    				PKCSObjectIdentifiers.data.getId()
				),
				new BEROctetString(baos.toByteArray())
    		);
        }
        else {
            contentInfo = new ContentInfo(
        		new ASN1ObjectIdentifier(
    				PKCSObjectIdentifiers.data.getId()
				),
				null
			);
        }

        // Certificados
        final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
        for (final Certificate cert : signerCertificateChain) {
            try {
                ce.add(
            		org.spongycastle.asn1.x509.Certificate.getInstance(
        				ASN1Primitive.fromByteArray(
    						cert.getEncoded()
						)
    				)
        		);
            }
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
            		"Error insertando el certificado '" + AOUtil.getCN((X509Certificate) cert) + "' en la cadena de confianza: " + e //$NON-NLS-1$ //$NON-NLS-2$
        		);
            }
        }
        final ASN1Set certificates = SigUtils.createBerSetFromList(ce);

        // Algoritmos de huella digital
        final ASN1EncodableVector digestAlgorithms = new ASN1EncodableVector();
        digestAlgorithms.add(digestAlgorithmOID);

        try {
			return new ContentInfo(
			   PKCSObjectIdentifiers.signedData,
			   new SignedData(
			      new DERSet(digestAlgorithms),
			      contentInfo,
			      certificates,
			      null,
			      new DERSet(signerInfo)
			   )
			).getEncoded(ASN1Encoding.DER);
		}
        catch (final IOException e) {
			throw new AOException("Error creando el ContentInfo de CAdES: " + e, e); //$NON-NLS-1$
		}

    }

}
