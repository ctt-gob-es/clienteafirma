/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.util.zip.DataFormatException;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.BEROctetString;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.CMSObjectIdentifiers;
import org.spongycastle.asn1.cms.CompressedData;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

/** Clase que crea un tipo Compressed Data seg&uacute;n el RFC 3274 - CMS
 * Compressed Data.
 * La Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 * CompressedData ::= SEQUENCE {
 *  version CMSVersion,
 *  compressionAlgorithm CompressionAlgorithmIdentifier,
 *  encapContentInfo EncapsulatedContentInfo
 * }
 *
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje Compressed Data de SpongyCastle. */
final class CMSCompressedData {

	private CMSCompressedData() {
		// No permitimos la instanciacion
	}

    /** OID de ZLIB * */
    static final String ZLIB = "1.2.840.113549.1.9.16.3.8"; //$NON-NLS-1$

    /** Obtiene un tipo CompressedData.
     * @param data
     *        Datos a comprimir
     * @return Tipo CompressedData.
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
    static byte[] genCompressedData(final byte[] data) throws IOException {

        // Algoritmo de compresion
        final AlgorithmIdentifier comAlgId = new AlgorithmIdentifier(new ASN1ObjectIdentifier(ZLIB));

        // Se comprimen los datos
        final byte[] compressed = BinaryUtils.compress(data);

        final ASN1OctetString comOcts = new BEROctetString(compressed);

        // Contenido comprimido
        final ContentInfo comContent = new ContentInfo(CMSObjectIdentifiers.data, comOcts);

        return new ContentInfo(CMSObjectIdentifiers.compressedData, new CompressedData(comAlgId, comContent)).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que extrae el contenido de un tipo CompressedData.
     * @param data El tipo CompressedData.
     * @return El contenido del envoltorio.
     * @throws IOException Cuando hay un error de lectura de datos.
     * @throws DataFormatException Si los datos no estaban comprimidos. */
    static byte[] getContentCompressedData(final byte[] data) throws IOException, DataFormatException {
        final ASN1Sequence contentEnvelopedData = Utils.fetchWrappedData(data);
        final CompressedData compressed = CompressedData.getInstance(contentEnvelopedData);
        final DEROctetString dos = (DEROctetString) compressed.getEncapContentInfo().getContent();
        return BinaryUtils.uncompress(dos.getOctets());
    }
}
