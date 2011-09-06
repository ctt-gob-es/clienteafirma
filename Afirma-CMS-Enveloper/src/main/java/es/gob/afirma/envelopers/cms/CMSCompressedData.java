/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;

import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

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
 * para crear un mensaje Compressed Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
public final class CMSCompressedData {

    /** OID de ZLIB * */
    public static final String ZLIB = "1.2.840.113549.1.9.16.3.8";

    /** Obtiene un tipo CompressedData.
     * @param data
     *        Datos a comprimir
     * @return Tipo CompressedData. */
    public byte[] genCompressedData(final byte[] data) {

        // Algoritmo de compresion
        final AlgorithmIdentifier comAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(ZLIB));

        // Se comprimen los datos
        final byte[] compressed = BinaryUtils.compress(data);

        final ASN1OctetString comOcts = new BERConstructedOctetString(compressed);

        // Contenido comprimido
        final ContentInfo comContent = new ContentInfo(CMSObjectIdentifiers.data, comOcts);

        return new ContentInfo(CMSObjectIdentifiers.compressedData, new CompressedData(comAlgId, comContent)).getDEREncoded();

    }

    /** M&eacute;todo que extrae el contenido de un tipo CompressedData.
     * @param data
     *        El tipo CompressedData.
     * @return El contenido del envoltorio.
     * @throws IOException
     *         Se produce cuando hay un error de lectura de datos. */
    public byte[] getContentCompressedData(final byte[] data) throws IOException {
        final ASN1Sequence contentEnvelopedData = Utils.fetchWrappedData(data);
        final CompressedData compressed = CompressedData.getInstance(contentEnvelopedData);
        final DEROctetString dos = (DEROctetString) compressed.getEncapContentInfo().getContent();

        return BinaryUtils.uncompress(dos.getOctets());

    }
}
