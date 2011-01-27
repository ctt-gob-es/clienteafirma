/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.aobinarysignhelper;

import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.misc.AOUtil;

/**
 * Clase que crea un tipo Compressed Data seg&uacute;n el RFC 3274 - CMS
 * Compressed Data.
 * 
 * La Estructura del mensaje es la siguiente:<br>
 * 
 * <pre><code>
 * 
 * CompressedData ::= SEQUENCE {
 *  version CMSVersion,
 *  compressionAlgorithm CompressionAlgorithmIdentifier,
 *  encapContentInfo EncapsulatedContentInfo
 * }
 * 
 * 
 * </code></pre>
 * 
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje Compressed Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */
public class CMSCompressedData {

	/** OID de ZLIB * */
	public static final String ZLIB = "1.2.840.113549.1.9.16.3.8";

	/**
	 * M&eacute;todo que obtiene un tipo CompressedData.
	 * 
	 * @param data
	 *            Datos a comprimir
	 * @return Tipo CompressedData.
	 * @throws IOException
	 *             Se produce cuando se produce un error al comprimir los datos.
	 */
	public byte[] genCompressedData(InputStream data)
			throws IOException {

		// Algoritmo de compresion
		AlgorithmIdentifier comAlgId = new AlgorithmIdentifier(
				new DERObjectIdentifier(ZLIB));

		// Se obtienen los datos como array de bytes.
		byte[] input = AOUtil.getDataFromInputStream(data);

		// Se comprimen los datos
		byte[] compressed = BinaryUtils.compress(input);

		ASN1OctetString comOcts = new BERConstructedOctetString(compressed);

		// Contenido comprimido
		ContentInfo comContent = new ContentInfo(CMSObjectIdentifiers.data,
				comOcts);

		return new ContentInfo(CMSObjectIdentifiers.compressedData,
				new CompressedData(comAlgId, comContent)).getDEREncoded();

	}

	/**
	 * M&eacute;todo que extrae el contenido de un tipo CompressedData.
	 * 
	 * @param data
	 *            El tipo Compressed Data.
	 * @return El conteido de este tipo.
	 * @throws IOException
	 *             Se produce cuando hay un error de lectura de datos.
	 * 
	 */
	public byte[] getContentCompressedData(InputStream data)
			throws IOException {

		byte[] comp = AOUtil.getDataFromInputStream(data);
		ASN1InputStream is = new ASN1InputStream(comp);

		// Comenzamos a obtener los datos.
		ASN1Sequence dsq = null;
		dsq = (ASN1Sequence) is.readObject();
		Enumeration<Object> e = dsq.getObjects();
		// Elementos que contienen los elementos OID EnvelopedData.
		e.nextElement();
		// Contenido de EnvelopedData
		ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		ASN1Sequence contentEnvelopedData = (ASN1Sequence) doj.getObject();

		CompressedData compressed = CompressedData
				.getInstance(contentEnvelopedData);
		DEROctetString dos = (DEROctetString)compressed.getEncapContentInfo().getContent();

		return BinaryUtils.uncompress(dos.getOctets());

	}
}
