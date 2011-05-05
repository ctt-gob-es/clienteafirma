/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.createBerSetFromList;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.makeAlgId;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERConstructedOctetString;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERTaggedObject;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.X509Name;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.ciphers.AOCipherConfig;


/**
 *  Clase que implementa firma digital PKCS#7/CMS AuthenticatedData.
 *  La Estructura del mensaje es la siguiente:<br>
 *  <pre><code>
 *
 * id-ct-authData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *        us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16)
 *        ct(1) 2 }
 *
 *  The authenticated-data content type shall have ASN.1 type
 *  AuthenticatedData:
 *
 *     AuthenticatedData ::= SEQUENCE {
 *       version CMSVersion,
 *       originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *       recipientInfos RecipientInfos,
 *       macAlgorithm MessageAuthenticationCodeAlgorithm,
 *       digestAlgorithm [1] DigestAlgorithmIdentifier OPTIONAL,
 *       encapContentInfo EncapsulatedContentInfo,
 *       authAttrs [2] IMPLICIT AuthAttributes OPTIONAL,
 *       mac MessageAuthenticationCode,
 *       unauthAttrs [3] IMPLICIT UnauthAttributes OPTIONAL }
 *
 *     AuthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     UnauthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     MessageAuthenticationCode ::= OCTET STRING
 *
 * </code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje AuthenticatedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public final class CMSAuthenticatedData {

	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;

	/**
	 * 
	 * @param parameters		Par&aacute;metros necesarios que contienen tanto la firma
	 *                      	del archivo a firmar como los datos del firmante.
	 * @param autenticationAlgorithm Algoritmo para los codigos de autenticaci&oacute;n MAC
	 * @param config			Configuraci&oacute;n del algoritmo para firmar
	 * @param certDest			Certificado del destino al cual va dirigido la firma.
	 * @param dataType			Identifica el tipo del contenido a firmar.
	 * @param applyTimestamp	Si se aplica el Timestamp o no.
	 * @param atrib				Atributos firmados opcionales. 
	 * @param uatrib			Atributos no autenticados firmados opcionales.
	 * @return					Firma de tipo AuthenticatedData.
	 * @throws IOException	Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
	 */
	public byte[] genAuthenticatedData(final P7ContentSignerParameters parameters, 
			                           final String autenticationAlgorithm,
			                           final AOCipherConfig config, 
			                           final X509Certificate[] certDest, 
			                           final Oid dataType, 
			                           final boolean applyTimestamp,
			                           final Map<Oid, byte[]> atrib, 
			                           final Map<Oid,byte[]> uatrib) throws IOException, 
			                                                          CertificateEncodingException, 
			                                                          NoSuchAlgorithmException {
		this.cipherKey = Utils.initEnvelopedData(config, certDest);

		// 1. ORIGINATORINFO
		// obtenemos la lista de certificados
		final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
		final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);
		ASN1Set certrevlist = null;

		OriginatorInfo origInfo = null;
		if (signerCertificateChain.length != 0) {
			//introducimos una lista vacía en los CRL ya que no podemos modificar el codigo de bc.
			final List<DEREncodable> crl = new ArrayList<DEREncodable>();
			certrevlist = createBerSetFromList(crl);
			origInfo = new OriginatorInfo(certificates, certrevlist);
		}

		// 2. RECIPIENTINFOS
		Info infos = Utils.initVariables(parameters.getContent(), config, certDest, cipherKey);

		// 3. MACALGORITHM
		AlgorithmIdentifier macAlgorithm = null;
		try {
			macAlgorithm = makeAlgId(config.getAlgorithm().getOid(), null);
		}
		catch (final Throwable e) {
			throw new IOException("Error de codificacion: " + e);
		}

		// 4. DIGESTALGORITMIDENTIFIER
		AlgorithmIdentifier digAlgId;
		String signatureAlgorithm = parameters.getSignatureAlgorithm();
		String digestAlgorithm = null;

		int with = signatureAlgorithm.indexOf("with");
		if (with > 0) {
			digestAlgorithm = signatureAlgorithm.substring(0, with);            
		}

		AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
		try {
			digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
		}
		catch (final Throwable e) {
			throw new IOException("Error de codificacion: " + e);
		}

		// 5. ENCAPSULATEDCONTENTINFO

		// si se introduce el contenido o no

		ContentInfo encInfo = null;
		ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType.toString());       
		ByteArrayOutputStream bOut = new ByteArrayOutputStream();
		byte[] content2 = parameters.getContent();
		CMSProcessable msg = new CMSProcessableByteArray(content2);
		try {
			msg.write(bOut);
		}
		catch (Throwable ex) {
			throw new IOException("Error en la escritura del procesable CMS: " + ex);
		}
		encInfo = new ContentInfo(contentTypeOID, new BERConstructedOctetString(bOut.toByteArray()));
 
		// 6. ATRIBUTOS FIRMADOS
		ASN1Set authAttr = null;        
		authAttr = generateSignedAtt(
				signerCertificateChain[0], 
				digestAlgorithm, 
				parameters.getContent(),
				dataType, 
				applyTimestamp, 
				atrib);

		// 7. MAC

		byte[] mac = null;
		try{
			mac = Utils.genMac(autenticationAlgorithm, authAttr.getDEREncoded(), cipherKey);
		}
		catch(Throwable e) {
			throw new IOException("Error de codificacion: " + e);
		}

		// 8. ATRIBUTOS NO FIRMADOS.

		ASN1Set unAuthAttr = null;
		unAuthAttr = Utils.generateUnsignedAtt(uatrib);

		// construimos el Authenticated data y lo devolvemos
		return new ContentInfo(
				PKCSObjectIdentifiers.id_ct_authData,
				new AuthenticatedData(
						origInfo,					// OriginatorInfo
						new DERSet(infos.getRecipientInfos()), // ASN1Set
						macAlgorithm, 				// macAlgorithm
						digAlgId, 					// AlgorithmIdentifier
						encInfo, 					// ContentInfo
						authAttr, 					// ASN1Set
						new DEROctetString(mac),	// ASN1OctetString
						unAuthAttr					// ASN1Set
				)
		).getDEREncoded();

	}


	/**
	 *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
	 *  Se generan los atributos que se necesitan para generar la firma.
	 *
	 * @param cert              Certificado necesario para la firma.
	 * @param digestAlgorithm   Algoritmo Firmado.
	 * @param datos             Datos firmados.
	 * @param datatype          Identifica el tipo del contenido a firmar.
	 * @param timestamp			Introducir TimeStaming
	 * @param atrib             Lista de atributos firmados que se insertar&aacute;n dentro del archivo de firma.
	 *
	 * @return      Los atributos firmados de la firma.
	 *
	 * @throws java.security.NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
	 */
	private ASN1Set generateSignedAtt(X509Certificate cert,
			String digestAlgorithm,
			byte[] datos,
			Oid datatype,
			boolean timestamp,
			Map<Oid, byte[]> atrib)
	throws NoSuchAlgorithmException {

		//// ATRIBUTOS

		//authenticatedAttributes
		ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

		//tipo de contenido
		ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(datatype.toString()))));

		//fecha de firma
		if (timestamp){
			ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));
		}

		// Los DigestAlgorithms con SHA-2 tienen un guion:
		if (digestAlgorithm.equals("SHA512")) digestAlgorithm = "SHA-512";
		else if (digestAlgorithm.equals("SHA384")) digestAlgorithm = "SHA-384";
		else if (digestAlgorithm.equals("SHA256")) digestAlgorithm = "SHA-256";

		// Si nos viene el hash de fuera no lo calculamos
		final byte[] md = MessageDigest.getInstance(digestAlgorithm).digest(datos);

		//MessageDigest
		ContexExpecific.add(
				new Attribute(
						CMSAttributes.messageDigest,
						new DERSet(new DEROctetString(md.clone()))
				)
		);

		//Serial Number
		// comentar lo de abajo para version del rfc 3852
		ContexExpecific.add(
				new Attribute(
					X509Name.SERIALNUMBER,
					new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))
				)
		);

		//agregamos la lista de atributos a mayores.
		if (atrib.size()!=0){

			Iterator<Map.Entry<Oid, byte[]>> it = atrib.entrySet().iterator();
			while (it.hasNext()) {
				Map.Entry<Oid, byte[]> e = it.next();
				ContexExpecific.add(
						new Attribute(
								// el oid
								new DERObjectIdentifier((e.getKey()).toString()),
								// el array de bytes en formato string
								new DERSet(new DERPrintableString(e.getValue()))
						)
				);
			}

		}

		return getAttributeSet(new AttributeTable(ContexExpecific));
	}

	/**
	 *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
	 *  Se generan los atributos no firmados.
	 *
	 * @param uatrib    Lista de atributos no firmados que se insertar&aacute;n dentro del archivo de firma.
	 *
	 * @return      Los atributos no firmados de la firma.
	 */
	


	/*************************************************************************/
	/**************** Metodos auxiliares de cifrado **************************/
	/*************************************************************************/

	/**
	 * M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre de tipo AuthenticatedData.
	 *
	 * @param data 	fichero que tiene la firma.
	 * @param signerCertificateChain Cadena de certificados a agregar.
	 * @return  La nueva firma AuthenticatedData con los remitentes que ten&iacute;a (si los tuviera) 
	 * 		 con la cadena de certificados nueva.
	 */
	public byte[] addOriginatorInfo(InputStream data, X509Certificate[] signerCertificateChain){
		//boolean isValid = false;
		byte[] retorno = null;

		ASN1InputStream is = new ASN1InputStream(data);
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		ASN1Sequence dsq = null;
		try {
			dsq = (ASN1Sequence)is.readObject();
			Enumeration<?> e = dsq.getObjects();
			// Elementos que contienen los elementos OID Data
			DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
			if (doi.equals(PKCSObjectIdentifiers.id_ct_authData)){
				// Contenido de Data
				ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
	
				AuthenticatedData auth =new AuthenticatedData((ASN1Sequence)doj.getObject());
	
				AlgorithmIdentifier digAlg = extractAOIfromAuth((ASN1Sequence)doj.getObject());
	
				//Obtenemos los originatorInfo
				OriginatorInfo origInfo = auth.getOriginatorInfo();
				ASN1Set certs = null;
				if(origInfo!=null){
					certs = origInfo.getCertificates();
				}
	
				origInfo = Utils.checkCertificates(signerCertificateChain, origInfo, certs);
	
				// Se crea un nuevo AuthenticatedData a partir de los datos anteriores con los nuevos originantes.
				retorno = new ContentInfo(
						PKCSObjectIdentifiers.id_ct_authData,
						new AuthenticatedData(
								origInfo,					// OriginatorInfo
								auth.getRecipientInfos(), // ASN1Set
								auth.getMacAlgorithm(), 				// macAlgorithm
								digAlg, 					// AlgorithmIdentifier se les ha olvidado a BC implementar el getDigestAlgorithm
								auth.getEncapsulatedContentInfo(), 					// ContentInfo
								auth.getAuthAttrs(), 					// ASN1Set
								auth.getMac(),	// ASN1OctetString
								auth.getUnauthAttrs()					// ASN1Set
						)
				).getDEREncoded();
			}
		} catch (Throwable ex) {
			Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de insercion: " + ex);
		}

		return retorno;
	}

	private AlgorithmIdentifier extractAOIfromAuth(ASN1Sequence auth){

		Enumeration<?> e = auth.getObjects();
		// Elemento 0 : version
		e.nextElement();
		// Elemento 1 : OriginatorInfo
		e.nextElement();
		// Elemento 2 : RecipientsInfo
		e.nextElement();
		// Elemento 3 : MAC Algorithm
		e.nextElement();

		// Elemento 4 : DigestAlgorithm
		DERTaggedObject alg = (DERTaggedObject)e.nextElement();
		ASN1Sequence content = (ASN1Sequence) alg.getObject();
		AlgorithmIdentifier aoi = new AlgorithmIdentifier(content);

		return aoi;
	}
}
