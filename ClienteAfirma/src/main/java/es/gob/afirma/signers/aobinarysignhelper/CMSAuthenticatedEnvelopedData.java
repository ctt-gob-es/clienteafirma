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

import java.io.IOException;
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
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOCipherConfig;


/**
 *  Clase que implementa firma digital PKCS#7/CMS AuthenticatedEnvelopedData (RFC 5083)
 *  La Estructura del mensaje es la siguiente:<br>
 *  <pre><code>
 *
 *  id-ct-authEnvelopedData OBJECT IDENTIFIER ::= { iso(1)
 *         member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9)
 *         smime(16) ct(1) 23 }
 *
 *  The authenticated-data content type shall have ASN.1 type
 *  AuthenticatedEnvelopedData:
 *
 *      AuthEnvelopedData ::= SEQUENCE {
 *       version CMSVersion,
 *       originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *       recipientInfos RecipientInfos,
 *       authEncryptedContentInfo EncryptedContentInfo,
 *       authAttrs [1] IMPLICIT AuthAttributes OPTIONAL,
 *       mac MessageAuthenticationCode,
 *       unauthAttrs [2] IMPLICIT UnauthAttributes OPTIONAL }
 *
 *     AuthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     UnauthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     MessageAuthenticationCode ::= OCTET STRING
 *
 * </code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje AuthenticatedEnvelopedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public final class CMSAuthenticatedEnvelopedData {

	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;

	/**
	 * 
	 * @param parameters		Par&aacute;metros necesarios que contienen tanto la firma
	 *                      	del archivo a firmar como los datos del firmante.
	 * @param autenticationAlgorithm Algoritmo de autenticacion
	 * @param config			Configuraci&oacute;n del algoritmo para firmar
	 * @param certDest			Certificado del destino al cual va dirigido la firma.
	 * @param dataType			Identifica el tipo del contenido a firmar.
	 * @param applySigningTime	Si se aplica la hora de firma o no.
	 * @param atrib				Atributos firmados opcionales. 
	 * @param uatrib			Atributos no autenticados firmados opcionales.
	 * @return					Firma de tipo AuthenticatedData.
	 * @throws IOException	Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
	 */
	public byte[] genAuthenticatedEnvelopedData(P7ContentSignerParameters parameters, String autenticationAlgorithm,
			AOCipherConfig config, X509Certificate[] certDest, Oid dataType, 
			boolean applySigningTime, Map<Oid, byte[]> atrib, Map<Oid,byte[]> uatrib) 
	throws IOException, CertificateEncodingException, NoSuchAlgorithmException {
		this.cipherKey = Utils.initEnvelopedData(config, certDest);

		// 1.  ORIGINATORINFO
		// obtenemos la lista de certificados
		X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
		ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);
		ASN1Set certrevlist = null;

		OriginatorInfo origInfo = null;
		if (signerCertificateChain.length != 0) {
			//introducimos una lista vacía en los CRL ya que no podemos modificar el codigo de bc.
			List<DEREncodable> crl = new ArrayList<DEREncodable>();
			certrevlist = createBerSetFromList(crl);
			origInfo = new OriginatorInfo(certificates, certrevlist);
		}

		// 2.   RECIPIENTINFOS
		Info infos = Utils.initVariables(parameters.getContent(), config, certDest, cipherKey);

		// 4. ATRIBUTOS FIRMADOS
		ASN1Set authAttr = null;        
		authAttr = generateSignedAtt(dataType, applySigningTime, atrib);

		// 5. MAC
		byte[] mac = Utils.genMac(autenticationAlgorithm, genPack(authAttr.getDEREncoded(), 
				parameters.getContent()), cipherKey);


		// 6. ATRIBUTOS NO FIRMADOS.
		ASN1Set unAuthAttr = null;
		unAuthAttr = Utils.generateUnsignedAtt(uatrib);


		// construimos el Authenticated data y lo devolvemos
		return new ContentInfo(
				PKCSObjectIdentifiers.id_ct_authEnvelopedData,
				new AuthEnvelopedData(
						origInfo,					// originatorInfo,
						new DERSet(infos.getRecipientInfos()), // recipientInfos,
						infos.getEncInfo(), 				    // authEncryptedContentInfo,
						authAttr, 					// authAttrs
						new DEROctetString(mac),	// mac
						unAuthAttr 					// unauthAttrs
				)
		).getDEREncoded();

	}

	private byte[] genPack(byte[] parte1, byte[] parte2){
		byte [] pack = new byte[parte1.length + parte2.length];

		for (int i = 0; i<parte1.length; i++){
			pack[i] = parte1[i];
		}

		for (int i = 0; i<parte2.length; i++){
			pack[i+parte1.length] = parte2[i];
		}

		return pack;		
	}


	/**
	 *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
	 *  Se generan los atributos que se necesitan para generar la firma.
	 *
	 * @param cert              Certificado necesario para la firma.
	 * @param digestAlgorithm   Algoritmo Firmado.
	 * @param datos             Datos firmados.
	 * @param datatype          Identifica el tipo del contenido a firmar.
	 * @param signingTime		Introducir la hora de firma (tomada del sistema)
	 * @param atrib             Lista de atributos firmados que se insertar&aacute;n dentro del archivo de firma.
	 * @return      Los atributos firmados de la firma.
	 */
	private ASN1Set generateSignedAtt(final Oid datatype, final boolean signingTime, final Map<Oid, byte[]> atrib) {

		//// ATRIBUTOS

		//authenticatedAttributes
		ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

		//tipo de contenido
		ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(datatype.toString()))));

		//fecha de firma
		if (signingTime){
			ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));
		}

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

	


	/*************************************************************************/
	/**************** Metodos auxiliares de cifrado **************************/
	/*************************************************************************/

	/**
	 * M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre de tipo AuthenticatedEnvelopedData.
	 *
	 * @param data 	Datos CMS de tipo AuthenticatedEnvelopedData.
	 * @param signerCertificateChain Cadena de certificados a agregar.
	 * @return  La nueva firma AuthenticatedEnvelopedData con los remitentes que ten&iacute;a (si los tuviera) 
	 * 		 con la cadena de certificados nueva.
	 */
	public byte[] addOriginatorInfo(byte[] data, X509Certificate[] signerCertificateChain){
		//boolean isValid = false;
		byte[] retorno = null;
		try {
			ASN1InputStream is = new ASN1InputStream(data);
			// LEEMOS EL FICHERO QUE NOS INTRODUCEN
			ASN1Sequence dsq = null;
			dsq = (ASN1Sequence)is.readObject();
			
			Enumeration<?> e = dsq.getObjects();
			// Elementos que contienen los elementos OID Data
			DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
			
			if (doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)) {
				// Contenido de Data
				ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

				AuthEnvelopedData authEnv =new AuthEnvelopedData((ASN1Sequence)doj.getObject());

				//Obtenemos los originatorInfo
				OriginatorInfo origInfo = authEnv.getOriginatorInfo();
				ASN1Set certs = null;
				if(origInfo!=null){
					certs = origInfo.getCertificates();
				}

				origInfo = Utils.checkCertificates(signerCertificateChain, origInfo, certs);

				// Se crea un nuevo AuthenticatedEnvelopedData a partir de los datos anteriores con los nuevos originantes.
				retorno = new ContentInfo(
						PKCSObjectIdentifiers.id_ct_authEnvelopedData,
						new AuthEnvelopedData(
								origInfo,					// OriginatorInfo
								authEnv.getRecipientInfos(), // ASN1Set
								authEnv.getAuthEncryptedContentInfo(),
								authEnv.getAuthAttrs(),
								authEnv.getMac(),
								authEnv.getUnauthAttrs()
						)
				).getDEREncoded();
			}

		} 
		catch (Throwable ex) {
			Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de insercion: " + ex);
		}
		return retorno;
	}
}
