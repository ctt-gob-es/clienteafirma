/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.applet.pkcs7;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.AlgorithmParameterSpec;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEParameterSpec;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.signers.pkcs7.DigestedData;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/**
 * Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para CMS a partir de un fichero
 * pasado por par&aacute;metro.
 *
 * La informaci&oacute;n es para los tipo:
 *
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * <li>Authenticated Data</li>
 * <li>Authenticated and Enveloped Data</li>
 * </ul>
 */
public final class CMSInformation {

	private static final byte[] SALT = {
		(byte)0xA2, (byte)0x35, (byte)0xDC, (byte)0xA4,
		(byte)0x11, (byte)0x7C, (byte)0x99, (byte)0x4B
	};

	private static final int ITERATION_COUNT = 9;
	
	/**
	 * M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un fichero firmado
	 * de tipo CMS.
	 * @param data Objeto CMS.
	 * @return Texto descriptivo del objeto CMS.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws AOInvalidFormatException Error de formato no v&aacute;lido.
	 */
	public String getInformation(byte[] data) throws IOException, AOInvalidFormatException {
		String datos=""; //$NON-NLS-1$

		ASN1InputStream is = new ASN1InputStream(data);
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		ASN1Sequence dsq = null;
		dsq=(ASN1Sequence)is.readObject();
		Enumeration<?> e = dsq.getObjects();
		// Elementos que contienen los elementos OID Data
		DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
		// Contenido a obtener informacion
		ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
		if (doi.equals(PKCSObjectIdentifiers.data)){
			datos = "Tipo: Data\n"; //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.digestedData)){
			datos = getFromDigestedData(doj);
		}
		else if(doi.equals(PKCSObjectIdentifiers.encryptedData)){
			
			datos = extractData(doj, "5", "Tipo: Encrypted\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedData)){
			datos = extractData(doj, "4", "Tipo: SignedData\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
			datos = extractData(doj, "0", "Tipo: EnvelopedData\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
			datos = extractData(doj, "3", "Tipo: SignedAndEnvelopedData\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authData)){
			datos = extractData(doj, "1", "Tipo: AuthenticatedData\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)){
			datos = extractData(doj, "2", "Tipo: AuthenticatedEnvelopedData\n", "CMS"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		else if(doi.equals(CMSObjectIdentifiers.compressedData)){
			datos = getFromCompressedData(doj);
		}
		else {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
		}

		return datos;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Digested Data.
	 * @return  Representaci&oacute;n de los datos.
	 */
	private String getFromDigestedData(ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + "Tipo: DigestedData\n"; //$NON-NLS-1$

		//obtenemos el digestedData
		DigestedData dd = new DigestedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + "Version: " + dd.getVersion() + "\n"; //$NON-NLS-1$ //$NON-NLS-2$

		//obtenemos el algoritmo
		detalle = detalle + "Algoritmo de firma: " + dd.getDigestAlgorithm() + "\n"; //$NON-NLS-1$ //$NON-NLS-2$

		//obtenemos el tipo de contenido
		detalle = detalle + "Tipo de Contenido: " + dd.getContentType() + "\n"; //$NON-NLS-1$ //$NON-NLS-2$

		return detalle;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Compressed Data.
	 * @return  Representaci&oacute;n de los datos.
	 */
	private String getFromCompressedData(ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + "Tipo: CompressedData\n"; //$NON-NLS-1$
		CompressedData ed = new CompressedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + "Version: "+ed.getVersion()+"\n"; //$NON-NLS-1$ //$NON-NLS-2$

		final AlgorithmIdentifier aid = ed.getCompressionAlgorithmIdentifier();
		if (aid.getAlgorithm().toString().equals("1.2.840.113549.1.9.16.3.8")){ //$NON-NLS-1$
			detalle = detalle + "OID del Algoritmo de firma: ZLIB\n"; //$NON-NLS-1$
		}
		else{
			detalle = detalle + "OID del Algoritmo de firma: " + aid.getAlgorithm() + "\n"; //$NON-NLS-1$ //$NON-NLS-2$
		}

		return detalle;
	}

	/**
	 * Obtiene la informaci&oacute;n de diferentes tipos de formatos.
	 * @param doj Etiqueta ASN.1 de la que se obtienen los datos.
	 * @param envelopeType	Tipo de formato:
	 * <li>0: EnvelopedData</li>
	 * <li>1: AuthenticatedData</li>
	 * <li>2: AuthEnvelopedData</li>
	 * <li>3: SignedAndEnvelopedData</li>
	 * <li>4: SignedData</li>
	 * <li>5: Encrypted</li>
	 * @param tipoDetalle	Tipo de datos (literal)
	 * @param signBinaryType Tipo de firmado binario (CADES o CMS)
	 * @return  Representaci&oacute;n de los datos.
	 */
	public static String extractData(ASN1TaggedObject doj, String envelopeType, 
			String tipoDetalle, String signBinaryType) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + tipoDetalle;
	
		ASN1Set rins = null;
		EncryptedContentInfo encryptedContentInfo = null;
		ASN1Set unprotectedAttrs = null;
		DERInteger version = null;
		AlgorithmIdentifier aid = null;
		ContentInfo ci = null;
		ASN1Set authAttrs = null;
		ASN1Set ds = null;
		ASN1Set signerInfosSd = null;
	
		if (envelopeType.equals("0")) { //$NON-NLS-1$
			EnvelopedData ed = new EnvelopedData((ASN1Sequence)doj.getObject());
			version = ed.getVersion();
			rins = ed.getRecipientInfos();
			encryptedContentInfo = ed.getEncryptedContentInfo();
			unprotectedAttrs = ed.getUnprotectedAttrs();
		} else if (envelopeType.equals("1")) { //$NON-NLS-1$
			AuthenticatedData ed = new AuthenticatedData((ASN1Sequence)doj.getObject());
			version = ed.getVersion();
			rins = ed.getRecipientInfos();
			aid = ed.getMacAlgorithm();
			ci = ed.getEncapsulatedContentInfo();
			authAttrs = ed.getAuthAttrs();
			unprotectedAttrs = ed.getUnauthAttrs();
		} else if (envelopeType.equals("2")) { //$NON-NLS-1$
			AuthEnvelopedData ed = new AuthEnvelopedData((ASN1Sequence)doj.getObject());
			version = ed.getVersion();
			rins = ed.getRecipientInfos();
			encryptedContentInfo = ed.getAuthEncryptedContentInfo();
			authAttrs = ed.getAuthAttrs();
			unprotectedAttrs = ed.getUnauthAttrs();
		} else if (envelopeType.equals("3")) { //$NON-NLS-1$
			SignedAndEnvelopedData ed = new SignedAndEnvelopedData((ASN1Sequence)doj.getObject());
			version = ed.getVersion();
			rins = ed.getRecipientInfos();
			encryptedContentInfo = ed.getEncryptedContentInfo();
			signerInfosSd = ed.getSignerInfos();
		} else if (envelopeType.equals("4")) { //$NON-NLS-1$
			SignedData ed = new SignedData((ASN1Sequence)doj.getObject());
			version = ed.getVersion();
			ds = ed.getDigestAlgorithms();
			ci = ed.getEncapContentInfo();
			signerInfosSd = ed.getSignerInfos();
		} else if (envelopeType.equals("5")) { //$NON-NLS-1$
			ASN1Sequence ed = (ASN1Sequence) doj.getObject();
			version = DERInteger.getInstance(ed.getObjectAt(0));
			encryptedContentInfo = EncryptedContentInfo.getInstance(ed.getObjectAt(1));
			if (ed.size() == 3)
				unprotectedAttrs = (ASN1Set) ed.getObjectAt(2);
		}
	
		//obtenemos la version
		detalle = detalle + "Version: " + version + "\n"; //$NON-NLS-2$
	
		//recipientInfo
		if (rins != null) {
			if (!envelopeType.equals("4") && !envelopeType.equals("5") && rins.size()>0) {
				detalle = detalle + "Destinatarios:" + "\n"; //$NON-NLS-2$
			}
			for (int i=0; i<rins.size(); i++){
				KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(RecipientInfo.getInstance(rins.getObjectAt(i)).getInfo());
				detalle = detalle + " - Informacion de destino de firma " + (i+1) + ":\n"; //$NON-NLS-2$
				final AlgorithmIdentifier diAlg= kti.getKeyEncryptionAlgorithm();
	
				//issuer y serial
				final IssuerAndSerialNumber iss =
					(IssuerAndSerialNumber) SignerIdentifier.getInstance(kti.getRecipientIdentifier().getId()).getId();
				detalle = detalle + "\tIssuer: " + iss.getName().toString() + "\n"; //$NON-NLS-2$
				detalle = detalle + "\tNumero de serie: " + iss.getSerialNumber() + "\n"; //$NON-NLS-2$
	
				// el algoritmo de cifrado de los datos
				AOCipherAlgorithm algorithm = null;
				AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();
	
				// obtenemos el algoritmo usado para cifrar la pass
				for (int j=0;j<algos.length;j++){
					if (algos[j].getOid().equals(diAlg.getAlgorithm().toString())){
						algorithm = algos[j];
					}
				}
				if (algorithm != null){
					detalle = detalle + "\tAlgoritmo de cifrado: " + algorithm.getName() + "\n"; //$NON-NLS-2$
				} 
				else {
					detalle = detalle + "\tOID del algoritmo de cifrado: " + diAlg.getAlgorithm() + "\n"; //$NON-NLS-2$
	
				}
			}
		}
	
		if (envelopeType.equals("0") || envelopeType.equals("5")) { //$NON-NLS-1$ //$NON-NLS-2$
			//obtenemos datos de los datos cifrados.
			detalle = detalle + "Informacion de los datos cifrados:" + "\n"; //$NON-NLS-2$
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType.equals("1") && aid != null && ci != null){ //$NON-NLS-1$
			// mac algorithm
			detalle = detalle + "OID del Algoritmo de MAC: " + aid.getAlgorithm() + "\n"; //$NON-NLS-2$
	
			//digestAlgorithm
			ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			ASN1TaggedObject da = (ASN1TaggedObject)seq.getObjectAt(4);
			AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObject());
			detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n"; //$NON-NLS-2$
	
			//obtenemos datos de los datos cifrados.
			detalle = detalle + "OID del tipo de contenido: " + ci.getContentType() + "\n"; //$NON-NLS-2$
	
			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType.equals("2")) { //$NON-NLS-1$
			detalle = detalle + "Informacion de los datos cifrados:\n";
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
	
			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType.equals("3")) {
			//algoritmo de firma
			ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			ASN1Set da = (ASN1Set)seq.getObjectAt(2);
			AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
			detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n"; //$NON-NLS-2$
	
			//obtenemos datos de los datos cifrados.
			detalle = detalle + "Informacion de los datos cifrados:\n";
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType.equals("4") && ci != null && ds != null) { //$NON-NLS-1$
			//algoritmo de firma
			AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(ds.getObjectAt(0));
			detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n"; //$NON-NLS-2$
			detalle = detalle + "OID del tipo de contenido: " + ci.getContentType() + "\n"; //$NON-NLS-2$
		}
	
		//obtenemos lo atributos opcionales
		if (!envelopeType.equals("3")) //$NON-NLS-1$
			if (unprotectedAttrs == null){
				detalle = detalle + "Atributos : No tiene atributos opcionales\n";
			}
			else{
				String atributos = getUnSignedAttributes(unprotectedAttrs.getObjects());
				detalle = detalle + "Atributos : \n";
				detalle = detalle + atributos;
			}
		else if (envelopeType.equals("3") || envelopeType.equals("4")) { //$NON-NLS-1$ //$NON-NLS-2$
			//obtenemos el(los) firmate(s)
			if (signerInfosSd != null) {
				if (signerInfosSd.size()>0){
					detalle = detalle + "Firmantes:\n";
				}
				for(int i =0; i< signerInfosSd.size(); i++){
					SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));
	
					detalle = detalle + "- firmante "+(i+1)+" :\n"; //$NON-NLS-2$
					// version
					detalle = detalle + "\tversion: " + si.getVersion() + "\n"; //$NON-NLS-2$
					//signerIdentifier
					SignerIdentifier sident = si.getSID();
					IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
					detalle = detalle + "\tIssuer: " + iss.getName().toString() + "\n"; //$NON-NLS-2$
					detalle = detalle + "\tNumero de serie: "+iss.getSerialNumber() + "\n"; //$NON-NLS-2$
	
					//digestAlgorithm
					AlgorithmIdentifier algId = si.getDigestAlgorithm();
					detalle = detalle + "\tOID del algoritmo de firma de este firmante: " + algId.getAlgorithm() + "\n";
	
					//obtenemos lo atributos obligatorios
					ASN1Set sa =si.getAuthenticatedAttributes();
					String satributes = ""; //$NON-NLS-1$
					if (sa != null){
						satributes = getsignedAttributes(sa, signBinaryType);
					}
					detalle = detalle + "\tAtributos obligatorios : \n";
					detalle = detalle + satributes;
				}
			}
		}
		return detalle;
	}

	/**
	 * Obtiene los atributos obligatorios
	 * @param signBinaryType	Tipo de firma binaria (CADES o CMS)
	 * @param detalle
	 * @param authAttrs	
	 * @return
	 */
	private static String getObligatorieAtrib(String signBinaryType,
			String detalle, ASN1Set authAttrs) {
		//obtenemos lo atributos obligatorios
		if (authAttrs == null){
			detalle = detalle + "Atributos Autenticados: No tiene atributos autenticados\n";
		}
		else{
			String atributos = getsignedAttributes(authAttrs, signBinaryType);
			detalle = detalle + "Atributos Autenticados: \n";
			detalle = detalle + atributos;
		}
		return detalle;
	}
	
	/**
	 * Obtiene los atributos obligatorios de una firma.
	 *
	 * @param attributes    Grupo de atributos opcionales
	 * @param binarySignType	Identifica el tipo de firma binaria (CMS o CADES)
	 * @return              lista de atributos concatenados.
	 */
	public static String getsignedAttributes(ASN1Set attributes, String binarySignType){
		String attributos = ""; //$NON-NLS-1$

		final Enumeration<?> e = attributes.getObjects();

		while (e.hasMoreElements()){
			ASN1Sequence a = (ASN1Sequence)e.nextElement();
			DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + "\t\t" + "OID del tipo de contenido: "+ a.getObjectAt(1) + "\n"; //$NON-NLS-1$ //$NON-NLS-3$
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + "\t\t" + "Contiene el atributo \"MessageDigest\"" + "\n"; //$NON-NLS-1$ //$NON-NLS-3$
			}
			//la fecha de firma. obtenemos y casteamos a algo legible.
			if (derIden.equals(CMSAttributes.signingTime)){
				ASN1Set time = (ASN1Set)a.getObjectAt(1);
				DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
				Date date = null;
				try {
					date = d.getDate();
				} catch (ParseException ex) {
					Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss"); //$NON-NLS-1$
				String ds = formatter.format(date);

				attributos = attributos + "\t\tContiene fecha de firma: " + ds + "\n"; //$NON-NLS-2$
			}
			if (binarySignType.equals("CADES")) {            //$NON-NLS-1$
				//atributo signing certificate v2
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)){
					attributos = attributos + "\t\tContiene el atributo \"Signing Certificate V2\" \n";
				}
				//Politica de firma.
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)){                
					attributos = attributos + "\t\tContiene la politica de la firma \n";
				}
			}
		}
		return attributos;
	}
	
	/**
	 * Obtiene los atributos opcionales de una firma cualquiera.
	 * En caso de ser EncryptedData, usar el otro metodo, ya que por construccion
	 * no es posible utilizar este.
	 *
	 * @param attributes    Grupo de atributos opcionales
	 * @return              lista de atributos concatenados.
	 */
	private static String getUnSignedAttributes(Enumeration<?> e){
		String attributos="";

		while (e.hasMoreElements()){
			ASN1Sequence a = (ASN1Sequence)e.nextElement();
			DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + "\tOID del tipo de contenido: "+ a.getObjectAt(1) +"\n";
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + "\tContiene el atributo \"MessageDigest\"\n";
			}
			//la fecha de firma. obtenemos y casteamos a algo legible.
			if (derIden.equals(CMSAttributes.signingTime)){
				ASN1Set time = (ASN1Set)a.getObjectAt(1);
				DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
				Date date = null;
				try {
					date = d.getDate();
				} catch (ParseException ex) {
					Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
				}
				SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
				String ds = formatter.format(date);

				attributos = attributos + "\tContiene fecha de firma: "+ ds +"\n";
			}
			//contrafirma de la firma.
			if (derIden.equals(CMSAttributes.counterSignature)){
				attributos = attributos + "\tContiene la contrafirma de la firma \n";
			}

		}
		return attributos;
	}
	
	/**
	 * Obtiene los datos de cifrado usados.
	 *
	 * @param datos     informacion de los datos cifrados sin formatear.
	 * @return          informacion de los datos cifrados.
	 */
	public static String getEncryptedContentInfo(EncryptedContentInfo datos){
		String info = "";

		//especificamos el tipo de contenido
		if (datos.getContentType().equals(PKCSObjectIdentifiers.encryptedData)){
			info = info +"\tTipo: EncryptedData\n";
		}
		else{
			info = info +"\tTipo: "+datos.getContentType()+"\n";
		}

		// el algoritmo de cifrado de los datos
		AlgorithmIdentifier ai =datos.getContentEncryptionAlgorithm();
		AOCipherAlgorithm algorithm = null;
		AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

		// obtenemos el algoritmo usado para cifrar la pass
		for (int i=0;i<algos.length;i++){
			if (algos[i].getOid().equals(ai.getAlgorithm().toString())){
				algorithm = algos[i];
			}
		}

		if (algorithm != null){
			info = info +"\tAlgoritmo de cifrado: " + algorithm.getName() + "\n";
		}
		else {
			info = info +"\tOID del Algoritmo de cifrado: " + ai.getAlgorithm().toString() + "\n";
		}

		return info;
	}

	/**
	 * M&eacute;todo que obtiene el EncriptedContentInfo a partir del archivo
	 * a cifrar. El contenido es el siguiente:
	 * <pre><code>
	 * EncryptedContentInfo ::= SEQUENCE {
	 *     contentType ContentType,
	 *     contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
	 *     encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL
	 * }
	 * </code></pre>
	 *
	 * @param file Archivo a cifrar.
	 * @param config Configuracion de la clave de cifrado
	 * @param cipherKey 	Clave de cifrado
	 * @return Un sistema EncryptedContentInfo.
	 *
	 * @throws java.security.NoSuchAlgorithmException
	 * @throws javax.crypto.NoSuchPaddingException
	 * @throws java.security.InvalidAlgorithmParameterException
	 * @throws java.security.InvalidKeyException
	 * @throws java.io.IOException
	 * @throws org.bouncycastle.cms.CMSException
	 */
	public static EncryptedContentInfo getEncryptedContentInfo(byte[] file, AOCipherConfig config, SecretKey cipherKey)
	throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidAlgorithmParameterException, InvalidKeyException, IOException {

		AlgorithmParameterSpec params = getParams(config);
		Cipher cipher = createCipher(config.toString());
		cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
		return getEncryptedContentInfo(file, config, params, cipher);
	}
	
	/**
	 * Obtiene el contenido de un archivo encriptado
	 * @param file		Archivo con los datos
	 * @param config	Configuracion de cifrado
	 * @param params	Parametros
	 * @param cipher	Encriptador
	 * @return
	 * @throws IOException
	 */
	private static EncryptedContentInfo getEncryptedContentInfo(byte[] file,
			AOCipherConfig config, AlgorithmParameterSpec params, Cipher cipher)
	throws IOException {
		byte[] ciphered = null;
		try {
			ciphered = cipher.doFinal(file);
		} catch (IllegalBlockSizeException ex) {
			Logger.getLogger("es.gob.afirma").severe(ex.toString()); //$NON-NLS-1$
		} catch (BadPaddingException ex) {
			Logger.getLogger("es.gob.afirma").severe(ex.toString()); //$NON-NLS-1$
		}

		DEREncodable asn1Params;
		if (params != null){
			ASN1InputStream aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1"));
			asn1Params = aIn.readObject();
		}
		else{
			asn1Params = new DERNull();
		}

		// obtenemos el OID del algoritmo de cifrado
		AlgorithmIdentifier  encAlgId = new AlgorithmIdentifier(
				new DERObjectIdentifier(config.getAlgorithm().getOid()),
				asn1Params);

		// Obtenemos el identificador
		DERObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
		return new EncryptedContentInfo(
				contentType,
				encAlgId,
				new DEROctetString(ciphered)
		);
	}
	
	/**
	 * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
	 * cifrar dicho fichero.
	 *
	 * @param algName algoritmo utilizado para cifrar.
	 * @param provider  Proveedor que se utiliza para cifrar.
	 * @throws java.security.NoSuchAlgorithmException
	 * @throws javax.crypto.NoSuchPaddingException
	 */
	private static Cipher createCipher(String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
		return Cipher.getInstance(algName);
	}
	
	/**
	 * Genera los par&aacute;metros necesarios para poder operar con una configuracion concreta de cifrado.
	 * Si no es necesario ning&uacute;n par&aacute;metro especial, devolvemos <code>null</code>.
	 * @param algorithmConfig Configuracion de cifrado que debemos parametrizar.
	 * @return Par&aacute;metros para operar.
	 */
	public static AlgorithmParameterSpec getParams(AOCipherConfig algorithmConfig) {

		AlgorithmParameterSpec params = null;
		if(algorithmConfig.getAlgorithm().supportsPassword()) 
			params = new PBEParameterSpec(SALT, ITERATION_COUNT);
		else {
			if(!algorithmConfig.getBlockMode().equals(CipherConstants.AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
						algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}

		return params;
	}
	
	/**
	 * Vector de inicializacion de 8 bytes. Un vector de inicializaci&oacute;n
	 * de 8 bytes es necesario para el uso de los algoritmos DES y DESede.
	 */
	private static final byte[] IV_8 = {
		(byte)0xC6, (byte)0xBA, (byte)0xDE, (byte)0xA4,
		(byte)0x76, (byte)0x43, (byte)0x32, (byte)0x6B
	};

	/**
	 * Vector de inicializacion de 16 bytes. Un vector de inicializaci&oacute;n
	 * de 16 bytes es necesario para el uso de los algoritmos DES y DESede.
	 */
	private static final byte[] IV_16 = {
		(byte)0xB2, (byte)0xBA, (byte)0xDE, (byte)0xA4,
		(byte)0x41, (byte)0x7F, (byte)0x97, (byte)0x4B,
		(byte)0xAC, (byte)0x63, (byte)0xAC, (byte)0xAA,
		(byte)0x76, (byte)0x73, (byte)0x12, (byte)0x6B
	};
}