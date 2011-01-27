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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.ietf.jgss.Oid;

import sun.misc.BASE64Decoder;
import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;


/**
 * Clase que implementa firma digital PKCS#7/CMS EncryptedData.
 * La Estructura del mensaje es la siguiente:<br>
 * <pre><code>
 *
 * id-encryptedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *         us(840) rsadsi(113549) pkcs(1) pkcs7(7) 6 }
 *
 * EncryptedData ::= SEQUENCE {
 *       version CMSVersion,
 *       encryptedContentInfo EncryptedContentInfo,
 *       unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
 *
 *</code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje EncryptedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public final class CMSEncryptedData extends SigUtils {

  /**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;

    private static final byte[] SALT = {
		(byte)0xA2, (byte)0x35, (byte)0xDC, (byte)0xA4,
		(byte)0x11, (byte)0x7C, (byte)0x99, (byte)0x4B
	};

    private static final int ITERATION_COUNT = 9;

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


    /**
     * M&eacute;todo principal que genera la firma de tipo EncryptedData.
     *
     * @param file      Archivo espec&iacute;fico a cifrar.
     * @param digAlg    Algoritmo para realizar el Digest.
     * @param config    Configuraci&oacute;n del algoritmo para cifrar.
     * @param pass      Cadena que se usar&aacute; para cifrar los datos.
     * @param dataType  Identifica el tipo del contenido a firmar.
     * @param uatrib    Conjunto de atributos no firmados.
     * 
     * @return          la firma de tipo EncryptedData.
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
     */
    public byte[] genEncryptedData(InputStream file, String digAlg, AOAlgorithmConfig config, String pass, Oid dataType, HashMap<Oid, byte[]> uatrib) throws NoSuchAlgorithmException {
        
        byte[] codeFile = createArrayFromFile(file);

        // Asignamos la clave de cifrado
        assignKey(config, pass);

         //Datos previos &uacute;tiles
        String digestAlgorithm =AOCryptoUtil.getDigestAlgorithmName(digAlg);

        //generamos el contenedor de cifrado
        EncryptedContentInfo encInfo = null;
        try {
            // 3.   ENCRIPTEDCONTENTINFO
            encInfo = getEncryptedContentInfo(codeFile,config);
        } catch (Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado: " + ex);
        }


      // 4. ATRIBUTOS
        // obtenemos la lista de certificados
        ASN1Set unprotectedAttrs = null;
        unprotectedAttrs = generateSignerInfo(digestAlgorithm, codeFile, dataType, uatrib);

     // construimos el Enveloped Data y lo devolvemos
     return new ContentInfo(
        	PKCSObjectIdentifiers.encryptedData,
        	new EncryptedData(
                encInfo,
                unprotectedAttrs
            )
        ).getDEREncoded();

     }



     /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del usuario.
     *  Se generan los atributos que se necesitan para generar la firma.
     *
     * @param digestAlgorithm Identifica el algoritmo utilizado firmado.
     * @param datos             Datos firmados.
     * @param dataType          Identifica el tipo del contenido a firmar.
     * @param uatrib            Conjunto de atributos no firmados.
     *
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException
     */
    private ASN1Set generateSignerInfo(String digestAlgorithm,
                            byte[] datos,
                            Oid dataType,
                            HashMap<Oid, byte[]> uatrib)
                        throws NoSuchAlgorithmException {

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        //tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        //MessageDigest
        ContexExpecific.add(
            new Attribute(
            	CMSAttributes.messageDigest,
                new DERSet(
                	new DEROctetString(
                		MessageDigest.getInstance(digestAlgorithm.toString()).digest(datos)
                	)
                )
            )
        );

        //agregamos la lista de atributos a mayores.
        if (uatrib.size()!=0){
        	Iterator<Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
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
        else{
            return null;
        }


     return getAttributeSet(new AttributeTable(ContexExpecific));

    }

    /*************************************************************************/
    /**************** Metodos auxiliares de cifrado **************************/
    /*************************************************************************/

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
     * @return Un sistema EncryptedContentInfo.
     *
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException
     */
    private EncryptedContentInfo getEncryptedContentInfo(byte[] file,AOAlgorithmConfig config) throws NoSuchProviderException, NoSuchAlgorithmException, NoSuchPaddingException, InvalidAlgorithmParameterException, InvalidKeyException, IOException, IllegalBlockSizeException, BadPaddingException{

        AlgorithmParameterSpec params = this.getParams(config);
        Cipher cipher = createCipher(config.toString());
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
        byte [] ciphered = cipher.doFinal(file);

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
     * Asigna la clave para firmar el contenido del fichero que queremos envolver
     * y qeu m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica del usuario que
     * hace la firma.
     *
     * @param config Configuraci&oacute;n necesaria para crear la clave.
     * @param key    Contrase&ntilde;a que se va a usar para cifrar.
     */
    private void assignKey(AOAlgorithmConfig config, String key){
        
        // Generamos la clave necesaria para el cifrado
        if ((config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) ||
                (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE)) ||
                (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))){
            try {
             this.cipherKey = SecretKeyFactory.getInstance(config.getAlgorithm().getName())
				.generateSecret(new PBEKeySpec(key.toCharArray(), SALT, ITERATION_COUNT));
            } catch (Exception ex) {
                Logger.getLogger("es.gob.afirma").severe("Ocurrio un error durante el proceso de asignacion de la clave (a partir de password): " + ex);
            }
        }
        else{
            try {
                this.cipherKey = new SecretKeySpec(new BASE64Decoder().decodeBuffer(key), config.getAlgorithm().getName());
            } catch (IOException ex) {
               Logger.getLogger("es.gob.afirma").severe("Ocurrio un error durante el proceso de asignacion de la clave (a partir de key): " + ex);
            }
        }

		
    }

//    /**
//     * Genera el proveedor de cifrado.
//     *
//     * @param providerName  Nombre del proveedor.
//     * @return  El proveedor.
//     * @throws java.security.NoSuchProviderException
//     */
//    private static Provider getProvider(String providerName)
//        throws NoSuchProviderException
//    {
//        if (providerName != null)
//        {
//            Provider prov = Security.getProvider(providerName);
//            if (prov != null)
//            {
//                return prov;
//            }
//            throw new NoSuchProviderException("provider " + providerName + " not found.");
//        }
//        return null;
//    }

    /**
     * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
     * cifrar dicho fichero.
     *
     * @param algName algoritmo utilizado para cifrar.
     * @param provider  Proveedor que se utiliza para cifrar.
     * @return Cifrador.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     */
     private Cipher createCipher(String algName) throws NoSuchAlgorithmException, NoSuchPaddingException
    {
        return Cipher.getInstance(algName);
    }

    /**
	 * Genera los par&aacute;metros necesarios para poder operar con una configuracion concreta de cifrado.
	 * Si no es necesario ning&uacute;n par&aacute;metro especial, devolvemos <code>null</code>.
	 * @param algorithmConfig Configuracion de cifrado que debemos parametrizar.
	 * @return Par&aacute;metros para operar.
	 */
	private AlgorithmParameterSpec getParams(AOAlgorithmConfig algorithmConfig) {

		AlgorithmParameterSpec params = null;
		if(algorithmConfig.getAlgorithm().supportsPassword()) {
			params = new PBEParameterSpec(SALT, ITERATION_COUNT);
		} else {
			if(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
						algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}

		return params;
	}


    /**
     * M&eacute;todo que transforma un Inputstream en un array de bytes.
     * @param file  InputStream a ser transformado.
     * @return      fichero en formato array de bytes.
     */
    private byte[] createArrayFromFile(InputStream file) {

        ByteArrayOutputStream baos=null;
        try{

            InputStream dataReader = file;

            byte[] buffer = new byte[1024];
            int len;
            baos = new ByteArrayOutputStream();

            while (dataReader.available() != 0) {
                len = dataReader.read(buffer);
                baos.write(buffer, 0, len);
            }

        }catch (Exception ex){
            Logger.getLogger("es.gob.afirma").severe("Ocurrio un error durante el proceso de lectura del fichero: " + ex);
        }

        return baos.toByteArray();

    }

}

