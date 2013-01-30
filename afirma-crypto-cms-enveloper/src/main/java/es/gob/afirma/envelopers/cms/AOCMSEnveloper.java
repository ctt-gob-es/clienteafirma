/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.security.spec.InvalidKeySpecException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.envelopers.AOEnveloper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;


/** Funcionalidad de sobres digitales con CAdES. */
public class AOCMSEnveloper implements AOEnveloper {


    /** Envoltorio binario de tipo Data (datos envueltos en un envoltorio
     * PKCS#7). */
    public static final String CMS_CONTENTTYPE_DATA = "Data"; //$NON-NLS-1$

    /** Firma binaria de tipo Signed Data */
    public static final String CMS_CONTENTTYPE_SIGNEDDATA = "SignedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Digest. */
    public static final String CMS_CONTENTTYPE_DIGESTEDDATA = "DigestedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_COMPRESSEDDATA = "CompressedData"; //$NON-NLS-1$

    /** Firma binaria de tipo Encrypted Data */
    public static final String CMS_CONTENTTYPE_ENCRYPTEDDATA = "EncryptedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Enveloped (sobre digital). */
    public static final String CMS_CONTENTTYPE_ENVELOPEDDATA = "EnvelopedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Signed and Enveloped. */
    public static final String CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA = "SignedAndEnvelopedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedData. */
    public static final String CMS_CONTENTTYPE_AUTHENTICATEDDATA = "AuthenticatedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_AUTHENVELOPEDDATA = "AuthEnvelopedData"; //$NON-NLS-1$

    /** Envoltorio binario por defecto. */
    public static final String DEFAULT_CMS_CONTENTTYPE = CMS_CONTENTTYPE_ENVELOPEDDATA;

  //TODO
    /** M&eacute;todo para la generaci&oacute;n de envolturas de datos. Los tipos de
     * envoltura definidos para CMS son: <br/>
     * <ul>
     * <li>Data</li>
     * <li>Signed Data</li>
     * <li>Digested Data</li>
     * <li>Enveloped Data</li>
     * <li>Signed and Enveloped Data</li>
     * </ul>
     * Para la generaci&oacute;n de la clave interna se utiliza por defecto el
     * algoritmo AES.
     * En el caso de que sea tipo "Enveloped data" o
     * "Signed and enveloped data", la clave se generar&aacute; usando el
     * algoritmo pasado como par&aacute;metro. Dicha clave se cifrar&aacute;
     * despu&eacute;s con la clave p&uacute;blica del certificado que identifica
     * al usuario destinatario.
     * Nota: El par&aacute;metro algorithm no es el agoritmo de cifrado, es para
     * el digestAlgorithm usado en los "Unsigned Attributes".
     * @param data
     *        Datos que se desean envolver.
     * @param digestAlgorithm
     *        Algoritmo a usar para la envoltura (SHA1withRSA, MD5withRSA,...)
     * @param type
     *        Tipo de envoltura que se quiere hacer.
     * @param keyEntry
     *        Clave privada a usar para firmar.
     * @param certDest
     *        Certificados de los usuarios a los que va destinado el sobre
     *        digital.
     * @param cipherAlgorithm
     *        Algoritmo utilizado para cifrar
     * @param extraParams
     *        Par&aacute;metros adicionales
     * @return Envoltorio CADES.
     * @throws AOException
     *         Cuando ocurre cualquier problema en el proceso. */
    @Override
	public byte[] envelop(final byte[] data,
                          final String digestAlgorithm,
                          final String type,
                          final PrivateKeyEntry keyEntry,
                          final X509Certificate[] certDest,
                          final AOCipherAlgorithm cipherAlgorithm,
                          final String dataType,
                          final Properties extraParams) throws AOException {



        return null;
    }


    //TODO
    /** Cifra datos usando para ello una clave de cifrado.<br/>
     * Se usar&aacute; por defecto el algoritmo de cifrado "AES".
     * La clave usada para cifrar el contenido puede ser tanto un password como
     * una clave privada codificada.
     * En el caso de que sea una clave codificada en base 64, se usar&aacute;
     * como algoritmos los tipo AES, DES ... En el caso de que sea un password,
     * se usar&aacute; un algoritmo de tipo PBE.
     * Nota: El digestAlgorithm no es el agoritmo de cifrado, es el algoritmo de
     * huella digital usado en los "Unsigned Attributes".
     * @param data
     *        Datos a envolver.
     * @param digestAlgorithm
     *        Algoritmo a usar para la envoltura (SHA1withRSA, MD5withRSA,...)
     * @param key
     *        Puede ser una clave codificada o una contrase&ntilde;a usada
     *        para cifrar el contenido.
     * @param cipherAlgorithm
     *        Algoritmo a usar para los cifrados
     * @param dataType OID del tipo de datos a encriptar
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] encrypt(final byte[] data,
			              final String digestAlgorithm,
			              final String key,
			              final AOCipherAlgorithm cipherAlgorithm,
			              final String dataType) throws AOException {

        // Comprobamos que el archivo a cifrar no sea nulo.
        if (data == null) {
            throw new IllegalArgumentException("Los datos a cifrar no pueden ser nulo."); //$NON-NLS-1$
        }
        return null;
    }


    /** Tipo de los datos contenidos en la envoltura. Siempre data por
     * est&aacute;ndar. */
    private static final String DATA_TYPE_OID = PKCSObjectIdentifiers.data.getId();

    /** Algoritmo de firma. */
    private String signatureAlgorithm = AOSignConstants.DEFAULT_SIGN_ALGO;

    /** Atributos firmados que se desean agregar a los envoltorios firmados. */
    private final Map<String, byte[]> attrib = new HashMap<String, byte[]>();

    /** Atributos que no requieren firma y se desean agregar a todos los
     * envoltorios que los soporten. */
    private final Map<String, byte[]> uattrib = new HashMap<String, byte[]>();

    /** Clave para el descifrado de los datos de un envoltorio EncryptedData. Si
     * se utiliza un algoritmo PBE de cifrado, ser&aacute; una contrase&ntilde;a
     * en texto plano. Si es otro algoritmo ser&aacute; su clave en base 64. */
    private String cipherKey = null;

    /** Configura un atributo firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del objeto a introducir.
     * @param value
     *        Valor asignado */
    public void addSignedAttribute(final String oid, final byte[] value) {
        this.attrib.put(oid, value);
    }

    /** Configura un atributo no firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del atributo a introducir.
     * @param value
     *        Valor asignado */
    public void addUnsignedAttribute(final String oid, final byte[] value) {
        this.uattrib.put(oid, value);
    }

    /** Crea un envoltorio CMS de tipo Data.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio Data.
     * @throws IOException */
    static byte[] createCMSData(final byte[] content) throws IOException {
		return CMSData.genData(content);
    }

    /** Crea un envoltorio CMS de tipo DigestedData.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio DigestedData.
     * @throws IOException
     *         Error en la lectura de datos.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado. */
    byte[] createCMSDigestedData(final byte[] content) throws IOException, NoSuchAlgorithmException {
		return CMSDigestedData.genDigestedData(content, this.signatureAlgorithm, DATA_TYPE_OID);
    }

    /** Crea un envoltorio CMS de tipo CompressedData.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio Compressed Data.
     * @throws IOException */
    static byte[] createCMSCompressedData(final byte[] content) throws IOException {
		return CMSCompressedData.genCompressedData(content);
    }

    /** Crea un envoltorio CMS de tipo EncryptedData.
     * @param content
     *        Contenido a envolver
     * @param cipherConfig
     *        Configuraci&oacute;n del cifrado del envoltorio
     * @param key
     *        Clave de envoltura
     * @return Envoltorio EncryptedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     * 		   Cuando se produce algun error al codificar los datos.
     */
    public byte[] createCMSEncryptedData(final byte[] content, final AOCipherConfig cipherConfig, final Key key) throws NoSuchAlgorithmException, IOException {
		return CMSEncryptedData.genEncryptedData(content, this.signatureAlgorithm, cipherConfig, key, DATA_TYPE_OID, this.uattrib);
    }

    /** Crea un envoltorio CMS de tipo EnvelopedData.
     * @param content
     *        Contenido que se desea ensobrar.
     * @param ke
     *        Clave privada del remitente (s&oacute;lo si se quiere indicar
     *        remitente).
     * @param cipherConfig
     *        Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts
     *        Destinatarios del sobre electr&oacute;nico.
     * @return Envoltorio EnvelopedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     *         Error en la escritura de datos.
     * @throws CertificateEncodingException
     *         Cuando el certificado del remitente no es v&aacute;lido.
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchPaddingException
     * @throws InvalidKeyException
     */
    public byte[] createCMSEnvelopedData(final byte[] content,
                                         final PrivateKeyEntry ke,
                                         final AOCipherConfig cipherConfig,
                                         final X509Certificate[] recipientsCerts) throws NoSuchAlgorithmException,
                                                                                         CertificateEncodingException,
                                                                                         IOException,
                                                                                         AOException,
                                                                                         InvalidKeyException,
                                                                                         NoSuchPaddingException,
                                                                                         InvalidAlgorithmParameterException,
                                                                                         IllegalBlockSizeException,
                                                                                         BadPaddingException {

        // Si se establecion un remitente
        if (ke != null) {
            return new CMSEnvelopedData().genEnvelopedData(
            		AOCMSEnveloper.createContentSignerParementers(content, ke, this.signatureAlgorithm),
                    cipherConfig,
                    recipientsCerts,
                    DATA_TYPE_OID,
                    this.uattrib
            );
        }

        // Si no se establecio remitente
        return new CMSEnvelopedData().genEnvelopedData(content, this.signatureAlgorithm, cipherConfig, recipientsCerts, DATA_TYPE_OID, this.uattrib);
    }

    /** Crea un envoltorio CMS de tipo SignedAndEnvelopedData.
     * @param content
     *        Contenido que se desea ensobrar.
     * @param ke
     *        Clave privada del remitente.
     * @param cipherConfig
     *        Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts
     *        Destinatarios del sobre electr&oacute;nico.
     * @return Envoltorio SignedAndEnvelopedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     *         Error en la escritura de datos.
     * @throws CertificateEncodingException
     *         Cuando el certificado del remitente no es v&aacute;lido.
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     * @throws SignatureException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchPaddingException
     * @throws InvalidKeyException
     */
    public byte[] createCMSSignedAndEnvelopedData(final byte[] content,
                                                  final PrivateKeyEntry ke,
                                                  final AOCipherConfig cipherConfig,
                                                  final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                                  NoSuchAlgorithmException,
                                                                                                  IOException,
                                                                                                  AOException,
                                                                                                  InvalidKeyException,
                                                                                                  NoSuchPaddingException,
                                                                                                  InvalidAlgorithmParameterException,
                                                                                                  IllegalBlockSizeException,
                                                                                                  BadPaddingException,
                                                                                                  SignatureException {
        return new CMSSignedAndEnvelopedData().genSignedAndEnvelopedData(
        	 AOCMSEnveloper.createContentSignerParementers(content, ke, this.signatureAlgorithm),
             cipherConfig,
             recipientsCerts,
             DATA_TYPE_OID,
             ke,
             this.attrib,
             this.uattrib
        );
    }

    /** Crea un envoltorio CMS de tipo AuthenticatedData.
     * @param content
     *        Contenido que se desea ensobrar.
     * @param ke
     *        Clave privada del remitente.
     * @param cipherConfig
     *        Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts
     *        Destinatarios del sobre electr&oacute;nico.
     * @return Envoltorio AuthenticatedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     *         Error en la escritura de datos.
     * @throws CertificateEncodingException
     *         Cuando el certificado del remitente no es v&aacute;lido.
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     * @throws InvalidKeyException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchPaddingException
     */
    byte[] createCMSAuthenticatedData(final byte[] content,
    		                          final PrivateKeyEntry ke,
    		                          final AOCipherConfig cipherConfig,
    		                          final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                      NoSuchAlgorithmException,
                                                                                      IOException,
                                                                                      AOException,
                                                                                      InvalidKeyException,
                                                                                      NoSuchPaddingException,
                                                                                      InvalidAlgorithmParameterException,
                                                                                      IllegalBlockSizeException,
                                                                                      BadPaddingException {
		return CMSAuthenticatedData.genAuthenticatedData(
        		AOCMSEnveloper.createContentSignerParementers(content, ke, this.signatureAlgorithm), // ContentSignerParameters
                null, // Algoritmo de autenticacion (usamos el por defecto)
                cipherConfig, // Configuracion del cipher
                recipientsCerts, // certificados destino
                DATA_TYPE_OID, // dataType
                true, // applySigningTime,
                this.attrib, // atributos firmados
                this.uattrib // atributos no firmados
        );
    }

    /** Crea un envoltorio CMS de tipo AuthenticatedEnvelopedData.
     * @param content
     *        Contenido que se desea ensobrar.
     * @param ke
     *        Clave privada del remitente.
     * @param cipherConfig
     *        Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts
     *        Destinatarios del sobre electr&oacute;nico.
     * @return Envoltorio AuthenticatedEnvelopedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     *         Error en la escritura de datos.
     * @throws CertificateEncodingException
     *         Cuando el certificado del remitente no es v&aacute;lido.
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     * @throws InvalidKeyException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchPaddingException
     */
    public byte[] createCMSAuthenticatedEnvelopedData(final byte[] content,
                                                      final PrivateKeyEntry ke,
                                                      final AOCipherConfig cipherConfig,
                                                      final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                                      NoSuchAlgorithmException,
                                                                                                      IOException,
                                                                                                      AOException,
                                                                                                      InvalidKeyException,
                                                                                                      NoSuchPaddingException,
                                                                                                      InvalidAlgorithmParameterException,
                                                                                                      IllegalBlockSizeException,
                                                                                                      BadPaddingException {
		return CMSAuthenticatedEnvelopedData.genAuthenticatedEnvelopedData(
				AOCMSEnveloper.createContentSignerParementers(content, ke, this.signatureAlgorithm), // ContentSignerParameters
                null, // Algoritmo de autenticacion (usamos el por defecto)
                cipherConfig, // Configuracion del cipher
                recipientsCerts, // certificados destino
                DATA_TYPE_OID, // dataType
                true, // applySigningTime,
                this.attrib, // atributos firmados
                this.uattrib // atributos no firmados
        );
    }

    /** Genera el bloque de datos con la informaci&oacute;n del remitente de un
     * mensaje.
     * @param content
     *        Mensaje.
     * @param ke
     *        Clave privada del remitente.
     * @param digestAlgorithm
     *        Algoritmo de huella digital.
     * @return Bloque de datos con la informaci&oacute;n del remitente. */
    private static P7ContentSignerParameters createContentSignerParementers(final byte[] content, final PrivateKeyEntry ke, final String digestAlgorithm) {
        return new P7ContentSignerParameters(content, digestAlgorithm, (X509Certificate[]) ke.getCertificateChain());
    }

    /** Agrega un nuevo remitente a un envoltorio CMS compatible.
     * @param envelop
     *        Envoltorio original.
     * @param ke
     *        Referencia a la clave privada del certificado del remitente.
     * @return Envoltorio con el nuevo remitente.
     * @throws AOException
     *         Cuando se produce un error al agregar el nuevo remitente.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
     * @throws CertificateEncodingException Cuando el certificado del remitente es inv&aacute;lido
     * @throws AOInvalidFormatException
     *         Tipo de envoltorio no soportado. */
    public static byte[] addOriginator(final byte[] envelop, final PrivateKeyEntry ke) throws AOException, IOException, CertificateEncodingException {
        final String contentInfo;
        if (ValidateCMS.isCMSEnvelopedData(envelop)) {
            contentInfo = CMS_CONTENTTYPE_ENVELOPEDDATA;
        }
        else if (ValidateCMS.isCMSSignedAndEnvelopedData(envelop)) {
            contentInfo = CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA;
        }
        else if (ValidateCMS.isCMSAuthenticatedEnvelopedData(envelop)) {
            contentInfo = CMS_CONTENTTYPE_AUTHENVELOPEDDATA;
        }
        else {
            throw new AOInvalidFormatException("Los datos proporcionado no son un envoltorio que soporte multiples remitentes"); //$NON-NLS-1$
        }
        return addOriginator(envelop, contentInfo, ke);
    }

    /** Agrega los datos de un remitente adicional a un envoltorio compatible.
     * Los envoltorios que admiten m&aacute;s de un remitente son:
     * <ul>
     * <li>Enveloped Data</li>
     * <li>Authenticated Data</li>
     * <li>Authenticated And Enveloped Data</li>
     * <li>Signed And Enveloped Data</li>
     * </ul>
     * @param envelop
     *        Estructura a la que se le desea agregar un remitente.
     * @param contentInfo
     *        Tipo de contenido que se desea envolver.
     * @param ke
     *        Referencia a la clave privada del certificado del remitente.
     * @throws AOException
     *         Cuando ocurrio un error al agregar el remitente a la
     *         estructura.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
     * @throws CertificateEncodingException Si el certificado del remitente es inv&aacute;lido
     * @throws IllegalArgumentException
     *         Cuando se indica un contentInfo no compatible con
     *         m&uacute;tiples remitentes. */
    private static byte[] addOriginator(final byte[] envelop, final String contentInfo, final PrivateKeyEntry ke) throws AOException, IOException, CertificateEncodingException {

        byte[] newEnvelop;

        if (contentInfo.equals(CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            newEnvelop = CMSEnvelopedData.addOriginatorInfo(envelop, (X509Certificate[]) ke.getCertificateChain());
        }
        else if (contentInfo.equals(CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            newEnvelop = new AOCMSSigner().cosign(envelop, AOSignConstants.DEFAULT_SIGN_ALGO, ke, null);
        }
        else if (contentInfo.equals(CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
            newEnvelop = CMSAuthenticatedEnvelopedData.addOriginatorInfo(envelop, (X509Certificate[]) ke.getCertificateChain());

        }
        else {
            throw new IllegalArgumentException("La estructura para el ContentInfo indicado no esta soportada o no admite multiples remitentes"); //$NON-NLS-1$
        }

        if (newEnvelop == null) {
            throw new AOException("Error al agregar el nuevo remitente al envoltorio"); //$NON-NLS-1$
        }

        return newEnvelop;
    }

    /** Algoritmo de firma que se utilizar&aacute; internamente en el sobre. El
     * algoritmo de huella digital se tomar&aacute; de este.
     * @param algorithm
     *        Algoritmo de firma. */
    public void setSignatureAlgorithm(final String algorithm) {
        this.signatureAlgorithm = algorithm == null ? AOSignConstants.DEFAULT_SIGN_ALGO : algorithm;
    }

    /** Establece la contrase&ntilde;a o clave para la encriptaci&oacute;n de los
     * datos.
     * @param keyPass
     *        Clave en base 64 o contrase&ntilda;a de cifrado. */
    public void setCipherKey(final String keyPass) {
        this.cipherKey = keyPass;
    }

    /** Recupera el algoritmo de firma configurado. El algoritmo de huella
     * digital se tomar&aacute; de este.
     * @return Cadena de texto identificativa para el algoritmo de firma */
    String getSignatureAlgorithm() {
        return this.signatureAlgorithm;
    }

    /** Recupera la clave o contrascontrase&ntilde;a para la encriptaci&oacute;n
     * de los datos.
     * @return Clave en base 64 o contrase&ntilda;a de cifrado. */
    String getCipherKey() {
        return this.cipherKey;
    }

    /** Recupera el contenido de un envoltorio CMS.
     * @param cmsEnvelop
     *        Envoltorio CMS.
     * @param addresseePke
     *        Clave privada del destinatario que desea desensobrar.
     * @return Contenido del envoltorio.
     * @throws InvalidKeyException
     *         Cuando la clave de descifrado configurada no sea v&aacute;lida o pertenezca a un destinatario.
     * @throws AOException
     *         Cuando se produce un error durante al desenvolver los datos.
     * @throws InvalidKeySpecException */
    @Override
	public byte[] recoverData(final byte[] cmsEnvelop,
			                  final PrivateKeyEntry addresseePke) throws InvalidKeyException,
			                                                             AOException,
			                                                             IOException,
			                                                             InvalidKeySpecException {

    	final org.bouncycastle.asn1.ASN1InputStream is = new org.bouncycastle.asn1.ASN1InputStream(cmsEnvelop);

    	// Leemos los datos
    	final org.bouncycastle.asn1.ASN1Sequence dsq = (org.bouncycastle.asn1.ASN1Sequence) is.readObject();
		is.close();

    	final Enumeration<?> objects = dsq.getObjects();

    	// Elementos que contienen los elementos OID Data
    	final org.bouncycastle.asn1.DERObjectIdentifier doi = (org.bouncycastle.asn1.DERObjectIdentifier) objects.nextElement();

    	byte[] datos;
    	try {
    		if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.data)) {
    			Logger.getLogger("es.gob.afirma").warning("La extraccion de datos de los envoltorios CMS Data no esta implementada"); //$NON-NLS-1$ //$NON-NLS-2$
    			datos = null;
    		}
    		else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.digestedData)) {
    			Logger.getLogger("es.gob.afirma").warning("La extraccion de datos de los envoltorios CMS DigestedData no esta implementada"); //$NON-NLS-1$ //$NON-NLS-2$
    			datos = null;
    		}
    		else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.compressedData)) {
    			datos = AOCMSEnveloper.recoverCMSCompressedData(cmsEnvelop);
    		}
    		else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.encryptedData)) {
    			datos = AOCMSEnveloper.recoverCMSEncryptedData(cmsEnvelop, this.cipherKey);
    		}
    		else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.envelopedData)) {
    			datos = AOCMSEnveloper.recoverCMSEnvelopedData(cmsEnvelop, addresseePke);
    		}
    		else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authEnvelopedData)) {
    			datos = AOCMSEnveloper.recoverCMSAuthenticatedEnvelopedData(cmsEnvelop, addresseePke);
    		}
    		else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authenticatedData)) {
    			datos = AOCMSEnveloper.recoverCMSAuthenticatedData(cmsEnvelop, addresseePke);
    		}
    		else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.signedAndEnvelopedData)) {
    			datos = AOCMSEnveloper.recoverCMSSignedEnvelopedData(cmsEnvelop, addresseePke);
    		}
    		else {
    			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
    		}
    	}
    	catch (final AOInvalidRecipientException e) {
    		throw new InvalidKeyException("La clave indicada no pertenece a ninguno de los destinatarios del envoltorio", e); //$NON-NLS-1$
    	}
    	catch (final CertificateEncodingException e) {
    		throw new AOException("Error al descodificar los certificados del envoltorio", e); //$NON-NLS-1$
    	}
    	catch (final NoSuchAlgorithmException e) {
    		throw new AOException("No se reconoce el algoritmo indicado", e); //$NON-NLS-1$
    	}
    	catch (final NoSuchPaddingException e) {
    		throw new AOException("No se reconoce el tipo de relleno indicado", e); //$NON-NLS-1$
		}
    	catch (final InvalidAlgorithmParameterException e) {
    		throw new AOException("No se reconoce la configuracion del algoritmo indicado", e); //$NON-NLS-1$
		}
    	catch (final IllegalBlockSizeException e) {
    		throw new AOException("Tamano de bloque invalido: " + e, e); //$NON-NLS-1$
		}
    	catch (final BadPaddingException e) {
    		throw new AOException("relleno invalido: " + e, e); //$NON-NLS-1$
		}

    	return datos;
    }

    /** Recupera el contenido de un envoltorio CompressedData.
     * @param compressedData
     *        Envoltorio CMS de tipo CompressedData.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Cuando ocurre un error al descomprimir los datos. */
    static byte[] recoverCMSCompressedData(final byte[] compressedData) throws IOException {
		return CMSCompressedData.getContentCompressedData(compressedData);
    }

    /** Recupera el contenido de un envoltorio EncryptedData.
     * @param encryptedData
     *        Envoltorio CMS de tipo EncryptedData.
     * @param passkey
     *        Contrase&ntilde;a o clave (base64) necesaria para desencriptar
     *        los datos.
     * @return Contenido del envoltorio.
     * @throws InvalidKeyException
     *         Cuando la clave proporcionada no es v&aacute;lida.
     * @throws AOException
     *         Cuando se produce un error al desenvolver los datos.
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchPaddingException
     * @throws NoSuchAlgorithmException
     * @throws IOException
     * @throws InvalidKeySpecException */
    static byte[] recoverCMSEncryptedData(final byte[] encryptedData,
    		                              final String passkey) throws InvalidKeyException,
                                                                       AOException,
                                                                       NoSuchAlgorithmException,
                                                                       NoSuchPaddingException,
                                                                       InvalidAlgorithmParameterException,
                                                                       IllegalBlockSizeException,
                                                                       BadPaddingException,
                                                                       InvalidKeySpecException,
                                                                       IOException {
        return new CMSDecipherEncryptedData().dechiperEncryptedData(encryptedData, passkey);
    }

    /** Recupera el contenido de un envoltorio EnvelopedData.
     * @param envelopedData
     *        Envoltorio CMS de tipo EnvelopedData.
     * @param ke
     *        Clave de un destinatario del sobre.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException
     *         Cuando se indica un certificado que no est&aacute; entre los
     *         destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchPaddingException
     * @throws NoSuchAlgorithmException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException */
    static byte[] recoverCMSEnvelopedData(final byte[] envelopedData,
    		                              final PrivateKeyEntry ke) throws IOException,
                                                                           CertificateEncodingException,
                                                                           AOException,
                                                                           InvalidKeyException,
                                                                           NoSuchAlgorithmException,
                                                                           NoSuchPaddingException, InvalidAlgorithmParameterException, IllegalBlockSizeException, BadPaddingException {
		return CMSDecipherEnvelopData.dechiperEnvelopData(envelopedData, ke);
    }

    /** Recupera el contenido de un envoltorio SignedEnvelopedData.
     * @param signedEnvelopedData
     *        Envoltorio CMS de tipo SignedEnvelopedData.
     * @param ke
     *        Clave de un destinatario del sobre.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException
     *         Cuando se indica un certificado que no est&aacute; entre los
     *         destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchPaddingException
     * @throws NoSuchAlgorithmException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException */
    static byte[] recoverCMSSignedEnvelopedData(final byte[] signedEnvelopedData,
    		                                    final PrivateKeyEntry ke) throws IOException,
                                                                                 CertificateEncodingException,
                                                                                 AOException,
                                                                                 InvalidKeyException,
                                                                                 NoSuchAlgorithmException,
                                                                                 NoSuchPaddingException,
                                                                                 InvalidAlgorithmParameterException,
                                                                                 IllegalBlockSizeException,
                                                                                 BadPaddingException {
		return CMSDecipherSignedAndEnvelopedData.dechiperSignedAndEnvelopData(signedEnvelopedData, ke);
    }

    /** Comprueba la integridad de un envoltorio AuthenticatedData y, si es
     * correcto, extrae su contenido.
     * @param authenticatedData
     *        Envoltorio CMS de tipo AuthenticatedData.
     * @param ke
     *        Clave de un destinatario del sobre.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws CertificateEncodingException
     *         Si el certificado proporcionado no se adec&uacute;a a la
     *         norma X.509v3
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchAlgorithmException Cuando no se reconoce el algoritmo
     *                                  utilizado para generar el c&oacute;digo de autenticaci&oacute;n.
     * @throws NoSuchPaddingException
     */
    static byte[] recoverCMSAuthenticatedData(final byte[] authenticatedData,
    		                                  final PrivateKeyEntry ke) throws IOException,
                                                                               CertificateEncodingException,
                                                                               AOException,
                                                                               InvalidKeyException,
                                                                               NoSuchAlgorithmException,
                                                                               NoSuchPaddingException {
        return new CMSDecipherAuthenticatedData().decipherAuthenticatedData(authenticatedData, ke);
    }

    /** Recupera el contenido de un envoltorio AuthenticatedEnvelopedData.
     * @param authenticatedEnvelopedData
     *        Envoltorio CMS de tipo AuthenticatedEnvelopedData.
     * @param ke
     *        Clave de un destinatario del sobre.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Si ocurre algun error gen&eacute;rico de entrada/salida
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException
     *         Cuando se indica un certificado que no est&aacute; entre los
     *         destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchPaddingException
     * @throws NoSuchAlgorithmException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws InvalidAlgorithmParameterException */
    static byte[] recoverCMSAuthenticatedEnvelopedData(final byte[] authenticatedEnvelopedData,
    												   final PrivateKeyEntry ke) throws IOException,
                                                                                        CertificateEncodingException,
                                                                                        AOException,
                                                                                        InvalidKeyException,
                                                                                        NoSuchAlgorithmException,
                                                                                        NoSuchPaddingException,
                                                                                        InvalidAlgorithmParameterException,
                                                                                        IllegalBlockSizeException,
                                                                                        BadPaddingException {
		return CMSDecipherAuthenticatedEnvelopedData.dechiperAuthenticatedEnvelopedData(authenticatedEnvelopedData, ke);
    }

    /** M&eacute;todo que comprueba que unos datos se corresponden con una
     * estructura CMS/PKCS#7. Se realiza la verificaci&ocute;n sobre los los
     * siguientes tipos reconocidos:
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
     * @param cmsData
     *        Datos que deseamos comprobar.
     * @return La validez del archivo cumpliendo la estructura. */
    public static boolean isCMSValid(final byte[] cmsData) {
        return CMSHelper.isCMSValid(cmsData);
    }

    /** M&eacute;todo que comprueba que unos datos se corresponden con una
     * estructura CMS/PKCS#7 concreta.
     * @param data
     *        Datos que deseamos comprobar.
     * @param type
     *        Tipo de contenido del envoltorio que queremos comprobar.
     * @return Indica los datos son una envoltura CMS con el tipo de contenido
     *         indicado. */
    public static boolean isCMSValid(final byte[] data, final String type) {
        return CMSHelper.isCMSValid(data, type);
    }
}
