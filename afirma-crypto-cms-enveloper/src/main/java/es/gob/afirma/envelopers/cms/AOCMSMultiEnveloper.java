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
import java.util.zip.DataFormatException;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cms.AOCMSExtraParams;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;


/** Funcionalidad de sobres digitales con CAdES. */
public class AOCMSMultiEnveloper {

    private String dataTypeOID = null;
    private final Map<String, byte[]> atrib = new HashMap<>();
    private final Map<String, byte[]> uatrib = new HashMap<>();

  /** Cofirma un sobre digital CMS.
   * @param data Datos contenidos en el sobre digital a cofirmar
   * @param sign Sobre digital
   * @param algorithm Algoritmo de firma
   * @param keyEntry Entrada de clave privada a usar para la firma
   * @param xParams Par&aacute;metros adicionales. &Uacute;nicamente se lee <i>precalculatedHashAlgorithm</i>
   * @return Sobre digtal cofirmado
   * @throws AOException Si ocurre cualquier problema durante el proceso */
  public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        final String precalculatedDigest = xParams != null ? xParams.getProperty(AOCMSExtraParams.PRECALCULATED_HASH_ALGORITHM) : null;

        byte[] messageDigest = null;
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm);

        // tipos de datos a firmar.
        if (this.dataTypeOID == null) {
            this.dataTypeOID = PKCSObjectIdentifiers.data.getId();
        }

        // Si la firma que nos introducen es SignedAndEnvelopedData
        try {
            // El parametro omitContent no tiene sentido en un signed and
            // envelopedData.
            return new CoSignerEnveloped().coSigner(
        		csp,
        		(X509Certificate[]) keyEntry.getCertificateChain(),
        		sign,
        		this.dataTypeOID,
        		keyEntry,
        		this.atrib,
        		this.uatrib,
        		messageDigest
    		);
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma del sobre", e); //$NON-NLS-1$
        }
    }

    /** Cofirma un sobre digital CMS.
     * @param sign Sobre digital CMS ya firmado
     * @param algorithm Algoritmo de firma a usar
     * @param keyEntry ENtrada de clave privada para la firma
     * @return Sobre cofirmado
     * @throws AOException Si ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry) throws AOException {

        // tipos de datos a firmar.
        if (this.dataTypeOID == null) {
            this.dataTypeOID = PKCSObjectIdentifiers.data.getId();
        }

        // Cofirma de la firma usando unicamente el fichero de firmas.
        try {
            return new CoSignerEnveloped().coSigner(algorithm, (X509Certificate[]) keyEntry.getCertificateChain(), sign, this.dataTypeOID, keyEntry, this.atrib, this.uatrib, null);
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma PKCS#7", e); //$NON-NLS-1$
        }
    }

    /** Tipo de los datos contenidos en la envoltura. Siempre data por
     * est&aacute;ndar. */
    private static final String DATA_TYPE_OID = PKCSObjectIdentifiers.data.getId();

    /** Algoritmo de firma. */
    private String signatureAlgorithm = AOSignConstants.DEFAULT_SIGN_ALGO;

    /** Atributos firmados que se desean agregar a los envoltorios firmados. */
    private final Map<String, byte[]> attrib = new HashMap<>();

    /** Atributos que no requieren firma y se desean agregar a todos los
     * envoltorios que los soporten. */
    private final Map<String, byte[]> uattrib = new HashMap<>();

    /** Clave privada del usuario que genera o abre el envoltorio. */
    private PrivateKeyEntry configuredKe = null;

    /** Clave para el descifrado de los datos de un envoltorio EncryptedData. Si
     * se utiliza un algoritmo PBE de cifrado, ser&aacute; una contrase&ntilde;a
     * en texto plano. Si es otro algoritmo ser&aacute; su clave en base 64. */
    private String cipherKey = null;

    /** Configura un atributo firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del objeto a introducir.
     * @param value
     *        Valor asignado */
    void addSignedAttribute(final String oid, final byte[] value) {
        this.attrib.put(oid, value);
    }

    /** Configura un atributo no firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del atributo a introducir.
     * @param value
     *        Valor asignado */
    void addUnsignedAttribute(final String oid, final byte[] value) {
        this.uattrib.put(oid, value);
    }

    /** Crea un envoltorio CMS de tipo Data.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio Data.
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
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
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
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
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
    byte[] createCMSEncryptedData(final byte[] content, final AOCipherConfig cipherConfig, final Key key) throws NoSuchAlgorithmException, IOException {
		return CMSEncryptedData.genEncryptedData(content, this.signatureAlgorithm, cipherConfig, key, DATA_TYPE_OID, this.uattrib);
    }

    /** Crea un envoltorio CMS de tipo <code>EnvelopedData</code>.
     * @param content Contenido que se desea ensobrar.
     * @param ke Clave privada del remitente (s&oacute;lo si se quiere indicar
     *           remitente).
     * @param cipherConfig Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts Destinatarios del sobre electr&oacute;nico.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return Envoltorio EnvelopedData.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de cifrado indicado no est&aacute;
     *                                  soportado.
     * @throws IOException Cuando ocurre un error en la escritura de datos.
     * @throws CertificateEncodingException Cuando el certificado del remitente no es v&aacute;lido.
     * @throws BadPaddingException Si hay problemas estableciendo el relleno de los datos
     * @throws IllegalBlockSizeException Si no cuadran los tama&ntilde;os de bloque de los algoritmos usados
     * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
     *                                            para alg&uacute;n algoritmo
     * @throws NoSuchPaddingException Si no se soporta alg&uacute;n m&eacute;todo de relleno
     * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida */
    byte[] createCMSEnvelopedData(final byte[] content,
                                         final PrivateKeyEntry ke,
                                         final AOCipherConfig cipherConfig,
                                         final X509Certificate[] recipientsCerts,
                                         final Integer keySize) throws NoSuchAlgorithmException,
                                                                       CertificateEncodingException,
                                                                       IOException,
                                                                       InvalidKeyException,
                                                                       NoSuchPaddingException,
                                                                       InvalidAlgorithmParameterException,
                                                                       IllegalBlockSizeException,
                                                                       BadPaddingException {
        // Si se establecion un remitente
        if (ke != null) {
            return new CMSEnvelopedData().genEnvelopedData(
        		AOCMSMultiEnveloper.createContentSignerParamenters(content, this.signatureAlgorithm),
        		(X509Certificate[]) ke.getCertificateChain(),
                cipherConfig,
                recipientsCerts,
                DATA_TYPE_OID,
                this.uattrib,
                keySize
            );
        }

        // Si no se establecio remitente
        return new CMSEnvelopedData().genEnvelopedData(
    		content,
    		this.signatureAlgorithm,
    		cipherConfig,
    		recipientsCerts,
    		DATA_TYPE_OID,
    		this.uattrib,
    		keySize
		);
    }

    /** Crea un envoltorio CMS de tipo SignedAndEnvelopedData.
     * @param content Contenido que se desea ensobrar.
     * @param ke Clave privada del remitente.
     * @param cipherConfig Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts Destinatarios del sobre electr&oacute;nico.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return Envoltorio SignedAndEnvelopedData.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de cifrado indicado no est&aacute;
     *                                  soportado.
     * @throws IOException Error en la escritura de datos.
     * @throws CertificateEncodingException Cuando el certificado del remitente no es v&aacute;lido.
     * @throws SignatureException Cuando ocurren problemas en la firma PKCS#1.
     * @throws BadPaddingException Si hay problemas estableciendo el relleno de los datos
     * @throws IllegalBlockSizeException Si no cuadran los tama&ntilde;os de bloque de los algoritmos usados
     * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
     *                                            para alg&uacute;n algoritmo
     * @throws NoSuchPaddingException Si no se soporta alg&uacute;n m&eacute;todo de relleno
     * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida
     */
    byte[] createCMSSignedAndEnvelopedData(final byte[] content,
                                                  final PrivateKeyEntry ke,
                                                  final AOCipherConfig cipherConfig,
                                                  final X509Certificate[] recipientsCerts,
                                                  final Integer keySize) throws CertificateEncodingException,
                                                                                NoSuchAlgorithmException,
                                                                                IOException,
                                                                                InvalidKeyException,
                                                                                NoSuchPaddingException,
                                                                                InvalidAlgorithmParameterException,
                                                                                IllegalBlockSizeException,
                                                                                BadPaddingException,
                                                                                SignatureException {
        return new CMSSignedAndEnvelopedData().genSignedAndEnvelopedData(
    		AOCMSMultiEnveloper.createContentSignerParamenters(content, this.signatureAlgorithm),
    		(X509Certificate[]) ke.getCertificateChain(),
            cipherConfig,
            recipientsCerts,
            DATA_TYPE_OID,
            ke,
            this.attrib,
            this.uattrib,
            keySize
        );
    }

    /** Crea un envoltorio CMS de tipo AuthenticatedData.
     * @param content Contenido que se desea ensobrar.
     * @param ke Clave privada del remitente.
     * @param cipherConfig Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts Destinatarios del sobre electr&oacute;nico.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return Envoltorio AuthenticatedData.
     * @throws NoSuchAlgorithmException
     *         Cuando el algoritmo de cifrado indicado no est&aacute;
     *         soportado.
     * @throws IOException
     *         Error en la escritura de datos.
     * @throws CertificateEncodingException
     *         Cuando el certificado del remitente no es v&aacute;lido.
     * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida
     * @throws BadPaddingException Si hay problemas estableciendo el relleno de los datos
     * @throws IllegalBlockSizeException Si no cuadran los tama&ntilde;os de bloque de los algoritmos usados
     * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
     *                                            para alg&uacute;n algoritmo
     * @throws NoSuchPaddingException Si no se soporta alg&uacute;n m&eacute;todo de relleno */
    byte[] createCMSAuthenticatedData(final byte[] content,
    								  final PrivateKeyEntry ke,
    								  final AOCipherConfig cipherConfig,
    								  final X509Certificate[] recipientsCerts,
    								  final Integer keySize) throws CertificateEncodingException,
    																NoSuchAlgorithmException,
    																IOException,
    																InvalidKeyException,
    																NoSuchPaddingException,
    																InvalidAlgorithmParameterException,
    																IllegalBlockSizeException,
    																BadPaddingException {
    	return CMSAuthenticatedData.genAuthenticatedData(
    			AOCMSMultiEnveloper.createContentSignerParamenters(content, this.signatureAlgorithm), // ContentSignerParameters
    			(X509Certificate[]) ke.getCertificateChain(), // Certificados del firmante (remitente)
    			null, // Algoritmo de autenticacion (usamos el por defecto)
    			cipherConfig, // Configuracion del cipher
    			recipientsCerts, // certificados destino
    			DATA_TYPE_OID, // dataType
    			true, // applySigningTime,
    			this.attrib, // atributos firmados
    			this.uattrib, // atributos no firmados
    			keySize
    	);
    }

    /** Crea un envoltorio CMS de tipo AuthenticatedEnvelopedData.
     * @param content Contenido que se desea ensobrar.
     * @param ke Clave privada del remitente.
     * @param cipherConfig Configuraci&oacute;n para el cifrado de datos.
     * @param recipientsCerts Destinatarios del sobre electr&oacute;nico.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return Envoltorio AuthenticatedEnvelopedData.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de cifrado indicado no est&aacute;
     *                                  soportado.
     * @throws IOException Error en la escritura de datos.
     * @throws CertificateEncodingException Cuando el certificado del remitente no es v&aacute;lido.
     * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida
     * @throws BadPaddingException Si hay problemas estableciendo el relleno de los datos
     * @throws IllegalBlockSizeException Si no cuadran los tama&ntilde;os de bloque de los algoritmos usados
     * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
     *                                            para alg&uacute;n algoritmo
     * @throws NoSuchPaddingException Si no se soporta alg&uacute;n m&eacute;todo de relleno */
    byte[] createCMSAuthenticatedEnvelopedData(final byte[] content,
                                                      final PrivateKeyEntry ke,
                                                      final AOCipherConfig cipherConfig,
                                                      final X509Certificate[] recipientsCerts,
                                                      final Integer keySize) throws CertificateEncodingException,
                                                                                    NoSuchAlgorithmException,
                                                                                    IOException,
                                                                                    InvalidKeyException,
                                                                                    NoSuchPaddingException,
                                                                                    InvalidAlgorithmParameterException,
                                                                                    IllegalBlockSizeException,
                                                                                    BadPaddingException {
		return CMSAuthenticatedEnvelopedData.genAuthenticatedEnvelopedData(
			AOCMSMultiEnveloper.createContentSignerParamenters(content, this.signatureAlgorithm), // ContentSignerParameters
			(X509Certificate[]) ke.getCertificateChain(), // Certificados del firmante (remitente)
            null, // Algoritmo de autenticacion (usamos el por defecto)
            cipherConfig, // Configuracion del cipher
            recipientsCerts, // certificados destino
            DATA_TYPE_OID, // dataType
            true, // applySigningTime,
            this.attrib, // atributos firmados
            this.uattrib, // atributos no firmados
            keySize
        );
    }

    /** Genera el bloque de datos con la informaci&oacute;n del remitente de un
     * mensaje.
     * @param content Mensaje.
     * @param digestAlgorithm Algoritmo de huella digital.
     * @return Bloque de datos con la informaci&oacute;n del remitente. */
    private static P7ContentSignerParameters createContentSignerParamenters(final byte[] content,
    		                                                                final String digestAlgorithm) {
        return new P7ContentSignerParameters(content, digestAlgorithm);
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
     * @throws AOInvalidFormatException Si el tipo de envoltorio no est&aacute; soportado. */
    static byte[] addOriginator(final byte[] envelop, final PrivateKeyEntry ke) throws AOException, IOException, CertificateEncodingException {
        final String contentInfo;
        if (ValidateCMS.isCMSEnvelopedData(envelop)) {
            contentInfo = AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA;
        }
        else if (ValidateCMS.isCMSSignedAndEnvelopedData(envelop)) {
            contentInfo = AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA;
        }
        else if (ValidateCMS.isCMSAuthenticatedEnvelopedData(envelop)) {
            contentInfo = AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA;
        }
        else {
            throw new AOInvalidFormatException("Los datos proporcionado no son un envoltorio que soporte multiples remitentes"); //$NON-NLS-1$
        }
        return addOriginator(envelop, contentInfo, ke);
    }

    /** Agrega los datos de un remitente adicional a un envoltorio compatible.
     * Los envoltorios que admiten m&aacute;s de un remitente son:
     * <ul>
     *   <li>Enveloped Data</li>
     *   <li>Authenticated Data</li>
     *   <li>Authenticated And Enveloped Data</li>
     *   <li>Signed And Enveloped Data</li>
     * </ul>
     * @param envelop Estructura a la que se le desea agregar un remitente.
     * @param contentInfo Tipo de contenido que se desea envolver.
     * @param ke Referencia a la clave privada del certificado del remitente.
     * @return Sobre con el remitente a&ntilde;adido.
     * @throws AOException Cuando ocurre un error al agregar el remitente a la
     *                     estructura.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
     * @throws CertificateEncodingException Cuando el certificado del remitente es inv&aacute;lido
     * @throws IllegalArgumentException
     *         Cuando se indica un contentInfo no compatible con
     *         m&uacute;tiples remitentes. */
    private static byte[] addOriginator(final byte[] envelop,
    		                            final String contentInfo,
    		                            final PrivateKeyEntry ke) throws AOException, IOException, CertificateEncodingException {

        final byte[] newEnvelop;

        if (contentInfo.equals(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            newEnvelop = CMSEnvelopedData.addOriginatorInfo(envelop, (X509Certificate[]) ke.getCertificateChain());
        }
        else if (contentInfo.equals(AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            newEnvelop = new AOCMSSigner().cosign(
        		envelop,
        		AOSignConstants.DEFAULT_SIGN_ALGO,
        		ke.getPrivateKey(),
        		ke.getCertificateChain(),
        		null
    		);
        }
        else if (contentInfo.equals(AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
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

    /** Establece la clave privada del remitente del envoltorio.
     * @param originatorKe
     *        Clave del remitente. */
    void setOriginatorKe(final PrivateKeyEntry originatorKe) {
        this.configuredKe = originatorKe;
    }

    /** Establece la contrase&ntilde;a o clave para la encriptaci&oacute;n de los
     * datos.
     * @param keyPass
     *        Clave en base 64 o contrase&ntilde;a de cifrado. */
    void setCipherKey(final String keyPass) {
        this.cipherKey = keyPass;
    }

    /** Recupera el algoritmo de firma configurado. El algoritmo de huella
     * digital se tomar&aacute; de este.
     * @return Cadena de texto identificativa para el algoritmo de firma */
    String getSignatureAlgorithm() {
        return this.signatureAlgorithm;
    }

    /** Recupera la clave privada del remitente del envoltorio.
     * @return Clave del remitente. */
    PrivateKeyEntry getOriginatorKe() {
        return this.configuredKe;
    }

    /** Recupera la clave o contrascontrase&ntilde;a para la encriptaci&oacute;n
     * de los datos.
     * @return Clave en base 64 o contrase&ntilde;a de cifrado. */
    String getCipherKey() {
        return this.cipherKey;
    }

    /** Recupera el contenido de un envoltorio CMS.
     * @param cmsEnvelop
     *        Envoltorio CMS.
     * @return Contenido del envoltorio.
     * @throws AOInvalidRecipientException
     *         Cuando el usuario no es uno de los destinatarios del sobre.
     * @throws InvalidKeyException Cuando la clave de descifrado configurada no es v&aacute;lida.
     * @throws CertificateEncodingException
     *         Cuando el certificado del destinatario no es v&aacute;lido.
     * @throws IOException
     *         Cuando el envoltorio est&aacute; corrupto o no puede leerse.
     * @throws AOInvalidFormatException
     *         Cuando no se ha indicado un envoltorio soportado.
     * @throws AOException
     *         Cuando se produce un error durante al desenvolver los datos.
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws BadPaddingException Si hay problemas estableciendo el relleno de los datos
     * @throws IllegalBlockSizeException Si no cuadran los tama&ntilde;os de bloque de los algoritmos usados
     * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
     *                                            para alg&uacute;n algoritmo
     * @throws NoSuchPaddingException Si no se soporta alg&uacute;n m&eacute;todo de relleno
     * @throws InvalidKeySpecException Cuando ocurren problemas relacionados con la estructura interna de las claves
     * @throws DataFormatException Si hay problemas en el formado de datos esperado.
     * @throws Pkcs11WrapOperationException Cuando se produce un error derivado del uso del PKCS#11
     * 			de un dispositivo criptogr&aacute;fico.  */
    byte[] recoverData(final byte[] cmsEnvelop) throws InvalidKeyException,
                                                       CertificateEncodingException,
                                                       IOException,
                                                       AOException,
                                                       NoSuchAlgorithmException,
                                                       NoSuchPaddingException,
                                                       InvalidAlgorithmParameterException,
                                                       IllegalBlockSizeException,
                                                       BadPaddingException,
                                                       InvalidKeySpecException,
                                                       DataFormatException,
                                                       Pkcs11WrapOperationException {
    	final org.spongycastle.asn1.ASN1Sequence dsq;
    	try (
    			final org.spongycastle.asn1.ASN1InputStream is = new org.spongycastle.asn1.ASN1InputStream(cmsEnvelop);
		) {
	        // Leemos los datos
	        dsq = (org.spongycastle.asn1.ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> objects = dsq.getObjects();

        // Elementos que contienen los elementos OID Data
        final org.spongycastle.asn1.ASN1ObjectIdentifier doi = (org.spongycastle.asn1.ASN1ObjectIdentifier) objects.nextElement();

        final Logger logger = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

        if (doi.equals(org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers.data)) {
            logger.warning("La extraccion de datos de los envoltorios CMS Data no esta implementada"); //$NON-NLS-1$
            return null;
        }
        if (doi.equals(org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers.digestedData)) {
            logger.warning("La extraccion de datos de los envoltorios CMS DigestedData no esta implementada");  //$NON-NLS-1$
            return null;
        }
        if (doi.equals(org.spongycastle.asn1.cms.CMSObjectIdentifiers.compressedData)) {
        	return CMSCompressedData.getContentCompressedData(cmsEnvelop);
        }
        if (doi.equals(org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers.encryptedData)) {
        	return new CMSDecipherEncryptedData().dechiperEncryptedData(cmsEnvelop, this.cipherKey);
        }
        if (doi.equals(org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers.envelopedData)) {
        	return CMSDecipherEnvelopData.dechiperEnvelopData(cmsEnvelop, this.configuredKe);
        }
        if (doi.equals(org.spongycastle.asn1.cms.CMSObjectIdentifiers.authEnvelopedData)) {
            return CMSDecipherAuthenticatedEnvelopedData.dechiperAuthenticatedEnvelopedData(cmsEnvelop, this.configuredKe);
        }
        if (doi.equals(org.spongycastle.asn1.cms.CMSObjectIdentifiers.authenticatedData)) {
            return new CMSDecipherAuthenticatedData().decipherAuthenticatedData(cmsEnvelop, this.configuredKe);
        }
        if (doi.equals(org.spongycastle.asn1.cms.CMSObjectIdentifiers.signedAndEnvelopedData)) {
        	final CMSDecipherSignedAndEnvelopedData enveloper = new CMSDecipherSignedAndEnvelopedData(cmsEnvelop);
        	return enveloper.decipher(this.configuredKe);
        }
        throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$

    }

}
