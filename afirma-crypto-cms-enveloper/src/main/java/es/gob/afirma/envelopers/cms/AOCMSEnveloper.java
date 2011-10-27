/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

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
    /** M&eacute;todo que realiza el resto de firmas permitidas por CADES. Son
     * las siguientes: <br/>
     * <ul>
     * <li>Data</li>
     * <li>Signed Data</li>
     * <li>Digested Data</li>
     * <li>Enveloped Data</li>
     * <li>Signed and Enveloped Data</li>
     * </ul>
     * Para la generaci&oacute;n de la clave interna se utiliza por defecto el
     * AES.
     * En el caso de que sea tipo "Enveloped data" o
     * "Signed and enveloped data", la clave se generar&aacute; usando el
     * algoritmo pasado como par&aacute;metro. Dicha clave se cifrar&aacute;
     * despu&eacute;s con la clave p&uacute;blica del certificado que identifica
     * al usuario destinatario.
     * Nota: El par&aacute;metro algorithm no es el agoritmo de cifrado, es para
     * el digestAlgorithm usado en los "Unsigned Attributes".
     * @param file
     *        Flujo de lectura de los datos a firmar.
     * @param digestAlgorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param type
     *        Tipo de "envelop" que se quiere hacer.
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
    public byte[] envelop(final InputStream file,
                          final String digestAlgorithm,
                          String type,
                          final PrivateKeyEntry keyEntry,
                          final X509Certificate[] certDest,
                          final AOCipherAlgorithm cipherAlgorithm,
                          String dataType,
                          Properties extraParams) throws AOException {

        

        return null;
    }


    //TODO
    /** Cifra un contenido (t&iacute;picamente un fichero) usando para ello una
     * contrase&ntilde;a.<br/>
     * Se usar&aacute; por defecto el algoritmo de cifrado "AES".
     * La clave usada para cifrar el contenido puede ser tanto un password como
     * una clave privada del usuario codificada.
     * En el caso de que sea una clave codificada en base 64, se usar&aacute;
     * como algoritmos los tipo AES, DES ... En el caso de que sea un password,
     * se usar&aacute; un algoritmo de tipo PBE.
     * Nota: El par&aacute;metro algorithm no es el agoritmo de cifrado, es para
     * el digestAlgorithm usado en los "Unsigned Attributes".
     * @param file
     *        Flujo de lectura de los datos a firmar
     * @param digestAlgorithm
     *        Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param key
     *        Puede ser una clave codificada o una contrase&ntilde;a usada
     *        para cifrar el contenido.
     * @param cipherAlgorithm 
     *        Algoritmo a usar para los cifrados
     * @param dataType OID del tipo de datos a encriptar
     * @return Contenido firmado
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    public byte[] encrypt(final InputStream file, final String digestAlgorithm, final String key, final AOCipherAlgorithm cipherAlgorithm, final String dataType) throws AOException {

        // Comprobamos que el archivo a cifrar no sea nulo.
        if (file == null) {
            throw new IllegalArgumentException("El archivo a cifrar no puede ser nulo."); //$NON-NLS-1$
        }

        return null;
    }
    
    
    /** Tipo de los datos contenidos en la envoltura. Siempre data por
     * est&aacute;ndar. */
    private static final Oid DATA_TYPE_OID;

    /** Algoritmo de firma. */
    private String signatureAlgorithm = AOSignConstants.DEFAULT_SIGN_ALGO;

    /** Atributos firmados que se desean agregar a los envoltorios firmados. */
    private final Map<Oid, byte[]> attrib = new HashMap<Oid, byte[]>();

    /** Atributos que no requieren firma y se desean agregar a todos los
     * envoltorios que los soporten. */
    private final Map<Oid, byte[]> uattrib = new HashMap<Oid, byte[]>();

    /** Clave privada del usuario que genera o abre el envoltorio. */
    private PrivateKeyEntry configuredKe = null;

    /** Clave para el descifrado de los datos de un envoltorio EncryptedData. Si
     * se utiliza un algoritmo PBE de cifrado, ser&aacute; una contrase&ntilda;a
     * en texto plano. Si es otro algoritmo ser&aacute; su clave en base 64. */
    private String cipherKey = null;

    static {
        Oid tmpOid = null;
        try {
            tmpOid = new Oid( PKCSObjectIdentifiers.data.getId() );
        }
        catch (final Exception e) {
            /* Esto nunca podria fallar */
        }
        DATA_TYPE_OID = tmpOid;
    }

    /** Configura un atributo firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del objeto a introducir.
     * @param value
     *        Valor asignado */
    void addSignedAttribute(final org.ietf.jgss.Oid oid, final byte[] value) {
        this.attrib.put(oid, value);
    }

    /** Configura un atributo no firmado para agregarlo a un envoltorio.
     * @param oid
     *        Object Identifier. Identificador del atributo a introducir.
     * @param value
     *        Valor asignado */
    void addUnsignedAttribute(final org.ietf.jgss.Oid oid, final byte[] value) {
        this.uattrib.put(oid, value);
    }

    /** Crea un envoltorio CMS de tipo Data.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio Data. */
    byte[] createCMSData(final byte[] content) {
        return new CMSData().genData(content);
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
        return new CMSDigestedData().genDigestedData(content, this.signatureAlgorithm, DATA_TYPE_OID);
    }

    /** Crea un envoltorio CMS de tipo CompressedData.
     * @param content
     *        Datos que se desean envolver.
     * @return Envoltorio Compressed Data. */
    byte[] createCMSCompressedData(final byte[] content) {
        return new CMSCompressedData().genCompressedData(content);
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
     */
    byte[] createCMSEncryptedData(final byte[] content, final AOCipherConfig cipherConfig, final Key key) throws NoSuchAlgorithmException {
        return new CMSEncryptedData().genEncryptedData(content, this.signatureAlgorithm, cipherConfig, key, DATA_TYPE_OID, this.uattrib);
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
     */
    public byte[] createCMSEnvelopedData(final byte[] content,
                                         final PrivateKeyEntry ke,
                                         final AOCipherConfig cipherConfig,
                                         final X509Certificate[] recipientsCerts) throws NoSuchAlgorithmException,
                                                                                 CertificateEncodingException,
                                                                                 IOException, AOException {

        // Si se establecion un remitente
        if (ke != null) {
            return new CMSEnvelopedData().genEnvelopedData(this.createContentSignerParementers(content, ke, this.signatureAlgorithm),
                                                           cipherConfig,
                                                           recipientsCerts,
                                                           DATA_TYPE_OID,
                                                           this.uattrib);
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
     */
    public byte[] createCMSSignedAndEnvelopedData(final byte[] content,
                                                  final PrivateKeyEntry ke,
                                                  final AOCipherConfig cipherConfig,
                                                  final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                          NoSuchAlgorithmException,
                                                                                          IOException, AOException {
        return new CMSSignedAndEnvelopedData().genSignedAndEnvelopedData(this.createContentSignerParementers(content, ke, this.signatureAlgorithm),
                                                                         cipherConfig,
                                                                         recipientsCerts,
                                                                         DATA_TYPE_OID,
                                                                         ke,
                                                                         this.attrib,
                                                                         this.uattrib);
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
     */
    byte[] createCMSAuthenticatedData(final byte[] content, final PrivateKeyEntry ke, final AOCipherConfig cipherConfig, final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                                                                         NoSuchAlgorithmException,
                                                                                                                                         IOException, AOException {
        return new CMSAuthenticatedData().genAuthenticatedData(this.createContentSignerParementers(content, ke, this.signatureAlgorithm), // ContentSignerParameters
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
     */
    public byte[] createCMSAuthenticatedEnvelopedData(final byte[] content,
                                                      final PrivateKeyEntry ke,
                                                      final AOCipherConfig cipherConfig,
                                                      final X509Certificate[] recipientsCerts) throws CertificateEncodingException,
                                                                                        NoSuchAlgorithmException,
                                                                                        IOException, AOException {
        return new CMSAuthenticatedEnvelopedData().genAuthenticatedEnvelopedData(this.createContentSignerParementers(content, ke, this.signatureAlgorithm), // ContentSignerParameters
                                                                                 null, // Algoritmo de autenticacion (usamos el por
                                                                                       // defecto)
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
    private P7ContentSignerParameters createContentSignerParementers(final byte[] content, final PrivateKeyEntry ke, final String digestAlgorithm) {
        X509Certificate[] xCerts = new X509Certificate[0];
        final Certificate[] certs = ke.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            xCerts = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = ke.getCertificate();
            if (cert instanceof X509Certificate) {
                xCerts = new X509Certificate[] {
                    (X509Certificate) cert
                };
            }
        }
        return new P7ContentSignerParameters(content, digestAlgorithm, xCerts);
    }

    /** Agrega un nuevo remitente a un envoltorio CMS compatible.
     * @param envelop
     *        Envoltorio original.
     * @param ke
     *        Referencia a la clave privada del certificado del remitente.
     * @return Envoltorio con el nuevo remitente.
     * @throws AOException
     *         Cuando se produce un error al agregar el nuevo remitente.
     * @throws AOInvalidFormatException
     *         Tipo de envoltorio no soportado. */
    byte[] addOriginator(final byte[] envelop, final PrivateKeyEntry ke) throws AOException {
        String contentInfo;
        final ValidateCMS validator = new ValidateCMS();
        if (validator.isCMSEnvelopedData(envelop)) {
            contentInfo = CMS_CONTENTTYPE_ENVELOPEDDATA;
        }
        else if (validator.isCMSSignedAndEnvelopedData(envelop)) {
            contentInfo = CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA;
        }
        else if (validator.isCMSAuthenticatedEnvelopedData(envelop)) {
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
     * @throws IllegalArgumentException
     *         Cuando se indica un contentInfo no compatible con
     *         m&uacute;tiples remitentes. */
    private byte[] addOriginator(final byte[] envelop, final String contentInfo, final PrivateKeyEntry ke) throws AOException {

        byte[] newEnvelop;

        X509Certificate[] xCerts = new X509Certificate[0];
        final Certificate[] certs = ke.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            xCerts = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = ke.getCertificate();
            if (cert instanceof X509Certificate) {
                xCerts = new X509Certificate[] {
                    (X509Certificate) cert
                };
            }
        }

        final X509Certificate[] originatorCertChain = xCerts;

        if (contentInfo.equals(CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            final CMSEnvelopedData enveloper = new CMSEnvelopedData();
            newEnvelop = enveloper.addOriginatorInfo(envelop, originatorCertChain);
        }
        else if (contentInfo.equals(CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            newEnvelop = new AOCMSSigner().cosign(envelop, AOSignConstants.DEFAULT_SIGN_ALGO, ke, null);
        }
        else if (contentInfo.equals(CMS_CONTENTTYPE_AUTHENVELOPEDDATA)) {
            final CMSAuthenticatedEnvelopedData enveloper = new CMSAuthenticatedEnvelopedData();
            newEnvelop = enveloper.addOriginatorInfo(envelop, originatorCertChain);

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
        this.signatureAlgorithm = (algorithm == null ? AOSignConstants.DEFAULT_SIGN_ALGO : algorithm);
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
     *        Clave en base 64 o contrase&ntilda;a de cifrado. */
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
     * @return Clave en base 64 o contrase&ntilda;a de cifrado. */
    String getCipherKey() {
        return this.cipherKey;
    }

    /** Recupera el contenido de un envoltorio CMS.
     * @param cmsEnvelop
     *        Envoltorio CMS.
     * @return Contenido del envoltorio.
     * @throws AOInvalidRecipientException
     *         Cuando el usuario no es uno de los destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave de descifrado configurada no es
     *         v&aacute;lida.
     * @throws CertificateEncodingException
     *         Cuando el certificado del destinatario no es v&aacute;lido.
     * @throws IOException
     *         Cuando el envoltorio est&aacute; corrupto o no puede leerse.
     * @throws AOInvalidFormatException
     *         Cuando no se ha indicado un envoltorio soportado.
     * @throws AOException
     *         Cuando se produce un error durante al desenvolver los datos. 
     * @throws NoSuchAlgorithmException 
     * @throws BadPaddingException 
     * @throws IllegalBlockSizeException 
     * @throws InvalidAlgorithmParameterException 
     * @throws NoSuchPaddingException */
    byte[] recoverData(final byte[] cmsEnvelop) throws 
                                         InvalidKeyException,
                                         CertificateEncodingException,
                                         IOException,
                                         AOException, 
                                         NoSuchAlgorithmException, 
                                         NoSuchPaddingException, 
                                         InvalidAlgorithmParameterException, 
                                         IllegalBlockSizeException, 
                                         BadPaddingException {

        final org.bouncycastle.asn1.ASN1InputStream is = new org.bouncycastle.asn1.ASN1InputStream(cmsEnvelop);

        // Leemos los datos
        final org.bouncycastle.asn1.ASN1Sequence dsq;
        try {
            dsq = (org.bouncycastle.asn1.ASN1Sequence) is.readObject();
        }
        finally {
            try {
                is.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
        }

        final Enumeration<?> objects = dsq.getObjects();

        // Elementos que contienen los elementos OID Data
        final org.bouncycastle.asn1.DERObjectIdentifier doi = (org.bouncycastle.asn1.DERObjectIdentifier) objects.nextElement();

        byte[] datos;
        if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.data)) {
            Logger.getLogger("es.gob.afirma").warning("La extraccion de datos de los envoltorios CMS Data no esta implementada"); //$NON-NLS-1$ //$NON-NLS-2$
            datos = null;
            // datos = this.recoverCMSEncryptedData(cmsEnvelop, cipherKey);
        }
        else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.digestedData)) {
            Logger.getLogger("es.gob.afirma").warning("La extraccion de datos de los envoltorios CMS DigestedData no esta implementada"); //$NON-NLS-1$ //$NON-NLS-2$
            datos = null;
            // datos = this.recoverCMSEncryptedData(cmsEnvelop, cipherKey);
        }
        else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.compressedData)) {
            datos = this.recoverCMSCompressedData(cmsEnvelop);
        }
        else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.encryptedData)) {
            datos = this.recoverCMSEncryptedData(cmsEnvelop, this.cipherKey);
        }
        else if (doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.envelopedData)) {
            datos = this.recoverCMSEnvelopedData(cmsEnvelop, this.configuredKe);
        }
        else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authEnvelopedData)) {
            datos = this.recoverCMSAuthenticatedEnvelopedData(cmsEnvelop, this.configuredKe);
        }
        else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authenticatedData)) {
            datos = this.recoverCMSAuthenticatedData(cmsEnvelop, this.configuredKe);
        }
        else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.signedAndEnvelopedData)) {
            datos = this.recoverCMSSignedEnvelopedData(cmsEnvelop, this.configuredKe);
        }
        else {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
        }

        return datos;
    }

    /** Recupera el contenido de un envoltorio CompressedData.
     * @param compressedData
     *        Envoltorio CMS de tipo CompressedData.
     * @return Contenido del envoltorio.
     * @throws IOException
     *         Cuando ocurre un error al descomprimir los datos. */
    byte[] recoverCMSCompressedData(final byte[] compressedData) throws IOException {
        return new CMSCompressedData().getContentCompressedData(compressedData);
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
     * @throws NoSuchAlgorithmException */
    byte[] recoverCMSEncryptedData(final byte[] encryptedData, final String passkey) throws InvalidKeyException, 
                                                                                            AOException, 
                                                                                            NoSuchAlgorithmException, 
                                                                                            NoSuchPaddingException, 
                                                                                            InvalidAlgorithmParameterException, 
                                                                                            IllegalBlockSizeException, 
                                                                                            BadPaddingException {
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
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida. */
    byte[] recoverCMSEnvelopedData(final byte[] envelopedData, final PrivateKeyEntry ke) throws IOException,
                                                                                        CertificateEncodingException,
                                                                                        AOException,
                                                                                        InvalidKeyException {
        return new CMSDecipherEnvelopData().dechiperEnvelopData(envelopedData, ke);
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
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida. */
    byte[] recoverCMSSignedEnvelopedData(final byte[] signedEnvelopedData, final PrivateKeyEntry ke) throws IOException,
                                                                                                    CertificateEncodingException,
                                                                                                    AOException,
                                                                                                    InvalidKeyException {
        return new CMSDecipherSignedAndEnvelopedData().dechiperSignedAndEnvelopData(signedEnvelopedData, ke);
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
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de
     *         extracci&oacute;n.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida. 
     * @throws NoSuchAlgorithmException
     */
    byte[] recoverCMSAuthenticatedData(final byte[] authenticatedData, final PrivateKeyEntry ke) throws IOException,
                                                                                                CertificateEncodingException,
                                                                                                AOException,
                                                                                                InvalidKeyException, NoSuchAlgorithmException {
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
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida. */
    byte[] recoverCMSAuthenticatedEnvelopedData(final byte[] authenticatedEnvelopedData, final PrivateKeyEntry ke) throws IOException,
                                                                                                                  CertificateEncodingException,
                                                                                                                  AOException,
                                                                                                                  InvalidKeyException {
        return new CMSDecipherAuthenticatedEnvelopedData().dechiperAuthenticatedEnvelopedData(authenticatedEnvelopedData, ke);
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
    public boolean isCMSValid(final byte[] cmsData) {
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
    public boolean isCMSValid(final byte[] data, final String type) {
        return CMSHelper.isCMSValid(data, type);
    }
}
