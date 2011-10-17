/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cades;

import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherPadding;
import es.gob.afirma.core.envelopers.AOEnveloper;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Funcionalidad de sobres digitales con CAdES. */
public class AOCAdESEnveloper implements AOEnveloper {
    
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
     * @param xParams
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
                          final String datTyp,
                          final Properties xParams) throws AOException {

        final Properties extraParams = (xParams !=null) ? xParams : new Properties();

        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false")); //$NON-NLS-1$ //$NON-NLS-2$

        // Comprobamos que el archivo a tratar no sea nulo.
        if (file == null) {
            throw new IllegalArgumentException("El archivo a tratar no puede ser nulo."); //$NON-NLS-1$
        }

        final byte[] plainData;
        try {
            plainData = AOUtil.getDataFromInputStream(file);
        }
        catch (final Exception e1) {
            throw new AOException("No se han podido leer los datos a firmar", e1); //$NON-NLS-1$
        }

        P7ContentSignerParameters csp = null;
        if (keyEntry != null) {

            X509Certificate[] xCerts = new X509Certificate[0];
            final Certificate[] certs = keyEntry.getCertificateChain();
            if (certs != null && (certs instanceof X509Certificate[])) {
                xCerts = (X509Certificate[]) certs;
            }
            else {
                final Certificate cert = keyEntry.getCertificate();
                if (cert instanceof X509Certificate) {
                    xCerts = new X509Certificate[] {
                                                    (X509Certificate) cert
                    };
                }
            }

            csp = new P7ContentSignerParameters(plainData, digestAlgorithm, xCerts);

        }

        // tipos de datos a firmar.
        final String dataType = (datTyp != null) ? datTyp : PKCSObjectIdentifiers.data.getId();

        // Datos firmados.
        byte[] dataSigned = null;

        // Seleccion del algoritmo de cifrado.
        AOCipherConfig config = null;
        if (cipherAlgorithm == null) {
            // Por defecto usamos el AES.
            config = new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING);
        }
        /*
         * En caso de usar un algoritmo de cifrado, si no funciona es porque el
         * Provider no lo soporta.
         */
        else {
            config = new AOCipherConfig(cipherAlgorithm, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING);
        }

        try {
            // Busqueda del tipo que nos han solicitado.

            // Es Data.
            if (AOSignConstants.CMS_CONTENTTYPE_DATA.equals(type)) {
                dataSigned = new CAdESData().genData(csp);
            }
            // Es Digested Data.
            else if (AOSignConstants.CMS_CONTENTTYPE_DIGESTEDDATA.equals(type)) {
                dataSigned = new CAdESDigestedData().genDigestedData(csp, dataType);
            }
            // Es Enveloped Data. El null y el vacio se consideran Enveloped Data, el por defecto
            else if (AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA.equals(type)  || type == null || "".equals(type)) { //$NON-NLS-1$
                if (keyEntry != null) {
                    dataSigned = new CAdESEnvelopedData().genEnvelopedData(csp, config, certDest, dataType);
                }
                else {
                    dataSigned = new CAdESEnvelopedData().genEnvelopedData(plainData, digestAlgorithm, config, certDest, dataType);
                }
            }
            // Es Signed and Enveloped Data.
            else {
                dataSigned =
                        new CAdESEPESSignedAndEnvelopedData().genCADESEPESSignedAndEnvelopedData(csp,
                                                                                                 config,
                                                                                                 new AdESPolicy(extraParams),
                                                                                                 signingCertificateV2,
                                                                                                 certDest,
                                                                                                 PKCSObjectIdentifiers.signedData.getId(),
                                                                                                 keyEntry);
            }
        }
        catch (final Exception e) {
            throw new AOException("Error generando el enveloped de CADES", e); //$NON-NLS-1$
        }

        return dataSigned;
    }


    /** Cifra un contenido (t&iacute;picamente un fichero) usando para ello una
     * contrase&ntilde;a.<br/>
     * Los algoritmos y modos de firma disponibles se declaran en AOSignConstants.<br/>
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

        // Seleccion del algoritmo de cifrado.
        AOCipherConfig config = null;
        if (cipherAlgorithm == null) {
            // Por defecto usamos el PBEWITHSHA1ANDDESEDE. El AES en este caso
            // no funciona.
            config = new AOCipherConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING);
        }
        /*
         * En caso de usar un algoritmo de cifrado, si no funciona es porque el
         * Provider no lo soporta.
         */
        else {
            config = new AOCipherConfig(cipherAlgorithm, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING);
        }

        try {
            return new CADESEncryptedData().genEncryptedData(file, digestAlgorithm, config, key, (dataType != null) ? dataType : PKCSObjectIdentifiers.data.getId());
        }
        catch (final Exception e) {
            throw new AOException("Error generando el enveloped de CADES", e); //$NON-NLS-1$
        }

    }

    
//  /** Inserta un nuevo firmante dentro de una firma signedAndEnveloped dada.
//  * @param signFile
//  *        Flujo de entrada de datos que contiene la firma.
//  * @param file
//  *        Fichero de firma, necesario para calcular los datos del nuevo
//  *        firmante.
//  * @param signatureAlgorithm
//  *        Algoritmo de firma.
//  * @param keyEntry
//  *        Clave privada a usar para firmar.
//  * @param extraParams
//  *        Par&aacute;metros adiocionales (variables)
//  * @return Firma original con el nuevo firmante a&ntilde;adido
//  * @throws AOException
//  *         Cuando ocurre cualquier problema durante el proceso */
// public byte[] addOriginatorInfo(final InputStream signFile,
//                                 final InputStream file,
//                                 final String signatureAlgorithm,
//                                 final PrivateKeyEntry keyEntry,
//                                 final Properties extraParams) throws AOException {
//
//     // Comprobamos que el archivo a tratar no sea nulo.
//     if (file == null) {
//         throw new IllegalArgumentException("El archivo a tratar no puede ser nulo.");
//     }
//
//     final byte[] plainData;
//     try {
//         plainData = AOUtil.getDataFromInputStream(file);
//     }
//     catch (final Exception e1) {
//         throw new AOException("No se han podido leer los datos a firmar", e1);
//     }
//
//     P7ContentSignerParameters csp = null;
//     if (keyEntry != null) {
//
//         X509Certificate[] xCerts = new X509Certificate[0];
//         final Certificate[] certs = keyEntry.getCertificateChain();
//         if (certs != null && (certs instanceof X509Certificate[])) {
//             xCerts = (X509Certificate[]) certs;
//         }
//         else {
//             final Certificate cert = keyEntry.getCertificate();
//             if (cert instanceof X509Certificate) {
//                 xCerts = new X509Certificate[] {
//                                                 (X509Certificate) cert
//                 };
//             }
//         }
//
//         csp = new P7ContentSignerParameters(plainData, signatureAlgorithm, xCerts);
//
//     }
//
//     // Tipos de datos a firmar.
//     if (this.dataType == null) {
//         this.dataType = PKCSObjectIdentifiers.data.getId();
//     }
//
//     String policyQualifier = extraParams.getProperty("policyQualifier");
//
//     final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
//
//     // Datos firmados.
//     byte[] dataSigned = null;
//
//     try {
//         dataSigned =
//                 new CAdESEPESSignedAndEnvelopedData().addOriginatorInfo(signFile,
//                                                                         csp,
//                                                                         keyEntry,
//                                                                         dataType,
//                                                                         extraParams.getProperty("policyIdentifier"),
//                                                                         policyQualifier,
//                                                                         signingCertificateV2);
//
//     }
//     catch (final Exception e) {
//         throw new AOException("Error generando el enveloped de CAdES", e);
//     }
//     return dataSigned;
// }
    
    
}
