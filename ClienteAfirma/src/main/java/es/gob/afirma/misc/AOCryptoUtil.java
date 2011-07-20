/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.awt.Component;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.xml.crypto.dsig.DigestMethod;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.binary.StringUtils;

import es.gob.afirma.Messages;
import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;

/** M&eacute;todos generales de utilidad para criptograf&iacute;a. */
public final class AOCryptoUtil {

    /** Obtiene una huella digital. El proveedor utilizado para generar la huella
     * es "SUN y los algoritmos que acepta:
     * <ul>
     * <li>MD2</li>
     * <li>MD5</li>
     * <li>SHA-1</li>
     * <li>SHA-256</li>
     * <li>SHA-384</li>
     * <li>SHA-512</li>
     * </ul>
     * @param data
     *        Datos origen de la huella digital
     * @param algorithm
     *        Algoritmo a usar para la generaci&oacute;n de la huella
     *        digital
     * @return Huella digital de los datos proporcionados, obtenida mediante el
     *         algoritmo indicado
     * @throws NoSuchAlgorithmException
     *         Si no se soporta el algoritmo de huella digital indicado */
    public static byte[] getMessageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {

        final String PROVIDER = "SUN";

        final MessageDigest md;
        try {
            md = MessageDigest.getInstance(algorithm, PROVIDER);
        }
        catch (final NoSuchProviderException e) {
            Logger.getLogger("es.gob.afirma").severe("No se encuentra el proveedor '" + PROVIDER + "': " + e);
            return null;
        }
        md.update(data);
        return md.digest();
    }

    /** Recupera un manejador de firma v&aacute;lido para el formato
     * especificado. Si el formato no est&aacute; soportado se devuelve <code>null</code>.
     * @param format
     *        Formato de firma.
     * @return Manejador de firma especificado.
     * @see es.gob.afirma.misc.AOConstants */
    public static AOSigner getSigner(final String format) {
        return AOSignerFactory.getInstance().getSigner(format);
    }

    /** Recupera un manejador de firma v&aacute;lido para la firma indicada. Si
     * el formato no est&aacute; soportado u ocurre alg&uacute;n error se
     * devuelve <code>null</code>.
     * @param signFile
     *        Fichero de firma.
     * @return Manejador de firma especificado.
     * @see es.gob.afirma.misc.AOConstants */
    static AOSigner getSigner(final File signFile) {

        int nBytes = 0;
        final byte[] buffer = new byte[1024];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final InputStream fis;
        try {
            fis = new FileInputStream(signFile);
            while ((nBytes = fis.read(buffer)) != -1) {
                baos.write(buffer, 0, nBytes);
            }
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo leer el fichero '" + signFile.getAbsolutePath() + "': " + e);
            return null;
        }
        try {
            fis.close();
        }
        catch (final Exception e) {}

        final AOSigner signer = AOSignerFactory.getSigner(baos.toByteArray());

        try {
            baos.close();
        }
        catch (final Exception e) {}

        return signer;
    }

    /** Recupera el manejador de claves asociado a un certificado seg&uacute;n el
     * repositorio en el que se aloja.
     * @param store
     *        Almace&eacute;n de claves del certificado.
     * @param parent
     *        Componente sobre el que se deben visualizar los
     *        di&aacute;logos modales.
     * @return Manejador para la solicitud de la clave. */
    public static PasswordCallback getCertificatePC(final AOConstants.AOKeyStore store, final Component parent) {
        PasswordCallback pssCallback;
        if (store == AOConstants.AOKeyStore.WINDOWS || store == AOConstants.AOKeyStore.WINROOT
            || store == AOConstants.AOKeyStore.WINADDRESSBOOK
            || store == AOConstants.AOKeyStore.WINCA
            || store == AOConstants.AOKeyStore.SINGLE
            || store == AOConstants.AOKeyStore.MOZILLA
            || store == AOConstants.AOKeyStore.MOZ_UNI
            || store == AOConstants.AOKeyStore.PKCS11) {
            pssCallback = new NullPasswordCallback();
        }
        else {
            pssCallback = new UIPasswordCallback("Contrase\u00F1a del certificado", parent);
        }

        return pssCallback;
    }

    /** Recupera el PasswordCallback que com&uacute;nmente se requiere para el
     * acceso a un almac&eacute;n de claves.
     * @param kStore
     *        Almac&eacuten de claves
     * @param parent
     *        Componente sobre el que se deben visualizar los
     *        di&aacute;logos modales.
     * @return Manejador para la solicitud de la clave. */
    public static PasswordCallback getPreferredPCB(final AOConstants.AOKeyStore kStore, final Component parent) {

        if (kStore == null)
         {
            throw new IllegalArgumentException("No se ha indicado el KeyStore del que desea " + //$NON-NLS-1$
                                                               "obtener le PasswordCallBack"); //$NON-NLS-1$
        }

        PasswordCallback pssCallback;
        if (kStore == AOConstants.AOKeyStore.WINDOWS || kStore == AOConstants.AOKeyStore.WINROOT
            || kStore == AOConstants.AOKeyStore.PKCS11
            || kStore == AOConstants.AOKeyStore.APPLE) {
            pssCallback = new NullPasswordCallback();
        }
        else {
            pssCallback = new UIPasswordCallback(Messages.getString("AOCryptoUtil.0", kStore.getDescription()), parent); //$NON-NLS-1$
        }
        return pssCallback;
    }

    /** Crea un X509Certificate a partir de un certificado en Base64.
     * @param b64Cert
     *        Certificado en Base64. No debe incluir <i>Bag Attributes</i>
     * @return Certificado X509 o <code>null</code> si no se pudo crear */
    public static X509Certificate createCert(final String b64Cert) {
        if (b64Cert == null || "".equals(b64Cert)) {
            Logger.getLogger("es.gob.afirma").severe("Se ha proporcionado una cadena nula o vacia, se devolvera null");
            return null;
        }
        final X509Certificate cert;
        try {
            final InputStream isCert = new ByteArrayInputStream(decodeBase64(b64Cert));
            cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(isCert);
            try {
                isCert.close();
            }
            catch (final Exception e) {}
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se pudo decodificar el certificado en Base64, se devolvera null: " + e);
            return null;
        }
        return cert;
    }

    /** Obtiene el nombre de un algoritmo de huella digital a partir de una de
     * las variantes de este.
     * @param pseudoName
     *        Nombre o variante del nombre del algoritmo de huella digital
     * @return Nombre del algoritmo de huella digital */
    public static String getDigestAlgorithmName(final String pseudoName) {
        final String upperPseudoName = pseudoName.toUpperCase();
        if (upperPseudoName.equals("SHA") || upperPseudoName.equals(DigestMethod.SHA1.toUpperCase())
            || upperPseudoName.startsWith("SHA1")
            || upperPseudoName.startsWith("SHA-1")) {
            return "SHA1";
        }

        if (upperPseudoName.equals(DigestMethod.SHA256.toUpperCase()) || upperPseudoName.startsWith("SHA256")
            || upperPseudoName.startsWith("SHA-256")) {
            return "SHA-256";
        }

        if (upperPseudoName.startsWith("SHA384") || upperPseudoName.startsWith("SHA-384")) {
            return "SHA-384";
        }

        if (upperPseudoName.equals(DigestMethod.SHA512.toUpperCase()) || upperPseudoName.startsWith("SHA512")
            || upperPseudoName.startsWith("SHA-512")) {
            return "SHA-512";
        }

        if (upperPseudoName.equals(DigestMethod.RIPEMD160.toUpperCase()) || upperPseudoName.startsWith("RIPEMD160")
            || upperPseudoName.startsWith("RIPEMD-160")) {
            return "RIPEMD160";
        }

        if (upperPseudoName.equals("MD5") || upperPseudoName.startsWith("MD5")) {
            return "MD5";
        }

        if (upperPseudoName.equals("MD2") || upperPseudoName.startsWith("MD2")) {
            return "MD2";
        }

        throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + pseudoName);
    }

    /** Codifica unos datos a base 64. Si ocurre cualquier error durante la
     * lectura de los datos, se devolver&aacute; {@code null}.
     * @param data
     *        Datos que deseamos transformar.
     * @param chunked
     *        Indica si debe insertarse un salto de l&iacute;nea cada 76
     *        caracteres.
     * @return Cadena en base 64. */
    public static String encodeBase64(final byte[] data, final boolean chunked) {

        try {
            return StringUtils.newStringUtf8(Base64.encodeBase64(data, chunked));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No se pudo convertir un binario a base 64, se devolvera null: " + e);
            return null;
        }
    }

    /** Descodifica una cadena en base 64. Si se le proporciona un {@code null},
     * devuelve {@code null}.
     * @param b64Data
     *        Cadena de texto en base 64.
     * @return Datos descodificados. */
    public static byte[] decodeBase64(final String b64Data) {
        return (b64Data == null ? null : Base64.decodeBase64(b64Data));
    }

    /** Descodifica un array en base 64. Si se le proporciona un {@code null},
     * devuelve {@code null}.
     * @param b64Data
     *        Array con los contenidos en base 64.
     * @return Datos descodificados. */
    public static byte[] decodeBase64(final byte[] b64Data) {
        return (b64Data == null ? null : Base64.decodeBase64(b64Data));
    }

}
