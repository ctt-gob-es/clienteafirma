/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros 
 * para la realizacion de procesos de autenticacion, firma electronica y validacion 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion 
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha 
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos 
 * e Impulso de la Administracion Electronica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * European Union Public License publicada por la Comision Europea, 
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 * 
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 * 
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.jmulticard.jse.provider;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import es.gob.jmulticard.CryptoHelper;
import es.gob.jmulticard.jse.provider.digest.Digest;
import es.gob.jmulticard.jse.provider.digest.SHA1Digest;
import es.gob.jmulticard.jse.provider.digest.SHA256Digest;
import es.gob.jmulticard.jse.provider.digest.SHA384Digest;
import es.gob.jmulticard.jse.provider.digest.SHA512Digest;

/** Funcionalidades criptogr&aacute;ficas de utilidad implementadas mediante proveedores de seguridad JSE6.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class JseCryptoHelper implements CryptoHelper {

    private static final String SHA512 = "SHA-512"; //$NON-NLS-1$
    private static final String SHA384 = "SHA-384"; //$NON-NLS-1$
    private static final String SHA256 = "SHA-256"; //$NON-NLS-1$
    private static final String SHA1 = "SHA-1"; //$NON-NLS-1$

    /** {@inheritDoc} */
    @Override
    public byte[] digest(final String algorithm, final byte[] data) throws IOException {

        if (algorithm == null) {
            throw new IllegalArgumentException("El algoritmo de huella digital no puede ser nulo"); //$NON-NLS-1$
        }

        final String digestAlgorithm = normalizeDigestAlgorithm(algorithm);
        try {
            final Digest digest = selectMessageDigest(digestAlgorithm);
            digest.update(data, 0, data.length);
            final byte[] result = new byte[digest.getDigestSize()];
            digest.doFinal(result, 0);
            return result;
        }
        catch (final Exception e) {
            throw new IOException("Error obteniendo la huella digital de los datos: " + e, e); //$NON-NLS-1$
        }
    }

    /* Devuelve una instancia del algoritmo de huella pedido o null si la opcion no existe */
    private static Digest selectMessageDigest(final String digestAlgorithm) {
        if (JseCryptoHelper.SHA1.equals(digestAlgorithm)) {
            return new SHA1Digest();
        }
        else if (JseCryptoHelper.SHA256.equals(digestAlgorithm)) {
            return new SHA256Digest();
        }
        else if (JseCryptoHelper.SHA384.equals(digestAlgorithm)) {
            return new SHA384Digest();
        }
        else if (JseCryptoHelper.SHA512.equals(digestAlgorithm)) {
            return new SHA512Digest();
        }
        return null;
    }

    /** Normaliza el nombre de un algoritmo de huella digital.
     * @param algo Algoritmo.
     * @return nom,bre normalizado o el mismo de entrada si no se puede identificar. */
    private static String normalizeDigestAlgorithm(final String algorithm) {
        if ("SHA".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
            "SHA1".equalsIgnoreCase(algorithm) //$NON-NLS-1$
            || JseCryptoHelper.SHA1.equalsIgnoreCase(algorithm)) {
            return JseCryptoHelper.SHA1;
        }
        else if ("SHA256".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 JseCryptoHelper.SHA256.equalsIgnoreCase(algorithm)) {
            return JseCryptoHelper.SHA256;
        }
        else if ("SHA384".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 JseCryptoHelper.SHA384.equalsIgnoreCase(algorithm)) {
            return JseCryptoHelper.SHA384;
        }
        else if ("SHA512".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 JseCryptoHelper.SHA512.equalsIgnoreCase(algorithm)) {
            return JseCryptoHelper.SHA512;
        }
        return null;
    }

    private static byte[] doDesede(final byte[] data, final byte[] key, final int direction) throws IOException {
        final byte[] ivBytes = new byte[8];
        for (int i = 0; i < 8; i++) {
            ivBytes[i] = 0x00;
        }

        final SecretKey k = new SecretKeySpec(prepareDesedeKey(key), "DESede"); //$NON-NLS-1$
        try {
            final Cipher cipher = Cipher.getInstance("DESede/CBC/NoPadding"); //$NON-NLS-1$
            cipher.init(direction, k, new IvParameterSpec(ivBytes));
            final byte[] cipheredData = cipher.doFinal(data);
            // Machacamos los datos para evitar que queden en memoria
            for(int i=0;i<data.length;i++) {
                data[i] = '\0';
            }
            return cipheredData; 
        }
        catch (final Exception e) {
            // Machacamos los datos para evitar que queden en memoria
            for(int i=0;i<data.length;i++) {
                data[i] = '\0';
            }
            throw new IOException("Error encriptando datos: " + e, e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
    public byte[] desedeEncrypt(final byte[] data, final byte[] key) throws IOException {
        return doDesede(data, key, Cipher.ENCRYPT_MODE);
    }

    /** {@inheritDoc} */
    @Override
    public byte[] desedeDecrypt(final byte[] data, final byte[] key) throws IOException {
        return doDesede(data, key, Cipher.DECRYPT_MODE);
    }

    private static byte[] prepareDesedeKey(final byte[] key) {
        if (key == null) {
            throw new IllegalArgumentException("La clave 3DES no puede ser nula"); //$NON-NLS-1$
        }
        if (key.length == 24) {
            return key;
        }
        if (key.length == 16) {
            final byte[] newKey = new byte[24];
            System.arraycopy(key, 0, newKey, 0, 16);
            System.arraycopy(key, 0, newKey, 16, 8);
            return newKey;
        }
        throw new IllegalArgumentException("Longitud de clave invalida, se esperaba 16 o 24, pero se indico " + Integer.toString(key.length)); //$NON-NLS-1$
    }

    private static byte[] doDes(final byte[] data, final byte[] key, final int direction) throws IOException {
        if (key == null) {
            throw new IllegalArgumentException("La clave DES no puede ser nula"); //$NON-NLS-1$
        }
        if (key.length != 8) {
            throw new IllegalArgumentException(
               "La clave DES debe ser de 8 octetos, pero la proporcionada es de " + key.length //$NON-NLS-1$
            );
        }
        try {
            final Cipher cipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
            cipher.init(direction, new SecretKeySpec(key, "DES")); //$NON-NLS-1$
            return cipher.doFinal(data);
        }
        catch (final Exception e) {
            throw new IOException("Error cifrando los datos con DES: " + e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Override
    public byte[] desEncrypt(final byte[] data, final byte[] key) throws IOException {
        return doDes(data, key, Cipher.ENCRYPT_MODE);
    }

    /** {@inheritDoc} */
    @Override
    public byte[] desDecrypt(final byte[] data, final byte[] key) throws IOException {
        return doDes(data, key, Cipher.DECRYPT_MODE);
    }

    private static byte[] doRsa(final byte[] cipheredData, final Key key, final int direction) throws IOException {
        try {
            final Cipher dec = Cipher.getInstance("RSA/ECB/NOPADDING"); //$NON-NLS-1$
            dec.init(direction, key);
            return dec.doFinal(cipheredData);
        }
        catch (final Exception e) {
            throw new IOException("Error descifrando los datos mediante la clave RSA: " + e, e); //$NON-NLS-1$
        }

    }

    /** {@inheritDoc} */
    @Override
    public byte[] rsaDecrypt(final byte[] cipheredData, final Key key) throws IOException {
        return doRsa(cipheredData, key, Cipher.DECRYPT_MODE);
    }

    /** {@inheritDoc} */
    @Override
    public byte[] rsaEncrypt(final byte[] data, final Key key) throws IOException {
        return doRsa(data, key, Cipher.ENCRYPT_MODE);
    }

    /** {@inheritDoc} */
    @Override
    public Certificate generateCertificate(final byte[] encode) throws CertificateException {
        return CertificateFactory.getInstance("X.509").generateCertificate(new ByteArrayInputStream(encode)); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public byte[] generateRandomBytes(final int numBytes) throws IOException {
        final SecureRandom sr;
        try {
            sr = SecureRandom.getInstance("SHA1PRNG"); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            throw new IOException("Algoritmo de generacion de aleatorios no valido: " + e, e); //$NON-NLS-1$
        }
        final byte[] randomBytes = new byte[numBytes];
        sr.nextBytes(randomBytes);
        return randomBytes;
    }
}