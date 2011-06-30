/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.AlgorithmParameterGenerator;
import java.security.AlgorithmParameters;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;
import java.util.HashMap;
import java.util.Map;

import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.RC2ParameterSpec;

import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.cms.CMSEnvelopedGenerator;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

/** Clase con m&eacute;todos auxiliares obtenidos de Bouncy Castle. Se han
 * extraido los m&eacute;todos de ayuda que son necesarios para generar una
 * firma de tipo Authenticated-data. Utilizando estos m&eacute;todos auxiliares
 * se evita retocar en el c&oacute;digo fuente de Bouncy Castle. */
final class AuthenticatedDataHelper {

    private static final Map<String, String> MAC_ALG_NAMES = new HashMap<String, String>();
    private static final Map<String, String> BASE_CIPHER_NAMES = new HashMap<String, String>();

    static {
        BASE_CIPHER_NAMES.put(CMSEnvelopedGenerator.DES_EDE3_CBC, "DESEDE");
        BASE_CIPHER_NAMES.put(CMSEnvelopedGenerator.AES128_CBC, "AES");
        BASE_CIPHER_NAMES.put(CMSEnvelopedGenerator.AES192_CBC, "AES");
        BASE_CIPHER_NAMES.put(CMSEnvelopedGenerator.AES256_CBC, "AES");

        MAC_ALG_NAMES.put(CMSEnvelopedGenerator.DES_EDE3_CBC, "DESEDEMac");
        MAC_ALG_NAMES.put(CMSEnvelopedGenerator.AES128_CBC, "AESMac");
        MAC_ALG_NAMES.put(CMSEnvelopedGenerator.AES192_CBC, "AESMac");
        MAC_ALG_NAMES.put(CMSEnvelopedGenerator.AES256_CBC, "AESMac");
    }

    /** M&eacute;todo que genera la MAC que se utiliza para cifrar los datos
     * autenticados.
     * @param encryptionOID
     *        OID del algoritmo que se utilizar&aacute; para generar la MAC.
     * @param content
     *        Contenido que se cifrar&aacute; con dicha MAC.
     * @return Cifrado MAC. */
    byte[] macGenerator(String encryptionOID, byte[] content, SecretKey cipherKey) throws Exception {
        MacOutputStream mOut = null;

        Security.addProvider(new BouncyCastleProvider());
        Provider provider = Security.getProvider("BC");

        Mac mac = getMac(encryptionOID, provider);

        AlgorithmParameterSpec params;

        params = generateParameterSpec(encryptionOID, cipherKey, provider);

        mac.init(cipherKey, params);

        ByteArrayOutputStream bOut = new ByteArrayOutputStream();
        mOut = new MacOutputStream(bOut, mac);
        mOut.write(content);

        return mOut.getMac();
    }

    Mac getMac(String macOID, Provider provider) throws NoSuchAlgorithmException, NoSuchPaddingException {
        try {
            return createMac(macOID, provider);
        }
        catch (NoSuchAlgorithmException e) {
            String alternate = MAC_ALG_NAMES.get(macOID);

            try {
                return createMac(alternate, provider);
            }
            catch (NoSuchAlgorithmException ex) {
                if (provider != null) {
                    return getMac(macOID, null); // roll back to default
                }
                throw e;
            }
        }
    }

    private Mac createMac(String algName, Provider provider) throws NoSuchAlgorithmException {
        if (provider != null) {
            return Mac.getInstance(algName, provider);
        }
        return Mac.getInstance(algName);
    }

    protected AlgorithmParameterSpec generateParameterSpec(String encryptionOID, SecretKey encKey, Provider encProvider) {
        SecureRandom rand = new SecureRandom();
        try {
            if (encryptionOID.equals(PKCSObjectIdentifiers.RC2_CBC.getId())) {
                byte[] iv = new byte[8];

                rand.nextBytes(iv);

                return new RC2ParameterSpec(encKey.getEncoded().length * 8, iv);
            }

            AlgorithmParameterGenerator pGen = this.createAlgorithmParameterGenerator(encryptionOID, encProvider);

            AlgorithmParameters p = pGen.generateParameters();

            return p.getParameterSpec(IvParameterSpec.class);
        }
        catch (GeneralSecurityException e) {
            return null;
        }
    }

    AlgorithmParameterGenerator createAlgorithmParameterGenerator(String encryptionOID, Provider provider) throws NoSuchAlgorithmException {
        try {
            return createAlgorithmParamsGenerator(encryptionOID, provider);
        }
        catch (NoSuchAlgorithmException e) {
            try {
                String algName = BASE_CIPHER_NAMES.get(encryptionOID);
                if (algName != null) {
                    return createAlgorithmParamsGenerator(algName, provider);
                }
            }
            catch (NoSuchAlgorithmException ex) {
                // ignore
            }
            //
            // can't try with default provider here as parameters must be from
            // the specified provider.
            //
            throw e;
        }
    }

    private AlgorithmParameterGenerator createAlgorithmParamsGenerator(String algName, Provider provider) throws NoSuchAlgorithmException {
        if (provider != null) {
            return AlgorithmParameterGenerator.getInstance(algName, provider);
        }
        return AlgorithmParameterGenerator.getInstance(algName);
    }

    protected AlgorithmIdentifier getAlgorithmIdentifier(String encryptionOID, AlgorithmParameterSpec paramSpec, Provider provider) throws IOException,
                                                                                                                                   NoSuchAlgorithmException,
                                                                                                                                   InvalidParameterSpecException {
        AlgorithmParameters params = this.createAlgorithmParameters(encryptionOID, provider);
        params.init(paramSpec);

        return getAlgorithmIdentifier(encryptionOID, params);
    }

    AlgorithmParameters createAlgorithmParameters(String encryptionOID, Provider provider) throws NoSuchAlgorithmException {
        try {
            return createAlgorithmParams(encryptionOID, provider);
        }
        catch (NoSuchAlgorithmException e) {
            try {
                String algName = BASE_CIPHER_NAMES.get(encryptionOID);
                if (algName != null) {
                    return createAlgorithmParams(algName, provider);
                }
            }
            catch (NoSuchAlgorithmException ex) {
                // ignore
            }
            //
            // can't try with default provider here as parameters must be from
            // the specified provider.
            //
            throw e;
        }
    }

    private AlgorithmParameters createAlgorithmParams(String algName, Provider provider) throws NoSuchAlgorithmException {
        if (provider != null) {
            return AlgorithmParameters.getInstance(algName, provider);
        }
        return AlgorithmParameters.getInstance(algName);
    }

    protected AlgorithmIdentifier getAlgorithmIdentifier(String encryptionOID, AlgorithmParameters params) throws IOException {
        DEREncodable asn1Params;
        if (params != null) {
            asn1Params = ASN1Object.fromByteArray(params.getEncoded("ASN.1"));
        }
        else {
            asn1Params = DERNull.INSTANCE;
        }

        return new AlgorithmIdentifier(new DERObjectIdentifier(encryptionOID), asn1Params);
    }

    /** Clase Auxiliar para el manejo de las MAC. Es Original de Bouncy Castle */
    protected static class MacOutputStream extends OutputStream {
        private final OutputStream out;
        private Mac mac;

        MacOutputStream(OutputStream out, Mac mac) {
            this.out = out;
            this.mac = mac;
        }

        @Override
        public void write(byte[] buf) throws IOException {
            mac.update(buf, 0, buf.length);
            out.write(buf, 0, buf.length);
        }

        @Override
        public void write(byte[] buf, int off, int len) throws IOException {
            mac.update(buf, off, len);
            out.write(buf, off, len);
        }

        @Override
        public void write(int i) throws IOException {
            mac.update((byte) i);
            out.write(i);
        }

        @Override
        public void close() throws IOException {
            out.close();
        }

        byte[] getMac() {
            return mac.doFinal();
        }
    }

}
