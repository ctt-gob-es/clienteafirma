package es.gob.afirma.crypto.handwritten.wacom;

import java.math.BigInteger;
import java.security.Key;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import com.WacomGSS.STU.Tablet;
import com.WacomGSS.STU.Protocol.DHbase;
import com.WacomGSS.STU.Protocol.DHprime;
import com.WacomGSS.STU.Protocol.PublicKey;

final class MyEncryptionHandler implements Tablet.IEncryptionHandler {

    private BigInteger p;
    private BigInteger g;
    private BigInteger privateKey;
    private Cipher     aesCipher;

    @Override
    public void reset() {
      clearKeys();
      this.p = this.g = null;
    }

    @Override
    public void clearKeys() {
      this.privateKey = null;
      this.aesCipher = null;
    }

    @Override
    public boolean requireDH() {
      return this.p == null || this.g == null;
    }

    @Override
    public void setDH(final DHprime dhPrime, final DHbase dhBase) {
      this.p = new BigInteger(1, dhPrime.getValue());
      this.g = new BigInteger(1, dhBase.getValue());
    }

    @Override
    public PublicKey generateHostPublicKey() {
      this.privateKey = new BigInteger("F965BC2C949B91938787D5973C94856C", 16); // should be randomly chosen according to DH rules. //$NON-NLS-1$

      final BigInteger publicKey_bi = this.g.modPow(this.privateKey, this.p);
      try {
        final PublicKey publicKey = new PublicKey(publicKey_bi.toByteArray());
        return publicKey;
      }
      catch (final Exception e) {
      	e.printStackTrace();
      }
      return null;
    }

    @Override
    public void computeSharedKey(final PublicKey devicePublicKey) {
      final BigInteger devicePublicKey_bi = new BigInteger(devicePublicKey.getValue());
      final BigInteger sharedKey = devicePublicKey_bi.modPow(this.privateKey, this.p);

      byte[] array = sharedKey.toByteArray();
      if (array[0] == 0) {
        final byte[] tmp = new byte[array.length - 1];
        System.arraycopy(array, 1, tmp, 0, tmp.length);
        array = tmp;
      }

      try {
        final Key aesKey = new SecretKeySpec(array, "AES"); //$NON-NLS-1$

        this.aesCipher = Cipher.getInstance("AES/ECB/NoPadding"); //$NON-NLS-1$
        this.aesCipher.init(Cipher.DECRYPT_MODE, aesKey);
        return;
      }
      catch (final Exception e) {
      	e.printStackTrace();
      }
      this.aesCipher = null;
    }

    @Override
    public byte[] decrypt(final byte[] data) {
      try {
        final byte[] decryptedData = this.aesCipher.doFinal(data);
        return decryptedData;
      }
      catch (final Exception e) {
      	e.printStackTrace();
      }
      return null;
    }

  }
