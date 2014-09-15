package es.gob.afirma.crypto.handwritten.wacom;

import java.math.BigInteger;
import java.security.Key;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import com.WacomGSS.STU.Tablet;
import com.WacomGSS.STU.Protocol.AsymmetricKeyType;
import com.WacomGSS.STU.Protocol.AsymmetricPaddingType;
import com.WacomGSS.STU.Protocol.SymmetricKeyType;

final class MyEncryptionHandler2 implements Tablet.IEncryptionHandler2 {

    private BigInteger n;
    private BigInteger d;
    private final BigInteger e;
    private Cipher     aesCipher;

    public MyEncryptionHandler2() {
      this.e = BigInteger.valueOf(65537);
    }

    @Override
    public void reset() {
      clearKeys();
      this.d = null;
      this.n = null;
    }

    @Override
    public void clearKeys() {
      this.aesCipher = null;
    }

    @Override
    public SymmetricKeyType getSymmetricKeyType() {
      return SymmetricKeyType.AES128;
      //return SymmetricKeyType.AES256; // requires "Java Crypotography Extension (JCE) Unlimited Strength Jurisdiction Policy Files"
    }

    @Override
    public AsymmetricPaddingType getAsymmetricPaddingType() {
      return AsymmetricPaddingType.None; // not recommended
      //return AsymmetricPaddingType.OAEP;
    }

    @Override
    public AsymmetricKeyType getAsymmetricKeyType() {
      return AsymmetricKeyType.RSA2048;
    }

    public static String toHex(final byte[] arr) {
      final StringBuilder sb = new StringBuilder(arr.length * 2);
      final java.util.Formatter formatter = new java.util.Formatter(sb);
      for (final byte b : arr) {
          formatter.format("%02x", Byte.valueOf(b)); //$NON-NLS-1$
      }
      formatter.close();
      return sb.toString();
    }

    @Override
    public byte[] getPublicExponent() {
      final byte[] ea = this.e.toByteArray();
      return ea;
    }

    @Override
    public byte[] generatePublicKey() {
      if (this.n != null) {
        return this.n.toByteArray();
      }

      // Generate your key pair here.
      // For speed and ease of demonstration, we use some precalulated values.
      // This is NOT recommended for production use!

      this.n = new BigInteger("93DDCD8BC9E478491C54413F0484FE79DDDA464A0F53AC043C6194FD473FB75B893C783F56701D2D30B021C4EE0401F058B98F035804CFBB0E67A8136A2F052A98037457460FAB7B3B148EC7C95604FF2192EA03FCC04285EC539DDF3375678E4C4D926163ABBC609C41EF5673C449DF5AC74FFA8150D33FC5436C5CC2621E642C42C10E71BF3895B07A52E7D86C84D3A9269462CF2E484E17D34DEDFF9090D6745A00EF40EE33C71C5688E856AF3C6C42AF3C4C8523711498F4508DC18BC5E24F38C2C7E971BA61BB24B19E3AE74D4D57023AF59BA9D979FCF48080E18D920E31A319C544DEA0E9DAF088E09B6098C07C20328DD0F62C5C99FCD2EB7C4F7CD3",16); //$NON-NLS-1$
      this.d = new BigInteger("2B1DD41FDCE1180A098EAFEFD63B8990B3964044BC2F63CB6067FBEFD6E4C76C9399E45E63B01171E9EE920A40753EB37CCBAEDE04BE726C5308FAC39E84D376D618BBC5EF1206A8CA537646DF788BC07163CB851A205DC57B61EE78F52258EDEF65F7371ABF2B10E8BF7930B655184D5EC51B972A3A0D3F5D2009EB0A6B5DFCD8DDD29CA704CDFF2086A211CFE7E0C395E9B53D5B1FF370BFC90C3A8255A64A8674E8FB41002838ABFC430EA558DECFFE1B563D96D06DCAEA8A5793DCA68C3FB4265BCE38CBEFBBAEB3B8FC1689F7B8510BF20B9D72E490887FB36F4722FEB813E6252DDC3BB17DA645ACEE8292AB85FA1A3048B7BBB34F3B50489BE7913421",16); //$NON-NLS-1$

      return this.n.toByteArray();
    }

    @Override
    public void computeSessionKey(final byte[] data) {

      final BigInteger c = new BigInteger(1, data);

      final BigInteger m = c.modPow(this.d, this.n);

      final int keySizeBytes = 128/8;

      byte[] k = m.toByteArray();
      if (k.length != keySizeBytes) {
        final byte[] k2 = new byte[keySizeBytes];
        System.arraycopy(k, k.length>keySizeBytes ? k.length-keySizeBytes : 0, k2, 0, k2.length);
        k = k2;
      }

      final Key aesKey = new SecretKeySpec(k, "AES"); //$NON-NLS-1$

      try {
        this.aesCipher = Cipher.getInstance("AES/ECB/NoPadding"); //$NON-NLS-1$
        this.aesCipher.init(Cipher.DECRYPT_MODE, aesKey);
        return;
      }
      catch (final Exception e1) {
      	e1.printStackTrace();
      }
      this.aesCipher = null;
    }

    @Override
    public byte[] decrypt(final byte[] data) {
      try {
        final byte[] decryptedData = this.aesCipher.doFinal(data);
        return decryptedData;
      }
      catch (final Exception e1) {
      	e1.printStackTrace();
      }
      return null;
    }

  }
