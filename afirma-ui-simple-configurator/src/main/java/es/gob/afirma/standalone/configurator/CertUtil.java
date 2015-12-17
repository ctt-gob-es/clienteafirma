//In latest Java 8, there are some changes with respect to the source code structure. Now you need to use below command to compile the source code to remove the errors ad warnings:
//javac -XDignore.symbol.file
//When javac is compiling code it doesn't link against rt.jar by default in latest Java 8. Instead it uses special symbol file lib/ct.sym with class stubs. The option -XDignore.symbol.file is to ignore the symbol file so that it will link against rt.jar.

package es.gob.afirma.standalone.configurator;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.Vector;

import sun.security.tools.keytool.CertAndKeyGen;
import sun.security.x509.CertificateExtensions;
import sun.security.x509.ExtendedKeyUsageExtension;
import sun.security.x509.GeneralName;
import sun.security.x509.KeyUsageExtension;

/** Utilidades para la creaci&oacute;n de certificados X.509.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CertUtil {

	private static final int KEY_SIZE = 2048;

	static class CertPack {

		private final X509Certificate cert;
		private final PrivateKey prK;
		private final String alias;
		private final char[] password;
		private byte[] p12 = null;

		CertPack(final X509Certificate c, final PrivateKey pk, final String a, final char[] p) {
			this.cert = c;
			this.prK = pk;
			this.alias = a;
			this.password = p;
		}

		X509Certificate getCertificate() {
			return this.cert;
		}

		byte[] getPkcs12() throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException {
			if (this.p12 == null) {
				final KeyStore keyStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
				keyStore.load(null);
				keyStore.setKeyEntry(
					this.alias,
					this.prK,
					this.password,
					new Certificate[] { this.cert }
				);
				final ByteArrayOutputStream baos = new ByteArrayOutputStream();
				keyStore.store(baos, this.password);
				this.p12 = baos.toByteArray();
			}
			return this.p12;
		}

	}

	static CertPack generateSSLCertificate(final String cn,
                                           final String alias,
                                           final char[] password,
                                           final boolean asCa ) throws NoSuchAlgorithmException,
                                                                          NoSuchProviderException,
                                                                          InvalidKeyException,
                                                                          CertificateException,
                                                                          SignatureException,
                                                                          IOException {
		return generateSSLCertificate(cn, alias, password, asCa, "127.0.0.1", "localhost"); //$NON-NLS-1$ //$NON-NLS-2$
	}


	static CertPack generateSSLCertificate(final String cn,
			                               final String alias,
			                               final char[] password,
			                               final boolean asCa,
			                               final String ipaddress,
			                               final String dnsname
			                               ) throws NoSuchAlgorithmException,
                                                                        NoSuchProviderException,
                                                                        InvalidKeyException,
                                                                        CertificateException,
                                                                        SignatureException,
                                                                        IOException {

		final CertAndKeyGen keyGen=new CertAndKeyGen("RSA","SHA256WithRSA"); //$NON-NLS-1$ //$NON-NLS-2$
        keyGen.generate(KEY_SIZE);

        final CertificateExtensions certExts = new CertificateExtensions();

        certExts.set(
    		KeyUsageExtension.NAME,
    		new KeyUsageExtension(
				new boolean[] {
					true
				}
			)
		);

        final Vector<sun.security.util.ObjectIdentifier> keyUsages = new Vector<>(1);
        keyUsages.add(new sun.security.util.ObjectIdentifier("1.3.6.1.5.5.7.3.1")); //$NON-NLS-1$
        keyUsages.add(new sun.security.util.ObjectIdentifier("1.3.6.1.5.5.7.3.2")); //$NON-NLS-1$
        final sun.security.x509.Extension extendedKeyUsageExtension = new ExtendedKeyUsageExtension(keyUsages);
        certExts.set(
    		ExtendedKeyUsageExtension.NAME,
    		extendedKeyUsageExtension
		);

        if (asCa) {
        	certExts.set(
        			sun.security.x509.BasicConstraintsExtension.NAME,
        			new sun.security.x509.BasicConstraintsExtension(true, 1)
        			);
        }

        final sun.security.x509.GeneralNames generalNames = new sun.security.x509.GeneralNames();
        generalNames.add(
    		new GeneralName(
        		new sun.security.x509.DNSName(dnsname)
    		)
		);
        if (ipaddress != null && !ipaddress.isEmpty()) {
	        generalNames.add(
	    		new GeneralName(
	        		new sun.security.x509.IPAddressName(ipaddress)
	    		)
			);
        }

		return new CertPack(
			keyGen.getSelfCertificate(
	    		new sun.security.x509.X500Name("CN=" + cn), //$NON-NLS-1$
	    		new Date(),
	    		(long)10*365*24*3600,
	    		certExts
    		),
    		keyGen.getPrivateKey(),
    		alias,
    		password
		);
	}
}
