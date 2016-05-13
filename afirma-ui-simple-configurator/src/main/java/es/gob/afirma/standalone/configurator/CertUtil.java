//In latest Java 8, there are some changes with respect to the source code structure. Now you need to use below command to compile the source code to remove the errors ad warnings:
//javac -XDignore.symbol.file
//When javac is compiling code it doesn't link against rt.jar by default in latest Java 8. Instead it uses special symbol file lib/ct.sym with class stubs. The option -XDignore.symbol.file is to ignore the symbol file so that it will link against rt.jar.

package es.gob.afirma.standalone.configurator;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
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
import sun.security.x509.X500Name;

/** Utilidades para la creaci&oacute;n de certificados X.509.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CertUtil {

	private static final String AF_ROOT_SUBJECT_PRINCIPAL = "CN=AutoFirma ROOT"; //$NON-NLS-1$

	private static final int KEY_SIZE = 2048;
	private static final String SIGNATURE_ALGORITHM = "SHA256withRSA"; //$NON-NLS-1$

	static class CertPack {

		private final Certificate sslCert;
		private final PrivateKey prK;
		private final String alias;
		private final char[] password;
		private byte[] p12 = null;
		private final Certificate caCert;

		CertPack(final Certificate caCertificate,
				 final PrivateKeyEntry sslCertificatePrivateKeyEntry,
				 final String sslCertificateAlias,
				 final char[] storePassword) {

			this.sslCert = sslCertificatePrivateKeyEntry.getCertificate();
			this.caCert = caCertificate;
			this.prK = sslCertificatePrivateKeyEntry.getPrivateKey();
			this.alias = sslCertificateAlias;
			this.password = storePassword;
		}

		Certificate getSslCertificate() {
			return this.sslCert;
		}

		Certificate getCaCertificate() {
			return this.caCert;
		}

		byte[] getPkcs12() throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException {
			if (this.p12 == null) {
				final KeyStore keyStore = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
				keyStore.load(null, null);
				keyStore.setKeyEntry(
					this.alias,
					this.prK,
					this.password,
					new Certificate[] { this.sslCert, this.caCert }
				);
				final ByteArrayOutputStream baos = new ByteArrayOutputStream();
				keyStore.store(baos, this.password);
				this.p12 = baos.toByteArray();
			}
			return this.p12;
		}
	}

	static CertPack getCertPackForLocalhostSsl(final String sslCertificateAlias, final String storePassword) throws InvalidKeyException, NoSuchAlgorithmException, NoSuchProviderException, CertificateException, SignatureException, IOException {

		final PrivateKeyEntry caCertificatePrivateKeyEntry = generateCaCertificate(
			AF_ROOT_SUBJECT_PRINCIPAL
		);
		final PrivateKeyEntry sslCertificatePrivateKeyEntry = generateSslCertificate(
			"127.0.0.1", //$NON-NLS-1$
			"127.0.0.1", //$NON-NLS-1$
			"localhost", //$NON-NLS-1$
			caCertificatePrivateKeyEntry
		);
		return new CertPack(
			caCertificatePrivateKeyEntry.getCertificate(),
			sslCertificatePrivateKeyEntry,
			sslCertificateAlias,
			storePassword.toCharArray()
		);

	}

	private static PrivateKeyEntry generateCaCertificate(final String subjectPrincipal) throws NoSuchAlgorithmException,
                                                                                               NoSuchProviderException,
                                                                                               InvalidKeyException,
                                                                                               CertificateException,
                                                                                               SignatureException,
                                                                                               IOException {
		// Generamos el par de claves...
        final CertAndKeyGen keyGen = new CertAndKeyGen("RSA", SIGNATURE_ALGORITHM, null); //$NON-NLS-1$
        keyGen.generate(KEY_SIZE);
        final PrivateKey rootPrivateKey = keyGen.getPrivateKey();

        final X509Certificate rootCertificate = keyGen.getSelfCertificate(
    		new X500Name(subjectPrincipal),
    		new Date(),
    		(long)10*365*24*3600
		);

        return new PrivateKeyEntry(
			rootPrivateKey,
			new Certificate[] {
				signCertificate(
					rootCertificate,
					rootCertificate,
					rootPrivateKey,
					true
				)
			}
		);
	}

	private static PrivateKeyEntry generateSslCertificate(final String cn,
			                                              final String ipaddress,
			                                              final String dnsname,
			                                              final PrivateKeyEntry issuerKeyEntry) throws NoSuchAlgorithmException, InvalidKeyException, IOException, CertificateException, SignatureException, NoSuchProviderException {

		// Generamos las claves...
		final CertAndKeyGen keyGen2 = new CertAndKeyGen("RSA", SIGNATURE_ALGORITHM); //$NON-NLS-1$
        keyGen2.generate(KEY_SIZE);

        //Definicion de propiedades del certificado
        final CertificateExtensions certExts = new CertificateExtensions();

        //Marcamos todos los usos posibles para el certificado ssl
        certExts.set(
    		KeyUsageExtension.NAME,
    		new KeyUsageExtension(
    			new boolean[] {
        			true, true, true, true, true, true, true, true, true
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

        final X509Certificate sslCertificate = keyGen2.getSelfCertificate(
    		new X500Name("CN=" + cn), //$NON-NLS-1$
    		new Date(),
    		(long)10*365*24*3600,
    		certExts
	    );

        return new PrivateKeyEntry(
    		keyGen2.getPrivateKey(),
    		new Certificate[] {
				signCertificate(
		    		sslCertificate,
		    		(X509Certificate) issuerKeyEntry.getCertificate(),
		    		issuerKeyEntry.getPrivateKey(),
		    		false
	    		)
    		}
		);

	}

	private static Certificate signCertificate(final X509Certificate certificateToBeSigned,
                                       final X509Certificate issuerCertificate,
                                       final PrivateKey issuerPrivateKey,
                                       final boolean isCA) throws CertificateException, IOException, InvalidKeyException, NoSuchAlgorithmException, NoSuchProviderException, SignatureException {

        final byte[] inCertBytes = certificateToBeSigned.getTBSCertificate();
        final sun.security.x509.X509CertInfo info = new sun.security.x509.X509CertInfo(inCertBytes);

        info.set(
    		sun.security.x509.X509CertInfo.ISSUER,
    		issuerCertificate.getSubjectDN()
		);

        if(isCA) {
            //Extensiones del certificado
            final CertificateExtensions ext = new CertificateExtensions();
            ext.set(
            		KeyUsageExtension.NAME,
            		new KeyUsageExtension(
        				new boolean[] {
        					true, true, true, true, true, true, true, true, true
        				}
        			)
        		);

            final sun.security.x509.BasicConstraintsExtension bce = new sun.security.x509.BasicConstraintsExtension(true, -1);

            ext.set(sun.security.x509.BasicConstraintsExtension.NAME,
            		new sun.security.x509.BasicConstraintsExtension(
            		Boolean.FALSE,
            		bce.getExtensionValue()
            		)
            );

           ext.set(sun.security.x509.SubjectKeyIdentifierExtension.NAME,
                    new sun.security.x509.SubjectKeyIdentifierExtension(
                    new sun.security.x509.KeyIdentifier(certificateToBeSigned.getPublicKey()).getIdentifier()
                    )
        	);
            //Resto de atributos para la CA
            info.set(sun.security.x509.X509CertInfo.VERSION,
                    new sun.security.x509.CertificateVersion(sun.security.x509.CertificateVersion.V3));
            info.set(sun.security.x509.X509CertInfo.SERIAL_NUMBER,
                    new sun.security.x509.CertificateSerialNumber((int) (new Date().getTime() / 1000)));
            final sun.security.x509.AlgorithmId algID = new sun.security.x509.AlgorithmId(sun.security.x509.AlgorithmId.sha256WithRSAEncryption_oid);
            info.set(sun.security.x509.X509CertInfo.ALGORITHM_ID,
                    new sun.security.x509.CertificateAlgorithmId(algID));
            info.set(sun.security.x509.X509CertInfo.KEY, new sun.security.x509.CertificateX509Key(certificateToBeSigned.getPublicKey()));
            info.set(sun.security.x509.X509CertInfo.EXTENSIONS, ext);
        }
        final sun.security.x509.X509CertImpl outCert = new sun.security.x509.X509CertImpl(info);
        outCert.sign(
    		issuerPrivateKey,
    		issuerCertificate.getSigAlgName()
		);

        return outCert;
	}

}
