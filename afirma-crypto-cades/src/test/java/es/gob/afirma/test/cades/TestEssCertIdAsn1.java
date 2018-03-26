package es.gob.afirma.test.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.security.MessageDigest;

import org.junit.Ignore;
import org.junit.Test;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.DERNull;
import org.spongycastle.asn1.ess.ESSCertIDv2;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.IssuerSerial;

/** Pruebas de codificaci&oacute;n de ESSCertIDv2.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestEssCertIdAsn1 {

	private static final AlgorithmIdentifier SHA256 = new AlgorithmIdentifier(
		new ASN1ObjectIdentifier("2.16.840.1.101.3.4.2.1"), //$NON-NLS-1$
		DERNull.INSTANCE
	);

	/** Pruebas de codificaci&oacute;n de ESSCertIDv2.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Ignore
	@Test
	public void TestEssCertIdAsn1DefaultValue() throws Exception {
		final ESSCertIDv2 essCertIDv2WithOid = new ESSCertIDv2(
			SHA256,
			MessageDigest.getInstance("SHA-256").digest("DATA".getBytes()), //$NON-NLS-1$ //$NON-NLS-2$
			new IssuerSerial(
				new X500Name("cn=demo"), //$NON-NLS-1$
				new BigInteger("1") //$NON-NLS-1$
			)
		);
		try (
			final OutputStream fos = new FileOutputStream(File.createTempFile("WITH_OID_", ".der")) //$NON-NLS-1$ //$NON-NLS-2$
		) {
			fos.write(
				essCertIDv2WithOid.getEncoded()
			);
			fos.flush();
			fos.close();
		}

		final ESSCertIDv2 essCertIDv2WithoutOid = new ESSCertIDv2(
			null,
			MessageDigest.getInstance("SHA-256").digest("DATA".getBytes()), //$NON-NLS-1$ //$NON-NLS-2$
			new IssuerSerial(
				new X500Name("cn=demo"), //$NON-NLS-1$
				new BigInteger("1") //$NON-NLS-1$
			)
		);
		try (
			final OutputStream fos = new FileOutputStream(File.createTempFile("WITHOUT_OID_", ".der")) //$NON-NLS-1$ //$NON-NLS-2$
		) {
			fos.write(
				essCertIDv2WithoutOid.getEncoded()
			);
			fos.flush();
			fos.close();
		}
	}

}
