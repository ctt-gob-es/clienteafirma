package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.x509.Certificate;

import es.gob.afirma.signers.pkcs7.SigUtils;

final class CAdESMultiUtil {

	private CAdESMultiUtil() {
		// No instanciable
	}

	static ASN1Set addCertificates(final SignedData sd, final java.security.cert.Certificate[] certChain) throws CertificateEncodingException, IOException {
		ASN1Set certificates = null;

		final ASN1Set certificatesSigned = sd.getCertificates();
		final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
		final Enumeration<?> certs = certificatesSigned.getObjects();

		// COJEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
		while (certs.hasMoreElements()) {
			vCertsSig.add((ASN1Encodable) certs.nextElement());
		}

		// Y ANADIMOS LOS DE LA NUEVA CADENA
		if (certChain.length != 0) {
			final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
			for (final java.security.cert.Certificate element : certChain) {
				ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
			}
			certificates = SigUtils.fillRestCerts(ce, vCertsSig);
		}
		return certificates;
	}

}
