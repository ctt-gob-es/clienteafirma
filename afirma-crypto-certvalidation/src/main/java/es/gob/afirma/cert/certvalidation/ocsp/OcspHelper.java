package es.gob.afirma.cert.certvalidation.ocsp;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateHolder;
import org.bouncycastle.cert.ocsp.BasicOCSPResp;
import org.bouncycastle.cert.ocsp.CertificateID;
import org.bouncycastle.cert.ocsp.CertificateStatus;
import org.bouncycastle.cert.ocsp.OCSPException;
import org.bouncycastle.cert.ocsp.OCSPReqBuilder;
import org.bouncycastle.cert.ocsp.OCSPResp;
import org.bouncycastle.cert.ocsp.RespID;
import org.bouncycastle.cert.ocsp.RevokedStatus;
import org.bouncycastle.cert.ocsp.UnknownStatus;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.DigestCalculator;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;

import es.gob.afirma.cert.certvalidation.ValidationResult;
import es.gob.afirma.core.misc.AOUtil;

/** Utilidades varias para el servicio OCSP.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class OcspHelper {

	static {
		Security.addProvider(new BouncyCastleProvider());
	}

	private OcspHelper() {
		// No permitimos la instanciacion
	}

	private static class Sha1DigestCalculator implements DigestCalculator {
		private final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		private final MessageDigest digest;

		Sha1DigestCalculator() throws NoSuchAlgorithmException {
			this.digest = MessageDigest.getInstance("SHA-1"); //$NON-NLS-1$
		}

		@Override
		public OutputStream getOutputStream() {
			return this.baos;
		}

		@Override
		public byte[] getDigest() {
			final byte[] bytes = this.digest.digest(this.baos.toByteArray());
	        this.baos.reset();
	        return bytes;
		}

		@Override
		public AlgorithmIdentifier getAlgorithmIdentifier() {
			return RespID.HASH_SHA1;
		}
	}

	/** Obtiene una entrada a una clave privada de un almac&eacute;n en formato PKCS#12 / PFX.
	 * @param pfxFile Archivo PKCS#12 / PFX
	 * @param pfxPassword Contrase&ntilde;a del archivo PKCS#12 / PFX
	 * @param alias Alias del certificado a usar
	 * @return Entrada a una clave privada
	 * @throws KeyStoreException
	 * @throws NoSuchAlgorithmException
	 * @throws CertificateException
	 * @throws IOException
	 * @throws UnrecoverableEntryException */
	static PrivateKeyEntry getSignData(final String pfxFile,
			                    final String pfxPassword,
			                    final String alias) throws KeyStoreException,
	                                                       NoSuchAlgorithmException,
	                                                       CertificateException,
	                                                       IOException,
	                                                       UnrecoverableEntryException {
		if (pfxFile == null) {
			throw new IllegalArgumentException("Debe indicarse un nombre de almacen PKCS#12"); //$NON-NLS-1$
		}
		if (pfxPassword == null) {
			throw new IllegalArgumentException("Debe indicarse una contrasena para el almacen PKCS#12"); //$NON-NLS-1$
		}
		if (alias == null) {
			throw new IllegalArgumentException("Debe indicarse un alias de certificado contenido en el almacen PKCS#12"); //$NON-NLS-1$
		}
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(OcspHelper.class.getResourceAsStream(pfxFile), pfxPassword.toCharArray());

		if (!ks.containsAlias(alias)) {
			throw new IllegalArgumentException(
				"El almacen proporcionado no contiene ninguna entrada con el alias: " + alias //$NON-NLS-1$
			);
		}

		return (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(pfxPassword.toCharArray()));
	}

	/** Env&iacute;a una solicitud OSCP al <i>responder</i>.
	 * @param responderUrl URL del servidor OCSP
	 * @param ocspRequest Solicitud OCSP a enviar
	 * @return Respuesta del servidor OCSP
	 * @throws IOException */
	static byte[] sendOcspRequest(final URL responderUrl, final byte[] ocspRequest) throws IOException {
		if (responderUrl == null) {
			throw new IllegalArgumentException("La URL del servicio OCSP no puede ser nula"); //$NON-NLS-1$
		}
		if (ocspRequest == null) {
			throw new IllegalArgumentException("La peticion OCSP no puede ser nula"); //$NON-NLS-1$
		}

		final HttpURLConnection conn = (HttpURLConnection) responderUrl.openConnection();
		conn.setDoOutput(true);
		conn.setRequestMethod("POST"); //$NON-NLS-1$
		conn.setRequestProperty("Content-Type", "application/ocsp-request");  //$NON-NLS-1$//$NON-NLS-2$
		conn.setRequestProperty("Accept", "application/ocsp-response");  //$NON-NLS-1$//$NON-NLS-2$
		final OutputStream out = conn.getOutputStream();
		final DataOutputStream dataOut = new DataOutputStream(new BufferedOutputStream(out));
		dataOut.write(ocspRequest);
		dataOut.flush();
		dataOut.close();
		if (conn.getResponseCode() / 100 != 2) {
			throw new IOException("El servidor OCSP ha devuelto un codigo de error " + conn.getResponseCode()); //$NON-NLS-1$
		}
		final byte[] res;
		final InputStream in = (InputStream) conn.getContent();
		res = AOUtil.getDataFromInputStream(in);
		in.close();
		return res;
	}

	/** Crea una solicitud OCSP.
	 * @param certToValidate Certificado a validar
	 * @param issuerCert Certificado del emisor del certificado
	 * @return Solicitud en ASN.1 binario
	 * @throws OCSPException
	 * @throws NoSuchAlgorithmException
	 * @throws CertificateEncodingException
	 * @throws IOException */
	static byte[] createOcspRequest(final X509Certificate certToValidate,
                                           final X509Certificate issuerCert) throws CertificateEncodingException,
                                                                                    NoSuchAlgorithmException,
                                                                                    OCSPException,
                                                                                    IOException {
		final CertificateID certId = new CertificateID(
			new Sha1DigestCalculator(),
			new JcaX509CertificateHolder(issuerCert),
			certToValidate.getSerialNumber()
		);
		final OCSPReqBuilder ocspRequestBuilder = new OCSPReqBuilder();
		ocspRequestBuilder.addRequest(certId);
		return ocspRequestBuilder.build().getEncoded();
	}

	/** Crea una solicitud OCSP firmada.
	 * @param certToValidate Certificado a validar
	 * @param issuerCert Certificado del emisor del certificado
	 * @param requestSignKey Entrada a la clave privada para la firma de la solicitud
	 * @return Solicitud en ASN.1 binario
	 * @throws CertificateEncodingException
	 * @throws NoSuchAlgorithmException
	 * @throws OCSPException
	 * @throws OperatorCreationException
	 * @throws IOException */
	static byte[] createSignedOcspRequest(final X509Certificate certToValidate,
			                              final X509Certificate issuerCert,
			                              final PrivateKeyEntry requestSignKey) throws CertificateEncodingException,
			                                                                  	       NoSuchAlgorithmException,
			                                                                  	       OCSPException,
			                                                                  	       OperatorCreationException,
			                                                                  	       IOException {
		final CertificateID certId = new CertificateID(
			new Sha1DigestCalculator(),
			new JcaX509CertificateHolder(issuerCert),
			certToValidate.getSerialNumber()
		);

		final OCSPReqBuilder ocspRequestBuilder = new OCSPReqBuilder();
		ocspRequestBuilder.addRequest(certId);
		ocspRequestBuilder.setRequestorName(
			new X500Name(
				((X509Certificate)requestSignKey.getCertificate()).getSubjectX500Principal().toString()
			)
		);
		return ocspRequestBuilder.build(
			new JcaContentSignerBuilder("SHA1withRSA").build( //$NON-NLS-1$
				requestSignKey.getPrivateKey()
			),
			new X509CertificateHolder[] {
				new JcaX509CertificateHolder((X509Certificate) requestSignKey.getCertificate())
			}
		).getEncoded();
	}

	/** Analiza una respuesta OCSP.
	 * @param resp Respuesta OCSP
	 * @return Resultado de la validaci&oacute;n seg&uacute;n la respuesta OCSP
	 * @throws OCSPException
	 * @throws IOException */
	static ValidationResult analyzeOcspResponse(final byte[] resp) throws OCSPException, IOException {
		if (resp == null) {
			throw new IOException("La respuesta OCSP es nula"); //$NON-NLS-1$
		}
		final CertificateStatus certificateStatus = ((BasicOCSPResp) new OCSPResp(resp).getResponseObject()).getResponses()[0].getCertStatus();
		if (certificateStatus == CertificateStatus.GOOD) {
			return ValidationResult.VALID;
		}
		if (certificateStatus instanceof RevokedStatus) {
			return ValidationResult.REVOKED;
		}
		if (certificateStatus instanceof UnknownStatus) {
			return ValidationResult.UNKNOWN;
		}
		throw new IllegalArgumentException("La validacion ha devuelto un estado desconocido: " + certificateStatus.getClass().getName()); //$NON-NLS-1$
	}

}
