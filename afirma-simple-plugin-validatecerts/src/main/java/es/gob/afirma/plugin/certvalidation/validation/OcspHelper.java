/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

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
import java.util.ArrayList;
import java.util.List;

import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.DERIA5String;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AccessDescription;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.AuthorityInformationAccess;
import org.spongycastle.asn1.x509.Extension;
import org.spongycastle.asn1.x509.GeneralName;
import org.spongycastle.cert.X509CertificateHolder;
import org.spongycastle.cert.jcajce.JcaX509CertificateHolder;
import org.spongycastle.cert.ocsp.BasicOCSPResp;
import org.spongycastle.cert.ocsp.CertificateID;
import org.spongycastle.cert.ocsp.CertificateStatus;
import org.spongycastle.cert.ocsp.OCSPException;
import org.spongycastle.cert.ocsp.OCSPReqBuilder;
import org.spongycastle.cert.ocsp.OCSPResp;
import org.spongycastle.cert.ocsp.RespID;
import org.spongycastle.cert.ocsp.RevokedStatus;
import org.spongycastle.cert.ocsp.SingleResp;
import org.spongycastle.cert.ocsp.UnknownStatus;
import org.spongycastle.jce.provider.BouncyCastleProvider;
import org.spongycastle.operator.DigestCalculator;
import org.spongycastle.operator.OperatorCreationException;
import org.spongycastle.operator.jcajce.JcaContentSignerBuilder;

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
	 * @param pfxFile Archivo PKCS#12 / PFX.
	 * @param pfxPassword Contrase&ntilde;a del archivo PKCS#12 / PFX.
	 * @param alias Alias del certificado a usar.
	 * @return Entrada a una clave privada.
	 * @throws KeyStoreException Si hay problemas en el tratamiento del almac&eacute;n de claves.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws CertificateException Si hay problemas tratando los certificados.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws UnrecoverableEntryException Si una entrada del almac&eacute;n de claves es inaccesible. */
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
		try (
			final InputStream is = OcspHelper.class.getResourceAsStream(pfxFile)
		) {
			ks.load(is, pfxPassword.toCharArray());
		}

		if (!ks.containsAlias(alias)) {
			throw new IllegalArgumentException(
				"El almacen proporcionado no contiene ninguna entrada con el alias: " + alias //$NON-NLS-1$
			);
		}

		return (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(pfxPassword.toCharArray()));
	}

	/** Env&iacute;a una solicitud OSCP al <i>responder</i>.
	 * @param responderUrl URL del servidor OCSP.
	 * @param ocspRequest Solicitud OCSP a enviar.
	 * @return Respuesta del servidor OCSP.
	 * @throws IOException Si hay problemas en el tratamiento de datos o en la red. */
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
		try (
			final OutputStream out = conn.getOutputStream();
			final DataOutputStream dataOut = new DataOutputStream(new BufferedOutputStream(out))
		) {
			dataOut.write(ocspRequest);
		}
		if (conn.getResponseCode() / 100 != 2) {
			throw new IOException("El servidor OCSP ha devuelto un codigo de error " + conn.getResponseCode()); //$NON-NLS-1$
		}
		try (
			final InputStream in = (InputStream) conn.getContent()
		) {
			return AOUtil.getDataFromInputStream(in);
		}
	}

	/** Crea una solicitud OCSP.
	 * @param certToValidate Certificado a validar.
	 * @param issuerCert Certificado del emisor del certificado.
	 * @return Solicitud en ASN.1 binario.
	 * @throws OCSPException Si hay problemas accediendo al servicio OCSP.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws CertificateEncodingException Si hay problemas en el tratamiento de los certificados.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
	static byte[] createOcspRequest(final X509Certificate certToValidate,
                                    final X509Certificate issuerCert) throws CertificateEncodingException,
                                                                             NoSuchAlgorithmException,
                                                                             OCSPException,
                                                                             IOException {
		final CertificateID certId = new CertificateID(
			new Sha1DigestCalculator(),
			new JcaX509CertificateHolder(issuerCert != null ? issuerCert : certToValidate),
			certToValidate.getSerialNumber()
		);
		final OCSPReqBuilder ocspRequestBuilder = new OCSPReqBuilder();
		ocspRequestBuilder.addRequest(certId);
		return ocspRequestBuilder.build().getEncoded();
	}

	/** Crea una solicitud OCSP firmada.
	 * @param certToValidate Certificado a validar.
	 * @param issuerCert Certificado del emisor del certificado.
	 * @param requestSignKey Entrada a la clave privada para la firma de la solicitud.
	 * @return Solicitud en ASN.1 binario.
	 * @throws CertificateEncodingException Si hay problemas en el tratamiento de los certificados.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario.
	 * @throws OCSPException Si hay problemas accediendo al servicio OCSP.
	 * @throws OperatorCreationException Si hay problemas creando los comandos OCSP.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
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
	 * <pre>
	 * OCSPResponse ::= SEQUENCE {
     *  responseStatus         OCSPResponseStatus,
     *  responseBytes          [0] EXPLICIT ResponseBytes OPTIONAL
     * }
     *
     * OCSPResponseStatus ::= ENUMERATED {
     *  successful            (0),      --Response has valid confirmations
     *  malformedRequest      (1),      --Illegal confirmation request
     *  internalError         (2),      --Internal error in issuer
     *  tryLater              (3),      --Try again later
     *                                  --(4) is not used
     *  sigRequired           (5),      --Must sign the request
     *  unauthorized          (6)       --Request unauthorized
     * }
	 * </pre>
	 * @param resp Respuesta OCSP.
	 * @return Resultado de la validaci&oacute;n seg&uacute;n la respuesta OCSP.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws OCSPException Si hay problemas accediendo al servicio OCSP. */
	static ValidationResult analyzeOcspResponse(final byte[] resp) throws OCSPException, IOException {
		if (resp == null) {
			throw new IOException("La respuesta OCSP es nula"); //$NON-NLS-1$
		}

		final OCSPResp ocspResponse = new OCSPResp(resp);

		if (ocspResponse.getStatus() == OCSPResp.SUCCESSFUL) {
			final SingleResp[] responses = ((BasicOCSPResp) ocspResponse.getResponseObject()).getResponses();
			final CertificateStatus certificateStatus = responses[0].getCertStatus();
			if (certificateStatus == CertificateStatus.GOOD) {
				return ValidationResult.VALID;
			}
			if (certificateStatus instanceof RevokedStatus) {
				return ValidationResult.REVOKED;
			}
			if (certificateStatus instanceof UnknownStatus) {
				return ValidationResult.UNKNOWN;
			}
			throw new IllegalArgumentException("La validacion ha devuelto una respuesta desconocida: " + certificateStatus.getClass().getName()); //$NON-NLS-1$
		}
		if (ocspResponse.getStatus() == OCSPResp.UNAUTHORIZED) {
			return ValidationResult.UNAUTHORIZED;
		}
		if (ocspResponse.getStatus() == OCSPResp.INTERNAL_ERROR || ocspResponse.getStatus() == OCSPResp.TRY_LATER) {
			return ValidationResult.SERVER_ERROR;
		}
		if (ocspResponse.getStatus() == OCSPResp.MALFORMED_REQUEST) {
			return ValidationResult.MALFORMED_REQUEST;
		}
		if (ocspResponse.getStatus() == OCSPResp.SIG_REQUIRED) {
			return ValidationResult.SIG_REQUIRED;
		}
		throw new IllegalArgumentException("La validacion ha devuelto un estado desconocido: " + ocspResponse.getStatus()); //$NON-NLS-1$
	}

    static List<String> getAIALocations(final X509Certificate cert) throws IOException {
        final byte[] aiaExtensionValue = cert.getExtensionValue(Extension.authorityInfoAccess.getId());
        if (aiaExtensionValue == null) {
            return new ArrayList<>(0);
        }

        try (
    		final ASN1InputStream asn1In = new ASN1InputStream(aiaExtensionValue);
		) {
        	final DEROctetString aiaDEROctetString = (DEROctetString) asn1In.readObject();
        	try (
    			final ASN1InputStream asn1InOctets = new ASN1InputStream(aiaDEROctetString.getOctets())
			) {
        		final ASN1Sequence aiaASN1Sequence = (ASN1Sequence) asn1InOctets.readObject();
        		final AuthorityInformationAccess authorityInformationAccess = AuthorityInformationAccess.getInstance(aiaASN1Sequence);
                final List<String> ocspUrlList = new ArrayList<>();
                final AccessDescription[] accessDescriptions = authorityInformationAccess.getAccessDescriptions();
                for (final AccessDescription accessDescription : accessDescriptions) {
                    final GeneralName gn = accessDescription.getAccessLocation();
                    if (gn.getTagNo() == GeneralName.uniformResourceIdentifier) {
                        final DERIA5String str = DERIA5String.getInstance(gn.getName());
                        final String accessLocation = str.getString();
                        ocspUrlList.add(accessLocation);
                    }
                }
                if (ocspUrlList.isEmpty()) {
                	return new ArrayList<>(0);
                }
                return ocspUrlList;
        	}
        }
    }

}
