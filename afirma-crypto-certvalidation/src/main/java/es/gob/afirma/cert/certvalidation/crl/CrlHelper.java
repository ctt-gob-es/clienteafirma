package es.gob.afirma.cert.certvalidation.crl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.PublicKey;
import java.security.cert.CRLException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.InitialDirContext;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.x509.CRLDistPoint;
import org.bouncycastle.asn1.x509.DistributionPoint;
import org.bouncycastle.asn1.x509.DistributionPointName;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.X509Extension;

import es.gob.afirma.cert.certvalidation.ValidationResult;
import es.gob.afirma.core.misc.AOUtil;

/** Utilidades varias para el uso de lista de revocaci&oacute;n de certificados.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CrlHelper {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private CrlHelper() {
		// No permitimos la instanciacion
	}

	/** Valida un certificado mediante listas de revocaci&oacute;n.
	 * @param cert Certificado a validar
	 * @param vaPublicKey Clave p&uacute;blica de la autoridad de validaci&oacute;n.
	 *                    Si se indica <code>null</code> no se verifica la firma de las CRL
	 * @param overridingDistributionPoints Lista de puntos de distribuci&oacute;n de las listas
	 *                                     de revocaci&oacute;n. Si se indica <code>null</code> se
	 *                                     usar&aacute;n las indicadas en el propio certificado
	 * @return Resultado de la validaci&oacute;n */
	static ValidationResult verifyCertificateCRLs(final X509Certificate cert,
			                                      final PublicKey vaPublicKey,
			                                      final List<String> overridingDistributionPoints) {
		if (cert == null) {
			return ValidationResult.CORRUPT;
		}

		final List<String> crlDistPoints;
		try {
			crlDistPoints = overridingDistributionPoints == null || overridingDistributionPoints.isEmpty() ?
								getCrlDistributionPoints(cert) :
									overridingDistributionPoints;
		}
		catch (final IOException e) {
			LOGGER.severe("Error obteniendo los puntos de distribucion de CRL: " + e); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}

		LOGGER.info(
			"El certificado con serie '" + cert.getSerialNumber() + "' tiene asociadas las siguientes CRL: " + crlDistPoints //$NON-NLS-1$ //$NON-NLS-2$
		);

		final CertificateFactory cf;
		try {
			cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		}
		catch (final CertificateException e1) {
			LOGGER.severe("Error instanciando la factoria de certificados: " + e1); //$NON-NLS-1$
			return ValidationResult.SERVER_ERROR;
		}

		boolean checked = false;
		for (final String crlDP : crlDistPoints) {

			// Descargamos
			final byte[] crlBytes;
			try {
				crlBytes = downloadCRL(crlDP);
			}
			catch (final Exception e1) {
				LOGGER.severe("No se ha podido descargar la CRL (" + crlDP + "), se continuara con el siguiente punto de distribucion: " + e1); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}

			final X509CRL crl;
			try {
				crl = (X509CRL)cf.generateCRL(new ByteArrayInputStream(crlBytes));
			}
			catch (final Exception e) {
				LOGGER.severe("Error analizando la lista de revocacion: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
			// Comprobamos la firma de la CRL
			try {
				crl.verify(vaPublicKey);
			}
			catch (final Exception e) {
				LOGGER.severe("No se ha podido comprobar la firma de la CRL: " + e); //$NON-NLS-1$
				return ValidationResult.SERVER_ERROR;
			}
			if (crl.isRevoked(cert)) {
				return ValidationResult.REVOKED;
			}

			checked = true;
		}

		if (checked) {
			return ValidationResult.VALID;
		}
		return ValidationResult.UNKNOWN;
	}

	private static byte[] downloadCRL(final String crlURL) throws CRLException,
	                                                              IOException,
	                                                              NamingException,
	                                                              URISyntaxException {
	 	if (crlURL.startsWith("http://") || crlURL.startsWith("https://") || crlURL.startsWith("ftp://")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	 		return downloadCRLFromWeb(crlURL);
	 	}
	 	else if (crlURL.startsWith("ldap://")) { //$NON-NLS-1$
	 		return downloadCRLFromLDAP(crlURL);
	 	}
	 	else if (crlURL.startsWith("file:/")) { //$NON-NLS-1$
	 		return downloadCRLFromFile(crlURL);
	 	}
	 	throw new CRLException(
			"No se soporta el protocolo del punto de distribucion de CRL: " + crlURL //$NON-NLS-1$
		);
	}

	private static byte[] downloadCRLFromFile(final String ldapURL) throws IOException, URISyntaxException {
		return AOUtil.getDataFromInputStream(AOUtil.loadFile(new URI(ldapURL)));
	}

	private static byte[] downloadCRLFromLDAP(final String ldapURL) throws NamingException {
	 	final Hashtable<String , String> env = new Hashtable<String , String>();
	 	env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory"); //$NON-NLS-1$
	 	env.put(Context.PROVIDER_URL, ldapURL);

	 	final Attribute aval = new InitialDirContext(env).getAttributes("").get("certificateRevocationList;binary"); //$NON-NLS-1$ //$NON-NLS-2$
	 	final byte[] val = (byte[])aval.get();
	 	if (val == null || val.length == 0) {
	 		throw new NamingException("No se ha podido descargar la CRL desde " + ldapURL); //$NON-NLS-1$
	 	}
		return val;
	}

	private static byte[] downloadCRLFromWeb(final String crlURL) throws IOException {
	 	final URL url = new URL(crlURL);
	 	final InputStream crlStream = url.openStream();
		final byte[] ret = AOUtil.getDataFromInputStream(crlStream);
		crlStream.close();
		return ret;
	}

	private static List<String> getCrlDistributionPoints(final X509Certificate cert) throws IOException {
		final byte[] crldpExt = cert.getExtensionValue(X509Extension.cRLDistributionPoints.getId());
		if (crldpExt == null) {
			return new ArrayList<String>();
		}
		final ASN1Primitive derObjCrlDP;
		final ASN1InputStream oAsnInStream = new ASN1InputStream(new ByteArrayInputStream(crldpExt));
		derObjCrlDP = oAsnInStream.readObject();
		final byte[] crldpExtOctets = ((DEROctetString) derObjCrlDP).getOctets();
		final ASN1Primitive derObj2;
		final ASN1InputStream oAsnInStream2 = new ASN1InputStream(new ByteArrayInputStream(crldpExtOctets));
		derObj2 = oAsnInStream2.readObject();
		final CRLDistPoint distPoint = CRLDistPoint.getInstance(derObj2);
		final List<String> crlUrls = new ArrayList<String>();
		for (final DistributionPoint dp : distPoint.getDistributionPoints()) {
			final DistributionPointName dpn = dp.getDistributionPoint();
		 	// Buscamos URIs en el fullName
		 	if (dpn != null) {
		 		if (dpn.getType() == DistributionPointName.FULL_NAME) {
		 			final GeneralName[] genNames = GeneralNames.getInstance(dpn.getName()).getNames();
		 			// Buscamos la URI
		 			for (final GeneralName genName : genNames) {
		 				if (genName.getTagNo() == GeneralName.uniformResourceIdentifier) {
		 					crlUrls.add(DERIA5String.getInstance(genName.getName()).getString());
		 				}
		 			}
			 	}
			}
		}
		oAsnInStream.close();
		oAsnInStream2.close();
		return crlUrls;
	}
}
