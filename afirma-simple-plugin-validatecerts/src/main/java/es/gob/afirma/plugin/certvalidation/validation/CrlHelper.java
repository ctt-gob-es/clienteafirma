/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
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

import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.DERIA5String;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.x509.CRLDistPoint;
import org.spongycastle.asn1.x509.DistributionPoint;
import org.spongycastle.asn1.x509.DistributionPointName;
import org.spongycastle.asn1.x509.Extension;
import org.spongycastle.asn1.x509.GeneralName;
import org.spongycastle.asn1.x509.GeneralNames;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.http.DataDownloader;

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

		if (crlDistPoints == null || crlDistPoints.isEmpty()) {
			LOGGER.warning(
				"El certificado con serie '" + cert.getSerialNumber() + "' no tiene CRL asociadas" //$NON-NLS-1$ //$NON-NLS-2$
			);
			return ValidationResult.UNKNOWN;
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
		boolean cannotDownload = false;
		for (final String crlDP : crlDistPoints) {

			// Descargamos
			final byte[] crlBytes;
			try {
				crlBytes = downloadCrl(crlDP);
			}
			catch (final Exception e1) {
				LOGGER.severe(
					"No se ha podido descargar la CRL (" + crlDP + "), se continuara con el siguiente punto de distribucion: " + e1 //$NON-NLS-1$ //$NON-NLS-2$
				);
				cannotDownload = true;
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
			if (vaPublicKey != null) {
				try {
					crl.verify(vaPublicKey);
				}
				catch (final Exception e) {
					LOGGER.severe("No se ha podido comprobar la firma de la CRL: " + e); //$NON-NLS-1$
					return ValidationResult.SERVER_ERROR;
				}
			}
			if (crl.isRevoked(cert)) {
				return ValidationResult.REVOKED;
			}

			checked = true;
		}

		if (checked) {
			return ValidationResult.VALID;
		}

		if (cannotDownload) {
			return ValidationResult.CANNOT_DOWNLOAD_CRL;
		}

		return ValidationResult.UNKNOWN;
	}

	private static byte[] downloadCrl(final String crlURL) throws CRLException,
	                                                              IOException,
	                                                              NamingException,
	                                                              URISyntaxException {
	 	if (crlURL.startsWith("http://") || crlURL.startsWith("https://") || crlURL.startsWith("ftp://")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	 		return downloadCRLFromWeb(crlURL);
	 	}
		if (crlURL.startsWith("ldap://")) { //$NON-NLS-1$
	 		return downloadCrlFromLdap(crlURL);
	 	}
		if (crlURL.startsWith("file:/")) { //$NON-NLS-1$
	 		return downloadCrlFromFile(crlURL);
	 	}
	 	throw new CRLException(
			"No se soporta el protocolo del punto de distribucion de CRL: " + crlURL //$NON-NLS-1$
		);
	}

	private static byte[] downloadCrlFromFile(final String ldapURL) throws IOException, URISyntaxException {
		try (
			final InputStream is = AOUtil.loadFile(new URI(ldapURL))
		) {
			return AOUtil.getDataFromInputStream(is);
		}
	}

	private static byte[] downloadCrlFromLdap(final String ldapURL) throws NamingException {
	 	final Hashtable<String , String> env = new Hashtable<>();
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
	 	return DataDownloader.downloadData(crlURL);
	}

	private static List<String> getCrlDistributionPoints(final X509Certificate cert) throws IOException {
		final byte[] crldpExt = cert.getExtensionValue(Extension.cRLDistributionPoints.getId());
		if (crldpExt == null) {
			return new ArrayList<>(0);
		}

		try (
			final ASN1InputStream oAsnInStream = new ASN1InputStream(new ByteArrayInputStream(crldpExt))
		) {
			final ASN1Primitive derObjCrlDP = oAsnInStream.readObject();
			final byte[] crldpExtOctets = ((DEROctetString) derObjCrlDP).getOctets();
			final ASN1Primitive derObj2;
			try (
				final ASN1InputStream oAsnInStream2 = new ASN1InputStream(new ByteArrayInputStream(crldpExtOctets))
			) {
				derObj2 = oAsnInStream2.readObject();
				final CRLDistPoint distPoint = CRLDistPoint.getInstance(derObj2);
				final List<String> crlUrls = new ArrayList<>();
				for (final DistributionPoint dp : distPoint.getDistributionPoints()) {
					final DistributionPointName dpn = dp.getDistributionPoint();
				 	// Buscamos URIs en el fullName
				 	if (dpn != null && dpn.getType() == DistributionPointName.FULL_NAME) {
			 			final GeneralName[] genNames = GeneralNames.getInstance(dpn.getName()).getNames();
			 			// Buscamos la URI
			 			for (final GeneralName genName : genNames) {
			 				if (genName.getTagNo() == GeneralName.uniformResourceIdentifier) {
			 					crlUrls.add(DERIA5String.getInstance(genName.getName()).getString());
			 				}
			 			}
					}
				}
				return crlUrls;
			}
		}
	}
}
