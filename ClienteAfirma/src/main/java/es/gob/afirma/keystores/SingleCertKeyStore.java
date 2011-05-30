/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.security.Key;
import java.security.KeyStoreException;
import java.security.KeyStoreSpi;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.logging.Logger;

import es.gob.afirma.misc.AOUtil;

/**
 * <code>KeyStore</code> para el manejo de certificados en disco en formato
 * PKCS#7 o X.509 en Base64.
 */
final class SingleCertKeyStore extends KeyStoreSpi {

	private Hashtable<String, X509Certificate> certificates = new Hashtable<String, X509Certificate>();

	@Override
	public Enumeration<String> engineAliases() {
		return certificates.keys();
	}

	@Override
	public boolean engineContainsAlias(String alias) {
		if (alias == null)
			return false;
		for (Enumeration<String> e = engineAliases(); e.hasMoreElements();)
			if (e.nextElement().equals(alias))
				return true;
		return false;
	}

	@Override
	public void engineDeleteEntry(String alias) throws KeyStoreException {
		if (alias == null)
			return;
		certificates.remove(alias);
	}

	@Override
	public Certificate engineGetCertificate(String alias) {
		if (alias == null)
			return null;
		return certificates.get(alias);
	}

	@Override
	public String engineGetCertificateAlias(Certificate cert) {
		if (!(cert instanceof X509Certificate))
			return null;
		String tmpAlias;
		for (Enumeration<String> e = engineAliases(); e.hasMoreElements();) {
			tmpAlias = e.nextElement();
			if (certificates.get(tmpAlias).equals(cert))
				return tmpAlias;
		}
		return null;
	}

	@Override
	public Certificate[] engineGetCertificateChain(String alias) {
		if (!engineContainsAlias(alias))
			return new Certificate[0];
		return new Certificate[] { certificates.get(alias) };
	}

	@Override
	public Date engineGetCreationDate(String arg0) {
		return new Date();
	}

	@Override
	public Key engineGetKey(String alias, char[] arg1)
			throws NoSuchAlgorithmException, UnrecoverableKeyException {
		if (!engineContainsAlias(alias))
			throw new UnrecoverableKeyException(
					"No hay ningun certificado con el alias '" + alias + "'");
		return engineGetCertificate(alias).getPublicKey();
	}

	@Override
	public boolean engineIsCertificateEntry(String arg0) {
		return false;
	}

	@Override
	public boolean engineIsKeyEntry(String arg0) {
		return false;
	}

	@Override
	public void engineLoad(InputStream is, char[] pwd) throws IOException,
			NoSuchAlgorithmException, CertificateException {
		if (is == null)
			throw new IOException("Se necesitan certificados");

		// Primero leemos todo el Stream en un ByteArray
		byte[] certs = AOUtil.getDataFromInputStream(is);

		// Probamos con la factoría de Sun
		Collection<? extends Certificate> tmpColCerts = null;
		try {
			if (cf == null)
				cf = CertificateFactory.getInstance("X.509");
			tmpColCerts = cf.generateCertificates(new ByteArrayInputStream(
					certs));
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma")
					.warning(
							"La factoria no ha podido generar los certificados directamente, se probara con un pretratamiento: "
									+ e);
			getCertificatesFromStream(new ByteArrayInputStream(certs));
			return;
		}
		if (tmpColCerts != null) {
			for (Certificate c : tmpColCerts) {
				if (!(c instanceof X509Certificate)) {
					Logger.getLogger("es.gob.afirma")
							.warning(
									"Se ha encontrado un certificado en un formato que no es X.509, se ignorara");
					continue;
				}
				try {
					certificates.put(AOUtil.getCN((X509Certificate) c),
							(X509Certificate) c);
				} catch (Exception e) {
					Logger.getLogger("es.gob.afirma")
							.warning(
									"Error anadiendo un certificado, se ignorara y se continuara con los siguientes: "
											+ e);
				}
			}
		}
		// Si la coleccion es nula queda la opcion de que haya que pretratar,
		// aun cuando no salto la
		// excepcion...
		else
			getCertificatesFromStream(new ByteArrayInputStream(certs));
	}

	@Override
	public void engineSetCertificateEntry(String arg0, Certificate arg1)
			throws KeyStoreException {
	}

	@Override
	public void engineSetKeyEntry(String arg0, byte[] arg1, Certificate[] arg2)
			throws KeyStoreException {
	}

	@Override
	public void engineSetKeyEntry(String arg0, Key arg1, char[] arg2,
			Certificate[] arg3) throws KeyStoreException {
	}

	@Override
	public int engineSize() {
		return 1;
	}

	@Override
	public void engineStore(OutputStream arg0, char[] arg1) throws IOException,
			NoSuchAlgorithmException, CertificateException {
	}

	private void getCertificatesFromStream(InputStream stream) {
		BufferedReader br = new BufferedReader(new InputStreamReader(
				new DataInputStream(stream)));
		String strLine;
		String currentAlias = null;
		StringBuilder currentCertificate = null;
		try {
			while ((strLine = br.readLine()) != null) {
				// Certificado nuevo
				if (strLine.trim().equals("-----BEGIN CERTIFICATE-----")) {
					currentCertificate = new StringBuilder(strLine);
					currentCertificate.append("\n");
				} else if (strLine.trim().equals("-----END CERTIFICATE-----")) {
					if (currentCertificate != null) {
						currentCertificate.append(strLine);
						addCertificate(currentCertificate.toString(),
								currentAlias);
						currentCertificate = null;
						currentAlias = null;
					}
				} else if (strLine.trim().startsWith("friendlyName:")) {
					currentAlias = strLine.replace("friendlyName:", "").trim();
				} else if (currentCertificate != null) {
					currentCertificate.append(strLine);
					currentCertificate.append("\n");
				}
			}
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error leyendo los certificados, puede que no se anadiesen todos: "
							+ e);
		}
	}

	private CertificateFactory cf = null;

	private void addCertificate(final String base64Cert, String alias) {
		if (base64Cert == null) {
			Logger.getLogger("es.gob.afirma").warning(
					"El certificado es nulo, no se anadira al almacen");
			return;
		}
		X509Certificate tmpCert;
		try {
			if (cf == null)
				cf = CertificateFactory.getInstance("X.509");
			tmpCert = (X509Certificate) cf
					.generateCertificate(new ByteArrayInputStream(base64Cert
							.getBytes()));
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
					"Error generando el certificado, no se anadira al almacen: "
							+ e);
			return;
		}
		if (tmpCert == null) {
			Logger.getLogger("es.gob.afirma").warning(
					"Error generando el certificado, no se anadira al almacen");
			return;
		}
		if (alias == null || "".equals(alias))
			alias = AOUtil.getCN(tmpCert);
		certificates.put(alias, tmpCert);
	}

}
