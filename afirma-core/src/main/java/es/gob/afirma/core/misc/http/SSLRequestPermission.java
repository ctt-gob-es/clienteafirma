package es.gob.afirma.core.misc.http;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;
import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.UIMessages;

public class SSLRequestPermission implements HttpProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$

	private final Exception e;

	public SSLRequestPermission (final Exception e) {
		this.e = e;
	}

	@Override
	public byte[] processHttpError(final String url, final int timeout, final String contentType, final String accept, final UrlHttpMethod method) throws IOException {
		if (this.e instanceof SSLHandshakeException &&
			JOptionPane.YES_OPTION == AOUIFactory.showConfirmDialog(null,
																	UIMessages.getString("SSLRequestPermissionDialog.2"), //$NON-NLS-1$
																	UIMessages.getString("SSLRequestPermissionDialog.1"), //$NON-NLS-1$
																	AOUIFactory.YES_NO_OPTION,
																	AOUIFactory.WARNING_MESSAGE
																	)
		) {
			final File trustedKSFile = new File(getTrustedCertKSPath());
			if (!trustedKSFile.exists()) {
				try {
					createTrustedKeystore(trustedKSFile);
				} catch (final IOException ioe) {
					LOGGER.warning(
							"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + ioe //$NON-NLS-1$
					);
					throw ioe;
				}
			}

			try {
				downloadFromRemoteServer(trustedKSFile, url);
			} catch (final Exception e1) {
				LOGGER.warning(
						"Error al descargar certificados SSL del servidor: " + e1 //$NON-NLS-1$
				);
				throw new IOException(e1);
			}

			final UrlHttpManager httpManager = UrlHttpManagerFactory.getInstalledManager();
			return httpManager.readUrl(url, timeout, contentType, accept, method);

		}
		throw new IOException("Se ha decidido no importar el certificado de confianza"); //$NON-NLS-1$
	}

	private static String getTrustedCertKSPath() {
		return Platform.getUserHome() + File.separator + ".afirma" + File.separator + "TrustedCertsKeystore.jks"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static void createTrustedKeystore(final File trustedKSFile) throws IOException {

		trustedKSFile.createNewFile();

		try (final OutputStream bos = new BufferedOutputStream(new FileOutputStream(trustedKSFile))) {

			final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			ks.load(null, TRUSTED_KS_PWD.toCharArray());
    		ks.store(bos, TRUSTED_KS_PWD.toCharArray());

		} catch (final Exception e) {
			LOGGER.warning("No se ha podido crear el almacen de confianza SSL: " + e); //$NON-NLS-1$
		}
	}

	private static void downloadFromRemoteServer(final File trustedKSFile, final String domainName) throws Exception {

		try (InputStream trustedKSStream = new FileInputStream(trustedKSFile)) {

			final CertificateFactory certFactory = CertificateFactory.getInstance("X509"); //$NON-NLS-1$
			final URL url = new URL(domainName);
			UrlHttpManagerImpl.disableSslChecks();
			final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
			conn.connect();
			final Certificate [] trustedServerCerts = conn.getServerCertificates();
			conn.disconnect();
			UrlHttpManagerImpl.enableSslChecks();
			final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			ks.load(trustedKSStream, TRUSTED_KS_PWD.toCharArray());
			if (trustedServerCerts.length > 1) {
				// Solo se obtienen los certificados emisores
				for (int i = 1 ; i < trustedServerCerts.length ; i++) {
					final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(
																	new ByteArrayInputStream(trustedServerCerts[i].getEncoded())
																	);
					ks.setCertificateEntry(cert.getSubjectDN().toString(), cert);
				}
			} else {
				final X509Certificate cert = (X509Certificate) certFactory.generateCertificate(
						new ByteArrayInputStream(trustedServerCerts[0].getEncoded())
						);
				ks.setCertificateEntry(cert.getSubjectDN().toString(), cert);
			}

			try (final OutputStream fos = new FileOutputStream(trustedKSFile)) {
				ks.store(fos, TRUSTED_KS_PWD.toCharArray());
				SslSecurityManager.configureTrustManagers();
			}
		}

	}

}
