package es.gob.afirma.core.misc.http;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.CoreMessages;

/**
 * Gestor del almac&eacute;n de confianza del Cliente @firma.
 * @author Carlos Gamuci
 */
public class TrustStoreManager {

	private static final char[] TRUSTED_KS_PWD = "changeit".toCharArray(); //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private KeyStore ks;
	private final File tsPath;

	private static TrustStoreManager instance = null;

	/**
	 * Obtiene la instancia &uacute;nica de la clase gestor del almac&eacute;n de confianza.
	 * @return Gestor del almac&eacute;n de confianza.
	 */
	public static TrustStoreManager getInstance() {
		return getInstance(null);
	}

	/**
	 * Obtiene la instancia &uacute;nica de la clase gestor del almac&eacute;n de confianza.
	 * @param parent Componente padre sobre el que mostrar cualquier di&aacute;logo que sea
	 * necesario durante la operaci&oacute;n de carga del almac&eacute;n.
	 * @return Gestor del almac&eacute;n de confianza.
	 */
	public static TrustStoreManager getInstance(final Object parent) {
		if (instance == null) {
			instance = new TrustStoreManager(parent);
		}
		return instance;
	}

	/**
	 * Carga el almac&eacute;n si existe. En caso de que exista pero no se puede cargar,
	 * se preguntar&aacute; al usuario si desea eliminar el almac&eacute;n y crear uno nuevo.
	 * @param parent Componente padre sobre el que mostrar cualquier di&aacute;logo que sea
	 * necesario durante la operaci&oacute;n de carga.
	 */
	private TrustStoreManager(final Object parent) {
		this.tsPath = getJKSFile();

		// Si el fichero existe, lo cargamos
		if (this.tsPath.isFile()) {
			try {
				this.ks = loadTrustedKeyStore();
			}
			catch (final Exception e) {
				final int result = AOUIFactory.showConfirmDialog(
						parent,
						CoreMessages.getString("AutofirmaTrustStore.0"), //$NON-NLS-1$
						CoreMessages.getString("AutofirmaTrustStore.1"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE);
				if (result == AOUIFactory.OK_OPTION) {
					try {
						Files.delete(this.tsPath.toPath());
					} catch (final IOException e1) {
						LOGGER.warning("No se pudo eliminar el TrustStore corrupto: " + e1); //$NON-NLS-1$
					}
					this.ks = null;
				}
			}
		}
	}

	/**
	 * Obtiene el listado de certificados del almac&eacute;n.
	 * @return Listado de certificados o, si no hay almac&eacute;n, una lista vac&iacute;a.
	 * @throws KeyStoreException Cuando no se puedan cargar los certificados del almac&eacute;n.
	 */
	public X509Certificate[] getCertificates() throws KeyStoreException {

		if (this.ks == null) {
			return new X509Certificate[0];
		}

		final List<X509Certificate> certs = new ArrayList<>();

		final Enumeration<String> aliases = this.ks.aliases();
		while (aliases.hasMoreElements()) {
			certs.add((X509Certificate) this.ks.getCertificate(aliases.nextElement()));
		}
		return certs.toArray(new X509Certificate[0]);
	}

	/**
	 * Importa un listado de certificados en el almac&eacute;n de confianza.
	 * @param certs Listado de certificados.
	 * @throws IOException No se ha podido guardar el almac&eacute;n.
	 * @throws KeyStoreException No se han podido importar los certificados en el almac&eacute;n.
	 */
	public void importCerts(final X509Certificate ...certs) throws KeyStoreException, IOException  {

		if (certs == null) {
			return;
		}

		if (this.ks == null) {
			this.ks = createTrustedKeystore();
		}

		for (final X509Certificate cert : certs) {

			// Comprobamos si el certificado ya esta en el almacen y lo omitimos si asi fuese
			String alias = searchCert(cert);
			if (alias != null) {
				continue;
			}

			// Generamos un alias para el certificado que no coincida con ningun otro del almacen
			alias = generateAlias(cert);
			this.ks.setCertificateEntry(alias, cert);
		}
		store();
	}

	/**
	 * Genera un alias representativo del certificado &uacute;nico en el almac&eacute;n.
	 * @param cert Certificado para el que obtener el alias.
	 * @return Alias.
	 * @throws KeyStoreException Cuando no se puede acceder al almac&eacute;n para comprobar el alias.
	 */
	private String generateAlias(final X509Certificate cert) throws KeyStoreException {

		final String principal = cert.getSubjectX500Principal().toString();
		String alias = principal;
		for (int i = 1; this.ks.containsAlias(alias); i++) {
			alias = principal + " (" + i + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return alias;
	}

	/**
	 * Almacena en disco el almac&eacute;n con los cambios que se hayan realizado.
	 * @throws IOException Cuando no se encuentre una ruta v&aacute;lida para almacenar el almac&eacute;n.
	 * @throws KeyStoreException Cuando no se pueda almacenar el almac&eacute;n.
	 */
	private void store() throws KeyStoreException, IOException {

		synchronized(instance) {
			try (OutputStream fos = new FileOutputStream(this.tsPath);
					OutputStream bos = new BufferedOutputStream(fos);) {
				this.ks.store(bos, TRUSTED_KS_PWD);
			}
			catch (final IOException e) {
				throw e;
			}
			catch (final Exception e) {
				throw new KeyStoreException("No se ha podido guardar el almacen", e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Elimina un certificado del almac&eacute;n.
	 * @param cert Certificado que se quiere eliminar.
	 * @throws KeyStoreException Cuando no se pueda acceder al almac&eacute;n.
	 * @throws IOException Cuando no se pueda guardar el almac&eacute;n.
	 */
	public void deleteCert(final X509Certificate cert) throws KeyStoreException, IOException {

		if (this.ks == null) {
			return;
		}

		final String alias = searchCert(cert);
		if (alias == null) {
			LOGGER.warning(
					"No se encontro en el almacen el certificado que desea eliminar"); //$NON-NLS-1$
			return;
		}

		this.ks.deleteEntry(alias);

		store();
	}

	/**
	 * Busca un certificado en el almac&eacute;n y devuelve el alias que tiene asignado.
	 * @param cert Certificado que se desea buscar.
	 * @return Alias del certificado o {@code null} si no se encuentra en el almac&eacute;n.
	 * @throws KeyStoreException Cuando no se puede acceder al almac&eacute;n.
	 */
	private String searchCert(final X509Certificate cert) throws KeyStoreException {
		X509Certificate c;
		final Enumeration<String> aliases = this.ks.aliases();
		while (aliases.hasMoreElements()) {

			final String alias = aliases.nextElement();
			c = (X509Certificate) this.ks.getCertificate(alias);
			if (c.equals(cert)) {
				return alias;
			}
		}

		return null;
	}

	/**
	 * Se crea un almac&eacute;n de confianza vac&iacute;o.
	 * @return Almac&eacute;n de confianza.
	 * @throws KeyStoreException Si no se pudiere crear el almacen.
	 */
	private static KeyStore createTrustedKeystore() throws KeyStoreException {

		KeyStore trustStore;
		try {
			trustStore = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			trustStore.load(null, TRUSTED_KS_PWD);
		}
		catch (final Exception e) {
			throw new KeyStoreException("No se ha podido crear un almacen vacio", e); //$NON-NLS-1$
		}

		return trustStore;
	}

	/**
	 * Carga el almac&eacute;n de confianza de disco.
	 * @return Almac&eacute;n de confianza.
	 * @throws IOException Cuando no se encuentra o no se puede leer el almac&eacute;n en disco.
	 * @throws KeyStoreException Cuando nos se pueda cargar el almac&eacute;n.
	 */
	private KeyStore loadTrustedKeyStore() throws IOException, KeyStoreException {

		byte[] ksContent;
		try (InputStream is = new FileInputStream(this.tsPath)) {
			ksContent = AOUtil.getDataFromInputStream(is);
		}

		KeyStore trustStore;
		try (InputStream is = new ByteArrayInputStream(ksContent)) {
			trustStore = KeyStore.getInstance("JKS"); //$NON-NLS-1$
			trustStore.load(is, TRUSTED_KS_PWD);
		}
		catch (final Exception e) {
			throw new KeyStoreException("No se ha podido cargar el almacen de confianza de la aplicacion", e); //$NON-NLS-1$
		}

		return trustStore;
	}

	/**
	 * Devuelve la ruta en la que debe encontrarse el almac&eacute;n de confianza del Cliente @firma.
	 * @return Ruta del almac&eacute;n.
	 */
	public static File getJKSFile() {
		return new File(Platform.getUserHome(), ".afirma" + File.separator + "TrustedCertsKeystore.jks"); //$NON-NLS-1$ //$NON-NLS-2$;
	}
}
