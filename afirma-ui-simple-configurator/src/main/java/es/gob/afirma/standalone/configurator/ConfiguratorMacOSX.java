package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.ConfiguratorFirefox.MozillaProfileNotFoundException;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de
 * AutoFirma. */
final class ConfiguratorMacOSX implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String KS_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "'AutoFirma ROOT'"; //$NON-NLS-1$
	static final String GET_USER_SCRIPT = "/getUsers.sh";//$NON-NLS-1$
	private static final String MACOSX_CERTIFICATE = "/AutoFirma_ROOT.cer";//$NON-NLS-1$
	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$
	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	private static final String OSX_SEC_KS_CERT_COMMAND = "security add-trusted-cert -d -r trustAsRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	static final String OSX_GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	static final String MAC_SCRIPT_NAME = "/installCerScript"; //$NON-NLS-1$
	static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	static final String EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$
	private static final String TRUST_SETTINGS_COMMAND = "security trust-settings-import -d "; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_FILE = "/trust_settings.plist"; //$NON-NLS-1$
	static String mac_script_path;
	private static File ksFile;

	@Override
	public void configure(final Console window) throws IOException, GeneralSecurityException {


		window.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		window.print(Messages.getString("ConfiguratorMacOSX.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		if (!checkSSLKeyStoreGenerated(appDir)) {
			window.print(Messages.getString("ConfiguratorMacOSX.5")); //$NON-NLS-1$

			try {
				mac_script_path = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT).getAbsolutePath();
			}
			catch(final Exception e) {
				window.print(Messages.getString("ConfiguratorMacOSX.15"));  //$NON-NLS-1$
				LOGGER.severe("Error creando script temporal: " + e); //$NON-NLS-1$
				return;
			}
			final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
				ConfiguratorUtil.CERT_ALIAS,
				KS_PASSWORD
			);

			window.print(Messages.getString("ConfiguratorMacOSX.11")); //$NON-NLS-1$

			ConfiguratorUtil.installFile(
				certPack.getPkcs12(),
				new File(ConfiguratorUtil.getApplicationDirectory(), KS_FILENAME)
			);

			window.print(Messages.getString("ConfiguratorMacOSX.6")); //$NON-NLS-1$

			// damos permisos al script
			ConfiguratorFirefox.addExexPermissionsToAllFilesOnDirectory(ConfiguratorUtil.getApplicationDirectory());
			try {
				ConfiguratorFirefox.copyConfigurationFiles(appDir);
				uninstall();

				window.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$
				// Generamos el script para instalar el certificado en Mozilla

				ConfiguratorFirefox.installRootCAMozillaKeyStore(
					appDir,
					new String[] { OSX_GET_USERS_COMMAND }
				);
				// Tambien hay que instalar el certificado en el almacen de Apple
				importCARootOnMacOSXKeyStore();
				ConfiguratorFirefox.addExexPermissionsToFile(new File(mac_script_path));
				ConfiguratorFirefox.executeScriptMacOsx(mac_script_path, true, true);
				LOGGER.info("Configuracion de NSS"); //$NON-NLS-1$
				MozillaKeyStoreUtilitiesOsX.configureMacNSS(MozillaKeyStoreUtilities.getSystemNSSLibDir());
			}
			catch (final MozillaProfileNotFoundException e) {
				window.print(Messages.getString("ConfiguratorMacOSX.12")); //$NON-NLS-1$
			}
			catch (final AOException e1) {
				LOGGER.info("La configuracion de NSS para Mac OS X ha fallado: " + e1); //$NON-NLS-1$
			}
			finally {
				if (ksFile != null) {
					LOGGER.info("Elimino .cer: " + ksFile.delete()); //$NON-NLS-1$
				}
			}
		}
		else {
			LOGGER.info("checkSSLKeyStoreGenerated existe" ); //$NON-NLS-1$
			window.print(Messages.getString("ConfiguratorMacOSX.14")); //$NON-NLS-1$
		}

		window.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado" ); //$NON-NLS-1$

	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
		return new File(appConfigDir, KS_FILENAME).exists();
	}

	/** Genera el comando de instalaci&oacute;n del certificado en el almac&eacute;n de apple en el script de instalaci&oacute;n.
	 * @throws GeneralSecurityException Se produce si hay un problema de seguridad durante el proceso.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void importCARootOnMacOSXKeyStore() throws GeneralSecurityException, IOException {
		final File f = new File(ConfiguratorUtil.getApplicationDirectory() + MACOSX_CERTIFICATE);
		final String cmd = OSX_SEC_COMMAND.replace(
			"%KEYCHAIN%", //$NON-NLS-1$
			KEYCHAIN_PATH
			).replace(
				"%CERT%", //$NON-NLS-1$
				f.getAbsolutePath().replace(" ", "\\ ") //$NON-NLS-1$ //$NON-NLS-2$
		);
		LOGGER.info("comando de instalacion del certificado en el almacen de apple: " + cmd); //$NON-NLS-1$
		ConfiguratorFirefox.writeScriptFile(mac_script_path, new StringBuilder(cmd), true);


		final X509Certificate root;
		try (final InputStream is = new FileInputStream(f)) {
			root = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(is); //$NON-NLS-1$
		}

		final String snRoot = AOUtil.hexify(root.getSerialNumber().toByteArray(), false);
		final String sha1Root = AOUtil.hexify(MessageDigest.getInstance("SHA1").digest(root.getEncoded()), false); //$NON-NLS-1$

		//Instalamos pfx sacando el .cer
		final File pfx = new File(ConfiguratorUtil.getApplicationDirectory() + KS_FILENAME);
		final KeyStore ks;
		try (final InputStream is = new FileInputStream(pfx)) {
			ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
			ks.load(is, KS_PASSWORD.toCharArray());
		}
		final X509Certificate certPfx = (X509Certificate) ks.getCertificate(ConfiguratorUtil.CERT_ALIAS);
		final String snCer = AOUtil.hexify(certPfx.getSerialNumber().toByteArray(), false);
		final String sha1Cer =  AOUtil.hexify(MessageDigest.getInstance("SHA1").digest(certPfx.getEncoded()), false); //$NON-NLS-1$

		final byte[] buf = certPfx.getEncoded();

		ksFile = new File(ConfiguratorUtil.getApplicationDirectory() + KS_CER_FILENAME);
		try (
			final FileOutputStream os = new FileOutputStream(ksFile);
		) {
			os.write(buf);
		}

		final String cmdKs = OSX_SEC_KS_CERT_COMMAND.replace(
			"%KEYCHAIN%", //$NON-NLS-1$
			KEYCHAIN_PATH
			).replace(
				"%CERT%", //$NON-NLS-1$
				ksFile.getAbsolutePath().replace(" ", "\\ ") //$NON-NLS-1$ //$NON-NLS-2$
		);
		LOGGER.info("comando de instalacion del certificado en el almacen de apple: " + cmdKs); //$NON-NLS-1$
		ConfiguratorFirefox.writeScriptFile(mac_script_path, new StringBuilder(cmdKs), true);

		editTrusFile(sha1Root, sha1Cer, snRoot, snCer);

		final String trustCmd = TRUST_SETTINGS_COMMAND
			+ ConfiguratorUtil.getApplicationDirectory().getAbsolutePath()
			+ TRUST_SETTINGS_FILE
		;
		LOGGER.info("comando de instalacion de ajustes de confianza: " + trustCmd); //$NON-NLS-1$
		ConfiguratorFirefox.writeScriptFile(mac_script_path, new StringBuilder(trustCmd), true);

	}

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalacion del certificado raiz del almacen de MacOSX"); //$NON-NLS-1$

		try {
			// generamos script para borrar almacen de certificados de apple
			uninstallRootCAMacOSXKeyStore();
			// Generamos script para borrar el almacen certificados firefox
			ConfiguratorFirefox.generateUninstallScriptMac(ConfiguratorUtil.getApplicationDirectory());
			ConfiguratorFirefox.addExexPermissionsToAllFilesOnDirectory(
				ConfiguratorUtil.getApplicationDirectory()
			);
		}
		catch (MozillaProfileNotFoundException | IOException e) {
			LOGGER.severe("Se ha producido un error durante la desinstalacion: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Genera el script de desinstalaci&oacute;n del llavero OS X mediante AppleScript del certificado generado
	 * y elimina los links simb&oacute;licos.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAMacOSXKeyStore() throws IOException {
		LOGGER.severe("Desintalamos los certificados y eliminamos los enlaces simbolicos:"); //$NON-NLS-1$
		// Creamos comandos para eliminar enlaces simbolicos de firfox y certificados del llavero
		final String deleteLinks = "ls -ln /usr/local/lib | grep Firefox | awk '{print $9}' | xargs -I {} rm /usr/local/lib/{}"; //$NON-NLS-1$
		final String deleteCaCerts = "security find-certificate -c " + CERT_CN + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final String deleteKsCerts = "security find-certificate -c " + CERT_CN_ROOT + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final StringBuilder sb = new StringBuilder();
		sb.append(deleteLinks);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteCaCerts);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteKsCerts);
		ConfiguratorFirefox.writeScriptFile(mac_script_path, sb, true);

	}

	private static void editTrusFile(final String sha1Root, final String sha1Cer, final String snRoot, final String snCer) {
		final String sha1RootOrig = "04E48FFEA277B8CB1FD310D7598420138B0A885D"; //$NON-NLS-1$
		final String sha1CerOrig = "1D68080DCF91C8D1163F352828787C080E925BCC"; //$NON-NLS-1$
		final String snRootOrig = "57343ACD"; //$NON-NLS-1$
		final String snCerOrig = "5B9F4E37"; //$NON-NLS-1$

		try(final InputStream in = new FileInputStream(
			ConfiguratorUtil.getApplicationDirectory().getAbsolutePath()
			+ TRUST_SETTINGS_FILE
		)) {
			final DocumentBuilderFactory docFactory =
			DocumentBuilderFactory.newInstance();
			final DocumentBuilder docBuilder =
			docFactory.newDocumentBuilder();
			final Document doc = docBuilder.parse(in);
			final Node dict = doc.getElementsByTagName("dict").item(1); //$NON-NLS-1$
			final NodeList list = dict.getChildNodes();

			for (int i = 0; i < list.getLength(); i++) {
		         final Node node = list.item(i);
		         if (node.getNodeType() == Node.ELEMENT_NODE) {
		        	 final Element element = (Element) node;
		        	 if (element.getNodeName().equals("key")) { //$NON-NLS-1$
		        		 if (element.getTextContent().equals(sha1RootOrig)) {
		        			 element.setTextContent(sha1Root);
		        		 }
		        		 else if (element.getTextContent().equals(sha1CerOrig)) {
		        			 element.setTextContent(sha1Cer);
		        		 }
		        	 }
		        	 else if (element.getNodeName().equals("dict")) { //$NON-NLS-1$
		        		 final NodeList certList = element.getChildNodes();
		        		 for (int j = 0; j < certList.getLength(); j++) {
		        			 final Node n = certList.item(j);
		        			 if (n.getNodeType() == Node.ELEMENT_NODE) {
		        				 final Element el = (Element) n;
		        				 if (el.getNodeName().equals("data")) { //$NON-NLS-1$
		        					 if (AOUtil.hexify(Base64.decode(el.getTextContent()), false).equals(snRootOrig)) {
		        						 el.setTextContent(Base64.encode(hexStringToByteArray(snRoot)));
		        					 }
		        					 else if (AOUtil.hexify(Base64.decode(el.getTextContent()), false).equals(snCerOrig)) {
		        						 el.setTextContent(Base64.encode(hexStringToByteArray(snCer)));
		        					 }
			   		        	}
		        			}
		        		 }
		        	 }
		         }
		    }

			final TransformerFactory transformerFactory = TransformerFactory.newInstance();
			final Transformer transformer = transformerFactory.newTransformer();
			final DOMSource domSource = new DOMSource(doc);
			final StreamResult streamResult = new StreamResult(
				new File(
					ConfiguratorUtil.getApplicationDirectory().getAbsolutePath()
					+ TRUST_SETTINGS_FILE
				)
			);
			transformer.transform(domSource, streamResult);
		}
		catch (final Exception e) {
			LOGGER.severe("Error analizando el PList: " + e); //$NON-NLS-1$
		}
	}

	private static byte[] hexStringToByteArray(final String s) {
	    final int len = s.length();
	    final byte[] data = new byte[len / 2];
	    for (int i = 0; i < len; i += 2) {
	        data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
	                             + Character.digit(s.charAt(i+1), 16));
	    }
	    return data;
	}
}
