package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.swing.JOptionPane;
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
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.ConfiguratorFirefox.MozillaProfileNotFoundException;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de
 * AutoFirma. */
final class ConfiguratorMacOSX implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String SSL_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "'AutoFirma ROOT'"; //$NON-NLS-1$
	private static final String MACOSX_CERTIFICATE = "/AutoFirma_ROOT.cer";//$NON-NLS-1$
	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$
	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	private static final String OSX_SEC_KS_CERT_COMMAND = "security add-trusted-cert -d -r trustAsRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	static final String OSX_GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	static final String MAC_SCRIPT_NAME = "installCerScript"; //$NON-NLS-1$
	static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	static final String EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$
	private static final String TRUST_SETTINGS_COMMAND = "security trust-settings-import -d "; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_FILE = "/trust_settings.plist"; //$NON-NLS-1$
	static String mac_script_path;
	private static File sslCerFile;

	@Override
	public void configure(final Console console) throws IOException, GeneralSecurityException {


		console.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		console.print(Messages.getString("ConfiguratorMacOSX.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		if (!checkSSLKeyStoreGenerated(appDir)) {
			configureSSL(appDir, console);

		}
		else {
			LOGGER.info("Los certificados SSL existen y no se crearan ni instalaran" ); //$NON-NLS-1$
			console.print(Messages.getString("ConfiguratorMacOSX.14")); //$NON-NLS-1$
		}

		console.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado"); //$NON-NLS-1$

	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
		return new File(appConfigDir, KS_FILENAME).exists();
	}

	/**
	 * Genera e instala los certificados SSL para la comunicaci&oacute;n con la aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al generar el certificado SSL.
	 */
	private static void configureSSL(final File appDir, final Console console) throws IOException, GeneralSecurityException {
		console.print(Messages.getString("ConfiguratorMacOSX.5")); //$NON-NLS-1$

		// Generamos un fichero que utilizaremos para guardar y ejecutar AppleScripts
		try {
			mac_script_path = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT).getAbsolutePath();
		}
		catch(final Exception e) {
			console.print(Messages.getString("ConfiguratorMacOSX.15"));  //$NON-NLS-1$
			LOGGER.severe("Error creando script temporal: " + e); //$NON-NLS-1$
			throw new IOException("Error creando script temporal", e); //$NON-NLS-1$
		}

		// Damos permisos al script
		addExexPermissionsToAllFilesOnDirectory(appDir);

		// Generamos los certificados de CA y SSL
		final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
			ConfiguratorUtil.CERT_ALIAS,
			KS_PASSWORD
		);

		console.print(Messages.getString("ConfiguratorMacOSX.11")); //$NON-NLS-1$

		// Copiamos los certificados CA y SSL a disco
        ConfiguratorUtil.installFile(
        		certPack.getCaCertificate().getEncoded(),
        		new File(appDir, MACOSX_CERTIFICATE));

		ConfiguratorUtil.installFile(
			certPack.getPkcs12(),
			new File(appDir, KS_FILENAME)
		);

		// Cerramos el almacen de firefox si esta abierto
		closeFirefox();

		// Desinstalamos de los almacenes cualquier certificado anterior generado para este proposito
		LOGGER.info("Desinstalacion de versiones anteriores del certificado raiz del almacen de MacOSX"); //$NON-NLS-1$
		try {
			uninstallRootCAMacOSXKeyStore();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la busqueda y desinstalacion de versiones anteriores del certificado SSL en el llavero de macOS: " + e, e); //$NON-NLS-1$
		}

		LOGGER.info("Desinstalacion de versiones anteriores del certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		try {
			uninstallRootCAFirefoxKeyStore();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la busqueda y desinstalacion de versiones anteriores del certificado SSL en Firefox: " + e, e); //$NON-NLS-1$
		}


		// Se instalan los certificados en el almacen de Mozilla
		try {
			// Copiamos en disco certUtil para la configuracion de los certificados en el almacen de Firefox
			ConfiguratorFirefox.copyConfigurationFiles(appDir);

			console.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$

			// Instalar el certificado en Mozilla
			ConfiguratorFirefox.installRootCAMozillaKeyStore(
				appDir,
				new String[] { OSX_GET_USERS_COMMAND }
			);

			LOGGER.info("Configuracion de NSS"); //$NON-NLS-1$
			MozillaKeyStoreUtilitiesOsX.configureMacNSS(MozillaKeyStoreUtilities.getSystemNSSLibDir());
		}
		catch (final MozillaProfileNotFoundException e) {
			console.print(Messages.getString("ConfiguratorMacOSX.12")); //$NON-NLS-1$
		}
		catch (final AOException e1) {
			LOGGER.info("La configuracion de NSS para Mac OS X ha fallado: " + e1); //$NON-NLS-1$
		}

		// Se instalan los certificados en el almacen de Apple
		console.print(Messages.getString("ConfiguratorMacOSX.16")); //$NON-NLS-1$
		try {
			createScriptToImportCARootOnMacOSXKeyStore(appDir);
			addExexPermissionsToFile(new File(mac_script_path));
			executeScript(mac_script_path, true, true);
		}
		catch (final Exception e1) {
			LOGGER.log(Level.WARNING, "Error en la importacion del certificado de confianza en el llavero del sistema operativo: " + e1, e1); //$NON-NLS-1$
		}
		finally {
			if (sslCerFile != null) {
				LOGGER.info("Elimino .cer del certificado SSL: " + sslCerFile.delete()); //$NON-NLS-1$
			}
		}
	}

	/** Genera el comando de instalaci&oacute;n del certificado en el almac&eacute;n de apple en el script de instalaci&oacute;n.
	 * @throws GeneralSecurityException Se produce si hay un problema de seguridad durante el proceso.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void createScriptToImportCARootOnMacOSXKeyStore(final File appDir) throws GeneralSecurityException, IOException {


		// Creamos el script para la instalacion del certificado SSL en el almacen de confianza de Apple
		final File certFile = new File(appDir, MACOSX_CERTIFICATE);
		final String cmd = OSX_SEC_COMMAND.replace(
			"%KEYCHAIN%", //$NON-NLS-1$
			KEYCHAIN_PATH
			).replace(
				"%CERT%", //$NON-NLS-1$
				certFile.getAbsolutePath().replace(" ", "\\ ") //$NON-NLS-1$ //$NON-NLS-2$
		);
		LOGGER.info("Comando de instalacion del certificado de CA en el almacen de confianza de Apple: " + cmd); //$NON-NLS-1$
		writeScriptFile(mac_script_path, new StringBuilder(cmd), true);


		// Creamos el script para la instalacion del certificado SSL en el almacen de confianza de Apple
		final File pfx = new File(appDir, KS_FILENAME);
		final KeyStore ks;
		try (final InputStream is = new FileInputStream(pfx)) {
			ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
			ks.load(is, KS_PASSWORD.toCharArray());
		}
		final X509Certificate certPfx = (X509Certificate) ks.getCertificate(ConfiguratorUtil.CERT_ALIAS);
		final byte[] buf = certPfx.getEncoded();

		sslCerFile = new File(appDir, SSL_CER_FILENAME);
		try (
			final FileOutputStream os = new FileOutputStream(sslCerFile);
		) {
			os.write(buf);
		}

		final String cmdKs = OSX_SEC_KS_CERT_COMMAND.replace(
			"%KEYCHAIN%", //$NON-NLS-1$
			KEYCHAIN_PATH
			).replace(
				"%CERT%", //$NON-NLS-1$
				sslCerFile.getAbsolutePath().replace(" ", "\\ ") //$NON-NLS-1$ //$NON-NLS-2$
		);
		LOGGER.info("Comando de instalacion del certificado SSL en el almacen de confianza de Apple: " + cmd); //$NON-NLS-1$
		writeScriptFile(mac_script_path, new StringBuilder(cmdKs), true);

		// Creamos el fichero de perfil y el script necesario para que se confie automaticamente en los nuevos certificados
		final X509Certificate root;
		try (final InputStream is = new FileInputStream(certFile)) {
			root = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(is); //$NON-NLS-1$
		}

		final String snRoot = AOUtil.hexify(root.getSerialNumber().toByteArray(), false);
		final String sha1Root = AOUtil.hexify(MessageDigest.getInstance("SHA1").digest(root.getEncoded()), false); //$NON-NLS-1$
		final String snCer = AOUtil.hexify(certPfx.getSerialNumber().toByteArray(), false);
		final String sha1Cer =  AOUtil.hexify(MessageDigest.getInstance("SHA1").digest(certPfx.getEncoded()), false); //$NON-NLS-1$

		editTrusFile(sha1Root, sha1Cer, snRoot, snCer);

		final String trustCmd = TRUST_SETTINGS_COMMAND
			+ appDir.getAbsolutePath().replace(" ", "\\ ") //$NON-NLS-1$ //$NON-NLS-2$
			+ TRUST_SETTINGS_FILE
		;
		LOGGER.info("Comando de instalacion de ajustes de confianza: " + trustCmd); //$NON-NLS-1$
		writeScriptFile(mac_script_path, new StringBuilder(trustCmd), true);

	}

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalacion del certificado raiz de los almacenes de MacOSX"); //$NON-NLS-1$

		try {
			uninstallRootCAMacOSXKeyStore();

			uninstallRootCAFirefoxKeyStore();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la desinstalacion: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Desinstala el certificado de CA del almacen de autoridades de confianza de Mozilla Firefox.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAFirefoxKeyStore() throws IOException {

		// Generamos script para borrar el almacen certificados firefox
		ConfiguratorFirefox.generateUninstallScriptMac(ConfiguratorUtil.getApplicationDirectory());
		// Le damos permisos para poder ejecutarlo
		addExexPermissionsToAllFilesOnDirectory(ConfiguratorUtil.getApplicationDirectory());
	}

	/**
	 * Genera el script de desinstalaci&oacute;n del llavero OS X mediante AppleScript del certificado generado
	 * y elimina los links simb&oacute;licos.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAMacOSXKeyStore() throws IOException {
		LOGGER.info("Desinstalamos los certificados y eliminamos los enlaces simbolicos"); //$NON-NLS-1$
		// Creamos comandos para eliminar enlaces simbolicos de firefox y certificados del llavero
		final String deleteLinks = "ls -ln /usr/local/lib | grep Firefox | awk '{print $9}' | xargs -I {} rm /usr/local/lib/{}"; //$NON-NLS-1$
		final String deleteCaCerts = "security find-certificate -c " + CERT_CN + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final String deleteKsCerts = "security find-certificate -c " + CERT_CN_ROOT + " -a -Z|grep SHA-1|awk '{ print $NF }' | xargs -I {} security delete-certificate -Z {}"; //$NON-NLS-1$ //$NON-NLS-2$
		final StringBuilder sb = new StringBuilder();
		sb.append(deleteLinks);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteCaCerts);
		sb.append(";"); //$NON-NLS-1$
		sb.append(deleteKsCerts);
		writeScriptFile(mac_script_path, sb, true);
	}

	private static void editTrusFile(final String sha1Root, final String sha1Cer, final String snRoot, final String snCer) {

		final String sha1RootOrig = "%CA_SHA1%"; //$NON-NLS-1$
		final String sha1CerOrig = "%SSL_SHA1%"; //$NON-NLS-1$
		final String snRootOrig = "%CA_SERIALNUMBER%"; //$NON-NLS-1$
		final String snCerOrig = "%SSL_SERIALNUMBER%"; //$NON-NLS-1$

		try(final InputStream in = new FileInputStream(
			ConfiguratorUtil.getApplicationDirectory().getAbsolutePath()
			+ TRUST_SETTINGS_FILE
		);
				) {

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

	/** Escribe un <i>script</i> en un fichero dado.
	 * @param path Ruta donde se escribir&aacute; el <i>script</i>.
	 * @param data Datos a escribir.
	 * @param append <code>true</code> permite contatenar el contenido del fichero con lo que se va a escribir. <code>false</code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
	static void writeScriptFile(final String path, final StringBuilder data, final boolean append ) throws IOException{
		LOGGER.info("Se escribira en fichero el siguiente comando:\n" + data.toString()); //$NON-NLS-1$
		final File macScript = new File(path);
		data.append("\n"); //$NON-NLS-1$
		try (final FileOutputStream fout = new FileOutputStream(macScript, append);) {
			fout.write(data.toString().getBytes());
		}
	}

	/** Ejecuta un script en OS X.
	 * @param path Ruta donde se encuentra el <i>script</i>.
	 * @param administratorMode <code>true</code> el <i>script</i> se ejecuta como permisos de adminsitrador, <code>false</code> en caso contrario.
	 * @param delete <code>true</code> se borra el fichero despu&eacute;s de haberse ejecutado.
	 * @return El objeto que da como resultado el <i>script</i>.
	 * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del <i>script</i>. */
	public static Object executeScript(final String path, final boolean administratorMode, final boolean delete) throws IOException {

		final ScriptEngine se = MozillaKeyStoreUtilitiesOsX.getAppleScriptEngine();
		if (se == null) {
			LOGGER.severe("No se ha podido instanciar el motor AppleScript"); //$NON-NLS-1$
			throw new IOException("No se ha podido instanciar el motor AppleScript"); //$NON-NLS-1$
		}

		LOGGER.info("Path del script: " + path); //$NON-NLS-1$
		try {
			Object o;
			if (administratorMode) {
				o = se.eval("do shell script \"" + path + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			else {
				o = se.eval("do shell script \"" + path + "\" "); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (delete){
				final File scriptInstall = new File(path);
				if (scriptInstall.exists()){
					scriptInstall.delete();
				}
			}
			return o;
		}
		catch (final ScriptException e) {
			throw new IOException("Error en la ejecucion del script via AppleScript: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Da permisos de ejecuci&oacute;n a todos los ficheros de un directorio dado.
	 * @param dir Directorio al que dar permiso.
	 */
	public static void addExexPermissionsToAllFilesOnDirectory(final File dir) {

		for (final File fileEntry : dir.listFiles()) {
			addExexPermissionsToFile(fileEntry);
		}
	}

	static void addExexPermissionsToFile(final File f) {
		final Set<PosixFilePermission> perms = new HashSet<>();
		perms.add(PosixFilePermission.OWNER_EXECUTE);
		perms.add(PosixFilePermission.GROUP_EXECUTE);
		perms.add(PosixFilePermission.OTHERS_EXECUTE);
		perms.add(PosixFilePermission.OWNER_READ);
		perms.add(PosixFilePermission.GROUP_READ);
		perms.add(PosixFilePermission.OTHERS_READ);
		perms.add(PosixFilePermission.OWNER_WRITE);
		perms.add(PosixFilePermission.GROUP_WRITE);
		perms.add(PosixFilePermission.OTHERS_WRITE);
		try {
			Files.setPosixFilePermissions(
				Paths.get(f.getAbsolutePath()),
				perms
			);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido dar permiso de ejecucion a '" + f.getAbsolutePath() + "': " + e//$NON-NLS-1$ //$NON-NLS-2$
			);
		}
	}

	/**
	 * Pide al usuario que cierre el navegador Mozilla Firefox y no permite continuar hasta que lo hace.
	 */
	private static void closeFirefox() {

		while (isFirefoxOpen()) {
			JOptionPane.showMessageDialog(
					null,
					Messages.getString("ConfiguratorMacOSX.6"), //$NON-NLS-1$
					Messages.getString("ConfiguratorMacOSX.17"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/**
	 * Detecta si el proceso de Firefox est&aacute; abierto.
	 * @return
	 */
	private static boolean isFirefoxOpen() {

		// Listamos los procesos abiertos y buscamos uno que contenga una cadena identificativa de Firefox
		try {
			final ProcessBuilder psProcessBuilder = new ProcessBuilder("ps", "aux"); //$NON-NLS-1$ //$NON-NLS-2$
			final Process ps = psProcessBuilder.start();

			String line;
			try (
					final InputStream resIs = ps.getInputStream();
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							256, // Maximo 256 lineas de salida
							1024 // Maximo 1024 caracteres por linea
							);
					) {
				while ((line = resReader.readLine()) != null) {
					if (line.contains("Firefox.app") //$NON-NLS-1$
							|| line.contains("FirefoxNightly.app") //$NON-NLS-1$
							|| line.contains("FirefoxDeveloperEdition.app")) { //$NON-NLS-1$
						return true;
					}
				}
			}
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo completar la deteccion del proceso de Firefox. Se considerara que no esta en ejecucion: " + e); //$NON-NLS-1$
		}

		return false;
	}

	public static void main(final String[] args) throws Exception {

		final ConfiguratorMacOSX configurator = new ConfiguratorMacOSX();
		configurator.configure(ConsoleManager.getConsole(null));

		//System.out.println("Firefox abierto: " + ConfiguratorMacOSX.isFirefoxOpen());
	}
}
