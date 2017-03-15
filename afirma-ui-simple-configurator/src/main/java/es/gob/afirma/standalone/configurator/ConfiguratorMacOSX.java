package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
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

/** Configura la instalaci&oacute;n en Mac para la correcta ejecuci&oacute;n de
 * AutoFirma. */
final class ConfiguratorMacOSX implements Configurator {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "/autofirma.pfx"; //$NON-NLS-1$
	private static final String SSL_CER_FILENAME = "/autofirma.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	private static final String CERT_CN_ROOT = "'AutoFirma ROOT'"; //$NON-NLS-1$
	private static final String MACOSX_CERTIFICATE = "/AutoFirma_ROOT.cer";//$NON-NLS-1$
	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$
	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	private static final String OSX_SEC_KS_CERT_COMMAND = "security add-trusted-cert -d -r trustAsRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	static final String GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	private static final String GET_USER_SCRIPTS_NAME = "scriptGetUsers";//$NON-NLS-1$
	private static final String SCRIPT_EXT = ".sh";//$NON-NLS-1$
	static final String MAC_SCRIPT_NAME = "installCerScript"; //$NON-NLS-1$
	static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_COMMAND = "security trust-settings-import -d "; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_FILE = "/trust_settings.plist"; //$NON-NLS-1$
	private static final String OSX_RESOURCES = "/osx"; //$NON-NLS-1$
	static String mac_script_path;
	private static File sslCerFile;

    /** Directorios de los usuarios del sistema. */
    private static String[] userDirs = null;


	@Override
	public void configure(final Console console) throws IOException, GeneralSecurityException {


		console.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		console.print(Messages.getString("ConfiguratorMacOSX.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		// Creamos los nuevos certificados SSL y los instalamos en los almacenes de confianza,
		// eliminando versiones anteriores si es necesario
		configureSSL(appDir, console);

		console.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado"); //$NON-NLS-1$

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
		ConfiguratorMacUtils.addExexPermissionsToAllFilesOnDirectory(appDir);

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
		console.print(Messages.getString("ConfiguratorMacOSX.15")); //$NON-NLS-1$
		uninstallProcess();

		// Se instalan los certificados en el almacen de Mozilla
		final String[] userHomes = getSystemUsersHomes();
		try {
			console.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$

			// Instalar el certificado en Mozilla
			ConfiguratorFirefoxMac.createScriptToInstallOnMozillaKeyStore(appDir, userHomes, new File(mac_script_path));

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
		console.print(Messages.getString("ConfiguratorMacOSX.6")); //$NON-NLS-1$
		try {
			createScriptToImportCARootOnMacOSXKeyStore(appDir);
			ConfiguratorMacUtils.addExexPermissionsToFile(new File(mac_script_path));
			executeScriptFile(mac_script_path, true, true);
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
		ConfiguratorMacUtils.writeScriptFile(new StringBuilder(cmd), mac_script_path, true);


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
		ConfiguratorMacUtils.writeScriptFile(new StringBuilder(cmdKs), mac_script_path, true);

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
		ConfiguratorMacUtils.writeScriptFile(new StringBuilder(trustCmd), mac_script_path, true);

	}

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalacion del certificado raiz de los almacenes de MacOSX"); //$NON-NLS-1$

		uninstallProcess();
	}

	/**
	 * Ejecuta el proceso de desinstalaci&oacute;n. Durante el mismo se desinstalan los certificados
	 * de confianza SSL de los almacenes del sistema.
	 */
	private static void uninstallProcess() {
		try {
			uninstallRootCAMacOSXKeyStore();

			final File appDir = ConfiguratorUtil.getApplicationDirectory();
			final String[] usersHomes = getSystemUsersHomes();
			ConfiguratorFirefoxMac.createScriptToUnistallFromMozillaKeyStore(appDir, usersHomes, new File(mac_script_path));
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.info("No se han encontrado perfiles de Mozilla de los que desinstalar: " + e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Se ha producido un error durante la desinstalacion: " + e, e); //$NON-NLS-1$
		}
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
		ConfiguratorMacUtils.writeScriptFile(sb, mac_script_path, true);
	}

	private static void editTrusFile(final String sha1Root, final String sha1Cer, final String snRoot, final String snCer) {

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		// Copia a disco la plantilla que rellenaremos para usarla como fichero de perfil
		// que instalar para configurar la confianza en los certificados SSL. Si existiese
		// una version anterior, la eliminariamos previamente
		try {
			deleteTrustTemplate(appDir);
			exportResource(OSX_RESOURCES,TRUST_SETTINGS_FILE, appDir.getAbsolutePath());
		} catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "No ha sido posible exportar la plantilla de confianza para la instalacion de los certificados SSL. Quizas no se confie en los certificados.", e); //$NON-NLS-1$
		}

		final String sha1RootOrig = "%CA_SHA1%"; //$NON-NLS-1$
		final String sha1CerOrig = "%SSL_SHA1%"; //$NON-NLS-1$
		final String snRootOrig = "%CA_SERIALNUMBER%"; //$NON-NLS-1$
		final String snCerOrig = "%SSL_SERIALNUMBER%"; //$NON-NLS-1$

		try(final InputStream in = new FileInputStream(
				appDir.getAbsolutePath()
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

	/** Ejecuta un fichero de scripts.
	 * @param path Ruta donde se encuentra el <i>script</i>.
	 * @param administratorMode <code>true</code> el <i>script</i> se ejecuta con permisos de adminsitrador, <code>false</code> en caso contrario.
	 * @param delete <code>true</code> se borra el fichero despu&eacute;s de haberse ejecutado.
	 * @return El objeto que da como resultado el <i>script</i>.
	 * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del <i>script</i>. */
	public static Object executeScriptFile(final String path, final boolean administratorMode, final boolean delete) throws IOException {

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

	/** Comprueba si ya existe una plantilla de confianzas instalada en el
	 * directorio de la aplicaci&oacute;n.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe una plantilla de confianza, {@code false} en caso contrario. */
	private static boolean checkTrutsTemplateInstalled(final File appDir) {
		return new File(appDir, TRUST_SETTINGS_FILE).exists();
	}

	/**
	 * Elimina los ficheros de certificado ra&iacutez y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos
	 * @param appDir Ruta del directorio de la aplicaci&oacute;n
	 * @throws IOException
	 */
	private static void deleteTrustTemplate(File appDir) throws IOException {

		if (checkTrutsTemplateInstalled(appDir)) {

			final File sslKey = new File(appDir, TRUST_SETTINGS_FILE);

			if (!sslKey.delete()) {
				throw new IOException("No puedo eliminar " + TRUST_SETTINGS_FILE); //$NON-NLS-1$
			}

		}

	}

	/**
     * Copia un recurso desde dentro del jar hacia una ruta externa
     *
     * @param pathToResource Carpeta del recurso dentro del jar
     * @param resourceName Nombre del recurso a copiar
     * @param destinationPath Ruta externa destino
     * @return Ruta completa del recurso copiado
     * @throws Exception
     */
    static public String exportResource(String pathToResource, String resourceName, String destinationPath) throws Exception {

        try (final InputStream stream = ConfiguratorMacOSX.class.getResourceAsStream(pathToResource + resourceName);) {

            if (stream == null) {
                throw new IOException("No ha podido obtenerse el recurso \"" + resourceName + "\" del jar."); //$NON-NLS-1$ //$NON-NLS-2$
            }

            int readBytes;
            final byte[] buffer = new byte[4096];
            try (OutputStream resStreamOut = new FileOutputStream(destinationPath + resourceName);) {
            	while ((readBytes = stream.read(buffer)) > 0) {
            		resStreamOut.write(buffer, 0, readBytes);
            	}
            }
        } catch (final Exception ex) {
            throw ex;
        }

        return destinationPath + resourceName;
    }

    /** Devuelve un listado con todos los directorios de usuario del sistema.
	 * @return Listado de directorios. */
	private static String[] getSystemUsersHomes() {

		if (userDirs != null) {
			return userDirs;
		}

		try {
			final File getUsersScriptFile = createGetUsersScript();
			final Object o = executeScriptFile(getUsersScriptFile.getAbsolutePath(), false, true);
			final List<String> dirs = new ArrayList<>();
			String line;
			final String initLine = "dir: "; //$NON-NLS-1$
			try (
					final InputStream resIs = new ByteArrayInputStream(o.toString().getBytes());
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 2048 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(initLine)){
						dirs.add(
								line.substring(
										line.indexOf(initLine) + initLine.length()
										)
								);
					}
				}
			}
			userDirs = dirs.toArray(new String[dirs.size()]);
		}
		catch (final IOException e) {
			LOGGER.severe("Error al generar el listado perfiles de Firefox del sistema: " + e); //$NON-NLS-1$
			userDirs = null;
		}

		return userDirs;
	}

	/**
	 * Crea un fichero de script para la obtenci&oacute;n de los usuarios del sistema.
	 * @throws IOException Cuando no se pueda crear el fichero de script.
	 */
	private static File createGetUsersScript() throws IOException {
		final StringBuilder script = new StringBuilder(ConfiguratorMacOSX.GET_USERS_COMMAND);
		final File scriptFile = File.createTempFile(GET_USER_SCRIPTS_NAME, SCRIPT_EXT);
		try {
			ConfiguratorMacUtils.writeScriptFile(script, scriptFile.getAbsolutePath(), true);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Ha ocurrido un error al generar el script de obtencion de usuarios: " + e, e); //$NON-NLS-1$
		}
		ConfiguratorMacUtils.addExexPermissionsToFile(scriptFile);

		return scriptFile;
	}

}
