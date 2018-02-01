/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

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
import es.gob.afirma.keystores.mozilla.apple.AppleScript;
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
	private static final String GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	private final static String USER_DIR_LINE_PREFIX = "dir: "; //$NON-NLS-1$
	private static final String GET_USER_SCRIPTS_NAME = "scriptGetUsers";//$NON-NLS-1$
	private static final String SCRIPT_EXT = ".sh";//$NON-NLS-1$
	private static final String MAC_SCRIPT_NAME = "installCerScript"; //$NON-NLS-1$
	private static final String MAC_SCRIPT_EXT = ".sh"; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_COMMAND = "security trust-settings-import -d "; //$NON-NLS-1$
	private static final String TRUST_SETTINGS_FILE = "/trust_settings.plist"; //$NON-NLS-1$
	private static final String OSX_RESOURCES = "/osx"; //$NON-NLS-1$

	private static final String MAC_CHROME_V56_OR_LOWER_PREFS_PATH = "/Library/Application Support/Google/Chrome/Local State"; //$NON-NLS-1$
	private static final String MAC_CHROME_V57_OR_HIGHER_PREFS_PATH = "/Library/Application Support/Google/Chrome/Default/Preferences"; //$NON-NLS-1$

	static String mac_script_path;
	private static File sslCerFile;

    /** Directorios de los usuarios del sistema. */
    private static String[] userDirs = null;

	@Override
	public void configure(final Console console) throws IOException, GeneralSecurityException {

		userDirs = getSystemUsersHomes();

		console.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File resourcesDir = getResourcesDirectory();

		console.print(Messages.getString("ConfiguratorMacOSX.3") + resourcesDir.getAbsolutePath()); //$NON-NLS-1$

		// Creamos los nuevos certificados SSL y los instalamos en los almacenes de confianza,
		// eliminando versiones anteriores si es necesario
		configureSSL(resourcesDir, console);

		// Eliminamos los warnings de Chrome
		createScriptsRemoveChromeWarnings(resourcesDir, userDirs);

		console.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
		LOGGER.info("Finalizado"); //$NON-NLS-1$

	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appDir) {
		return new File(appDir, KS_FILENAME).exists();
	}

	 /** Genera el <i>script</i> que elimina el warning al ejecutar AutoFirma desde Chrome.
	  * En linux genera el <i>script</i> que hay que ejecutar para realizar la
	  * instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	  * @param targetDir Directorio de instalaci&oacute;n del sistema.
	  *        En LINUX contiene el contenido del script a ejecutar.
	  * @param usersDirs Directorios de los usuarios del sistema. */
	private static void createScriptsRemoveChromeWarnings(final File targetDir,
			                                              final String[] usersDirs) {
		for (final String userDir : usersDirs) {

			// Generamos el script de instalacion
			final StringBuilder installationScript = new StringBuilder();

			// Montamos el script de instalacion y desinstalacion que
			// incluya el protocolo "afirma" en el fichero Local State o Preferences (segun la version)
			// para Google Chrome o Chromium
			try {
				// Se escriben los comandos en el script de instalacion
				final ArrayList<String[]> installCommands = getCommandsToRemoveChromeAndChromiumWarningsOnInstall(targetDir, userDir);
				final Iterator<String[]> list = installCommands.iterator();
				while(list.hasNext()) {
					ConfiguratorUtil.printScript(list.next(), installationScript);
				}
				// Se almacenan los script de instalacion
				try {
					ConfiguratorMacUtils.writeScriptFile(installationScript, new File(mac_script_path).getAbsolutePath(), true);
				}
				catch (final Exception e) {
					throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma'", e); //$NON-NLS-1$
				}
			} catch (final IOException e) {
				LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
			}
		}
	}

	/** Genera e instala los certificados SSL para la comunicaci&oacute;n con la
	 * aplicaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @param console Consola sobre la que escribir los mensajes de instalaci&oacute;n.
	 * @throws IOException Cuando ocurre un error en el proceso de instalaci&oacute;n.
	 * @throws GeneralSecurityException Cuando ocurre un error al generar el certificado SSL. */
	private static void configureSSL(final File appDir,
			                         final Console console) throws IOException,
	                                                               GeneralSecurityException {
		console.print(Messages.getString("ConfiguratorMacOSX.5")); //$NON-NLS-1$

		// Generamos un fichero que utilizaremos para guardar y ejecutar AppleScripts
		try {
			mac_script_path = File.createTempFile(MAC_SCRIPT_NAME, MAC_SCRIPT_EXT).getAbsolutePath();
		}
		catch(final Exception e) {
			console.print(Messages.getString("ConfiguratorMacOSX.18"));  //$NON-NLS-1$
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

		// Cerramos las instancias de firefox que esten abiertas
		closeFirefox();

		// Desinstalamos de los almacenes cualquier certificado anterior generado para este proposito
		console.print(Messages.getString("ConfiguratorMacOSX.15")); //$NON-NLS-1$
		uninstallProcess(appDir);

		// Se instalan los certificados en el almacen de Apple
		JOptionPane.showMessageDialog(console.getParentComponent(), Messages.getString("ConfiguratorMacOSX.20")); //$NON-NLS-1$
		console.print(Messages.getString("ConfiguratorMacOSX.6")); //$NON-NLS-1$
		try {
			createScriptToImportCARootOnMacOSXKeyStore(appDir);
			ConfiguratorMacUtils.addExexPermissionsToFile(new File(mac_script_path));
			executeScriptFile(mac_script_path, true, true);
		}
		catch (final Exception e1) {
			LOGGER.log(Level.WARNING, "Error en la importacion del certificado de confianza en el llavero del sistema operativo: " + e1, e1); //$NON-NLS-1$
		}

		// Se instalan los certificados en el almacen de Firefox
		console.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$
		final String[] userHomes = getSystemUsersHomes();
		try {
			ConfiguratorFirefoxMac.createScriptToInstallOnMozillaKeyStore(appDir, userHomes, new File(mac_script_path));
			LOGGER.info("Configuracion de NSS"); //$NON-NLS-1$
			MozillaKeyStoreUtilitiesOsX.configureMacNSS(MozillaKeyStoreUtilities.getSystemNSSLibDir());
		}
		catch (final MozillaProfileNotFoundException e) {
			LOGGER.severe("Perfil de Mozilla no encontrado: " + e); //$NON-NLS-1$
			console.print(Messages.getString("ConfiguratorMacOSX.12")); //$NON-NLS-1$
		}
		catch (final AOException e1) {
			LOGGER.severe("La configuracion de NSS para Mac OS X ha fallado: " + e1); //$NON-NLS-1$
		}
		finally {
			if (sslCerFile != null) {
				LOGGER.info("Elimino .cer del certificado SSL: " + sslCerFile.delete()); //$NON-NLS-1$
			}
		}
	}

	/** Genera el comando de instalaci&oacute;n del certificado en el almac&eacute;n de Apple en
	 * el <i>script</i> de instalaci&oacute;n.
	 * @param appDir Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @throws GeneralSecurityException Se produce si hay un problema de seguridad durante el proceso.
	 * @throws IOException Cuando hay un error en la creaci&oacute;n del fichero. */
	static void createScriptToImportCARootOnMacOSXKeyStore(final File appDir) throws GeneralSecurityException,
	                                                                                 IOException {

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

		editTrustFile(appDir, sha1Root, sha1Cer, snRoot, snCer);

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

		final File resourcesDir;
		try {
			resourcesDir = getResourcesDirectory();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se pudo obtener el directorio de recursos de la aplicacion", e); //$NON-NLS-1$
			return;
		}

		uninstallProcess(resourcesDir);
	}

	/** Ejecuta el proceso de desinstalaci&oacute;n. Durante el mismo se desinstalan los certificados
	 * de confianza SSL de los almacenes del sistema.
	 * @param appDir Directorio de instalaci&oacute;n. */
	private static void uninstallProcess(final File appDir) {
		try {
			uninstallRootCAMacOSXKeyStore();
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido generar el script para la desinstalacion del almacen del sistema", e); //$NON-NLS-1$
		}

		try {
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

	/** Genera el <i>script</i> de desinstalaci&oacute;n del llavero macOS mediante AppleScript
	 * del certificado generado y elimina los links simb&oacute;licos.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero. */
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

	private static void editTrustFile(final File appDir, final String sha1Root, final String sha1Cer, final String snRoot, final String snCer) {

		// Copia a disco la plantilla que rellenaremos para usarla como fichero de perfil
		// que instalar para configurar la confianza en los certificados SSL. Si existiese
		// una version anterior, la eliminariamos previamente
		try {
			deleteTrustTemplate(appDir);
			exportResource(OSX_RESOURCES, TRUST_SETTINGS_FILE, appDir.getAbsolutePath());
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
				new File(appDir, TRUST_SETTINGS_FILE)
			);
			transformer.transform(domSource, streamResult);

		}
		catch (final Exception e) {
			LOGGER.severe("Error analizando el PList: " + e); //$NON-NLS-1$
		}
	}

	private static File getResourcesDirectory() throws IOException {

		// Devuelve un directorio en el que copiar y generar los recursos
		// necesarios por la aplicacion
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		final File appDir = new File (userDir, "Library/Application Support/AutoFirma"); //$NON-NLS-1$
		if (!appDir.isDirectory() && !appDir.mkdirs()) {
			throw new IOException("No se ha podido generar el directorio de recursos de la aplicacion: " + appDir.getAbsolutePath()); //$NON-NLS-1$
		}
		return appDir;
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
	 * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del <i>script</i>.
	 * @throws InterruptedException Cuando se interrumpe la ejecuci&oacute;n del script. */
	private static Object executeScriptFile(final String path, final boolean administratorMode, final boolean delete) throws IOException, InterruptedException {

		final AppleScript script = new AppleScript(new File(path), delete);

		LOGGER.info("Path del script: " + path); //$NON-NLS-1$
		try {
			Object o;
			if (administratorMode) {
				o = script.runAsAdministrator();
			}
			else {
				o = script.run();
			}
			return o;
		}
		catch (final IOException e) {
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
					Messages.getString("ConfiguratorMacOSX.17"), //$NON-NLS-1$
					Messages.getString("ConfiguratorMacOSX.16"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/** Detecta si el proceso de Firefox est&aacute; abierto.
	 * @return <code>true</code> si el proceso de Firefox est&aacute; abierto,
	 *         <code>false</code> en caso contrario. */
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

	/** Elimina los ficheros de certificado ra&iacute;z y almac&eacute;n SSL del disco
	 * como paso previo a volver a generarlos
	 * @param appDir Ruta del directorio de la aplicaci&oacute;n
	 * @throws IOException En cualquier error. */
	private static void deleteTrustTemplate(final File appDir) throws IOException {

		if (checkTrutsTemplateInstalled(appDir)) {

			final File sslKey = new File(appDir, TRUST_SETTINGS_FILE);

			if (!sslKey.delete()) {
				throw new IOException("No puedo eliminar " + TRUST_SETTINGS_FILE); //$NON-NLS-1$
			}

		}

	}

	/** Copia un recurso desde dentro del JAR hacia una ruta externa.
     * @param pathToResource Carpeta del recurso dentro del JAR.
     * @param resourceName Nombre del recurso a copiar.
     * @param destinationPath Ruta externa destino.
     * @return Ruta completa del recurso copiado.
     * @throws IOException En cualquier error. */
    private static String exportResource(final String pathToResource,
    		                             final String resourceName,
    		                             final String destinationPath) throws IOException {

    	final File outFile = new File(destinationPath + resourceName);
        try (
    		final InputStream stream = ConfiguratorMacOSX.class.getResourceAsStream(pathToResource + resourceName);
		) {

            if (stream == null) {
                throw new IOException("No ha podido obtenerse el recurso \"" + resourceName + "\" del jar."); //$NON-NLS-1$ //$NON-NLS-2$
            }

            int readBytes;
            final byte[] buffer = new byte[4096];
            final boolean jnlpDeploy = AutoFirmaConfiguratiorJNLPUtils.isJNLPDeployment();
            try (OutputStream resStreamOut = jnlpDeploy ?
            		AutoFirmaConfiguratiorJNLPUtils.selectFileToWrite(outFile) :
            			new FileOutputStream(outFile);) {
            	while ((readBytes = stream.read(buffer)) > 0) {
            		resStreamOut.write(buffer, 0, readBytes);
            	}
            }
        }

        return outFile.getAbsolutePath();
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
			final Set<String> dirs = new HashSet<>();
			try (
					final InputStream resIs = new ByteArrayInputStream(o.toString().getBytes());
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 2048 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				String line;
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(USER_DIR_LINE_PREFIX)){
						dirs.add(line.substring(USER_DIR_LINE_PREFIX.length()));
					}
				}
			}
			userDirs = dirs.toArray(new String[dirs.size()]);
		}
		catch (final IOException | InterruptedException e) {
			LOGGER.severe("Error al generar el listado perfiles de Firefox del sistema: " + e); //$NON-NLS-1$
			userDirs = null;
		}

		return userDirs;
	}

	/** Crea un fichero de <i>script</i> para la obtenci&oacute;n de los usuarios del sistema.
	 * @return <i>Script</i> para la obtenci&oacute;n de los usuarios del sistema.
	 * @throws IOException Cuando no se pueda crear el fichero de <i>script</i>. */
	private static File createGetUsersScript() throws IOException {
		final StringBuilder script = new StringBuilder(GET_USERS_COMMAND);
		final File scriptFile = File.createTempFile(GET_USER_SCRIPTS_NAME, SCRIPT_EXT);
		try {
			ConfiguratorMacUtils.writeScriptFile(script, scriptFile.getAbsolutePath(), true);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Ha ocurrido un error al generar el script de obtencion de usuarios: " + e, e); //$NON-NLS-1$
		}
		ConfiguratorMacUtils.addExexPermissionsToFile(scriptFile);

		return scriptFile;
	}

	/** Genera los <i>scripts</i> que registran el esquema "afirma" como un
	 * protocolo de confiable en Chrome.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @return <i>Scripts</i> que registran el esquema "afirma" como un
	 * protocolo de confiable en Chrome.
	 * @throws IOException En cualquier error. */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnInstall(final File appDir,
			                                                                                 final String userDir) throws IOException {

		final ArrayList<String[]> commandList = new ArrayList<>();
		// Final del if
		final String[] endIfStatement = new String[] {
				"fi", //$NON-NLS-1$
		};

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v56 o inferior
			final String[] commandInstallChrome56OrLower01 =
					deleteProtocolInPreferencesFile1(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChrome56OrLower02 =
					deleteProtocolInPreferencesFile2(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChrome56OrLower1 =
					addProtocolInPreferencesFile(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChrome56OrLower2 =
					correctProtocolInPreferencesFile(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] ifContainsString2 = getIfNotCointainsStringCommand(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			// Comando para agregar la confianza del esquema 'afirma' en caso de tener Chrome v56 o inferior recien instalado
			final String[] commandInstallChrome56OrLower4 = new String[] {
					"sed -i ''", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + MAC_CHROME_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome56OrLower01);
			commandList.add(commandInstallChrome56OrLower02);
			commandList.add(commandInstallChrome56OrLower1);
			commandList.add(commandInstallChrome56OrLower2);
			commandList.add(ifContainsString2);
			commandList.add(commandInstallChrome56OrLower4);
			commandList.add(endIfStatement);
			commandList.add(
					copyConfigurationFile(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH));
		}

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v57 o superior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v57 o superior
			final String[] commandInstallChrome57OrHigher01 =
					deleteProtocolInPreferencesFile1(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher02 =
					deleteProtocolInPreferencesFile2(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher1 =
					addProtocolInPreferencesFile(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher2 =
					correctProtocolInPreferencesFile(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome57OrHigher01);
			commandList.add(commandInstallChrome57OrHigher02);
			commandList.add(commandInstallChrome57OrHigher1);
			commandList.add(commandInstallChrome57OrHigher2);
			commandList.add(
					copyConfigurationFile(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH));

		}
		return commandList;
	}

	/** Genera los <i>scripts</i> para confirmar si existen protocolos definidos en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para confirmar si existen protocolos definidos en el fichero.*/
	private static String[] getIfNotCointainsStringCommand(final String userDir, final String browserPath) {
		// If para comprobar si es necesario incluir la sintaxis entera de definicion de protocolos o si,
		// por el contrario, ya estaba
		final String[] ifStatement = new String[] {
				"if ! ", //$NON-NLS-1$
				"grep -q \"excluded_schemes\" " +  //$NON-NLS-1$
				escapePath(userDir + browserPath),
				"; then", //$NON-NLS-1$
		};
		return ifStatement;
	}

	/** Genera los <i>scripts</i> para reemplazar el fichero original por el temporal con
	 * el que se estaba trabajando.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para reemplazar el fichero original por el temporal con
	 *         el que se estaba trabajando. */
	private static String[] copyConfigurationFile(final String userDir, final String browserPath) {
		// Comando para sobreescribir el fichero de configuracion
		final String[] commandCopy = new String[] {
				"\\cp", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
				escapePath(userDir + browserPath),
		};

		return commandCopy;
	}

	/** Genera los <i>scripts</i> para eliminar el protocolo <code>afirma</code>.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para eliminar el protocolo <code>afirma</code>. */
	private static String[] deleteProtocolInPreferencesFile1(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false,//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los <i>scripts</i> para eliminar el protocolo <code>afirma</code>.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para eliminar el protocolo <code>afirma</code>. */
	private static String[] deleteProtocolInPreferencesFile2(final String userDir,
			                                                 final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed -i ''", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los <i>scripts</i> para eliminar el protocolo <code>afirma</code>.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para eliminar el protocolo <code>afirma</code>.*/
	private static String[] addProtocolInPreferencesFile(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed -i ''", //$NON-NLS-1$
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los <i>scripts</i> para eliminar la coma en caso de que sea el unico protocolo definido en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para eliminar la coma en caso de que sea el unico protocolo definido en el fichero. */
	private static String[] correctProtocolInPreferencesFile(final String userDir, final String browserPath) {

		// Comando para eliminar la coma en caso de ser el unico protocolo de confianza
		final String[] commandInstall2 = new String[] {
				"sed -i ''", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,}/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall2;
	}


	/** <i>Escapa</i> rutas de fichero para poder usarlas como parte de un <i>script</i>.
	 * @param path Ruta de fichero.
	 * @return Ruta <i>escapada</i>. */
	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
