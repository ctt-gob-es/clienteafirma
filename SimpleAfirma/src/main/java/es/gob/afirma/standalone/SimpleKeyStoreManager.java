package es.gob.afirma.standalone;

import java.awt.Component;
import java.io.File;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;
import es.gob.afirma.exceptions.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.AOConstants;

/**
 * Gestor simple de <code>KeyStores</code>. Obtiene o un <code>KeyStore</code> de DNIe
 * v&iacute;a PKCS#11 o el <code>KeyStore</code> por defecto del sistema operativo
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SimpleKeyStoreManager {
	
	private SimpleKeyStoreManager() {}
	
	
	private static String getPKCS11DNIeLib() throws AOKeyStoreManagerException {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			final String lib = AOUtil.getSystemLibDir();
			if (new File(lib + "\\UsrPkcs11.dll").exists()) return lib + "\\UsrPkcs11.dll";
			//if (new File(lib + "\\AutBioPkcs11.dll").exists()) lib = lib + "\\AutBioPkcs11.dll";
			if (new File(lib + "\\opensc-pkcs11.dll").exists()) return lib + "\\opensc-pkcs11.dll";
			throw new AOKeyStoreManagerException(
				"No hay controlador PKCS#11 de DNIe instalado en este sistema Windows"
			);
		}
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			if (new File("/Library/OpenSC/lib/libopensc-dnie.dylib").exists()) return "/Library/OpenSC/lib/libopensc-dnie.dylib";
			if (new File("/Library/OpenSC/lib/opensc-pkcs11.so").exists()) return "/Library/OpenSC/lib/opensc-pkcs11.so";
			if (new File("/Library/OpenSC/lib/libopensc-dnie.1.0.3.dylib").exists()) return "/Library/OpenSC/lib/libopensc-dnie.1.0.3.dylib";
			throw new AOKeyStoreManagerException(
				"No hay controlador PKCS#11 de DNIe instalado en este sistema Mac OS X"
			);
		}
		if (new File("/usr/local/lib/libopensc-dnie.so").exists()) return "/usr/local/lib/libopensc-dnie.so"; 
		if (new File("/usr/lib/libopensc-dnie.so").exists()) return "/usr/lib/libopensc-dnie.so";
		if (new File("/lib/libopensc-dnie.so").exists()) return "/lib/libopensc-dnie.so";
		if (new File("/usr/lib/opensc-pkcs11.so").exists()) return "/usr/lib/opensc-pkcs11.so";
		if (new File("/lib/opensc-pkcs11.so").exists()) return "/lib/opensc-pkcs11.so";
		if (new File("/usr/local/lib/opensc-pkcs11.so").exists()) return "/usr/local/lib/opensc-pkcs11.so";
		throw new AOKeyStoreManagerException(
			"No hay controlador PKCS#11 de DNIe instalado en este sistema"
		);
	}

	/**
	 * Obtiene un <code>KeyStore</code>.
	 * @param dnie <code>true</code> si desea obtenerse un <code>KeyStore</code> para DNIe v&iacute;a PKCS#11,
	 *             <code>false</code> si desea obtenerse el <code>KeyStore</code> por defecto del sistema operativo
	 * @param parent Componente padre para la modalidad
	 * @return <code>KeyStore</code> apropiado
	 * @throws AOKeyStoreManagerException Si ocurre cualquier problema durante la obtenci&oacute;n del <code>KeyStore</code>
	 */
	public static AOKeyStoreManager getKeyStore(final boolean dnie, final Component parent) throws AOKeyStoreManagerException {
		
		final String lib = getPKCS11DNIeLib();
		
		if (dnie) {
			try {
				return AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.PKCS11, 
					lib, 
					"DNIe", 
					new UIPasswordCallback("PIN del DNIe", parent), 
					parent
				);
			}
			catch(final Exception e) {
				throw new AOKeyStoreManagerException(
					"No se ha podido inicializar el controlador PKCS#11 del DNIe (" + lib + ")", e
				);
			}
		}
		
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			try {
				return AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.WINDOWS, 
					null, 
					null, 
					new NullPasswordCallback(), 
					parent
				);
			}
			catch(final Exception e) {
				throw new AOKeyStoreManagerException("No se ha podido inicializar SunMSCAPI", e);
			}
		}
		
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			try {
				return AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.APPLE, 
					null, 
					null, 
					new NullPasswordCallback(), 
					parent
				);
			}
			catch(final Exception e) {
				throw new AOKeyStoreManagerException("No se ha podido incializar el Llavero de Mac OS X", e);
			}
		}
		
		return null;
	}
	
	/**
	 * Selecciona el primer certificado del <code>KeyStore</code> proporcionado que cuente con
	 * un <i>KeyUsage</i> apto para firma electr&oacute;nica.
	 * @param ks <code>KeyStore</code> sobre el que buscar los certificados
	 * @return Alias del primer certificado encontrado con <i>KeyUsage</i> apto para firma electr&oacute;nica
	 * @throws AOCertificatesNotFoundException Si no se encuentra ning&uacute;n certificado apto para firma en el almac&eacute;n
	 * @throws KeyStoreException Si ocurre cualquier error en el tratamiento del <code>KeyStore</code>
	 */
	public static String autoSelectSignCert(final KeyStore ks) throws AOCertificatesNotFoundException, KeyStoreException {
		if (ks == null) throw new NullPointerException("El KeyStore proporcionado no puede ser nulo");
		String alias;
		final Enumeration<String> aliases = ks.aliases();
		while (aliases.hasMoreElements()) {
			alias = aliases.nextElement();
			//TODO: Filtrar por KeyUsage de firma
//			if (KeyStoreUtilities.matchesKeyUsageFilter((X509Certificate) ks.getCertificate(alias), AOConstants.SIGN_CERT_USAGE)) {
				return alias;
//			}
		}
		throw new AOCertificatesNotFoundException(
			"El almacen seleccionado no contiene certificados validos para firma electronica"
		);
	}

}
