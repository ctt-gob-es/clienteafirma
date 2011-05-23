package es.gob.afirma.cliente.test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.callbacks.CachePasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;

public class TestUtils {
	
	public static final String[] CYPHER_ALGOS = new String[] {
		"AES", "ARCFOUR", "Blowfish", "DES", "DESede", "RC2"
	};
	
	public static final String[] CYPHER_ALGOS_PBE = new String[] {
		"PBEWithMD5AndDES", "PBEWithSHA1AndDESede", "PBEWithSHA1AndDESede"
	};
	
	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
		p1.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);

		final Properties p4 = new Properties();
		p4.setProperty("format", AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING);
		p4.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);

		final Properties p2 = new Properties();
		p2.setProperty("format", AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED);
		
		final Properties p3 = new Properties();
		p3.setProperty("format", AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED);
		p3.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
		
		final Properties p5 = new Properties();
		p5.setProperty("format", AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED);
		p5.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);
		
		XMLDSIG_MODES = new Properties[] { p1, p2, p3, p4, p5 };
	}
	
	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOConstants.SIGN_FORMAT_XADES_ENVELOPING);
		p1.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);

		final Properties p4 = new Properties();
		p4.setProperty("format", AOConstants.SIGN_FORMAT_XADES_ENVELOPING);
		p4.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);

		final Properties p2 = new Properties();
		p2.setProperty("format", AOConstants.SIGN_FORMAT_XADES_ENVELOPED);
		
		final Properties p3 = new Properties();
		p3.setProperty("format", AOConstants.SIGN_FORMAT_XADES_DETACHED);
		p3.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
		
		final Properties p5 = new Properties();
		p5.setProperty("format", AOConstants.SIGN_FORMAT_XADES_DETACHED);
		p5.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);
		
		XADES_MODES = new Properties[] { p1, p2, p3, p4, p5 };
	}
	
	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOConstants.SIGN_FORMAT_CADES);
		p1.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
		
		final Properties p2 = new Properties();
		p2.setProperty("format", AOConstants.SIGN_FORMAT_CADES);
		p2.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);
		
		CADES_MODES = new Properties[] { p1, p2 };
	}
	
	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOConstants.SIGN_FORMAT_CMS);
		p1.setProperty("mode", AOConstants.SIGN_MODE_IMPLICIT);
		
		final Properties p2 = new Properties();
		p2.setProperty("format", AOConstants.SIGN_FORMAT_CMS);
		p2.setProperty("mode", AOConstants.SIGN_MODE_EXPLICIT);
		
		CMS_MODES = new Properties[] { p1, p2 };
	}
	
	public final static Properties[] XADES_MODES;
	public final static Properties[] CADES_MODES;
	public final static Properties[] CMS_MODES;
	public final static Properties[] XMLDSIG_MODES;
	
	public final static String[] ALGOS = new String[] {
		AOConstants.SIGN_ALGORITHM_SHA1WITHRSA,
		AOConstants.SIGN_ALGORITHM_SHA512WITHRSA,
	};
	
	public static final CounterSignTarget[] CS_TARGETS = new CounterSignTarget[] {
		CounterSignTarget.Leafs,
		CounterSignTarget.Tree,
	};
	
	public final static byte[] BIN_CONTENT = "DATOS-BINARIOS".getBytes();
	
	public final static byte[] XML_CONTENT = new String(
		"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\r\n" +
        " <Cuerpo>\r\n" +
        "  <etiqueta1>Cuerpo de la etiqueta 1</etiqueta1>\r\n" +
        "  <etiqueta2>Cuerpo de la etiqueta 2</etiqueta2>\r\n" +
        "  <etiqueta3>\r\n" +
        "   <subetiqueta>Prueba1</subetiqueta>\r\n" +
        "   <subetiqueta>Prueba2</subetiqueta>\r\n" +
        "  </etiqueta3>\r\n" +
        "</Cuerpo>"
	).getBytes();
	
	public final static byte[][] TEST_CONTENT = new byte[][] {
		BIN_CONTENT,
		XML_CONTENT
	};
	
	public final static AOConstants.AOKeyStore[] KEYSTORES = new  AOConstants.AOKeyStore[] {
		AOConstants.AOKeyStore.APPLE,
		//AOConstants.AOKeyStore.MOZ_UNI,
		AOConstants.AOKeyStore.PKCS12,
		AOConstants.AOKeyStore.WINDOWS
	};


	public static PrivateKeyEntry getValidKeyEntryFromKeyStore(final AOConstants.AOKeyStore keyStore) {
		
		if (AOKeyStore.WINDOWS.equals(keyStore) && (!Platform.OS.WINDOWS.equals(Platform.getOS()))) return null;
		if (AOKeyStore.APPLE.equals(keyStore) && (!Platform.OS.MACOSX.equals(Platform.getOS()))) return null;
		
		AOKeyStoreManager ksm = null;
		PasswordCallback pss = null;
		
		if (AOKeyStore.PKCS12.equals(keyStore)) {
			// Primero copiamos nuestro P12 a un temporal
			String libName = null;
			try {
				final File P12 = File.createTempFile("p12", ".pfx");
				final OutputStream os = new FileOutputStream(P12);
				os.write(
					AOUtil.getDataFromInputStream(
							ClassLoader.getSystemResourceAsStream(
							"ANF_PF_Activo.pfx"
						)
					)
				);
				try { os.flush(); } catch(final Throwable e) {}
				try { os.close(); } catch(final Throwable e) {}
				libName = P12.getAbsolutePath();
			}
			catch(final Throwable e) {
				e.printStackTrace();
				return null;
			}
			pss = new CachePasswordCallback(new char[] { '1', '2', '3', '4', '1', '2', '3', '4' });
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOConstants.AOKeyStore.PKCS12,
					libName,
					null,
					pss,
					null
				);
			}
			catch(final Throwable e) {
				e.printStackTrace();
				return null;
			}
		}
		else if (AOKeyStore.MOZ_UNI.equals(keyStore) ||
				 AOKeyStore.WINDOWS.equals(keyStore) ||
				 AOKeyStore.APPLE.equals(keyStore)
		) {
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					keyStore,
					null,
					null,
					null,
					null
				);
			}
			catch(final Throwable e) {
				e.printStackTrace();
				return null;
			}
		}
		else if (AOKeyStore.PKCS11.equals(keyStore)) {
			String p11lib = null;
			if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				if (new File("C:\\Windows\\SysWOW64\\UsrPkcs11.dll").exists()) p11lib = "C:\\Windows\\SysWOW64\\UsrPkcs11.dll";
				else p11lib = "C:\\Windows\\System32\\UsrPkcs11.dll";
			}
			else if (Platform.OS.LINUX.equals(Platform.getOS())) {
				p11lib = "";
			}
			else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				p11lib = "";
			}
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOConstants.AOKeyStore.PKCS11,
					p11lib,
					null,
					new UIPasswordCallback("Contrasena del DNIe", null),
					null
				);
			}
			catch(final Throwable e) {
				e.printStackTrace();
				return null;
			}
		}
		
		// En este punto ya tenemos ksm
		if (ksm == null) return null;
		for (String alias : ksm.getAliases()) {
			try {
				return ksm.getKeyEntry(alias, pss);
			}
			catch(final Throwable e) {
				continue;
			}
		}
		
		System.out.println("El almacen de prueba no tiene ningun certificado valido");
		return null;
	}
	
	//**********************************************************************
	//****************** COPIA DE KEYSTORES ********************************
	//**********************************************************************

	
	private static File tmpJKS = null;
	private static File tmpP12 = null;
	
	static {
		
		try {
			tmpJKS = File.createTempFile("jks", ".jks");
			OutputStream os = new FileOutputStream(tmpJKS);
			os.write(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(
						"trusted.jssecacerts"
					)
				)
			);
			try { os.flush(); } catch(final Throwable e) {}
			try { os.close(); } catch(final Throwable e) {}
			
			tmpP12 = File.createTempFile("p12", ".pfx");
			os = new FileOutputStream(tmpP12);
			os.write(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(
						"Carac_raros.pfx"
					)
				)
			);
			try { os.flush(); } catch(final Throwable e) {}
			try { os.close(); } catch(final Throwable e) {}

		}
		catch(final Throwable e) {}
		
	}
	//**********************************************************************
	//****************** FIN COPIA DE KEYSTORES ****************************
	//**********************************************************************

	
	public static String verifyBinSignature(byte[] sign) throws Throwable {
		
		if (sign == null) throw new NullPointerException("Los datos de firma a validar son nulos");
		
		final String endpointURL = "https://des-afirma.redinteradministrativa.es:443/afirmaws/services/DSSAfirmaVerify";
		//final String endpointURL = "http://217.15.39.2:8080/afirmaws/services/DSSAfirmaVerify";
		
		//TODO: Hacer esto por reflexion para evitar problemas de carga en Java 5??
//		final AFirmaWSSignatureVerifier afirmaWSVerifier = new AFirmaWSSignatureVerifier(endpointURL, "map.age.evisor_pruebas");
//		if (tmpJKS != null && tmpP12 != null) afirmaWSVerifier.initSSLStores(
//			tmpJKS.getAbsolutePath(), null, null, 
//			tmpP12.getAbsolutePath(), "PKCS12", "cacertica08"
//		);
//		return afirmaWSVerifier.verifyBin(sign);
		return "";
	}
	
	public static String verifyXMLSignature(byte[] sign) throws Throwable {
		return "";
	}
}
