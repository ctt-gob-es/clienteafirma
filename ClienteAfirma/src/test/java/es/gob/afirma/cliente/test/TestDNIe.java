package es.gob.afirma.cliente.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static es.gob.afirma.cliente.test.TestUtils.ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.TEST_CONTENT;
import static es.gob.afirma.cliente.test.TestUtils.XADES_MODES;

import java.io.File;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOXAdESSigner;

public class TestDNIe {

	@Test
	public void testXADESSignatureDNIe() {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
		AOSigner signer = new AOXAdESSigner();
		assertNotNull(signer);
		byte[] result = null;
		PrivateKeyEntry pke = null;
		//for (byte[] content : TEST_CONTENT) {
		//	for(Properties extraParams : XADES_MODES) {
				for (String algo : ALGOS) {
					System.out.println();System.out.println();System.out.println();
					System.out.println("Prueba DNIe con XAdES, " + algo); 
					try {
						pke = TestUtils.getValidKeyEntryFromKeyStore(AOConstants.AOKeyStore.PKCS11);
						assertNotNull("No hemos encontrado una clave de prueba en DNIe", pke);
						result = signer.sign(TEST_CONTENT[1], algo, pke, XADES_MODES[0]);
					}
					catch(final Throwable e) {
						result = null;
						e.printStackTrace();
					}
			//	}
			//}
		}
		assertNotNull(result);
	}
	
	
	@Test
	public void testPKCS11KeyStore() {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
		System.out.println("Probando almacen PKCS#11 con DNIe...");
		String p11lib = null;
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			if (new File("C:\\Windows\\SysWOW64\\UsrPkcs11.dll").exists()) p11lib = "C:\\Windows\\SysWOW64\\UsrPkcs11.dll";
			else p11lib = "C:\\Windows\\System32\\UsrPkcs11.dll";
		}
//		else if (Platform.OS.LINUX.equals(Platform.getOS())) {
//			p11lib = null;
//		}
//		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
//			p11lib = null;
//		}
		AOKeyStoreManager ksm = null;
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
		}
		assertNotNull(ksm);
		assertTrue(ksm.getKeyStores().size() > 0);
		assertNotNull(ksm.getKeyStores().get(0));
		System.out.println();
	}

	
}
