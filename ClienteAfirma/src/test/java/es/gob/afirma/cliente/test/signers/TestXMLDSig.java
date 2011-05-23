package es.gob.afirma.cliente.test.signers;

import static es.gob.afirma.cliente.test.TestUtils.ALGOS;
import static es.gob.afirma.cliente.test.TestUtils.BIN_CONTENT;
import static es.gob.afirma.cliente.test.TestUtils.CS_TARGETS;
import static es.gob.afirma.cliente.test.TestUtils.KEYSTORES;
import static es.gob.afirma.cliente.test.TestUtils.TEST_CONTENT;
import static es.gob.afirma.cliente.test.TestUtils.XMLDSIG_MODES;
import static es.gob.afirma.cliente.test.TestUtils.getValidKeyEntryFromKeyStore;
import static org.junit.Assert.assertNotNull;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.cliente.test.TestUtils;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOXMLDSigSigner;

public final class TestXMLDSig {

	@Test
	public void testSignature() {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
		final AOSigner signer = new AOXMLDSigSigner();
		assertNotNull("No se ha podido instanciar el Signer", signer);
		byte[] result = null;
		PrivateKeyEntry pke = null;
		for (AOConstants.AOKeyStore kstore : KEYSTORES) {
			for (byte[] content : TEST_CONTENT) {
				for (String algo : ALGOS) {
					for (Properties extraParams : XMLDSIG_MODES) {

						System.out.println();
						System.out.println();
						System.out.println();
						System.out.print("Prueba con '" + algo + "', '"
								+ kstore + "', formato '"
								+ extraParams.getProperty("format")
								+ "' y contenido ");
						if (content.equals(TestUtils.BIN_CONTENT))
							System.out.println("binario");
						else
							System.out.println("XML");

						if (AOConstants.AOKeyStore.WINDOWS.equals(kstore)
								&& AOConstants.SIGN_ALGORITHM_SHA512WITHRSA
										.equals(algo)) {
							System.out
									.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
							continue;
						}
						if (AOKeyStore.APPLE.equals(kstore)
								&& (!Platform.OS.MACOSX
										.equals(Platform.getOS()))) {
							System.out
									.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
							continue;
						}
						if (AOKeyStore.WINDOWS.equals(kstore)
								&& (!Platform.OS.WINDOWS.equals(Platform
										.getOS()))) {
							System.out
									.println("Omitimos las pruebas de CAPI si no estamos en Windows");
							continue;
						}
						if (AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED
								.equals(extraParams.getProperty("format"))
								&& content.equals(BIN_CONTENT)) {
							System.out
									.println("Omitimos las pruebas en ENVELOPED con contenido binario");
							continue;
						}
						try {
							pke = getValidKeyEntryFromKeyStore(kstore);
							assertNotNull(
									"No hemos encontrado una clave de prueba en "
											+ kstore, pke);
							result = null;
							result = signer.sign(content, algo, pke,
									extraParams);
						} catch (final Throwable e) {
							e.printStackTrace();
							result = null;
						}
						assertNotNull("El resultado de la firma es nulo",
								result);

					}
				}
			}
		}
	}

	@Test
	public void testCounterSignature() {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
		final AOSigner signer = new AOXMLDSigSigner();
		assertNotNull("No se ha podido instanciar el Signer", signer);
		byte[] result = null;
		PrivateKeyEntry pke = null;

		byte[] xml = null;
		// Primero cargamos el XML a contrafirmar
		try {
			xml = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.xsig"));
		} catch (final Throwable e) {
			e.printStackTrace();
			xml = null;
		}
		if (xml != null && xml.length == 0)
			xml = null;
		assertNotNull("No se ha podido cargar el XML a contrafirmar", xml);

		for (AOConstants.AOKeyStore kstore : KEYSTORES) {
			for (String algo : ALGOS) {
				for (Properties extraParams : XMLDSIG_MODES) {
					for (CounterSignTarget targetType : CS_TARGETS) {

						System.out.println();
						System.out.println();
						System.out.println();
						System.out.println("Prueba CONTRAFIRMA con '" + algo
								+ "', '" + kstore + "', formato '"
								+ extraParams.getProperty("format")
								+ "', objetivo '" + targetType + "' y modo '"
								+ extraParams.getProperty("mode") + "'");
						if (AOConstants.AOKeyStore.WINDOWS.equals(kstore)
								&& AOConstants.SIGN_ALGORITHM_SHA512WITHRSA
										.equals(algo)) {
							System.out
									.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
							continue;
						}
						if (AOKeyStore.APPLE.equals(kstore)
								&& (!Platform.OS.MACOSX
										.equals(Platform.getOS()))) {
							System.out
									.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
							continue;
						}
						if (AOKeyStore.WINDOWS.equals(kstore)
								&& (!Platform.OS.WINDOWS.equals(Platform
										.getOS()))) {
							System.out
									.println("Omitimos las pruebas de CAPI si no estamos en Windows");
							continue;
						}
						try {
							pke = getValidKeyEntryFromKeyStore(kstore);
							assertNotNull(
									"No hemos encontrado una clave de prueba en "
											+ kstore, pke);
							result = null;
							result = signer.countersign(xml, algo, targetType,
									new Object[0], pke, extraParams);
						} catch (final Throwable e) {
							e.printStackTrace();
							result = null;
						}
						assertNotNull("El resultado de la firma es nulo",
								result);

					}
				}
			}
		}
	}

	@Test
	public void testCoSignature() {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING);
		final AOSigner signer = new AOXMLDSigSigner();
		assertNotNull("No se ha podido instanciar el Signer", signer);
		byte[] result = null;
		PrivateKeyEntry pke = null;

		byte[] xml = null;
		// Primero cargamos el XML a contrafirmar
		try {
			xml = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("Contrafirma3.xsig"));
		} catch (final Throwable e) {
			e.printStackTrace();
			xml = null;
		}
		if (xml != null && xml.length == 0)
			xml = null;
		assertNotNull("No se ha podido cargar el XML a contrafirmar", xml);

		for (AOConstants.AOKeyStore kstore : KEYSTORES) {
			for (String algo : ALGOS) {
				for (Properties extraParams : XMLDSIG_MODES) {

					System.out.println();
					System.out.println();
					System.out.println();
					System.out.println("Prueba COFIRMA con '" + algo + "', '"
							+ kstore + "', formato '"
							+ extraParams.getProperty("format") + "' y modo '"
							+ extraParams.getProperty("mode") + "'");
					if (AOConstants.AOKeyStore.WINDOWS.equals(kstore)
							&& AOConstants.SIGN_ALGORITHM_SHA512WITHRSA
									.equals(algo)) {
						System.out
								.println("Omitimos las pruebas en CAPI con SHA2 por errores conocidos de Java");
						continue;
					}
					if (AOKeyStore.APPLE.equals(kstore)
							&& (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
						System.out
								.println("Omitimos las pruebas de Llavero Apple si no estamos en Mac OS X");
						continue;
					}
					if (AOKeyStore.WINDOWS.equals(kstore)
							&& (!Platform.OS.WINDOWS.equals(Platform.getOS()))) {
						System.out
								.println("Omitimos las pruebas de CAPI si no estamos en Windows");
						continue;
					}
					try {
						pke = getValidKeyEntryFromKeyStore(kstore);
						assertNotNull(
								"No hemos encontrado una clave de prueba en "
										+ kstore, pke);
						result = null;
						result = signer.cosign(xml, algo, pke, extraParams);
					} catch (final Throwable e) {
						e.printStackTrace();
						result = null;
					}
					assertNotNull("El resultado de la firma es nulo", result);

				}
			}
		}
	}

}
