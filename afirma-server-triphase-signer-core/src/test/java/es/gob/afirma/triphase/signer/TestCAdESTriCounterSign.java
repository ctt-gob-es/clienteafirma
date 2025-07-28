/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseDataSigner;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.triphase.signer.processors.CAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

/** Pruebas de contrafirmas trif&aacute;sicas CAdES. */
public class TestCAdESTriCounterSign {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String CERT_PATH_TO_SIGN = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS_TO_SIGN = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS_TO_SIGN = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

    private static final String CERT_PATH_TO_COSIGN = "00_empleado_publico-hsm.p12"; //$NON-NLS-1$
    private static final String CERT_PASS_TO_COSIGN = "12345"; //$NON-NLS-1$
    private static final String CERT_ALIAS_TO_COSIGN = "nombre apellido1 apellido2 - dni 12345678z"; //$NON-NLS-1$

	private static final String DATA_FILE = "xml"; //$NON-NLS-1$

	private byte[] data;

	private Properties implicitParams;

	private Properties explicitParams;

	private PrivateKeyEntry pke = null;

	private byte[] explicitSHA256Signature = null;

	private byte[] explicitSHA512Signature = null;

	private byte[] implicitSHA256Signature = null;

	private byte[] implicitSHA512Signature = null;

	private AOSigner signer = null;

	private TriPhasePreProcessor processor = null;

	/** Antes de ejecutar cualquier prueba se ejecutar&aacute; este m&eacute;todo que cargar6aacute;
	 * todos los objetos que se vaya a necesitar en las distintas pruebas.
	 * @throws Exception En cualquier error. */
	@Before
	public void loadParams() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		this.implicitParams = new Properties();
		this.implicitParams.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		this.explicitParams = new Properties();
		this.explicitParams.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_TO_SIGN), CERT_PASS_TO_SIGN.toCharArray());
		final PrivateKeyEntry pkeSign = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS_TO_SIGN, new KeyStore.PasswordProtection(CERT_PASS_TO_SIGN.toCharArray()));

		final KeyStore ksCosign = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ksCosign.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_TO_COSIGN), CERT_PASS_TO_COSIGN.toCharArray());
		this.pke = (PrivateKeyEntry) ksCosign.getEntry(CERT_ALIAS_TO_COSIGN, new KeyStore.PasswordProtection(CERT_PASS_TO_COSIGN.toCharArray()));

		try {
			this.data = AOUtil.getDataFromInputStream(TestCAdESTriCounterSign.class.getResourceAsStream("/" + DATA_FILE)); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido cargar el fichero de pruebas: " + DATA_FILE, e);  //$NON-NLS-1$
		}

		this.signer = new AOCAdESSigner();
		this.explicitSHA256Signature = this.signer.sign(this.data, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pkeSign.getPrivateKey(), pkeSign.getCertificateChain(), this.explicitParams);
		this.explicitSHA512Signature = this.signer.sign(this.data, AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, pkeSign.getPrivateKey(), pkeSign.getCertificateChain(), this.explicitParams);
		this.implicitSHA256Signature = this.signer.sign(this.data, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pkeSign.getPrivateKey(), pkeSign.getCertificateChain(), this.implicitParams);
		this.implicitSHA512Signature = this.signer.sign(this.data, AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, pkeSign.getPrivateKey(), pkeSign.getCertificateChain(), this.implicitParams);

		this.processor = new CAdESTriPhasePreProcessor();
	}

	/** Contrafirma con algoritmo SHA512withRSA sobre firma impl&iacute;cita
	 * con algoritmo de SHA512withRSA.
	 * @throws Exception en cualquier error. */
	@Test
	public void testContrafirmaSHA512SobreFirmaImplicitaSHA512() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = countersign(
				this.implicitSHA512Signature, algorithm, null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCAdESTriCounterSign() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCounterSign(result, algorithm, true);
	}


	/** Contrafirma con algoritmo SHA512withRSA sobre firma expl&iacute;cita
	 * con algoritmo de SHA512withRSA.
	 * @throws Exception en cualquier error. */
	@Test
	public void testContrafirmaSHA512SobreFirmaExplicitaSHA512() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = countersign(
				this.explicitSHA512Signature, algorithm, null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCAdESTriCounterSign() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCounterSign(result, algorithm, false);
	}

	/** Contrafirma con algoritmo SHA512withRSA sobre firma impl&iacute;cita
	 * con algoritmo de SHA256withRSA.
	 * @throws Exception en cualquier error. */
	@Test
	public void testContrafirmaSHA512SobreFirmaImplicitaSHA256() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = countersign(
				this.implicitSHA256Signature, algorithm, null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCAdESTriCounterSign() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCounterSign(result, algorithm, true);
	}

	/** Contrafirma con algoritmo SHA512withRSA sobre firma expl&iacute;cita
	 * con algoritmo de SHA256withRSA.
	 * @throws Exception en cualquier error. */
	@Test
	public void testContrafirmaSHA512SobreFirmaExplicitaSHA256() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = countersign(
				this.explicitSHA256Signature, algorithm, null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCAdESTriCounterSign() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCounterSign(result, algorithm, false);
	}


	/**
	 * Ejecuta la contrafirma en tres fases.
	 * @param signature Firma a cofirmar.
	 * @param algorithm Algoritmo de firma.
	 * @param extraParams Configuraci&oacute;n.
	 * @return Firma electr&oacute;nica con la cofirma.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	private byte[] countersign(final byte[] signature, final String algorithm, final Properties extraParams) throws Exception {

		final X509Certificate[] certChain = (X509Certificate[]) this.pke.getCertificateChain();
		final CounterSignTarget targets = CounterSignTarget.LEAFS;

		// Prefirma
		TriphaseData td = this.processor.preProcessPreCounterSign(
				signature, algorithm, certChain, extraParams, targets, false);

		// Firma
		td = TriphaseDataSigner.doSign(
				new AOPkcs1Signer(),
				algorithm,
				this.pke.getPrivateKey(),
				certChain,
				td,
				extraParams);

		// Postfirma
		final byte[] result = this.processor.preProcessPostCounterSign(
				signature, algorithm, certChain, extraParams, td, targets);

		return result;
	}

	/**
	 * Guarda datos temporales en disco.
	 * @param data Datos a guardar.
	 * @return Fichero temporal generado.
	 * @throws IOException Cuando ocurre un error en el guardado del fichero en disco.
	 */
	private static File saveTempFile(final byte[] data) throws IOException {
		final File saveFile = File.createTempFile("testCounterSign-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try ( final OutputStream os = new FileOutputStream(saveFile) ) {
			os.write(data);
			os.flush();
		}
		return saveFile;
	}


	/**
	 * Realiza las comprobaciones necesarias sobre la firma generada.
	 * @param signature Firma generada.
	 * @param algorithm Algoritmo de firma empleado en la nueva firma.
	 * @param hasData Indica si la firma debe contener los datos firmados.
	 * @throws IOException Cuando ocurre un error al leer la firma.
	 * @throws AOException Cuando ocurre un error al extraer los datos originalmente firmados.
	 */
	private void checkCounterSign(final byte[] signature, final String algorithm, final boolean hasData) throws IOException, AOException {

		Assert.assertNotNull("Se ha obtenido una contrafirma nula", signature); //$NON-NLS-1$
		Assert.assertTrue("La contrafirma no se reconoce como firma CAdES", this.signer.isSign(signature)); //$NON-NLS-1$

		final byte[] contentData = this.signer.getData(signature);
		if (hasData) {
			Assert.assertNotNull("No se han encontrado los datos en la contrafirma", contentData); //$NON-NLS-1$
			Assert.assertNotEquals("El campo de datos de la firma esta vacio", contentData.length, 0); //$NON-NLS-1$
			Assert.assertTrue("Los datos extraidos de la firma no son los que originalmente se firmaron", Arrays.equals(contentData, this.data)); //$NON-NLS-1$
		}
		else {
			Assert.assertNull("El campo de datos de la firma deberia estar vacio", contentData); //$NON-NLS-1$
		}

		final AOTreeModel tree = this.signer.getSignersStructure(signature, true);

		final AOTreeNode rootNode = (AOTreeNode) tree.getRoot();
		Assert.assertEquals("No se interpreto correctamente la estructura de la contrafirma", "Datos", rootNode.getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$

		Assert.assertTrue("El resultado no tiene la estructura que se espera de una firma con una contrafirma", //$NON-NLS-1$
				rootNode.getChildCount() == 1 && rootNode.getChildAt(0).getChildCount() == 1);

		final AOSimpleSignInfo counterSignInfo = (AOSimpleSignInfo) rootNode.getChildAt(0).getChildAt(0).getUserObject();
		Assert.assertEquals("La contrafirma no se genero con el certificado esperado", //$NON-NLS-1$
				this.pke.getCertificate(), counterSignInfo.getCerts()[0]);

		Assert.assertEquals("El algoritmo de firma no es el esperado", algorithm, counterSignInfo.getSignAlgorithm()); //$NON-NLS-1$
		Assert.assertNotNull("No se encontro la hora de firma", counterSignInfo.getSigningTime()); //$NON-NLS-1$
	}
}
