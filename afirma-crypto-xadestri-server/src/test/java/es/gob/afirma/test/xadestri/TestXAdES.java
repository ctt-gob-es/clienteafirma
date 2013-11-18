/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.xadestri;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.xadestri.server.XAdESTriPhaseSignerServerSide;
import es.gob.afirma.signers.xadestri.server.XmlPreSignResult;

/**
 * Pruebas del m&oacute;dulo XAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestXAdES {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String SIGNATURE_FILENAME = "firma.xml"; //$NON-NLS-1$
	private static final String COSIGNATURE_FILENAME = "cofirma.xml"; //$NON-NLS-1$

	/** Prueba de prefirma simple.
	 * @throws Exception en cualquier error */
	@SuppressWarnings("static-method")
	@Test
	public void testPreSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final XmlPreSignResult pre = XAdESTriPhaseSignerServerSide.preSign("DATA".getBytes(), "SHA512withRSA", pke.getCertificateChain(), null, XAdESTriPhaseSignerServerSide.Op.SIGN); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Firma:\n" + new String(pre.getXmlSign())); //$NON-NLS-1$
	}

	/** Prueba de cofirma en 3 fases.
	 * @throws Exception en cualquier error */
	@SuppressWarnings("static-method")
	@Test
	public void testCoSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(SIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final XmlPreSignResult pre = XAdESTriPhaseSignerServerSide.preSign(sign, "SHA512withRSA", pke.getCertificateChain(), null, XAdESTriPhaseSignerServerSide.Op.COSIGN); //$NON-NLS-1$

		System.out.println("Cofirma:\n" + new String(pre.getXmlSign())); //$NON-NLS-1$
	}

	/** Prueba de contrafirma en 3 fases.
	 * @throws Exception en cualquier error */
	@SuppressWarnings("static-method")
	@Test
	public void testCounterSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(COSIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final XmlPreSignResult pre = XAdESTriPhaseSignerServerSide.preSign(sign, "SHA512withRSA", pke.getCertificateChain(), null, XAdESTriPhaseSignerServerSide.Op.COUNTERSIGN); //$NON-NLS-1$

		System.out.println("Contrafirma:\n" + new String(pre.getXmlSign())); //$NON-NLS-1$
	}
}
