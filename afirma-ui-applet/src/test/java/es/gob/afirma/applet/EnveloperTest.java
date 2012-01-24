/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.File;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;

/** Generadoe de conjuntos completos de firmas. */
public class EnveloperTest {

	private static final String PLAIN_DATA_FILE = "plain"; //$NON-NLS-1$

	private static final String ENVELOP_DATA_PREFIX = "envelop_"; //$NON-NLS-1$

	private static final String CIPHER_AES_KEY = "41vAOZAQqNLA591mdcBwVw=="; //$NON-NLS-1$

	private static final String CIPHER_PBE_PASS = "(0NtrA5en4"; //$NON-NLS-1$

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String CERT_PATH2 = "ANF_PJ_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS2 = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS2 = "anf usuario activo"; //$NON-NLS-1$

    /**
    * Genera un envoltorio CMS encriptado.
    */
    @SuppressWarnings("static-method")
    @Test
    public void encryptedDataTest() {

    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

    	final String plainDataFile = this.getResourcePathToApplet(PLAIN_DATA_FILE);

    	final SignApplet applet = new SignApplet();
    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

    	applet.setUseCipherKeyStore(false);
    	applet.setData(plainDataB64);
    	applet.setCMSContentType(AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA);
    	applet.buildCMSStructure();
    	Assert.assertEquals(applet.isError(), false);

    	final String b64Data = applet.getB64Data();
    	Assert.assertNotNull(b64Data);

    	final String b64Key = applet.getKey();
    	Assert.assertNotNull(b64Key);

    	System.out.println(b64Key);

    	applet.initialize();

    	applet.setData(b64Data);
    	applet.setKey(b64Key);
    	applet.recoverCMS();

    	final String resultData = applet.getB64Data();
    	Assert.assertNotNull(resultData);
    	Assert.assertEquals(plainDataB64, resultData);
    }

    /**
     * Genera un envoltorio CMS envuelto.
     */
    @SuppressWarnings("static-method")
	@Test
	public void envelopedDataTest() {

    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

    	final String plainDataFile = this.getResourcePathToApplet(PLAIN_DATA_FILE);

    	final SignApplet applet = new SignApplet();
    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

    	final String ksPath = this.getResourcePath(CERT_PATH);
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	final String certB64 = applet.getSignCertificateBase64Encoded();
    	applet.addRecipientToCMS(certB64);

    	applet.setData(plainDataB64);
    	applet.setCMSContentType(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA);
    	applet.buildCMSStructure();
    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

    	final String b64Data = applet.getB64Data();
    	Assert.assertNotNull(b64Data);

    	applet.initialize();

    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);
    	applet.setData(b64Data);
    	applet.recoverCMS();
    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

    	final String resultData = applet.getB64Data();
    	Assert.assertNotNull(resultData);
    	Assert.assertEquals(plainDataB64, resultData);
	}

    /**
     * Genera un envoltorio CMS firmado y envuelto.
     */
    @SuppressWarnings("static-method")
	@Test
	public void signedAndEnvelopedDataTest() {

    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

    	final String plainDataFile = this.getResourcePathToApplet(PLAIN_DATA_FILE);

    	final SignApplet applet = new SignApplet();
    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

    	final String ksPath = this.getResourcePath(CERT_PATH);
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	final String certB64 = applet.getSignCertificateBase64Encoded();
    	applet.addRecipientToCMS(certB64);

    	final String ksPath2 = this.getResourcePath(CERT_PATH2);
    	applet.setKeyStore(ksPath2, CERT_PASS2, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS2);

    	applet.setData(plainDataB64);
    	applet.setCMSContentType(AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
    	applet.buildCMSStructure();
    	Assert.assertEquals(applet.isError(), false);

    	final String b64Data = applet.getB64Data();
    	Assert.assertNotNull(b64Data);

    	applet.initialize();

    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);
    	applet.setData(b64Data);
    	applet.recoverCMS();

    	final String resultData = applet.getB64Data();
    	Assert.assertNotNull(resultData);
    	Assert.assertEquals(plainDataB64, resultData);
	}

    /**
    * Genera un envoltorio CMS autenticado y envuelto.
    */
   @SuppressWarnings("static-method")
	@Test
	public void authenticatedAndEnvelopedDataTest() {

	   final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

	   final String plainDataFile = this.getResourcePathToApplet(PLAIN_DATA_FILE);

	   final SignApplet applet = new SignApplet();
	   final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

	   final String ksPath = this.getResourcePath(CERT_PATH);
	   applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	   applet.setSelectedCertificateAlias(CERT_ALIAS);

	   final String certB64 = applet.getSignCertificateBase64Encoded();
	   applet.addRecipientToCMS(certB64);

	   final String ksPath2 = this.getResourcePath(CERT_PATH2);
	   applet.setKeyStore(ksPath2, CERT_PASS2, "PKCS12"); //$NON-NLS-1$
	   applet.setSelectedCertificateAlias(CERT_ALIAS2);

	   applet.setData(plainDataB64);
	   applet.setCMSContentType(AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA);
	   applet.buildCMSStructure();
	   Assert.assertEquals(applet.isError(), false);

	   final String b64Data = applet.getB64Data();
	   Assert.assertNotNull(b64Data);

	   applet.initialize();

	   applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	   applet.setSelectedCertificateAlias(CERT_ALIAS);
	   applet.setData(b64Data);
	   applet.recoverCMS();

	   final String resultData = applet.getB64Data();
	   Assert.assertNotNull(resultData);
	   Assert.assertEquals(plainDataB64, resultData);
   }

    private String getResourcePath(final String filename) {
    	return EnveloperTest.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private String getResourcePathToApplet(final String filename) {
    	return EnveloperTest.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
