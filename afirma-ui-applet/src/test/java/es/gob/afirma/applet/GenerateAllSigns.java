/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;

/** Generadoe de conjuntos completos de firmas. */
public class GenerateAllSigns {

	private static final boolean applyAlgos = false;

	private static final String DEFAULT_ALGO = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;

    private final static String[] ALGOS = new String[] {
        AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
        AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
        AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
        AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    /**
     * Formatos de los cuales ejecutarse el test.
     * Campo 1: Identificador del formato
     * Campo 2: Nombre para la generacion del nombre de fichero
     * Campo 3: Extension de firma
     * Campo 4: Soporta contrafirma
     */
    private static final String[][] FORMATS = {
    	{AOSignConstants.SIGN_FORMAT_CMS, "CMS", "csig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_CADES, "CADES", "csig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "XADES_DETACHED", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "XADES_ENVELOPING", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, "XADES_ENVELOPED", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "XMLDSIG_DETACHED", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "XMLDSIG_ENVELOPING", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, "XMLDSIG_ENVELOPED", "xsig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_ODF, "ODF", "odt", "false"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_OOXML, "OOXML", "docx", "false"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{AOSignConstants.SIGN_FORMAT_PDF, "PDF", "pdf", "false"} //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    };

    private static final String[][] FORMATS_FILES = {
    	{AOSignConstants.SIGN_FORMAT_CMS, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_CADES, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XADES_DETACHED, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "bin"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, "xml"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_PDF, "pdf"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_ODF, "odt"},  //$NON-NLS-1$
    	{AOSignConstants.SIGN_FORMAT_OOXML, "docx"} //$NON-NLS-1$
    };

    private static final String[][] FORMATS_MODES = {
    	{AOSignConstants.SIGN_FORMAT_CMS, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_CMS, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_CADES, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XADES_DETACHED, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XADES_DETACHED, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, AOSignConstants.SIGN_MODE_EXPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_PDF, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_ODF, AOSignConstants.SIGN_MODE_IMPLICIT},
    	{AOSignConstants.SIGN_FORMAT_OOXML, AOSignConstants.SIGN_MODE_IMPLICIT}
	};

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String CERT_PATH2 = "ANF_PJ_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS2 = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS2 = "anf usuario activo"; //$NON-NLS-1$

    private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$

    /**
     * Genera todos los tipos de firmas posibles.
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateSigns() {

    	String path = null;
		try {
			path = File.createTempFile("temp", null).getParent(); //$NON-NLS-1$
		} catch (final IOException e) {
			Assert.fail("No se pudo obtener el directorio temporal para la prueba de los datos: " + e.toString()); //$NON-NLS-1$
		}

    	final SignApplet applet = new SignApplet();
    	applet.initialize();
    	final String ksPath = GenerateAllSigns.getResourcePath(CERT_PATH);
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
		for (final String[] format : FORMATS) {
			applet.setSignatureFormat(format[0]);
			for (final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					applet.setSignatureMode(mode[1]);

					for (final String[] file : FORMATS_FILES) {

						if (format[0].equals(file[0])) {
							applet.setFileuri(GenerateAllSigns.getResourcePath(file[1]));
							for(final String algo : algos) {
								applet.setSignatureAlgorithm(algo);
								applet.sign();
								Assert.assertFalse("Error al firmar en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								final String signaturePath = path + "/" + "Firma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
								applet.setOutFilePath(signaturePath);
								Logger.getLogger("es.gob.afirma").info("Se almacenara la firma para su posterior validacion: " + signaturePath); //$NON-NLS-1$ //$NON-NLS-2$
								applet.saveSignToFile();
							}
						}
					}
				}
			}
		}
	}

    /**
     * Genera todos los tipos de cofirmas posibles (respectando que la cofirma se
     * realice sobre una firma del mismo tipo).
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateCosigns() {

    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

    	final SignApplet applet = new SignApplet();
    	applet.initialize();
    	final String ksPath = GenerateAllSigns.getResourcePath(CERT_PATH2).toString();
    	applet.setKeyStore(ksPath, CERT_PASS2, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS2);

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
		for (final String[] format : FORMATS) {
			applet.setSignatureFormat(format[0]);
			for (final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					applet.setSignatureMode(mode[1]);

					for (final String[] file : FORMATS_FILES) {

						if (format[0].equals(file[0])) {
							for(final String algo : algos) {
								applet.setSignatureAlgorithm(algo);

								applet.setFileuri(GenerateAllSigns.getResourcePath(file[1]));
								applet.sign();
								Assert.assertFalse("Error al firmar en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								applet.setOutFilePath(path + "/" + "Firma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
								applet.saveSignToFile();

								applet.setFileuri(GenerateAllSigns.getResourcePath(file[1]));
								applet.setElectronicSignatureFile(path + "/" + "Firma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
								applet.coSign();
								Assert.assertFalse("Error al cofirmar en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								applet.setOutFilePath(path + "/" + "Cofirma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
								applet.saveSignToFile();
							}
						}
					}
				}
			}
		}
	}

    /**
     * Genera todos los tipos de firmas posibles.
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateCounterSigns() {

    	final SignApplet applet = new SignApplet();
    	applet.initialize();
    	final String ksPath = GenerateAllSigns.getResourcePath(CERT_PATH3);
    	applet.setKeyStore(ksPath, CERT_PASS3, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS3);

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
		for (final String[] format : FORMATS) {

			// Si no se soporta la contrafirma para este formato
			if (!Boolean.parseBoolean(format[3])) {
				continue;
			}

			applet.setSignatureFormat(format[0]);
			for (final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					applet.setSignatureMode(mode[1]);
					for (final String[] file : FORMATS_FILES) {
						if (format[0].equals(file[0])) {
							for(final String algo : algos) {
								applet.setSignatureAlgorithm(algo);

								// Cada operacion de firma deja establecida internamente cual es la firma resultado, de tal forma que, al no indicar otra,
								// las subsiguientes operaciones tomaran esa firma como entrada

								applet.setFileuri(GenerateAllSigns.getResourcePath(file[1]));
								applet.sign();
								Assert.assertFalse("Error al firmar en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								applet.setFileuri(GenerateAllSigns.getResourcePath(file[1]));
								applet.coSign();
								Assert.assertFalse("Error al cofirmar en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								applet.counterSignTree();
								Assert.assertFalse("Error al contrafirmar el arbol en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								applet.counterSignLeafs();
								Assert.assertFalse("Error al contrafirmar las hojas en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								applet.setSignersToCounterSign("0\n2"); //$NON-NLS-1$
								applet.counterSignIndexes();
								Assert.assertFalse("Error al contrafirmar nodos en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								final String signers = applet.getSignersStructure().replace("\t", "").replace("\r", "").trim(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
								applet.setSignersToCounterSign(signers.substring(0, signers.indexOf('\n')));
								applet.counterSignSigners();
								Assert.assertFalse("Error al contrafirmar firmantes en formato " + format[1] + " y modo " + mode[1] + ": " + applet.getErrorMessage(), applet.isError()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							}
						}
					}
				}
			}
		}
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }
}
