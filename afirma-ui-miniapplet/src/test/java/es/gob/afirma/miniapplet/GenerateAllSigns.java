/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;

/** Generador de conjuntos completos de firmas. */
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
//    	{AOSignConstants.SIGN_FORMAT_CMS, "CMS", "csig", "true"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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

    private static final String SIGNS_PATH = "src" + File.separator + "test" + File.separator + "signs" + File.separator; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /**
     * Genera todos los tipos de firmas posibles.
     * @throws AOException Cuando ocurre alg&uacute;n error al generar las firmas.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de firma no est&aacute; soportado.
     * @throws GeneralSecurityException Cuando se produce un problema de seguridad.
     * @throws IOException Cuando ocurre un error en la lectura de los datos.
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateSigns() throws AOException, NoSuchAlgorithmException, GeneralSecurityException, IOException {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final Map<String, byte[]> files = loadFiles();

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
    	final Properties extraParams = new Properties();

		AOSigner signer;
		for(final String[] format : FORMATS) {
			signer = AOSignerFactory.getSigner(format[0]);
			if (signer == null) {
				System.out.println("No se encontro el manejador del formato " + format[0]); //$NON-NLS-1$
				continue;
			}
			for(final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					extraParams.setProperty("mode", mode[1]); //$NON-NLS-1$
					for(final String[] file : FORMATS_FILES) {
						if (format[0].equals(file[0])) {
							for(final String algo : algos) {
								final byte[] signature = signer.sign(
									files.get(file[1]),
									algo,
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									extraParams
								);
								saveSign(
									signature, "Firma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2] //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
								);
							}
						}
					}
				}
			}
		}
	}

    /**
     * Genera todos los tipos de firmas posibles.
     * @throws AOException Cuando ocurre alg&uacute;n error al generar las firmas.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de firma no est&aacute; soportado.
     * @throws GeneralSecurityException Cuando se produce un problema de seguridad.
     * @throws IOException Cuando ocurre un error en la lectura de los datos.
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateCosigns() throws AOException, NoSuchAlgorithmException, GeneralSecurityException, IOException {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
    	final Properties extraParams = new Properties();

		AOSigner signer;
		for(final String[] format : FORMATS) {
			signer = AOSignerFactory.getSigner(format[0]);
			if (signer == null) {
				System.out.println("No se encontro el manejador del formato " + format[0]); //$NON-NLS-1$
				continue;
			}
			for(final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					extraParams.setProperty("mode", mode[1]); //$NON-NLS-1$
					for(final String[] file : FORMATS_FILES) {
						if (format[0].equals(file[0])) {
							for(final String algo : algos) {
								final String filename = "Firma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
								final byte[] signature = signer.cosign(
									loadFile(SIGNS_PATH + filename),
									algo,
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									extraParams
								);
								saveSign(signature, "Cofirma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

							}
						}
					}
				}
			}
		}
	}

    /**
     * Genera todos los tipos de firmas posibles.
     * @throws AOException Cuando ocurre alg&uacute;n error al generar las firmas.
     * @throws NoSuchAlgorithmException Cuando el algoritmo de firma no est&aacute; soportado.
     * @throws GeneralSecurityException Cuando se produce un problema de seguridad.
     * @throws IOException Cuando ocurre un error en la lectura de los datos.
     */
    @SuppressWarnings("static-method")
	@Test
	public void generateCounterSigns() throws AOException, NoSuchAlgorithmException, GeneralSecurityException, IOException {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH3), CERT_PASS3.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS3, new KeyStore.PasswordProtection(CERT_PASS3.toCharArray()));

    	final String[] algos = applyAlgos ? ALGOS : new String[] {DEFAULT_ALGO};
    	final Properties extraParams = new Properties();

		AOSigner signer;
		for(final String[] format : FORMATS) {
			// Si no se soporta la contrafirma para este formato
			if (!Boolean.parseBoolean(format[3])) {
				continue;
			}

			signer = AOSignerFactory.getSigner(format[0]);
			if (signer == null) {
				System.out.println("No se encontro el manejador del formato " + format[0]); //$NON-NLS-1$
				continue;
			}
			for(final String[] mode : FORMATS_MODES) {
				if (format[0].equals(mode[0])) {
					for(final String[] file : FORMATS_FILES) {
						if (format[0].equals(file[0])) {
							for(final String algo : algos) {
								final String filename = "Cofirma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
								final byte[] signature = signer.countersign(
									loadFile(SIGNS_PATH + filename),
									algo,
									CounterSignTarget.LEAFS,
									null,
									pke.getPrivateKey(),
									pke.getCertificateChain(),
									extraParams
								);
								saveSign(signature, "Contrafirma_" + format[1] + "_" + mode[1] + "_" + file[1] + "_" + algo + "." + format[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
							}
						}
					}
				}
			}
		}
	}

    private static Map<String, byte[]> loadFiles() throws IOException {
    	final ConcurrentHashMap<String, byte[]> files = new ConcurrentHashMap<>();
    	for (final String[] formatsFiles : FORMATS_FILES) {
    		if (!files.contains(formatsFiles[1])) {
				files.put(
					formatsFiles[1],
					AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(formatsFiles[1]))
				);
    		}
    	}
    	return files;
	}

    private static byte[] loadFile(final String path) throws IOException {
    		try (
			final InputStream fis = new FileInputStream(path);
		) {
    			return AOUtil.getDataFromInputStream(fis);
    		}
    }

    private static void saveSign(final byte[] signData, final String filename) throws IOException {
	    	final File signFile = new File(SIGNS_PATH + filename);
	    	try (
			final OutputStream fos = new FileOutputStream(signFile);
		) {
	    		fos.write(signData);
	    		fos.flush();
	    	}
    }
}
