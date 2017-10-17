/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
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

/** Generador de conjuntos completos de firmas. */
public class GenerateEPESSigns {

	private static final boolean DEBUG = true;

	private static final boolean applyAlgos = false;

	private static final String DEFAULT_ALGO = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;

    private final static String[] ALGOS = new String[] {
        AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA
//        AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
//        AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
//        AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    /**
     * Formatos de los cuales ejecutarse el test.
     * Campo 1: Identificador del formato
     * Campo 2: Nombre para la generacion del nombre de fichero
     * Campo 3: Extension de firma
     * Campo 4: Soporta contrafirma
     */
    private static final String[][] CONFIGS = {

    	{"CAdES-EPES", AOSignConstants.SIGN_FORMAT_CADES, "csig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-SMALL-AGE", AOSignConstants.SIGN_FORMAT_CADES, "csig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-SMALL-AGE-IMP", AOSignConstants.SIGN_FORMAT_CADES, "csig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-SMALL-AGE-EXP", AOSignConstants.SIGN_FORMAT_CADES, "csig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-LARGE-AGE", AOSignConstants.SIGN_FORMAT_CADES, "csig", "large"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-LARGE-AGE-IMP", AOSignConstants.SIGN_FORMAT_CADES, "csig", "large"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"CAdES-LARGE-AGE-EXP", AOSignConstants.SIGN_FORMAT_CADES, "csig", "large"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_alt1", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_alt2", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "pdf"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_factura30_alt1", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "factura"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_factura30_alt2", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "factura"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_factura31_alt1", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "factura"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	{"XAdES-EPES_factura31_alt2", AOSignConstants.SIGN_FORMAT_XADES, "xsig", "factura"} //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    };

    private static final String[][] CONFIG_PARAMS = {
    	{"CAdES-EPES", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "policyIdentifier=urn:oid:2.16.724.1.3.1.1.2.1.8"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "policyIdentifierHash=7SxX3erFuH31TvAw9LZ70N7p1vA="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "policyQualifier=http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "contentDescription=Adobe PDF Document"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-EPES", "contentTypeOid=1.2.840.10003.5.109.1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-SMALL-AGE", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-SMALL-AGE-IMP", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-SMALL-AGE-IMP", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-SMALL-AGE-EXP", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-SMALL-AGE-EXP", "mode=explicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-LARGE-AGE", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-LARGE-AGE-IMP", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-LARGE-AGE-IMP", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-LARGE-AGE-EXP", "expPolicy=FirmaAGE"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"CAdES-LARGE-AGE-EXP", "mode=explicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "format=XAdES Detached"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "policyIdentifier=urn:oid:2.16.724.1.3.1.1.2.1.8"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "policyIdentifierHash=V8lVVNGDCPen6VELRD1Ja8HARFk="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt1", "policyQualifier=http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "format=XAdES Detached"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "policyIdentifier=urn:oid:2.16.724.1.3.1.1.2.1.8"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "policyIdentifierHash=VYICYpNOjso9g1mBiXDVxNORpKk="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_alt2", "policyQualifier=http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "format=XAdES Enveloped"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "policyIdentifier=http://www.facturae.es/politica%20de%20firma%20formato%20facturae/politica%20de%20firma%20formato%20facturae%20v3_0.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "policyIdentifierHash=gVI+R5m6XW111fHWaeKEDnUoxtg="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt1", "policyQualifier=http://www.facturae.es/politica%20de%20firma%20formato%20facturae/politica%20de%20firma%20formato%20facturae%20v3_0.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "format=XAdES Enveloped"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "policyIdentifier=http://www.facturae.es/politica%20de%20firma%20formato%20facturae/politica%20de%20firma%20formato%20facturae%20v3_0.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "policyIdentifierHash=HQvPemjDslVpcNmaJPpbHzhdZ50="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura30_alt2", "policyQualifier=http://www.facturae.es/politica%20de%20firma%20formato%20facturae/politica%20de%20firma%20formato%20facturae%20v3_0.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "format=XAdES Enveloped"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "policyIdentifier=http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "policyIdentifierHash=T76hEwl/oPYW7o0EdCXjEWki4as="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt1", "policyQualifier=http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "mode=implicit"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "format=XAdES Enveloped"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "policyIdentifier=http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "policyIdentifierHash=Ohixl6upD6av8N7pEvDABhEL6hM="}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "policyIdentifierHashAlgorithm=http://www.w3.org/2000/09/xmldsig#sha1"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-EPES_factura31_alt2", "policyQualifier=http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"} //$NON-NLS-1$ //$NON-NLS-2$
	};

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    /**
     * Genera todos los tipos de firmas posibles.
     * @throws AOException Cuando ocurre un problema durante la firma.
     * @throws NoSuchAlgorithmException Cuando no se puede identificar el algoritmo del almac&eacute;n.
     * @throws GeneralSecurityException Cuando hay un problema en la carga del almac&eacute;n.
     * @throws IOException Cuando ocurre un error de carga de datos (ficheros, almacen, ...)
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
		for(final String[] config : CONFIGS) {
			final String configName = config[0];
			signer = AOSignerFactory.getSigner(config[1]);
			if (signer == null) {
				System.out.println("No se encontro el manejador del formato " + config[1]); //$NON-NLS-1$
				continue;
			}

			String paramsString = ""; //$NON-NLS-1$
			for(final String[] params : CONFIG_PARAMS) {
				if (params[0].equals(configName)) {
					paramsString += params[1] + "\n"; //$NON-NLS-1$
				}
			}

			extraParams.load(new ByteArrayInputStream(paramsString.getBytes()));

			for(final String algo : algos) {
				final byte[] signature = signer.sign(
					files.get(config[3]),
					algo,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					extraParams);

				if (DEBUG) {
					saveSign(signature, "Firma_" + configName + "_" + config[3] + "_" + algo + "." + config[2]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				}
			}
		}
	}

    private static Map<String, byte[]> loadFiles() throws IOException {
    	byte[] data;
    	final ConcurrentHashMap<String, byte[]> files = new ConcurrentHashMap<>();
    	for (final String[] config : CONFIGS) {
    		if (!files.contains(config[3])) {
				data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(config[3]));
    			files.put(config[3], data);
    		}
    	}
    	return files;
	}

    private static void saveSign(final byte[] signData, final String filename) throws IOException {
	    	final File signFile = File.createTempFile(filename, null);
	    	System.out.println("Fichero para comprobacion manual: " + signFile.getAbsolutePath()); //$NON-NLS-1$
	    	try (
			final FileOutputStream fos = new FileOutputStream(signFile);
		) {
	    		fos.write(signData);
	    	}
    }
}
