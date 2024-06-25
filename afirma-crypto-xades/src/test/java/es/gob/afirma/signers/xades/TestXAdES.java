/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.DigestMethod;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;

/** Pruebas del m&oacute;dulo XAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXAdES {

    private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R__1234.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

    private static final String CERT_EC_PATH = "ciudadanohw_ecc_2023v1.p12"; //$NON-NLS-1$
    private static final String CERT_EC_PASS = "ciudadanohw_ecc_2023v1"; //$NON-NLS-1$
    private static final String CERT_EC_ALIAS = "manuela blanco vidal - nif:10000322z"; //$NON-NLS-1$

    private static final Properties[] XADES_MODES;

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "V8lVVNGDCPen6VELRD1Ja8HARFk=");  //$NON-NLS-1$//$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        p1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("addKeyInfoKeyValue", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("addKeyInfoKeyName", "true"); //$NON-NLS-1$ //$NON-NLS-2$
//        p1.setProperty("tsaURL", "https://psis.catcert.net/psis/catcert/tsp"); //$NON-NLS-1$ //$NON-NLS-2$
//        p1.setProperty("tsaRequireCert", "true"); //$NON-NLS-1$ //$NON-NLS-2$
//        p1.setProperty("tsaPolicy", "0.4.0.2023.1.1"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$


        final Properties p4 = new Properties();
        p4.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p4.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p5 = new Properties();
        p5.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p5.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p6 = new Properties();
        p6.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p6.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p6.setProperty("encoding", "Base64"); //$NON-NLS-1$ //$NON-NLS-2$


        XADES_MODES = new Properties[] {
                p1 , p2, p3, p4, p5, p6
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS_EC = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHECDSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHECDSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHECDSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHECDSA
    };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
            "PFActivoFirSHA256.pfx", //$NON-NLS-1$
//            "base64.b64", //$NON-NLS-1$
            "sample-class-attributes.xml", //$NON-NLS-1$
//            "sample-facturae.xml", //$NON-NLS-1$
            "sample-embedded-style.xml", //$NON-NLS-1$
//            "sample-encoding-UTF-8.xml", //$NON-NLS-1$
//            "sample-internal-dtd.xml", //$NON-NLS-1$
//            "sample-namespace-encoding-us-ascii.xml" //$NON-NLS-1$
            	"xmlwithremotestyle.xml" //$NON-NLS-1$

    };

    private static final String[][] TEST_FILES_MULTISIGN = new String[][] {
    	{"XAdES-Detached-SHA1withRSA-B64.xml", "false"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-Detached-SHA1withRSA-XML.xml", "false"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-Enveloped-SHA1withRSA-XML.xml", "true"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-Enveloping-SHA1withRSA-B64.xml", "false"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"XAdES-Enveloping-SHA1withRSA-XML.xml", "false"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"firmaMinetad.xsig", "false"}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"xades_con_manifest.xsig", "false"} //$NON-NLS-1$ //$NON-NLS-2$
    };

    /** Prueba de firma de nodo indicado expl&iacute;citamente.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testNodeTbs() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	try {
    		Logger.getLogger("com.sun.org.apache.xml.internal.security.utils.CachedXPathFuncHereAPI").setLevel(Level.WARNING); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		// Vacio
    	}

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS,
    		new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

    	final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")); //$NON-NLS-1$

    	final String[] formats = new String[] {
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
    			AOSignConstants.SIGN_FORMAT_XADES_DETACHED
    	};

    	final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("nodeToSign", "1"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$

    	for (final String format : formats) {
    		p.setProperty("format", format); //$NON-NLS-1$
    		final byte[] signature = signer.sign(data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				p
			);
    		final File f = File.createTempFile("xades-NODESIGN-" + format + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(signature);
				fos.flush();
			}
    		System.out.println("Firma " + format + " para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

    /** Prueba de firma de nodo indicado expl&iacute;citamente.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testNodeTbsWithNamespace() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	try {
    		Logger.getLogger("com.sun.org.apache.xml.internal.security.utils.CachedXPathFuncHereAPI").setLevel(Level.WARNING); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		// Vacio
    	}

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("SimpleXmlWithNamespace.xml")); //$NON-NLS-1$

    	final String[] formats = new String[] {
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
    			//AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
    			//AOSignConstants.SIGN_FORMAT_XADES_DETACHED
    	};

    	final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("nodeToSign", "Schroeder"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$

    	for (final String format : formats) {
    		p.setProperty("format", format); //$NON-NLS-1$
    		final byte[] signature = signer.sign(data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				p
			);
    		final File f = File.createTempFile("xades-NODESIGN-" + format + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(signature);
				fos.flush();
			}
    		System.out.println("Firma " + format + " para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

    /** Prueba de firma de nodo indicado expl&iacute;citamente e indicando el tipo de dato que se
     * est&aacute; firmando.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testNodeTbsWithMimeType() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	try {
    		Logger.getLogger("com.sun.org.apache.xml.internal.security.utils.CachedXPathFuncHereAPI").setLevel(Level.WARNING); //$NON-NLS-1$
    	}
    	catch(final Exception e) {
    		// Vacio
    	}

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = "<du:DocUniversitario xmlns:du=\"http://www.facturae.es/Facturae/2007/v3.1/Facturae\"><du:Documento encoding=\"base64\" Id=\"POR2iK5000\">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hE2aLwTLnPD/lQSYFi8FQ59+Bc/z0E+MEusClpNhLtwD5VABd8JMyIIqKEEFJ8F8vBuEMnDGexonA3zNAQnfpaF8zZK6B2r901etbFj18FLDLtmBb0Tm78joX/r1Q//gDQ3D/04IXWTojwkgff8deB7+PoiNYGEcvSPY/Q3YEscGeArRjNg/gregbgSb8DzKvmXu8blF4NDBA+/cL4e/ANuQGK8q61KN66GV8mYAvVLokoz/4+xfa/FPZDAv1QplbmRzdHJlYW0KZW5kb2JqCjEyIDAgb2JqCjw8L1R5cGUvTWV0YWRhdGEKL1N1YnR5cGUvWE1ML0xlbmd0aCAxNzQ3Pj5zdHJlYW0KPD94cGFja2V0IGJlZ2luPSfvu78nIGlkPSdXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQnPz4KPD9hZG9iZS14YXAtZmlsdGVycyBlc2M9IkNSTEYiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSdhZG9iZTpuczptZXRhLycgeDp4bXB0az0nWE1QIHRvb2xraXQgMi45LjEtMTMsIGZyYW1ld29yayAxLjYnPgo8cmRmOlJERiB4bWxuczpyZGY9J2h0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMnIHhtbG5zOmlYPSdodHRwOi8vbnMuYWRvYmUuY29tL2lYLzEuMC8nPgo8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0nNjYwZDVlMmEtN2NlMS0xMWRlLTAwMDAtZmQxNDgyNmJhZDIxJyB4bWxuczpwZGY9J2h0dHA6Ly9ucy5hZG9iZS5jb20vcGRmLzEuMy8nPjxwZGY6UHJvZHVjZXI+R1BMIEdob3N0c2NyaXB0IDguNjQ8L3BkZjpQcm9kdWNlcj4KPHBkZjpLZXl3b3Jkcz4oKTwvcGRmOktleXdvcmRzPgo8L3JkZjpEZXNjcmlwdGlvbj4KPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9JzY2MGQ1ZTJhLTdjZTEtMTFkZS0wMDAwLWZkMTQ4MjZiYWQyMScgeG1sbnM6eG1wPSdodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvJz48eG1wOk1vZGlmeURhdGU+MjAwOS0wNy0yN1QxMDoxNjo0NSswMjowMDwveG1wOk1vZGlmeURhdGU+Cjx4bXA6Q3JlYXRlRGF0ZT4yMDA5LTA3LTI3VDEwOjE2OjQ1KzAyOjAwPC94bXA6Q3JlYXRlRGF0ZT4KPHhtcDpDcmVhdG9yVG9vbD5cMzc2XDM3N1wwMDBQXDAwMERcMDAwRlwwMDBDXDAwMHJcMDAwZVwwMDBhXDAwMHRcMDAwb1wwMDByXDAwMCBcMDAwVlwwMDBlXDAwMHJcMDAwc1wwMDBpXDAwMG9cMDAwblwwMDAgXDAwMDBcMDAwLlwwMDA5XDAwMC5cMDAwODwveG1wOkNyZWF0b3JUb29sPjwvcmRmOkRlc2NyaXB0aW9uPgo8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0nNjYwZDVlMmEtN2NlMS0xMWRlLTAwMDAtZmQxNDgyNmJhZDIxJyB4bWxuczp4YXBNTT0naHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLycgeGFwTU06RG9jdW1lbnRJRD0nNjYwZDVlMmEtN2NlMS0xMWRlLTAwMDAtZmQxNDgyNmJhZDIxJy8+CjxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSc2NjBkNWUyYS03Y2UxLTExZGUtMDAwMC1mZDE0ODI2YmFkMjEnIHhtbG5zOmRjPSdodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLycgZGM6Zm9ybWF0PSdhcHBsaWNhdGlvbi9wZGYnPjxkYzp0aXRsZT48cmRmOkFsdD48cmRmOmxpIHhtbDpsYW5nPSd4LWRlZmF1bHQnPlwzNzZcMzc3XDAwMEhcMDAwb1wwMDBtXDAwMGFcMDAwIFwwMDBNXDAwMHVcMDAwblwwMDBkXDAwMG88L3JkZjpsaT48L3JkZjpBbHQ+PC9kYzp0aXRsZT48ZGM6Y3JlYXRvcj48cmRmOlNlcT48cmRmOmxpPlwzNzZcMzc3XDAwMEFcMDAwZFwwMDBtXDAwMGlcMDAwblwwMDBpXDAwMHNcMDAwdFwwMDByXDAwMGFcMDAwdFwwMDBvXDAwMHI8L3JkZjpsaT48L3JkZjpTZXE+PC9kYzpjcmVhdG9yPjxkYzpkZXNjcmlwdGlvbj48cmRmOlNlcT48cmRmOmxpPigpPC9yZGY6bGk+PC9yZGY6U2VxPjwvZGM6ZGVzY3JpcHRpb24+PC9yZGY6RGVzY3JpcHRpb24+CjwvcmRmOlJERj4KPC94OnhtcG1ldGE+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKPD94cGFja2V0IGVuZD0ndyc/PgplbmRzdHJlYW0KZW5kb2JqCjIgMCBvYmoKPDwvUHJvZHVjZXIoR1BMIEdob3N0c2NyaXB0IDguNjQpCi9DcmVhdGlvbkRhdGUoRDoyMDA5MDcyNzEwMTY0NSswMicwMCcpCi9Nb2REYXRlKEQ6MjAwOTA3MjcxMDE2NDUrMDInMDAnKQovVGl0bGUoXDM3NlwzNzdcMDAwSFwwMDBvXDAwMG1cMDAwYVwwMDAgXDAwME1cMDAwdVwwMDBuXDAwMGRcMDAwbykKL0NyZWF0b3IoXDM3NlwzNzdcMDAwUFwwMDBEXDAwMEZcMDAwQ1wwMDByXDAwMGVcMDAwYVwwMDB0XDAwMG9cMDAwclwwMDAgXDAwMFZcMDAwZVwwMDByXDAwMHNcMDAwaVwwMDBvXDAwMG5cMDAwIFwwMDAwXDAwMC5cMDAwOVwwMDAuXDAwMDgpCi9BdXRob3IoXDM3NlwzNzdcMDAwQVwwMDBkXDAwMG1cMDAwaVwwMDBuXDAwMGlcMDAwc1wwMDB0XDAwMHJcMDAwYVwwMDB0XDAwMG9cMDAwcikKL0tleXdvcmRzKCkKL1N1YmplY3QoKT4+ZW5kb2JqCnhyZWYKMCAxMwowMDAwMDAwMDAwIDY1NTM1IGYgCjAwMDAwMDA0NTkgMDAwMDAgbiAKMDAwMDAwNTYzMiAwMDAwMCBuIAowMDAwMDAwNDAwIDAwMDAwIG4gCjAwMDAwMDAyNTkgMDAwMDAgbiAKMDAwMDAwMDAxNSAwMDAwMCBuIAowMDAwMDAwMjQwIDAwMDAwIG4gCjAwMDAwMDA1NTMgMDAwMDAgbiAKMDAwMDAwMDg5NiAwMDAwMCBuIAowMDAwMDAwNTI0IDAwMDAwIG4gCjAwMDAwMDExMTQgMDAwMDAgbiAKMDAwMDAwMDc3MCAwMDAwMCBuIAowMDAwMDAzODA4IDAwMDAwIG4gCnRyYWlsZXIKPDwgL1NpemUgMTMgL1Jvb3QgMSAwIFIgL0luZm8gMiAwIFIKL0lEIFs8Nzk1MkQ0QjRFN0JGQTI0Mjc1OEM3RDdDM0U0MEE5Mjk+PDc5NTJENEI0RTdCRkEyNDI3NThDN0Q3QzNFNDBBOTI5Pl0KPj4Kc3RhcnR4cmVmCjYwNjcKJSVFT0YK</du:Documento></du:DocUniversitario>" //$NON-NLS-1$
    			.getBytes(StandardCharsets.UTF_8);

    	final String[] formats = new String[] {
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
    			AOSignConstants.SIGN_FORMAT_XADES_DETACHED
    	};

    	final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("nodeToSign", "POR2iK5000"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("mimeType", "application/pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("encoding", "http://www.w3.org/2000/09/xmldsig#base64"); //$NON-NLS-1$ //$NON-NLS-2$

    	for (final String format : formats) {
    		p.setProperty("format", format); //$NON-NLS-1$
    		final byte[] signature = signer.sign(data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				p
			);
    		final File f = File.createTempFile("xades-NODESIGN-" + format + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(signature);
				fos.flush();
			}
    		System.out.println("Firma " + format + " para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

    /** Pruebas de cofirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCoSign() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

//        es.gob.afirma.platform.ws.TestSignVerifier verifier = null;
//        try {
//          verifier = new es.gob.afirma.platform.ws.TestSignVerifier();
//        }
//        catch (Exception e) {
//          System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
//        }

        for (final String algo : ALGOS) {

          for(final String[] signfile : TEST_FILES_MULTISIGN) {

        	final String filename = signfile[0];

            prueba = "Cofirma XAdES con el algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.cosign(
        		data,
        		algo,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p
    		);

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            // Enviamos a validar a AFirma
//            if (verifier != null) {
//                Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//            }

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }

    /** Pruebas de cofirmacon Manifest.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCoSignManifest() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {
          for(final String[] signfile : TEST_FILES_MULTISIGN) {

        	final String filename = signfile[0];
        	boolean isEnveloped = Boolean.parseBoolean(signfile[1]);

            prueba = "Cofirma XAdES con manifest, algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));
            byte[] result;
            try {
	            result = signer.cosign(
	        		data,
	        		algo,
	        		pke.getPrivateKey(),
	        		pke.getCertificateChain(),
	        		p
	    		);
            }
            catch (final AOUnsupportedSignFormatException e) {
            	if (isEnveloped) {
            		System.out.println("Las firmas enveloped no se pueden firmar con manifest. Se pasa a la siguiente"); //$NON-NLS-1$
            		isEnveloped = false;
            		continue;
            	}
				throw e;
			}

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }

    /** Pruebas de contrafirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCounterSign() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {
          for(final String[] signfile : TEST_FILES_MULTISIGN) {

          	final String filename = signfile[0];

            prueba = "Contrafirma XAdES con el algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.countersign(
        		data,
        		algo,
        		CounterSignTarget.LEAFS,
        		null,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p
    		);

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }

    /** Pruebas de contrafirma con Manifest.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCounterSignManifest() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS,
    		new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {
          for(final String[] signfile : TEST_FILES_MULTISIGN) {

            final String filename = signfile[0];

            prueba = "Contrafirma XAdES con el algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.countersign(
        		data,
        		algo,
        		CounterSignTarget.LEAFS,
        		null,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p
    		);

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }


    /** Prueba con hoja de estilo externa.
     * <b>Necesita GUI</b>
     * @throws Exception Cuando ocurre un error. */
    @SuppressWarnings("static-method")
	@Test
    //@Ignore // Necesita GUI
    public void testSignExternalStyle() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOXAdESSigner();

        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "ESTEESUNHASH=");  //$NON-NLS-1$//$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        p1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {

            prueba = "Firma XAdES con hoja de estilo externa con el algoritmo ': " + //$NON-NLS-1$
            algo +
            "'"; //$NON-NLS-1$

            System.out.println();
            System.out.println(prueba);

            final String filename = "external-style.xml"; //$NON-NLS-1$

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.sign(
        		data,
        		algo,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p1
    		);

            final File f = File.createTempFile(algo + "-" + p1.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
            try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

//                    // Enviamos a validar a AFirma
//                    if (verifier != null) {
//                        Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//                    }

            Assert.assertTrue(isValidUnsignedProperties(new ByteArrayInputStream(result),null));

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue(signer.isSign(result));

            AOTreeModel tree = signer.getSignersStructure(result, false);
            Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
            //Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

            tree = signer.getSignersStructure(result, true);
            Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
            final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

            Assert.assertNotNull(simpleSignInfo.getSigningTime());
            Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);

        }

    }


    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

    	System.out.println("Prueba con JAVA: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

//        TestSignVerifier verifier = null;
//        try {
//            verifier = new TestSignVerifier();
//        }
//        catch (Exception e) {
//            System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
//        }

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    // Omitimos la firma de binarios en modo enveloped
                    if ("XAdES Enveloped".equals(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        continue;
                    }

                    prueba = "Firma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

                    System.out.println();
                    System.out.println(prueba);

                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

                    final byte[] result = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertFalse("El XML contiene '&#13;'", new String(result).contains("&#13;")); //$NON-NLS-1$ //$NON-NLS-2$

                    File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    try (
        				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        			) {
        				fos.write(result);
        				fos.flush();
        			}
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

//                    // Enviamos a validar a AFirma
//                    if (verifier != null) {
//                        Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//                    }

                    Assert.assertTrue(isValidUnsignedProperties(new ByteArrayInputStream(result),null));

                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));

                    if ("implicit".equals(extraParams.getProperty("mode")) && !filename.toLowerCase().endsWith(".xml") && !Arrays.equals(signer.getData(result), data)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", "-" + filename); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                        try (
            				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            			) {
            				fos.write(result);
            				fos.flush();
            			}
                        System.out.println("Temporal de los datos extraidos para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
                        Assert.fail("Los datos extraidos no coinciden con los originales: " + filename); //$NON-NLS-1$
                    }

                    AOTreeModel tree = signer.getSignersStructure(result, false);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
//                    Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

                    Assert.assertNotNull(simpleSignInfo.getSigningTime());
                    Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);

                }
            }
        }

    }

    /**
     * Prueba de firma externally detached.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
	public void testSignExternallyDetached() throws Exception {

    	System.out.println("Prueba con JAVA: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
    	final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
    	final PrivateKey pk = pke.getPrivateKey();
    	final Certificate[] certChain = pke.getCertificateChain();

    	final AOSigner signer = new AOXAdESSigner();


    	final String digestAlgorithm = "SHA-256"; //$NON-NLS-1$

    	byte[] data = null;
    	final URL url = new URL("http://estaticos.redsara.es/comunes/autofirma/autofirma.version"); //$NON-NLS-1$
    	final URLConnection conn = url.openConnection();
    	try (InputStream is = conn.getInputStream()) {
    		data = AOUtil.getDataFromInputStream(is);
    	}

    	final Properties extraParams = new Properties();
    	extraParams.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED); //$NON-NLS-1$
    	extraParams.setProperty("uri", url.toString()); //$NON-NLS-1$
    	extraParams.setProperty("precalculatedHashAlgorithm", digestAlgorithm); //$NON-NLS-1$

    	final byte[] md = MessageDigest.getInstance(digestAlgorithm).digest(data);

    	final byte[] signature = signer.sign(md, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pk, certChain, extraParams);

    	final File f = File.createTempFile("externallyDetached", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
    	try (final java.io.FileOutputStream fos = new java.io.FileOutputStream(f)) {
    		fos.write(signature);
    		fos.flush();
    	}
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

    /** Comprueba que el nodo UnsignedSignatureProperties (en caso de aparecer)
     * de la firma XAdES contiene atributos. Busca el nodo con el namespace
     * indicado.
     * @param sign Firma.
     * @param namespacePrefix Espacio de nombres a utilizar.
     * @return {@code false} si se encuentra el nodo UnsignedSignatureProperties
     * vac&iacute;o, {@code true} en caso contrario. */
    private static boolean isValidUnsignedProperties(final InputStream sign, final String namespacePrefix) {

        final Document document;
        try {
            document = Utils.getNewDocumentBuilder().parse(sign);
        }
        catch (final Exception e) {
            System.out.println("No es una firma valida: " + e); //$NON-NLS-1$
            return false;
        }

        String xadesPrefix = namespacePrefix;
        if (xadesPrefix == null) {
        	final Element signedPropertiesElement = XAdESUtil.getSignedPropertiesElement(XAdESUtil.getFirstSignatureElement(document.getDocumentElement()));
        	xadesPrefix = signedPropertiesElement.getPrefix();
        }

        final NodeList upNodes = document.getElementsByTagName(xadesPrefix + ":UnsignedProperties"); //$NON-NLS-1$
        for (int i = 0; i < upNodes.getLength(); i++) {
            final NodeList uspNodes = upNodes.item(i).getChildNodes();
            for (int j = 0; j < uspNodes.getLength(); j++) {
                if (uspNodes.item(i).getNodeName().equals(xadesPrefix + ":UnsignedSignatureProperties")) { //$NON-NLS-1$
                    if (uspNodes.item(i).getChildNodes().getLength() == 0) {
                        return false;
                    }
                    break;
                }
            }
        }

        return true;
    }

    /** Prueba de detecci&oacute;n de formato XAdES.
     * @throws Exception Cuando se produce un error durante la prueba.
     */
    @SuppressWarnings("static-method")
	@Test
    public void testDetection() throws Exception {
    	final String[][] files = new String[][] {
    			{ "firmaIgae.xsig.xml", "true"}, //$NON-NLS-1$ //$NON-NLS-2$
    	    	{"XAdES-XL_con_sello_binario.xml", "true" }, //$NON-NLS-1$ //$NON-NLS-2$
    			{"XAdES-XL_con_sello_xml.xml", "true" } //$NON-NLS-1$ //$NON-NLS-2$
    	};
    	final AOSigner signer = new AOXAdESSigner();
    	for (final String[] f : files) {
    		if (Boolean.parseBoolean(f[1])) {
    			Assert.assertTrue("La firma " + f + " no se reconoce como XAdES", signer.isSign(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(f[0])))); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    		else {
    			Assert.assertFalse("La firma " + f + " se ha reconocido como XAdES", signer.isSign(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(f[0])))); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    	}
    }

    /**
     * Prueba de firma con certificado de curva el&iacute;ptica.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSignatureWithECDSA() throws Exception {

    	System.out.println("Prueba de firma de curva eliptica con JAVA: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_EC_PATH), CERT_EC_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_EC_ALIAS, new KeyStore.PasswordProtection(CERT_EC_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
        	for (final String algo : ALGOS_EC) {
        		for (final String filename : TEST_FILES_DATA) {

        			// Omitimos la firma de binarios en modo enveloped
        			if ("XAdES Enveloped".equals(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        				continue;
        			}

        			prueba = "Firma XAdES en modo '" +  //$NON-NLS-1$
        					extraParams.getProperty("mode") +  //$NON-NLS-1$
        					", formato '" + //$NON-NLS-1$
        					extraParams.getProperty("format") +  //$NON-NLS-1$
        					"' con el algoritmo ': " + //$NON-NLS-1$
        					algo +
        					"' y el fichero '" + //$NON-NLS-1$
        					filename + "'"; //$NON-NLS-1$

        			System.out.println();
        			System.out.println(prueba);


        			final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

        			final byte[] result = signer.sign(
        					data,
        					algo,
        					pke.getPrivateKey(),
        					pke.getCertificateChain(),
        					extraParams
        					);

        			final File f = File.createTempFile("Firma_" + algo, ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        			try (final java.io.FileOutputStream fos = new java.io.FileOutputStream(f)) {
        				fos.write(result);
        				fos.flush();
        			}
        			System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
        		}
        	}
        }
    }

}
