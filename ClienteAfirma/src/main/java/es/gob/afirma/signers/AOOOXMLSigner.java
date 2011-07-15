/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import javax.activation.MimeType;

import org.ietf.jgss.Oid;

import es.gob.afirma.be.fedict.eid.applet.service.signer.AbstractOOXMLSignatureServiceContainer;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOFileUtils;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.OfficeXMLAnalizer;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.beans.AOSignInfo;
import es.gob.afirma.signers.ooxmlhelper.OOXMLUtil;

/** Firmas OOXML basadas en una versi&oacute;n fuertemente modificada de las
 * clases <code>es.gob.afirma.be.fedict.eid.applet.service</code>. */
public final class AOOOXMLSigner implements AOSigner {

    // En Java 5 es necesario disponer del proveedor XMLDSigRI para generar
    // firmas XMLdSig
    static {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                if (Platform.getJavaVersion().equals(Platform.JREVER.J5)) {
                    try {
                        Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma")
                              .warning("No se ha podido agregar el proveedor de firma XMLDSig necesario para firmas XML: " + e);
                    }
                }
                return null;
            }
        });
    }

    public byte[] getData(final byte[] sign) throws AOInvalidFormatException, AOException {

        // Si no es una firma OOXML valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida");
        }

        // TODO: Por ahora, devolveremos el propio OOXML firmado.
        return sign;
    }

    /** Comprueba que unos datos se adecuen a la estructura b&aacute;sica de un
     * documento OOXML.
     * @param data Datos que deseamos analizar
     * @return {@code true} si el documento es un OOXML, {@code false} en caso
     *         contrario */
    private boolean isOOXMLFile(final byte[] data) {

        final ZipFile zipFile;
        try {
            zipFile = AOFileUtils.createTempZipFile(data);
        }
        catch (final ZipException e) {
            // El fichero no era un Zip ni, por tanto, OOXML
            return false;
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error al cargar el fichero OOXML: " + e);
            return false;
        }

        // Comprobamos si estan todos los ficheros principales del documento
        return zipFile.getEntry("[Content_Types].xml") != null && (zipFile.getEntry("_rels/.rels") != null || zipFile.getEntry("_rels\\.rels") != null)
               && (zipFile.getEntry("docProps/app.xml") != null || zipFile.getEntry("docProps\\app.xml") != null)
               && (zipFile.getEntry("docProps/core.xml") != null || zipFile.getEntry("docProps\\core.xml") != null);
    }

    public String getDataMimeType(final byte[] sign) throws AOUnsupportedSignFormatException {

        if (sign == null) {
            throw new NullPointerException("Los datos de firma introducidos son nulos");
        }

        final InputStream contentTypesXml;
        final ZipEntry zipEntry;
        try {
            final ZipFile zipFile = AOFileUtils.createTempZipFile(sign);
            zipEntry = zipFile.getEntry("[Content_Types].xml");
            contentTypesXml = zipFile.getInputStream(zipEntry);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error al analizar el fichero de firma: " + e);
            return null;
        }

        if (zipEntry == null || contentTypesXml == null || isOOXMLFile(sign)) {
            throw new AOUnsupportedSignFormatException("La firma introducida no es un documento OOXML");
        }

        return OfficeXMLAnalizer.getOOXMLMimeType(contentTypesXml);
    }

    public AOSignInfo getSignInfo(final byte[] sign) throws AOInvalidFormatException, AOException {
        if (sign == null) {
            throw new NullPointerException("No se han introducido datos para analizar");
        }

        if (!isSign(sign)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con documento OOXML");
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return new AOSignInfo(AOConstants.SIGN_FORMAT_OOXML);
    }

    public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = (inText != null ? inText : "");
        if (originalName == null) {
            return inTextInt + ".ooxml";
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.length() <= 4) {
            return originalName + inTextInt + ".ooxml";
        }
        if (originalNameLC.endsWith(".docx")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".docx";
        }
        if (originalNameLC.endsWith(".xlsx")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".xlsx";
        }
        if (originalNameLC.endsWith(".pptx")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".pptx";
        }
        if (originalNameLC.endsWith(".ppsx")) {
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".ppsx";
        }
        return originalName + inTextInt + ".ooxml";
    }

    public TreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        if (sign == null) {
            throw new NullPointerException("Los datos de firma introducidos son nulos");
        }

        if (!isSign(sign)) {
            Logger.getLogger("es.gob.afirma").severe("La firma indicada no es de tipo OOXML");
            return null;
        }

        // Las firmas contenidas en el documento OOXML son de tipo XMLdSig asi
        // que utilizaremos el
        // signer de este tipo para gestionar el arbol de firmas
        final AOXMLDSigSigner xmldsigSigner = new AOXMLDSigSigner();

        // Recuperamos las firmas individuales del documento y creamos el arbol
        final TreeNode tree = new TreeNode("Datos");
        try {
            for (final byte[] elementSign : OOXMLUtil.getOOXMLSignatures(sign)) {

                // Recuperamos el arbol de firmas de la firma individual. Ya que
                // esta sera una firma simple
                // solo debe contener un nodo de firma. Ignoramos la raiz del
                // arbol, que contiene
                // el ejemplo representativo de los datos firmados y no de la
                // propia firma.
                final TreeModel signTree = xmldsigSigner.getSignersStructure(elementSign, asSimpleSignInfo);
                tree.add(((TreeNode) signTree.getRoot()).getChildAt(0));
            }
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("La estructura de una de las firmas elementales no es valida: " + e);
            return null;
        }

        return new TreeModel(tree, tree.getChildCount());
    }

    public boolean isSign(final byte[] sign) {
        if (sign == null) {
            Logger.getLogger("es.gob.afirma").warning("Se ha introducido una firma nula para su comprobacion");
            return false;
        }
        return isOOXMLFile(sign);
    }

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
            return false;
        }
        return isOOXMLFile(data);
    }

    public void setDataObjectFormat(final String description, final Oid objectIdentifier, final MimeType mimeType, final String encoding) {}

    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return addNewSign(data, algorithm, keyEntry, extraParams);
    }

    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return addNewSign(sign, algorithm, keyEntry, extraParams);
    }

    public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return addNewSign(sign, algorithm, keyEntry, extraParams);
    }

    /** M&eacute;todo no implementado. No es posible realizar contrafirmas de
     * documentos OOXML. Lanza una <code>UnsupportedOperationException</code>. */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros OOXML");
    }

    /** Agrega una nueva firma a un documento OOXML.
     * @param ooxmlFile Documento OOXML
     * @param algorithm Algoritmo de firma
     * @param keyEntry Clave del certificado
     * @param cert Certificado de firma
     * @param extraParams Configuraci&oacute;n adicional de firma
     * @return Documento firmado
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma */
    private byte[] addNewSign(final byte[] ooxmlDocument, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {

        return signOOXML(ooxmlDocument, OOXMLUtil.countOOXMLSignatures(ooxmlDocument) + 1, algorithm, keyEntry, extraParams);
    }

    /** @param ooxmlDocument Documento OOXML.
     * @param signNum N&uacute;mero de la firma que se va a realizar
     * @param algorithm Algoritmo de firma
     * @param keyEntry Clave del certificado
     * @param cert Certificado de firma
     * @param extraParams Configuraci&oacute;n adicional de firma
     * @return Documento firmado
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma */
    private byte[] signOOXML(final byte[] ooxmlDocument,
                             final int signNum,
                             final String algorithm,
                             final PrivateKeyEntry keyEntry,
                             final Properties extraParams) throws AOException {

        // Cogemos el algoritmo de digest
        String digestAlgo;
        if (algorithm == null) {
            digestAlgo = "SHA1";
        }
        else if (algorithm.startsWith("SHA-1") || algorithm.startsWith("SHA1") || algorithm.startsWith("SHAwith")) {
            digestAlgo = "SHA1";
        }
        else if (algorithm.startsWith("SHA-512") || algorithm.startsWith("SHA512")) {
            digestAlgo = "SHA512";
        }
        else if (algorithm.startsWith("SHA-384") || algorithm.startsWith("SHA384")) {
            digestAlgo = "SHA384";
        }
        else if (algorithm.startsWith("SHA-256") || algorithm.startsWith("SHA256")) {
            digestAlgo = "SHA256";
        }
        else if (algorithm.startsWith("RIPEND-160") || algorithm.startsWith("RIPEND160")) {
            digestAlgo = "RIPEND160";
        }
        else {
            Logger.getLogger("es.gob.afirma").warning("El algoritmo de firma '" + algorithm
                                                      + "' no esta soportado en OOXML, se utilizara SHA1 con RSA");
            digestAlgo = "SHA1";
        }

        /*
         * Si se solicito una firma explicita, advertimos no son compatibles con
         * OOXML y se ignorara esta configuracion
         */
        if (extraParams != null && extraParams.containsKey("mode") && extraParams.getProperty("mode").equals(AOConstants.SIGN_MODE_EXPLICIT)) {
            Logger.getLogger("es.gob.afirma")
                  .warning("El formato de firma OOXML no soporta el modo de firma explicita, " + "se ignorara esta configuracion");
        }

        // Comprobamos si es un documento OOXML valido.
        if (!OfficeXMLAnalizer.isOOXMLDocument(ooxmlDocument)) {
            throw new AOFormatFileException("El fichero introducido no es un documento OOXML");
        }

        // Pasamos la cadena de certificacion a un vector
        if (keyEntry == null) {
            throw new AOException("No se ha proporcionado una clave valida");
        }

        X509Certificate[] xCerts = new X509Certificate[0];
        final Certificate[] certs = keyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            xCerts = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = keyEntry.getCertificate();
            if (cert instanceof X509Certificate) {
                xCerts = new X509Certificate[] {
                    (X509Certificate) cert
                };
            }
        }

        final Vector<X509Certificate> certChain = new Vector<X509Certificate>(xCerts.length);
        for (final X509Certificate c : xCerts) {
            certChain.add(c);
        }

        try {
            return new AbstractOOXMLSignatureServiceContainer().sign(new ByteArrayInputStream(ooxmlDocument),
                                                                     certChain,
                                                                     digestAlgo,
                                                                     keyEntry.getPrivateKey(),
                                                                     signNum);
        }
        catch (final Exception e) {
            throw new AOException("Error durante la firma OOXML", e);
        }
    }
}
