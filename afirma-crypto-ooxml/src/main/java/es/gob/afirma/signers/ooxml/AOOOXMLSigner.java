/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import es.gob.afirma.be.fedict.eid.applet.service.signer.AbstractOOXMLSignatureServiceContainer;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.OfficeXMLAnalizer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.xmldsig.AOXMLDSigSigner;

/** Firmas OOXML basadas en una versi&oacute;n fuertemente modificada de las
 * clases <code>es.gob.afirma.be.fedict.eid.applet.service</code>. */
public final class AOOOXMLSigner implements AOSigner {
    
    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    static {
        if (Security.getProvider("XMLDSig") == null) { //$NON-NLS-1$
            try {
                Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido agregar el proveedor de firma XMLDSig necesario para firmas XML: " + e); //$NON-NLS-1$
            }
        }
    }

    public byte[] getData(final byte[] sign) throws AOException {

        // Si no es una firma OOXML valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
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
            LOGGER.severe("Error al cargar el fichero OOXML: " + e); //$NON-NLS-1$
            return false;
        }

        // Comprobamos si estan todos los ficheros principales del documento
        return zipFile.getEntry("[Content_Types].xml") != null && (zipFile.getEntry("_rels/.rels") != null || zipFile.getEntry("_rels\\.rels") != null) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               && (zipFile.getEntry("docProps/app.xml") != null || zipFile.getEntry("docProps\\app.xml") != null) //$NON-NLS-1$ //$NON-NLS-2$
               && (zipFile.getEntry("docProps/core.xml") != null || zipFile.getEntry("docProps\\core.xml") != null); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public AOSignInfo getSignInfo(final byte[] sign) throws AOException {
        if (sign == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(sign)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con documento OOXML"); //$NON-NLS-1$
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros
        // datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_OOXML);
    }

    public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = (inText != null ? inText : ""); //$NON-NLS-1$
        if (originalName == null) {
            return inTextInt + ".ooxml"; //$NON-NLS-1$
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.length() <= 4) {
            return originalName + inTextInt + ".ooxml"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".docx")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".docx"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".xlsx")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".xlsx"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".pptx")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".pptx"; //$NON-NLS-1$
        }
        if (originalNameLC.endsWith(".ppsx")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".ppsx"; //$NON-NLS-1$
        }
        return originalName + inTextInt + ".ooxml"; //$NON-NLS-1$
    }

    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        if (sign == null) {
            throw new IllegalArgumentException("Los datos de firma introducidos son nulos"); //$NON-NLS-1$
        }

        if (!isSign(sign)) {
            LOGGER.severe("La firma indicada no es de tipo OOXML"); //$NON-NLS-1$
            return null;
        }

        // Las firmas contenidas en el documento OOXML son de tipo XMLdSig asi
        // que utilizaremos el
        // signer de este tipo para gestionar el arbol de firmas
        final AOSigner xmldsigSigner = new AOXMLDSigSigner();

        // Recuperamos las firmas individuales del documento y creamos el arbol
        final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$
        try {
            for (final byte[] elementSign : OOXMLUtil.getOOXMLSignatures(sign)) {

                // Recuperamos el arbol de firmas de la firma individual. Ya que
                // esta sera una firma simple
                // solo debe contener un nodo de firma. Ignoramos la raiz del
                // arbol, que contiene
                // el ejemplo representativo de los datos firmados y no de la
                // propia firma.
                final AOTreeModel signTree = xmldsigSigner.getSignersStructure(elementSign, asSimpleSignInfo);
                tree.add(((AOTreeNode) signTree.getRoot()).getChildAt(0));
            }
        }
        catch (final Exception e) {
            LOGGER.severe("La estructura de una de las firmas elementales no es valida: " + e); //$NON-NLS-1$
            return null;
        }

        return new AOTreeModel(tree, tree.getChildCount());
    }

    public boolean isSign(final byte[] sign) {
        if (sign == null) {
            LOGGER.warning("Se ha introducido una firma nula para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isOOXMLFile(sign);
    }

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isOOXMLFile(data);
    }

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
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros OOXML"); //$NON-NLS-1$
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


         // Si se solicito una firma explicita, advertimos no son compatibles con
         // OOXML y se ignorara esta configuracion

        if (extraParams != null && extraParams.containsKey("mode") && extraParams.getProperty("mode").equals(AOSignConstants.SIGN_MODE_EXPLICIT)) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.warning("El formato de firma OOXML no soporta el modo de firma explicita, " + "se ignorara esta configuracion"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Comprobamos si es un documento OOXML valido.
        if (!OfficeXMLAnalizer.isOOXMLDocument(ooxmlDocument)) {
            throw new AOFormatFileException("El fichero introducido no es un documento OOXML"); //$NON-NLS-1$
        }

        // Pasamos la cadena de certificacion a un vector
        if (keyEntry == null) {
            throw new AOException("No se ha proporcionado una clave valida"); //$NON-NLS-1$
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
                                                                     AOSignConstants.getDigestAlgorithmName(algorithm),
                                                                     keyEntry.getPrivateKey(),
                                                                     signNum);
        }
        catch (final Exception e) {
            throw new AOException("Error durante la firma OOXML", e); //$NON-NLS-1$
        }
    }
}
