package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Manejador de firmas XML XAdES Factura-E.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class AOFacturaESigner implements AOSigner {

    private static final AOSigner XADES_SIGNER = new AOXAdESSigner();

    private static final Set<String> ALLOWED_PARAMS = new HashSet<String>(5);
    static {
        ALLOWED_PARAMS.add("signerCertifiedRole"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionCity"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionProvince"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionPostalCode"); //$NON-NLS-1$
        ALLOWED_PARAMS.add("signatureProductionCountry"); //$NON-NLS-1$
    }

    private static final Properties EXTRA_PARAMS = new Properties();
    static {
        EXTRA_PARAMS.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("policyIdentifier", "http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("policyIdentifierHash", "Ohixl6upD6av8N7pEvDABhEL6hM=");  //$NON-NLS-1$//$NON-NLS-2$
        EXTRA_PARAMS.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        EXTRA_PARAMS.setProperty("policyDescription", "facturae31"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("policyQualifier", "http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1"); //$NON-NLS-1$ //$NON-NLS-2$
        EXTRA_PARAMS.setProperty("signerClaimedRole", "emisor"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Cofirma Facturas en formato XAdES Factura-E.
     * @param data Factura que deseamos firmar.
     * @param sign Factura con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar.
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signerCertifiedRole</i></b></dt>
     *   <dd>Cargo confirmado para el firmante</dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionProvince</i></b></dt>
     *   <dd>Provincia en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionPostalCode</i></b></dt>
     *   <dd>C&oacute;digo postal en el que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionCountry</i></b></dt>
     *   <dd>Pa&iacute;s en el que se realiza la firma</dd>
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKeyEntry keyEntry,
                         final Properties extraParams) throws AOException {
        if (!isValidDataFile(data)) {
            throw new IllegalArgumentException("Los datos proporcionados no son una factura electronica"); //$NON-NLS-1$
        }
        final Properties xParams = (Properties) EXTRA_PARAMS.clone();
        if (extraParams != null) {
            for (final Object key : extraParams.keySet()) {
                if (ALLOWED_PARAMS.contains(key)) {
                    xParams.put(key, extraParams.get(key));
                }
            }
        }
        return XADES_SIGNER.cosign(data, sign, algorithm, keyEntry, xParams);
    }

    /** Cofirma Facturas en formato XAdES Factura-E.
     * @param sign Factura con las firmas iniciales.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar.
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signerCertifiedRole</i></b></dt>
     *   <dd>Cargo confirmado para el firmante</dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionProvince</i></b></dt>
     *   <dd>Provincia en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionPostalCode</i></b></dt>
     *   <dd>C&oacute;digo postal en el que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionCountry</i></b></dt>
     *   <dd>Pa&iacute;s en el que se realiza la firma</dd>
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKeyEntry keyEntry,
                         final Properties extraParams) throws AOException {
        if (!isValidDataFile(sign)) {
            throw new IllegalArgumentException("Los datos proporcionados no son una factura electronica"); //$NON-NLS-1$
        }
        final Properties xParams = (Properties) EXTRA_PARAMS.clone();
        if (extraParams != null) {
            for (final Object key : extraParams.keySet()) {
                if (ALLOWED_PARAMS.contains(key)) {
                    xParams.put(key, extraParams.get(key));
                }
            }
        }
        return XADES_SIGNER.cosign(sign, algorithm, keyEntry, xParams);
    }

    /** Operaci&oacute;n no soportada, se lanza una <code>UnsupportedOperationException</code>. */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No se soporta la contrafirma de facturas"); //$NON-NLS-1$
    }

    /** Firma Facturas en formato XAdES Factura-E.
     * @param data Factura electr&oacute;nica.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar.
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signerCertifiedRole</i></b></dt>
     *   <dd>Cargo confirmado para el firmante</dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionProvince</i></b></dt>
     *   <dd>Provincia en la que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionPostalCode</i></b></dt>
     *   <dd>C&oacute;digo postal en el que se realiza la firma</dd>
     *  <dt><b><i>signatureProductionCountry</i></b></dt>
     *   <dd>Pa&iacute;s en el que se realiza la firma</dd>
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] sign(final byte[] data,
                       final String algorithm, final PrivateKeyEntry keyEntry,
                       final Properties extraParams) throws AOException {
        if (!isValidDataFile(data)) {
            throw new IllegalArgumentException("Los datos proporcionados no son una factura electronica"); //$NON-NLS-1$
        }
        final Properties xParams = (Properties) EXTRA_PARAMS.clone();
        if (extraParams != null) {
            for (final Object key : extraParams.keySet()) {
                if (ALLOWED_PARAMS.contains(key)) {
                    xParams.put(key, extraParams.get(key));
                }
            }
        }
        return XADES_SIGNER.sign(data, algorithm, keyEntry, xParams);
    }

    /** {@inheritDoc} */
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        return XADES_SIGNER.getSignersStructure(sign, asSimpleSignInfo);
    }

    /** {@inheritDoc} */
    public boolean isSign(final byte[] is) {
        return XADES_SIGNER.isSign(is) && isValidDataFile(is);
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param is Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public boolean isValidDataFile(final byte[] is) {
        if (is == null || is.length == 0) {
            return false;
        }
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        try {
            final Document doc = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(is));
            final Element rootNode = doc.getDocumentElement();
            final String rootNodePrefix = rootNode.getPrefix();

            if (!(((rootNodePrefix != null) ? (rootNodePrefix + ":") : "") + "Facturae").equals(rootNode.getNodeName())) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                return false;
            }

            final Set<String> childs = new HashSet<String>(3);
            childs.add("FileHeader"); //$NON-NLS-1$
            childs.add("Parties"); //$NON-NLS-1$
            childs.add("Invoices"); //$NON-NLS-1$

            final NodeList nl = rootNode.getChildNodes();
            for (int i=0;i<nl.getLength();i++) {
                final String nodeName = nl.item(i).getNodeName();
                if (childs.contains(nodeName)) {
                    childs.remove(nodeName);
                }
            }
            if (childs.size() > 0) {
                return false;
            }

        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public String getSignedName(final String originalName, final String inText) {
        return XADES_SIGNER.getSignedName(originalName, inText);
    }

    /** {@inheritDoc} */
    public byte[] getData(final byte[] signData) throws AOException {
        return XADES_SIGNER.getData(signData);
    }

    /** {@inheritDoc} */
    public AOSignInfo getSignInfo(final byte[] signData) throws AOException {
        return XADES_SIGNER.getSignInfo(signData);
    }

}
