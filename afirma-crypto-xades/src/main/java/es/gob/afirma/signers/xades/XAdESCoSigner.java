package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;

import javax.xml.crypto.dsig.CanonicalizationMethod;
import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.Transform;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.parsers.DocumentBuilderFactory;

import net.java.xades.security.xml.XAdES.SignaturePolicyIdentifier;
import net.java.xades.security.xml.XAdES.SignatureProductionPlace;
import net.java.xades.security.xml.XAdES.SignerRole;
import net.java.xades.security.xml.XAdES.SignerRoleImpl;
import net.java.xades.security.xml.XAdES.XAdES;
import net.java.xades.security.xml.XAdES.XAdES_EPES;
import net.java.xades.util.XMLUtils;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.AOException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;

import static es.gob.afirma.signers.xades.AOXAdESSigner.LOGGER;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_TAG;
import static es.gob.afirma.signers.xades.AOXAdESSigner.STYLE_REFERENCE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.DIGEST_METHOD;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADES_SIGNATURE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XADESNS;
import static es.gob.afirma.signers.xades.AOXAdESSigner.XML_SIGNATURE_PREFIX;
import static es.gob.afirma.signers.xades.AOXAdESSigner.SIGNATURE_NODE_NAME;

final class XAdESCoSigner {
	
    /** Cofirma datos en formato XAdES.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta 
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto 
     *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con 
     *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES, 
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param data Datos que deseamos firmar.
     * @param sign Documento con las firmas iniciales. 
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar.
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>Identificador de la pol&iacute;tica de firma (normalmente una URL hacia la pol&iacute;tica en formato XML procesable)</dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normlamente del mismo fichero en formato XML procesable).
     *    Si no se indica, es obligatorio que el par&aacute;metro <code>policyIdentifier</code> sea una URL accesible universalmente 
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>
     *  <dt><b><i>policyDescription</i></b></dt>
     *   <dd>Descripci&oacute;n textual de la pol&iacute;tica</dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>URL hacia el documento (legible por personas, normalmente en formato PDF) descriptivo de la pol&iacute;tica de firma</dd>
     *  <dt><b><i>signerClaimedRole</i></b></dt>
     *   <dd>Cargo atribuido para el firmante</dd>
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
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>
     *    Algoritmo de huella digital a usar en las referencias XML (referencesDigestMethod). Debe indicarse como una URL, 
     *    acept&aacute;ndose los siguientes valores:
     *    <ul>
     *     <li><i>http://www.w3.org/2000/09/xmldsig#sha1</i> (SHA-1)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#sha256</i> (SHA-256, valor recomendado)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#sha512</i> (SHA-512)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#ripemd160 (RIPEMD-160)</i></li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n</dd>
     *  <dt><b><i>xadesNamespace</i></b></dt>
     *   <dd>URL de definici&oacute;n del espacio de nombres de XAdES (y por extensi&oacute;n, versi&oacute;n de XAdES)</dd>
     *  <dt><b><i>applySystemDate</i></b></dt>
     *   <dd>
     *    Indica si se debe introducir en la firma el atributo <i>signingTime</i> con la fecha actual
     *    del sistema. Por defecto, se encuentra a {@code true}. 
     *   </dd>
     * </dl>
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    static byte[] cosign(final byte[] data, 
                         final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties xParams) throws AOException {

        final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
        if (algoUri == null) {
            throw new UnsupportedOperationException("Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final Properties extraParams = (xParams != null) ? xParams: new Properties();

        final String digestMethodAlgorithm = extraParams.getProperty("referencesDigestMethod", DIGEST_METHOD); //$NON-NLS-1$
        final String canonicalizationAlgorithm = extraParams.getProperty("canonicalizationAlgorithm", CanonicalizationMethod.INCLUSIVE); //$NON-NLS-1$
        final String xadesNamespace = extraParams.getProperty("xadesNamespace", XADESNS); //$NON-NLS-1$

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // Propiedades del documento XML original
        final Map<String, String> originalXMLProperties = new Hashtable<String, String>();

        // carga el documento XML de firmas y su raiz
        Document docSig;
        Element rootSig;
        try {
            docSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign));
            rootSig = docSig.getDocumentElement();

            // Si el documento contiene una firma simple se inserta como raiz el
            // nodo AFIRMA
            if (rootSig.getNodeName().equals(SIGNATURE_NODE_NAME)) {
                docSig = AOXAdESSigner.insertarNodoAfirma(docSig);
                rootSig = docSig.getDocumentElement();
            }
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido leer el documento XML de firmas", e); //$NON-NLS-1$
        }

        final List<Reference> referenceList = new ArrayList<Reference>();
        final XMLSignatureFactory fac = XMLSignatureFactory.getInstance("DOM"); //$NON-NLS-1$
        final DigestMethod digestMethod;
        try {
            digestMethod = fac.newDigestMethod(digestMethodAlgorithm, null);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido obtener un generador de huellas digitales para el algoritmo '" + digestMethodAlgorithm + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Localizamos la primera firma (primer nodo "Signature") en profundidad
        // en el arbol de firma.
        // Se considera que todos los objetos "Signature" del documento firman
        // (referencian) los mismos
        // objetos, por lo que podemos extraerlos de cualquiera de las firmas
        // actuales.
        // Buscamos dentro de ese Signature todas las referencias que apunten a
        // datos para firmarlas
        final List<String> referencesIds = new ArrayList<String>();
        Node currentElement;
        final NodeList nl = ((Element) docSig.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG).item(0)).getElementsByTagNameNS(XMLConstants.DSIGNNS, "Reference"); //$NON-NLS-1$

        // Se considera que la primera referencia de la firma son los datos que
        // debemos firmar, ademas
        // de varias referencias especiales
        for (int i = 0; i < nl.getLength(); i++) {
            currentElement = nl.item(i);

            // Firmamos la primera referencia (que seran los datos firmados) y
            // las hojas de estilo que
            // tenga asignadas. Las hojas de estilo tendran un identificador que
            // comience por STYLE_REFERENCE_PREFIX.
            // TODO: Identificar las hojas de estilo de un modo generico.
            final NamedNodeMap currentNodeAttributes = currentElement.getAttributes();
            if (i == 0 || (currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id")  //$NON-NLS-1$//$NON-NLS-2$
                                                                                                    .getNodeValue()
                                                                                                    .startsWith(STYLE_REFERENCE_PREFIX))) { 

                // Buscamos las transformaciones declaradas en la Referencia,
                // para anadirlas
                // tambien en la nueva
                final List<Transform> currentTransformList;
                try {
                    currentTransformList = Utils.getObjectReferenceTransforms(currentElement, XML_SIGNATURE_PREFIX);
                }
                catch (final NoSuchAlgorithmException e) {
                    throw new AOException("Se ha declarado una transformacion personalizada de un tipo no soportado", e); //$NON-NLS-1$
                }
                catch (final InvalidAlgorithmParameterException e) {
                    throw new AOException("Se han especificado parametros erroneos para una transformacion personalizada", e); //$NON-NLS-1$
                }

                // Creamos un identificador de referencia para el objeto a
                // firmar y la almacenamos
                // para mantener un listado con todas. En el caso de las hojas
                // de estilo lo creamos con un
                // identificador descriptivo
                String referenceId = null;
                if ((currentNodeAttributes.getNamedItem("Id") != null && currentNodeAttributes.getNamedItem("Id")  //$NON-NLS-1$//$NON-NLS-2$
                                                                                              .getNodeValue()
                                                                                              .startsWith(STYLE_REFERENCE_PREFIX))) { 
                    referenceId = STYLE_REFERENCE_PREFIX + UUID.randomUUID().toString(); 
                }
                else {
                    referenceId = "Reference-" + UUID.randomUUID().toString(); //$NON-NLS-1$
                }
                referencesIds.add(referenceId);

                // Creamos la propia referencia con las transformaciones de la
                // original
                referenceList.add(fac.newReference(((Element) currentElement).getAttribute("URI"), digestMethod, currentTransformList, //$NON-NLS-1$
                                                   null,
                                                   referenceId));
            }
        }

        final XAdES_EPES xades =
                (XAdES_EPES) XAdES.newInstance(XAdES.EPES,
                                               xadesNamespace,
                                               XADES_SIGNATURE_PREFIX,
                                               XML_SIGNATURE_PREFIX,
                                               digestMethodAlgorithm,
                                               rootSig);

        // establece el certificado
        final X509Certificate cert = (X509Certificate) keyEntry.getCertificate();
        xades.setSigningCertificate(cert);

        // SignaturePolicyIdentifier
        final SignaturePolicyIdentifier spi =
            AOXAdESSigner.getPolicy(extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                      extraParams.getProperty("policyIdentifierHash"), //$NON-NLS-1$
                      extraParams.getProperty("policyIdentifierHashAlgorithm"), //$NON-NLS-1$
                      extraParams.getProperty("policyDescription"), //$NON-NLS-1$
                      extraParams.getProperty("policyQualifier")); //$NON-NLS-1$
        if (spi != null) {
            xades.setSignaturePolicyIdentifier(spi);
        }

        // SignatureProductionPlace
        final SignatureProductionPlace spp =
        		AOXAdESSigner.getSignatureProductionPlace(extraParams.getProperty("signatureProductionCity"), //$NON-NLS-1$
                                            extraParams.getProperty("signatureProductionProvince"), //$NON-NLS-1$
                                            extraParams.getProperty("signatureProductionPostalCode"), //$NON-NLS-1$
                                            extraParams.getProperty("signatureProductionCountry")); //$NON-NLS-1$
        if (spp != null) {
            xades.setSignatureProductionPlace(spp);
        }

        // SignerRole
        SignerRole signerRole = null;
        try {
            final String claimedRole = extraParams.getProperty("signerClaimedRole"); //$NON-NLS-1$
            final String certifiedRole = extraParams.getProperty("signerCertifiedRole"); //$NON-NLS-1$
            signerRole = new SignerRoleImpl();
            if (claimedRole != null) {
                signerRole.addClaimedRole(claimedRole);
            }
            if (certifiedRole != null) {
                signerRole.addCertifiedRole(certifiedRole);
            }
        }
        catch (final Exception e) {
            // Se ignoran los errores, son parametros opcionales
        }

        if (signerRole != null) {
            xades.setSignerRole(signerRole);
        }

        // SigningTime
        if (Boolean.parseBoolean(extraParams.getProperty("applySystemDate", Boolean.TRUE.toString()))) { //$NON-NLS-1$ 
            xades.setSigningTime(new Date());
        }

        // crea la firma
        final AOXMLAdvancedSignature xmlSignature;
        try {
            xmlSignature = AOXMLAdvancedSignature.newInstance(xades);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido instanciar la firma Avanzada XML JXAdES", e); //$NON-NLS-1$
        }

        try {
            xmlSignature.setDigestMethod(digestMethodAlgorithm);
            xmlSignature.setCanonicalizationMethod(canonicalizationAlgorithm);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido establecer el algoritmo de huella digital (" + algoUri //$NON-NLS-1$
                                                     + "), es posible que el usado en la firma difiera del indicado: " //$NON-NLS-1$
                                                     + e);
        }

        try {
            xmlSignature.sign(Arrays.asList((X509Certificate[])keyEntry.getCertificateChain()), keyEntry.getPrivateKey(), algoUri, referenceList, "Signature-" + UUID.randomUUID().toString(), null/*TSA*/); //$NON-NLS-1$
        }
        catch (final NoSuchAlgorithmException e) {
            throw new UnsupportedOperationException("No se soporta el algoritmo de firma '" + algorithm + "': " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la cofirma", e); //$NON-NLS-1$
        }

        return Utils.writeXML(rootSig, originalXMLProperties, null, null);
    }

    /** Cofirma datos en formato XAdES.
     * <p>
     *  Este m&eacute;todo firma todas las referencias a datos declaradas en la firma original,
     *  ya apunten estas a datos, hojas de estilo o cualquier otro elemento. En cada referencia
     *  firmada se introduciran las mismas transformaciones que existiesen en la firma original.
     * </p>
     * <p>
     *  A nivel de formato interno, cuando cofirmamos un documento ya firmado previamente, esta 
     *  firma previa no se modifica. Si tenemos en cuenta que XAdES es en realidad un subconjunto 
     *  de XMLDSig, el resultado de una cofirma XAdES sobre un documento firmado previamente con 
     *  XMLDSig (o viceversa), son dos firmas independientes, una en XAdES y otra en XMLDSig.<br>
     *  Dado que todas las firmas XAdES son XMLDSig pero no todas las firmas XMLDSig son XAdES, 
     *  el resultado global de la firma se adec&uacute;a al estandar mas amplio, XMLDSig en este caso.
     * </p>
     * @param sign Documento con las firmas iniciales. 
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
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>Identificador de la pol&iacute;tica de firma (normalmente una URL hacia la pol&iacute;tica en formato XML procesable)</dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normlamente del mismo fichero en formato XML procesable).
     *    Si no se indica, es obligatorio que el par&aacute;metro <code>policyIdentifier</code> sea una URL accesible universalmente 
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>
     *  <dt><b><i>policyDescription</i></b></dt>
     *   <dd>Descripci&oacute;n textual de la pol&iacute;tica</dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>URL hacia el documento (legible por personas, normalmente en formato PDF) descriptivo de la pol&iacute;tica de firma</dd>
     *  <dt><b><i>signerClaimedRole</i></b></dt>
     *   <dd>Cargo atribuido para el firmante</dd>
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
     *  <dt><b><i>referencesDigestMethod</i></b></dt>
     *   <dd>
     *    Algoritmo de huella digital a usar en las referencias XML (referencesDigestMethod). Debe indicarse como una URL, 
     *    acept&aacute;ndose los siguientes valores:
     *    <ul>
     *     <li><i>http://www.w3.org/2000/09/xmldsig#sha1</i> (SHA-1)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#sha256</i> (SHA-256, valor recomendado)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#sha512</i> (SHA-512)</li>
     *     <li><i>http://www.w3.org/2001/04/xmlenc#ripemd160 (RIPEMD-160)</i></li>
     *    </ul>
     *   </dd>
     *  <dt><b><i>canonicalizationAlgorithm</i></b></dt>
     *   <dd>Algoritmo de canonicalizaci&oacute;n</dd>
     *  <dt><b><i>xadesNamespace</i></b></dt>
     *   <dd>URL de definici&oacute;n del espacio de nombres de XAdES (y por extensi&oacute;n, versi&oacute;n de XAdES)</dd>
     *   <dt><b><i>applySystemDate</i></b></dt>
     *   <dd>
     *    Indica si se debe introducir en la firma el atributo <i>signingTime</i> con la fecha actual
     *    del sistema. Por defecto, se encuentra a {@code true}. 
     *   </dd>
     * </dl>
     * @return Cofirma en formato XAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    static byte[] cosign(final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties extraParams) throws AOException {

        // nueva instancia de DocumentBuilderFactory que permita espacio de
        // nombres (necesario para XML)
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);

        // carga la raiz del documento XML de firmas
        // y crea un nuevo documento que contendra solo los datos sin firmar
        final Element rootSig;
        final Element rootData;
        try {
            rootSig = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(sign)).getDocumentElement();

            final Document docData = dbf.newDocumentBuilder().newDocument();
            rootData = (Element) docData.adoptNode(rootSig.cloneNode(true));

            // Obtiene las firmas y las elimina. Para evitar eliminar firmas de
            // las que cuelgan otras
            // y despues intentar eliminar estas, las buscamos y eliminamos de
            // una en una
            NodeList signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG);
            while (signatures.getLength() > 0) {
                rootData.removeChild(signatures.item(0));
                signatures = rootData.getElementsByTagNameNS(XMLConstants.DSIGNNS, SIGNATURE_TAG);
            }

            docData.appendChild(rootData);
        }
        catch (final Exception ioex) {
            throw new AOException("Error al leer el documento de firmas", ioex); //$NON-NLS-1$
        }

        // convierte el documento de firmas en un InputStream
        final ByteArrayOutputStream baosSig = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosSig, rootSig, false);

        // convierte el documento a firmar en un InputStream
        final ByteArrayOutputStream baosData = new ByteArrayOutputStream();
        XMLUtils.writeXML(baosData, rootData, false);

        return cosign(baosData.toByteArray(), baosSig.toByteArray(), algorithm, keyEntry, extraParams);
    }
    
}
