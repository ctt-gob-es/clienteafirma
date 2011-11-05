/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfPKCS7;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignature;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;

/** Clase para la firma electr&oacute;nica en tres fases de ficheros Adobe PDF en formato PAdES.
 * No firma (aun) PDF cifrados
 * <p>Necesita iText 2.1.7 con modificaciones espec&iacute;ficas.</p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * */
public class PAdESTriPhaseSigner {
    
    /** Versi&oacute;n de iText necesaria para el uso de esta clase (2.1.7). */
    public static final String ITEXT_VERSION = "2.1.7"; //$NON-NLS-1$
    
    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.46 o superior). */
    public static final String BC_VERSION = "1.46"; //$NON-NLS-1$
    
    /** Construye un firmador PAdES trif&aacute;sico, comprobando que la versiones existentes de iText y Bouncycastle sean las adecuadas. 
     * @throws UnsupportedOperationException si se encuentra bibliotecas iText o BouncyCastle en versiones incompatibles
     */
    public PAdESTriPhaseSigner() {
        final String itextVersion = Platform.getITextVersion();
        if (!ITEXT_VERSION.equals(itextVersion)) {
            throw new UnsupportedOperationException("Se necesita iText version " + ITEXT_VERSION + ", pero se ha encontrado la version: " + itextVersion); //$NON-NLS-1$ //$NON-NLS-2$
        }
        final String bcVersion = Platform.getBouncyCastleVersion();
        if (BC_VERSION.compareTo(bcVersion) > 0) {
            throw new UnsupportedOperationException("Se necesita BouncyCastle version igual o superior a " + BC_VERSION + ", pero se ha encontrado la version: " + bcVersion); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$
    
    private static final int CSIZE = 8000;
    
    //TODO: La fecha de firma debe ser establecida externamente, y no ser estatica
    private static final Calendar CAL = new GregorianCalendar();
    
    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    public static final int LAST_PAGE = -666;
    
    /** Obtiene la prefirma PAdES/CAdES de un PDF (atributos CAdES a firmar)
     * @param digestAlgorithmName Nombre del algoritmo de huella digital usado para la firma. Debe usarse exactamente el mismo valor en la post-firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>digestAlgorithmName</code>:</p>
     * <ul>
     *  <li><i>SHA1</i></li>
     *  <li><i>MD5</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA-256</i></li>
     *  <li><i>SHA-384</i></li>
     *  <li><i>SHA-512</i></li>
     * </ul>
     * @param inPDF PDF a firmar. Debe usarse exactamente el mismo documento en la post-firma.
     * @param signerCertificateChain Cadena de certificados del firmante Debe usarse exactamente la misma cadena de certificados en la post-firma.
     * @param rubricJpegImage R&uacute;brica (JPEG binario) a insertar en el PDF. Debe usarse exactamente la misma imagen en la post-firma.
     * @param xParams Par&aacute;metros adicionales para la firma. Deben usarse exactamente los mismos valores en la post-firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signReason</i></b></dt>
     *   <dd>Raz&oacute;n por la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signField</i></b></dt>
     *   <dd>
     *    Nombre del campo en donde insertar la firma.
     *    Si el documento PDF tiene ya un campo de firma precreado es posible utilizarlo para insertar la firma generada, referenci&aacute;ndolo 
     *    por su nombre.<br>
     *    Si se indica un nombre de campo de firma que no exista en el documento PDF proporcionado, se generar&aacute; una excepci&oacute;n.
     *   </dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signerContact</i></b></dt>
     *   <dd>
     *    Contacto del firmante, usualmente una direcci&oacute;n de coreo electr&oacute;nico 
     *    (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).
     *   </dd>
     *  <dt><b><i>signaturePage</i></b></dt>
     *   <dd>
     *    P&aacute;gina del documento PDF donde insertar la firma. Puede usarse la constante <code>LAST_PAGE</code>
     *    para referirse a la &uacute;ltima p&aacute;gina del documento PDF si se desconoce el n&uacute;mero total de
     *    p&aacute;ginas de este.
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique 
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible 
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.     
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>allowSigningCertifiedPdfs</i></b></dt>
     *   <dd>
     *    Si se establece a <code>true</code> permite la firma o cofirma de PDF certificados, si no se establece o se establece a <code>false</code> se
     *    lanza una excepci&oacute;n en caso de intentar firmar o cofirmar un PDF certificado.<br>
     *    <b>Solo tiene efecto cuando <code>headLess</code> est&aacute;
     *    establecido a <code>true</code>, si <code>headLess</code> est&aacute; a <code>false</code> se ignora este par&aacute;metro.</b><br>
     *    No se soporta el cifrado de documentos PDF con certificados o con algoritmo AES256.
     *   </dd>
     *  <dt><b><i>tsaURL</i></b></dt>
     *   <dd>URL de la autoridad de sello de tiempo (si no se indica no se a&ntilde;ade sello de tiempo).</dd>
     *  <dt><b><i>tsaPolicy</i></b></dt>
     *   <dd>Pol&iacute;tica de sellado de tiempo (obligatoria si se indica <code>tsaURL</code>).</dd>
     *  <dt><b><i>tsaHashAlgorithm</i></b></dt>
     *   <dd>Algoritmo de huella digital a usar para el sello de tiempo (si no se establece se usa SHA-1).</dd>
     *  <dt><b><i>tsaRequireCert</i></b></dt>
     *   <dd><code>true</code> si se requiere el certificado de la TSA, false en caso contrario (si no se establece se asume <code>true</code>).</dd>
     *  <dt><b><i>tsaUsr</i></b></dt>
     *   <dd>Nombre de usuario de la TSA.</dd>
     *  <dt><b><i>tsaPwd</i></b></dt>
     *   <dd>Contrase&ntilde;a del usuario de la TSA. Se ignora si no de ha establecido adem&aacute;s <code>tsaUsr</code>.</dd>
     * </dl>
     * @return Prefirma CAdES/PAdES (atributos CAdES a firmar)
     * @throws IOException
     * @throws AOException
     */
    public PdfPreSignResult preSign(final String digestAlgorithmName,
                   final byte[] inPDF, 
                   final X509Certificate[] signerCertificateChain,
                   final byte[] rubricJpegImage,
                   final Properties xParams) throws IOException, AOException {
        
        final Properties extraParams = (xParams != null) ? xParams : new Properties();
        
        final PdfTriPhaseSession ptps = getSessionData(inPDF, signerCertificateChain, rubricJpegImage, extraParams);
        
        final byte[] original = AOUtil.getDataFromInputStream(ptps.getSAP().getRangeStream());
        
        // Calculamos el MessageDigest
        final MessageDigest md;
        try {
            md = MessageDigest.getInstance(digestAlgorithmName);
        }
        catch (NoSuchAlgorithmException e) {
            throw new AOException("El algoritmo de huella digital no es valido", e); //$NON-NLS-1$
        } 
        md.update(original);

        // Prefirma CAdES
        return new PdfPreSignResult(
            ptps.getFileID(),
            CAdESTriPhaseSigner.preSign(
                digestAlgorithmName, 
                null, 
                signerCertificateChain, 
                new AdESPolicy(extraParams),
                true, 
                md.digest(),
                CAL.getTime()
            )
        );
    }

    /**
     * Post-firma en PAdES.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital usado para la firma (debe ser el mismo que el usado en la prefirma)
     * @param inPDF PDF a firmar (debe ser el mismo que el usado en la prefirma)
     * @param signerCertificateChain Cadena de certificados del firmante (debe ser la misma que la usado en la prefirma)
     * @param rubricJpegImage R&uacute;brica (JPEG binario) a insertar en el PDF (debe ser la misma que la usada en la prefirma)
     * @param extraParams Opciones adicionales para la firma (deben ser las mismas que las usadas en la prefirma)
     * @param signature Resultado de la firma PKCS#1 v1.5
     * @param signedAttributes Resultado de la prefirma CAdES/PAdES (atributos CAdES a firmar)
     * @param fileID FileID del PDF generado en la prefirma
     * @return PDF firmado
     * @throws AOException en caso de cualquier tipo de error
     * @throws IOException 
     */
    public byte[] postSign(final String digestAlgorithmName,
                    final byte[] inPDF,
                    final X509Certificate[] signerCertificateChain,
                    final byte[] rubricJpegImage,
                    final Properties extraParams,
                    final byte[] signature,
                    final byte[] signedAttributes,
                    final String fileID) throws AOException, IOException {
        
        final byte[] completeCAdESSignature = CAdESTriPhaseSigner.postSign(digestAlgorithmName, null, signerCertificateChain, signature, signedAttributes);
        final byte[] outc = new byte[CSIZE];

        final PdfDictionary dic2 = new PdfDictionary();
        System.arraycopy(completeCAdESSignature, 0, outc, 0, completeCAdESSignature.length);
        dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));
        
        final PdfTriPhaseSession pts = getSessionData(inPDF, signerCertificateChain, rubricJpegImage, extraParams);
        final PdfSignatureAppearance sap = pts.getSAP();
        final ByteArrayOutputStream baos = pts.getBAOS();
        final String badFileID = pts.getFileID();
        
        try {
           sap.close(dic2);
        }
        catch (final Exception e) {
            throw new AOException("Error al cerrar el PDF para finalizar el proceso de firma", e); //$NON-NLS-1$
        }
        
        return new String(baos.toByteArray(), "ISO-8859-1").replace(badFileID, fileID).getBytes("ISO-8859-1"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    
    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse
     * la firma. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a
     * arriba. */
    private static Rectangle getSignaturePositionOnPage(final Properties extraParams) {
        try {
            return new Rectangle(Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftX")), //$NON-NLS-1$
                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftY")), //$NON-NLS-1$
                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightX")), //$NON-NLS-1$
                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightY")) //$NON-NLS-1$
            );
        }
        catch (final Exception e) {
            return null;
        }
    }
    
    
    /** <i>JavaBean</i> que encapsula los resultados de la prefirma PDF. */
    public static class PdfPreSignResult {
        private final String fileID;
        private final byte[] preSign;
        
        PdfPreSignResult(final String fid, final byte[] pre) {
            if (fid == null || pre == null || "".equals(fid) || pre.length < 1) { //$NON-NLS-1$
                throw new IllegalArgumentException("Es obligatorio proporcionar un MAC y una prefirma"); //$NON-NLS-1$
            }
            this.fileID = fid;
            this.preSign = pre.clone();
        }

        /** Obtiene el FileID (<i>/ID</i>) del diccionario PDF generado.
         * @return FileID del diccionario PDF generado
         */
        public String getFileID() {
            return this.fileID;
        }

        /** Obtiene los atributos CAdES a firmar.
         * @return Atributos CAdES a firmar (prefirma) */
        public byte[] getPreSign() {
            return this.preSign;
        }
        
    }
    private static class PdfTriPhaseSession {
        
        private final PdfSignatureAppearance sap;
        private final ByteArrayOutputStream baos;
        private final String fileID;
        
        PdfTriPhaseSession(final PdfSignatureAppearance s, final ByteArrayOutputStream b, final String fid) {
            this.sap = s;
            this.baos = b;
            this.fileID = fid;
        }
        
        ByteArrayOutputStream getBAOS() {
            return this.baos;
        }
        
        PdfSignatureAppearance getSAP() {
            return this.sap;
        }
        
        String getFileID() {
            return this.fileID;
        }
    }
    
    private PdfTriPhaseSession getSessionData(final byte[] inPDF, 
                                              final X509Certificate[] signerCertificateChain,
                                              final byte[] rubricJpegImage,
                                              final Properties extraParams) throws IOException, AOException {
        
        final String reason = extraParams.getProperty("signReason"); //$NON-NLS-1$
        final String signField = extraParams.getProperty("signField"); //$NON-NLS-1$
        final String signatureProductionCity = extraParams.getProperty("signatureProductionCity"); //$NON-NLS-1$
        final String signerContact = extraParams.getProperty("signerContact"); //$NON-NLS-1$

        int page = 1;
        try {
            page = Integer.parseInt(extraParams.getProperty("signaturePage")); //$NON-NLS-1$
        }
        catch (final Exception e) { 
            /* Se deja la pagina tal y como esta */
        }

        // No hacemos comprobaciones de contrasena en el PDF
        final PdfReader pdfReader = new PdfReader(inPDF);

        // Comprobaciones de certificacion en el PDF
        if (pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED && !"true".equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$ //$NON-NLS-2$
            throw new AOException("No se permite la firma de PDF certificados (el paramtro allowSigningCertifiedPdfs estaba establecido a false)"); //$NON-NLS-1$
        }

        // Los derechos van firmados por Adobe, y como desde iText se invalidan
        // es mejor quitarlos
        pdfReader.removeUsageRights();

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        
        // Activar el atributo de "agregar firma" (cuarto parametro del metodo
        // "PdfStamper.createSignature") hace que se cree una nueva revision del
        // documento y evita que las firmas previas queden invalidadas. Sin embargo, este
        // exige que el PDF no incorpore ningun error, asi que lo mantendremos desactivado
        // para la primera firma y activado para las subsiguientes. Un error incorporado
        // en un PDF erroneo puede quedar subsanado en su version firmada, haciendo
        // posible incorporar nuevas firmas agregando revisiones del documento.
        final PdfStamper stp;
        try {
            stp = PdfStamper.createSignature(
                  pdfReader, // PDF de entrada
                  baos, // Salida
                  '\0', // Mantener version
                  null, // No crear temporal
                  pdfReader.getAcroFields().getSignatureNames().size() > 0 // Si hay mas firmas, creo una revision
            );
        }
        catch(final Exception e) {
            throw new AOException("Error al crear el campo de firma en el PDF", e); //$NON-NLS-1$
        }
        
        // Aplicamos todos los atributos de firma
        final PdfSignatureAppearance sap = stp.getSignatureAppearance();
        stp.setFullCompression();
        sap.setAcro6Layers(true);
        sap.setLayer2Text(""); //$NON-NLS-1$
        sap.setLayer4Text(""); //$NON-NLS-1$
        
        // iText antiguo
        sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        
        if (reason != null) {
            sap.setReason(reason);
        }
        
        sap.setSignDate(CAL);

        // Pagina en donde se imprime la firma
        if (page == LAST_PAGE) {
            page = pdfReader.getNumberOfPages();
        }

        // Posicion de la firma
        final Rectangle signaturePositionOnPage = getSignaturePositionOnPage(extraParams);
        if (signaturePositionOnPage != null && signField == null) {
            sap.setVisibleSignature(signaturePositionOnPage, page, null);
        }
        else if (signField != null) {
            sap.setVisibleSignature(signField);
        }

        // Localizacion en donde se produce la firma
        if (signatureProductionCity != null) {
            sap.setLocation(signatureProductionCity);
        }

        // Contacto del firmante
        if (signerContact != null) {
            sap.setContact(signerContact);
        }

        // Rubrica de la firma
        if (rubricJpegImage != null) {
            try {
                sap.setImage(new Jpeg(rubricJpegImage));
            }
            catch (final Exception e) {
                LOGGER.severe(
                  "No se pudo establecer la imagen de firma para el documento PDF, no se usara imagen: " + e //$NON-NLS-1$
                );
            }
        }
        
        sap.setCrypto(null, signerCertificateChain, null, null);

        final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, PdfName.ADBE_PKCS7_DETACHED);
        if (sap.getSignDate() != null) {
            dic.setDate(new PdfDate(CAL));
        }
        dic.setName(PdfPKCS7.getSubjectFields(signerCertificateChain[0]).getField("CN")); //$NON-NLS-1$
        if (sap.getReason() != null) {
            dic.setReason(sap.getReason());
        }
        if (sap.getLocation() != null) {
            dic.setLocation(sap.getLocation());
        }
        if (sap.getContact() != null) {
            dic.setContact(sap.getContact());
        }

        sap.setCryptoDictionary(dic);

        // Reservamos el espacio necesario en el PDF para insertar la firma
        final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
        exc.put(PdfName.CONTENTS, Integer.valueOf(CSIZE * 2 + 2));
        
        try {
            sap.preClose(exc);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido cerrar el conjunto de atributos de la firma PDF", e); //$NON-NLS-1$
        }

        PdfObject pdfObject;
        try {
            Class<?> pdfStamperImpClass = AOUtil.classForName("com.lowagie.text.pdf.PdfStamperImp"); //$NON-NLS-1$
            Method getFileIdMethod = pdfStamperImpClass.getMethod("getFileID", (Class[]) null); //$NON-NLS-1$
            pdfObject = (PdfObject) getFileIdMethod.invoke(stp.getWriter(), (Object[]) null);
        } catch (Exception e) {
            throw new AOException("Necesita un iText modificado para la realizacion de firmas trifasicas", e); //$NON-NLS-1$
        }
        
        return new PdfTriPhaseSession(sap, baos, new String(pdfObject.getBytes()));
    }
    

}
