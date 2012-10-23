package es.gob.afirma.signers.padestri.server;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.exceptions.InvalidPdfException;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfPKCS7;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignature;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;

/** Clase para la firma electr&oacute;nica en tres fases de ficheros Adobe PDF en formato PAdES.
 * <p>No firma PDF cifrados.</p>
 * <p>Necesita iText 2.1.7.</p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class PAdESBiPhaseSigner {

    private static final int CSIZE = 8000;

    /** Versi&oacute;n de iText necesaria para el uso de esta clase (2.1.7). */
    public static final String ITEXT_VERSION = "2.1.7"; //$NON-NLS-1$

    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.46 o superior). */
    public static final String BC_VERSION = "1.46"; //$NON-NLS-1$

    /** Construye un firmador PAdES bif&aacute;sico, comprobando que la versiones existentes de iText y Bouncycastle sean las adecuadas.
     * @throws UnsupportedOperationException si se encuentra bibliotecas iText o BouncyCastle en versiones incompatibles */
    public PAdESBiPhaseSigner() {
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

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    public static final int LAST_PAGE = -666;


    /** Realiza la primera fase de una firma PAdES bif&aacute;sica.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param inPDF Documento PDF a pre-firmar.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>applySystemDate</i></b></dt>
     *   <dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario.
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
     *    No se soporta el cifrado de documentos PDF con certificados o con algoritmo AES256.
     *   </dd>
     * </dl>
     * @return Pre-firma del documento PDF
     * @throws IOException En caso de problemas de entrada - salida
     * @throws AOException En caso de cualquier otro problema */
    public static PAdESBiPhasePreSignResult preSign(final String algorithm,
                                             final byte[] inPDF,
                                             final X509Certificate[] signerCertificateChain,
                                             final Properties xParams) throws IOException, AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        try {
            return signPDF(signerCertificateChain, inPDF, extraParams, algorithm);
        }
        catch (final InvalidPdfException e) {
            throw new AOFormatFileException("El documento no era un PDF valido", e); //$NON-NLS-1$
        }
        catch (final AOException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error firmando el PDF: " + e, e); //$NON-NLS-1$
        }

    }

    private static PAdESBiPhasePreSignResult signPDF(final X509Certificate[] certChain,
                                              final byte[] inPDF,
                                              final Properties extraParams,
                                              final String algorithm) throws IOException,
                                                                             AOException,
                                                                             DocumentException,
                                                                             NoSuchAlgorithmException {

        final boolean useSystemDateTime = Boolean.parseBoolean(extraParams.getProperty("applySystemDate", Boolean.TRUE.toString())); //$NON-NLS-1$
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

        final PdfReader pdfReader;
        try {
            pdfReader = new PdfReader(inPDF);
        }
        catch (final BadPasswordException e) {
            throw new UnsupportedOperationException("El PDF esta cifrado y no se soporta la firma en dos fases de PDF cifrados", e); //$NON-NLS-1$
        }

        if (pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED && !Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$
            throw new UnsupportedOperationException("No se permite la firma de PDF certificados (el paramtro allowSigningCertifiedPdfs estaba establecido a false)"); //$NON-NLS-1$
        }

        // Los derechos van firmados por Adobe, y como desde iText se invalidan
        // es mejor quitarlos
        pdfReader.removeUsageRights();

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();

        final Calendar globalDate = Calendar.getInstance();

        // Activar el atributo de "agregar firma" (cuarto parametro del metodo
        // "PdfStamper.createSignature") hace que se cree una nueva revision del
        // documento y evita que las firmas previas queden invalidadas. Sin embargo, este
        // exige que el PDF no incorpore ningun error, asi que lo mantendremos desactivado
        // para la primera firma y activado para las subsiguientes. Un error incorporado
        // en un PDF erroneo puede quedar subsanado en su version firmada, haciendo
        // posible incorporar nuevas firmas agregando revisiones del documento.
        final PdfStamper stp = PdfStamper.createSignature(
              pdfReader, // PDF de entrada
              baos, // Salida
              '\0', // Mantener version
              null, // No crear temporal
              pdfReader.getAcroFields().getSignatureNames().size() > 0, // Si hay mas firmas, creo una revision
              globalDate
        );

        final PdfSignatureAppearance sap = stp.getSignatureAppearance();

        stp.setFullCompression();
        sap.setAcro6Layers(true);
        sap.setLayer2Text(""); //$NON-NLS-1$
        sap.setLayer4Text(""); //$NON-NLS-1$
        // iText antiguo
        sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        // En iText nuevo seria "sap.setRenderingMode(PdfSignatureAppearance.RenderingMode.NAME_AND_DESCRIPTION);"
        if (reason != null) {
            sap.setReason(reason);
        }
        if (useSystemDateTime) {
            sap.setSignDate(new GregorianCalendar());
        }

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
        final String rub = extraParams.getProperty("rubric"); //$NON-NLS-1$
        if (rub != null) {
            try {
                sap.setImage(new Jpeg(Base64.decode(rub)));
            }
            catch (final Exception e) {
                LOGGER.severe(
                  "No se pudo establecer la imagen de firma para el documento PDF, no se usara imagen: " + e //$NON-NLS-1$
                );
            }
        }

        sap.setCrypto(null, certChain, null, null);

        final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, PdfName.ADBE_PKCS7_DETACHED);
        if (sap.getSignDate() != null) {
            dic.setDate(new PdfDate(sap.getSignDate()));
        }
        dic.setName(PdfPKCS7.getSubjectFields(certChain[0]).getField("CN")); //$NON-NLS-1$
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

        final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
        exc.put(PdfName.CONTENTS, Integer.valueOf(CSIZE * 2 + 2));

        sap.preClose(exc, globalDate);

        // ********************************************************************************
        // **************** CALCULO DEL SIGNED DATA ***************************************
        // ********************************************************************************

        // Las firmas CAdES internas a las PAdES son siempre explicitas
        extraParams.put("mode", "explicit"); //$NON-NLS-1$ //$NON-NLS-2$

        final CAdESBiPhasePreSignResult preSignCAdES = CAdESBiPhaseSigner.preSign(
             algorithm,
             AOUtil.getDataFromInputStream(sap.getRangeStream()),
             certChain,
             extraParams
        );

        // Guardamos los bytes a reemplazar para ver luego donde han quedado
        final byte[] pkcs1Sign = new byte[preSignCAdES.getLengthOfPKCS1ToReplace()];
        System.arraycopy(
            preSignCAdES.getSignature(),
            preSignCAdES.getIndexOfPKCS1ToReplace(),
            pkcs1Sign,
            0,
            preSignCAdES.getLengthOfPKCS1ToReplace()
        );

        // ********************************************************************************
        // *************** FIN CALCULO DEL SIGNED DATA ************************************
        // ********************************************************************************

        final byte[] outc = new byte[CSIZE];
        final PdfDictionary dic2 = new PdfDictionary();
        System.arraycopy(preSignCAdES.getSignature(), 0, outc, 0, preSignCAdES.getSignature().length);
        dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));
        sap.close(dic2);

        final byte[] finalPDF = baos.toByteArray();

        return new PAdESBiPhasePreSignResult(
             finalPDF,
             AOUtil.hexify(pkcs1Sign, false).toLowerCase(),
             preSignCAdES.getPreSign()
        );

    }

    /** Resultado de la primera fase (prefirma) de una firma bif&aacute;sica PAdES. */
    public static class PAdESBiPhasePreSignResult {
        private final byte[] signature;
        private final String PKCS1ToReplace;
        private final byte[] pkcs1Data;

        PAdESBiPhasePreSignResult(final byte[] sign, final String hexifiedPKCS1ToReplace, final byte[] p1Data) {
            this.signature = sign.clone();
            this.pkcs1Data = p1Data.clone();
            this.PKCS1ToReplace = hexifiedPKCS1ToReplace;
        }

        /** Devuelve el PDF completo pero con su firma interna CAdES realizada
         * con una clave privada impostada.
         * @return PDF firmado (inv&aacute;lido, es necesario sustituir el valor PKCS#1) */
        public byte[] getSignature() {
            return this.signature.clone();
        }

        /** Devuelve el valor textual del PKCS#1 que hay que reemplazar en el PDF obtenido mediante <code>getSignature()</code> para
         * obtener un PDF firmado v&aacute;lido.
         * <p>Una forma de hacer esta sustituci&oacute;n pdr&iacute;a ser (la cadena del nuevo PKCS#1 debe usar letras min&uacute;sculas):</p>
         * <p><code>new String(padesBiPhasePreSignResult.getSignature(), "ISO-8859-1").replace(padesBiPhasePreSignResult.getPKCS1ToReplace(), AOUtil.hexify(mybinarypkcs1, false).toLowerCase()).getBytes("ISO-8859-1");</code></p>
         * <p>Use siempre la codificaci&oacute;n "ISO-8859-1" para evitar problemas en sistemas operativos Unicode (como Windows o Linux).</p>
         * @return Indice de la primera posici&oacute;n a sustituir por el PKCS#1 correcto */
        public String getPKCS1ToReplace() {
            return this.PKCS1ToReplace;
        }

        /** Devuelve los datos (atributos CAdES a firmar) que deben firmarse con PKCS#1 v1.5 y la verdadera clave privada del
         * firmante para reemplazar en el PDF.
         * @return Atributos CAdES a firmar mediante PKCS#1 */
        public byte[] getPreSign() {
            return this.pkcs1Data;
        }
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

}
