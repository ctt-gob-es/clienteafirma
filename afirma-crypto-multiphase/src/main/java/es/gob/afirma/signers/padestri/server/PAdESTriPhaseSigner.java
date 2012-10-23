/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.padestri.server;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
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
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;
import es.gob.afirma.signers.padestri.server.BadPdfPasswordException;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;

/** Clase para la firma electr&oacute;nica en tres fases de ficheros Adobe PDF en formato PAdES.
 * <p>No firma PDF cifrados.</p>
 * <p>Necesita iText 2.1.7 con modificaciones espec&iacute;ficas.</p>
 * <p>Esta clase no interacciona directamente en ning&uacute;n momento con el usuario ni usa interfaces gr&aacute;ficos.</p>
 * <p>La firma electr&oacute;nica en tres fases est&aacute; pensada para entornos donde la clave privada reside
 * en un sistema con al menos alguna de las siguientes restricciones:</p>
 * <ul>
 *  <li>
 *   El sistema no es compatible con el Cliente @firma. En este caso, dado que el 95% del c&oacute;digo se
 *   ejecuta en un sistema externo, solo es necesario portar el 5% restante.
 *  </li>
 *  <li>
 *   El sistema tiene unas capacidades muy limitadas en cuanto a proceso computacional, memoria o comunicaciones por
 *   red. En este caso, el sistema solo realiza una operaci&oacute;n criptogr&aacute;fica, una firma PKCS#1,
 *   mucho menos demandante de potencia de proceso que una firma completa PAdES, y, adicionalmente, no trata el
 *   documento a firmar completo, sino &uacute;icamente una prque&ntilde;a cantidad de datos resultante de un
 *   pre-proceso (la pre-firma) realizado por el sistema externo, lo que resulta en un enorme decremento en las necesidades
 *   de memoria y transmisi&oacute;n de datos.
 *  </li>
 *  <li>
 *   Por motivos de seguridad, el documento a firmar no puede salir de un sistema externo. Como se ha descrito en el punto
 *   anterior, en este caso el documento jam&aacute;s sale del sistema externo, sino que se transfiere &uacute;nicamente
 *   el resultado de la pre-firma, desde la cual es imposible reconstruir el documento original.
 *  </li>
 * </ul>
 * <p>
 *  Estos condicionantes convierten la firma trif&aacute;sica en una opci&oacute;n perfectamente adaptada a los
 *  dispositivos m&oacute;viles, donde se dan tanto la heterogeneidad de sistemas operativos (Apple iOS, Google
 *  Android, RIM BlackBerry, Microsoft Windows Phone, etc.) y las limitaciones en potencia de proceso, memoria
 *  y comunicaciones; en estas &uacute;ltimas hay que tener en cuenta el coste, especialmente si estamos haciendo
 *  uso de una red de otro operador en itinerancia (<i>roaming</i>).
 * </p>
 * <p>
 *  El funcionamiento t&iacute;pico de una firma trif&aacute;sica en la que intervienen un disposotivo m&oacute;vil,
 *  un servidor Web (que hace la pre-firma y la post-firma) y un servidor documental podr&iacute;a ser el siguiente:
 * </p>
 * <p><b>Pre-firma:</b></p>
 * <p align="center"><img src="doc-files/PAdESTriPhaseSigner-1.png"></p>
 * <ul>
 *  <li>El dispositivo m�vil solicita una pre-firma al servidor Web indicando un identificador de documento.</li>
 *  <li>El servidor Web solicita el documento a servidor documental.</li>
 *  <li>
 *   El servidor documental entrega el documento al servidor Web.<br>Es importante recalcar que el servidor
 *   documental no necesita almacenar ning&uacute;n dato de sesi&oacute;n y que este no est&aacute; expuesto a Internet
 *   de forma directa en ning&uacute;n momento.
 *  </li>
 *  <li>
 *   El servidor Web calcula la pre-firma, entregando el resultado (muy peque&ntilde;o en tama&ntilde;o) al dispositivo.<br>
 *   Es importante recalcar que el servidor Web no necesita almacenar ning&uacute;n dato de sesi&oacute;n ni
 *   exponer los documentos directamente al dispositivo.
 *  </li>
 * </ul>
 * <p><b>Firma:</b></p>
 * <p align="center"><img src="doc-files/PAdESTriPhaseSigner-2.png"></p>
 * <ul>
 *  <li>
 *   El dispositivo m&oacute;vil realiza, de forma completamente aislada una firma electr&oacute;nica
 *   simple (computacionalmente ligera) de los datos de la pre-firma. La clave privada del usuario nunca sale
 *   del dispositivo y no se expone externamente en ning�n momento.
 *  </li>
 * </ul>
 * <p><b>Post-firma:</b></p>
 * <p align="center"><img src="doc-files/PAdESTriPhaseSigner-3.png"></p>
 * <ul>
 *  <li>
 *   El dispositivo m&oacute;vil solicita una post-firma al servidor Web indicando un identificador de
 *   documento y proporcionando el resultado de su pre-firma firmada.
 *  </li>
 *  <li>El servidor Web solicita el documento a servidor documental.</li>
 *  <li>El servidor documental entrega el documento al servidor Web.</li>
 *  <li>
 *   El servidor Web calcula la post-firma y compone el documento final firmado, entregando el resultado
 *   al servidor documental para su almac&eacute;n.
 *  </li>
 *  <li>El servidor documental almacena el nuevo documento y devuelve un identificador al servidor Web.</li>
 *  <li>
 *   El servidor Web comunica al dispositivo el �xito de la operaci�n y el identificador del fichero
 *   ya firmado y almacenado.
 *  </li>
 * </ul>
 * <p>
 *  Es conveniente tener en cuenta al usar firmas trif&aacute;sicas que es necesario disponer de un mecanismo
 *  para que el usuario pueda ver en todo momento los documentos que est&aacute; firmando (una copia que refleje
 *  con fidelidad el contenido firmado puede ser suficiente) para evitar situaciones de repudio.
 * </p>
 * <p>
 *  Una pecualiaridad de las firmas trif&aacute;sicas PAdES es que en la generaci&oacute;n o firma de un PDF se genera de forma
 *  autom&aacute;tica un identificador &uacute;nico y aleatorio llamado <i>FILE_ID</i>, que hace que al firmar en momentos diferentes
 *  dos PDF exactamente iguales se generen PDF con un <i>FILE_ID</i> distinto, y, por lo tanto, con la huella
 *  digital de la firma electr&oacute;nica distinta.<br>
 *  Para solventar este inconveniente, en la firma trif&aacute;sica PDF, se considera prefirma tanto la totalidad de los atributos
 *  CAdES a firmar como el <i>FILE_ID</i> del PDF que se debe compartir entre pre-firma y post-firma.
 * </p>
 *  Notas sobre documentos <i>certificados</i>:<br>
 *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
 *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
 *  las firmas previamente existentes.<br>
 *  Consulte la documentaci&oacute;n de la opci&oacute;n <code>allowSigningCertifiedPdfs</code> para establecer un comportamiento por
 *  defecto respecto a los PDF certificados.
 * </p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * */
public final class PAdESTriPhaseSigner {

	private static final String PDF_OID = "1.2.826.0.1089.1.5"; //$NON-NLS-1$
	private static final String PDF_DESC = "Documento en formato PDF"; //$NON-NLS-1$

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    public static final int LAST_PAGE = -666;

    /** Versi&oacute;n de iText necesaria para el uso de esta clase (2.1.7). */
    public static final String ITEXT_VERSION = "2.1.7"; //$NON-NLS-1$

    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.46 o superior). */
    public static final String BC_VERSION = "1.46"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

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

    private static final int CSIZE = 8000;

    /** Obtiene la pre-firma PAdES/CAdES de un PDF (atributos CAdES a firmar)
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
     * @param xParams Par&aacute;metros adicionales para la firma. Deben usarse exactamente los mismos valores en la post-firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signatureField</i></b></dt>
     *   <dd>
     *    Nombre del campo en donde insertar la firma.
     *    Si el documento PDF tiene ya un campo de firma precreado es posible utilizarlo para insertar la firma generada, referenci&aacute;ndolo
     *    por su nombre.<br>
     *    Si se indica un nombre de campo de firma que no exista en el documento PDF proporcionado, se generar&aacute; una excepci&oacute;n.
     *   </dd>
     *  <dt><b><i>signatureRubricImage</i></b></dt>
     *   <dd>Imagen JPEG codificada en Base64 de la r&uacute;brica de la firma manuscrita que se desea aparezca como firma visible en el PDF.</dd>
     *  <dt><b><i>signaturePage</i></b></dt>
     *   <dd>
     *    P&aacute;gina del documento PDF donde insertar la firma. Puede usarse la constante <code>LAST_PAGE</code>
     *    para referirse a la &uacute;ltima p&aacute;gina del documento PDF si se desconoce el n&uacute;mero total de
     *    p&aacute;ginas de este.<br>
     *    Este par&aacute;metro se ignora si se ha establecido valor al par&aacute;metro <i>signatureField</i> y necesita que se
     *    establezcan valores v&aacute;lidos a los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>, <i>signaturePositionOnPageLowerLeftY</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageLowerLeftX</i></b></dt>
     *   <dd>
     *    Coordenada horizontal inferior izquiera de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftY</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageLowerLeftY</i></b></dt>
     *   <dd>
     *    Coordenada vertical inferior izquiera de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageUpperRightX</i></b></dt>
     *   <dd>
     *    Coordenada horizontal superior derecha de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageLowerLeftY</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageUpperRightY</i></b></dt>
     *   <dd>
     *    Coordenada vertical superior derecha de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageLowerLeftY</i> y <i>signaturePositionOnPageUpperRightX</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>applySystemDate</i></b></dt>
     *   <dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario.
     *  <dt><b><i>signReason</i></b></dt>
     *   <dd>Raz&oacute;n por la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signerContact</i></b></dt>
     *   <dd>
     *    Contacto del firmante, usualmente una direcci&oacute;n de coreo electr&oacute;nico
     *    (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique
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
     *  <dt><b><i>ownerPassword</i></b></dt>
     *   <dd>
     *    Contrase&ntilde;a de apertura del PDF (contrase&ntilde;a del propietario) si este estaba cifrado.<br>
     *    No se soporta la firma de documentos PDF cifrados con certificados o con algoritmo AES256.
     *   </dd>
     *  <dt><b><i>headLess</i></b></dt>
     *   <dd>
     *    Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si no se establece o se establece a <code>false</code>
     *    act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para
     *    los procesos desatendidos y por lotes
     *   </dd>
     *  <dt><b><i>avoidEncryptingSignedPdfs</i></b></dt>
     *   <dd>
     *    Si se establece a <code>true</code> no cifra los PDF firmados aunque el original estuviese firmado, si no se establece o se establece a
     *    <code>false</code> los PDF se cifran tras firmarse si el original lo estaba, usando la misma contrase&ntilde;a y opciones que este
     *   </dd>
     *  <dt><b><i>allowSigningCertifiedPdfs</i></b></dt>
     *   <dd>
     *    Si se establece a <code>true</code> permite la firma o cofirma de PDF certificados sin consultarlo al usuario, si se establece a
     *    <code>false</code> o cualquier otro valor se lanza una excepci&oacute;n en caso de intentar firmar o cofirmar un PDF certificado y
     *    si no se establece se mostrar&aacute; un di&aacute;logo al usuario para que confirme que desea realizar la firma a pesar de que
     *    el resultado ser&aacute;n una firma no v&aacute;lida.<br>
     *    <b>Si el par&aacute;metro <code>headLess</code> est&aacute; establecido a <code>true</code>, no podr&aacute; mostrar el di&aacute;logo
     *    de confirmaci&oacute;n as&iacute; que llegados a este punto se lanzar&aacute; una excepci&oacute;n.</b><br>
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
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>Si se indica a {@code true} se utilizar SigningCertificateV2, si se indica cualquier otra cosa SigningCertificateV1.
     *   Si no se indica nada, se utilizar&aacute; V1 para las firmas SHA1 y V2 para el resto.</dd>
     * </dl>
     * @param signTime Momento de la firma. Debe usarse exactamente el mismo valor en la post-firma.
     * @return pre-firma CAdES/PAdES (atributos CAdES a firmar)
     * @throws IOException
     * @throws AOException
     */
    public static PdfPreSignResult preSign(final String digestAlgorithmName,
                                           final byte[] inPDF,
                                           final X509Certificate[] signerCertificateChain,
                                           final Calendar signTime,
                                           final Properties xParams) throws IOException, AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final PdfTriPhaseSession ptps = getSessionData(inPDF, signerCertificateChain, signTime, extraParams);

        final byte[] original = AOUtil.getDataFromInputStream(ptps.getSAP().getRangeStream());

        // Calculamos el MessageDigest
        final MessageDigest md;
        try {
            md = MessageDigest.getInstance(digestAlgorithmName);
        }
        catch (final NoSuchAlgorithmException e) {
            throw new AOException("El algoritmo de huella digital no es valido", e); //$NON-NLS-1$
        }
        md.update(original);

        // pre-firma CAdES
        return new PdfPreSignResult(
            ptps.getFileID(),
            CAdESTriPhaseSigner.preSign(
                digestAlgorithmName, // Algoritmo de huella digital
                null, // Datos a firmar (null por ser explicita))
                signerCertificateChain, // Cadena de certificados del firmante
                new AdESPolicy(extraParams), // Politica de firma
                true, // signingCertificateV2
                md.digest(), // Valor de la huella digital del contenido
                signTime.getTime(), // Fecha de la firma (debe establecerse externamente para evitar desincronismos en la firma trifasica)
                true, // Modo PAdES
                PDF_OID,
                PDF_DESC
            )
        );
    }

    /** Post-firma en PAdES un documento PDF a partir de una pre-firma y la firma PKCS#1, generando un PDF final completo.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital usado para la firma (debe ser el mismo que el usado en la pre-firma).
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>digestAlgorithmName</code>:</p>
     * <ul>
     *  <li><i>SHA1</i></li>
     *  <li><i>MD5</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA-256</i></li>
     *  <li><i>SHA-384</i></li>
     *  <li><i>SHA-512</i></li>
     * </ul>
     * @param inPDF PDF a firmar (debe ser el mismo que el usado en la pre-firma).
     * @param signerCertificateChain Cadena de certificados del firmante (debe ser la misma que la usado en la pre-firma).
     * @param xParams Opciones adicionales para la firma (deben ser las mismas que las usadas en la pre-firma).
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>signatureField</i></b></dt>
     *   <dd>
     *    Nombre del campo en donde insertar la firma.
     *    Si el documento PDF tiene ya un campo de firma precreado es posible utilizarlo para insertar la firma generada, referenci&aacute;ndolo
     *    por su nombre.<br>
     *    Si se indica un nombre de campo de firma que no exista en el documento PDF proporcionado, se generar&aacute; una excepci&oacute;n.
     *   </dd>
     *  <dt><b><i>signatureRubricImage</i></b></dt>
     *   <dd>Imagen JPEG codificada en Base64 de la r&uacute;brica de la firma manuscrita que se desea aparezca como firma visible en el PDF.</dd>
     *  <dt><b><i>signaturePage</i></b></dt>
     *   <dd>
     *    P&aacute;gina del documento PDF donde insertar la firma. Puede usarse la constante <code>LAST_PAGE</code>
     *    para referirse a la &uacute;ltima p&aacute;gina del documento PDF si se desconoce el n&uacute;mero total de
     *    p&aacute;ginas de este.<br>
     *    Este par&aacute;metro se ignora si se ha establecido valor al par&aacute;metro <i>signatureField</i> y necesita que se
     *    establezcan valores v&aacute;lidos a los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>, <i>signaturePositionOnPageLowerLeftY</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageLowerLeftX</i></b></dt>
     *   <dd>
     *    Coordenada horizontal inferior izquiera de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftY</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageLowerLeftY</i></b></dt>
     *   <dd>
     *    Coordenada vertical inferior izquiera de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageUpperRightX</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageUpperRightX</i></b></dt>
     *   <dd>
     *    Coordenada horizontal superior derecha de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageLowerLeftY</i> y <i>signaturePositionOnPageUpperRightY</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>signaturePositionOnPageUpperRightY</i></b></dt>
     *   <dd>
     *    Coordenada vertical superior derecha de la posici&oacute;n del recuadro visible de la fimra dentro de la p&aacute;gina.<br>
     *    Es necesario indicar el resto de coordenadas del recuadro mediante los par&aacute;metros <i>signaturePositionOnPageLowerLeftX</i>,
     *    <i>signaturePositionOnPageLowerLeftY</i> y <i>signaturePositionOnPageUpperRightX</i>.<br>
     *    Si no se indica una p&aacute;gina en el par&aacute;metro <i>signaturePage</i> la firma se inserta en la &uacute;ltima p&aacute;gina
     *    del documento.
     *   </dd>
     *  <dt><b><i>applySystemDate</i></b></dt>
     *   <dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario.
     *  <dt><b><i>signReason</i></b></dt>
     *   <dd>Raz&oacute;n por la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signatureProductionCity</i></b></dt>
     *   <dd>Ciudad en la que se realiza la firma (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).</dd>
     *  <dt><b><i>signerContact</i></b></dt>
     *   <dd>
     *    Contacto del firmante, usualmente una direcci&oacute;n de coreo electr&oacute;nico
     *    (este dato se a&ntilde;ade al diccionario PDF, y no a la propia firma).
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique
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
     *  <dt><b><i>ownerPassword</i></b></dt>
     *   <dd>
     *    Contrase&ntilde;a de apertura del PDF (contrase&ntilde;a del propietario) si este estaba cifrado.<br>
     *    No se soporta la firma de documentos PDF cifrados con certificados o con algoritmo AES256.
     *   </dd>
     *  <dt><b><i>headLess</i></b></dt>
     *   <dd>
     *    Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si no se establece o se establece a <code>false</code>
     *    act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para
     *    los procesos desatendidos y por lotes
     *   </dd>
     *  <dt><b><i>avoidEncryptingSignedPdfs</i></b></dt>
     *   <dd>
     *    Si se establece a <code>true</code> no cifra los PDF firmados aunque el original estuviese firmado, si no se establece o se establece a
     *    <code>false</code> los PDF se cifran tras firmarse si el original lo estaba, usando la misma contrase&ntilde;a y opciones que este
     *   </dd>
     *  <dt><b><i>allowSigningCertifiedPdfs</i></b></dt>
     *   <dd>
     *    Si se establece a <code>true</code> permite la firma o cofirma de PDF certificados sin consultarlo al usuario, si se establece a
     *    <code>false</code> o cualquier otro valor se lanza una excepci&oacute;n en caso de intentar firmar o cofirmar un PDF certificado y
     *    si no se establece se mostrar&aacute; un di&aacute;logo al usuario para que confirme que desea realizar la firma a pesar de que
     *    el resultado ser&aacute;n una firma no v&aacute;lida.<br>
     *    <b>Si el par&aacute;metro <code>headLess</code> est&aacute; establecido a <code>true</code>, no podr&aacute; mostrar el di&aacute;logo
     *    de confirmaci&oacute;n as&iacute; que llegados a este punto se lanzar&aacute; una excepci&oacute;n.</b><br>
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
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>Si se indica a {@code true} se utilizar SigningCertificateV2, si se indica cualquier otra cosa SigningCertificateV1.
     *   Si no se indica nada, se utilizar&aacute; V1 para las firmas SHA1 y V2 para el resto.</dd>
     * </dl>
     * @param signature Resultado de la firma PKCS#1 v1.5 de los datos de la pre-firma.
     * @param signedAttributes Resultado de la pre-firma CAdES/PAdES (atributos CAdES a firmar)
     * @param fileID FileID del PDF generado en la pre-firma
     * @param signTime  Momento de la firma (debe ser el mismo que el usado en la pre-firma).
     * @return PDF firmado
     * @throws AOException en caso de cualquier tipo de error
     * @throws IOException
     * @throws NoSuchAlgorithmException Si hay problemas con el algoritmo durante el sello de tiempo */
    public static byte[] postSign(final String digestAlgorithmName,
                    final byte[] inPDF,
                    final X509Certificate[] signerCertificateChain,
                    final Properties xParams,
                    final byte[] signature,
                    final byte[] signedAttributes,
                    final String fileID,
                    final Calendar signTime) throws AOException, IOException, NoSuchAlgorithmException {

        byte[] completeCAdESSignature = CAdESTriPhaseSigner.postSign(digestAlgorithmName, null, signerCertificateChain, signature, signedAttributes);

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        //***************** SELLO DE TIEMPO ****************
        final String tsa = extraParams.getProperty("tsaURL"); //$NON-NLS-1$
        URL tsaURL;
        if (tsa != null) {
            try {
                tsaURL = new URL(tsa);
            }
            catch(final Exception e) {
                LOGGER.warning("Se ha indicado una URL de TSA invalida (" + tsa + "), no se anadira sello de tiempo: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                tsaURL = null;
            }
            if (tsaURL != null) {
                final String tsaPolicy = extraParams.getProperty("tsaPolicy"); //$NON-NLS-1$
                if (tsaPolicy == null) {
                    LOGGER.warning("Se ha indicado una URL de TSA pero no una politica, no se anadira sello de tiempo"); //$NON-NLS-1$
                }
                else {
                    final String tsaHashAlgorithm = extraParams.getProperty("tsaHashAlgorithm"); //$NON-NLS-1$
                    completeCAdESSignature = new CMSTimestamper(
                         !(Boolean.FALSE.toString()).equalsIgnoreCase(extraParams.getProperty("tsaRequireCert")),  //$NON-NLS-1$
                         tsaPolicy,
                         tsaURL,
                         extraParams.getProperty("tsaUsr"),  //$NON-NLS-1$
                         extraParams.getProperty("tsaPwd") //$NON-NLS-1$
                     ).addTimestamp(completeCAdESSignature, AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName((tsaHashAlgorithm != null) ? tsaHashAlgorithm : "SHA1"))); //$NON-NLS-1$
                }
            }

        }
        //************** FIN SELLO DE TIEMPO ****************

        final byte[] outc = new byte[CSIZE];

        final PdfDictionary dic2 = new PdfDictionary();
        System.arraycopy(completeCAdESSignature, 0, outc, 0, completeCAdESSignature.length);
        dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));

        final PdfTriPhaseSession pts = getSessionData(inPDF, signerCertificateChain, signTime, extraParams);
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

    /** Resultado de una pre-firma (como primera parte de un firma trif&aacute;sica) PAdES. */
    /** <i>JavaBean</i> que encapsula los resultados de la pre-firma PDF. */
    public static class PdfPreSignResult {

        private final String fileID;
        private final byte[] preSign;

        PdfPreSignResult(final String pdfFileId, final byte[] preSignature) {
            if (pdfFileId == null || preSignature == null || "".equals(pdfFileId) || preSignature.length < 1) { //$NON-NLS-1$
                throw new IllegalArgumentException("Es obligatorio proporcionar un MAC y una pre-firma"); //$NON-NLS-1$
            }
            this.fileID = pdfFileId;
            this.preSign = preSignature.clone();
        }

        /** Obtiene el FileID (<i>/ID</i>) del diccionario PDF generado.
         * @return FileID del diccionario PDF generado
         */
        public String getFileID() {
            return this.fileID;
        }

        /** Obtiene los atributos CAdES a firmar.
         * @return Atributos CAdES a firmar (pre-firma) */
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

    private static PdfTriPhaseSession getSessionData(final byte[] inPDF,
                                              final X509Certificate[] chain,
                                              final Calendar signTime,
                                              final Properties extraParams) throws AOException {

        final String reason = extraParams.getProperty("signReason"); //$NON-NLS-1$
        final String signatureField = extraParams.getProperty("signatureField"); //$NON-NLS-1$
        final String signatureProductionCity = extraParams.getProperty("signatureProductionCity"); //$NON-NLS-1$
        final String signerContact = extraParams.getProperty("signerContact"); //$NON-NLS-1$
        int page = LAST_PAGE;
        try {
            page = Integer.parseInt(extraParams.getProperty("signaturePage")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            /* Se deja la pagina tal y como esta */
        }

        PdfReader pdfReader;
        final String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$
        final String userPassword =  extraParams.getProperty("userPassword"); //$NON-NLS-1$
        try {
            if (ownerPassword != null) {
            	pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
            }
            else if (userPassword != null) {
            	pdfReader = new PdfReader(inPDF, userPassword.getBytes());
            }
            else {
            	pdfReader = new PdfReader(inPDF);
            }
        }
        catch (final BadPasswordException e) {
            throw new BadPdfPasswordException(e);
        }
        catch (final  com.lowagie.text.exceptions.InvalidPdfException e) {
        	throw new InvalidPdfException(e);
		}
        catch (final IOException e) {
			throw new AOException("Error firmando el PDF", e); //$NON-NLS-1$
		}

        if ((pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED) && (!Boolean.parseBoolean(extraParams.getProperty("allowSigningCertifiedPdfs")))) { //$NON-NLS-1$
            throw new PdfIsCertifiedException();
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
                  pdfReader.getAcroFields().getSignatureNames().size() > 0, // Si hay mas firmas, creo una revision
                  signTime
            );
        }
        catch(final BadPasswordException e) {
        	throw new PdfIsPasswordProtectedException(e);
        }
        catch(final Exception e) {
        	throw new AOException("Error creando el campo de firma: " + e, e); //$NON-NLS-1$
        }

        // Aplicamos todos los atributos de firma
        final PdfSignatureAppearance sap = stp.getSignatureAppearance();
        stp.setFullCompression();
        sap.setAcro6Layers(true);

        // iText antiguo
        sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        // En iText nuevo seria "sap.setRenderingMode(PdfSignatureAppearance.RenderingMode.NAME_AND_DESCRIPTION);"

        if (reason != null) {
            sap.setReason(reason);
        }

        sap.setSignDate(signTime);

        if (pdfReader.isEncrypted() && (ownerPassword != null || userPassword != null)) {
            if (Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("avoidEncryptingSignedPdfs"))) { //$NON-NLS-1$
                LOGGER.info(
                    "Aunque el PDF original estaba encriptado no se encriptara el PDF firmado (se establecio el indicativo 'avoidEncryptingSignedPdfs')" //$NON-NLS-1$
                );
            }
            else {
                LOGGER.info(
                    "El PDF original estaba encriptado, se intentara encriptar tambien el PDF firmado" //$NON-NLS-1$
                );
                try {
                	stp.setEncryption(
                		(ownerPassword != null) ? ownerPassword.getBytes() : null,
        				(userPassword != null) ? userPassword.getBytes() : null,
                		pdfReader.getPermissions(),
                		pdfReader.getCryptoMode()
            		);
                }
                catch (final DocumentException de) {
                    LOGGER.warning(
                       "No se ha podido cifrar el PDF destino, se escribira sin contrasena: " + de //$NON-NLS-1$
                    );
                }
            }
        }

        // Pagina en donde se imprime la firma
        if (page == LAST_PAGE) {
            page = pdfReader.getNumberOfPages();
        }

        // Posicion de la firma
        final Rectangle signaturePositionOnPage = getSignaturePositionOnPage(extraParams);
        if (signaturePositionOnPage != null && signatureField == null) {
            sap.setVisibleSignature(signaturePositionOnPage, page, null);
        }
        else if (signatureField != null) {
            sap.setVisibleSignature(signatureField);
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
        final Image rubric = getRubricImage(extraParams.getProperty("signatureRubricImage")); //$NON-NLS-1$
        if (rubric != null) {
            sap.setImage(rubric);
            sap.setLayer2Text(""); //$NON-NLS-1$
            sap.setLayer4Text(""); //$NON-NLS-1$
        }

        sap.setCrypto(null, chain, null, null);

        final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, PdfName.ADBE_PKCS7_DETACHED);
        if (sap.getSignDate() != null) {
            dic.setDate(new PdfDate(signTime));
        }
        dic.setName(PdfPKCS7.getSubjectFields(chain[0]).getField("CN")); //$NON-NLS-1$
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
            sap.preClose(exc, signTime);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido cerrar el conjunto de atributos de la firma PDF", e); //$NON-NLS-1$
        }

        final PdfObject pdfObject = ((com.lowagie.text.pdf.PdfStamperImp) stp.getWriter()).getFileID();

        return new PdfTriPhaseSession(sap, baos, new String(pdfObject.getBytes()));
    }

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse
     * la firma. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a
     * arriba. */
    private static Rectangle getSignaturePositionOnPage(final Properties extraParams) {
    	if (extraParams.getProperty("signaturePositionOnPageLowerLeftX") != null && //$NON-NLS-1$
    		extraParams.getProperty("signaturePositionOnPageLowerLeftY") != null && //$NON-NLS-1$
			extraParams.getProperty("signaturePositionOnPageUpperRightX") != null && //$NON-NLS-1$
			extraParams.getProperty("signaturePositionOnPageUpperRightY") != null //$NON-NLS-1$
		) {
	        try {
	            return new Rectangle(Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftX")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftY")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightX")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightY")) //$NON-NLS-1$
	            );
	        }
	        catch (final Exception e) {
	        	LOGGER.severe("Se ha indicado una posicion de firma invalida: " + e); //$NON-NLS-1$
	        }
    	}
    	return null;
    }

    private static com.lowagie.text.Image getRubricImage(final String imagebase64Encoded) {
    	if (imagebase64Encoded == null || "".equals(imagebase64Encoded)) { //$NON-NLS-1$
    		return null;
    	}
    	final byte[] image;
    	try {
			image = Base64.decode(imagebase64Encoded);
		}
    	catch (final Exception e) {
    		LOGGER.severe("Se ha proporcionado una imagen de rubrica que no esta codificada en Base64: " + e); //$NON-NLS-1$
			return null;
		}
    	try {
			return new Jpeg(image);
		}
    	catch (final Exception e) {
    		LOGGER.severe("Se ha proporcionado una imagen de rubrica que no esta codificada en JPEG: " + e); //$NON-NLS-1$
			return null;
		}
    }

}
