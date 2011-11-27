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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.exceptions.InvalidPdfException;
import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.PdfArray;
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
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.SHA2AltNamesProvider;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.tsp.CMSTimestamper;

/** Manejador de firmas binarias de ficheros Adobe PDF en formato PAdES.
 * <p>Para compatibilidad estricta con PAdES-BES/EPES se utiliza <i>ETSI.CAdES.detached</i> como nombre del subfiltro.</p>
 * <p>La compatibilidad con PAdES no es completa, omiti&eacute;ndose los siguientes aspectos de la normativa:</p>
 * <ul>
 *  <li>Firma separada de ficheros empotrados en el documento PDF.</li>
 *  <li>Firma separada de ficheros adjuntos al documento PDF.</li>
 * </ul>
 * <p>
 *  Estas mismas deficiencias provocan igualmente la incompatibilidad de las firmas generadas con "Carpetas PDF" (<i>Portfolios PDF</i>). 
 *  Cuando se encuentran documentos PDF con ficheros adjuntos o empotrados se imprime informaci&oacute;n relativa en consola.
 * </p>
 * <p>
 *  La clase necesita espec&iacute;ficamente iText 2.1.7 (no se usan versiones m&aacute;s actuales por cuestiones de licencia) y
 *  BouncyCastle 1.46 o superior (Proveedor + TSP + <i>Mail</i>).
 * </p>
 */
public final class AOPDFSigner implements AOSigner {
    
    private static final int CSIZE = 8000;
    
    private static final String PADES_BES_SUBFILTER = "ETSI.CAdES.detached"; //$NON-NLS-1$
    
    private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$
    private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$
    
    /** Versi&oacute;n de iText necesaria para el uso de esta clase (2.1.7). */
    private static final String ITEXT_VERSION = "2.1.7"; //$NON-NLS-1$
    
    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.46 o superior). */
    private static final String BC_VERSION = "1.46"; //$NON-NLS-1$
    
    /** Construye un firmador PAdES, comprobando que la versiones existentes en el <i>CLASSPATH</i> de iText y BouncyCastle sean las adecuadas. 
     * @throws UnsupportedOperationException si se encuentra bibliotecas iText o BouncyCastle en versiones incompatibles */
    public AOPDFSigner() {
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

    /** Firma un documento PDF en formato PAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headLess</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headLess</code>.
     *  Adicionalmente, es posible, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, que la salida sea un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a. Consulte la documentaci&oacute;n de la opci&oacute;n <code>avoidEncryptingSignedPdfs</code>
     *  para m&aacute;s informaci&oacute;n.
     * </p>  
     * @param data Documento PDF a firmar
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
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
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
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        String signAlgorithm = algorithm;

        if (!algorithm.equals(AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA) && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA)) {

            LOGGER.warning("El algoritmo '" + algorithm + "' no esta soportado por PDF, se usara SHA-1"); //$NON-NLS-1$ //$NON-NLS-2$
            signAlgorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        try {
            return signPDF(keyEntry, data, extraParams, signAlgorithm);
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

    /** A&ntilde;ade una firma PAdES a un documento PDF. El comportamiento es exactamente el mismo que una llamada al m&eacute;todo <code>sign(...)</code>
     * puesto que las multifirmas en los ficheros PDF se limitan a firmas independientes "en serie", pero no implementando los mecanismos de
     * cofirma o contrafirma de CAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headLess</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headLess</code>.
     *  Adicionalmente, es posible, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, que la salida sea un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a. Consulte la documentaci&oacute;n de la opci&oacute;n <code>avoidEncryptingSignedPdfs</code>
     *  para m&aacute;s informaci&oacute;n.
     * </p>
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>
     * @param data Se ignora el valor de este par&aacute;metro. <b>El documento PDF debe proporcionarse mediante el par&aacute;tro <code>sign</code></b>.
     * @param sign Documento PDF a firmar
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
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
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
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] data, 
                         final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    /** A&ntilde;ade una firma PAdES a un documento PDF. El comportamiento es exactamente el mismo que una llamada al m&eacute;todo <code>sign(...)</code>
     * puesto que las multifirmas en los ficheros PDF se limitan a firmas independientes "en serie", pero no implementando los mecanismos de
     * cofirma o contrafirma de CAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headLess</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headLess</code>.
     *  Adicionalmente, es posible, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, que la salida sea un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a. Consulte la documentaci&oacute;n de la opci&oacute;n <code>avoidEncryptingSignedPdfs</code>
     *  para m&aacute;s informaci&oacute;n.
     * </p>
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>
     * @param sign Documento PDF a firmar
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
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
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
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    /** A&ntilde;ade una firma PAdES a un documento PDF. El comportamiento es exactamente el mismo que una llamada al m&eacute;todo <code>sign(...)</code>
     * puesto que las multifirmas en los ficheros PDF se limitan a firmas independientes "en serie", pero no implementando los mecanismos de
     * cofirma o contrafirma de CAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headLess</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headLess</code>.
     *  Adicionalmente, es posible, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, que la salida sea un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a. Consulte la documentaci&oacute;n de la opci&oacute;n <code>avoidEncryptingSignedPdfs</code>
     *  para m&aacute;s informaci&oacute;n.
     * </p>
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>
     * @param sign Documento PDF a firmar
     * @param targetType Se ignora el valor de este par&aacute;metro
     * @param targets Se ignora el valor de este par&aacute;metro
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
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
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
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros PDF"); //$NON-NLS-1$
    }

    /** Devuelve el nombre de fichero de firma predeterminado que se recomienda usar para
     * un PDF firmado con nombre original igual al proporcionado.
     * En este caso el resultado ser&aacute; siempre el nombre original m&aacute;s un
     * sufijo adicional (opcional) previo a la extensi&oacute;n. 
     * Siempre se termina el nombre de fichero con la extensi&oacute;n <i>.pdf</i>, incluso si el nombre original carec&iacute;a de esta.
     * @param originalName Nombre del fichero original que se firma.
     * @param inText Sufijo a agregar al nombre de fichero devuelto, inmediatamente anterior a la extensi&oacute;n.
     * @return Nombre apropiado para el fichero de firma. */
    public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = (inText != null ? inText : ""); //$NON-NLS-1$
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.toLowerCase().endsWith(PDF_FILE_SUFFIX)) { 
            return originalName.substring(0, originalName.length() - PDF_FILE_SUFFIX.length()) + inTextInt + PDF_FILE_SUFFIX; 
        }
        return originalName + inTextInt + PDF_FILE_SUFFIX; 
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n textos con el <i>CommonName</i> (CN X.500)
     * del titular del certificado u objetos de tipo AOSimpleSignInfo con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param asSimpleSignInfo
     *        Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres (CN X.500) de los
     *        titulares certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error. */
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        
        SHA2AltNamesProvider.install();
        
        final AOTreeNode root = new AOTreeNode("Datos"); //$NON-NLS-1$
        final AcroFields af;

        PdfReader pdfReader;
        try {
            pdfReader = new PdfReader(sign);
        }
        catch (final BadPasswordException e) {
            try {
                pdfReader =
                        new PdfReader(sign, new String(AOUIFactory.getPassword(PDFMessages.getString("AOPDFSigner.0"), null)).getBytes()); //$NON-NLS-1$
            }
            catch (final BadPasswordException e2) {
                LOGGER.severe("La contrasena del PDF no es valida, se devolvera un arbol vacio: " + e2); //$NON-NLS-1$
                return new AOTreeModel(root, root.getChildCount());
            }
            catch (final Exception e3) {
                LOGGER.severe("No se ha podido leer el PDF, se devolvera un arbol vacio: " + e3); //$NON-NLS-1$
                return new AOTreeModel(root, root.getChildCount());
            }
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido leer el PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
            return new AOTreeModel(root, root.getChildCount());
        }

        try {
            af = pdfReader.getAcroFields();
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido obtener la informacion de los firmantes del PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
            return new AOTreeModel(root, root.getChildCount());
        }
        final ArrayList<?> names = af.getSignatureNames();
        Object pkcs1Object = null;
        for (int i = 0; i < names.size(); ++i) {
            final PdfPKCS7 pcks7 = af.verifySignature(names.get(i).toString());
            if (asSimpleSignInfo) {
                final AOSimpleSignInfo ssi = new AOSimpleSignInfo(new X509Certificate[] {
                    pcks7.getSigningCertificate()
                }, pcks7.getSignDate().getTime());

                // Extraemos el PKCS1 de la firma
                try {
                    // iText antiguo
                    final Field digestField = AOUtil.classForName("com.lowagie.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
                    // En iText nuevo seria "final Field digestField = AOUtil.classForName("com.itextpdf.text.pdf.PdfPKCS7").getDeclaredField("digest");"
                    digestField.setAccessible(true);
                    pkcs1Object = digestField.get(pcks7);
                }
                catch (final Exception e) {
                    LOGGER.severe(
                      "No se ha podido obtener informacion de una de las firmas del PDF, se continuara con la siguiente: " + e //$NON-NLS-1$
                    );
                    continue;
                }
                if (pkcs1Object instanceof byte[]) {
                    ssi.setPkcs1((byte[]) pkcs1Object);
                }
                root.add(new AOTreeNode(ssi));
            }
            else {
                root.add(new AOTreeNode((AOUtil.getCN(pcks7.getSigningCertificate()))));
            }
        }

        return new AOTreeModel(root, root.getChildCount());
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos proporcionados son un documento PDF, <code>false</code> en caso contrario
     */
    public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
    }

    @SuppressWarnings("unused")
    private boolean isPdfFile(final byte[] data) {

        byte[] buffer = new byte[PDF_FILE_HEADER.length()];
        try {
            new ByteArrayInputStream(data).read(buffer);
        }
        catch (final Exception e) {
            buffer = null;
        }

        // Comprobamos que cuente con una cabecera PDF
        if (buffer != null && !PDF_FILE_HEADER.equals(new String(buffer))) {
            return false;
        }

        try {
            // Si lanza una excepcion al crear la instancia, no es un fichero PDF
            new PdfReader(data);
        }
        catch (final BadPasswordException e) {
            LOGGER.warning("El PDF esta protegido con contrasena, se toma como PDF valido"); //$NON-NLS-1$
            return true;
        }
        catch (final Exception e) {
            return false;
        }

        return true;
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos proporcionados son un documento PDF, <code>false</code> en caso contrario
     */
    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
    }

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse
     * la firma. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a
     * arriba. */
    private Rectangle getSignaturePositionOnPage(final Properties extraParams) {
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

    private byte[] signPDF(final PrivateKeyEntry ke, 
                           final byte[] inPDF, 
                           final Properties extraParams, 
                           final String algorithm) throws IOException, 
                                                          AOException, 
                                                          DocumentException, 
                                                          NoSuchAlgorithmException, 
                                                          CertificateException {

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

        PdfReader pdfReader;
        String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$
        try {
            if (ownerPassword == null) {
                pdfReader = new PdfReader(inPDF);
            }
            else {
                pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
            }
        }
        catch (final BadPasswordException e) {
            // Comprobamos que el signer esta en modo interactivo, y si no lo
            // esta no pedimos contrasena por dialogo, principalmente para no interrumpir un firmado por lotes
            // desatendido
            if (Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("headLess"))) { //$NON-NLS-1$ 
                throw new AOException("La contrasena proporcionada no es valida para el PDF actual", e); //$NON-NLS-1$
            }
            // La contrasena que nos han proporcionada no es buena o no nos
            // proporcionaron ninguna
            ownerPassword = new String(AOUIFactory.getPassword(PDFMessages.getString("AOPDFSigner.0"), null)); //$NON-NLS-1$
            try {
                pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
            }
            catch (final BadPasswordException e2) {
                throw new AOException("La contrasena proporcionada no es valida para el PDF actual", e2); //$NON-NLS-1$
            }
        }

        if (pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED && !Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$ 
            if (Boolean.FALSE.toString().equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$ 
                throw new UnsupportedOperationException("No se permite la firma de PDF certificados (el paramtro allowSigningCertifiedPdfs estaba establecido a false)"); //$NON-NLS-1$
            }

            if (Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("headLess"))) {  //$NON-NLS-1$
                throw new UnsupportedOperationException("No se permite la firma de PDF certificados (el paramtro allowSigningCertifiedPdfs no estaba establecido y no se permiten dialogos graficos)"); //$NON-NLS-1$
            }

            if (AOUIFactory.NO_OPTION == AOUIFactory.showConfirmDialog(null, 
                                                                       PDFMessages.getString("AOPDFSigner.8"), //$NON-NLS-1$
                                                                       PDFMessages.getString("AOPDFSigner.9"), //$NON-NLS-1$
                                                                       AOUIFactory.YES_NO_OPTION,
                                                                       AOUIFactory.WARNING_MESSAGE)) {
                throw new UnsupportedOperationException("No se ha permitido la firma de un PDF certificado"); //$NON-NLS-1$
            }
        }

        // ******************************************************************************
        // ********* Comprobaciones de adjuntos y empotrados PDF ************************
        // ******************************************************************************

        PdfArray array;
        PdfDictionary annot;
        PdfDictionary fs;
        PdfDictionary refs;
        for (int i = 1; i <= pdfReader.getNumberOfPages(); i++) {
            array = pdfReader.getPageN(i).getAsArray(PdfName.ANNOTS);
            if (array == null) {
                continue;
            }
            for (int j = 0; j < array.size(); j++) {
                annot = array.getAsDict(j);
                if (annot != null && PdfName.FILEATTACHMENT.equals(annot.getAsName(PdfName.SUBTYPE))) {
                    fs = annot.getAsDict(PdfName.FS);
                    if (fs != null) {
                        refs = fs.getAsDict(PdfName.EF);
                        if (refs != null) {
                            for (final Object name : refs.getKeys()) {
                                if (name instanceof PdfName) {
                                    LOGGER.warning(
                                       "Se ha encontrado un adjunto (" + fs.getAsString((PdfName) name) //$NON-NLS-1$
                                          + ") en el PDF, pero no se firmara de forma independiente"); //$NON-NLS-1$
                                }
                                // System.out.println(new String(PdfReader.getStreamBytes((PRStream)refs.getAsStream(name))));
                            }
                        }
                    }
                }
            }
        }

        final PdfDictionary catalog = pdfReader.getCatalog();
        if (catalog != null) {
            final PdfDictionary namesCatalog = catalog.getAsDict(PdfName.NAMES);
            if (namesCatalog != null) {
                final PdfDictionary filesCatalog = namesCatalog.getAsDict(PdfName.EMBEDDEDFILES);
                if (filesCatalog != null) {
                    final PdfArray filespecs = filesCatalog.getAsArray(PdfName.NAMES);
                    PdfDictionary filespec;
                    for (int i = 0; i < filespecs.size();) {
                        filespecs.getAsString(i++);
                        filespec = filespecs.getAsDict(i++);
                        refs = filespec.getAsDict(PdfName.EF);
                        for (final Object key : refs.getKeys()) {
                            if (key instanceof PdfName) {
                                LOGGER.warning("Se ha encontrado un fichero empotrado (" + filespec.getAsString((PdfName) key) //$NON-NLS-1$
                                               + ") en el PDF, pero no se firmara de forma independiente"); //$NON-NLS-1$

                                // System.out.println(new String(PdfReader.getStreamBytes((PRStream)
                                // PdfReader.getPdfObject(refs.getAsIndirectObject(key)))));
                            }
                        }
                    }
                }
            }
        }

        // ******************************************************************************
        // ********* Fin comprobaciones de adjuntos y empotrados PDF ********************
        // ******************************************************************************

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
        final PdfStamper stp = PdfStamper.createSignature(
              pdfReader, // PDF de entrada
              baos, // Salida
              '\0', // Mantener version
              null, // No crear temporal
              pdfReader.getAcroFields().getSignatureNames().size() > 0 // Si hay mas firmas, creo una revision
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

        if (pdfReader.isEncrypted() && ownerPassword != null) {
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
                    stp.setEncryption(ownerPassword.getBytes(), ownerPassword.getBytes(), pdfReader.getPermissions(), pdfReader.getCryptoMode());
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
        if (this.rubric != null) {
            try {
                sap.setImage(new Jpeg(this.rubric));
            }
            catch (final Exception e) {
                LOGGER.severe(
                  "No se pudo establecer la imagen de firma para el documento PDF, no se usara imagen: " + e //$NON-NLS-1$
                );
            }
        }

        final X509Certificate[] chain = (X509Certificate[]) ke.getCertificateChain();

        sap.setCrypto(null, chain, null, null);

        final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, new PdfName(PADES_BES_SUBFILTER));
        if (sap.getSignDate() != null) {
            dic.setDate(new PdfDate(sap.getSignDate()));
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

        final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
        exc.put(PdfName.CONTENTS, Integer.valueOf(CSIZE * 2 + 2));

        sap.preClose(exc);

        // ********************************************************************************
        // **************** CALCULO DEL SIGNED DATA ***************************************
        // ********************************************************************************

        byte[] pk =
                new GenCAdESEPESSignedData().generateSignedData(
                    new P7ContentSignerParameters(inPDF, algorithm, chain), 
                    true, // omitContent
                    new AdESPolicy(extraParams),
                    true, // true -> isSigningCertificateV2, false -> isSigningCertificateV1
                    ke,
                    MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(algorithm)).digest(AOUtil.getDataFromInputStream(sap.getRangeStream())));

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
                    pk = new CMSTimestamper(
                         !(Boolean.FALSE.toString()).equalsIgnoreCase(extraParams.getProperty("tsaRequireCert")),  //$NON-NLS-1$ 
                         tsaPolicy, 
                         tsaURL, 
                         extraParams.getProperty("tsaUsr"),  //$NON-NLS-1$
                         extraParams.getProperty("tsaPwd") //$NON-NLS-1$
                     ).addTimestamp(pk, AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName((tsaHashAlgorithm != null) ? tsaHashAlgorithm : "SHA1"))); //$NON-NLS-1$
                }
            }
            
        }
      //************** FIN SELLO DE TIEMPO ****************
        

        // ********************************************************************************
        // *************** FIN CALCULO DEL SIGNED DATA ************************************
        // ********************************************************************************

        final byte[] outc = new byte[CSIZE];
        final PdfDictionary dic2 = new PdfDictionary();
        System.arraycopy(pk, 0, outc, 0, pk.length);
        dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));
        sap.close(dic2);

        return baos.toByteArray();
    }

    /** R&uacute;brica de la firma. */
    private byte[] rubric = null;

    /** Establece la r&uacute;brica de la firma.
     * @param rubric
     *        Imagen de la r&uacute;brica (JPEG en binario). */
    public void setRubric(final byte[] rubric) {
        this.rubric = (rubric != null) ? rubric.clone() : null;
    }

    /** Obtiene el nombre con el que deber&iacute;a guardarse un PDF tras ser
     * firmado. B&aacute;sicamente se le anexa el sufijo <i>.signed</i> al
     * nombre original, manteniendo la extensi&oacute;n (se respetan
     * may&uacute;culas y min&uacute;sculas en esta, pero no se admite una
     * extensi&oacute;n con mezcla de ambas).
     * @param originalName Nombre original del fichero PDF.
     * @return Nombre recomendado para el PDF ya firmado. */
    public static String getSignedName(final String originalName) {
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.endsWith(PDF_FILE_SUFFIX)) { //{
            return originalName.replace(PDF_FILE_SUFFIX, ".signed.pdf"); //$NON-NLS-1$ 
        }
        if (originalName.endsWith(".PDF")) { //$NON-NLS-1$
            return originalName.replace(".PDF", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return originalName + ".signed.pdf"; //$NON-NLS-1$
    }

    /** Si la entrada es un documento PDF, devuelve el mismo documento PDF.
     * @param sign Documento PDF
     * @return Mismo documento PDF de entrada, sin modificar en ning&uacute; aspecto.
     * @throws AOInvalidFormatException Si los datos de entrada no son un documento PDF. */
    public byte[] getData(final byte[] sign) throws AOInvalidFormatException {

        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Devolver el PDF sin firmar
        return sign;
    }

    /** Si la entrada es un documento PDF, devuelve un objeto <code>AOSignInfo</code>
     * con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @param data Documento PDF.
     * @return Objeto <code>AOSignInfo</code> con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @throws AOException Si los datos de entrada no son un documento PDF. */
    public AOSignInfo getSignInfo(final byte[] data) throws AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia que se almacenan en el objeto AOSignInfo
    }

}
