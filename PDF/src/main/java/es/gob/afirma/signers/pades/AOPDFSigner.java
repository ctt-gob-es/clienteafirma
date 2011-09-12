/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.pades;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
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
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.SHA2AltNamesProvider;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSignInfo;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;
import es.gob.afirma.signers.cades.PKCS1ExternalizableSigner;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Clase para la firma electr&oacute;nica de ficheros Adobe PDF.
 * <p>
 * Se mantiene contra iText 2.1.7 por cuestiones de licencia.
 * </p>
 * <p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 * <dt>applySystemDate</dt>
 * <dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario
 * <dt>signReason</dt>
 * <dd>Raz&oacute;n por la que se realiza la firma</dd>
 * <dt>signField</dt>
 * <dd>Nombre del campo en donde insertar la firma</dd>
 * <dt>signatureProductionCity</dt>
 * <dd>Ciudad en la que se realiza la firma</dd>
 * <dt>signerContact</dt>
 * <dd>Contacto del firmante</dd>
 * <dt>signaturePage</dt>
 * <dd>P&aacute;gina del PDF donde insertar la firma</dd>
 * <dt>policyIdentifier</dt>
 * <dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 * <dt>policyQualifier</dt>
 * <dd>OID calificador de la pol&iacute;tica de firma</dd>
 * <dt>ownerPassword</dt>
 * <dd>Contrase&ntilde;a de apertura del PDF (contrase&ntilde;a del propietario) si este estaba cifrado</dd>
 * <dt>headLess</dt>
 * <dd>Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si no se establece o se establece a <code>false</code>
 * act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para
 * los procesos desatendidos y por lotes</dd>
 * <dt>avoidEncryptingSignedPdfs</dt>
 * <dd>Si se establece a <code>true</code> no cifra los PDF firmados aunque el original estuviese firmado, si no se establece o se establece a
 * <code>false</code> los PDF se cifran tras firmarse si el original lo estaba, usando la misma contrase&ntilde;a y opciones que este</dd>
 * <dt>allowSigningCertifiedPdfs</dt>
 * <dd>Si se establece a <code>true</code> permite la firma o cofirma de PDF certificados, si no se establece o se establece a <code>false</code> se
 * lanza una excepci&oacute;n en caso de intentar firmar o cofirmar un PDF certificado. <b>Solo tiene efecto cuando <code>headLess</code> est&aacute;
 * establecido a <code>true</code>, si <code>headLess</code> est&aacute; a <code>false</code> se ignora este par&aacute;metro.</b>
 * </dl>
 * </p> */
public final class AOPDFSigner implements AOSigner {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    public static final int LAST_PAGE = -666;

    /** Mimetype asignado a los ficheros PDF. */
    private static final String MIMETYPE_PDF = "application/pdf"; //$NON-NLS-1$

    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        String signAlgorithm = algorithm;

        if (!algorithm.equals(AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA) && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA)
            && !algorithm.equals(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA)) {

            LOGGER.warning("El algoritmo '" + algorithm + "' no esta soportado por PDF, se usara SHA-1"); //$NON-NLS-1$ //$NON-NLS-2$
            signAlgorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

        if (extraParams == null) {
            extraParams = new Properties();
        }
        try {
            X509Certificate[] certChain = (X509Certificate[]) keyEntry.getCertificateChain(); 
            final PAdESTriPhaseSigner padesTri = new PAdESTriPhaseSigner();
            final byte[] preSignature = padesTri.preSign(
                 AOSignConstants.getDigestAlgorithmName(signAlgorithm), 
                 data, 
                 certChain, 
                 this.rubric, 
                 extraParams
            );
            final byte[] signature = PKCS1ExternalizableSigner.sign(signAlgorithm, keyEntry, preSignature);
            return padesTri.postSign(
                 AOSignConstants.getDigestAlgorithmName(signAlgorithm), 
                 data,
                 certChain, 
                 this.rubric,
                 extraParams,
                 signature, 
                 preSignature
            );
        }
        catch (final InvalidPdfException e) {
            throw new AOFormatFileException("El documento no era un PDF valido"); //$NON-NLS-1$
        }
        catch (final AOException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error firmando el PDF: " + e, e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }

    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros PDF"); //$NON-NLS-1$
    }

    public String getSignedName(final String originalName, final String inText) {

        final String inTextInt = (inText != null ? inText : ""); //$NON-NLS-1$

        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.toLowerCase().endsWith(".pdf")) { //$NON-NLS-1$
            return originalName.substring(0, originalName.length() - 4) + inTextInt + ".pdf"; //$NON-NLS-1$
        }
        return originalName + inTextInt + ".pdf"; //$NON-NLS-1$
    }

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
                }, (pcks7.getSignDate() != null) ? pcks7.getSignDate().getTime() : null);

                // Extraemos el PKCS1 de la firma
                try {
                    // iText antiguo
                    final Field digestField = AOUtil.classForName("com.lowagie.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
                    // iText nuevo
                    //final Field digestField = AOUtil.classForName("com.itextpdf.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
                    digestField.setAccessible(true);
                    pkcs1Object = digestField.get(pcks7);
                }
                catch (final Exception e) {
                    LOGGER.severe(
                                  "No se ha podido obtener informacion de una de las firmas del PDF, se continuara con la siguiente: " + e //$NON-NLS-1$
                    );
                    continue;
                }
                if (pkcs1Object != null && pkcs1Object instanceof byte[]) {
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

    public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
    }

    @SuppressWarnings("unused")
    private boolean isPdfFile(final byte[] data) {

        byte[] buffer = new byte[5];
        try {
            new ByteArrayInputStream(data).read(buffer);
        }
        catch (final Exception e) {
            buffer = null;
        }

        // Comprobamos que cuente con una cabecera PDF
        if (buffer != null && !"%PDF-".equals(new String(buffer))) { //$NON-NLS-1$
            return false;
        }

        try {
            // Si lanza una excepcion al crear la instancia, no es un fichero
            // PDF
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

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
    }

    /** R&uacute;brica de la firma. */
    private byte[] rubric = null;

    /** Establece la r&uacute;brica de la firma.
     * @param rubric
     *        Imagen de la r&uacute;brica (JPEG en binario). */
    public void setRubric(final byte[] rubric) {
        this.rubric = rubric;
    }

    /** Obtiene el nombre con el que deber&iacute;a guardarse un PDF tras ser
     * firmado. B&aacute;sicamente se le anexa el sufijo <i>.signed</i> al
     * nombre original, manteniendo la extensi&oacute;n (se respetan
     * may&uacute;culas y min&uacute;sculas en esta, pero no se admite una
     * extensi&oacute;n con mezcla de ambas)
     * @param originalName
     *        Nombre original del PDF
     * @return Nombre del PDF ya firmado */
    public static String getSignedName(final String originalName) {
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.endsWith(".pdf")) { //$NON-NLS-1${
            return originalName.replace(".pdf", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if (originalName.endsWith(".PDF")) { //$NON-NLS-1$
            return originalName.replace(".PDF", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return originalName + ".signed.pdf"; //$NON-NLS-1$
    }

    public byte[] getData(final byte[] sign) throws AOInvalidFormatException {

        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Devolver el PDF sin firmar
        return sign;
    }

    public AOSignInfo getSignInfo(final byte[] data) throws AOInvalidFormatException, AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
    }

    public String getDataMimeType(final byte[] sign) throws AOUnsupportedSignFormatException {
        if (sign == null) {
            throw new IllegalArgumentException("Los datos de firma introducidos son nulos"); //$NON-NLS-1$
        }

        if (!this.isSign(sign)) {
            throw new AOUnsupportedSignFormatException("La firma introducida no es un fichero de firma PDF"); //$NON-NLS-1$
        }

        return MIMETYPE_PDF;
    }
}
