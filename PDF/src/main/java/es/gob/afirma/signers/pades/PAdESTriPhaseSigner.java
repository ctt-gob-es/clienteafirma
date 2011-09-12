package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
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
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;

class PAdESTriPhaseSigner {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$
    
    private static final int CSIZE = 8000;
    
    // Atributos que hay que conservar de prefirma a postfirma
    private ByteArrayOutputStream baos;
    private PdfSignatureAppearance sap;

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    public static final int LAST_PAGE = -666;
    
    byte[] preSign(final String digestAlgorithmName,
                          final byte[] inPDF, 
                          final X509Certificate[] signerCertificateChain,
                          final byte[] rubricJpegImage,
                          Properties extraParams) throws IOException, AOException {
        
        // Lectura de parametros adicionales
        final boolean useSystemDateTime = Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true")); //$NON-NLS-1$ //$NON-NLS-2$
        final String reason = extraParams.getProperty("signReason"); //$NON-NLS-1$
        final String signField = extraParams.getProperty("signField"); //$NON-NLS-1$
        final String signatureProductionCity = extraParams.getProperty("signatureProductionCity"); //$NON-NLS-1$
        final String signerContact = extraParams.getProperty("signerContact"); //$NON-NLS-1$
        final String policyIdentifier = extraParams.getProperty("policyIdentifier"); //$NON-NLS-1$
        final String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$
        int page = 1;
        try {
            page = Integer.parseInt(extraParams.getProperty("signaturePage")); //$NON-NLS-1$
        }
        catch (final Exception e) { 
            /* Se deja la pagina tal y como esta */
        }

        // Comprobaciones de contrasena en el PDF
        final PdfReader pdfReader;
        final String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$
        try {
            if (ownerPassword == null) {
                pdfReader = new PdfReader(inPDF);
            }
            else {
                pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
            }
        }
        catch (final BadPasswordException e) {
            throw new AOException("La contrasena proporcionada no es valida (o no se ha proporcionado ninguna) para el PDF actual", e); //$NON-NLS-1$
        }

        // Comprobaciones de certificacion en el PDF
        if (pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED) {
            if (!"true".equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$ //$NON-NLS-2$
                throw new AOException("No se permite la firma de PDF certificados (el paramtro allowSigningCertifiedPdfs estaba establecido a false)"); //$NON-NLS-1$
            }
        }

        // Comprobaciones de adjuntos en los PDF
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
                            }
                        }
                    }
                }
            }
        }

        // Comprobaciones de empotrados en los PDF
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
                            }
                        }
                    }
                }
            }
        }

        // Los derechos van firmados por Adobe, y como desde iText se invalidan
        // es mejor quitarlos
        pdfReader.removeUsageRights();

        this.baos = new ByteArrayOutputStream();

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
                  this.baos, // Salida
                  '\0', // Mantener version
                  null, // No crear temporal
                  pdfReader.getAcroFields().getSignatureNames().size() > 0 // Si hay mas firmas, creo una revision
            );
        }
        catch(final Exception e) {
            throw new AOException("Error al crear el campo de firma en el PDF", e); //$NON-NLS-1$
        }
        
        // Aplicamos todos los atributos de firma
        this.sap = stp.getSignatureAppearance();
        stp.setFullCompression();
        sap.setAcro6Layers(true);
        sap.setLayer2Text(""); //$NON-NLS-1$
        sap.setLayer4Text(""); //$NON-NLS-1$
        
        // iText antiguo
        sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        // iText nuevo
        // sap.setRenderingMode(PdfSignatureAppearance.RenderingMode.NAME_AND_DESCRIPTION);
        
        if (reason != null) {
            sap.setReason(reason);
        }
        if (useSystemDateTime) {
            sap.setSignDate(new GregorianCalendar());
        }

        if (pdfReader.isEncrypted() && ownerPassword != null) {
            if ("true".equalsIgnoreCase(extraParams.getProperty("avoidEncryptingSignedPdfs"))) { //$NON-NLS-1$ //$NON-NLS-2$
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
        if (rubricJpegImage != null) {
            try {
                this.sap.setImage(new Jpeg(rubricJpegImage));
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
            dic.setDate(new PdfDate(sap.getSignDate()));
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
        exc.put(PdfName.CONTENTS, new Integer(CSIZE * 2 + 2));
        
        try {
            this.sap.preClose(exc);
        }
        catch (final Exception e) {
            throw new AOException("No se ha podido cerrar el conjunto de atributos de la firma PDF", e); //$NON-NLS-1$
        }
        
        // Calculamos el MessageDigest
        final MessageDigest md;
        try {
            md = MessageDigest.getInstance(digestAlgorithmName);
        }
        catch (NoSuchAlgorithmException e) {
            throw new AOException("El algoritmo de huella digital no es valido", e); //$NON-NLS-1$
        }
        md.update(AOUtil.getDataFromInputStream(this.sap.getRangeStream()));
        
        // Prefirma CAdES
        return CAdESTriPhaseSigner.preSign(
            digestAlgorithmName, 
            null, 
            signerCertificateChain, 
            policyIdentifier, 
            (policyQualifier != null) ? policyQualifier.replace("urn:oid:", "").replace("URN:oid:", "").replace("Urn:oid:", "") : null, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$, 
            true, 
            md.digest()
        );
    }

    /**
     * Post-firma en PAdES.
     * @param digestAlgorithmName Nombre del algoritmo de huella digital usado para la firma
     * @param signerCertificateChain Cadena de certificados del firmante
     * @param signature Resultado de la firma PKCS#1 v1.5
     * @param signedAttributes Resultado de la pre-firma CAdES (PAdES)
     * @return PDF firmado
     * @throws AOException en caso de cualquier tipo de error
     */
    byte[] postSign(final String digestAlgorithmName,
                                  final X509Certificate[] signerCertificateChain,
                                  final byte[] signature,
                                  final byte[] signedAttributes
                      ) throws AOException {
        
        final byte[] completeCAdESSignature = CAdESTriPhaseSigner.postSign(digestAlgorithmName, null, signerCertificateChain, signature, signedAttributes);
        final byte[] outc = new byte[CSIZE];

        final PdfDictionary dic2 = new PdfDictionary();
        System.arraycopy(completeCAdESSignature, 0, outc, 0, completeCAdESSignature.length);
        dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));
        try {
            this.sap.close(dic2);
        }
        catch (final Exception e) {
            throw new AOException("Error al cerrar el PDF para finalizar el proceso de firma", e); //$NON-NLS-1$
        }
        return this.baos.toByteArray();
        
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
