package es.gob.afirma.signers.pades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDeveloperExtension;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.text.pdf.PdfWriter;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidades variadas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class PdfUtil {

    private static final int DEFAULT_LAYER_2_FONT_SIZE = 12;
    private static final int COURIER = 0;
    private static final int UNDEFINED = -1;
    private static final String BLACK = "black"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String FILTER_ADOBE_PKCS7_DETACHED = "/adbe.pkcs7.detached"; //$NON-NLS-1$

	private static final Set<String> SUPPORTED_SUBFILTERS;
	static {
		SUPPORTED_SUBFILTERS = new HashSet<String>();
		SUPPORTED_SUBFILTERS.add("/ETSI.RFC3161".toLowerCase(Locale.US)); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add(FILTER_ADOBE_PKCS7_DETACHED.toLowerCase(Locale.US));
		SUPPORTED_SUBFILTERS.add("/ETSI.CAdES.detached".toLowerCase(Locale.US)); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add("/adbe.pkcs7.sha1".toLowerCase(Locale.US)); //$NON-NLS-1$
	}

	private PdfUtil() {
		// No instanciable
	}

	private static final String LAYERTEXT_TAG_DELIMITER = "$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_DATE_PREFIX = LAYERTEXT_TAG_DELIMITER + "SIGNDATE"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_DATE_DELIMITER = "="; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SUBJECTCN = "$$SUBJECTCN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_ISSUERCN = "$$ISSUERCN$$"; //$NON-NLS-1$

	static String getLayerText(final String txt, final X509Certificate cert, final Calendar signDate) {
		if (txt == null) {
			return null;
		}
		String ret = cert == null ?
			txt :
				txt.replace(LAYERTEXT_TAG_SUBJECTCN, AOUtil.getCN(cert))
				   .replace(LAYERTEXT_TAG_ISSUERCN, AOUtil.getCN(cert.getIssuerX500Principal().getName()));
		if (txt.contains(LAYERTEXT_TAG_DATE_PREFIX)) {
			final int strIdx = txt.indexOf(LAYERTEXT_TAG_DATE_PREFIX);
			final String sdTag = txt.substring(
				strIdx,
				txt.indexOf(LAYERTEXT_TAG_DELIMITER, strIdx + LAYERTEXT_TAG_DATE_PREFIX.length()) + LAYERTEXT_TAG_DELIMITER.length()
			);
			String date;
			final Date tbpDate = signDate != null ? signDate.getTime() : new Date();
			if (sdTag.contains(LAYERTEXT_TAG_DATE_DELIMITER)) {
				final String dateFormat = sdTag.replace(LAYERTEXT_TAG_DELIMITER, "").split(LAYERTEXT_TAG_DATE_DELIMITER)[1]; //$NON-NLS-1$
				try {
					date = new SimpleDateFormat(dateFormat).format(
							tbpDate
					);
				}
				catch(final Exception e) {
					LOGGER.warning(
						"Patron incorrecto para la fecha de firma en la firma visible (" + dateFormat + "), se usara el por defecto: " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
					date = new SimpleDateFormat().format(
						tbpDate
					);
				}
			}
			else {
				date = new SimpleDateFormat().format(
					tbpDate
				);
			}
			ret = ret.replace(sdTag, date);
		}
		return ret;
	}

	static PdfReader getPdfReader(final byte[] inPDF,
			                      final Properties extraParams,
			                      final boolean headLess) throws BadPdfPasswordException,
			                                                     InvalidPdfException,
			                                                     IOException {
		// Contrasena del propietario del PDF
		final String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$

		// Contrasena del usuario del PDF
		final String userPassword =  extraParams.getProperty("userPassword"); //$NON-NLS-1$

		PdfReader pdfReader;
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
			// Comprobamos que el signer esta en modo interactivo, y si no lo
			// esta no pedimos contrasena por dialogo, principalmente para no interrumpir un firmado por lotes
			// desatendido
			if (headLess) {
				throw new BadPdfPasswordException(e);
			}
			// La contrasena que nos han proporcionada no es buena o no nos
			// proporcionaron ninguna
			final String ownerPwd = new String(
				AOUIFactory.getPassword(
					ownerPassword == null ? CommonPdfMessages.getString("AOPDFSigner.0") : CommonPdfMessages.getString("AOPDFSigner.1"), //$NON-NLS-1$ //$NON-NLS-2$
					null
				)
			);
			if ("".equals(ownerPwd)) { //$NON-NLS-1$
                throw new AOCancelledOperationException(
                    "Entrada de contrasena de PDF cancelada por el usuario", e //$NON-NLS-1$
                );
			}
			try {
				pdfReader = new PdfReader(inPDF, ownerPwd.getBytes());
			}
			catch (final BadPasswordException e2) {
				throw new BadPdfPasswordException(e2);
			}
			extraParams.put("ownerPassword", ownerPwd); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new InvalidPdfException(e);
		}
		return pdfReader;

	}

	static void checkPdfCertification(final int pdfCertificationLevel, final Properties extraParams) throws PdfIsCertifiedException {
		if (pdfCertificationLevel != PdfSignatureAppearance.NOT_CERTIFIED &&
				!Boolean.parseBoolean(extraParams.getProperty("allowSigningCertifiedPdfs"))) { //$NON-NLS-1$
			// Si no permitimos dialogos graficos o directamente hemos indicado que no permitimos firmar PDF certificados lanzamos
			// una excepcion
			if (Boolean.parseBoolean(extraParams.getProperty("headLess")) || "false".equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) {  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				throw new PdfIsCertifiedException();
			}
			// En otro caso, perguntamos al usuario
			if (AOUIFactory.NO_OPTION == AOUIFactory.showConfirmDialog(
				null,
				CommonPdfMessages.getString("AOPDFSigner.8"), //$NON-NLS-1$
				CommonPdfMessages.getString("AOPDFSigner.9"), //$NON-NLS-1$
				AOUIFactory.YES_NO_OPTION,
				AOUIFactory.WARNING_MESSAGE)
			) {
				throw new AOCancelledOperationException("El usuario no ha permitido la firma de un PDF certificado"); //$NON-NLS-1$
			}
			extraParams.setProperty("allowSigningCertifiedPdfs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	static void enableLtv(final PdfStamper stp) {
		// PAdES parte 3 seccion 4.7 - Habilitacion para LTV
		stp.getWriter().addDeveloperExtension(
			new PdfDeveloperExtension(
				new PdfName("ESIC"), //$NON-NLS-1$
				PdfWriter.PDF_VERSION_1_7,
				1
			)
		);
	}

	static boolean getAppendMode(final Properties extraParams, final PdfReader pdfReader) {
		if (extraParams.getProperty("ownerPassword") != null || extraParams.getProperty("userPassword") != null) { //$NON-NLS-1$ //$NON-NLS-2$
			return true;
		}
		return Boolean.parseBoolean(extraParams.getProperty("alwaysCreateRevision")) || pdfReader.getAcroFields().getSignatureNames().size() > 0; //$NON-NLS-1$
	}

	static boolean pdfHasUnregisteredSignatures(final byte[] pdf, final Properties xParams) throws InvalidPdfException, BadPdfPasswordException, IOException {
		final Properties extraParams = xParams != null ? xParams : new Properties();
		final PdfReader pdfReader = PdfUtil.getPdfReader(
			pdf,
			extraParams,
			Boolean.parseBoolean(extraParams.getProperty("headLess")) //$NON-NLS-1$
		);
		return pdfHasUnregisteredSignatures(pdfReader);
	}

	static String getFirstSupportedSignSubFilter(final byte[] pdf, final Properties xParams) throws IOException,
	                                                                                                InvalidPdfException,
	                                                                                                BadPdfPasswordException {
		if (pdf == null) {
			throw new IllegalArgumentException("El PDF de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final PdfReader pdfReader = PdfUtil.getPdfReader(
			pdf,
			extraParams,
			Boolean.parseBoolean(extraParams.getProperty("headLess")) //$NON-NLS-1$
		);

    	for (int i = 0; i < pdfReader.getXrefSize(); i++) {
    		final PdfObject pdfobj = pdfReader.getPdfObject(i);
    		if (pdfobj != null && pdfobj.isDictionary()) {
    			final PdfDictionary d = (PdfDictionary) pdfobj;
    			if (PdfName.SIG.equals(d.get(PdfName.TYPE))) {

    				final String subFilter = d.get(PdfName.SUBFILTER) != null ?
						d.get(PdfName.SUBFILTER).toString().toLowerCase(Locale.US) : null;

					if (SUPPORTED_SUBFILTERS.contains(subFilter)) {
						return subFilter;
					}

    			}
    		}
    	}

    	LOGGER.info("No se ha encontrado ningun filtro de firma soportado, se devolvera " + FILTER_ADOBE_PKCS7_DETACHED); //$NON-NLS-1$
		return FILTER_ADOBE_PKCS7_DETACHED;
	}

	static boolean pdfHasUnregisteredSignatures(final PdfReader pdfReader) {

		boolean ret = false;
    	for (int i = 0; i < pdfReader.getXrefSize(); i++) {
    		final PdfObject pdfobj = pdfReader.getPdfObject(i);
    		if (pdfobj != null && pdfobj.isDictionary()) {
    			final PdfDictionary d = (PdfDictionary) pdfobj;
    			if (PdfName.SIG.equals(d.get(PdfName.TYPE))) {

    				final String subFilter = d.get(PdfName.SUBFILTER) != null ?
    						d.get(PdfName.SUBFILTER).toString().toLowerCase(Locale.US) : null;

    				if (subFilter == null || !SUPPORTED_SUBFILTERS.contains(subFilter)) {
    					ret = true;
	    				try {
	    					final PdfObject o = d.get(PdfName.CERT);
	    					final byte[] data;
	    					if (o instanceof PdfString) {
	    						data = ((PdfString)o).getOriginalBytes();
	    					}
	    					else {
	    						data = ((PdfString) ((PdfArray) d.get(PdfName.CERT)).getArrayList().get(0)).getOriginalBytes();
	    					}

							final X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
								new ByteArrayInputStream(
									data
								)
							);
							LOGGER.info(
								"Encontrada firma no registrada, hecha con certificado emitido por: " + cert.getIssuerX500Principal().toString() //$NON-NLS-1$
							);
						}
	    				catch (final Exception e) {
							LOGGER.warning("No se ha podido comprobar la identidad de una firma no registrada con el subfiltro: " + subFilter + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
						}
    				}
    			}
    		}
    	}
    	return ret;
	}

	private static final class ColorValues {

		private final int r;
		private final int g;
		private final int b;

		ColorValues(final int red, final int green, final int blue) {
			this.r = red;
			this.g = green;
			this.b = blue;
		}

		int getR() {
			return this.r;
		}

		int getG() {
			return this.g;
		}

		int getB() {
			return this.b;
		}
	}

	private static final Map<String, ColorValues> COLORS = new HashMap<String, ColorValues>(7);
	static {
		COLORS.put(BLACK      , new ColorValues(0,     0,   0));
		COLORS.put("white"    , new ColorValues(255, 255, 255)); //$NON-NLS-1$
		COLORS.put("lightGray", new ColorValues(192, 192, 192)); //$NON-NLS-1$
		COLORS.put("gray"     , new ColorValues(128, 128, 128)); //$NON-NLS-1$
		COLORS.put("darkGray" , new ColorValues(64,   64,  64)); //$NON-NLS-1$
		COLORS.put("red"      , new ColorValues(255,   0,   0)); //$NON-NLS-1$
		COLORS.put("pink"     , new ColorValues(255, 175, 175)); //$NON-NLS-1$
	}

	static com.lowagie.text.Font getFont(final int fontFamily,
										 final int fontSize,
										 final int fontStyle,
			                             final String fontColor) {

		final String colorName = fontColor != null ? fontColor.toLowerCase() : BLACK;

		final ColorValues cv = COLORS.get(colorName) != null ? COLORS.get(colorName) : COLORS.get(BLACK);

		try {
			Class<?> colorClass;
			if (Platform.getOS() == OS.ANDROID) {
				colorClass = Class.forName("harmony.java.awt.Color"); //$NON-NLS-1$
			}
			else {
				colorClass = Class.forName("java.awt.Color"); //$NON-NLS-1$
			}
			final Object color = colorClass.getConstructor(Integer.TYPE, Integer.TYPE, Integer.TYPE).newInstance(
				Integer.valueOf(cv.getR()),
				Integer.valueOf(cv.getG()),
				Integer.valueOf(cv.getB())
			);

			return com.lowagie.text.Font.class
				.getConstructor(Integer.TYPE, Float.TYPE, Integer.TYPE, colorClass)
					.newInstance(
						// Family (COURIER = 0, HELVETICA = 1, TIMES_ROMAN = 2, SYMBOL = 3, ZAPFDINGBATS = 4)
						Integer.valueOf(fontFamily == UNDEFINED ? COURIER : fontFamily),
						// Size (DEFAULTSIZE = 12)
						Float.valueOf(fontSize == UNDEFINED ? DEFAULT_LAYER_2_FONT_SIZE : fontSize),
						// Style (NORMAL = 0, BOLD = 1, ITALIC = 2, BOLDITALIC = 3, UNDERLINE = 4, STRIKETHRU = 8)
						Integer.valueOf(fontStyle == UNDEFINED ? com.lowagie.text.Font.NORMAL : fontStyle),
						// Color
						color
			);
		}
		catch (final Exception e) {
			return new com.lowagie.text.Font(
				// Family (COURIER = 0, HELVETICA = 1, TIMES_ROMAN = 2, SYMBOL = 3, ZAPFDINGBATS = 4)
				fontFamily == UNDEFINED ? COURIER : fontFamily,
				// Size (DEFAULTSIZE = 12)
				fontSize == UNDEFINED ? DEFAULT_LAYER_2_FONT_SIZE : fontSize,
				// Style (NORMAL = 0, BOLD = 1, ITALIC = 2, BOLDITALIC = 3, UNDERLINE = 4, STRIKETHRU = 8)
				fontStyle == UNDEFINED ? com.lowagie.text.Font.NORMAL : fontStyle,
				// Color
				null
			);
		}
	}
}
