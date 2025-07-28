/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.aowagie.text.Rectangle;
import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfArray;
import com.aowagie.text.pdf.PdfDeveloperExtension;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfObject;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;
import com.aowagie.text.pdf.PdfString;
import com.aowagie.text.pdf.PdfWriter;

import es.gob.afirma.core.AOException;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;

/** Utilidades variadas para el tratamiento de PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String FILTER_ADOBE_PKCS7_DETACHED = "/adbe.pkcs7.detached"; //$NON-NLS-1$

	private static final String RANGE_INDICATOR = "-"; //$NON-NLS-1$

	private static final String RANGE_SEPARATOR = ","; //$NON-NLS-1$

    /** Valor en el par&aacute;metro signaturePage o signaturePages que
     * indica una nueva p&aacute;gina para agregar al final del documento*/
    private static final String APPEND_PAGE = "append"; //$NON-NLS-1$

    /** N&uacute;mero que indica una p&aacute;gina nueva. */
    private static final int NEW_PAGE = 0;

    /** Valor en el par&aacute;metro signaturePage o signaturePages que
     * indica que se estampar&aacute; la firma visible en todas las paginas*/
    private static final String ALL_PAGES = "all"; //$NON-NLS-1$

	private static final Set<String> SUPPORTED_SUBFILTERS;
	static {
		SUPPORTED_SUBFILTERS = new HashSet<>();
		SUPPORTED_SUBFILTERS.add("/ETSI.RFC3161"); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add(FILTER_ADOBE_PKCS7_DETACHED);
		SUPPORTED_SUBFILTERS.add("/ETSI.CAdES.detached"); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add("/adbe.pkcs7.sha1"); //$NON-NLS-1$
	}

	private PdfUtil() {
		// No instanciable
	}

	/** Indica si un PDF es de tipo PDF-A1.
	 * @param metadata Metadatos XMP del PDF.
	 * @return <code>true</code> si el PDF es de tipo PDF-A1,
	 *         <code>false</code> en caso contrario. */
	static boolean isPdfA1(final byte[] metadata) {
		if (metadata == null) {
			return false;
		}
		final String rdf = new String(metadata);
		return rdf.replace("\n", "") //$NON-NLS-1$ //$NON-NLS-2$
					.replace("\r", "") //$NON-NLS-1$ //$NON-NLS-2$
					  .replace("\t", "") //$NON-NLS-1$ //$NON-NLS-2$
					    .replace(" ", "") //$NON-NLS-1$ //$NON-NLS-2$
					      .contains("<pdfaid:part>1</pdfaid:part>"); //$NON-NLS-1$
	}

	static boolean isPdfAx(final byte[] metadata) {
		if (metadata == null) {
			return false;
		}
		final String rdf = new String(metadata);
		return rdf.replace("\n", "") //$NON-NLS-1$ //$NON-NLS-2$
					.replace("\r", "") //$NON-NLS-1$ //$NON-NLS-2$
					  .replace("\t", "") //$NON-NLS-1$ //$NON-NLS-2$
					    .replace(" ", "") //$NON-NLS-1$ //$NON-NLS-2$
					      .contains("<pdfaid:part>"); //$NON-NLS-1$
	}

	static GregorianCalendar getSignTime(final String stStr) {
		if (stStr == null) {
			return new GregorianCalendar();
		}

		final Date date;
		final SimpleDateFormat formatter = new SimpleDateFormat("yyyy:MM:dd:HH:mm:ss"); //$NON-NLS-1$
		try {
			date = formatter.parse(stStr);
		}
		catch(final Exception e) {
			LOGGER.severe(
				"La fecha indicada ('" + stStr + "') como momento de firma para PAdES no sigue el patron 'yyyy:MM:dd:HH:mm:ss': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return new GregorianCalendar();
		}

		// Creamos el calendar a partir de un Date para que tome de el
		final GregorianCalendar calendar = new GregorianCalendar();
		calendar.setTime(date);

		return calendar;
	}

	/** Obtiene el lector iText de PDF.
	 * @param inPDF PDF de entrada.
	 * @param xParams Par&aacute;metros adicionales.
	 * @param headless Si se establece a <code>true</code> se evita cualquier di&aacute;logo
	 *                 gr&aacute;fico.
	 * @return Lector iText de PDF.
	 * @throws PdfIsPasswordProtectedException Si el PDF estaba protegido con contrase&ntilde;a y
	 *                                 esta no se proporcion&oacute;
	 * @throws BadPdfPasswordException Si el PDF estaba protegido con contrase&ntilde;a y
	 *                                 se indic&oacute; una incorrecta.
	 * @throws InvalidPdfException Si el PDF era inv&aacute;lido o estaba corrupto.
	 * @throws IOException Si hay errores en la lectura o escritura de datos.
	 *  */
	public static PdfReader getPdfReader(final byte[] inPDF,
			                             final Properties xParams,
			                             final boolean headless) throws PdfIsPasswordProtectedException,
																		BadPdfPasswordException,
			                                                            InvalidPdfException,
			                                                            IOException {

		final Properties extraParams = xParams != null ? xParams : new Properties();

		// Contrasena del propietario del PDF
		final String ownerPassword = extraParams.getProperty(PdfExtraParams.OWNER_PASSWORD_STRING);

		// Contrasena del usuario del PDF
		final String userPassword =  extraParams.getProperty(PdfExtraParams.USER_PASSWORD_STRING);

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
			// Devolvemos una excepcion u otra segun si se nos proporciono
			// contrasena o no
			if (ownerPassword != null || userPassword != null) {
				throw new BadPdfPasswordException("Se ha indicado una contrasena incorrecta para el PDF", e); //$NON-NLS-1$
			}
			throw new PdfIsPasswordProtectedException("El PDF esta protegido por contrasena para lectura", e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new InvalidPdfException(e);
		}
		return pdfReader;
	}

	/**
	 * Comprueba si un PDF esta certificados y se pueden agregar firmas a &eacute;l.
	 * @param pdfCertificationLevel Nivel de certificaci&oacute;n.
	 * @param extraParams Configuraci&oacute;n establecida para la operaci&oacute;n.
	 * @throws PdfIsCertifiedException Cuando el PDF esta certificado y se requiere
	 * autorizaci&oacute;n del usuario para firmarlo.
	 * @throws AOException Cuando el PDF est&aacute; certificado y no se permite su firma.
	 */
	static void checkPdfCertification(final int pdfCertificationLevel, final Properties extraParams) throws PdfIsCertifiedException, AOException {

		// Si se ha configurado que se firme en cualquier caso, no hacemos mas comprobaciones
		final String forceSignature = extraParams.getProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS);
		if (forceSignature != null && Boolean.parseBoolean(forceSignature)) {
			return;
		}

		// Identificamos si el PDF permite firmas
		final boolean signAllowed = pdfCertificationLevel != PdfSignatureAppearance.CERTIFIED_NO_CHANGES_ALLOWED;

		// Si no se permiten firmas y no se establecio ningun comportamiento, usamos una excepcion
		// que permita a la aplicacion identificar el problema y dar la opcion de firmar la firma
		// (PdfIsCertifiedException). Si no se permiten y se establecio un comportamiento (que ya
		// sabemos que no fue forzar las firmas), lanzamos un error definitivo (AOException).
		if (!signAllowed) {
			if (forceSignature == null) {
				throw new PdfIsCertifiedException("El PDF esta certificado"); //$NON-NLS-1$
			}
			throw new AOException("El PDF esta certificado y se configuro que no se admitia su firma"); //$NON-NLS-1$
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
		if (extraParams.getProperty(PdfExtraParams.OWNER_PASSWORD_STRING) != null ||
				extraParams.getProperty(PdfExtraParams.USER_PASSWORD_STRING) != null) {
			return true;
		}
		return Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.ALWAYS_CREATE_REVISION)) || pdfReader.getAcroFields().getSignatureNames().size() > 0;
	}

	static boolean pdfHasUnregisteredSignatures(final byte[] pdf, final Properties xParams)
			throws InvalidPdfException, PdfIsPasswordProtectedException, BadPdfPasswordException, IOException {
		final Properties extraParams = xParams != null ? xParams : new Properties();
		final PdfReader pdfReader = PdfUtil.getPdfReader(
			pdf,
			extraParams,
			Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS))
		);
		return pdfHasUnregisteredSignatures(pdfReader);
	}

	/** Obtiene el primer filtro de firma obtenido de un documento PDF.
	 * Si no se encuentra ninguno, devuelve {@code null}.
	 * @param pdf PDF que analizar.
	 * @param xParams Par&aacute;metros extra con la configuraci&oacute;n de la operaci&oacute;n.
	 * @return Filtro de firma o {@code null} si no se encuentra.
	 * @throws IOException Cuando ocurre un error al leer el PDF.
	 * @throws InvalidPdfException Cuando los datos proporcionados no son un PDF.
	 * @throws BadPdfPasswordException Cuando se ha insertado una contrase&ntilde;a err&oacute;nea en el PDF. */
	static String getFirstSupportedSignSubFilter(final byte[] pdf, final Properties xParams) throws IOException,
	                                                                                                InvalidPdfException,
	                                                                                                PdfIsPasswordProtectedException,
	                                                                                                BadPdfPasswordException {
		if (pdf == null) {
			throw new IllegalArgumentException("El PDF de entrada no puede ser nulo"); //$NON-NLS-1$
		}

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final PdfReader pdfReader = PdfUtil.getPdfReader(
			pdf,
			extraParams,
			Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS))
		);

    	for (int i = 0; i < pdfReader.getXrefSize(); i++) {
    		final PdfObject pdfobj = pdfReader.getPdfObject(i);
    		if (pdfobj != null && pdfobj.isDictionary()) {
    			final PdfDictionary d = (PdfDictionary) pdfobj;
    			if (PdfName.SIG.equals(d.get(PdfName.TYPE))) {

    				final String subFilter = d.get(PdfName.SUBFILTER) != null ?
						d.get(PdfName.SUBFILTER).toString() : null;

					if (SUPPORTED_SUBFILTERS.contains(subFilter)) {
						return subFilter;
					}
    			}
    		}
    	}

    	LOGGER.info("No se ha encontrado ningun filtro de firma soportado, se devolvera null"); //$NON-NLS-1$
		return null;
	}

	static boolean pdfHasUnregisteredSignatures(final PdfReader pdfReader) {

		boolean ret = false;
    	for (int i = 0; i < pdfReader.getXrefSize(); i++) {
    		final PdfObject pdfobj = pdfReader.getPdfObject(i);
    		if (pdfobj != null && pdfobj.isDictionary()) {
    			final PdfDictionary d = (PdfDictionary) pdfobj;
    			if (PdfName.SIG.equals(d.get(PdfName.TYPE))) {

    				final String subFilter = d.get(PdfName.SUBFILTER) != null ?
    						d.get(PdfName.SUBFILTER).toString() : null;

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

	/** Campo de firma dentro de un PDF. */
	public static final class SignatureField {

		private final int signaturePositionOnPageLowerLeftX;
	    private final int signaturePositionOnPageLowerLeftY;
	    private final int signaturePositionOnPageUpperRightX;
	    private final int signaturePositionOnPageUpperRightY;
	    private final int page;
	    private final String name;

	    /** Crea un campo de firma para un PDF.
	     * @param pg P&aacute;gina del campo de firma.
	     * @param llx Coordenada horizontal inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @param lly Coordenada vertical inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @param urx Coordenada horizontal superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @param ury Coordenada vertical superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @param n Nombre del campo de firma. */
	    public SignatureField(final int pg, final int llx, final int lly, final int urx, final int ury, final String n) {
	    	if (n == null) {
	    		throw new IllegalArgumentException(
    				"El campo de firma debe tener un nombre no nulo" //$NON-NLS-1$
				);
	    	}
	    	this.signaturePositionOnPageLowerLeftX = llx;
	        this.signaturePositionOnPageLowerLeftY = lly;
	        this.signaturePositionOnPageUpperRightX = urx;
	        this.signaturePositionOnPageUpperRightY = ury;
	        this.page = pg;
	        this.name = n;
	    }

	    @Override
		public String toString() {
	    	return this.name;
	    }

	    /** Obtiene el nombre del campo de firma.
	     * @return Nombre del campo de firma. */
	    public String getName() {
	    	return this.name;
	    }

	    /** Obtiene el n&uacute;mero de p&aacute;gina en el que est&aacute; el campo de firma.
	     * @return N&uacute;mero de p&aacute;gina en el que est&aacute; el campo de firma (empezando desde 1). */
	    public int getPage() {
	    	return this.page;
	    }

	    /** Obtiene la coordenada horizontal superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @return Coordenada horizontal superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina. */
	    public int getSignaturePositionOnPageUpperRightX() {
	    	return this.signaturePositionOnPageUpperRightX;
	    }

	    /** Obtiene la coordenada vertical superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @return Coordenada vertical superior derecha de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina. */
	    public int getSignaturePositionOnPageUpperRightY() {
	    	return this.signaturePositionOnPageUpperRightY;
	    }

	    /** Obtiene la Coordenada vertical inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @return Coordenada vertical inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina. */
	    public int getSignaturePositionOnPageLowerLeftY() {
	    	return this.signaturePositionOnPageLowerLeftY;
	    }

	    /** Obtiene la Coordenada horizontal inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina.
	     * @return Coordenada horizontal inferior izquierda de la posici&oacute;n del recuadro visible de la firma dentro de la p&aacute;gina. */
	    public int getSignaturePositionOnPageLowerLeftX() {
	    	return this.signaturePositionOnPageLowerLeftX;
	    }

	}

	/** Obtiene los campos de firma vac&iacute;os de un PDF.
	 * @param pdf PDF de entrada.
	 * @return Campos de firma vac&iacute;os del PDF proporcionado. */
	public static List<SignatureField> getPdfEmptySignatureFields(final byte[] pdf) {
		if (pdf == null) {
			return new ArrayList<>(0);
		}
		final PdfReader reader;
		try {
			reader = new PdfReader(pdf);
		}
		catch (final Exception e) {
			LOGGER.severe(
				"Error leyendo el PDF de entrada: " + e //$NON-NLS-1$
			);
			return new ArrayList<>(0);
		}
		final AcroFields fields = reader.getAcroFields();
		if (fields != null) {
			final List<String> emptySignatureFields = fields.getBlankSignatureNames();
			final List<SignatureField> ret = new ArrayList<>();
			if (emptySignatureFields != null) {
				for(final String signame : emptySignatureFields) {
					final float[] positions = fields.getFieldPositions(signame);
					if (positions == null || positions.length < 5) {
						continue;
					}
					ret.add(
						new SignatureField(
							Math.round(positions[0]),
							Math.round(positions[1]),
							Math.round(positions[2]),
							Math.round(positions[3]),
							Math.round(positions[4]),
							signame
						)
					);
				}
				return ret;
			}
		}
		return new ArrayList<>(0);
	}

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse el elemento
     * gr&aacute;fico indicado como prefijo. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a arriba.
     * @param extraParams Definici&oacute;n de las coordenadas como conjunto de propiedades
     * @param prefix Prefijo de las propiedades de coordenada en el conjunto
     * @return Rect&aacute;ngulo que define una posici&oacute;n de un elemento en una p&aacute;gina del PDF */
    public static com.aowagie.text.Rectangle getPositionOnPage(final Properties extraParams, final String prefix) {
	    	if (extraParams == null || prefix == null) {
	    		LOGGER.severe("Se ha pedido una posicion para un elemento grafico nulo"); //$NON-NLS-1$
	    		return null;
	    	}
	    	if (extraParams.getProperty(prefix + "PositionOnPageLowerLeftX") != null //$NON-NLS-1$
	    			&& extraParams.getProperty(prefix + "PositionOnPageLowerLeftY") != null //$NON-NLS-1$
	    			&& extraParams.getProperty(prefix + "PositionOnPageUpperRightX") != null //$NON-NLS-1$
	    			&& extraParams.getProperty(prefix + "PositionOnPageUpperRightY") != null) { //$NON-NLS-1$
	    		try {
	    			return new com.aowagie.text.Rectangle(
	    					Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageLowerLeftX").trim()), //$NON-NLS-1$
	    					Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageLowerLeftY").trim()), //$NON-NLS-1$
	    					Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageUpperRightX").trim()), //$NON-NLS-1$
	    					Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageUpperRightY").trim()) //$NON-NLS-1$
	    					);
	    		}
	    		catch (final Exception e) {
	    			LOGGER.severe(
	    					"Se ha indicado una posicion invalida para el elemento grafico '" + prefix + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
	    					);
	    		}
	    	}
	    	return null;
    }



    /**
     * Carga en el listado de p&aacute;ginas las p&aacute;ginas indicadas en una cadena, que puede
     * ser un n&uacute;mero de p&aacute;gina o un rango de ellas. Las p&aacute;ginas se enumeran
     * desde 1 y se pueden referenciar desde el final mediante n&uacute;meros negativos, donde la
     * &uacute;ltima p&aacute;gina ser&iacute;a -1. Cualquier n&uacute;mero que exceda el
     * n&uacute;mero de p&aacute;ginas ser&aacute; se interpretar&aacute; como la &uacute;ltima
     * p&aacute;gina si era positivo o la primera si era negativo. Los rangos se indican con guion
     * ('-').
     * @param pageStr P&aacute;gina o rango de p&aacute;ginas a tratar.
     * @param totalPages N&uacute;mero total de p&aacute;ginas del documento.
     * @param pagesList Lista de enteros donde se indican las p&aacute;ginas una a una.
     * @throws IncorrectPageException Cuando la cadena no es v&aacute;lida.
     */
    public static void getPagesRange(final String pageStr, final int totalPages, final List<Integer> pagesList) throws IncorrectPageException {


    	final String range = pageStr.trim().replace(" ", ""); //$NON-NLS-1$ //$NON-NLS-2$

    	// Intentamos cargar la cadena como si fuese el numero de pagina.
    	// Si falla, intentaremos hacerlo como si fuese un rango de paginas.
    	int page;
    	try {
    		page = normalizePage(range, totalPages);
    	} catch (final NumberFormatException nfe) {
    		page = -1;
    	}

    	if (page > 0) {
    		addPageToResult(page, totalPages, pagesList);
    	}
    	else {
    		addRangeToResult(range, totalPages, pagesList);
    	}
    }

	/**
     * Traduce un texto a n&uacute;mero de p&aacute;gina. Las p&aacute;ginas se enumeran
     * desde 1 y se pueden referenciar desde el final mediante n&uacute;meros negativos, donde la
     * &uacute;ltima p&aacute;gina ser&iacute;a -1. Cualquier n&uacute;mero que exceda el
     * n&uacute;mero de p&aacute;ginas ser&aacute; se interpretar&aacute; como la &uacute;ltima
     * p&aacute;gina si era positivo o la primera si era negativo o cero.
     * @param pageStr P&aacute;gina a normalizar.
     * @param totalPages N&uacute;mero total de p&aacute;ginas del documento.
     * @return N&uacute;mero de p&aacute;gina normalizada (n&uacute;mero positivo).
     * @throws NumberFormatException Cuando la cadena no sea un n&uacute;mero.
     */
    private static int normalizePage(final String pageStr, final int totalPages) {
		int page = Integer.parseInt(pageStr);
		// Si es un numero negativo, calculamos el numero de pagina
		if (page < 0) {
			page = page + totalPages + 1;
		}
		// Si se introdujo un numero negativo mayor que el numero de paginas, se firmara en la primera pagina
		if (page <= 0) {
			page = 1;
		}
		// Si el numero que se indica es mayor que el numero de paginas, se agregara en la ultima pagina.
		if (page > totalPages) {
			page = totalPages;
		}
		return page;
    }

    /**
     * Agrega una p&aacute;gina al listado de p&aacute;ginas resultantes siempre y cuando sea un
     * n&uacute;mero de p&aacute;gina v&aacute;lido y no est&eacute; ya incluida en &eacute;l.
     * @param page P&aacute;gina que se quiere agregar al resultado.
     * @param totalPages N&uacute;mero p&aacute;ginas del documento.
     * @param pagesList Listado de p&aacute;ginas resultante.
     */
    private static void addPageToResult(final int page, final int totalPages, final List<Integer> pagesList) {
		if (page <= totalPages && !pagesList.contains(Integer.valueOf(page))) {
			pagesList.add(Integer.valueOf(page));
		}
    }

    /**
     * Agrega un rango de p&aacute;ginas al resultado.
     * @param range Cadena con el rango de p&aacute;ginas.
     * @param totalPages N&uacute;mero total de p&aacute;ginas del documento.
     * @param pagesList Listado de p&aacute;ginas de resultado.
     */
    private static void addRangeToResult(final String range, final int totalPages, final List<Integer> pagesList) {

		int firstNumber;
		int limitNumber;
		try {
			// Obtenemos la posicion del separador de rango teniendo en cuenta que el primer
			// numero podria ser negativo
			final int sepIdx = range.startsWith(RANGE_INDICATOR)
					? range.indexOf(RANGE_INDICATOR, 1) : range.indexOf(RANGE_INDICATOR);
			firstNumber = normalizePage(range.substring(0, sepIdx), totalPages);
			limitNumber = normalizePage(range.substring(sepIdx + RANGE_INDICATOR.length()), totalPages);
		}
		catch (final Exception e) {
			final String cleanedText = range.length() > 12 ? range.substring(0, 12) + "..." : range; //$NON-NLS-1$
			throw new IncorrectPageException("La cadena introducida no se corresponde con un rango de paginas: " + cleanedText); //$NON-NLS-1$
		}

		if (limitNumber < firstNumber) {
			final String cleanedText = range.length() > 12 ? range.substring(0, 12) + "..." : range; //$NON-NLS-1$
			throw new IncorrectPageException("Se ha indicado un rango incorrecto: " + cleanedText); //$NON-NLS-1$
		}
		// Se agregan las paginas comprendidas entre el primer y ultimo numero
		for (int i = firstNumber; i <= limitNumber; i++) {
			addPageToResult(i, totalPages, pagesList);
		}
	}

    /**
     * Corrige el tama&ntilde;o del area de firma visible para que no se imprima completamente
     * fuera de ninguna p&aacute;gina y que no sobresalga en al menos la primera de las
     * p&aacute;ginas en la que se va a imprimir.
     * @param pdfReader Lector de PDF donde se encuentran las p&aacute;ginas del documento a firmar.
     * @param pagesList Lista con las p&aacute;ginas donde estampar la firma visible.
     * @param signaturePosition Posici&oacute;n de la firma.
     */
    public static void correctPositionSignature(final PdfReader pdfReader, final List<Integer> pagesList, final Rectangle signaturePosition) {

    	// Comprobamos que la firma se pueda estampar en al menos una de las paginas
    	final Integer[] pages = pagesList.toArray(new Integer[0]);
    	for (final Integer page : pages) {

    		final Rectangle pageSize = pdfReader.getPageSizeWithRotation(page.intValue());
    		if (pageSize.getWidth() <= signaturePosition.getLeft() || pageSize.getHeight() <= signaturePosition.getBottom()) {
    			pagesList.remove(page);
    		}
    	}

    	if (pagesList.isEmpty()) {
    		throw new InvalidSignaturePositionException(
    				"La posicion proporcionada no se encuentra en el rango de ninguna de las paginas a estampar del documento"); //$NON-NLS-1$
    	}

    	// Redimensionamos el area de firma para que quede completamente dentro de la primera pagina en la que se vaya a imprimir
    	final int firstPage = pagesList.get(0).intValue();
    	final Rectangle firstPageSize = pdfReader.getPageSizeWithRotation(firstPage);
    	if (signaturePosition.getTop() > firstPageSize.getTop()) {
    		signaturePosition.setTop(firstPageSize.getTop());
    	}
    	if (signaturePosition.getRight() > firstPageSize.getRight()) {
    		signaturePosition.setRight(firstPageSize.getRight());
    	}
    }

    /**
     * Comprueba que la cadena introducida en el campo de texto para la selecci&oacute;n de p&aacute;ginas sea correcta.
     * @param rangeInput Entrada de texto.
     * @return Devuelve true en caso de que el formato sea correcto y false en caso contrario.
     */
    public static boolean checkPagesRangeInputFormat(final String rangeInput) {
    	if (!rangeInput.isEmpty()) {

	    	final String rangeChars = "0123456789-,"; //$NON-NLS-1$

    		if (RANGE_SEPARATOR.equals(String.valueOf(rangeInput.charAt(rangeInput.length() -1)))
    				|| rangeInput.length() == 1 && RANGE_SEPARATOR.equals(rangeInput)) {
    			return false;
    		}

	    	for (int i = 0; i < rangeInput.length(); i++) {
	    		// Comprobamos que sea un caracter correcto
	    		if (!rangeChars.contains(String.valueOf(rangeInput.charAt(i)))
	    			// Comprobamos que no se hayan introducido tres guiones seguidos
	    			|| i+2 < rangeInput.length()
		    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i)))
		    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i +1)))
		    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i +2)))
	    			// Comprobamos que no se hayan introducido dos comas seguidas
	    			|| i+1 < rangeInput.length()
		    				&& RANGE_SEPARATOR.equals(String.valueOf(rangeInput.charAt(i)))
		    				&& RANGE_SEPARATOR.equals(String.valueOf(rangeInput.charAt(i +1)))) {
	    			return false;
	            }
	    	}

	    	final String[] rangesArray = rangeInput.split(RANGE_SEPARATOR);
	    	for (final String range : rangesArray) {

	    		if (RANGE_INDICATOR.equals(String.valueOf(range.charAt(range.length() -1)))
	    			// Comprobamos que el rango no termine con un guion
	    			|| range.length() >= 2
		    				&& RANGE_INDICATOR.equals(String.valueOf(range.charAt(0)))
		    				&& RANGE_INDICATOR.equals(String.valueOf(range.charAt(1)))
		    		// Comprobamos que se haya introducido un numero en caso de que la longitud sea 1
		    		|| range.length() == 1 && RANGE_INDICATOR.equals(range)) {
	    			return false;
	    		}
	    	}
    	} else {
    		return false;
    	}
    	return true;
    }



    /**
     * Obtiene el listado de p&aacute;ginas del documento que se han pedido firmar.
     * Si se devuelve una p&aacute;gina '0', se referir&aacute; a una p&aacute;gina
     * posterior a la &oacute;ltima.
     * @param extraParams Propiedades con la configuracion de firma de las que extraer
     * las p&aacute;ginas.
     * @param totalPages N&oacute;mero la &uacute;ltima p&aacute;gina del documento.
     * @return P&aacute;ginas del documento.
     */
	public static List<Integer> getPages(final Properties extraParams, final int totalPages) {

		// Comprobamos si se han indicado los parametros Si se encuentra el parametro signaturePages, prevalecera sobre el antiguo signaturePage
		final String[] pagesStr = extraParams.containsKey(PdfExtraParams.SIGNATURE_PAGES)
				? extraParams.getProperty(PdfExtraParams.SIGNATURE_PAGES).split(RANGE_SEPARATOR)
				: extraParams.containsKey(PdfExtraParams.SIGNATURE_PAGE)
					? extraParams.getProperty(PdfExtraParams.SIGNATURE_PAGE).split(RANGE_SEPARATOR)
					: new String[0];

		final List<Integer> pages = new ArrayList<>();

		// Si no se ha indicado ninguna pagina, se firmara en la ultima
		if (pagesStr.length == 0) {
			pages.add(totalPages);
		}
		// El valor APPEND_PAGE pide que se firme en una pagina posterior a la primera
		else if (APPEND_PAGE.equalsIgnoreCase(pagesStr[0].trim())) {
			pages.add(NEW_PAGE);
		// El valor ALL_PAGES pide que se firme en todas las paginas
		} else if (ALL_PAGES.equalsIgnoreCase(pagesStr[0].trim())) {
			for (int page = 1; page <= totalPages; page++) {
				pages.add(page);
			}
		// Rellenamos con las paginas y rangos indicados, evitando que se indiquen
		// paginas posteriores a la ultima
		} else {
			for (final String pageStr : pagesStr) {
				try {
					getPagesRange(pageStr, totalPages, pages);
				} catch (final IncorrectPageException e) {
					LOGGER.log(Level.WARNING, "Se ha indicado un numero o rango de paginas invalido. Se ignorara.", e); //$NON-NLS-1$
				}
			}
		}

		// Ordenamos el listado de paginas
		Collections.sort(pages);

		// Si nada de lo que se configuro definio un numero de pagina valido para el documento,
		// se usara la ultima pagina para la firma visible PDF
		if (pages.isEmpty()) {
			pages.add(totalPages);
		}

		return pages;
	}
}
