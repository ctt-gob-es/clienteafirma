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
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

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
	static PdfReader getPdfReader(final byte[] inPDF,
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

	static void checkPdfCertification(final int pdfCertificationLevel, final Properties extraParams) throws PdfIsCertifiedException {

		// Si el PDF esta certificado, se comprobara si se ha indicado expresamente que se permite
		// multifirmar este tipo de documentos. Si no se permite, se lanza una excepcion
		if (pdfCertificationLevel != PdfSignatureAppearance.NOT_CERTIFIED) {
			final String allow = extraParams.getProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS);
			if (!Boolean.parseBoolean(allow)) {
				throw new PdfIsCertifiedException("El PDF esta certificado"); //$NON-NLS-1$
			}
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
	    	if (extraParams.getProperty(prefix + "PositionOnPageLowerLeftX") != null && //$NON-NLS-1$
	    		extraParams.getProperty(prefix + "PositionOnPageLowerLeftY") != null && //$NON-NLS-1$
			extraParams.getProperty(prefix + "PositionOnPageUpperRightX") != null && //$NON-NLS-1$
			extraParams.getProperty(prefix + "PositionOnPageUpperRightY") != null //$NON-NLS-1$
	    	) {
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
     * Metodo que controla si se ha introducido alg&uacute;n rango en el par&aacute;metro con las p&aacute;ginas donde estampar la firma
     * visible para introducirlo en un array correctamente.
     * @param pageStr P&aacute;gina o rango de p&aacute;ginas a tratar.
     * @param totalPages N&uacute;mero total de p&aacute;ginas del documento.
     * @param pagesList Lista de enteros donde se indican las p&aacute;ginas una a una.
     */
    public static void checkPagesRange(final String pageStr, final int totalPages, final List<Integer> pagesList) {

    	try {
    		int page = Integer.parseInt(pageStr);
			if (page < 0) {
				page = page + totalPages + 1;
			}
			if (page <= 0) {
				throw new IncorrectPageException("El numero de pagina indicado no es correcto: " + pageStr); //$NON-NLS-1$
			}
			// Si el numero que se indica supera las paginas que tiene el documento, no se agregara.
			if (page <= totalPages && !pagesList.contains(page)) {
				pagesList.add(page);
			}

    	} catch (final NumberFormatException nfe) {
    	// En caso de error al parsear, quiere decir que se ha introducido un rango
			int firstNumber;
			int limitNumber;
			String limitNumberStr;
			if (RANGE_INDICATOR.equals(pageStr.substring(0, 1))) {
				firstNumber = Integer.parseInt(pageStr.substring(0, pageStr.indexOf(RANGE_INDICATOR, 1))) + totalPages + 1;
    			limitNumberStr = pageStr.replaceAll(pageStr.substring(0, pageStr.indexOf(RANGE_INDICATOR, 1)) + RANGE_INDICATOR, "");  //$NON-NLS-1$
    			limitNumber = Integer.parseInt(limitNumberStr) + totalPages + 1;
			}
			// Si el primer numero no es negativo, se obtiene el rango directamente
			else {
				firstNumber = Integer.parseInt(pageStr.substring(0, pageStr.indexOf(RANGE_INDICATOR)));
				limitNumberStr = pageStr.replaceAll(pageStr.substring(0, pageStr.indexOf(RANGE_INDICATOR)) + RANGE_INDICATOR, ""); //$NON-NLS-1$
				limitNumber = Integer.parseInt(limitNumberStr);
				if (limitNumber < 0) {
					limitNumber = limitNumber + totalPages + 1;
				}
			}
			if (firstNumber <= 0 || limitNumber <= 0 || limitNumber < firstNumber || totalPages < firstNumber) {
				throw new IncorrectPageException("El rango indicado no es correcto: " + pageStr); //$NON-NLS-1$
			}
			// Se agregan las paginas comprendidas entre el primer y ultimo numero
			for ( ; firstNumber <= limitNumber ; firstNumber++) {
				if (firstNumber <= totalPages && !pagesList.contains(firstNumber)) {
					pagesList.add(firstNumber);
    			}
			}
    	}
    }

    /**
     * Comprueba que la posici&oacute;n indicada donde estampar la firma visible corresponda a alguna de las p&aacute;ginas a estampar
     * del documento. En caso contrario, se lanzara una InvalidSignaturePositionException.
     * @param pdfReader Lector de PDF donde se encuentran las p&aacute;ginas del documento a firmar.
     * @param pagesList Lista con las p&aacute;ginas donde estampar la firma visible.
     * @param extraParams Par&aacute;metros extra con informaci&aacute;n sobre la posici&oacute;n.
     */
    public static void checkCorrectPositionSignature(final PdfReader pdfReader, final List<Integer> pagesList, final Properties extraParams) {
    	if (!pagesList.isEmpty()) {
	    	for (final int page : pagesList) {
	    		if (pdfReader.getPageSize(page).getBottom() - 15 <
						Float.parseFloat(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY))
					&& pdfReader.getPageSize(page).getLeft() - 15 <
						Float.parseFloat(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX))
	    			&& pdfReader.getPageSize(page).getTop() - 15 >
	    				Float.parseFloat(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY))
	    			&& pdfReader.getPageSize(page).getRight() - 15 >
						Float.parseFloat(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX)))
	    		{
	    				return;
	    		}
	    	}

	    	throw new InvalidSignaturePositionException(
	    			"La posicion proporcionada no se encuentra en el rango de ninguna de las paginas a estampar del documento" //$NON-NLS-1$
	    	);
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
	    		if (!rangeChars.contains(String.valueOf(rangeInput.charAt(i)))) {
	    			return false;
	            }
	    		// Comprobamos que no se hayan introducido tres guiones seguidos
	    		else if (i+2 < rangeInput.length()
	    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i)))
	    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i +1)))
	    				&& RANGE_INDICATOR.equals(String.valueOf(rangeInput.charAt(i +2)))) {
	    			return false;
	    		}
	    		// Comprobamos que no se hayan introducido dos comas seguidas
	    		else if (i+1 < rangeInput.length()
	    				&& RANGE_SEPARATOR.equals(String.valueOf(rangeInput.charAt(i)))
	    				&& RANGE_SEPARATOR.equals(String.valueOf(rangeInput.charAt(i +1)))) {
	    			return false;
	    		}
	    	}

	    	final String[] rangesArray = rangeInput.split(","); //$NON-NLS-1$
	    	for (final String range : rangesArray) {

	    		if (RANGE_INDICATOR.equals(String.valueOf(range.charAt(range.length() -1)))) {
	    			return false;
	    		}
	    		// Comprobamos que el rango no termine con un guion
	    		else if (range.length() >= 2
	    				&& RANGE_INDICATOR.equals(String.valueOf(range.charAt(0)))
	    				&& RANGE_INDICATOR.equals(String.valueOf(range.charAt(1))) )
	    		{
	    			return false;
	    		}
	    		// Comprobamos que se haya introducido un numero en caso de que la longitud sea 1
	    		else if (range.length() == 1 && RANGE_INDICATOR.equals(range)) {
	    			return false;
	    		}
	    	}
    	} else {
    		return false;
    	}
    	return true;
    }
}
