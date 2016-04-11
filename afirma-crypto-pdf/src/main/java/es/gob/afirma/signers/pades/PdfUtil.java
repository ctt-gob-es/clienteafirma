/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;

/** Utilidades variadas para el tratamiento de PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String FILTER_ADOBE_PKCS7_DETACHED = "/adbe.pkcs7.detached"; //$NON-NLS-1$

	private static final Set<String> SUPPORTED_SUBFILTERS;
	static {
		SUPPORTED_SUBFILTERS = new HashSet<String>();
		SUPPORTED_SUBFILTERS.add("/ETSI.RFC3161"); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add(FILTER_ADOBE_PKCS7_DETACHED);
		SUPPORTED_SUBFILTERS.add("/ETSI.CAdES.detached"); //$NON-NLS-1$
		SUPPORTED_SUBFILTERS.add("/adbe.pkcs7.sha1"); //$NON-NLS-1$
	}

	private PdfUtil() {
		// No instanciable
	}

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

		Date date;
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

	static PdfReader getPdfReader(final byte[] inPDF,
			                      final Properties extraParams,
			                      final boolean headless) throws BadPdfPasswordException,
			                                                     InvalidPdfException,
			                                                     IOException {
		// Contrasena del propietario del PDF
		final String ownerPassword = extraParams.getProperty(PdfExtraParams.OWNER_PASSWORD);

		// Contrasena del usuario del PDF
		final String userPassword =  extraParams.getProperty(PdfExtraParams.USER_PASSWORD);

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
			if (headless) {
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
			if (ownerPwd.isEmpty()) {
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
				!Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS))) {
			// Si no permitimos dialogos graficos o directamente hemos indicado que no permitimos firmar PDF certificados lanzamos
			// una excepcion
			if (Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS)) || "false".equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS))) {  //$NON-NLS-1$
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
			extraParams.setProperty(PdfExtraParams.ALLOW_SIGNING_CERTIFIED_PDFS, "true"); //$NON-NLS-1$
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
		if (extraParams.getProperty(PdfExtraParams.OWNER_PASSWORD) != null || extraParams.getProperty(PdfExtraParams.USER_PASSWORD) != null) {
			return true;
		}
		return Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.ALWAYS_CREATE_REVISION)) || pdfReader.getAcroFields().getSignatureNames().size() > 0;
	}

	static boolean pdfHasUnregisteredSignatures(final byte[] pdf, final Properties xParams) throws InvalidPdfException, BadPdfPasswordException, IOException {
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
			return new ArrayList<SignatureField>(0);
		}
		final PdfReader reader;
		try {
			reader = new PdfReader(pdf);
		}
		catch (final Exception e) {
			LOGGER.severe(
				"Error leyendo el PDF de entrada: " + e //$NON-NLS-1$
			);
			return new ArrayList<SignatureField>(0);
		}
		final AcroFields fields = reader.getAcroFields();
		if (fields != null) {
			final List<String> emptySignatureFields = fields.getBlankSignatureNames();
			final List<SignatureField> ret = new ArrayList<SignatureField>();
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
		return new ArrayList<SignatureField>(0);
	}
}
