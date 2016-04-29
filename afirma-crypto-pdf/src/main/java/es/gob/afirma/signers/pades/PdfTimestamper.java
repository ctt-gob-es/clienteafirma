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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.DocumentException;
import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.PdfDate;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfSignature;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;
import com.aowagie.text.pdf.PdfString;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Sellador de tiempo para documentos PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfTimestamper {

	private static final String TIMESTAMP_SUBFILTER = "ETSI.RFC3161"; //$NON-NLS-1$

	private static final int CSIZE = 27000;

    private static final int UNDEFINED = -1;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    private static final int PDF_MAX_VERSION = 7;
    private static final int PDF_MIN_VERSION = 2;

    private static final int PDF_MIN_COMPRESABLE_VERSION = 5;

	private PdfTimestamper() {
		// No instanciable
	}

	/** Aplica un sello de tiempo a un PDF.
	 * @param inPDF PDF de entrada.
	 * @param extraParams Par&aacute;metros de la TSA.
	 * @param signTime Tiempo para el sello.
	 * @return PDF con el sello de tiempo aplicado.
	 * @throws AOException Si hay problemas durante el proceso.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital indicado. */
	public static byte[] timestampPdf(final byte[] inPDF,
			                   final Properties extraParams,
			                   final Calendar signTime) throws AOException, IOException, NoSuchAlgorithmException {
    	// Comprobamos si se ha pedido un sello de tiempo
    	if (extraParams != null) {
    		final String tsa = extraParams.getProperty(PdfExtraParams.TSA_URL);
    		final String tsType = extraParams.getProperty(PdfExtraParams.TS_TYPE);

    		// Solo hacemos este tipo de sello en esta situacion:
    		// Han establecido URL de TSA y nos piden sello de tipo 2 (a nivel de documento) o de tipo 3
    		// (a nivel de documento y tambien a nivel de firma). Si el tipo del sello solicitado es null
    		// no se aplica este sello (pero si se hace el sello a nivel de firma).
    		// 1.- Solo sello firma.
    		// 2.- Solo sello de documento.
    		// 3.- Ambos sellos, documento y firma.
            if (tsa != null && (TsaParams.TS_DOC.equals(tsType) || TsaParams.TS_SIGN_DOC.equals(tsType))) {

                // Y procesamos normalmente el PDF
                final PdfReader pdfReader = PdfUtil.getPdfReader(
            		inPDF,
            		extraParams,
            		Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS))
        		);

            	// Comprobamos el nivel de certificacion del PDF
                PdfUtil.checkPdfCertification(pdfReader.getCertificationLevel(), extraParams);

        		// Establecimiento de version PDF
        		int pdfVersion;
        		try {
        			pdfVersion = extraParams.getProperty(PdfExtraParams.PDF_VERSION) != null ?
        				Integer.parseInt(extraParams.getProperty(PdfExtraParams.PDF_VERSION)) :
        					PDF_MAX_VERSION;
        		}
        		catch(final Exception e) {
        			LOGGER.warning("Error en el establecimiento de la version PDF, se usara " + PDF_MAX_VERSION + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
        			pdfVersion = PDF_MAX_VERSION;
        		}
        		if (pdfVersion != UNDEFINED && (pdfVersion < PDF_MIN_VERSION || pdfVersion > PDF_MAX_VERSION)) {
        			LOGGER.warning("Se ha establecido un valor invalido para version, se ignorara: " + pdfVersion); //$NON-NLS-1$
        			pdfVersion = UNDEFINED;
        		}

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        		final PdfStamper stp;
        		try {
        			stp = PdfStamper.createSignature(
        				pdfReader, // PDF de entrada
        				baos,      // Salida
        				pdfVersion == UNDEFINED ? '\0' /* Mantener version */ : Integer.toString(pdfVersion).toCharArray()[0] /* Version a medida */,
        				null,      // No crear temporal
        				PdfUtil.getAppendMode(extraParams, pdfReader), // Append Mode
        				signTime   // Momento de la firma
        			);
        		}
        		catch(final BadPasswordException e) {
        			throw new PdfIsPasswordProtectedException(e);
        		}
        		catch (final DocumentException e) {
					throw new AOException("Error de formato en el PDF de entrada: " + e, e); //$NON-NLS-1$
				}

        		// Aplicamos todos los atributos de firma
        		final PdfSignatureAppearance sap = stp.getSignatureAppearance();

        		// La compresion solo para versiones 5 y superiores
        		// Hacemos la comprobacion a "false", porque es el valor que deshabilita esta opcion
        		if (pdfVersion >= PDF_MIN_COMPRESABLE_VERSION && !"false".equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.COMPRESS_PDF))) { //$NON-NLS-1$
        			stp.setFullCompression();
        		}

        		PdfUtil.enableLtv(stp);

        		sap.setAcro6Layers(true);
        		sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        		sap.setSignDate(signTime);

        		final PdfSignature pdfSignature = new PdfSignature(
    				PdfName.ADOBE_PPKLITE,
    				new PdfName(TIMESTAMP_SUBFILTER)
    			);

        		pdfSignature.setDate(new PdfDate(signTime));
        		sap.setCryptoDictionary(pdfSignature);

        		// Reservamos el espacio necesario en el PDF para insertar la firma
        		final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
        		exc.put(PdfName.CONTENTS, Integer.valueOf(CSIZE * 2 + 2));

        		try {
					sap.preClose(exc, signTime);
				}
        		catch (final DocumentException e) {
					throw new AOException("Error en el procesado del PDF: " + e, e); //$NON-NLS-1$
				}

        		// Obtenemos el rango procesable
        		final byte[] original = AOUtil.getDataFromInputStream(sap.getRangeStream());

        		// Obtenemos el sello
        		final byte[] tspToken = getTspToken(extraParams, original, signTime);

            	// Y lo insertamos en el PDF
        		final byte[] outc = new byte[CSIZE];

                if (tspToken.length > CSIZE) {
                	throw new AOException(
            			"El tamano del sello de tiempo (" + tspToken.length + ") supera el maximo permitido para un PDF (" + CSIZE + ")" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        			);
                }

        		final PdfDictionary dic2 = new PdfDictionary();
        		System.arraycopy(tspToken, 0, outc, 0, tspToken.length);
                dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));

        	    try {
    		       sap.close(dic2);
    		    }
    		    catch (final Exception e) {
    		    	baos.close();
    		        throw new AOException("Error al cerrar el PDF para finalizar el proceso de firma", e); //$NON-NLS-1$
    		    }

        	    return baos.toByteArray();
            }
    	}
		return inPDF;
	}

	private static byte[] getTspToken(final Properties extraParams, final byte[] original, final Calendar signTime) throws AOException, NoSuchAlgorithmException, IOException {

    	// Obtenemos el sellador de tiempo
		final TsaParams tsaParams = new TsaParams(extraParams);
        final CMSTimestamper timestamper = new CMSTimestamper(tsaParams);

		// Obtenemos el token TSP
		return timestamper.getTimeStampToken(
			MessageDigest.getInstance(
				tsaParams.getTsaHashAlgorithm()
    		).digest(
				original
			),
			tsaParams.getTsaHashAlgorithm(),
			signTime
		);
	}

}
