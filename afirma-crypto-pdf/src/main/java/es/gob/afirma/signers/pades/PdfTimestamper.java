/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.MessageDigest;
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
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;

/** Sellador de tiempo para documentos PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfTimestamper {

	private static final String TIMESTAMP_SUBFILTER = "ETSI.RFC3161"; //$NON-NLS-1$

	private static final int CSIZE = 27000;

    private static final int UNDEFINED = -1;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    /** Sello a nivel de firma. No permite mas cambios. */
    public static final String TS_LEVEL_SIGN = "1"; //$NON-NLS-1$
    /** Sello a nivel de documento. Permite cambios de formulario. */
    public static final String TS_LEVEL_DOC = "2"; //$NON-NLS-1$
    /** Sello a nivel de firma y a nivel de documento. Permite cambios de anotaciones y formularios. */
    public static final String TS_LEVEL_SIGN_DOC = "3"; //$NON-NLS-1$

    private static final int PDF_MAX_VERSION = 7;
    private static final int PDF_MIN_VERSION = 2;

    private static final int PDF_MIN_COMPRESABLE_VERSION = 5;

    private static Class<?> cmsTimestamperClass = null;

    private static Class<?> tsaParamsClass = null;

    private static boolean initialized = false;

	private PdfTimestamper() {
		// No instanciable
	}

	private static void initialize() {
		if (initialized) {
			return;
		}

		try {
			cmsTimestamperClass = Class.forName("es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper", //$NON-NLS-1$
					false, PdfTimestamper.class.getClassLoader());
			tsaParamsClass = Class.forName("es.gob.afirma.signers.tsp.pkcs7.TsaParams"); //$NON-NLS-1$
		}
		catch (final Throwable e) {
			LOGGER.info("No se encuentran las clases de sellado de tiempo CAdES: " + e); //$NON-NLS-1$
			cmsTimestamperClass = null;
			tsaParamsClass = null;
		}
		initialized = true;
	}

	/**
	 * Aplica un sello de tiempo a un PDF.
	 * @param inPDF PDF de entrada.
	 * @param extraParams Par&aacute;metros de la TSA.
	 * @param signTime Tiempo para el sello.
	 * @return PDF con el sello de tiempo aplicado.
	 * @throws AOException Si hay problemas durante el proceso.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 */
	public static byte[] timestampPdf(final byte[] inPDF,
			             final Properties extraParams,
			             final Calendar signTime) throws AOException,
	                                                     IOException {

		// Si no estan disponibles las clases necesarias para el sellado
		// de tiempo, no hacemos nada
		if (!isAvailable()) {
			return inPDF;
		}

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
            if (tsa != null && (TS_LEVEL_DOC.equals(tsType) || TS_LEVEL_SIGN_DOC.equals(tsType))) {

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
        				Integer.parseInt(extraParams.getProperty(PdfExtraParams.PDF_VERSION).trim()) :
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

        		try (
    				final ByteArrayOutputStream baos = new ByteArrayOutputStream();
				) {
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
        				new PdfName("DocTimeStamp"), //$NON-NLS-1$
	    				PdfName.ADOBE_PPKLITE,
	    				new PdfName(TIMESTAMP_SUBFILTER)
	    			);

	        		pdfSignature.setDate(new PdfDate(signTime));
	        		sap.setCryptoDictionary(pdfSignature);

	        		// Reservamos el espacio necesario en el PDF para insertar la firma
	        		final HashMap<PdfName, Integer> exc = PdfSessionManager.reserveSignSizes(extraParams);

	        		try {
						sap.preClose(exc, signTime, null);
					}
	        		catch (final DocumentException e) {
						throw new AOException("Error en el procesado del PDF: " + e, e); //$NON-NLS-1$
					}

	        		// Obtenemos el rango procesable
	        		final byte[] original;
	        		try (
        				final InputStream is = sap.getRangeStream();
    				) {
	        			original = AOUtil.getDataFromInputStream(is);
	        		}

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
    	}
		return inPDF;
	}

	private static byte[] getTspToken(final Properties extraParams,
			final byte[] original,
			final Calendar signTime) throws AOException {

		try {
			// Cargamos los parametros
			final Object tsaParamsObject = buildTsaParams(extraParams);

			// Obtenemos el sellador de tiempo
			final Object timestamperObject = buildCmsTimeStamper(tsaParamsObject);

			// Obtenemos el algoritmo de hash del sello
			final String tsaHashAlgorithm = getHashAlgorithm(tsaParamsObject);

			// Identificamos el metodo para la generacion del sello de tiempo
			final Method getTimeStampTokenMethod = cmsTimestamperClass.getMethod(
					"getTimeStampToken", //$NON-NLS-1$
					byte[].class, String.class, Calendar.class);

			final byte[] tsDigest = MessageDigest.getInstance(tsaHashAlgorithm)
					.digest(original);

			// Obtenemos el token TSP
			return (byte[]) getTimeStampTokenMethod.invoke(
					timestamperObject,
					tsDigest,
					tsaHashAlgorithm,
					signTime
					);
		}
		catch (final InvocationTargetException e) {
			throw new AOException("Error al generar los sellos de tiempo", e); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido generar el sello de tiempo", e); //$NON-NLS-1$
		}
	}

	public static boolean isAvailable() {

		// Cargamos las clases de sellado de tiempo
		initialize();

		// Si no se encuentran cargadas las clases,
		// no estara disponible el sellado de tiempo
		return cmsTimestamperClass != null;
	}

	/**
	 * Agrega un sello de tiempo a una firma CMS/CAdES.
	 * @param cmsSignature Firma CMS/CAdES
	 * @param extraParams Par&aacute;metros de configuraci&oacute;n de firma.
	 * @param signingTime Hora de la firma.
	 * @return Firma CMS/CAdES con el sello de tiempo.
	 */
	public static byte[] addCmsTimeStamp(final byte[] cmsSignature,
			final Properties extraParams, final Calendar signingTime) {

        Object tsaParamsObject;
        try {
        	tsaParamsObject = buildTsaParams(extraParams);
        }
        catch(final Exception e) {
        	LOGGER.warning(
    			"Se ha pedido aplicar sello de tiempo, pero falta informacion necesaria: " + e //$NON-NLS-1$
			);
        	tsaParamsObject = null;
        }

        byte[] timeStampedSignature = cmsSignature;
        if (tsaParamsObject != null) {
        	try {
        		// Obtenemos el algoritmo de hash del sello
        		final String tsaHashAlgorithm = getHashAlgorithm(tsaParamsObject);
        		// Obtenemos el algoritmo de hash del sello de tiempo
        		final Object timestamperObject = buildCmsTimeStamper(tsaParamsObject);

        		final Method addTimestampMethod = cmsTimestamperClass.getMethod(
        				"addTimestamp", byte[].class, String.class, Calendar.class); //$NON-NLS-1$

        		timeStampedSignature = (byte[]) addTimestampMethod.invoke(
        				timestamperObject,
        				cmsSignature,
        				tsaHashAlgorithm,
        				signingTime);
        	}
        	catch (final Exception e) {
        		LOGGER.warning(
            			"No se ha podido actualizar la firma: " + e //$NON-NLS-1$
        			);
        		timeStampedSignature = cmsSignature;
        	}
        }

		return timeStampedSignature;
	}

	/**
	 * Carga los par&aacute;metros para la generaci&oacute;n de un sello de tiempo
	 * a partir de los extraParams de firma.
	 * @param params ExtraParams de firma.
	 * @return Objeto TsaParams para la generaci&oacute;n del sello de tiempo.
	 * @throws InstantiationException Error en la instanciacion del objeto.
	 * @throws IllegalAccessException Intento de acceso a un constructor protegido.
	 * @throws IllegalArgumentException Parametro no v&aacute;lido.
	 * @throws InvocationTargetException Error al contruir el objeto.
	 * @throws NoSuchMethodException El constructor no existe.
	 * @throws SecurityException Acceso no permitido.
	 */
	private static Object buildTsaParams(final Properties params)
			throws InstantiationException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException,
			NoSuchMethodException, SecurityException {
		return tsaParamsClass
				.getConstructor(Properties.class)
				.newInstance(params);

	}

	/**
	 * Obtiene el algoritmo de hash para el sello de tiempo solicitado
	 * en los par&aacute;metros TSA.
	 * @param tsaParams Par&aacute;metros TSA.
	 * @return Objeto TsaParams para la generaci&oacute;n del sello de tiempo.
	 * @throws IllegalAccessException Intento de acceso a un constructor protegido.
	 * @throws IllegalArgumentException Parametro no v&aacute;lido.
	 * @throws InvocationTargetException Error al contruir el objeto.
	 * @throws NoSuchMethodException El constructor no existe.
	 * @throws SecurityException Acceso no permitido.
	 */
	private static String getHashAlgorithm(final Object tsaParams)
			throws IllegalAccessException, IllegalArgumentException,
			InvocationTargetException, NoSuchMethodException,
			SecurityException {
		return (String) tsaParamsClass
				.getMethod("getTsaHashAlgorithm") //$NON-NLS-1$
				.invoke(tsaParams);
	}

	/**
	 * Carga un objeto para la generaci&oacute;n de sellos de tiempo CMS a
	 * partir de los parametros de TSA necesarios.
	 * @param tsaParams Par&aacute;metros TSA.
	 * @return Objeto Constructor de sellos de tiempo.
	 * @throws InstantiationException Error en la instanciacion del objeto.
	 * @throws IllegalAccessException Intento de acceso a un constructor protegido.
	 * @throws IllegalArgumentException Parametro no v&aacute;lido.
	 * @throws InvocationTargetException Error al contruir el objeto.
	 * @throws NoSuchMethodException El constructor no existe.
	 * @throws SecurityException Acceso no permitido.
	 */
	private static Object buildCmsTimeStamper(final Object tsaParams)
			throws InstantiationException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException,
			NoSuchMethodException, SecurityException {
		return cmsTimestamperClass
				.getConstructor(tsaParamsClass.getClass())
				.newInstance(tsaParams);
	}
}
