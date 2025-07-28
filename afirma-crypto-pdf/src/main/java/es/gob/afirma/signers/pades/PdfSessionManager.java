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
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.DocumentException;
import com.aowagie.text.Font;
import com.aowagie.text.Image;
import com.aowagie.text.Rectangle;
import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.exceptions.InvalidPageNumberException;
import com.aowagie.text.pdf.PdfDate;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfObject;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfSignature;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;

/** Gestor del n&uacute;cleo de firma PDF. Esta clase realiza las operaciones necesarias tanto para
 * la firma monof&aacute;sica PAdES como para las trif&aacute;sicas de una forma unificada, pero
 * &uacute;nicamente en lo referente al formato PDF, sin entrar en la parte CAdES o PKCS#7
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfSessionManager {

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    static final int LAST_PAGE = -1;

    static final int NEW_PAGE = 0;

    private static final int UNDEFINED = -1;

    private static final int CSIZE = 27000;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    private static final int PDF_MAX_VERSION = 7;
    private static final int PDF_MIN_VERSION = 2;

    /** Rotaci&oacute;n de las firmas en grados (cuando se indica que deben rotarse). */
    private static final String DEFAULT_SIGNATURE_ROTATION = "0"; //$NON-NLS-1$

    private PdfSessionManager() {
    	// No permitimos la instanciacion
    }

    /** Obtiene los datos PDF relevantes en cuanto a las firmas electr&oacute;nicas. Estos son en los datos
     * a ser firmados con CAdES o PKCS#7 y los metadatos necesarios para su correcta inserci&oacute;n en el PDF.
     * @param pdfBytes Documento PDF que se desea firmar
     * @param certChain Cadena de certificados del firmante
     * @param signTime Hora de la firma
     * @param xParams Par&aacute;metros adicionales de la firma
     * @param secureMode Modo seguro.
     * @return Datos PDF relevantes en cuanto a las firmas electr&oacute;nicas
     * @throws IOException En caso de errores de entrada / salida.
     * @throws InvalidPdfException Si el formato del documento no es v&aacute;lido.
     * @throws AOException En caso de que ocurra cualquier otro tipo de error.
     */
    public static PdfTriPhaseSession getSessionData(final byte[] pdfBytes,
                                                    final Certificate[] certChain,
                                                    final Calendar signTime,
                                                    final Properties xParams,
                                                    final boolean secureMode) throws IOException,
                                                                                         InvalidPdfException,
                                                                                         AOException {

		// *********************************************************************************************************************
		// **************** LECTURA PARAMETROS ADICIONALES *********************************************************************
		// *********************************************************************************************************************

		final Properties extraParams = xParams != null ? xParams : new Properties();

		// Omision de informacion del firmante diccionario o estructura de apariencia PDF.
		final boolean doNotUseCertChainOnPostSign = Boolean
				.parseBoolean(extraParams.getProperty(PdfExtraParams.DO_NOT_USE_CERTCHAIN_ON_POSTSIGN));

		// Rotacion del campo de firma (90 grados)
		final int signatureRotation = Integer
				.parseInt(extraParams.getProperty(PdfExtraParams.SIGNATURE_ROTATION, DEFAULT_SIGNATURE_ROTATION));

		// Imagen de la rubrica
		final com.aowagie.text.Image rubric = PdfPreProcessor.getImage(extraParams.getProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE), secureMode);

		// Motivo de la firma
		final String reason = extraParams.getProperty(PdfExtraParams.SIGN_REASON);

		// Nombre del campo de firma preexistente en el PDF a usar
		final String signatureField = extraParams.getProperty(PdfExtraParams.SIGNATURE_FIELD);

		// Lugar de realizacion de la firma
		final String signatureProductionCity = extraParams.getProperty(PdfExtraParams.SIGNATURE_PRODUCTION_CITY);

		// Datos de contacto (correo electronico) del firmante
		final String signerContact = extraParams.getProperty(PdfExtraParams.SIGNER_CONTACT);

		byte[] inPDF;
		try {
			inPDF = XmpHelper.addSignHistoryToXmp(pdfBytes, signTime, extraParams);
		}
		catch (final BadPasswordException e) {
			// Devolvemos una excepcion u otra segun si se nos proporciono
			// contrasena o no
			if (extraParams.containsKey(PdfExtraParams.OWNER_PASSWORD_STRING) || extraParams.containsKey(PdfExtraParams.USER_PASSWORD_STRING)) {
				throw new BadPdfPasswordException("La contrasena del PDF es incorrecta", e); //$NON-NLS-1$
			}
			throw new PdfIsPasswordProtectedException("El PDF esta protegido contra lectura", e); //$NON-NLS-1$
		}
		catch (final Exception e1) {
			LOGGER.warning("No ha podido registrarse la firma en el historico XMP: " + e1); //$NON-NLS-1$
			inPDF = pdfBytes;
		}

		final PdfReader pdfReader = PdfUtil.getPdfReader(inPDF, extraParams,
				Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS)));

		// Nombre del subfiltro de firma en el diccionario PDF
		String signatureSubFilter = extraParams.getProperty(PdfExtraParams.SIGNATURE_SUBFILTER);

		// Obtenemos el id de la politica de firma
		final String policyID = extraParams.getProperty(PdfExtraParams.POLICY_IDENTIFIER);

		// Obtenemos el perfil de firma configurado
		final String profile = extraParams.getProperty(PdfExtraParams.PROFILE);

		// Si existe una politica de firma o si la firma sigue el perfil baseline, el subfiltro
		// siempre debera ser "ETSI.CAdES.detached"
		if (policyID != null || AOSignConstants.SIGN_PROFILE_BASELINE.equals(profile)) {
			signatureSubFilter = AOSignConstants.PADES_SUBFILTER_BES;
			extraParams.setProperty(PdfExtraParams.SIGNATURE_SUBFILTER, AOSignConstants.PADES_SUBFILTER_BES);
		}

		// Nivel de certificacion del PDF
		int certificationLevel;
		try {
			certificationLevel = extraParams.getProperty(PdfExtraParams.CERTIFICATION_LEVEL) != null
					? Integer.parseInt(extraParams.getProperty(PdfExtraParams.CERTIFICATION_LEVEL).trim())
					: UNDEFINED;
		}
		catch (final Exception e) {
			LOGGER.warning("Se ha indicado un nivel de certificacion no valido ('" //$NON-NLS-1$
					+ extraParams.getProperty(PdfExtraParams.CERTIFICATION_LEVEL) + "'): " + e //$NON-NLS-1$
			);
			certificationLevel = UNDEFINED;
		}

		// Establecimiento de version PDF
		int pdfVersion;
		try {
			pdfVersion = extraParams.getProperty(PdfExtraParams.PDF_VERSION) != null
					? Integer.parseInt(extraParams.getProperty(PdfExtraParams.PDF_VERSION).trim())
					: PDF_MAX_VERSION;
		}
		catch (final Exception e) {
			LOGGER.warning("Error en el establecimiento de la version PDF, se usara " + PDF_MAX_VERSION + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			pdfVersion = PDF_MAX_VERSION;
		}
		if (pdfVersion != UNDEFINED && (pdfVersion < PDF_MIN_VERSION || pdfVersion > PDF_MAX_VERSION)) {
			LOGGER.warning("Se ha establecido un valor invalido para version, se ignorara: " + pdfVersion); //$NON-NLS-1$
			pdfVersion = UNDEFINED;
		}

		// **************************************************************
		// ***** Comprobaciones y parametros necesarios para PDF-A1 *****
		final byte[] xmpBytes = pdfReader.getMetadata();
		final boolean pdfA1 = PdfUtil.isPdfA1(xmpBytes);
		if (pdfA1) {
			LOGGER.info("Detectado PDF-A1, no se comprimira el PDF"); //$NON-NLS-1$
		}

		// *** Fin comprobaciones y parametros necesarios para PDF-A1 ***
		// **************************************************************

		// *****************************
		// **** Texto firma visible ****

		String layer4Text = null;
		String layer2Text = null;
		Font layer2Font = null;
		if (PdfVisibleAreasUtils.isVisibleSignature(extraParams)) {
			// Por defecto, siempre se ofuscara la informacion del certificado, salvo que usemos un certificado
			// de seudonimo
			boolean obfuscate = true;
			if (extraParams.containsKey(PdfExtraParams.OBFUSCATE_CERT_DATA)) {
				obfuscate = Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.OBFUSCATE_CERT_DATA));
			}
			else if (AOUtil.isPseudonymCert((X509Certificate) certChain[0])) {
				obfuscate = false;
			}

			final String pdfMaskConfig = extraParams.getProperty(PdfExtraParams.OBFUSCATION_MASK);

			// Texto en capa 4
			layer4Text = PdfVisibleAreasUtils.getLayerText(
					extraParams.getProperty(PdfExtraParams.LAYER4_TEXT),
					certChain != null && certChain.length > 0 ? (X509Certificate) certChain[0] : null,
							signTime,
							reason,
							signatureProductionCity,
							signerContact,
							obfuscate,
							pdfMaskConfig
					);

			// Texto en capa 2
			String configuredLayer2Text = extraParams.getProperty(PdfExtraParams.LAYER2_TEXT);
			if (configuredLayer2Text == null && !extraParams.containsKey(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE)) {
				configuredLayer2Text = getDefaultLayer2Text(reason != null, signatureProductionCity != null);
			}
			layer2Text = PdfVisibleAreasUtils.getLayerText(
					configuredLayer2Text,
					certChain != null && certChain.length > 0 ? (X509Certificate) certChain[0] : null,
							signTime,
							reason,
							signatureProductionCity,
							signerContact,
							obfuscate,
							pdfMaskConfig
					);

			// Tipo de letra en capa 2
			int layer2FontFamily;
			try {
				layer2FontFamily = extraParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY) != null
						? Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY).trim())
								: UNDEFINED;
			}
			catch (final Exception e) {
				LOGGER.warning("Se ha indicado un tipo de letra no valido para la capa 2 del PDF ('" //$NON-NLS-1$
						+ extraParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY) + "'): " + e //$NON-NLS-1$
						);
				layer2FontFamily = UNDEFINED;
			}

			// Tamano del tipo de letra en capa 2
			int layer2FontSize;
			try {
				layer2FontSize = extraParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE) != null
						? Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE).trim())
								: UNDEFINED;
			}
			catch (final Exception e) {
				LOGGER.warning("Se ha indicado un tamano de letra no valido para la capa 2 del PDF ('" //$NON-NLS-1$
						+ extraParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE) + "'): " + e //$NON-NLS-1$
						);
				layer2FontSize = UNDEFINED;
			}

			// Estilo del tipo de letra en capa 2
			int layer2FontStyle;
			try {
				layer2FontStyle = extraParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE) != null
						? Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE).trim())
								: UNDEFINED;
			}
			catch (final Exception e) {
				LOGGER.warning("Se ha indicado un estilo de letra no valido para la capa 2 del PDF ('" //$NON-NLS-1$
						+ extraParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE) + "'): " + e //$NON-NLS-1$
						);
				layer2FontStyle = UNDEFINED;
			}

			// Color del tipo de letra en capa 2
			final String layer2FontColor = extraParams.getProperty(PdfExtraParams.LAYER2_FONTCOLOR);

			layer2Font = PdfVisibleAreasUtils.getFont(
					layer2FontFamily,
					layer2FontSize,
					layer2FontStyle,
					layer2FontColor,
					pdfA1
					);
		}
		// ** Fin texto firma visible **
		// *****************************

		// *********************************************************************************************************************
		// **************** FIN LECTURA PARAMETROS ADICIONALES *****************************************************************
		// *********************************************************************************************************************

		PdfUtil.checkPdfCertification(pdfReader.getCertificationLevel(), extraParams);

		// Definimos el comportamiento cuando se encuentran firmas no registradas
		if (PdfUtil.pdfHasUnregisteredSignatures(pdfReader)) {
			final String allowUnregisteredSignatureValue = extraParams.getProperty(PdfExtraParams.ALLOW_COSIGNING_UNREGISTERED_SIGNATURES);
			// Si no se especifica el comportamiento, informamos de que se requiere autorizacion para completar la operacion
			if (allowUnregisteredSignatureValue == null || allowUnregisteredSignatureValue.trim().isEmpty()) {
				throw new PdfHasUnregisteredSignaturesException("El PDF contiene firmas sin registrar"); //$NON-NLS-1$
			}
			// Si se ha indicado expresamente que no se permite, lanzamos un error; y si esta permitido, se continua con el proceso
			if (!Boolean.parseBoolean(allowUnregisteredSignatureValue)) {
				throw new AOException("El PDF contiene firmas sin registrar y estas no estan permitidas"); //$NON-NLS-1$
			}
		}

		// Los derechos van firmados por Adobe, y como desde iText se invalidan
		// es mejor quitarlos
		pdfReader.removeUsageRights();

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		// Activar el atributo de "agregar firma" (quinto parametro del metodo
		// "PdfStamper.createSignature") hace que se cree una nueva revision del
		// documento y evita que las firmas previas queden invalidadas.
		// Sin embargo, este procedimiento no funciona cuando los PDF contienen
		// informacion
		// despues de la ultima marca %%EOF, aspecto no permitido en PDF 1.7 (ISO
		// 32000-1:2008)
		// pero si en PDF 1.3 (Adobe) y que se da con frecuencia en PDF generados con
		// bibliotetcas
		// de software libre como QPDF.
		//
		// Especificacion PDF 1.3
		// 	3.4.4, "File Trailer"
		// 		Acrobat viewers require only that the %%EOF marker appear somewhere within
		// 		the last 1024 bytes of the file.
		//
		// Especificacion PDF 1.7
		// 	7.5.5. File Trailer
		// 		The trailer of a PDF file enables a conforming reader to quickly find the
		// 		cross-reference table and certain special objects. Conforming readers should read a
		// 		PDF file from its end. The last line of the file shall contain only the
		// 		end-of-file marker, %%EOF.
		//
		// Para aceptar al menos en algunos casos PDF 1.3 (son aun muy frecuentes,
		// especialmente
		// en archivos, lo mantendremos desactivado para la primera firma y activado
		// para las subsiguientes.
		//
		// No obstante, el integrador puede siempre forzar la creacion de revisiones mediante
		// el parametro "alwaysCreateRevision".
		// Aplicamos todos los atributos de firma
		PdfStamper stp;

		try {
			stp = PdfStamper.createSignature(pdfReader, // PDF de entrada
					baos, // Salida
					pdfVersion == UNDEFINED ? '\0' /* Mantener version */
							: Integer.toString(pdfVersion).toCharArray()[0] /* Version a medida */,
					null, // No crear temporal
					PdfUtil.getAppendMode(extraParams, pdfReader), // Append Mode
					signTime // Momento de la firma
			);
		}
		catch (final DocumentException e) {
			LOGGER.severe("Error al crear la firma para estampar: " + e); //$NON-NLS-1$
			throw new AOException("Error al crear la firma para estampar", e); //$NON-NLS-1$
		}
		catch (final BadPasswordException e) {
			// Devolvemos una excepcion u otra segun si se nos proporciono
			// contrasena o no
			if (extraParams.containsKey(PdfExtraParams.OWNER_PASSWORD_STRING) || extraParams.containsKey(PdfExtraParams.USER_PASSWORD_STRING)) {
				throw new BadPdfPasswordException("La contrasena del PDF es incorrecta", e); //$NON-NLS-1$
			}
			throw new PdfIsPasswordProtectedException("El PDF esta protegido contra modificaciones", e); //$NON-NLS-1$
		}

		// Obtenemos la posicion de la firma si se ha indicado
		List<Integer> pages = null;
		final Rectangle signaturePositionOnPage = PdfVisibleAreasUtils.getSignaturePositionOnPage(extraParams);
		if (signaturePositionOnPage != null) {

			final int totalPages = pdfReader.getNumberOfPages();

			// Definimos las paginas en las que imprimir la firma
			pages = PdfUtil.getPages(extraParams, totalPages);

			// Antes de nada, miramos si nos han pedido que insertemos una pagina en blanco
			// para poner ahi la firma visible, en cuyo caso, una pagina con el mismo tamano
			// que la primera del documento
			if (pages.contains(Integer.valueOf(NEW_PAGE)) && signatureField == null) {
				stp.insertPage(totalPages + 1, pdfReader.getPageSizeWithRotation(1));
				// La pagina pasa a ser la nueva, que es la ultima,
				pages.remove(Integer.valueOf(NEW_PAGE));
				pages.add(Integer.valueOf(totalPages + 1));
			}

			// Comprobamos que la posicion indicada para la firma visible
			// se pueda estampar en al menos en una de las paginas indicadas
			// y que sus dimensiones no soprepasen fuera de la primera pagina
			// en la que se imprima
			PdfUtil.correctPositionSignature(pdfReader, pages, signaturePositionOnPage);
		}

		final PdfSignatureAppearance sap = stp.getSignatureAppearance();

		// La compresion solo para versiones superiores a la 4
		// Hacemos la comprobacion a "false", porque es el valor que deshabilita esta opcion
		if (pdfVersion > PDF_MIN_VERSION && !pdfA1
				&& !"false".equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.COMPRESS_PDF))) { //$NON-NLS-1$
			stp.setFullCompression();
		}

		// Si se ha configurado, permitimos que el lector de PDF muestre una marca junto a la firma
		final boolean includeQuestionMark = Boolean
				.parseBoolean(extraParams.getProperty(PdfExtraParams.INCLUDE_QUESTION_MARK));
		if (includeQuestionMark) {
			sap.setAcro6Layers(false);
			sap.setLayer4Text(PdfSignatureAppearance.questionMark);
		}
		else {
			sap.setAcro6Layers(true);
		}

		PdfUtil.enableLtv(stp);

		// Adjuntos
		PdfPreProcessor.attachFile(extraParams, stp, secureMode);

		// Imagenes
		PdfPreProcessor.addImage(extraParams, stp, pdfReader, secureMode);

		// Establecemos el render segun iText antiguo, varia en versiones modernas
		//TODO: Se podria configurar PdfSignatureAppearance.SignatureRenderGraphicAndDescription
		// para que se muestre la imagen y el texto uno al lado del otro, pero eso obligaria a
		// tener que retocar la previsualizacion en Autofirma. Tambien se deberia establecer el
		// campo SignatureGraphic en lugar de Image.
		sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);

		// Razon de firma
		if (reason != null) {
			sap.setReason(reason);
		}

		sap.setSignDate(signTime);

		sap.setCrypto(
				null,
				doNotUseCertChainOnPostSign ? null : certChain,
				null,
				null
		);

		// Localizacion en donde se produce la firma
		if (signatureProductionCity != null) {
			sap.setLocation(signatureProductionCity);
		}

		// Contacto del firmante
		if (signerContact != null) {
			sap.setContact(signerContact);
		}

		// Si se ha establecido una imagen de firma, se
		// elimina el texto por defecto
		if (rubric != null) {
			sap.setLayer2Text(""); //$NON-NLS-1$
			sap.setLayer4Text(""); //$NON-NLS-1$
		}

		// **************************
		// ** Texto en las capas ****
		// **************************

		if (PdfVisibleAreasUtils.isVisibleSignature(extraParams)) {

			// Capa 2
			if (layer2Text != null) {
				sap.setLayer2Text(layer2Text);
			}
			if (layer2Font != null) {
				sap.setLayer2Font(layer2Font);
			}

			// Capa 4
			if (layer4Text != null) {
				sap.setLayer4Text(layer4Text);
			}

			// Firma visible
			if (signaturePositionOnPage != null && signatureField == null) {

				try {
					// Si no hay que rotar la firma, agregamos la imagen de rubrica si procede y listo
					if (signatureRotation == 0) {

						// Rubrica de la firma
						if (rubric != null) {
							sap.setImage(rubric);

							// Establecemos que la imagen no se ajuste al campo
							// para que no se deforme
							sap.setImageScale(-1);
						}
					}
					// Si hay que rotar la firma, generamos una imagen con el texto y la imagen de
					// rubrica ya rotados y la agregamos
					else {

						final Image rotatedRubric = PdfVisibleAreasUtils.buildRotatedSignatureImage(
								stp,
								sap,
								signaturePositionOnPage,
								signatureRotation,
								rubric
								);

						// Eliminamos el texto de la apariencia, ya que este se habra impreso en la imagen
						// de rubrica
						sap.setLayer2Text(""); //$NON-NLS-1$

						// Agregamos la imagen con el texto y/o rubrica ya rotadas
						sap.setImage(rotatedRubric);
					}

					// Configuramos la firma visible
					sap.setVisibleSignature(signaturePositionOnPage, pages != null ? pages.get(0).intValue() : pdfReader.getNumberOfPages(), null);
				}
				catch (final InvalidPageNumberException e) {
					LOGGER.warning("Numero de pagina incorrecto. La firma no sera visible: " + e); //$NON-NLS-1$
				}
				catch (final DocumentException e) {
					throw new IOException("Error en la insercion de la firma rotada: " + e, e); //$NON-NLS-1$
				}
			}
			// Firma en un campo preexistente (visile o invisible)
			else if (signatureField != null) {
				sap.setVisibleSignature(signatureField);
			}
		}

		// ***************************
		// ** Fin texto en las capas *
		// ***************************

		final PdfSignature dic = new PdfSignature(
				PdfName.ADOBE_PPKLITE,
				signatureSubFilter != null && !signatureSubFilter.isEmpty()
						? new PdfName(signatureSubFilter)
						: new PdfName(AOSignConstants.PADES_SUBFILTER_BES)
		);

		// Fecha de firma
		if (sap.getSignDate() != null) {
			dic.setDate(new PdfDate(sap.getSignDate()));
		}

		if (certChain != null && certChain.length > 0 && !doNotUseCertChainOnPostSign) {
			dic.setName(PdfPKCS7.getSubjectFields((X509Certificate) certChain[0]).getField("CN")); //$NON-NLS-1$
		}

		if (sap.getReason() != null) {
			dic.setReason(sap.getReason());
		}

		// Lugar de la firma
		if (sap.getLocation() != null) {
			dic.setLocation(sap.getLocation());
		}

		// Contacto del firmante
		if (sap.getContact() != null) {
			dic.setContact(sap.getContact());
		}

		sap.setCryptoDictionary(dic);

		// Certificacion del PDF (NOT_CERTIFIED = 0, CERTIFIED_NO_CHANGES_ALLOWED = 1,
		// CERTIFIED_FORM_FILLING = 2, CERTIFIED_FORM_FILLING_AND_ANNOTATIONS = 3)
		if (certificationLevel != UNDEFINED) {
			sap.setCertificationLevel(certificationLevel);
		}

		// Version del PDF
		if (pdfVersion != UNDEFINED) {
			stp.getWriter().setPdfVersion(Integer.toString(pdfVersion).toCharArray()[0]);
		}

		// Reservamos el espacio necesario en el PDF para insertar la firma
		final HashMap<PdfName, Integer> exc = reserveSignSizes(extraParams);

		try {
			sap.preClose(exc, signTime, pages);
		}
		catch (final DocumentException e) {
			LOGGER.severe("Error al estampar la firma: " + e); //$NON-NLS-1$
			throw new AOException("Error al estampar la firma", e); //$NON-NLS-1$
		}

		final PdfObject pdfObject = ((com.aowagie.text.pdf.PdfStamperImp) stp.getWriter()).getFileID();

		return new PdfTriPhaseSession(sap, baos, new String(pdfObject.getBytes()));
	}

    private static String getDefaultLayer2Text(final boolean hasReason, final boolean hasLocation) {
    	final StringBuilder buf = new StringBuilder();
    	buf.append("Firmado por ") //$NON-NLS-1$
    	.append(PdfVisibleAreasUtils.LAYERTEXT_TAG_SUBJECTCN).append('\n')
    	.append("Fecha: ").append(PdfVisibleAreasUtils.LAYERTEXT_TAG_DATE_PREFIX).append("=dd/MM/yyyy HH:mm:ss z$$"); //$NON-NLS-1$ //$NON-NLS-2$
    	if (hasReason) {
    		buf.append('\n').append("Motivo: ").append(PdfVisibleAreasUtils.LAYERTEXT_TAG_REASON); //$NON-NLS-1$
    	}
    	if (hasLocation) {
    		buf.append('\n').append("Lugar de firma: ").append(PdfVisibleAreasUtils.LAYERTEXT_TAG_LOCATION); //$NON-NLS-1$
    	}
    	return buf.toString();
    }

    /**
     * Calcula el espacio que se debe reservar espacio dentro del PDF para almacenar firmas.
     * @param extraParams Conjunto de par&aacute;metros donde se informan el valor de las
     * propiedades de reserva de espacio.
     * @return Mapa con las propiedades de reserva de exclusiones de firma.
     */
    public static HashMap<PdfName, Integer> reserveSignSizes(final Properties extraParams) {

    	final int sizeParamInt = getReservedSignatureSized(extraParams);

		final HashMap<PdfName, Integer> exc = new HashMap<>();
		exc.put(PdfName.CONTENTS, Integer.valueOf(sizeParamInt * 2 + 2));

		return exc;
    }

    /**
     * Recupera el tama&ntilde;o que se ha configurado reservar para la firma o el por defecto si
     * no se indic&oacute;.
     * @param extraParams Configuraci&oacute;n de la operaci&oacute;n de firma.
     * @return Tama&ntilde;o que se reservar&aacute; para la firma.
     */
    public static int getReservedSignatureSized(final Properties extraParams) {
    	int sizeParamInt = CSIZE;
    	final String reservedSizeParam = extraParams.getProperty(PdfExtraParams.SIGN_RESERVED_SIZE);
    	if (reservedSizeParam != null && !reservedSizeParam.isEmpty()) {
    		try {
    			sizeParamInt = Integer.parseInt(reservedSizeParam);
    		} catch (final NumberFormatException e) {
    			LOGGER.warning(
    					"El valor de la propiedad " + PdfExtraParams.SIGN_RESERVED_SIZE //$NON-NLS-1$
    					+ " no es valido, se le asignara el valor por defecto (" + CSIZE + "):" + e); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    	}
    	return sizeParamInt;
    }
}
