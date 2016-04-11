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
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.DocumentException;
import com.aowagie.text.Image;
import com.aowagie.text.Rectangle;
import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.PdfDate;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfObject;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfSignature;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ui.AOUIFactory;

/** Gestor del n&uacute;cleo de firma PDF. Esta clase realiza las operaciones necesarias tanto para
 * la firma monof&aacute;sica PAdES como para las trif&aacute;sicas de una forma unificada, pero
 * &uacute;nicamente en lo referente al formato PDF, sin entrar en la parte CAdES o PKCS#7
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfSessionManager {

    /** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
    static final int LAST_PAGE = -1;

    static final int NEW_PAGE = -2;

    private static final int UNDEFINED = -1;

    private static final int CSIZE = 27000;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    private static final int PDF_MAX_VERSION = 7;
    private static final int PDF_MIN_VERSION = 2;

    private PdfSessionManager() {
    	// No permitimos la instanciacion
    }

    /** Obtiene los datos PDF relevantes en cuanto a las firmas electr&oacute;nicas, consistentes en los datos
     * a ser firmados con CAdES o PKCS#7 y los metadatos necesarios para su correcta inserci&oacute;n en el PDF.
     * @param pdfBytes Documento PDF que se desea firmar
     * @param certChain Cadena de certificados del firmante
     * @param signTime Hora de la firma
     * @param extraParams Par&aacute;metros adicionales de la firma
     * @return Datos PDF relevantes en cuanto a las firmas electr&oacute;nicas
     * @throws IOException En caso de errores de entrada / salida.
     * @throws InvalidPdfException Si el formato del documento no es v&aacute;lido.
     * @throws AOException En caso de que ocurra cualquier otro tipo de error.
     */
    public static PdfTriPhaseSession getSessionData(final byte[] pdfBytes,
                                                    final Certificate[] certChain,
                                                    final Calendar signTime,
                                                    final Properties extraParams) throws IOException,
                                                                                         InvalidPdfException,
                                                                                         AOException {

		// *********************************************************************************************************************
		// **************** LECTURA PARAMETROS ADICIONALES *********************************************************************
		// *********************************************************************************************************************

    	// Imagen de la rubrica
		final Image rubric = PdfPreProcessor.getImage(extraParams.getProperty(PdfExtraParams.SIGNATURE_RUBRIC_IMAGE));

		// Motivo de la firma
		final String reason = extraParams.getProperty(PdfExtraParams.SIGN_REASON);

		// Nombre del campo de firma preexistente en el PDF a usar
		final String signatureField = extraParams.getProperty(PdfExtraParams.SIGNATURE_FIELD);

		// Lugar de realizacion de la firma
		final String signatureProductionCity = extraParams.getProperty(PdfExtraParams.SIGNATURE_PRODUCTION_CITY);

		// Datos de contacto (correo electronico) del firmante
		final String signerContact = extraParams.getProperty(PdfExtraParams.SIGNER_CONTACT);

		// Pagina donde situar la firma visible
		int page = LAST_PAGE;
		final String pageStr = extraParams.getProperty(PdfExtraParams.SIGNATURE_PAGE, "-1"); //$NON-NLS-1$
		if ("append".equalsIgnoreCase(pageStr)) { //$NON-NLS-1$
			page = NEW_PAGE;
		}
		else {
			try {
				page = Integer.parseInt(pageStr);
			}
			catch (final Exception e) {
				LOGGER.warning(
					"Se ha indicado un numero de pagina invalido ('" + pageStr + "'), se usara la ultima pagina: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		// Nombre del subfiltro de firma en el diccionario PDF
		final String signatureSubFilter = extraParams.getProperty(PdfExtraParams.SIGNATURE_SUBFILTER);

		// Nivel de certificacion del PDF
		int certificationLevel;
		try {
			certificationLevel = extraParams.getProperty(PdfExtraParams.CERTIFICATION_LEVEL) != null ?
				Integer.parseInt(extraParams.getProperty(PdfExtraParams.CERTIFICATION_LEVEL)) :
					UNDEFINED;
		}
		catch(final Exception e) {
			certificationLevel = UNDEFINED;
		}

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

		// *****************************
		// **** Texto firma visible ****

		// Texto en capa 4
		final String layer4Text = PdfVisibleAreasUtils.getLayerText(
			extraParams.getProperty(PdfExtraParams.LAYER4_TEXT),
			(X509Certificate) certChain[0],
			signTime
		);

		// Texto en capa 2
		final String layer2Text = PdfVisibleAreasUtils.getLayerText(
			extraParams.getProperty(PdfExtraParams.LAYER2_TEXT),
			(X509Certificate) certChain[0],
			signTime
		);

		// Tipo de letra en capa 2
		int layer2FontFamily;
		try {
			layer2FontFamily = extraParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY) != null ?
				Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTFAMILY)) :
					-1;
		}
		catch(final Exception e) {
			layer2FontFamily = UNDEFINED;
		}

		// Tamano del tipo de letra en capa 2
		int layer2FontSize;
		try {
			layer2FontSize = extraParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE) != null ?
				Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTSIZE)) :
					-1;
		}
		catch(final Exception e) {
			layer2FontSize = UNDEFINED;
		}

		// Estilo del tipo de letra en capa 2
		int layer2FontStyle;
		try {
			layer2FontStyle = extraParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE) != null ?
				Integer.parseInt(extraParams.getProperty(PdfExtraParams.LAYER2_FONTSTYLE)) :
					-1;
		}
		catch(final Exception e) {
			layer2FontStyle = UNDEFINED;
		}

		// Color del tipo de letra en capa 2
		final String layer2FontColor = extraParams.getProperty(PdfExtraParams.LAYER2_FONTCOLOR);

		// ** Fin texto firma visible **
		// *****************************

		// *********************************************************************************************************************
		// **************** FIN LECTURA PARAMETROS ADICIONALES *****************************************************************
		// *********************************************************************************************************************

		byte[] inPDF;
		try {
			inPDF = XmpHelper.addSignHistoryToXmp(pdfBytes, signTime);
		}
		catch (final Exception e1) {
			LOGGER.warning("No ha podido registrarse la firma en el historico XMP: " + e1); //$NON-NLS-1$
			inPDF = pdfBytes;
		}

		final PdfReader pdfReader = PdfUtil.getPdfReader(
			inPDF,
			extraParams,
			Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS))
		);

		// **************************************************************
		// ***** Comprobaciones y parametros necesarios para PDF-A1 *****
		final byte[] xmpBytes = pdfReader.getMetadata();
		final boolean pdfA1 = PdfUtil.isPdfA1(xmpBytes);
		if (pdfA1) {
			LOGGER.info("Detectado PDF-A1, no se comprimira el PDF"); //$NON-NLS-1$
		}

		// *** Fin comprobaciones y parametros necesarios para PDF-A1 ***
		// **************************************************************

		PdfUtil.checkPdfCertification(pdfReader.getCertificationLevel(), extraParams);

		if (PdfUtil.pdfHasUnregisteredSignatures(pdfReader) && !Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.ALLOW_COSIGNING_UNREGISTERED_SIGNATURES))) {
			throw new PdfHasUnregisteredSignaturesException();
		}

		// Los derechos van firmados por Adobe, y como desde iText se invalidan
		// es mejor quitarlos
		pdfReader.removeUsageRights();

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		// Activar el atributo de "agregar firma" (quinto parametro del metodo
		// "PdfStamper.createSignature") hace que se cree una nueva revision del
		// documento y evita que las firmas previas queden invalidadas.
		// Sin embargo, este procedimiento no funciona cuando los PDF contienen informacion
		// despues de la ultima marca %%EOF, aspecto no permitido en PDF 1.7 (ISO 32000-1:2008)
		// pero si en PDF 1.3 (Adobe) y que se da con frecuencia en PDF generados con bibliotetcas
		// de software libre como QPDF.
		//
		//  Especificacion PDF 1.3
		//   3.4.4, "File Trailer"
		//     Acrobat viewers require only that the %%EOF marker appear somewhere within
		//     the last 1024 bytes of the file.
		//
		//  Especificacion PDF 1.7
		//   7.5.5. File Trailer
		//     The trailer of a PDF file enables a conforming reader to quickly find the
		//     cross-reference table and certain special objects. Conforming readers should read a
		//     PDF file from its end. The last line of the file shall contain only the end-of-file
		//     marker, %%EOF.
        //
		// Para aceptar al menos en algunos casos PDF 1.3 (son aun muy frecuentes, especialmente
		// en archivos, lo mantendremos desactivado para la primera firma y activado para las
		// subsiguientes.
		//
		// No obstante, el integrador puede siempre forzar la creacion de revisiones mediante
		// el parametro "alwaysCreateRevision".
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
		catch (final DocumentException e) {
			LOGGER.severe("Error al crear la firma para estampar: " + e); //$NON-NLS-1$
			throw new AOException("Error al crear la firma para estampar", e); //$NON-NLS-1$
		}
		catch(final BadPasswordException e) {
	        // Comprobamos que el signer esta en modo interactivo, y si no lo
            // esta no pedimos contrasena por dialogo, principalmente para no interrumpir un firmado por lotes
            // desatendido
            if (Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.HEADLESS))) {
                throw new BadPdfPasswordException(e);
            }
            // La contrasena que nos han proporcionada no es buena o no nos
            // proporcionaron ninguna
            final String userPwd = new String(
                AOUIFactory.getPassword(
                    extraParams.getProperty(PdfExtraParams.USER_PASSWORD) == null ? CommonPdfMessages.getString("AOPDFSigner.0") : CommonPdfMessages.getString("AOPDFSigner.1"), //$NON-NLS-1$ //$NON-NLS-2$
                    null
                )
            );
            if ("".equals(userPwd)) { //$NON-NLS-1$
                throw new AOCancelledOperationException(
                    "Entrada de contrasena de PDF cancelada por el usuario", e //$NON-NLS-1$
                );
            }
            extraParams.put("userPassword", userPwd); //$NON-NLS-1$
            return getSessionData(inPDF, certChain, signTime, extraParams);
		}

		// Antes de nada, miramos si nos han pedido que insertemos una pagina en blanco para poner ahi la firma
		// visible

		// Posicion de la firma
		final Rectangle signaturePositionOnPage = getSignaturePositionOnPage(extraParams);
		if (page == NEW_PAGE && signaturePositionOnPage != null && signatureField == null) {
			stp.insertPage(pdfReader.getNumberOfPages() + 1, pdfReader.getPageSizeWithRotation(1));
			// La pagina pasa a ser la nueva, que es la ultima,
			page = LAST_PAGE;
		}

		// Aplicamos todos los atributos de firma
		final PdfSignatureAppearance sap = stp.getSignatureAppearance();
		// La compresion solo para versiones superiores a la 4
		// Hacemos la comprobacion a "false", porque es el valor que deshabilita esta opcion
		if (pdfVersion > PDF_MIN_VERSION && !pdfA1 && !"false".equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.COMPRESS_PDF))) { //$NON-NLS-1$
			stp.setFullCompression();
		}
		sap.setAcro6Layers(true);

		PdfUtil.enableLtv(stp);

		// Adjuntos
		PdfPreProcessor.attachFile(extraParams, stp);

		// Imagenes
		PdfPreProcessor.addImage(extraParams, stp, pdfReader);

		// Establecemos el render segun iText antiguo, varia en versiones modernas
		sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);

		// Razon de firma
		if (reason != null) {
			sap.setReason(reason);
		}

		sap.setSignDate(signTime);

		// Pagina en donde se imprime la firma
		if (page == LAST_PAGE) {
			page = pdfReader.getNumberOfPages();
		}

		if (signaturePositionOnPage != null && signatureField == null) {
			sap.setVisibleSignature(signaturePositionOnPage, page, null);
		}
		else if (signatureField != null) {
			sap.setVisibleSignature(signatureField);
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
		if (rubric != null) {
			sap.setImage(rubric);
			sap.setLayer2Text(""); //$NON-NLS-1$
			sap.setLayer4Text(""); //$NON-NLS-1$
		}

		// **************************
		// ** Texto en las capas ****
		// **************************

		// Capa 2
		if (layer2Text != null) {
			sap.setLayer2Text(layer2Text);
			sap.setLayer2Font(
				PdfVisibleAreasUtils.getFont(
					layer2FontFamily,
					layer2FontSize,
					layer2FontStyle,
					layer2FontColor
				)
			);
		}

		// Capa 4
		if (layer4Text != null) {
			sap.setLayer4Text(layer4Text);
		}

		// ***************************
		// ** Fin texto en las capas *
		// ***************************

		sap.setCrypto(null, certChain, null, null);

		final PdfSignature dic = new PdfSignature(
			PdfName.ADOBE_PPKLITE,
			signatureSubFilter != null && !signatureSubFilter.isEmpty() ? new PdfName(signatureSubFilter) : PdfName.ADBE_PKCS7_DETACHED
		);

		// Fecha de firma
		if (sap.getSignDate() != null) {
			dic.setDate(new PdfDate(sap.getSignDate()));
		}

		dic.setName(PdfPKCS7.getSubjectFields((X509Certificate) certChain[0]).getField("CN")); //$NON-NLS-1$

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
		final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
		exc.put(PdfName.CONTENTS, Integer.valueOf(CSIZE * 2 + 2));

		try {
			sap.preClose(exc, signTime);
		} catch (final DocumentException e) {
			LOGGER.severe("Error al estampar la firma: " + e); //$NON-NLS-1$
			throw new AOException("Error al estampar la firma", e); //$NON-NLS-1$
		}

		final PdfObject pdfObject = ((com.aowagie.text.pdf.PdfStamperImp) stp.getWriter()).getFileID();

		return new PdfTriPhaseSession(sap, baos, new String(pdfObject.getBytes()));
    }

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse
     * la firma. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a
     * arriba.
     * @param extraParams Conjunto de propiedades con las coordenadas del rect&aacute;ngulo
     * @return  Rect&aacute;ngulo que define la posici&oacute;n de la p&aacute;gina en donde
     *          debe agregarse la firma*/
    private static Rectangle getSignaturePositionOnPage(final Properties extraParams) {
    	return PdfPreProcessor.getPositionOnPage(extraParams, "signature"); //$NON-NLS-1$
    }

}
