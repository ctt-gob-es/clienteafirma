package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Properties;

import com.lowagie.text.DocumentException;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignature;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Sellador de tiempo para documentos PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfTimestamper {

	private static final String TIMESTAMP_SUBFILTER = "ETSI.RFC3161"; //$NON-NLS-1$

	private static final int CSIZE = 27000;

	private PdfTimestamper() {
		// No instanciable
	}

	/** Aplica un sello de tiempo a un PDF.
	 * @param inPDF PDF de entrada.
	 * @param extraParams Par&aacute;metros de la TSA.
	 * @param signTime Tiempo para el sello.
	 * @return PDF con el sello de tiempo aplicado.
	 * @throws AOException Si hay problemas durante el proceso.
	 * @throws IOException Si hay problemas en el tratamiento de datos. */
	public static byte[] timestampPdf(final byte[] inPDF,
			                   final Properties extraParams,
			                   final Calendar signTime) throws AOException, IOException {
    	// Comprobamos si se ha pedido un sello de tiempo
    	if (extraParams != null) {
    		final String tsa = extraParams.getProperty("tsaURL"); //$NON-NLS-1$
            if (tsa != null) {

        		// Contrasena del propietario del PDF
        		final String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$

        		// Contrasena del usuario del PDF
        		final String userPassword =  extraParams.getProperty("userPassword"); //$NON-NLS-1$

                // Y procesamos normalmente el PDF
                final PdfReader pdfReader = PdfUtil.getPdfReader(
            		inPDF,
            		ownerPassword,
            		userPassword,
            		Boolean.getBoolean(extraParams.getProperty("headLess")) //$NON-NLS-1$
        		);

            	// Comprobamos el nivel de certificacion del PDF
                PdfUtil.checkPdfCertification(pdfReader.getCertificationLevel(), extraParams);

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        		final PdfStamper stp;
        		try {
        			stp = PdfStamper.createSignature(
        				pdfReader, // PDF de entrada
        				baos,      // Salida
        				'\0',      // Mantener version
        				null,      // No crear temporal
        				Boolean.parseBoolean(extraParams.getProperty("alwaysCreateRevision", "false")) || pdfReader.getAcroFields().getSignatureNames().size() > 0, //$NON-NLS-1$ //$NON-NLS-2$
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
        		stp.setFullCompression();

        		PdfUtil.enableLtv(stp);

        		sap.setAcro6Layers(true);
        		sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        		sap.setSignDate(signTime);

        		// Gestion de los cifrados
        		PdfUtil.managePdfEncryption(stp, pdfReader, ownerPassword, userPassword, extraParams);

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

	private static byte[] getTspToken(final Properties extraParams, final byte[] original, final Calendar signTime) throws AOException {

    	// Obtenemos el sellador de tiempo
		final TsaParams tsaParams = new TsaParams(extraParams);
        final CMSTimestamper timestamper = new CMSTimestamper(tsaParams);

		// Obtenemos el token TSP
		try {
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
    	catch(final Exception e) {
    		throw new AOException("Error en la obtencion del sello de tiempo PAdES: " + e, e); //$NON-NLS-1$
    	}
	}

}
