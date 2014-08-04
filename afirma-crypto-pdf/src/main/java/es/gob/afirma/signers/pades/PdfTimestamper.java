package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.security.MessageDigest;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

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
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;
import es.gob.afirma.signers.tsp.pkcs7.TsaRequestExtension;

final class PdfTimestamper {

	private static final String TIMESTAMP_DIGEST_ALGORITHM = "SHA-512"; //$NON-NLS-1$
	private static final String TIMESTAMP_SUBFILTER = "ETSI.RFC3161"; //$NON-NLS-1$

	private static final int CSIZE = 27000;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private PdfTimestamper() {
		// No instanciable
	}

	static byte[] timestampPdf(final byte[] inPDF,
			                   final Properties extraParams,
			                   final Calendar signTime) throws AOException, IOException {
    	// Comprobamos si se ha pedido un sello de tiempo
    	if (extraParams != null) {
    		final String tsa = extraParams.getProperty("tsaURL"); //$NON-NLS-1$
            if (tsa != null) {
            	// Obtenemos el sellador de tiempo
                final String tsaHashAlgorithm = extraParams.getProperty("tsaHashAlgorithm"); //$NON-NLS-1$
                final CMSTimestamper timestamper = getCmsTimestamper(extraParams);

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

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        		final PdfStamper stp;
        		try {
        			stp = PdfStamper.createSignature(
        				pdfReader, // PDF de entrada
        				baos,      // Salida
        				'\0',      // Mantener version
        				null,      // No crear temporal
        				true,      // Siempre creo revision
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

        		//TODO:QUITAR???
//        		// PAdES parte 3 seccion 4.7 - Habilitacion para LTV
//        		stp.getWriter().addDeveloperExtension(
//    				new PdfDeveloperExtension(
//	        			new PdfName("ESIC"), //$NON-NLS-1$
//	        			PdfWriter.PDF_VERSION_1_7,
//	        			1
//					)
//				);

        		sap.setAcro6Layers(true);
        		sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
        		sap.setSignDate(signTime);

        		// Gestion de los cifrados
        		if (pdfReader.isEncrypted() && (ownerPassword != null || userPassword != null)) {
        			if (Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("avoidEncryptingSignedPdfs"))) { //$NON-NLS-1$
        				LOGGER.info(
        					"Aunque el PDF original estaba encriptado no se encriptara el PDF firmado (se establecio el indicativo 'avoidEncryptingSignedPdfs')" //$NON-NLS-1$
        				);
        			}
        			else {
        				LOGGER.info(
        					"El PDF original estaba encriptado, se intentara encriptar tambien el PDF firmado" //$NON-NLS-1$
        				);
        				try {
        					stp.setEncryption(
        						ownerPassword != null ? ownerPassword.getBytes() : null,
        						userPassword != null ? userPassword.getBytes() : null,
        						pdfReader.getPermissions(),
        						pdfReader.getCryptoMode()
        					);
        				}
        				catch (final DocumentException de) {
        					LOGGER.warning(
        						"No se ha podido cifrar el PDF destino, se escribira sin contrasena: " + de //$NON-NLS-1$
        					);
        				}
        			}
        		}

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

        		// Obtenemos el token TSP
        		final byte[] tspToken;
        		try {
            		tspToken = timestamper.getTimeStampToken(
        				MessageDigest.getInstance(
    						tsaHashAlgorithm != null ? tsaHashAlgorithm : TIMESTAMP_DIGEST_ALGORITHM
    	        		).digest(
    	    				original
    					),
    					tsaHashAlgorithm != null ? tsaHashAlgorithm : TIMESTAMP_DIGEST_ALGORITHM,
						signTime
    				);
            	}
            	catch(final Exception e) {
            		throw new AOException("Error en la obtencion del sello de tiempo PAdES: " + e, e); //$NON-NLS-1$
            	}

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

    private static CMSTimestamper getCmsTimestamper(final Properties extraParams) throws AOException {
    	final String tsa = extraParams.getProperty("tsaURL"); //$NON-NLS-1$
        URI tsaURL;
        try {
            tsaURL = new URI(tsa);
        }
        catch(final Exception e) {
            throw new AOException(
        		"Se ha indicado una URL de TSA invalida (" + tsa + "), no se anadira sello de tiempo: " + e, e //$NON-NLS-1$ //$NON-NLS-2$
    		);
        }
        final String tsaPolicy = extraParams.getProperty("tsaPolicy"); //$NON-NLS-1$
        if (tsaPolicy == null) {
        	throw new AOException(
    			"Se ha indicado una URL de TSA pero no una politica" //$NON-NLS-1$
			);
        }

        final TsaRequestExtension[] extensions;
		try {
			extensions = extraParams.getProperty("tsaExtensionOid") != null && extraParams.getProperty("tsaExtensionValueBase64") != null ? //$NON-NLS-1$ //$NON-NLS-2$
				new TsaRequestExtension[] {
					new TsaRequestExtension(
						extraParams.getProperty("tsaExtensionOid"), //$NON-NLS-1$
						Boolean.getBoolean(extraParams.getProperty("tsaExtensionCritical", "false")), //$NON-NLS-1$ //$NON-NLS-2$
						Base64.decode(extraParams.getProperty("tsaExtensionValueBase64")) //$NON-NLS-1$
					)
				} :
			null;
		}
		catch (final IOException e) {
			throw new AOException("Se han indicado extensiones incorrectas en la peticion de sello de tiempo: " + e, e); //$NON-NLS-1$
		}

        return new CMSTimestamper(
            !Boolean.FALSE.toString().equalsIgnoreCase(extraParams.getProperty("tsaRequireCert")),  //$NON-NLS-1$
            tsaPolicy,
            tsaURL,
            extraParams.getProperty("tsaUsr"),  //$NON-NLS-1$
            extraParams.getProperty("tsaPwd"), //$NON-NLS-1$
            extensions
        );

    }


}
