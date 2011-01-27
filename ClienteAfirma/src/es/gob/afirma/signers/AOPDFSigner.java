/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import javax.activation.MimeType;
import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.MemoryCacheImageOutputStream;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.cms.CMSSignedGenerator;
import org.ietf.jgss.Oid;

import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.AcroFields;
import com.itextpdf.text.pdf.PdfDate;
import com.itextpdf.text.pdf.PdfDictionary;
import com.itextpdf.text.pdf.PdfName;
import com.itextpdf.text.pdf.PdfPKCS7;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfSignature;
import com.itextpdf.text.pdf.PdfSignatureAppearance;
import com.itextpdf.text.pdf.PdfStamper;
import com.itextpdf.text.pdf.PdfString;

import es.gob.afirma.beans.AOSignInfo;
import es.gob.afirma.beans.AOSimpleSignInfo;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signers.aobinarysignhelper.GenCadesEPESSignedData;
import es.gob.afirma.signers.aobinarysignhelper.P7ContentSignerParameters;

/**
 * Clase para la firma electr&oacute;nica de ficheros Adobe PDF.<p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt>applySystemDate</dt>
 *  	<dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario
 *  <dt>signReason</dt>
 *  	<dd>Raz&oacute;n por la que se realiza la firma</dd>
 *  <dt>signField</dt>
 *  	<dd>Nombre del campo en donde insertar la firma</dd>
 *  <dt>signatureProductionCity</dt>
 *  	<dd>Ciudad en la que se realiza la firma</dd>
 *  <dt>signerContact</dt>
 *      <dd>Contacto del firmante</dd>
 *  <dt>signaturePage</dt>
 *      <dd>P&aacute;gina del PDF donde insertar la firma</dd>
 *  <dt>policyIdentifier</dt>
 *  	<dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 *  <dt>policyQualifier</dt>
 *  	<dd>OID cualificador de la pol&iacute;tica de firma</dd>  
 * </dl> 
 */
public final class AOPDFSigner implements AOSigner {

   	/** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
   	public static final int LAST_PAGE = -666;

   	/** Mimetype asignado a los ficheros PDF. */
   	private static final String MIMETYPE_PDF = "application/pdf";
   	
    public byte[] sign(final InputStream file, 
    		           final String algorithm, 
    		           final PrivateKeyEntry keyEntry, 
    		           final X509Certificate cert, 
    		           Properties extraParams) throws AOException {
    	
    	String signAlgorithm = algorithm;
    	
    	if ( !algorithm.equals(AOConstants.SIGN_ALGORITHM_MD5WITHRSA)
    			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA1WITHRSA)
    			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA256WITHRSA)
    			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA384WITHRSA)
    			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA512WITHRSA)) {
    		
    		Logger.getLogger("es.gob.afirma").warning("El algoritmo '"+algorithm+"' no esta soportado por PDF, se usara SHA-1");
    		signAlgorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
    	}
    	
    	if (extraParams == null) extraParams = new Properties();
        try {
            return signPDF(
                    this.getLowagieDigestOid(signAlgorithm),
                    keyEntry,
                    AOUtil.getDataFromInputStream(file),
                    extraParams,
                    signAlgorithm
            );
        } catch (com.itextpdf.text.exceptions.InvalidPdfException e) {
        	throw new AOFormatFileException("El documento no era un PDF valido");
        } catch (final AOException e) {
        	throw e;
        } catch (final Throwable e) {
        	e.printStackTrace();
        	throw new AOException("Error firmando el PDF", e);
        }
    }

    public byte[] cosign(final InputStream file, 
    		             final InputStream signFile, 
    		             final String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             final X509Certificate cert, 
    		             final Properties extraParams) throws AOException {
        return sign(signFile, algorithm, keyEntry, cert, extraParams);
    }
    
    public byte[] cosign(final InputStream signFile, 
    		             final String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             final X509Certificate cert, 
    		             final Properties extraParams) throws AOException {
        return sign(signFile, algorithm, keyEntry, cert, extraParams);
    }
    
    public byte[] countersign(final InputStream signFile, 
    		                  final String algorithm, 
    		                  final CounterSignTarget targetType, 
    		                  final Object[] targets, 
    		                  final PrivateKeyEntry keyEntry, 
    		                  final X509Certificate cert, 
    		                  final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros PDF");
    }
    
    public String getSignedName(final String originalName, final String inText) {
    	
		final String inTextInt = (inText != null ? inText : ""); 
    	
        if (originalName == null) return "signed.pdf";
        if (originalName.endsWith(".pdf")) return originalName.replace(".pdf", inTextInt + ".pdf");
        if (originalName.endsWith(".PDF")) return originalName.replace(".PDF", inTextInt + ".pdf");
        return originalName + inTextInt + ".pdf";
    }
    
    public void setDataObjectFormat(final String description, 
    		                        final Oid objectIdentifier, 
    		                        final MimeType mimeType, 
    		                        final String encoding) {}

    public TreeModel getSignersStructure(final InputStream sign, final boolean asSimpleSignInfo) {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Datos");
        try {
            AcroFields af = new PdfReader(sign).getAcroFields();
            ArrayList<?> names = af.getSignatureNames();
            for (int i = 0; i < names.size(); ++i) {
            		PdfPKCS7 pcks7 = af.verifySignature(names.get(i).toString());
            		if (asSimpleSignInfo) {
            			AOSimpleSignInfo ssi = new AOSimpleSignInfo(new X509Certificate[] { pcks7.getSigningCertificate() }, pcks7.getSignDate().getTime());
            			
            			// Extraemos el PKCS1 de la firma
            			Field digestField = Class.forName("com.itextpdf.text.pdf.PdfPKCS7").getDeclaredField("digest");
            			digestField.setAccessible(true);
            			Object pkcs1Object = digestField.get(pcks7);
            			if(pkcs1Object != null && pkcs1Object instanceof byte[]) {
            				ssi.setPkcs1((byte[])pkcs1Object);
            			}
            			
            			root.add(new DefaultMutableTreeNode(ssi));
            		}
                else root.add(new DefaultMutableTreeNode((AOUtil.getCN(pcks7.getSigningCertificate()))));
            }
        }
        catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe(
                "No se ha podido obtener toda la informacion de los firmantes del PDF, el arbol puede estar incompleto: " + e
            );
            e.printStackTrace();
        }
        return new DefaultTreeModel(root);
    }
    
    public boolean isSign(final InputStream is) {
        if(is == null) {
            Logger.getLogger("es.gob.afirma").warning(
        		"Se han introducido datos nulos para su comprobacion"
    		);
            return false;
        }
        return isPdfFile(is);
    }
    
    private boolean isPdfFile(final InputStream is) {
        try {
            new PdfReader(is);
        }
        catch (final Throwable e) {
            return false;
        }
        return true;
    }
    
    public boolean isValidDataFile(final InputStream is) {
        if(is == null) {
            Logger.getLogger("es.gob.afirma").warning(
        		"Se han introducido datos nulos para su comprobacion"
    		);
            return false;
        }
        return isPdfFile(is);
    }
    
    private String getLowagieDigestOid(String algorithm) {
        
        String digestOid = CMSSignedGenerator.DIGEST_SHA1;  // SHA1 es el algoritmo por defecto
        if(algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA1WITHRSA)) {
            digestOid = CMSSignedGenerator.DIGEST_SHA1;
        } else if(algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA256WITHRSA)) {
            digestOid = CMSSignedGenerator.DIGEST_SHA256;
        } else if(algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA384WITHRSA)) {
            digestOid = CMSSignedGenerator.DIGEST_SHA384;
        } else if(algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA512WITHRSA)) {
            digestOid = CMSSignedGenerator.DIGEST_SHA512;
        } else if(algorithm.equals(AOConstants.SIGN_ALGORITHM_MD5WITHRSA)) {
            digestOid = CMSSignedGenerator.DIGEST_MD5;
        } else {
            Logger.getLogger("es.gob.afirma").warning("El algoritmo '"+algorithm+"' no esta soportado por PDF, se usara SHA-1");
        }
        
        return digestOid;
    }
    
    /**
     * Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse la firma. La
     * medida de posicionamiento es el p&iacute;xel y se cuenta en el eje horizontal de izquierda
     * a derecha y en el vertical de abajo a arriba.
     */
    private Rectangle getSignaturePositionOnPage(final Properties extraParams) {
    	
    	try {
    		return new Rectangle(
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftX")), 
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftY")), 
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightX")), 
				Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightY"))
			);
    	} catch(Throwable e) {
    		return null;
    	}

    }
        
    private byte[] signPDF(final String lowagieDigestOid, 
    		final PrivateKeyEntry ke, 
    		final byte[] inPDF,
    		Properties extraParams,
    		final String algorithm) throws Throwable {

    	if (extraParams == null) extraParams = new Properties();
    	final boolean useSystemDateTime = Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true"));
    	final String reason = extraParams.getProperty("signReason");
    	final String signField = extraParams.getProperty("signField");
    	final String signatureProductionCity = extraParams.getProperty("signatureProductionCity");
    	final String signerContact = extraParams.getProperty("signerContact");
    	final String policyIdentifier = extraParams.getProperty("policyIdentifier");
    	final String policyQualifier = extraParams.getProperty("policyQualifier");
    	int page = 1;
    	try {
    		page = Integer.parseInt(extraParams.getProperty("signaturePage"));
    	}
    	catch(final Throwable e) { /* Se deja la pagina tal y como esta */ };

      /*
       * Si se solicito una firma explicita, advertimos no son compatibles con PDF y se
       * ignorara esta configuracion
       */
      if (extraParams != null && extraParams.containsKey("mode")
      		&& extraParams.getProperty("mode").equals(AOConstants.SIGN_MODE_EXPLICIT)) {
      			Logger.getLogger("es.gob.afirma").warning(
      					"El formato de firma PDF no soporta el modo de firma explicita, " +
      					"se ignorara esta configuracion");
      }
    	
    	
    	final PdfReader pdfReader = new PdfReader(inPDF);
    	final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    	
    	// Activar el atributo de "agregar firma" (cuarto parametro del metodo
    	// "PdfStamper.createSignature") hace que se cree una nueva revision del documento
    	// y evita que las firmas previas queden invalidadas. Sin embargo, este exige que
    	// el PDF no incorpore ningun error, asi que lo mantendremos desactivado para la
    	// primera firma y activado para las subsiguientes. Un error incorporado en un PDF
    	// erroneo puede quedar subsanado en su version firmada, haciendo posible incorporar
    	// nuevas firmas agregando revisiones del documento.
    	final PdfStamper stp =
    		PdfStamper.createSignature(
    				pdfReader,	// PDF de entrada
    				baos,				// Salida
    				'\0',				// Mantener version
    				null,				// No crear temporal 
    				pdfReader.getAcroFields().getSignatureNames().size() > 0   // Si hay mas firmas, creo una revision
    		);
    	final PdfSignatureAppearance sap = stp.getSignatureAppearance();

    	stp.setFullCompression();
    	sap.setAcro6Layers(true);
    	sap.setLayer2Text("");
    	sap.setLayer4Text("");
    	sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
    	if (reason != null) sap.setReason(reason);
    	if (useSystemDateTime) sap.setSignDate(new GregorianCalendar());

    	// Pagina en donde se imprime la firma
    	if (page == LAST_PAGE) page = pdfReader.getNumberOfPages();

    	// Posicion de la firma 
    	final Rectangle signaturePositionOnPage = getSignaturePositionOnPage(extraParams);
    	if (signaturePositionOnPage != null && signField == null) {
    		sap.setVisibleSignature(signaturePositionOnPage, page, null);
    	}
    	else if (signField != null) {
    		sap.setVisibleSignature(signField);
    	}

    	// Localizacion en donde se produce la firma
    	if (signatureProductionCity != null) sap.setLocation(signatureProductionCity);

    	// Contacto del firmante
    	if (signerContact != null) sap.setContact(signerContact);

    	// Rubrica de la firma
    	if (rubric != null) {
    		try {
    			sap.setImage(javaImage2IText(rubric));
    		}
    		catch(final Throwable e) {
    			Logger.getLogger("es.gob.afirma").severe(
    					"No se pudo establecer la imagen de firma para el documento PDF, no se usara imagen: " + e
    			);
    		}
    	}


    	final Certificate[] chain = ke.getCertificateChain();

    	sap.setCrypto(null, chain, null, null);

    	final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, PdfName.ADBE_PKCS7_DETACHED);
    	if (sap.getSignDate() != null) dic.setDate(new PdfDate(sap.getSignDate()));
    	dic.setName(PdfPKCS7.getSubjectFields((X509Certificate)chain[0]).getField("CN"));
    	if (sap.getReason() != null) dic.setReason(sap.getReason());
    	if (sap.getLocation() != null) dic.setLocation(sap.getLocation());
    	if (sap.getContact() != null) dic.setContact(sap.getContact());

    	sap.setCryptoDictionary(dic);

    	int chainCertSize = 0;
    	for(Certificate cert : chain) {
    		chainCertSize += cert.getEncoded().length;
    	}
    	//        System.out.println("Tama\u00F1o de la cadena de certificacion: " + chainCertSize);
    	//        System.out.println("Tama\u00F1o del cert: " + chain[0].getEncoded().length);
    	//        System.out.println("Clave: " + ((java.security.interfaces.RSAPublicKey)chain[0].getPublicKey()).getModulus().bitLength());

    	// 2229 <- CamerFirma Demo SHA-1 (1024)
    	// 5123 <- Firma Profesional Demo SHA-1 (1024)
    	// 5031 <- DNIe SHA-2 (2048)
    	final int csize = 8000;
    	final HashMap<PdfName, Integer> exc = new HashMap<PdfName, Integer>();
    	exc.put(PdfName.CONTENTS, new Integer(csize * 2 + 2));

    	sap.preClose(exc);


    	//********************************************************************************
    	//**************** CALCULO DEL SIGNED DATA ***************************************
    	//********************************************************************************

    	P7ContentSignerParameters csp = new P7ContentSignerParameters(
    			inPDF,
    			algorithm,
    			(X509Certificate[]) ke.getCertificateChain()
    	);

    	// tipos de datos a firmar. es el tipo de datos. Para este tipo de firma siempre es "data".
    	Oid dataType = null;
    	try {
    		dataType = new Oid(PKCSObjectIdentifiers.data.getId());
    	}
    	catch (final Throwable ex) {
    		Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex);
    	}

    	String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(algorithm);

    	//        System.out.println(digestAlgorithm);

    	byte[] messageDigest = MessageDigest.getInstance(digestAlgorithm).digest(
    			AOUtil.getDataFromInputStream(sap.getRangeStream()));

    	byte[] pk = null;

    	try {
    		boolean omitContent = true;
    		Oid policyQualifier2 = null;
    		try {
    			policyQualifier2 = new Oid(policyQualifier);
    		}
    		catch(final Throwable e) {
    			// Si falla la conversion a OID, se queda como null y no se insertara en la firma
    		}
    		pk = new GenCadesEPESSignedData().generateSignedData(
    				csp, 
    				omitContent, 
    				policyIdentifier,
    				policyQualifier2,
    				true, // true -> isSigningCertificateV2, false -> isSigningCertificateV1
    				dataType, 
    				ke, 
    				messageDigest
    		);

    	}
    	catch (Throwable e) {
    		throw new AOException("Error generando la firma PAdES", e);
    	}

    	//********************************************************************************
    	//*************** FIN CALCULO DEL SIGNED DATA ************************************
    	//********************************************************************************

    	//        System.out.println("Tamano de la firma PDF: " + pk.length);

    	final byte[] outc = new byte[csize];

    	final PdfDictionary dic2 = new PdfDictionary();

    	System.arraycopy(pk, 0, outc, 0, pk.length);

    	dic2.put(PdfName.CONTENTS, new PdfString(outc).setHexWriting(true));

    	sap.close(dic2);

    	return baos.toByteArray();
    }
    
    /** R&uacute;brica de la firma. */
    private BufferedImage rubric = null;
    
    /**
     * Establece la r&uacute;brica de la firma.
     * @param rubric  Imagen de la r&uacute;brica.
     */
    public void setRubric(BufferedImage rubric) {
        this.rubric = rubric;
    }
    
    private static com.itextpdf.text.Image javaImage2IText(RenderedImage inImage) throws AOException {
        
        ByteArrayOutputStream imgbaos = new ByteArrayOutputStream();
        MemoryCacheImageOutputStream img = new MemoryCacheImageOutputStream(imgbaos);  
        
        try {
            // Configuramos el tipo de imagen
            ImageWriter writer = ImageIO.getImageWritersByFormatName("JPG").next();
            ImageWriteParam iwp = writer.getDefaultWriteParam();

            // Establecemos la compresion
            iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
            iwp.setCompressionQuality(iwp.getCompressionQualityValues()[iwp.getCompressionQualityValues().length-1]);

            // Generamos la imagen
            writer.setOutput(img);
            writer.write(null, new IIOImage(inImage, null, null), iwp);

            img.flush();
            
            return new com.itextpdf.text.Jpeg(imgbaos.toByteArray());
        }
        catch(final Throwable e) {
        	//e.printStackTrace();
            throw new AOException(
                "No se pudo crear la imagen iText a partir de la imagen Java" + e
            );
        }
    }
    
    /**
     * Obtiene el nombre con el que deber&iacute;a guardarse un PDF tras ser firmado.
     * B&aacute;sicamente se le anexa el sufijo <i>.signed</i> al nombre original, manteniendo
     * la extensi&oacute;n (se respetan may&uacute;culas y min&uacute;sculas en esta, pero no
     * se admite una extensi&oacute;n con mezcla de ambas)
     * @param originalName Nombre original del PDF
     * @return Nombre del PDF ya firmado
     */
    public static String getSignedName(final String originalName) {
        if (originalName == null) return "signed.pdf";
        if (originalName.endsWith(".pdf")) return originalName.replace(".pdf", ".signed.pdf");
        if (originalName.endsWith(".PDF")) return originalName.replace(".PDF", ".signed.pdf");
        return originalName + ".signed.pdf";
    }
    
    public byte[] getData(InputStream signData) throws AOInvalidFormatException {
        
        // Leemos la firma
        byte[] sign = null;
        try {
            sign = AOUtil.getDataFromInputStream(signData);
        } catch (IOException e) {
            throw new AOInvalidFormatException("No se pudieron leer los datos de entrada", e);
        } 

        // Si no es una firma PDF valida, lanzamos una excepcion
        if(!isSign(new ByteArrayInputStream(sign))) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida");
        }
        
        // TODO: Devolver el PDF sin firmar
        return sign;
    }


    public AOSignInfo getSignInfo(final InputStream signData) throws AOInvalidFormatException, AOException {
        if(signData == null)
            throw new NullPointerException("No se han introducido datos para analizar");
        
        byte[] signDataReaded;
        try {
            signDataReaded = AOUtil.getDataFromInputStream(signData);
        } 
        catch (final Throwable e) {
            throw new AOException("No se han podido leer los datos de firma: "+e);
        }
        
        if(!isSign(new ByteArrayInputStream(signDataReaded))) {
        	throw new AOInvalidFormatException(
        			"Los datos introducidos no se corresponden con un objeto de firma"
        	);
        }
        
        return new AOSignInfo(AOConstants.SIGN_FORMAT_PDF); 
        // Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
    }

    public String getDataMimeType(final InputStream signData) throws AOUnsupportedSignFormatException {
        if(signData == null)
            throw new NullPointerException("Los datos de firma introducidos son nulos");
            
        if(!this.isSign(signData))
            throw new AOUnsupportedSignFormatException("La firma introducida no es un fichero de firma PDF");
        return MIMETYPE_PDF;
    }
    
    
}
