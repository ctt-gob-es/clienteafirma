/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
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
import javax.naming.OperationNotSupportedException;
import javax.swing.JOptionPane;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.cms.CMSSignedGenerator;
import org.ietf.jgss.Oid;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.exceptions.InvalidPdfException;
import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfPKCS7;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfSignature;
import com.lowagie.text.pdf.PdfSignatureAppearance;
import com.lowagie.text.pdf.PdfStamper;
import com.lowagie.text.pdf.PdfString;

import es.gob.afirma.Messages;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.aobinarysignhelper.GenCadesEPESSignedData;
import es.gob.afirma.signers.aobinarysignhelper.P7ContentSignerParameters;
import es.gob.afirma.signers.beans.AOSignInfo;
import es.gob.afirma.signers.beans.AOSimpleSignInfo;

/**
 * Clase para la firma electr&oacute;nica de ficheros Adobe PDF.
 * <p>Se mantiene contra iText 2.1.7 por cuestiones de licencia.</p>
 * <p>Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
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
 *  <dt>ownerPassword</dt>
 *  	<dd>Contrase&ntilde;a de apertura del PDF (contrase&ntilde;a del propietario) si este est&aacute;ba cifrado</dd>
 *  <dt>headLess</dt>
 *      <dd>Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si no se establece o se establece a <code>false</code> act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para los procesos desatendidos y por lotes</dd>
 *  <dt>avoidEncryptingSignedPdfs</dt>
 *      <dd>Si se establece a <code>true</code> no cifra los PDF firmados aunque el original estuviese firmado, si no se establece o se establece a <code>false</code> los PDF se cifran tras firmarse si el original lo estaba, usando la misma contrase&ntilde;a y opciones que este</dd>
 *  <dt>allowSigningCertifiedPdfs</dt>
 *      <dd>Si se establece a <code>true</code> permite la firma o cofirma de PDF certificados, si no se establece o se establece a <code>false</code> se lanza una excepci&oacute;n en caso de intentar firmar o cofirmar un PDF certificado. <b>Solo tiene efecto cuando <code>headLess</code> est&aacute; establecido a <code>true</code>, si <code>headLess</code> est&aacute; a <code>false</code> se ignora este par&aacute;metro 
 * </dl> 
 * </p>
 */
public final class AOPDFSigner implements AOSigner {

   	/** Referencia a la &uacute;ltima p&aacute;gina del documento PDF. */
   	public static final int LAST_PAGE = -666;

   	/** Mimetype asignado a los ficheros PDF. */
   	private static final String MIMETYPE_PDF = "application/pdf"; //$NON-NLS-1$
   	
    public byte[] sign(final byte[] data, 
    		           final String algorithm, 
    		           final PrivateKeyEntry keyEntry, 
    		           Properties extraParams) throws AOException {
    	
    	String signAlgorithm = algorithm;
    	
    	if ( !algorithm.equals(AOConstants.SIGN_ALGORITHM_MD5WITHRSA)
			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA1WITHRSA)
			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA256WITHRSA)
			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA384WITHRSA)
			&& !algorithm.equals(AOConstants.SIGN_ALGORITHM_SHA512WITHRSA)) {
    		
    		Logger.getLogger("es.gob.afirma").warning("El algoritmo '"+algorithm+"' no esta soportado por PDF, se usara SHA-1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		signAlgorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
    	}
    	
    	if (extraParams == null) extraParams = new Properties();
        try {
            return signPDF(
                this.getLowagieDigestOid(signAlgorithm),
                keyEntry,
                data,
                extraParams,
                signAlgorithm
            );
        } 
        catch (final InvalidPdfException e) {
        	throw new AOFormatFileException("El documento no era un PDF valido"); //$NON-NLS-1$
        } 
        catch (final AOException e) {
        	throw e;
        } 
        catch (final Throwable e) {
        	throw new AOException("Error firmando el PDF", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] data, 
    		             final byte[] sign, 
    		             final String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }
    
    public byte[] cosign(final byte[] sign, 
    		             final String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             final Properties extraParams) throws AOException {
        return sign(sign, algorithm, keyEntry, extraParams);
    }
    
    public byte[] countersign(final byte[] sign, 
    		                  final String algorithm, 
    		                  final CounterSignTarget targetType, 
    		                  final Object[] targets, 
    		                  final PrivateKeyEntry keyEntry, 
    		                  final Properties extraParams) throws AOException {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros PDF"); //$NON-NLS-1$
    }
    
    public String getSignedName(final String originalName, final String inText) {
    	
		final String inTextInt = (inText != null ? inText : "");  //$NON-NLS-1$
    	
        if (originalName == null) return "signed.pdf"; //$NON-NLS-1$
        if (originalName.toLowerCase().endsWith(".pdf")) return originalName.substring(0, originalName.length()-4) + inTextInt + ".pdf"; //$NON-NLS-1$ //$NON-NLS-2$
        return originalName + inTextInt + ".pdf"; //$NON-NLS-1$
    }
    
    public void setDataObjectFormat(final String description, 
    		                        final Oid objectIdentifier, 
    		                        final MimeType mimeType, 
    		                        final String encoding) {}

    public TreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        TreeNode root = new TreeNode("Datos"); //$NON-NLS-1$
        final AcroFields af;
        
    	PdfReader pdfReader;
    	try {
    		pdfReader = new PdfReader(sign);
    	}
		catch(final BadPasswordException e) {
			try {
				pdfReader = new PdfReader(
					sign,
					new String(new UIPasswordCallback(Messages.getString("AOPDFSigner.0"), null).getPassword()).getBytes() //$NON-NLS-1$
				);
			}
			catch(final BadPasswordException e2) {
	            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                    "La contrasena del PDF no es valida, se devolvera un arbol vacio: " + e2 //$NON-NLS-1$
                );
	            return new TreeModel(root, root.getChildCount());
			}
			catch(final Throwable e3) {
	            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
	                "No se ha podido leer el PDF, se devolvera un arbol vacio: " + e3 //$NON-NLS-1$
	            );
	            return new TreeModel(root, root.getChildCount());
			}
		}
		catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                "No se ha podido leer el PDF, se devolvera un arbol vacio: " + e //$NON-NLS-1$
            );
            return new TreeModel(root, root.getChildCount());
		}
		
        try {
            af = pdfReader.getAcroFields();
        }
        catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                "No se ha podido obtener la informacion de los firmantes del PDF, se devolvera un arbol vacio: " + e //$NON-NLS-1$
            );
            return new TreeModel(root, root.getChildCount());
        }
        final ArrayList<?> names = af.getSignatureNames();
        Object pkcs1Object = null;
        for (int i = 0; i < names.size(); ++i) {
    		PdfPKCS7 pcks7 = af.verifySignature(names.get(i).toString());
    		if (asSimpleSignInfo) {
    			AOSimpleSignInfo ssi = new AOSimpleSignInfo(new X509Certificate[] { pcks7.getSigningCertificate() }, pcks7.getSignDate().getTime());
    			
    			// Extraemos el PKCS1 de la firma
    			try {
	    			// iText antiguo
    				final Field digestField = Class.forName("com.lowagie.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
    				// iText nuevo
    				//final Field digestField = Class.forName("com.itextpdf.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
	    			digestField.setAccessible(true);
	    			pkcs1Object = digestField.get(pcks7);
    			}
    			catch(final Throwable e) {
    				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
		                "No se ha podido obtener informacion de una de las firmas del PDF, se continuara con la siguiente: " + e //$NON-NLS-1$
		            );
    				continue;
    			}
    			if(pkcs1Object != null && pkcs1Object instanceof byte[]) {
    				ssi.setPkcs1((byte[])pkcs1Object);
    			}
    			root.add(new TreeNode(ssi));
    		}
            else root.add(new TreeNode((AOUtil.getCN(pcks7.getSigningCertificate()))));
        }
        
//        }
//        catch(final Throwable e) {
//            Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
//                "No se ha podido obtener toda la informacion de los firmantes del PDF, el arbol puede estar incompleto: " + e //$NON-NLS-1$
//            );
//        }
        return new TreeModel(root, root.getChildCount());
    }
    
    public boolean isSign(final byte[] data) {
        if(data == null) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"Se han introducido datos nulos para su comprobacion" //$NON-NLS-1$
    		);
            return false;
        }
        return isPdfFile(data);
    }
    
    private boolean isPdfFile(final byte[] data) {
    	
    	byte[] buffer = new byte[5];
    	try {
    		new ByteArrayInputStream(data).read(buffer);
    	} catch (Exception e) {
    		buffer = null;
    	}
    	
    	// Comprobamos que cuente con una cabecera PDF
    	if (buffer != null && !"%PDF-".equals(new String(buffer))) {
    		return false;
    	}

    	try {
    		// Si lanza una excepcion al crear la instancia, no es un fichero PDF
    		new PdfReader(data);
    	}
    	catch(final BadPasswordException e) {
    		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    				"El PDF esta protegido con contrasena por lo que no se toma como PDF valido" //$NON-NLS-1$
    		);
    		return false;
    	}
    	catch (final Throwable e) {
    		return false;
    	}

    	return true;
    }
    
    public boolean isValidDataFile(final byte[] data) {
        if(data == null) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"Se han introducido datos nulos para su comprobacion" //$NON-NLS-1$
    		);
            return false;
        }
        return isPdfFile(data);
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
            Logger.getLogger("es.gob.afirma").warning("El algoritmo '"+algorithm+"' no esta soportado por PDF, se usara SHA-1"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftX")),  //$NON-NLS-1$
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageLowerLeftY")),  //$NON-NLS-1$
    			Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightX")),  //$NON-NLS-1$
				Integer.parseInt(extraParams.getProperty("signaturePositionOnPageUpperRightY")) //$NON-NLS-1$
			);
    	} 
    	catch(final Throwable e) {
    		return null;
    	}

    }
        
    private byte[] signPDF(final String lowagieDigestOid, 
    		               final PrivateKeyEntry ke, 
    		               final byte[] inPDF,
    		               Properties extraParams,
    		               final String algorithm) throws Throwable {

    	if (extraParams == null) extraParams = new Properties();
    	final boolean useSystemDateTime = Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true")); //$NON-NLS-1$ //$NON-NLS-2$
    	final String reason = extraParams.getProperty("signReason"); //$NON-NLS-1$
    	final String signField = extraParams.getProperty("signField"); //$NON-NLS-1$
    	final String signatureProductionCity = extraParams.getProperty("signatureProductionCity"); //$NON-NLS-1$
    	final String signerContact = extraParams.getProperty("signerContact"); //$NON-NLS-1$
    	final String policyIdentifier = extraParams.getProperty("policyIdentifier"); //$NON-NLS-1$
    	final String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$
    	int page = 1;
    	try {
    		page = Integer.parseInt(extraParams.getProperty("signaturePage")); //$NON-NLS-1$
    	}
    	catch(final Throwable e) { /* Se deja la pagina tal y como esta */ }

        /*
         * Si se solicito una firma explicita, advertimos no son compatibles con PDF y se
         * ignorara esta configuracion
         */
        if (extraParams.containsKey("mode") //$NON-NLS-1$
      		&& extraParams.getProperty("mode").equals(AOConstants.SIGN_MODE_EXPLICIT)) { //$NON-NLS-1$
      			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
  					"El formato de firma PDF no soporta el modo de firma explicita, " + //$NON-NLS-1$
  					"se ignorara esta configuracion"); //$NON-NLS-1$
        }
    	
    	PdfReader pdfReader;
    	String ownerPassword = extraParams.getProperty("ownerPassword"); //$NON-NLS-1$
    	try {
    		if (ownerPassword == null) pdfReader = new PdfReader(inPDF);
    		else pdfReader = new PdfReader(inPDF, ownerPassword.getBytes());
    	}
    	catch(final BadPasswordException e) {
    		// Comprobamos que el signer esta en modo interactivo, y si no lo esta no pedimos contrasena por
    		// dialogo, principalmente para no interrumpir un firmado por lotes desatendido
    		if ("true".equalsIgnoreCase(extraParams.getProperty("headLess"))) throw new AOException("La contrasena proporcionada no es valida para el PDF actual"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		// La contrasena que nos han proporcionada no es buena o no nos proporcionaron ninguna
    		ownerPassword = new String(new UIPasswordCallback(Messages.getString("AOPDFSigner.0"), null).getPassword()); //$NON-NLS-1$
    		try {
	    		pdfReader = new PdfReader(
					inPDF,
					ownerPassword.getBytes()
				);
    		}
    		catch(final BadPasswordException e2) {
    			throw new AOException("La contrasena proporcionada no es valida para el PDF actual"); //$NON-NLS-1$
    		}
    	}
    	
    	if (pdfReader.getCertificationLevel() != PdfSignatureAppearance.NOT_CERTIFIED) {
    		if ("true".equalsIgnoreCase(extraParams.getProperty("headLess"))) { //$NON-NLS-1$ //$NON-NLS-2$
    			if (!"true".equalsIgnoreCase(extraParams.getProperty("allowSigningCertifiedPdfs"))) //$NON-NLS-1$ //$NON-NLS-2$
    				throw new OperationNotSupportedException(
						"No se permite la firma o cofirma de PDF certificados (el paramtro allowSigningCertifiedPdfs estaba establecido a true)" //$NON-NLS-1$
					);
    		}
    		else {
    			if (JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(
					null, 
					Messages.getString("AOPDFSigner.8"),  //$NON-NLS-1$
					Messages.getString("AOPDFSigner.9"),  //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION, 
					JOptionPane.WARNING_MESSAGE 
				)) {
    				throw new OperationNotSupportedException(
						"No se ha permitido la firma de un PDF certificado" //$NON-NLS-1$
					);
    			}
    		}
    	}
    	
    	//******************************************************************************
    	//********* Comprobaciones de adjuntos y empotrados PDF ************************
    	//******************************************************************************
    	
		PdfArray array;
		PdfDictionary annot;
		PdfDictionary fs;
		PdfDictionary refs;
		for (int i = 1; i <= pdfReader.getNumberOfPages(); i++) {
			array = pdfReader.getPageN(i).getAsArray(PdfName.ANNOTS);
			if (array == null) continue;
			for (int j = 0; j < array.size(); j++) {
				annot = array.getAsDict(j);
				if (annot != null && PdfName.FILEATTACHMENT.equals(annot.getAsName(PdfName.SUBTYPE))) {
					fs = annot.getAsDict(PdfName.FS);
					if (fs != null) {
						refs = fs.getAsDict(PdfName.EF);
						if (refs != null) {
							for (Object name : refs.getKeys()) {
								if (name instanceof PdfName) {
									Logger.getLogger("es.gob.afirma").warning(
											"Se ha encontrado un adjunto (" +
											fs.getAsString((PdfName) name) +
											") en el PDF, pero no se firmara de forma independiente"
									);
								}
								//System.out.println(fs.getAsString(name).toString());
								//System.out.println(new String(PdfReader.getStreamBytes((PRStream)refs.getAsStream(name))));
								//System.out.println();
							}
						}
					}
				}
			}
		}
		
		final PdfDictionary catalog = pdfReader.getCatalog();
		if (catalog != null) {
			final PdfDictionary namesCatalog = catalog.getAsDict(PdfName.NAMES);
			if (namesCatalog != null) {
				final PdfDictionary filesCatalog = namesCatalog.getAsDict(PdfName.EMBEDDEDFILES);
				if (filesCatalog != null) {
					final PdfArray filespecs = filesCatalog.getAsArray(PdfName.NAMES);
					PdfDictionary filespec;
					for (int i = 0; i < filespecs.size(); ) {
						filespecs.getAsString(i++);
						filespec = filespecs.getAsDict(i++);
						refs = filespec.getAsDict(PdfName.EF);
						for (Object key : refs.getKeys()) {
							if (key instanceof PdfName) {
								Logger.getLogger("es.gob.afirma").warning(
										"Se ha encontrado un fichero empotrado (" +
										filespec.getAsString((PdfName) key) +
										") en el PDF, pero no se firmara de forma independiente"
								);

								//System.out.println(filespec.getAsString(key).toString());
								//System.out.println(new String(PdfReader.getStreamBytes((PRStream) PdfReader.getPdfObject(refs.getAsIndirectObject(key)))));
								//System.out.println();
							}
						}
					}
				}
			}
		}

    	
    	//******************************************************************************
    	//********* Fin comprobaciones de adjuntos y empotrados PDF ********************
    	//******************************************************************************
    	
    	
    	// Los derechos van firmados por Adobe, y como desde iText se invalidan es mejor
    	// quitarlos
    	pdfReader.removeUsageRights();
    	    	
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
    	sap.setLayer2Text(""); //$NON-NLS-1$
    	sap.setLayer4Text(""); //$NON-NLS-1$
    	// iText antiguo
    	sap.setRender(PdfSignatureAppearance.SignatureRenderDescription);
    	// iText nuevo
    	//sap.setRenderingMode(PdfSignatureAppearance.RenderingMode.NAME_AND_DESCRIPTION);
    	if (reason != null) sap.setReason(reason);
    	if (useSystemDateTime) sap.setSignDate(new GregorianCalendar());
    	
    	if (pdfReader.isEncrypted() && ownerPassword != null) {
    		if ("true".equalsIgnoreCase(extraParams.getProperty("avoidEncryptingSignedPdfs"))) { //$NON-NLS-1$ //$NON-NLS-2$
        		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
    				"Aunque el PDF original estaba encriptado no se encriptara el PDF firmado (se establecio el indicativo 'avoidEncryptingSignedPdfs')" //$NON-NLS-1$
    			);
    		}
    		else {
	    		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
					"El PDF original estaba encriptado, se intentara encriptar tambien el PDF firmado" //$NON-NLS-1$
				);
	    		try {
		    		stp.setEncryption(
		    			ownerPassword.getBytes(),
						ownerPassword.getBytes(),
						pdfReader.getPermissions(),
						pdfReader.getCryptoMode()
					);
	    		}
	    		catch(final DocumentException de) {
		    		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se ha podido cifrar el PDF destino, se escribira sin contrasena: " + de //$NON-NLS-1$
					);
	    		}
    		}
    	}

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
    			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
    					"No se pudo establecer la imagen de firma para el documento PDF, no se usara imagen: " + e //$NON-NLS-1$
    			);
    		}
    	}

    	Certificate[] chain = ke.getCertificateChain();
    	if (chain == null) chain = new Certificate[] { ke.getCertificate() };

    	sap.setCrypto(null, chain, null, null);

    	final PdfSignature dic = new PdfSignature(PdfName.ADOBE_PPKLITE, PdfName.ADBE_PKCS7_DETACHED);
    	if (sap.getSignDate() != null) dic.setDate(new PdfDate(sap.getSignDate()));
    	dic.setName(PdfPKCS7.getSubjectFields((X509Certificate)chain[0]).getField("CN")); //$NON-NLS-1$
    	if (sap.getReason() != null) dic.setReason(sap.getReason());
    	if (sap.getLocation() != null) dic.setLocation(sap.getLocation());
    	if (sap.getContact() != null) dic.setContact(sap.getContact());

    	sap.setCryptoDictionary(dic);

    	//int chainCertSize = 0;
    	//for(Certificate cert : chain) {
    	//	chainCertSize += cert.getEncoded().length;
    	//}
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

    	// tipos de datos a firmar. es el tipo de datos. Para este tipo de firma siempre es "data".
    	Oid dataType = null;
    	try {
    		dataType = new Oid(PKCSObjectIdentifiers.data.getId());
    	}
    	catch (final Throwable ex) {
    		Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
    	}

    	final byte[] pk;

    	try {
    		Oid policyQualifier2 = null;
    		try {
    			policyQualifier2 = new Oid(policyQualifier);
    		}
    		catch(final Throwable e) {
    			// Si falla la conversion a OID, se queda como null y no se insertara en la firma
    		}
    		
        	X509Certificate[] xCerts = new X509Certificate[0];
        	final Certificate[] certs = ke.getCertificateChain();
        	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
        	else {
        		final Certificate cert = ke.getCertificate();
        		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
        	}
        	
    		pk = new GenCadesEPESSignedData().generateSignedData(
				new P7ContentSignerParameters(
					inPDF,
					algorithm,
					xCerts
		    	), 
				true, // omitContent 
				policyIdentifier,
				policyQualifier2,
				true, // true -> isSigningCertificateV2, false -> isSigningCertificateV1
				dataType, 
				ke, 
				MessageDigest.getInstance(AOCryptoUtil.getDigestAlgorithmName(algorithm)).digest(
					AOUtil.getDataFromInputStream(sap.getRangeStream())
				)
    		);

    	}
    	catch (final Throwable e) {
    		throw new AOException("Error generando la firma PAdES", e); //$NON-NLS-1$
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
    public void setRubric(final BufferedImage rubric) {
        this.rubric = rubric;
    }
    
    private static com.lowagie.text.Image javaImage2IText(final RenderedImage inImage) throws AOException {
        
        final ByteArrayOutputStream imgbaos = new ByteArrayOutputStream();
        final MemoryCacheImageOutputStream img = new MemoryCacheImageOutputStream(imgbaos);  
        
        try {
            // Configuramos el tipo de imagen
            final ImageWriter writer = ImageIO.getImageWritersByFormatName("JPG").next(); //$NON-NLS-1$
            final ImageWriteParam iwp = writer.getDefaultWriteParam();

            // Establecemos la compresion
            iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
            iwp.setCompressionQuality(iwp.getCompressionQualityValues()[iwp.getCompressionQualityValues().length-1]);

            // Generamos la imagen
            writer.setOutput(img);
            writer.write(null, new IIOImage(inImage, null, null), iwp);

            img.flush();
            
            return new Jpeg(imgbaos.toByteArray());
        }
        catch(final Throwable e) {
            throw new AOException(
                "No se pudo crear la imagen iText a partir de la imagen Java", e //$NON-NLS-1$
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
        if (originalName == null) return "signed.pdf"; //$NON-NLS-1$
        if (originalName.endsWith(".pdf")) return originalName.replace(".pdf", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        if (originalName.endsWith(".PDF")) return originalName.replace(".PDF", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        return originalName + ".signed.pdf"; //$NON-NLS-1$
    }
    
    public byte[] getData(final byte[] sign) throws AOInvalidFormatException {
        
        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }
        
        // TODO: Devolver el PDF sin firmar
        return sign;
    }


    public AOSignInfo getSignInfo(final byte[] data) throws AOInvalidFormatException, AOException {
        if(data == null)
            throw new NullPointerException("No se han introducido datos para analizar"); //$NON-NLS-1$
        
        if (!isSign(data)) {
        	throw new AOInvalidFormatException(
    			"Los datos introducidos no se corresponden con un objeto de firma" //$NON-NLS-1$
        	);
        }
        
        return new AOSignInfo(AOConstants.SIGN_FORMAT_PDF); 
        // Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
    }

    public String getDataMimeType(final byte[] sign) throws AOUnsupportedSignFormatException {
        if(sign == null)
            throw new NullPointerException("Los datos de firma introducidos son nulos"); //$NON-NLS-1$
            
        if(!this.isSign(sign))
            throw new AOUnsupportedSignFormatException("La firma introducida no es un fichero de firma PDF"); //$NON-NLS-1$
        
        return MIMETYPE_PDF;
    }
}

