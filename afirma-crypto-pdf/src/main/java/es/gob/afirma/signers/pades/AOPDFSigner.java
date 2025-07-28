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
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOConfigurableContext;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.SignEnhancer;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;

/** Manejador de firmas binarias de ficheros Adobe PDF en formato PAdES.
 * <p>Para compatibilidad estricta con PAdES-BES/EPES se utiliza <i>ETSI.CAdES.detached</i> como nombre del subfiltro.</p>
 * <p>La compatibilidad con PAdES no es completa, omiti&eacute;ndose los siguientes aspectos de la normativa:</p>
 * <ul>
 *  <li>Firma separada de ficheros empotrados en el documento PDF.</li>
 *  <li>Firma separada de ficheros adjuntos al documento PDF.</li>
 * </ul>
 * <p>
 *  Estas mismas deficiencias provocan igualmente la incompatibilidad de las firmas generadas con "Carpetas PDF" (<i>Portfolios PDF</i>).
 *  Cuando se encuentran documentos PDF con ficheros adjuntos o empotrados se imprime informaci&oacute;n relativa en consola.
 * </p>
 * <p>
 *   Por compatibilidad con Adobe Reader, la firmas se generan con el subfiltro "adbe.pkcs7.detached" en vez de con
 *   "ETSI.CAdES.detached". Consulte la documentaci&oacute;n del par&aacute;metro <code>signatureSubFilter</code> para variar este comportamiento.
 * </p>
 * <p>
 *  La clase necesita espec&iacute;ficamente la versi&oacute;n de iText 2.1.7 modificada para el Cliente &#64;firma.
 * </p> */
public final class AOPDFSigner implements AOSigner, AOConfigurableContext {

    private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$
    private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	public static final PdfName PDFNAME_ETSI_RFC3161 = new PdfName("ETSI.RFC3161"); //$NON-NLS-1$
	public static final PdfName PDFNAME_DOCTIMESTAMP = new PdfName("DocTimeStamp"); //$NON-NLS-1$

	/** Tama&ntilde;o m&iacute;nimo de un PDF.
	 * <a href="https://stackoverflow.com/questions/17279712/what-is-the-smallest-possible-valid-pdf">
	 *   https://stackoverflow.com/questions/17279712/what-is-the-smallest-possible-valid-pdf
	 * </a>. */
	private static final int PDF_MIN_FILE_SIZE = 70;

	/**
	 * Modo seguro. Si no esta activado se permiten algunas operaciones, como el uso de rutas a los
	 * datos en algunos extraParams en lugar de proporcionar estos datos en Base64.
	 */
	private boolean secureMode = true;

	private static SignEnhancer enhancer = null;

	private static Properties enhancerConfig;
	static {
		enhancerConfig = new Properties();
		String enhancerClassName = null;
		try (InputStream configIs = AOPDFSigner.class.getResourceAsStream("/enhancer.properties")) {  //$NON-NLS-1$
			enhancerConfig.load(configIs);
			enhancerClassName = enhancerConfig.getProperty("enhancerClassFile"); //$NON-NLS-1$
			if (enhancerClassName != null) {
				enhancer = (SignEnhancer) Class.forName(enhancerClassName).getConstructor().newInstance();
				LOGGER.info("Se usara el siguiente mejorador de firmas: " + enhancerClassName); //$NON-NLS-1$
			}
		}
		catch(final ClassNotFoundException e) {
			LOGGER.warning(
				"Se ha configurado la clase de mejora '" + enhancerClassName + "', pero esta no se encuentra: " + e  //$NON-NLS-1$//$NON-NLS-2$
			);
		}
		catch (final Exception e) {
			LOGGER.info("No hay un mejorador de firmas correctamente instalado: " + e); //$NON-NLS-1$
		}
	}

	// iText tiene ciertos problemas reconociendo ECDSA y a veces usa su OID, por lo que declaramos alias de los
	// algoritmos de firma en los proveedores mas comunes
	static {
		final String[] providers = new String[] { "SunEC", "BC", "SC" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		for (final String providerName : providers) {
		    final Provider p = Security.getProvider(providerName);
		    if (p != null) {
		    	p.put("Alg.Alias.Signature.SHA224with1.2.840.10045.4.3.2", "SHA224withECDSA"); //$NON-NLS-1$ //$NON-NLS-2$
		    	p.put("Alg.Alias.Signature.SHA256with1.2.840.10045.4.3.2", "SHA256withECDSA"); //$NON-NLS-1$ //$NON-NLS-2$
		    	p.put("Alg.Alias.Signature.SHA384with1.2.840.10045.4.3.2", "SHA384withECDSA"); //$NON-NLS-1$ //$NON-NLS-2$
		    	p.put("Alg.Alias.Signature.SHA512with1.2.840.10045.4.3.2", "SHA512withECDSA"); //$NON-NLS-1$ //$NON-NLS-2$
		    }
		}
	}

	/** Obtiene el mejorador de firmas por defecto.
	 * @return Mejorador de firmas por defecto. */
	public static SignEnhancer getSignEnhancer() {
		return enhancer;
	}

	/** Obtiene la configuraci&oacute;n del mejorador de firmas por defecto.
	 * @return Configuraci&oacute;n del mejorador de firmas por defecto. */
	public static Properties getSignEnhancerConfig() {
		return enhancerConfig != null ? (Properties) enhancerConfig.clone() : null;
	}

    /** Firma un documento PDF en formato PAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Acrobat), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headless</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headless</code>.
     *  Adicionalmente, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, la salida ser&aacute;a un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a..
     * </p>
     * @param inPDF Documento PDF a firmar.
     * @param signAlgorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Documento PDF firmado en formato PAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Cuando hay errores en el tratamiento de datos. */
    @Override
	public byte[] sign(final byte[] inPDF,
			           final String signAlgorithm,
			           final PrivateKey key,
			           final java.security.cert.Certificate[] certChain,
			           final Properties xParams) throws AOException,
			                                            IOException {

		final String algorithm = signAlgorithm != null ? signAlgorithm : AOSignConstants.DEFAULT_SIGN_ALGO;
        final Properties extraParams = getExtraParams(xParams);

        checkParams(algorithm, extraParams);

        final java.security.cert.Certificate[] certificateChain = Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.INCLUDE_ONLY_SIGNNING_CERTIFICATE, Boolean.FALSE.toString())) ?
    		new X509Certificate[] { (X509Certificate) certChain[0] } :
    			certChain;

    	final GregorianCalendar signTime = PdfUtil.getSignTime(extraParams.getProperty(PdfExtraParams.SIGN_TIME));

        // Sello de tiempo
        byte[] data = inPDF;
        if (PdfTimestamper.isAvailable()) {
       		data = PdfTimestamper.timestampPdf(data, extraParams, signTime);
        }

		// Prefirma
        final PdfSignResult pre;
        try {
			pre = PAdESTriPhaseSigner.preSign(
					algorithm,
				data,
				certificateChain,
				signTime,
				extraParams,
				this.secureMode
			);
		}
        catch (final InvalidPdfException e) {
			throw e;
		}

        // Firma PKCS#1
        final byte[] interSign;
        try {
	        interSign = new AOPkcs1Signer().sign(
	    		pre.getSign(),
	    		algorithm,
	    		key,
	    		certificateChain,
	    		extraParams
			);
        }
        catch (final AOCancelledOperationException e) {
        	throw e;
        }
        catch (final AOException e) {
        	throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la firma PKCS#1 de la firma PAdES: " + e, e); //$NON-NLS-1$
        }

        // Postfirma
        try {
			return PAdESTriPhaseSigner.postSign(
					algorithm,
				data,
				certificateChain,
				interSign,
				pre,
				getSignEnhancer(), // SignEnhancer
				getSignEnhancerConfig(),  // EnhancerConfig (si le llega null usa los ExtraParams)
				this.secureMode
			);
		}
        catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error el en algoritmo de firma: " + e, e); //$NON-NLS-1$
		}

    }

    /** A&ntilde;ade una firma PAdES a un documento PDF. El comportamiento es exactamente el mismo que una llamada al m&eacute;todo <code>sign(...)</code>
     * puesto que las multifirmas en los ficheros PDF se limitan a firmas independientes "en serie", pero no implementando los mecanismos de
     * cofirma o contrafirma de CAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headless</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headless</code>.
     *  Adicionalmente, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, la salida ser&aacute; un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a.
     * </p>
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>.
     * @param data Se ignora el valor de este par&aacute;metro. <b>El documento PDF debe proporcionarse mediante el par&aacute;tro <code>sign</code></b>.
     * @param sign Documento PDF a firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Documento PDF firmado en formato PAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException En caso de errores de entrada / salida. */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException, IOException {
        return sign(sign, algorithm, key, certChain, extraParams);
    }

    /** A&ntilde;ade una firma PAdES a un documento PDF. El comportamiento es exactamente el mismo que una llamada al m&eacute;todo <code>sign(...)</code>
     * puesto que las multifirmas en los ficheros PDF se limitan a firmas independientes "en serie", pero no implementando los mecanismos de
     * cofirma o contrafirma de CAdES.
     * <p>
     *  Notas sobre documentos <i>certificados</i>:<br>
     *  Si un PDF firmado se ha certificado (por ejemplo, a&ntilde;adiendo una firma electr&oacute;nica usando Adobe Reader), cualquier
     *  modificaci&oacute;n posterior del fichero (como la adici&oacute;n de nuevas firmas con este m&eacute;todo) invalidar&aacute;
     *  las firmas previamente existentes.<br>
     *  Si se detecta un documento PDF certificado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y pidiendo confirmaci&oacute;n para continuar.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>allowSigningCertifiedPdfs</code> y <code>headless</code>.<br>
     * </p>
     * <p>
     *  Notas sobre documentos protegidos con contrase&ntilde;a:<br>
     *  Si un PDF est&aacute; protegido con contrase&ntilde;a por estar cifrado, se mostrar&aacute; un di&aacute;logo gr&aacute;fico advirtiendo al usuario de esta
     *  situaci&oacute;n y solicitando la contrase&ntilde;a de apertura del PDF.<br>Si desea evitar interacciones directas con los usuarios
     *  consulte la documentaci&oacute;n de las opciones <code>ownerPassword</code> y <code>headless</code>.
     *  Adicionalmente, si el fichero de entrada estaba cifrado y protegido con contrase&ntilde;a, la salida ser&aacute; un documento PDF
     *  igualmente cifrado y protegido con contrase&ntilde;a.
     * </p>
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>
     * @param sign Documento PDF a firmar
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Documento PDF firmado en formato PAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException En caso de errores de entrada / salida. */
    @Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final java.security.cert.Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {
        return sign(sign, algorithm, key, certChain, extraParams);
    }

    /** Operaci&oacute;n no soportada para firmas PAdES. */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties extraParams) {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros PDF"); //$NON-NLS-1$
    }

    /** Devuelve el nombre de fichero de firma predeterminado que se recomienda usar para
     * un PDF firmado con nombre original igual al proporcionado.
     * En este caso el resultado ser&aacute; siempre el nombre original m&aacute;s un
     * sufijo adicional (opcional) previo a la extensi&oacute;n.
     * Siempre se termina el nombre de fichero con la extensi&oacute;n <i>.pdf</i>, incluso si el nombre original carec&iacute;a de esta.
     * @param originalName Nombre del fichero original que se firma.
     * @param inText Sufijo a agregar al nombre de fichero devuelto, inmediatamente anterior a la extensi&oacute;n.
     * @return Nombre apropiado para el fichero de firma. */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.toLowerCase(Locale.US).endsWith(PDF_FILE_SUFFIX)) {
            return originalName.substring(0, originalName.length() - PDF_FILE_SUFFIX.length()) + inTextInt + PDF_FILE_SUFFIX;
        }
        return originalName + inTextInt + PDF_FILE_SUFFIX;
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n textos con el <i>CommonName</i> (CN X.500)
     * del titular del certificado u objetos de tipo AOSimpleSignInfo con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param asSimpleSignInfo Si es <code>true</code> se devuelve un &aacute;rbol con la
     *                         informaci&oacute;n b&aacute;sica de cada firma individual
     *                         mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code>
     *                         un &aacute;rbol con los nombres (CN X.500) de los titulares certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error. */
    @Override
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

    	return getSignersStructure(sign, null, asSimpleSignInfo);
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma electr&oacute;nica.
     * Los nodos del &aacute;rbol ser&aacute;n textos con el <i>CommonName</i> (CN X.500)
     * del titular del certificado u objetos de tipo AOSimpleSignInfo con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, seg&uacute;n
     * el valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * La propia estructura de firma se considera el nodo ra&iacute;z, la firma y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param params necesarios para comprobar si los datos son una firma compatible.
     * @param asSimpleSignInfo Si es <code>true</code> se devuelve un &aacute;rbol con la
     *                         informaci&oacute;n b&aacute;sica de cada firma individual
     *                         mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code>
     *                         un &aacute;rbol con los nombres (CN X.500) de los titulares certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error. */
    @Override
    public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo) {

    	final AOTreeNode root = new AOTreeNode("Datos"); //$NON-NLS-1$

    	if (!isPdfFile(sign)) {
    		return new AOTreeModel(root);
    	}

    	PdfReader pdfReader;
    	boolean headLessProp = false;
    	try {
    		if (params != null && params.containsKey(PdfExtraParams.HEADLESS)) {
    			headLessProp = Boolean.parseBoolean(params.getProperty(PdfExtraParams.HEADLESS));
    		}
			pdfReader = PdfUtil.getPdfReader(sign, params, headLessProp);
    	}
    	catch (final BadPasswordException e) {
    		LOGGER.info(
				"El PDF necesita contrasena. Se devolvera el arbol vacio: " + e //$NON-NLS-1$
			);
    		return new AOTreeModel(root);
    	}
    	catch (final PdfIsPasswordProtectedException e) {
    		LOGGER.info(
				"El PDF necesita contrasena." + e //$NON-NLS-1$
			);
    		return new AOTreeModel(root);
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido leer el PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
    		return new AOTreeModel(root);
    	}

    	final AcroFields af;
    	try {
    		af = pdfReader.getAcroFields();
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido obtener la informacion de los firmantes del PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
    		return new AOTreeModel(root);
    	}

    	final List<String> names = af.getSignatureNames();
    	for (final String signatureName : names) {

    		// Comprobamos si es una firma o un sello
    		final PdfDictionary pdfDictionary = af.getSignatureDictionary(signatureName);
    		if (PDFNAME_ETSI_RFC3161.equals(pdfDictionary.get(PdfName.SUBFILTER)) || PDFNAME_DOCTIMESTAMP.equals(pdfDictionary.get(PdfName.SUBFILTER))) {
    			// Ignoramos los sellos
    			continue;
    		}

    		final PdfPKCS7 pkcs7;
    		try {
    			pkcs7 = af.verifySignature(signatureName);
    		}
    		catch(final Exception e) {
    			LOGGER.log(
					Level.SEVERE,
					"El PDF contiene una firma corrupta o con un formato desconocido (" + //$NON-NLS-1$
						signatureName +
							"), se continua con las siguientes si las hubiese: " + e, //$NON-NLS-1$
					e
				);
    			continue;
    		}

    		if (asSimpleSignInfo) {

       			final X509Certificate[] certChain = new X509Certificate[pkcs7.getSignCertificateChain().length];
    			for (int j = 0; j < certChain.length; j++) {
    				certChain[j] = (X509Certificate) pkcs7.getSignCertificateChain()[j];
    			}

    			final AOSimpleSignInfo ssi = new AOSimpleSignInfo(
					certChain,
					pkcs7.getSignDate() != null ? pkcs7.getSignDate().getTime() : null
				);

    			// Extraemos el PKCS#1 de la firma
    			final byte[] pkcs1 = pkcs7.getPkcs1();
    			if (pkcs1 != null) {
    				ssi.setPkcs1(pkcs1);
    			}

    			// Obtenemos el algoritmo de firma
    			final String digestAlgorithm = pkcs7.getDigestAlgorithm();
    			if (digestAlgorithm != null) {
    				ssi.setSignAlgorithm(digestAlgorithm);
    			}

    			root.add(new AOTreeNode(ssi));
    		}
    		else {
    			root.add(new AOTreeNode(AOUtil.getCN(pkcs7.getSigningCertificate())));
    		}
    	}

    	return new AOTreeModel(root);
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar.
     * @return <code>true</code> si los datos proporcionados son un documento PDF,
     *         <code>false</code> en caso contrario. */
    @Override
	public boolean isSign(final byte[] data) {
        return isSign(data, null);
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar.
     * @param params Par&aacute;metros de la firma.
     * @return <code>true</code> si los datos proporcionados son un documento PDF,
     *         <code>false</code> en caso contrario. */
	@Override
	public boolean isSign(final byte[] data, final Properties params){
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        if (!isPdfFile(data)) {
        	return false;
        }
        final Object root = getSignersStructure(data, params, false).getRoot();
        if (root instanceof AOTreeNode) {
        	// Si el arbol contiene firmas...
        	if (AOTreeModel.getChildCount(root) > 0) {
        		return true;
        	}
        	// Si no las contiene aun puede haber firmas no registradas

        	final Properties extraParams = System.getProperties();
        	try {
				if (PdfUtil.pdfHasUnregisteredSignatures(data, extraParams) &&
						Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty(PdfExtraParams.ALLOW_COSIGNING_UNREGISTERED_SIGNATURES))) {
					return true;
				}
			}
        	catch (final Exception e) {
				LOGGER.severe("No se han podido comprobar las firmas no registradas del PDF: " + e); //$NON-NLS-1$
			}
        }
        return false;
    }

    private static boolean isPdfFile(final byte[] data) {
		if (data == null || data.length < PDF_MIN_FILE_SIZE) {
			return false;
		}
        final byte[] buffer = new byte[PDF_FILE_HEADER.length()];
        try {
            new ByteArrayInputStream(data).read(buffer);
        }
        catch (final Exception e) {
			LOGGER.warning(
				"El contenido parece corrupto o truncado: " + e //$NON-NLS-1$
			);
			return false;
        }

        // Comprobamos que cuente con una cabecera PDF
        if (!PDF_FILE_HEADER.equals(new String(buffer))) {
            return false;
        }

        try {
            // Si lanza una excepcion al crear la instancia, no es un fichero PDF
            new PdfReader(data);
        }
        catch (final BadPasswordException e) {
            LOGGER.warning("El PDF esta protegido con contrasena, se toma como PDF valido: " + e); //$NON-NLS-1$
            return true;
        }
        catch (final Exception e) {
            return false;
        }

        return true;
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos proporcionados son un documento PDF, <code>false</code> en caso contrario */
    @Override
	public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return isPdfFile(data);
    }

    /** Obtiene el nombre con el que deber&iacute;a guardarse un PDF tras ser
     * firmado. B&aacute;sicamente se le anexa el sufijo <i>.signed</i> al
     * nombre original, manteniendo la extensi&oacute;n (se respetan
     * may&uacute;culas y min&uacute;sculas en esta, pero no se admite una
     * extensi&oacute;n con mezcla de ambas).
     * @param originalName Nombre original del fichero PDF.
     * @return Nombre recomendado para el PDF ya firmado. */
    public static String getSignedName(final String originalName) {
        if (originalName == null) {
            return "signed.pdf"; //$NON-NLS-1$
        }
        if (originalName.endsWith(PDF_FILE_SUFFIX)) {
            return originalName.replace(PDF_FILE_SUFFIX, ".signed.pdf"); //$NON-NLS-1$
        }
        if (originalName.endsWith(".PDF")) { //$NON-NLS-1$
            return originalName.replace(".PDF", ".signed.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return originalName + ".signed.pdf"; //$NON-NLS-1$
    }

    /** Si la entrada es un documento PDF, devuelve el mismo documento PDF.
     * @param sign Documento PDF
     * @param params Par&aacute;metros de la firma.
     * @return Mismo documento PDF de entrada, sin modificar en ning&uacute; aspecto.
     * @throws AOInvalidFormatException Si los datos de entrada no son un documento PDF. */
    @Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException {
        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign, params)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Devolver el PDF sin firmar
        return sign;

	}

    /** Si la entrada es un documento PDF, devuelve el mismo documento PDF.
     * @param sign Documento PDF
     * @return Mismo documento PDF de entrada, sin modificar en ning&uacute; aspecto.
     * @throws AOInvalidFormatException Si los datos de entrada no son un documento PDF. */
    @Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException {

        // Si no es una firma PDF valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // TODO: Devolver el PDF sin firmar
        return sign;
    }

    /** Si la entrada es un documento PDF, devuelve un objeto <code>AOSignInfo</code>
     * con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @param data Documento PDF.
     * @return Objeto <code>AOSignInfo</code> con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @throws AOException Si los datos de entrada no son un documento PDF. */
    @Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException {

    	return getSignInfo(data, null);
    }

    /** Si la entrada es un documento PDF, devuelve un objeto <code>AOSignInfo</code>
     * con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @param data Documento PDF.
     * @param params Par&aacute;metros de firma.
     * @return Objeto <code>AOSignInfo</code> con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_PDF</code>.
     * @throws AOException Si los datos de entrada no son un documento PDF. */
    @Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data, params)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia que se almacenan en el objeto AOSignInfo

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
    }

    /** Configura, cuando no lo esten ya, las propiedades necesarias para que las firmas
     * sobre unos datos respeten el formato que tuviesen firmas anteriores.
     * @param data Datos que se desean firmar.
     * @param config Configuraci&oacute;n establecida. */
    public static void configureRespectfulProperties(final byte[] data, final Properties config) {

    	if (config != null && !config.containsKey(PdfExtraParams.SIGNATURE_SUBFILTER)) {
    		String filter;
    		try {
    			filter = PdfUtil.getFirstSupportedSignSubFilter(data, config);
    		}
    		catch (final Exception e) {
    			LOGGER.warning("Error al configurar la firma PDF para que sea igual a las existentes: " + e); //$NON-NLS-1$
    			return;
    		}

    		if (filter != null) {
    			config.setProperty(PdfExtraParams.SIGNATURE_SUBFILTER, filter.substring(filter.indexOf('/') + 1));
    		}
    	}
    }

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }

    private static void checkParams(final String algorithm, final Properties extraParams) {

    	if (algorithm.toUpperCase(Locale.US).startsWith("MD")) { //$NON-NLS-1$
    		throw new IllegalArgumentException("PAdES no permite huellas digitales MD2 o MD5 (Decision 130/2011 CE)"); //$NON-NLS-1$
    	}

    	final String profile = extraParams.getProperty(PdfExtraParams.PROFILE);

		// Comprobacion del perfil de firma con la configuracion establecida
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			if (AOSignConstants.isSHA1SignatureAlgorithm(algorithm)) {
				LOGGER.warning("El algoritmo '" + algorithm + "' no esta recomendado para su uso en las firmas baseline"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			if (extraParams.containsKey(PdfExtraParams.SIGNATURE_SUBFILTER)) {
				LOGGER.warning("Se ignorara el valor establecido en el parametro '" + //$NON-NLS-1$
						PdfExtraParams.SIGNATURE_SUBFILTER +
						"' ya que en las firmas baseline el subfiltro siempre sera " + //$NON-NLS-1$
						AOSignConstants.PADES_SUBFILTER_BES);
				extraParams.remove(PdfExtraParams.SIGNATURE_SUBFILTER);
			}
		}

		// Las firmas BES no pueden declarar commitmentTypeIndications. Solo esta
		// permitido para las firmas EPES y B-Level.
		if (extraParams.containsKey(PdfExtraParams.COMMITMENT_TYPE_INDICATIONS)
				&& !AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)
				&& !extraParams.containsKey(PdfExtraParams.POLICY_IDENTIFIER)) {
			LOGGER.warning("Se ignoraran los commitment type indications establecidos por no estar permitidos en las firmas PAdES-EPES"); //$NON-NLS-1$
			extraParams.remove(PdfExtraParams.COMMITMENT_TYPE_INDICATIONS);
		}

		// Si se indico una politica de firma y una razon de firma, se omitira la
		// razon de firma
		if (extraParams.containsKey(PdfExtraParams.SIGN_REASON) &&
				extraParams.containsKey(PdfExtraParams.POLICY_IDENTIFIER)) {
				LOGGER.warning("Se ignorara la razon de firma establecida por haberse indicado una politica de firma"); //$NON-NLS-1$
				extraParams.remove(PdfExtraParams.SIGN_REASON);
		}

		// Si se declaran commintment type indications y una razon de firma,
		// solo se tendra en cuenta la razon de firma. El uso de la comprobacion
		// anterior y esta, permitiria usar politica de firma y  commitment
		// type indications simultaneamente
		if (extraParams.containsKey(PdfExtraParams.SIGN_REASON) &&
				extraParams.containsKey(PdfExtraParams.COMMITMENT_TYPE_INDICATIONS)) {
				LOGGER.warning("Se ignoraran los commitment type indications establecidos por haberse indicado una razon de firma"); //$NON-NLS-1$
				extraParams.remove(PdfExtraParams.COMMITMENT_TYPE_INDICATIONS);
		}
    }

    @Override
    public void setSecureMode(final boolean secure) {
    	this.secureMode = secure;
    }

    @Override
    public boolean isSecureMode() {
    	return this.secureMode;
    }

}
