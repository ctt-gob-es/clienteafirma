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
import java.lang.reflect.Field;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.DocumentException;
import com.lowagie.text.exceptions.BadPasswordException;
import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.PdfPKCS7;
import com.lowagie.text.pdf.PdfReader;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

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
public final class AOPDFSigner implements AOSigner {

    private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$
    private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

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
     * @param inPDF Documento PDF a firmar
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento PDF firmado en formato PAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Cuando hay errores en el tratamiento de datos. */
    @Override
	public byte[] sign(final byte[] inPDF,
			           final String algorithm,
			           final PrivateKey key,
			           final java.security.cert.Certificate[] certChain,
			           final Properties xParams) throws AOException, IOException {

        final Properties extraParams = xParams != null ? xParams : new Properties();

        checkIText();

        final java.security.cert.Certificate[] certificateChain = Boolean.parseBoolean(extraParams.getProperty("includeOnlySignningCertificate", Boolean.FALSE.toString())) ? //$NON-NLS-1$
    		new X509Certificate[] { (X509Certificate) certChain[0] } :
    			certChain;

        final GregorianCalendar signTime = new GregorianCalendar();

        // Sello de stiempo
        byte[] data;
		try {
			data = PdfTimestamper.timestampPdf(inPDF, extraParams, signTime);
		}
		catch (final NoSuchAlgorithmException e1) {
			throw new IOException(
				"No se soporta el algoritmo indicado para la huella digital del sello de tiempo: " + e1, e1 //$NON-NLS-1$
			);
		}

		// Prefirma
        final PdfSignResult pre;
        try {
			pre = PAdESTriPhaseSigner.preSign(
				algorithm,
				data,
				certificateChain,
				signTime,
				extraParams
			);
		}
        catch (final DocumentException e) {
			throw new InvalidPdfException(e);
		}

        // Firma PKCS#1
        final byte[] interSign = new AOPkcs1Signer().sign(
    		pre.getSign(),
    		algorithm,
    		key,
    		certificateChain,
    		extraParams
		);

        // Postfirma
        try {
			return PAdESTriPhaseSigner.postSign(
				algorithm,
				data,
				certificateChain,
				interSign,
				pre,
				null,
				null
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
     * En general, es recomendable prescindir de este m&eacute;todo y llamar directamente al m&eacute;todo <code>sign(...)</code>
     * @param data Se ignora el valor de este par&aacute;metro. <b>El documento PDF debe proporcionarse mediante el par&aacute;tro <code>sign</code></b>.
     * @param sign Documento PDF a firmar
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     * @throws IOException En caso de errores de entrada / salida */
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
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento PDF firmado en formato PAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     * @throws IOException En caso de errores de entrada / salida */
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
     * @param asSimpleSignInfo
     *        Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres (CN X.500) de los
     *        titulares certificados.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de error. */
    @Override
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {

    	final AOTreeNode root = new AOTreeNode("Datos"); //$NON-NLS-1$

    	if (!isPdfFile(sign)) {
    		return new AOTreeModel(root);
    	}

    	PdfReader pdfReader;
    	try {
    		pdfReader = new PdfReader(sign);
    	}
    	catch (final BadPasswordException e) {
    		try {
    			pdfReader = new PdfReader(
					sign,
					new String(
						AOUIFactory.getPassword(
							CommonPdfMessages.getString("AOPDFSigner.0"), //$NON-NLS-1$
							null
						)
					).getBytes()
				);
    		}
    		catch (final BadPasswordException e2) {
    			LOGGER.severe("La contrasena del PDF no es valida, se devolvera un arbol vacio: " + e2); //$NON-NLS-1$
    			return new AOTreeModel(root);
    		}
    		catch (final Exception e3) {
    			LOGGER.severe("No se ha podido leer el PDF, se devolvera un arbol vacio: " + e3); //$NON-NLS-1$
    			return new AOTreeModel(root);
    		}
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
    	Object pkcs1Object = null;
    	for (int i = 0; i < names.size(); ++i) {
    		final PdfPKCS7 pcks7;
    		try {
    			pcks7 = af.verifySignature(names.get(i).toString());
    		}
    		catch(final Exception e) {
    			LOGGER.severe(
					"El PDF contiene una firma corrupta o con un formato desconocido (" + //$NON-NLS-1$
						names.get(i).toString() +
							"), se continua con las siguientes si las hubiese: " + e //$NON-NLS-1$
				);
    			continue;
    		}
    		if (asSimpleSignInfo) {

       			final X509Certificate[] certChain = new X509Certificate[pcks7.getSignCertificateChain().length];
    			for (int j = 0; j < certChain.length; j++) {
    				certChain[j] = (X509Certificate) pcks7.getSignCertificateChain()[j];
    			}

    			final AOSimpleSignInfo ssi = new AOSimpleSignInfo(
					certChain,
					pcks7.getSignDate().getTime()
				);

    			// Extraemos el PKCS1 de la firma
    			try {
    				// iText antiguo
    				final Field digestField = Class.forName("com.lowagie.text.pdf.PdfPKCS7").getDeclaredField("digest"); //$NON-NLS-1$ //$NON-NLS-2$
    				digestField.setAccessible(true);
    				pkcs1Object = digestField.get(pcks7);
    			}
    			catch (final Exception e) {
    				LOGGER.severe(
						"No se ha podido obtener informacion de una de las firmas del PDF, se continuara con la siguiente: " + e //$NON-NLS-1$
					);
    				continue;
    			}
    			if (pkcs1Object instanceof byte[]) {
    				ssi.setPkcs1((byte[]) pkcs1Object);
    			}
    			root.add(new AOTreeNode(ssi));
    		}
    		else {
    			root.add(new AOTreeNode(AOUtil.getCN(pcks7.getSigningCertificate())));
    		}
    	}

    	return new AOTreeModel(root);
    }

    /** Comprueba que los datos proporcionados sean un documento PDF.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos proporcionados son un documento PDF, <code>false</code> en caso contrario */
    @Override
	public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        if (!isPdfFile(data)) {
        	return false;
        }
        final Object root = getSignersStructure(data, false).getRoot();
        if (root instanceof AOTreeNode) {
        	// Si el arbol contiene firmas...
        	if (AOTreeModel.getChildCount(root) > 0) {
        		return true;
        	}
        	// Si no las contiene aun puede haber firmas no registradas

        	// Como el metodo no recibe "extraParams" buscamos en las propiedades de sistema
        	final Properties extraParams = System.getProperties();
        	try {
				if (PdfUtil.pdfHasUnregisteredSignatures(data, extraParams) &&
						Boolean.TRUE.toString().equalsIgnoreCase(extraParams.getProperty("allowCosigningUnregisteredSignatures"))) { //$NON-NLS-1$
					return true;
				}
			}
        	catch (final Exception e) {
				LOGGER.severe("No se han podido comprobar las firmas no registradas del PDF: " + e); //$NON-NLS-1$
			}
        }
        return false;
    }

    @SuppressWarnings("unused")
    private boolean isPdfFile(final byte[] data) {

    	checkIText();

        byte[] buffer = new byte[PDF_FILE_HEADER.length()];
        try {
            new ByteArrayInputStream(data).read(buffer);
        }
        catch (final Exception e) {
            buffer = null;
        }

        // Comprobamos que cuente con una cabecera PDF
        if (buffer != null && !PDF_FILE_HEADER.equals(new String(buffer))) {
            return false;
        }

        try {
            // Si lanza una excepcion al crear la instancia, no es un fichero PDF
            new PdfReader(data);
        }
        catch (final BadPasswordException e) {
            LOGGER.warning("El PDF esta protegido con contrasena, se toma como PDF valido"); //$NON-NLS-1$
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
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia que se almacenan en el objeto AOSignInfo
    }

    /** Configura, cuando no lo esten ya, las propiedades necesarias para que las firmas
     * sobre unos datos respeten el formato que tuviesen firmas anteriores.
     * @param data Datos que se desean firmar.
     * @param config Configuraci&oacute;n establecida. */
    public static void configureRespectfulProperties(final byte[] data, final Properties config) {

    	if (config != null && !config.containsKey("signatureSubFilter")) { //$NON-NLS-1$
    		String filter;
    		try {
    			filter = PdfUtil.getFirstSupportedSignSubFilter(data, config);
    		}
    		catch (final Exception e) {
    			LOGGER.warning("Error al configurar la firma PDF para que sea igual a las existentes: " + e); //$NON-NLS-1$
    			return;
    		}

    		if (filter != null) {
    			config.setProperty("signatureSubFilter", filter.substring(filter.indexOf('/') + 1)); //$NON-NLS-1$
    		}
    	}
    }

    @SuppressWarnings("static-method")
	private void checkIText() {
    	try {
    		PdfReader.isAfirmaModifiedItext();
    	}
    	catch(final Exception e) {
    		throw new InvalidITextException(e);
    	}
    }

}
