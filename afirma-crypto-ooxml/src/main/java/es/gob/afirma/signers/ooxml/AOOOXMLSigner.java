/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.OfficeAnalizer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.ooxml.relprovider.OOXMLProvider;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;

/** Manejador de firmas electr&oacute;nicas XML de documentos OOXML de Microsoft Office. */
public final class AOOOXMLSigner implements AOSigner {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String EXTENSION_DOCX = ".docx"; //$NON-NLS-1$
    private static final String EXTENSION_XLSX = ".xlsx"; //$NON-NLS-1$
    private static final String EXTENSION_PPTX = ".pptx"; //$NON-NLS-1$
    private static final String EXTENSION_PPSX = ".ppsx"; //$NON-NLS-1$
    private static final String EXTENSION_OOXML = ".ooxml"; //$NON-NLS-1$

    private static final int THRESHOLD_FILE_SIZE = 1000000000; // 1 GB

    // Instalamos el proveedor de Apache. Esto es necesario para evitar problemas con los saltos de linea
    // de los Base 64
    static {
    	XmlDSigProviderHelper.configureXmlDSigProvider();
    }

    /** Consutruye un firmador OOXML, comprobando que se cuente con un JRE adecuado. */
    public AOOOXMLSigner() {
    	if (System.getProperty("java.version").startsWith("1.6")) { //$NON-NLS-1$ //$NON-NLS-2$
    		throw new UnsupportedJreVersionException();
    	}

        // Proveedor de transformadas de relacion OOXML.
        try {
        	final Provider provider = Security.getProvider(OOXMLProvider.RELATIONSHIP_TRANSFORM_PROVIDER_NAME);
        	if (provider == null) {
        		Security.addProvider(new OOXMLProvider());
        	}
        }
        catch (final Throwable e) {
        	LOGGER.log(Level.WARNING, "Error en la instalacion del proveedor OOXML: " + e, e); //$NON-NLS-1$
        }
    }

    /** Si la entrada es un documento OOXML, devuelve el mismo documento sin ninguna modificaci&oacute;n.
     * @param sign Documento OOXML
     * @return Documento de entrada si este es OOXML, <code>null</code> en cualquier otro caso. */
	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException, IOException, AOException {
		return getData(sign, null) ;
	}

    @Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOException {

        // Si no es una firma OOXML valida, lanzamos una excepcion
        if (!isSign(sign)) {
            throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
        }

        // Devolvemos el propio OOXML firmado.
        return sign;
    }

    /** Comprueba que unos datos se adecuen a la estructura b&aacute;sica de un
     * documento OOXML.
     * @param data Datos que deseamos analizar.
     * @return {@code true} si el documento es un OOXML, {@code false} en caso
     *         de no ser un OOXML o de no poder .
     * @throws IOException Cuando hay problemas en el tratamiento de los datos. */
    private static boolean isOOXMLFile(final byte[] data) throws IOException {

    	// Si no es un ZIP, no se trata de un OOXML
    	if (!isZipData(data)) {
    		return false;
    	}

    	boolean result = true;
    	try {

    		final List<ZipEntry> entryList = OOXMLUtil.getEntryList(data);
    		if (!OOXMLUtil.hasEntry(entryList, "[Content_Types].xml") //$NON-NLS-1$
    			|| !OOXMLUtil.hasEntry(entryList, "_rels/.rels") && !OOXMLUtil.hasEntry(entryList, "_rels\\.rels") //$NON-NLS-1$ //$NON-NLS-2$
    			|| !OOXMLUtil.hasEntry(entryList, "docProps/app.xml") && !OOXMLUtil.hasEntry(entryList, "docProps\\app.xml") //$NON-NLS-1$ //$NON-NLS-2$
    			|| !OOXMLUtil.hasEntry(entryList, "docProps/core.xml") && !OOXMLUtil.hasEntry(entryList, "docProps\\core.xml")) { //$NON-NLS-1$ //$NON-NLS-2$
    			result = false;
    		}
    	}
    	catch (final Exception e) {
    		result = false;
    	}

        return result;
    }

    /**
     * Comprueba si unos datos tienen formato ZIP.
     * @param data Datos a comprobar.
     * @return {@code true} si los datos son un ZIP, {@code false} en caso contrario.
     * @throws IOException Cuando no se pueden analizar los datos.
     */
    private static boolean isZipData(final byte[] data) throws IOException {
    	return new MimeHelper(data).isZipData();
    }

    /** { {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException, IOException {
		return getSignInfo(data, null);
	}

    /** { {@inheritDoc} */
    @Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(data)) {
            throw new AOFormatFileException("Los datos introducidos no se corresponden con documento OOXML"); //$NON-NLS-1$
        }

        // Aqui vendria el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_OOXML);
    }

    /** { {@inheritDoc} */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$
        if (originalName == null) {
            return inTextInt + EXTENSION_OOXML;
        }
        final String originalNameLC = originalName.toLowerCase();
        if (originalNameLC.endsWith(EXTENSION_DOCX)) {
            return originalName.substring(0, originalName.length() - EXTENSION_DOCX.length()) + inTextInt + EXTENSION_DOCX;
        }
        if (originalNameLC.endsWith(EXTENSION_XLSX)) {
            return originalName.substring(0, originalName.length() - EXTENSION_XLSX.length()) + inTextInt + EXTENSION_XLSX;
        }
        if (originalNameLC.endsWith(EXTENSION_PPTX)) {
            return originalName.substring(0, originalName.length() - EXTENSION_PPTX.length()) + inTextInt + EXTENSION_PPTX;
        }
        if (originalNameLC.endsWith(EXTENSION_PPSX)) {
            return originalName.substring(0, originalName.length() - EXTENSION_PPSX.length()) + inTextInt + EXTENSION_PPSX;
        }
        return originalName + inTextInt + EXTENSION_OOXML;
    }

    /** { {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo)
			throws IOException {
		return getSignersStructure(sign, null, asSimpleSignInfo);
	}

    /** { {@inheritDoc} */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo) throws IOException {
        if (sign == null) {
            throw new IllegalArgumentException("Los datos de firma introducidos son nulos"); //$NON-NLS-1$
        }

        if (sign.length >= THRESHOLD_FILE_SIZE) {
    		throw new IOException("El archivo tiene un tamano superior al permitido."); //$NON-NLS-1$
    	}

        if (!isSign(sign)) {
            LOGGER.severe("La firma indicada no es de tipo OOXML"); //$NON-NLS-1$
            return null;
        }

        // Las firmas contenidas en el documento OOXML son de tipo XAdES asi
        // que utilizaremos el signer de este tipo para gestionar el arbol de firmas
        final AOSigner xmldsigSigner = new AOXAdESSigner();

        // Recuperamos las firmas individuales del documento y creamos el arbol
        final AOTreeNode tree = new AOTreeNode("Datos"); //$NON-NLS-1$
        try {
            for (final byte[] elementSign : OOXMLUtil.getOOXMLSignatures(sign)) {
                // Recuperamos el arbol de firmas de la firma individual. Ya que
                // esta sera una firma simple solo debe contener un nodo de firma.
            	// Ignoramos la raiz del arbol, que contiene el ejemplo
            	// representativo de los datos firmados y no de la propia firma.
                final AOTreeModel signTree = xmldsigSigner.getSignersStructure(elementSign, asSimpleSignInfo);
                tree.add(((AOTreeNode) signTree.getRoot()).getChildAt(0));
            }
        }
        catch (final Exception e) {
            LOGGER.severe("La estructura de una de las firmas elementales no es valida: " + e); //$NON-NLS-1$
            return null;
        }

        return new AOTreeModel(tree);
    }

    /** Indica si los datos indicados son un documento OOXML susceptible de contener una firma
     * electr&oacute;nica.
     * @param sign Datos que deseamos comprobar.
     * @return Devuelve <code>true</code> si los datos indicados son un documento OOXML susceptible de contener una firma
     * electr&oacute;nica, <code>false</code> en caso contrario. */
	@Override
	public boolean isSign(final byte[] sign){
		return isSign(sign, null);
	}

    /** Indica si los datos indicados son un documento OOXML susceptible de contener una firma
     * electr&oacute;nica.
     * @param sign Datos que deseamos comprobar.
     * @param params Par&aacute;metros necesarios para comprobar si los datos de la firma son compatibles.
     * @return Devuelve <code>true</code> si los datos indicados son un documento OOXML susceptible de contener una firma
     * electr&oacute;nica, <code>false</code> en caso contrario. */
    @Override
	public boolean isSign(final byte[] sign, final Properties params) {
        if (sign == null) {
            LOGGER.warning("Se ha introducido una firma nula para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        try {
        	return isOOXMLFile(sign) && OOXMLUtil.countOOXMLSignatures(sign) > 0;
        }
        catch(final Exception e) {
        	return false;
        }
    }

    /** Indica si los datos son un documento OOXML susceptible de ser firmado.
     * @param data Datos a comprobar
     * @return <code>true</code> si los datos son un documento OOXML susceptible de ser firmado, <code>false</code> en caso contrario */
    @Override
	public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        try {
        	return isOOXMLFile(data);
        }
        catch(final Exception e) {
        	return false;
        }
    }

    /** Agrega una firma electr&oacute;nica a un documento OOXML.
     * @param data Documento OOXML.
     * @param algorithm Algoritmo de firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada del firmante.
     * @param certChain Cadena de certificados del firmante.
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Documento OOXML firmado.
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma.
     * @throws IOException Cuando hay errores en la lectura de los datos. */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final java.security.cert.Certificate[] certChain,
                       final Properties extraParams) throws AOException, IOException {

    	if (data.length >= THRESHOLD_FILE_SIZE) {
    		throw new IOException("El archivo tiene un tamano superior al permitido."); //$NON-NLS-1$
    	}

        // Comprobamos si es un documento OOXML valido.
        if (!OfficeAnalizer.isOOXMLDocument(data)) {
            throw new AOFormatFileException("Los datos introducidos no se corresponden con un documento OOXML"); //$NON-NLS-1$
        }

        if (certChain == null || certChain.length < 1) {
        	throw new IllegalArgumentException("Debe proporcionarse a menos el certificado del firmante"); //$NON-NLS-1$
        }

        final Properties xParams = extraParams != null ? extraParams : new Properties();

        return signOOXML(
    		data,
    		algorithm,
    		key,
    		new X509Certificate[] { (X509Certificate) certChain[0] }, // Office 2016 no acepta cadenas, solo debe estar el cert del firmante
    		xParams
		);
    }

    /** Agrega una firma electr&oacute;nica a un documento OOXML.
     * Este m&eacute;todo es completamente equivalente a <code>sign(byte[], String, PrivateKeyEntry, Properties)</code>.
     * @param sign Documento OOXML
     * @param algorithm Algoritmo de firma
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada del firmante
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento OOXML firmado
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma
     * @throws IOException Cuando hay errores en la lectura de los datos */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException, IOException {
    	return sign(sign, algorithm, key, certChain, extraParams);
    }

    /** Agrega una firma electr&oacute;nica a un documento OOXML.
     * Este m&eacute;todo es completamente equivalente a <code>sign(byte[], String, PrivateKeyEntry, Properties)</code>.
     * @param data No usado, se ignora el valor de este par&aacute;metro
     * @param sign Documento OOXML
     * @param algorithm Algoritmo de firma
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param key Clave privada del firmante
     * @param certChain Cadena de certificados del firmante
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento OOXML firmado
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma
     * @throws IOException Cuando hay errores en la lectura de los datos */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties extraParams) throws AOException, IOException {
    	return cosign(sign, algorithm, key, certChain, extraParams);
    }

    /** M&eacute;todo no implementado. No es posible realizar contrafirmas de
     * documentos OOXML. Lanza una <code>UnsupportedOperationException</code>. */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties extraParams) {
        throw new UnsupportedOperationException("No es posible realizar contrafirmas de ficheros OOXML"); //$NON-NLS-1$
    }

    /** Agrega una firma electr&oacute;nica a un documento OOXML.
     * @param ooxmlDocument Documento OOXML.
     * @param algorithm Algoritmo de firma
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512</i></li>
     * </ul>
     * @param key Clave privada del firmante
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Documento OOXML firmado
     * @throws AOException Cuando ocurre alg&uacute;n error durante el proceso de firma */
    private static byte[] signOOXML(final byte[] ooxmlDocument,
                                    final String algorithm,
                                    final PrivateKey key,
                                    final X509Certificate[] certChain,
                                    final Properties xParams) throws AOException {

        if (key == null) {
            throw new IllegalArgumentException("No se ha proporcionado una clave valida"); //$NON-NLS-1$
        }

        try {
            return OOXMLZipHelper.outputSignedOfficeOpenXMLDocument(
        		ooxmlDocument,
        		OOXMLXAdESSigner.getSignedXML(ooxmlDocument, algorithm, key, certChain, xParams)
    		);
        }
        catch (final AOException e) {
        	throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error durante la firma OOXML: " + e, e); //$NON-NLS-1$
        }
    }

}
