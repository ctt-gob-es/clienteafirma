/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.massive;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;


/** M&oacute;dulo para el soporte de multifirmas m&aacute;sivas. Permite
 * configurar una operaci&oacute;n de firma y ejecutarla sobre datos, hashes y
 * ficheros.<br> Se crea un log de la operaci&oacute;n masiva en donde cada
 * entrada se corresponde con el resultado de la ejecuci&oacute;n de una
 * operaci&oacute;n. */
public final class MassiveSignatureHelper {

	private static final String CADES_SIGNER = "es.gob.afirma.signers.cades.AOCAdESSigner"; //$NON-NLS-1$
    private static final String XADES_SIGNER = "es.gob.afirma.signers.xades.AOXAdESSigner"; //$NON-NLS-1$
    private static final String XMLDSIG_SIGNER = "es.gob.afirma.signers.xmldsig.AOXMLDSigSigner"; //$NON-NLS-1$

    private static final String REG_FIELD_SEPARATOR = " - "; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String MODE = "mode"; //$NON-NLS-1$
    private static final String FORMAT = "format"; //$NON-NLS-1$
    private static final String MIME_TYPE = "mimeType"; //$NON-NLS-1$

    private static final AOException SIGN_DATA_NOT_GENERATED = new AOException("No se generaron datos de firma"); //$NON-NLS-1$

	/** Generador de huellas digitales utilizado internamente. */
    private static MessageDigest md = null;

    /** Configuracion de la operaci&oacute;n masiva. */
    private MassiveSignConfiguration massiveConfiguration = null;

    /** Logger de las operaciones de firma masiva. */
    private List<String> log = null;

    /** Manejador de firma para el formato configurado por defecto. */
    private AOSigner defaultSigner = null;

    /** Manejador de firma exclusivo para la operaci&oacute;n de firma masiva. */
    private AOSigner massiveSignSigner = null;

    /** Indica si el objeto esta activo y preparado para ejecutar operaciones de firma. */
    private boolean enabled = false;

    /** Contruye el m&oacute;dulo de soporte para la multifirma masiva.
     * @param configuration
     *        Configuracion de la operaci&oacute;n.
     * @throws AOException
     *         La configuraci&oacute;n introducida no es v&aacute;lida. */
    public MassiveSignatureHelper(final MassiveSignConfiguration configuration) throws AOException {

        if (configuration == null) {
            throw new IllegalArgumentException("La configuracion de firma masiva no puede ser nula"); //$NON-NLS-1$
        }
        if (configuration.getMassiveOperation() == null) {
            throw new IllegalArgumentException("La configuracion indicada no tiene establecida ninguna operacion masiva"); //$NON-NLS-1$
        }

        // Establecemos los valores y nos aseguramos de que el formato de firma
        // inicialmente establecido es el formato de firma que hemos configurado
        // por defecto
        this.massiveConfiguration = configuration;
        this.massiveConfiguration.setSignatureFormat(this.massiveConfiguration.getDefaultFormat());
        this.enabled = true;

        // Creamos el manejador de firma por defecto
        this.defaultSigner = AOSignerFactory.getSigner(this.massiveConfiguration.getDefaultFormat());
        if (this.defaultSigner == null) {
            throw new AOException("Formato de firma no soportado: " + this.massiveConfiguration.getDefaultFormat()); //$NON-NLS-1$
        }
        this.massiveSignSigner = this.defaultSigner;
    }

    /**
     * Indica si el objeto esta inicializado correctamente con una configuraci&oacute;n de firma.
     * Este m&eacute;todo s&oacute;lo devolver&aacute; {@code false} despu&eacute;s de ejecutar
     * el m&acute;todo {{@link #disable()}.
     * @return Indica si el helper esta preparado para ejecutar operaciones.
     */
    public boolean isEnabled() {
    	return this.enabled;
    }

    /**
     * Elimina la configuracion establecida inhabilitando la operaci&oacute;n de este objeto.
     */
    public void disable() {
    	this.massiveConfiguration = null;
    	this.defaultSigner = null;
    	this.enabled = false;
    }

    /** Establece el tipo de operaci&oacute;n (firma, cofirma, contrafirma del
     * &aacute;rbol de firma o contrafirma de nodos hojas) que debe realizar el
     * m&oacute;dulo de firma masiva. Si se indica <code>null</code> se
     * establece la configuraci&oacute;n por defecto (firma).
     * @param massiveOperation
     *        Tipo de operaci&oacute;n.
     * @see MassiveType */
    public void setMassiveOperation(final MassiveType massiveOperation) {
    	this.massiveConfiguration.setMassiveOperation(
            massiveOperation != null ? massiveOperation : MassiveType.SIGN
        );
    }

    /** Establece el formato de firma para una operaci&oacute;n de firma masiva. Este
     * m&eacute;todo s&oacute;lo debe utilizarse cuando el formato de firma cambie durante
     * el proceso de firma masiva. Si no es llama a este m&eacute;todo se usar&aacute;
     * siempre el formato indicado como "defaultFormat".<br>
     * Por uniformidad en el resultado, cuando se encuentran configuradas las operaciones
     * masivas cofirma, contrafirma del &aacute;rbol de firma o contrafirma de nodos hojas,
     * se usar&aacute; siempre el formato de firma por defecto. Si se indica <code>null</code>
     * se establece la configuraci&oacute;n por defecto (defaultFormat).
     * @param signatureFormat Formato de firma.*/
    public void setSignatureFormat(final String signatureFormat) {

    	// Si el formato establecido es el actual, se evita volver a cargar un manejador de firma
    	if (this.massiveConfiguration.getSignatureFormat().equals(signatureFormat)) {
    		return;
    	}

    	this.massiveConfiguration.setSignatureFormat(signatureFormat);
    	this.massiveSignSigner = AOSignerFactory.getSigner(this.massiveConfiguration.getSignatureFormat());
    	if (this.massiveSignSigner == null) {
    		LOGGER.warning("No hay disponible un manejador de firma para el formato " + signatureFormat + //$NON-NLS-1$
    				", se utilizara el formato de firma por defecto: " + this.massiveConfiguration.getDefaultFormat()); //$NON-NLS-1$
    		this.massiveConfiguration.setSignatureFormat(this.massiveConfiguration.getDefaultFormat());
    		this.massiveSignSigner = this.defaultSigner;
        }
    }

    /** Realiza la firma de datos.
     * @param data
     *        Datos que se desean firmar.
     * @return Resultado de la firma. */
    public byte[] signData(final byte[] data) {

        if (data == null) {
            LOGGER.severe("No se han introducido datos para firmar"); //$NON-NLS-1$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.0")); //$NON-NLS-1$
            return null;
        }

        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone(); // Configuracion

        config.setProperty("headless", Boolean.toString(true));  //$NON-NLS-1$
        byte[] signData = null; // Firma resultante

        // Ejecutamos la operacion que corresponda
        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
                signData = signDataFromData(this.massiveSignSigner, data, null, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COSIGN)) { // Cofirma
                signData = cosign(this.defaultSigner, data, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COUNTERSIGN_ALL)) { // Contraforma del arbol completo
                signData = countersignTree(this.defaultSigner, data, config);
            }
            else { // Contrafirma de los nodos hoja
                signData = countersignLeafs(this.defaultSigner, data, config);
            }
        }
        catch (final AOFormatFileException e) {
            LOGGER.severe("Los datos introducidos no tienen un formato valido: " + e); //$NON-NLS-1$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.1")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("Error durante la operacion " + this.massiveConfiguration.getMassiveOperation() + " sobre los datos introducidos: " + e.getMessage());  //$NON-NLS-1$//$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.2") + REG_FIELD_SEPARATOR + this.massiveConfiguration.getMassiveOperation() + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
			addLog(MassiveSignMessages.getString("MassiveSignatureHelper.16")); //$NON-NLS-1$
			return null;
		}

        addLog(MassiveSignMessages.getString("MassiveSignatureHelper.3")); //$NON-NLS-1$

        return signData;
    }

    /** Realiza la firma de un hash. La cofirma y contrafirma de hashes no esta
     * soportada.
     * @param hash
     *        Hash que se desea firmar.
     * @return Resultado de la firma. */
    public byte[] signHash(final byte[] hash) {

        if (hash == null) {
            LOGGER.severe("No se ha introducido un hash para firmar"); //$NON-NLS-1$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.4")); //$NON-NLS-1$
            return null;
        }

        // Solo para aclarar los posibles mensajes por consola, almacenaremos
        String operation = "sign"; //$NON-NLS-1$

        // Firma resultante
        byte[] signData = null;

        // Ejecutamos la operacion que corresponda
        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone();
        config.setProperty("headless", Boolean.toString(true)); //$NON-NLS-1$
        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
            	checkvalidSignFormat(this.massiveConfiguration);
                operation = "sign"; //$NON-NLS-1$
                signData = signDataFromHash(this.massiveSignSigner, hash, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COSIGN)) { // Cofirma
                operation = "cosign"; //$NON-NLS-1$
                throw new UnsupportedOperationException("La cofirma de un hash no es una operacion valida"); //$NON-NLS-1$
            }
            else { // Contrafirma
                operation = "countersign"; //$NON-NLS-1$
                throw new UnsupportedOperationException("La contrafirma de un hash no es una operacion valida"); //$NON-NLS-1$
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error al operar sobre el hash indicado (" + operation + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.5") + REG_FIELD_SEPARATOR + operation + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
			addLog(MassiveSignMessages.getString("MassiveSignatureHelper.16")); //$NON-NLS-1$
			return null;
		}

        addLog("Operaci\u00F3n sobre hash: Correcta"); //$NON-NLS-1$

        return signData;
    }

    private static void checkvalidSignFormat(final MassiveSignConfiguration config) {
    	if (AOSignConstants.SIGN_FORMAT_PADES.equals(config.getSignatureFormat()) ||
    			AOSignConstants.SIGN_FORMAT_PDF.equals(config.getSignatureFormat())) {
            throw new UnsupportedOperationException("El formato de firma PDF/PAdES no puede realizarse sobre un hash"); //$NON-NLS-1$
        } else if (AOSignConstants.SIGN_FORMAT_ODF.equals(config.getSignatureFormat()) ||
        		AOSignConstants.SIGN_FORMAT_ODF_ALT1.equals(config.getSignatureFormat())) {
            throw new UnsupportedOperationException("El formato de firma ODF no puede realizarse sobre un hash"); //$NON-NLS-1$
        } else if (AOSignConstants.SIGN_FORMAT_OOXML.equals(config.getSignatureFormat()) ||
        		AOSignConstants.SIGN_FORMAT_OOXML_ALT1.equals(config.getSignatureFormat())) {
            throw new UnsupportedOperationException("El formato de firma OOXML no puede realizarse sobre un hash"); //$NON-NLS-1$
        } else if (AOSignConstants.SIGN_FORMAT_FACTURAE.equals(config.getSignatureFormat()) ||
        		AOSignConstants.SIGN_FORMAT_FACTURAE_ALT1.equals(config.getSignatureFormat())) {
            throw new UnsupportedOperationException("El formato de firma OOXML no puede realizarse sobre un hash"); //$NON-NLS-1$
        }
    }

    /** Realiza la operaci&oacute;n de multifirma sobre un fichero.
     * @param fileUri
     *        Path del fichero que se desea firmar.
     * @return Resultado de la firma. */
    public byte[] signFile(final String fileUri) {

        if (fileUri == null) {
            LOGGER.severe("No se ha introducido un fichero para firmar"); //$NON-NLS-1$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.6")); //$NON-NLS-1$
            return null;
        }

        // Creamos la URI del fichero
        final URI uri;
        try {
            uri = AOUtil.createURI(fileUri);
        }
        catch (final Exception e) {
            LOGGER.severe("La URI '" + fileUri + "' no posee un formato valido: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.7") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }

        // Creamos el flujo de datos del fichero y leemos su contenido
        final byte[] data;
        try (
    		final InputStream is = AOUtil.loadFile(uri);
		) {
        	data = AOUtil.getDataFromInputStream(is);
        }
        catch (final FileNotFoundException e) {
            LOGGER.severe("No ha sido posible encontrar el fichero '" + LoggerUtil.getCleanUserHomePath(fileUri) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.8") + REG_FIELD_SEPARATOR + LoggerUtil.getCleanUserHomePath(fileUri)); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("No es posible acceder al contenido del fichero '" + LoggerUtil.getCleanUserHomePath(fileUri) + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.9") + REG_FIELD_SEPARATOR + LoggerUtil.getCleanUserHomePath(fileUri)); //$NON-NLS-1$
            return null;
        }

        if (data == null) {
            LOGGER.severe("El fichero '" + fileUri + "' esta vacio"); //$NON-NLS-1$ //$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.12") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }

        // Ejecutamos la operacion que corresponda
        final byte[] signData;
        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone();
        config.setProperty("headless", Boolean.toString(true));  //$NON-NLS-1$
        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
                signData = signDataFromData(this.massiveSignSigner, data, uri, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COSIGN)) { // Cofirma
                signData = cosign(this.defaultSigner, data, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COUNTERSIGN_ALL)) { // Contraforma del arbol completo
                signData = countersignTree(this.defaultSigner, data, config);
            }
            else { // Contraforma de los nodos hoja
                signData = countersignLeafs(this.defaultSigner, data, config);
            }
        }
        catch (final AOFormatFileException e) {
            LOGGER.severe("El fichero '" + LoggerUtil.getCleanUserHomePath(fileUri) + "' no tiene un formato valido: " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.13") + REG_FIELD_SEPARATOR + LoggerUtil.getCleanUserHomePath(fileUri)); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("Error al realizar la operacion "  //$NON-NLS-1$
                    + this.massiveConfiguration.getMassiveOperation()
                    + " sobre el fichero '" //$NON-NLS-1$
                    + LoggerUtil.getCleanUserHomePath(fileUri)
                    + "': " //$NON-NLS-1$
                    + e.getMessage());
            addLog(MassiveSignMessages.getString("MassiveSignatureHelper.14") + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
			addLog(MassiveSignMessages.getString("MassiveSignatureHelper.16")); //$NON-NLS-1$
			return null;
		}

        addLog(MassiveSignMessages.getString("MassiveSignatureHelper.15")); //$NON-NLS-1$

        return signData;
    }

    /** Firma datos con el firmador indicado.
     * @param signer Manejador con el que firmar los datos.
     * @param data Datos a firmar.
     * @param uri URI de los datos a firmar (opcional seg&uacute;n formato de firma).
     * @param config Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de firma.
     * @throws AOException Cuando ocurre un error durante la operaci&oacute;n de firma.
     * @throws IOException Cuando ocurre un error durante la lectura o escritura de los datos. */
    private byte[] signDataFromData(final AOSigner signer, final byte[] data, final URI uri, final Properties config) throws AOException, IOException {

        // Configuramos y ejecutamos la operacion
    	if (!config.containsKey(MODE)) {
    		config.setProperty(MODE, this.massiveConfiguration.getMode());
    	}
    	if (!config.containsKey(FORMAT)) {
    		config.setProperty(FORMAT, this.massiveConfiguration.getSignatureFormat());
    	}
        if (uri != null) {
            config.setProperty("uri", uri.toString()); //$NON-NLS-1$
        }

        byte[] dataToSign = data;

        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(this.massiveConfiguration.getAlgorithm());

        // Deteccion del MIMEType u OID del tipo de datos, solo para CAdES, XAdES y XMLDSig
        final String signerClassName = signer.getClass().getName();
        if (CADES_SIGNER.equals(signerClassName) ||
        		XADES_SIGNER.equals(signerClassName) ||
        		XMLDSIG_SIGNER.equals(signerClassName)) {

        	final String mimeType;
        	if (config.containsKey(MIME_TYPE)) {
        		mimeType = config.getProperty(MIME_TYPE);
        	}
        	else if ((XADES_SIGNER.equals(signerClassName) || XMLDSIG_SIGNER.equals(signerClassName))
            		&& AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(config.getProperty(MODE))) {
        		// Forzamos que las firmas XAdES Explicitas firmen el hash de los datos y lo marcamos como tal
        		dataToSign = digest(dataToSign, digestAlgorithm);
        		mimeType = ("hash/" + digestAlgorithm).toLowerCase(); //$NON-NLS-1$
        	}
        	else {
        		final MimeHelper mimeHelper = new MimeHelper(dataToSign);
        		mimeType = mimeHelper.getMimeType();
        	}

        	config.setProperty(MIME_TYPE, mimeType);

        	if (!config.containsKey("contentTypeOid")) { //$NON-NLS-1$
            	final String dataOid = MimeHelper.transformMimeTypeToOid(mimeType);
            	if (dataOid != null) {
            		config.setProperty("contentTypeOid", dataOid); //$NON-NLS-1$
            	}
            }
        }

        final byte[] signData = signer.sign(
        	dataToSign,
            this.massiveConfiguration.getAlgorithm(),
            this.massiveConfiguration.getKeyEntry().getPrivateKey(),
            this.massiveConfiguration.getKeyEntry().getCertificateChain(),
            config
        );

        if (signData == null) {
            throw SIGN_DATA_NOT_GENERATED;
        }
        return signData;
    }

    /** Firma un hash con el signer indicado.
     * @param signer
     *        Manejador con el que firmar el hash.
     * @param data
     *        Hash a firmar.
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato configurado.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de firma.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos.
     */
    private byte[] signDataFromHash(final AOSigner signer, final byte[] data, final Properties config) throws AOException, IOException {

        // Configuramos y ejecutamos la operacion
    	if (!config.containsKey(MODE)) {
    		config.setProperty(MODE, this.massiveConfiguration.getMode());
    	}
    	if (!config.containsKey(FORMAT)) {
    		config.setProperty(FORMAT, this.massiveConfiguration.getSignatureFormat());
    	}
        config.setProperty("precalculatedHashAlgorithm", AOSignConstants.getDigestAlgorithmName(this.massiveConfiguration.getAlgorithm())); //$NON-NLS-1$

        // Introduccion MIMEType "hash/algo", solo para XAdES y XMLDSig
        if (signer.getClass().getName().equals(XADES_SIGNER) || signer.getClass().getName().equals(XMLDSIG_SIGNER)) {
            final String mimeType = "hash/" + AOSignConstants.getDigestAlgorithmName(this.massiveConfiguration.getAlgorithm()).toLowerCase(); //$NON-NLS-1$
            config.setProperty(MIME_TYPE, mimeType);
        }

        final byte[] signData = signer.sign(
    		data,
    		this.massiveConfiguration.getAlgorithm(),
    		this.massiveConfiguration.getKeyEntry().getPrivateKey(),
    		this.massiveConfiguration.getKeyEntry().getCertificateChain(),
    		config
		);
        if (signData == null) {
            throw SIGN_DATA_NOT_GENERATED;
        }
        return signData;
    }

    /** Cofirma datos con el manejador configurado o con el m&aacute;s apropiado
     * si se indic&oacute; que se buscase. Si los datos introducidos no se corresponden
     * con una firma soportada (por el manejador indicado, o cualquier otro, seg&uacute;n
     * configuraci&oacute;n), se realizar&aacute; una firma simple sobre los datos.
     * @param signer Manejador con el que cofirmar los datos.
     * @param sign Firma con los datos a cofirmar.
     * @param config Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException Cuando ocurre un error durante la operaci&oacute;n de firma.
     * @throws IOException Cuando ocurre un error durante la lectura o escritura de los datos. */
    private byte[] cosign(final AOSigner signer, final byte[] sign, final Properties config) throws AOException, IOException {

        // Configuramos y ejecutamos la operacion
    	if (!config.containsKey(MODE)) {
    		config.setProperty(MODE, this.massiveConfiguration.getMode());
    	}

        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        final AOSigner validSigner = getValidSigner(signer, sign);
        final byte[] signData = validSigner.cosign(
    		sign,
    		this.massiveConfiguration.getAlgorithm(),
    		this.massiveConfiguration.getKeyEntry().getPrivateKey(),
    		this.massiveConfiguration.getKeyEntry().getCertificateChain(),
    		config
		);

        if (signData == null) {
            throw SIGN_DATA_NOT_GENERATED;
        }
        return signData;
    }

    /** Contrafirma todos los nodos de firma de los datos de firma introducidos
     * usando el manejador configurado o el m&aacute;s apropiado si se
     * indic&oacute; que se buscase.
     * @param signer Manejador con el que contrafirmar.
     * @param sign Firma a contrafirmar.
     * @param config Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos. */
    private byte[] countersignTree(final AOSigner signer, final byte[] sign, final Properties config) throws AOException, IOException {
        return countersignOperation(signer, sign, CounterSignTarget.TREE, config);
    }

    /** Contrafirma las hojas de la estructura de firma introducida usando el
     * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
     * se buscase.
     * @param signer Manejador con el que contrafirmar.
     * @param sign Firma a contrafirmar.
     * @param config Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos. */
    private byte[] countersignLeafs(final AOSigner signer, final byte[] sign, final Properties config) throws AOException, IOException {
        return countersignOperation(signer, sign, CounterSignTarget.LEAFS, config);
    }

    /** Contrafirma los nodos indicados de una firma electr&oacute;nica usando el
     * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
     * se buscase.
     * @param signer Manejador con el que contrafirmar.
     * @param sign Firma a contrafirmar.
     * @param target Nodos objetivos para la contrafirma.
     * @param config Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma.
     * @throws IOException Si hay problemas en el proceso de datos. */
    private byte[] countersignOperation(final AOSigner signer, final byte[] sign, final CounterSignTarget target, final Properties config) throws AOException, IOException {

        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        final AOSigner validSigner = getValidSigner(signer, sign);

        final byte[] signData = validSigner.countersign(
    		sign,
    		this.massiveConfiguration.getAlgorithm(),
    		target,
    		null,
    		this.massiveConfiguration.getKeyEntry().getPrivateKey(),
    		this.massiveConfiguration.getKeyEntry().getCertificateChain(),
    		config
		);
        if (signData == null) {
            throw SIGN_DATA_NOT_GENERATED;
        }
        return signData;
    }

    /** Recupera el signer apropiado para la cofirma o contrafirma de la firma
     * introducida. Comprueba si se ha establecido que se respete el formato por
     * defecto introducido o si se debe buscar el formato m&aacute;s apropiado
     * seg&uacute;n el tipo de firma.
     * @param signer Manejador de firma.
     * @param signData Firma para la que deseamos obtener un manejador.
     * @return Manejador de firma por defecto compatible para la firma introducida.
     * @throws AOException Si la firma introducida no se corresponde con ning&uacute;n
     *         formato soportado o se obliga a usar el manejador por defecto
     *         y este no la soporta.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos. */
    private AOSigner getValidSigner(final AOSigner signer, final byte[] signData) throws AOException, IOException {
        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        AOSigner validSigner = signer;
        if (!this.massiveConfiguration.isOriginalFormat()) {
            if (!signer.isSign(signData)) {
                throw new AOException("La firma introducida no se corresponde con el formato de firma especificado"); //$NON-NLS-1$
            }
        }
        else {
        	validSigner = getSpecificSigner(signData);
        	if (validSigner == null) {
        		validSigner = AOSignerFactory.getSigner(signData);
        		if (validSigner == null) {
        			throw new AOException("La firma introducida no se corresponde con ning\u00FAn formato soportado"); //$NON-NLS-1$
        		}
        	}
        }
        return validSigner;
    }

    /** Indica si unos datos son compatibles con alguno de los formatos de firma para
     * documentos espec&iacute;ficos (PDF, ODF u OOXML). Es obligatorio que el manejador
     * de firma de cada formato este disponible para su uso.
     * @param data Datos que se desean revisar.
     * @return Manejador de firma compatible con los datos indicados o {@code null} si
     * no se encontr&oacute; ninguno.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura de los datos. */
    private static AOSigner getSpecificSigner(final byte[] data) throws IOException {
    	final String[] specificFormats = new String[] {
    			AOSignConstants.SIGN_FORMAT_PDF,
    			AOSignConstants.SIGN_FORMAT_ODF,
    			AOSignConstants.SIGN_FORMAT_OOXML
    	};

    	AOSigner signer;
    	for (final String specificFormat : specificFormats) {
    		signer = AOSignerFactory.getSigner(specificFormat);
    		if (signer != null && signer.isValidDataFile(data)) {
    			return signer;
    		}
    	}
    	return null;
    }

    /** Agrega una entrada al log de la operacion de multifirma masiva global.
     * @param message Entrada del log. */
    private void addLog(final String message) {
        if (this.log == null) {
            this.log = new ArrayList<>();
        }
        this.log.add(message);
    }

    /** Recupera entrada del log correspondiente a la &uacute;ltima operacion de
     * multifirma realizada.
     * @return Entrada de log. */
    public String getCurrentLogEntry() {
        String lastEntry = ""; //$NON-NLS-1$
        if (this.log != null) {
            lastEntry = this.log.get(this.log.size() - 1);
        }
        return lastEntry;
    }

    /** Recupera el formato de firma establecido como por defecto para las operaciones
     * de firma masiva.
     * @return Formato de firma o {@code null} si no se ha establecido. */
    public String getDefaultSignatureFormat() {
    	if (this.massiveConfiguration != null) {
    		return this.massiveConfiguration.getDefaultFormat();
    	}
    	return null;
    }

    /** Recupera todo el log de la operaci&oacute;n masiva.
     * @return Log de la operaci&oacute;n masiva completa. */
    public String getAllLogEntries() {
        final StringBuilder buffer = new StringBuilder();
        if (this.log != null) {
            for (final String logEntry : this.log) {
                buffer.append(logEntry).append("\r\n"); //$NON-NLS-1$
            }
        }
        return buffer.toString().trim();
    }

    /** Genera la huella digital de los datos con el algoritmo indicado por
     * {@code DEFAULT_MESSAGE_DIGEST_ALGORITHM}.
     * @param data Datos de la que generar la huella.
     * @return Huella digital. */
    private static byte[] digest(final byte[] data, final String digestAlgorithm) {
    	if (md == null) {
    		try {
				md = MessageDigest.getInstance(digestAlgorithm);
			}
    		catch (final NoSuchAlgorithmException e) {
				LOGGER.severe("Se ha utilizado internamente un algoritmo de huella digital no soportado: " + e); //$NON-NLS-1$
				throw new IllegalArgumentException("Algoritmo no soportado", e); //$NON-NLS-1$
			}
    	}
    	return md.digest(data);
    }
}
