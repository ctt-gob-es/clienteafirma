/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.massive;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;

/** M&oacute;dulo para la ejecuci&oacute;n de multifirmas masivas. La firma
 * masiva puede aplicar sobre distintos tipos de elementos (Ficheros en disco o
 * hashes), seg&uacute;n el objetivo establecido el m&oacute;dulo se
 * comporta&aacute; de una forma u otra, devolviendo el resultado de forma
 * acorde con el objetivo establecido. */
public class DirectorySignatureHelper {

    private static final String MODE_KEY = "mode"; //$NON-NLS-1$
    private static final String FORMAT_KEY = "format"; //$NON-NLS-1$

    private static final String URI_STR = "uri"; //$NON-NLS-1$

    private static final String CADES_SIGNER = "es.gob.afirma.signers.cades.AOCAdESSigner"; //$NON-NLS-1$
    private static final String XADES_SIGNER = "es.gob.afirma.signers.xades.AOXAdESSigner"; //$NON-NLS-1$
    private static final String XMLDSIG_SIGNER = "es.gob.afirma.signers.xmldsig.AOXMLDSigSigner"; //$NON-NLS-1$
    private static final String PDF_SIGNER = "es.gob.afirma.signers.pades.AOPDFSigner"; //$NON-NLS-1$
    private static final String ODF_SIGNER = "es.gob.afirma.signers.odf.AOODFSSigner"; //$NON-NLS-1$
    private static final String OOXML_SIGNER = "es.gob.afirma.signers.ooxml.AOOOXMLSigner"; //$NON-NLS-1$

    private static final String REG_FIELD_SEPARATOR = " - "; //$NON-NLS-1$

    /** Objeto para la impresi&oacute;n de los de consola. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Fichero de log por defecto. */
    private static final String DEFAULT_LOG_FILE = "result.log"; //$NON-NLS-1$

    /** Algoritmo de huella digital por defecto que se utilizar&aacute;, por ejemplo, para
     * la generaci&oacute;n de las firmas expl&iacute;citas XAdES. */
	private static final String DEFAULT_MESSAGE_DIGEST_ALGORITHM = "SHA-512"; //$NON-NLS-1$

	/** Generador de huellas digitales utilizado internamente. */
    private static MessageDigest md = null;

    /** Algoritmo de firma. */
    private String algorithm = null;

    /** Formato de firma por defecto. */
    private String format = null;

    /** Modo de firma por defecto. */
    private String mode = null;

    /** Signer por defecto. */
    private AOSigner defaultSigner = null;

    /** Filtro de ficheros. */
    private FileFilter fileFilter = null;

    /** Directorio base de salida para las firmas generadas. */
    private String outputDir = null;

    /** Indica si se deben sobrescribir ficheros cuando se almacenen en disco. */
    private boolean overwriteFiles = false;

    /** Manejador para el log de la operaci&oacute;n de multifirma masiva. */
    private LogHandler logHandler;

    /** Contador de errores para la operaci&oacute;n de multifirma masiva. */
    private int errorCount;

    /** Contador de warnings para la operaci&oacute;n de multifirma masiva. */
    private int warnCount;

    /** Directorio con los ficheros a firmar. */
    private String inDir = null;

    /** Listado con el path de los ficheros firmados. */
    private List<String> signedFilenames = new ArrayList<>();

    /** Indica si se debe generar un fichero de log con los resultados de las
     * operaciones. */
    private boolean activeLog = true;

    /** Ruta del fichero de log. */
    private String logPath = null;


    /** Contruye un objeto para la firma masiva. Este objeto se
     * configurar&aacute; con un tipo de firma por defecto. Este tipo
     * (algoritmo, formato, modo) es el que se usar&aacute; para las operaciones
     * de firma masiva o las operaciones de cofirma sobre ficheros no firmados.
     * En caso de realizar una cofirma o contrafirma sobre un fichero de firma,
     * se utilizar&aacute; el formato del propio fichero y no este por defecto.
     * @param algorithm
     *        Algoritmo de firma.
     * @param format
     *        Formato de firma.
     * @param mode
     *        Modo de firma.
     * @throws AOUnsupportedSignFormatException
     *         Cuando se indica un formato no soportado. */
    public DirectorySignatureHelper(final String algorithm, final String format, final String mode) throws AOUnsupportedSignFormatException {
        if (algorithm == null || format == null || mode == null) {
            throw new IllegalArgumentException("No se ha indicado una configuracion de algoritmo de firma valida"); //$NON-NLS-1$
        }
        this.algorithm = algorithm;
        this.format = format;
        this.mode = mode;
        this.defaultSigner = AOSignerFactory.getSigner(this.format);
        if (this.defaultSigner == null) {
            throw new AOUnsupportedSignFormatException("El formato de firma '" + format + "' seleccionado para la firma masiva no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Realiza una firma masiva sobre los ficheros de un directorio. El tipo de
     * firma puede ser cualquiera de los declarados en {@link MassiveType}. La
     * descripcion de las acciones que se realizar&aacute;n seg&uacute;n el tipo
     * de firma solicitada y el fichero encontrado son:
     * <ul>
     * <li><b>Operaci&oacute;n de firma:</b>
     * <ul>
     * <li><b>Fichero original: </b>Se firma normalmente y se crea un fichero de firma siguiendo la nomenclatura abajo descrita.</li>
     * <li><b>Fichero de firma: </b>Se firma normalmente y se crea un fichero de firma siguiendo la nomenclatura abajo descrita.</li>
     * </ul>
     * </li>
     * <li><b>Operaci&oacute;n de cofirma:</b>
     * <ul>
     * <li><b>Fichero original: </b>En caso de no encontra ningun fichero de firma asociado, se firmar&aacute; normalmente con el formato de firma
     * establecido por defecto.</li>
     * <li><b>Fichero de firma: </b>Se buscar&aacute; el fichero de datos asociado. Seguidamente se cofirmar&aacute; este fichero y se generar&aacute;
     * un nuevo fichero de cofirma siguiendo la nomenclatura abajo descrita. Si no se encuentra ning&uacute;n fichero de datos asociado, se
     * firmar&aacute; el fichero normalmente con el formato por defecto.</li>
     * </ul>
     * </li>
     * <li><b>Operaci&oacute;n de contrafirma:</b>
     * <ul>
     * <li><b>Fichero original: </b>No se hace nada.</li>
     * <li><b>Fichero de firma: </b>Se realizar&aacute; normalmente la contrafirma de los nodos indicados en el tipo de firma.</li>
     * </ul>
     * </li>
     * </ul>
     * Se considera un fichero original todo fichero que no sea fichero de firma
     * del formato de firma con el que estemos realizando la operaci&oacute;n.
     * Cualquier formato de firma, cofirma o contrafirma del formato que estemos
     * usando se considerar&aacute; fichero de firma. <br>
     * <h2>Nomenclatura de ficheros</h2> Cuando ya exista un fichero con el
     * nombre de la firma que deseamos crear, al nuevo fichero de firma se le
     * agregar&aacute;, previamente a su extensi&oacute;n de firma, una cadena
     * con un &iacute;ndice en forma del n&uacute;mero natural m&aacute;s bajo
     * posible entre par&eacute;ntesis de tal forma que el fichero nunca
     * coincida en nombre con otro existente. La estructura del nombre un
     * fichero de firma ser&aacute;: "nombreFichero.ext.extFir". En caso de
     * coincidir con otro nombre de fichero se agregar&aacute;a el
     * &iacute;ndice, quedando como "nombreFichero.ext(ind).extFir"<br>
     * En el caso de la cofirma y contrafirma, se agregar&aacute; al nombre de
     * fichero despues de su extensi&oacute;n propia y antes de la
     * extensi&oacute;n de firma (o el &iacute;ndice en caso de haberlo) las
     * pat&iacute;culas "cosign" y "countersign", segun corresponda, separadas
     * por puntos. Esto quedarar&iacute;a en la forma:
     * <ul>
     * <li>"nombreFichero.ext.cosign(ind).extFir" para las cofirmas.</li>
     * <li>"nombreFichero.ext.countersign(ind).extFir" para las contrafirmas.</li>
     * </ul>
     * En el directorio de salida y como resultado de la operaci&oacute;n, se
     * almacenar&aacute; un fichero de <code>log</code> con el nombre
     * ("result.log") en donde se almacenar&aacute; el resultado de la
     * operaci&oacute;n. Este fichero de <code>log</code> sustituir&aacute;a a
     * cualquiera creado anteriormente y nunca se firmar&aacute; en caso de ya
     * existir en el directorio de salida.
     * @param type
     *        Tipo de firma (firma, cofirma o contrafirma). Por defecto,
     *        sera firma.
     * @param startDir
     *        Directorio de entrada de ficheros. Por defecto, el directorio
     *        actual.
     * @param recurse
     *        Indica si se deben firmar los ficheros de los subdirectorios
     *        del seleccionado.
     * @param outDir
     *        Directorio de salida. Por defecto, el actual.
     * @param createOutDir
     *        Indica si debe crearse el directorio de salida en caso de no
     *        existir.
     * @param originalFormat
     *        Indica si se debe respetar el formato de firma original.
     * @param keyEntry
     *        Entrada con la clave privada para la firma.
     * @param config
     *        Configuraci&oacute;n de firma.
     * @return Devuelve <code>true</code> si todas las firmas se realizaron
     *         correctamente, <code>false</code> en caso contrario.
     * @throws AOException
     *         Error grave durante el proceso de firma masiva.
     * @throws IOException Cuando ocurre un error durante la lectura o escritura de los datos.
     */
    public boolean massiveSign(final MassiveType type,
                               final String startDir,
                               final boolean recurse,
                               final String outDir,
                               final boolean createOutDir,
                               final boolean originalFormat,
                               final PrivateKeyEntry keyEntry,
                               final Properties config) throws AOException, IOException {

        if (config == null || !config.containsKey(FORMAT_KEY) || !config.containsKey(MODE_KEY)) {
            throw new IllegalArgumentException("No se ha establecido el formato y modo de firma"); //$NON-NLS-1$
        }

        if (startDir == null || startDir.trim().length() < 1) {
            LOGGER.warning("No se ha indicado un directorio de inicio, se usara el actual"); //$NON-NLS-1$
        }

        final File id = new File(startDir != null && startDir.trim().length() > 0 ? startDir.trim() : "."); //$NON-NLS-1$
        this.inDir = id.getAbsolutePath();
        if (!id.exists() || !id.isDirectory()) {
            throw new AOException("El directorio de entrada no existe"); //$NON-NLS-1$
        }
        if (!id.canRead()) {
            throw new AOException("No se tienen permisos de lectura para el directorio de entrada"); //$NON-NLS-1$
        }
        final List<String> filenames = new ArrayList<>();
        final List<File> files = new ArrayList<>();
        for (final File file : id.listFiles()) {
            files.add(file);
        }
        for (int i = 0; i < files.size(); i++) {
            if (files.get(i).isFile()) {
                if (this.fileFilter == null) {
                    filenames.add(files.get(i).getPath());
                }
                else if (this.fileFilter.accept(files.get(i))) {
                    filenames.add(files.get(i).getAbsolutePath());
                }
            }
            else if (recurse) {
            	if (files.get(i).canRead()) {
            		for (final File file : files.get(i).listFiles()) {
                        files.add(file);
                    }
            	} else {
            		LOGGER.warning("Por falta de permisos no se procesaran los ficheros del subdirectorio: " + LoggerUtil.getCleanUserHomePath(files.get(i).getAbsolutePath())); //$NON-NLS-1$
            	}
            }
        }

        // Solicitamos la firma masiva de los ficheros concretos que se
        // enconrtraron en el directorio
        return this.massiveSign(type, filenames.toArray(new String[filenames.size()]), outDir, createOutDir, originalFormat, keyEntry, config);
    }

    /** Realiza una firma masiva sobre los ficheros de un directorio. El tipo de
     * firma puede ser cualquiera de los declarados en {@link MassiveType}. La
     * descripcion de las acciones que se realizar&aacute;n seg&uacute;n el tipo
     * de firma solicitada y el fichero encontrado son:
     * <ul>
     * <li><b>Operaci&oacute;n de firma:</b>
     * <ul>
     * <li><b>Fichero original: </b>Se firma normalmente y se crea un fichero de firma siguiendo la nomenclatura abajo descrita.</li>
     * <li><b>Fichero de firma: </b>Se firma normalmente y se crea un fichero de firma siguiendo la nomenclatura abajo descrita.</li>
     * </ul>
     * </li>
     * <li><b>Operaci&oacute;n de cofirma:</b>
     * <ul>
     * <li><b>Fichero original: </b>En caso de no encontra ningun fichero de firma asociado, se firmar&aacute; normalmente con el formato de firma
     * establecido por defecto.</li>
     * <li><b>Fichero de firma: </b>Se buscar&aacute; el fichero de datos asociado. Seguidamente se cofirmar&aacute; este fichero y se generar&aacute;
     * un nuevo fichero de cofirma siguiendo la nomenclatura abajo descrita. Si no se encuentra ning&uacute;n fichero de datos asociado, se
     * firmar&aacute; el fichero normalmente con el formato por defecto.</li>
     * </ul>
     * </li>
     * <li><b>Operaci&oacute;n de contrafirma:</b>
     * <ul>
     * <li><b>Fichero original: </b>No se hace nada.</li>
     * <li><b>Fichero de firma: </b>Se realizar&aacute; normalmente la contrafirma de los nodos indicados en el tipo de firma.</li>
     * </ul>
     * </li>
     * </ul>
     * Se considera un fichero original todo fichero que no sea fichero de firma
     * del formato de firma con el que estemos realizando la operaci&oacute;n.
     * Cualquier formato de firma, cofirma o contrafirma del formato que estemos
     * usando se considerar&aacute; fichero de firma. <br>
     * <h2>Nomenclatura de ficheros</h2> Cuando ya exista un fichero con el
     * nombre de la firma que deseamos crear, al nuevo fichero de firma se le
     * agregar&aacute;, previamente a su extensi&oacute;n de firma, una cadena
     * con un &iacute;ndice en forma del n&uacute;mero natural m&aacute;s bajo
     * posible entre par&eacute;ntesis de tal forma que el fichero nunca
     * coincida en nombre con otro existente. La estructura del nombre un
     * fichero de firma ser&aacute;: "nombreFichero.ext.extFir". En caso de
     * coincidir con otro nombre de fichero se agregar&aacute;a el
     * &iacute;ndice, quedando como "nombreFichero.ext(ind).extFir"<br>
     * En el caso de la cofirma y contrafirma, se agregar&aacute; al nombre de
     * fichero despues de su extensi&oacute;n propia y antes de la
     * extensi&oacute;n de firma (o el &iacute;ndice en caso de haberlo) las
     * pat&iacute;culas "cosign" y "countersign", segun corresponda, separadas
     * por puntos. Esto quedarar&iacute;a en la forma:
     * <ul>
     * <li>"nombreFichero.ext.cosign(ind).extFir" para las cofirmas.</li>
     * <li>"nombreFichero.ext.countersign(ind).extFir" para las contrafirmas.</li>
     * </ul>
     * En el directorio de salida y como resultado de la operaci&oacute;n, se
     * almacenar&aacute; un fichero de <code>log</code> con el nombre
     * ("result.log") en donde se almacenar&aacute; el resultado de la
     * operaci&oacute;n. Este fichero de <code>log</code> sustituir&aacute;a a
     * cualquiera creado anteriormente y nunca se firmar&aacute; en caso de ya
     * existir en el directorio de salida.<br>
     * <br>
     * @param type
     *        Tipo de firma (firma, cofirma o contrafirma). Por defecto,
     *        sera firma.
     * @param filenames
     *        Ficheros que se desean firmar.
     * @param outDir
     *        Directorio de salida. Por defecto, el actual.
     * @param createOutDir
     *        Indica si debe crearse el directorio de salida en caso de no
     *        existir.
     * @param originalFormat
     *        Indica si se debe respetar el formato de firma original.
     * @param keyEntry
     *        Entrada con la clave privada para la firma.
     * @param config
     *        Configuraci&oacute;n de firma.
     * @return Devuelve <code>true</code> si todas las firmas se realizaron
     *         correctamente, <code>false</code> en caso contrario.
     * @throws AOException
     *         Error grave durante el proceso de firma masiva.
     * @throws IOException Cuando ocurre un error durante la lectura o escritura de los datos.
     */
    public boolean massiveSign(final MassiveType type,
                               final String[] filenames,
                               final String outDir,
                               final boolean createOutDir,
                               final boolean originalFormat,
                               final PrivateKeyEntry keyEntry,
                               final Properties config) throws AOException, IOException {

        if (config == null || !config.containsKey(FORMAT_KEY) || !config.containsKey(MODE_KEY)) {
            throw new IllegalArgumentException("No se ha establecido el formato y modo de firma"); //$NON-NLS-1$
        }

        final Properties signConfig = (Properties) config.clone();
        signConfig.setProperty("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        // Establecemos la configuracion de firma por defecto si no se indica
        if (!signConfig.containsKey(MODE_KEY) && this.mode != null) {
            signConfig.setProperty(MODE_KEY, this.mode);
        }
        if (!signConfig.containsKey(FORMAT_KEY)) {
            signConfig.setProperty(FORMAT_KEY, this.format);
        }

        // Indica si todas las operaciones se finalizaron correctamente
        boolean allOK = true;

        // Inicializamos el vector de ficheros firmados
        this.signedFilenames = new ArrayList<>();

        if (filenames == null || filenames.length == 0) {
            LOGGER.warning("No se han proporcionado ficheros para firmar"); //$NON-NLS-1$
            return true;
        }

        if (outDir != null) {
            this.outputDir = outDir;
        }
        else {
            LOGGER.warning("No se ha especificado un directorio de salida, se usara el actual"); //$NON-NLS-1$
            this.outputDir = "."; //$NON-NLS-1$
        }

        final File od = new File(this.outputDir);
        if (!od.exists() && createOutDir) {
            od.mkdirs();
        }
        if (!od.exists() || !od.isDirectory()) {
            throw new IOException("El directorio de salida no existe o existe un fichero con el mismo nombre"); //$NON-NLS-1$
        }
        if (!od.canWrite()) {
            throw new IOException("No se tienen permisos de escritura en el directorio de salida"); //$NON-NLS-1$
        }

        // Inicializamos el log de operacion
        try (
    		final OutputStream logOs = new FileOutputStream(this.logPath != null ?
				this.logPath :
					outDir + File.separator + DEFAULT_LOG_FILE);
		) {
	        if (this.activeLog && this.logHandler == null) {
	            this.logHandler = DirectorySignatureHelper.initDefaultLogHandler(
	        		logOs
	    		);
	        }

	        // Realizamos la operacion masiva correspondiente
	        final File[] files = getFiles(filenames);
	        if (MassiveType.SIGN.equals(type) || type == null) { // Asumimos que null es el por defecto: MassiveType.SIGN
	            allOK = massiveSignOperation(files, od, keyEntry, signConfig);
	        }
	        else if (MassiveType.COSIGN.equals(type)) {
	            allOK = massiveCosignOperation(files, od, originalFormat, keyEntry, signConfig);
	        }
	        else if (MassiveType.COUNTERSIGN_ALL.equals(type) || MassiveType.COUNTERSIGN_LEAFS.equals(type)) {
	            allOK = massiveCounterSignOperation(type, files, od, originalFormat, keyEntry, signConfig);
	        }
	        else {
	            LOGGER.severe("Operacion masiva no reconocida");  //$NON-NLS-1$
	        }

	        // Cerramos el log de operacion
	        closeLogRegistry();
        }

        return allOK;
    }

    /** Firma masiva de hashes.
     * @param hashes
     *        Hashes que se desean firmar.
     * @param keyEntry
     *        Referencia a la clave de firma.
     * @param configuredSigner
     *        Configuraci&oacute;n de la operaci&oacute;n de firma.
     * @param config
     *        Configuraci&oacute;n preestablecida de firma.
     * @return Firmas generadas.
     * @throws AOException
     *         Error durante la operacion de firma de hashes. */
    public String[] hashesMassiveSign(final String[] hashes,
                                      final PrivateKeyEntry keyEntry,
                                      final AOSigner configuredSigner,
                                      final Properties config) throws AOException {

        if (hashes == null || keyEntry == null) {
            throw new IllegalArgumentException("Las huellas digitales a firmar y la clave privada no pueden ser nulas"); //$NON-NLS-1$
        }

        if (config == null || !config.containsKey(FORMAT_KEY) || !config.containsKey(MODE_KEY)) {
            throw new IllegalArgumentException("No se ha establecido el formato y modo de firma"); //$NON-NLS-1$
        }

        // Comprobamos que no se nos haya introducido un signer de distinto tipo
        if (configuredSigner != null && !configuredSigner.getClass().equals(this.defaultSigner.getClass())) {
            throw new ClassCastException("El signer configurado para la multifirma debe ser compatible con el signer del formato indicado en el constructor"); //$NON-NLS-1$
        }

        final Properties signConfig = (Properties) config.clone();
        signConfig.setProperty("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        final AOSigner signer = configuredSigner != null ? configuredSigner : this.defaultSigner;

        // Configuramos y ejecutamos la operacion
        if (!signConfig.containsKey(MODE_KEY) && this.mode != null) {
            signConfig.setProperty(MODE_KEY, this.mode);
        }
        if (!signConfig.containsKey(FORMAT_KEY)) {
            signConfig.setProperty(FORMAT_KEY, this.format);
        }
        signConfig.setProperty("precalculatedHashAlgorithm", AOSignConstants.getDigestAlgorithmName(this.algorithm)); //$NON-NLS-1$

        // Introduccion MIMEType "hash/algo", solo para XAdES y XMLDSig
        if (signer.getClass().getName().equals(XADES_SIGNER) || signer.getClass().getName().equals(XMLDSIG_SIGNER)) {
        	final String mimeType = "hash/" + AOSignConstants.getDigestAlgorithmName(this.algorithm).toLowerCase(); //$NON-NLS-1$
        	signConfig.setProperty("mimeType", mimeType); //$NON-NLS-1$
        }

        final byte[][] signs = new byte[hashes.length][];
        for (int i = 0; i < hashes.length; i++) {
        	try {
        		signs[i] = signer.sign(
    				Base64.decode(hashes[i]),
    				this.algorithm,
    				keyEntry.getPrivateKey(),
    				keyEntry.getCertificateChain(),
    				signConfig
				);
        	}
        	catch (final IOException e) {
        		throw new AOException("El hash '" + hashes[i] + "' no es un Base64 valido", e);  //$NON-NLS-1$//$NON-NLS-2$
        	}
        }

        final String[] signsB64 = new String[signs.length];
        for (int i = 0; i < signs.length; i++) {
            signsB64[i] = Base64.encode(signs[i]);
        }
        return signsB64;
    }

    /** Obtienene un array de ficheros que existen, no son directorios y tienen
     * permiso de lectura a partir de los paths de estos ficheros.
     * @param filenames
     *        Nombres de los ficheros.
     * @return Listado de ficheros v&aacute;lidos. */
    private File[] getFiles(final String[] filenames) {
        File tempFile = null;
        final List<File> vFiles = new ArrayList<>(filenames.length);
        for (final String filename : filenames) {
            tempFile = new File(filename);
            if (!tempFile.exists()) {
                LOGGER.severe("El fichero no existe: " + LoggerUtil.getCleanUserHomePath(filename));  //$NON-NLS-1$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.0"), LoggerUtil.getCleanUserHomePath(filename), null); //$NON-NLS-1$
                continue;
            }
            if (!tempFile.isFile()) {
                LOGGER.severe("El archivo '" + LoggerUtil.getCleanUserHomePath(filename) + "' es un directorio y no puede firmarse"); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.1"), LoggerUtil.getCleanUserHomePath(filename), null); //$NON-NLS-1$
                continue;
            }
            if (!tempFile.canRead()) {
                LOGGER.severe("No se puede leer el fichero '" + LoggerUtil.getCleanUserHomePath(filename) + "'"); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.2"), LoggerUtil.getCleanUserHomePath(filename), null); //$NON-NLS-1$
                continue;
            }
            vFiles.add(tempFile);
        }
        return vFiles.toArray(new File[vFiles.size()]);
    }

    /** Realiza la operaci&oacute;n de firma masiva.
     * @param files Ficheros que se desean firmar ya comprobados (existencia, no
     *              directorio, permisos,...).
     * @param outDir Directorio de salida (creado y con permisos).
     * @param keyEntry Clave de firma.
     * @param signConfig Configuraci&oacute; para la operaci&oacute;n de firma
     * @return Devuelve <code>true</code> si toda la operaci&oacute;n
     *         finaliz&oacute; correctamente, <code>false</code> en caso contrario.
     * @throws IOException Cuando hay errores de entrada / salida */
    private boolean massiveSignOperation(final File[] files,
                                         final File outDir,
                                         final PrivateKeyEntry keyEntry,
                                         final Properties signConfig) throws IOException {

        boolean allOK = true;
        byte[] dataToSign = null;
        final AOSigner signer = this.defaultSigner;
        for (final File file : files) {

            // Comprobamos que el fichero actual se pueda firmar con la
            // configuracion de firma actual
            try {
                if (!DirectorySignatureHelper.isValidDataFile(signer, file)) {
                	LOGGER.warning(
            			"El fichero no puede ser firmado con la configuracion de firma actual: " + LoggerUtil.getCleanUserHomePath(file.getPath()) //$NON-NLS-1$
        			);
                    addLogRegistry(Level.WARNING, MassiveSignMessages.getString("DirectorySignatureHelper.4"), file.getPath(), null); //$NON-NLS-1$
                    allOK = false;
                    continue;
                }
            }
            catch (final Exception e) {
                LOGGER.warning("No se pudo leer fichero '" + LoggerUtil.getCleanUserHomePath(file.getPath()) + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.5"), file.getPath(), null); //$NON-NLS-1$
                allOK = false;
                continue;
            }

            // Configuramos y ejecutamos la operacion
            signConfig.setProperty(URI_STR, file.toURI().toASCIIString());

            try (
        		final InputStream fis = new FileInputStream(file);
    		) {
                dataToSign = AOUtil.getDataFromInputStream(fis);
            }
            catch (final Exception e) {
                LOGGER.warning("No se pudo leer fichero '" + LoggerUtil.getCleanUserHomePath(file.getPath()) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.5"), file.getPath(), null); //$NON-NLS-1$
                dataToSign = null;
                allOK = false;
                continue;
            }

            // Deteccion del MIMEType y Oid de los datos, solo para CAdES, XAdES y XMLDSig
            final String signerClassName = signer.getClass().getName();
            if (CADES_SIGNER.equals(signerClassName) ||
            		XADES_SIGNER.equals(signerClassName) ||
            		XMLDSIG_SIGNER.equals(signerClassName)) {

                // Forzamos que las firmas XAdES Explicitas se realicen sobre el hash de los datos
            	// y que el mimetype sea el definido para hashes
            	String mimeType;
                if ((XADES_SIGNER.equals(signerClassName) || XMLDSIG_SIGNER.equals(signerClassName))
                		&& AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(this.mode)) {
                	dataToSign = digest(dataToSign);
                	mimeType = ("hash/" + DEFAULT_MESSAGE_DIGEST_ALGORITHM).toLowerCase(); //$NON-NLS-1$
                }
                else {
                	final MimeHelper mimeHelper = new MimeHelper(dataToSign);
                	mimeType = mimeHelper.getMimeType();
                }

                if (mimeType != null) {
            		signConfig.setProperty("mimeType", mimeType); //$NON-NLS-1$
            		final String dataOid = MimeHelper.transformMimeTypeToOid(mimeType);
            		if (dataOid != null) {
            			signConfig.setProperty("contentTypeOid", dataOid); //$NON-NLS-1$
            		}
            	}
            }

            byte[] signData = null;
            try {
                signData = signer.sign(
            		dataToSign,
            		this.algorithm,
            		keyEntry.getPrivateKey(),
            		keyEntry.getCertificateChain(),
            		signConfig
        		);
            }
            catch(final UnsupportedOperationException e) {
                LOGGER.severe("No ha sido posible firmar el fichero '" + LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()) + "': " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.7") + REG_FIELD_SEPARATOR + e.getMessage(), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                allOK = false;
                continue;
            }
            catch (final Exception e) {
            	// Esta excepcion se comprueba por nombre para no acoplar los proyectos
            	if ("es.gob.afirma.signers.xades.EFacturaAlreadySignedException".equals(e.getClass().getName())) { //$NON-NLS-1$
                	LOGGER.warning("La factura ya estaba firmada y no admite firmas adicionales '" + LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()) + "': " + e);   //$NON-NLS-1$//$NON-NLS-2$
                	addLogRegistry(Level.WARNING, MassiveSignMessages.getString("DirectorySignatureHelper.27"), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                    allOK = false;
                    continue;
            	}
                LOGGER.severe("No ha sido posible firmar el fichero '" + LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()) + "': " + e);   //$NON-NLS-1$//$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.7"), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                allOK = false;
                continue;
            }
			catch (final OutOfMemoryError e) {
				LOGGER.severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
				addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.8"), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                allOK = false;
                continue;
			}

        	// Para los formatos PDF, ODF y OOXML, en los que la firma de un documento firmado es
        	// una cofirma, se agrega la particula "cosign" en lugar de "signed" si los datos estaban
            // firmados
            String textAux = ".signed"; //$NON-NLS-1$
        	if ((PDF_SIGNER.equals(signerClassName) ||
        			ODF_SIGNER.equals(signerClassName) ||
        			OOXML_SIGNER.equals(signerClassName)) &&
        			signer.isSign(dataToSign)) {
        		textAux = ".cosign"; //$NON-NLS-1$
        	}

            // Guardamos la firma en disco
			final String signFilePath = saveSignToDirectory(file.getPath(), signData, outDir, signer, textAux);
            if (signFilePath == null) {
                allOK = false;
                continue;
            }
            LOGGER.info("El fichero se ha firmado correctamente: " + LoggerUtil.getCleanUserHomePath(file.getPath()));  //$NON-NLS-1$
            addLogRegistry(Level.INFO, MassiveSignMessages.getString("DirectorySignatureHelper.3"), file.getPath(), signFilePath); //$NON-NLS-1$
        }
        return allOK;
    }

    /** Realiza la operaci&oacute;n de cofirma masiva. La cofirma se encarga de
     * firmar ficheros de datos y cofirmar los ficheros de firma que encuentre,
     * teniendo la limitaci&oacute;n de que los ficheros de firma deben contener
     * los datos incrustados.
     * @param files Ficheros de firma y binarios ya comprobados (existencia, no
     *              directorio, permisos,...).
     * @param outDir Directorio de salida (creado y con permisos).
     * @param originalFormat Respectar formato de firma original
     * @param keyEntry Clave de firma.
     * @param signConfig Par&aacute;metros de configuraci&oacute;n de la firma
     * @return Devuelve <code>true</code> si toda la operaci&oacute;n
     *         finaliz&oacute; correctamente, <code>false</code> en caso
     *         contrario.
     * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de
     * 		   los datos.
     */
    private boolean massiveCosignOperation(final File[] files,
                                           final File outDir,
                                           final boolean originalFormat,
                                           final PrivateKeyEntry keyEntry,
                                           final Properties signConfig) throws IOException {

        boolean allOK = true;

        // Leemos cada uno de los ficheros del directorio y procedemos segun la
        // siguiente especificacion:
        // - Comprobamos que el fichero sea una firma con el formato indicado
        // - SI: Cofirmamos
        // - NO: Comprobamos si debemos respetar el formato de firma original
        //		 - SI: Comprobamos si el fichero se corresponde con un documento que requiera un formato de firma especifico (PDF, ODF u OOXML).
        //			   - SI: Comprobamos si es una firma de ese tipo
        //					 - SI: Cofirmamos en el formato especifico
        //					 - NO: Firmamos en el formato especifico
        //			   - NO: Comprobamos si el fichero se corresponde con un tipo de firma cualquiera
        // 					 - SI: Cofirmamos en ese formato
        // 					 - NO: Es un fichero de datos (o firma no soportada), asi que se firmara en el formato indicado
        // 		 - NO: Lo consideramos un fichero de datos, as&iacute; que se firmara en el formato indicado
        String textAux;
        byte[] signedData;
        byte[] originalData;
        AOSigner signer;
        for (final File file : files) {
            try (
        		final InputStream is = new FileInputStream(file);
    		) {
                originalData = AOUtil.getDataFromInputStream(is);
            }
            catch (final Exception e) {
                allOK = false;
                continue;
            }

            signConfig.setProperty(URI_STR, file.toURI().toASCIIString());

            // Indica si se debe realizar una cofirma o una firma de los datos
            boolean cosignOperation;

            if (this.defaultSigner.isSign(originalData)) {
                cosignOperation = true;
                signer = this.defaultSigner;
            }
            else if (originalFormat) {
            	signer = getSpecificSigner(originalData);
            	if (signer != null) {
            		cosignOperation = signer.isSign(originalData);
            	}
            	else {
            		signer = AOSignerFactory.getSigner(originalData);
            		if (signer != null) {
            			cosignOperation = true;
            		}
            		else {
            			cosignOperation = false;
            			signer = this.defaultSigner;
            		}
            	}
            }
            else {
    			cosignOperation = false;
                signer = this.defaultSigner;
            }

            // Operamos segun los condicionales definidos
            if (cosignOperation) {
            	textAux = "cosign"; //$NON-NLS-1$
            	signedData = cosign(signer, originalData, this.algorithm, keyEntry, signConfig);
            }
            else {
            	textAux = "sign"; //$NON-NLS-1$
            	signedData = sign(signer, originalData, this.algorithm, keyEntry, signConfig);
            }

            // Comprobamos si la operacion ha finalizado correctamente
            if (signedData == null) {
                allOK = false;
                continue;
            }

            // Guardamos los datos de la firma
			final String signFilePath = saveSignToDirectory(file.getPath(), signedData, outDir, signer, "." + textAux); //$NON-NLS-1$
            if (signFilePath == null) {
                allOK = false;
                continue;
            }
            LOGGER.info("Se ha operado (" + textAux + ") correctamente sobre el fichero '" + LoggerUtil.getCleanUserHomePath(file.getPath()) + "'");   //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
            addLogRegistry(Level.INFO, MassiveSignMessages.getString("DirectorySignatureHelper.10") + " (" + textAux + ")", file.getPath(), signFilePath); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        return allOK;
    }

    /** Cofirma datos con el manejador y la configuracion indicados.
     * @param signer
     *        Manejador de firma.
     * @param signData
     *        Datos firmados.
     * @param algo
     *        Algoritmo de firma.
     * @param keyEntry
     *        Clave para firmar.
     * @param signConfig
     *        Configurac&oacute;n de firma.
     * @return Cofirma. */
    private byte[] cosign(final AOSigner signer,
                          final byte[] signData,
                          final String algo,
                          final PrivateKeyEntry keyEntry,
                          final Properties signConfig) {

        // Configuramos y ejecutamos la operacion
        byte[] signedData;
        try {
            signedData = signer.cosign(
        		signData,
        		algo,
        		keyEntry.getPrivateKey(),
        		keyEntry.getCertificateChain(),
        		signConfig
    		);
        }
        catch (final Exception e) {
            LOGGER.severe("No ha sido posible cofirmar el fichero '" + LoggerUtil.getCleanUserHomePath(signConfig.getProperty(URI_STR)) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.11") + ": " + e.getMessage(), LoggerUtil.getCleanUserHomePath(signConfig.getProperty(URI_STR)), null); //$NON-NLS-1$ //$NON-NLS-2$
            signedData = null;
        }
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante la cofirma: " + e); //$NON-NLS-1$
			addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.8"), LoggerUtil.getCleanUserHomePath(signConfig.getProperty(URI_STR)), null); //$NON-NLS-1$
            signedData = null;
		}
        return signedData;
    }

    /** Firma datos con el manejador y la configuracion indicados.
     * @param signer
     *        Manejador de firma.
     * @param data
     *        Datos para firmar.
     * @param algo
     *        Algoritmo de firma.
     * @param keyEntry
     *        Clave para firmar.
     * @param signConfig
     *        Configurac&oacute;n de firma.
     * @return Firma electr&oacute;nica.
     * @throws IOException Cuando ocurre un error durante el an&aacute;lisis del tipo de dato. */
    private byte[] sign(final AOSigner signer,
                        final byte[] data,
                        final String algo,
                        final PrivateKeyEntry keyEntry,
                        final Properties signConfig) throws IOException {

    	byte[] dataToSign = data;

    	// Deteccion del MIMEType y Oid de los datos, solo para CAdES, XAdES y XMLDSig
        final String signerClassName = signer.getClass().getName();
        if (CADES_SIGNER.equals(signerClassName) ||
        		XADES_SIGNER.equals(signerClassName) ||
        		XMLDSIG_SIGNER.equals(signerClassName)) {

        	String mimeType;
        	if ((XADES_SIGNER.equals(signerClassName) || XMLDSIG_SIGNER.equals(signerClassName)) &&
        			AOSignConstants.SIGN_MODE_EXPLICIT.equalsIgnoreCase(signConfig.getProperty("mode"))) { //$NON-NLS-1$
        		dataToSign = digest(dataToSign);
        		mimeType = ("hash/" + DEFAULT_MESSAGE_DIGEST_ALGORITHM).toLowerCase(); //$NON-NLS-1$
        	}
        	else {
        		final MimeHelper mimeHelper = new MimeHelper(dataToSign);
        		mimeType = mimeHelper.getMimeType();
        	}
            if (mimeType != null) {
            	signConfig.setProperty("mimeType", mimeType); //$NON-NLS-1$
            	final String dataOid = MimeHelper.transformMimeTypeToOid(mimeType);
            	if (dataOid != null) {
            		signConfig.setProperty("contentTypeOid", dataOid); //$NON-NLS-1$
            	}
            }
        }

        // Configuramos y ejecutamos la operacion
        try {
        	return signer.sign(
        			dataToSign,
        			algo,
        			keyEntry.getPrivateKey(),
        			keyEntry.getCertificateChain(),
        			signConfig
        			);
        }
        catch (final Exception e) {
            LOGGER.severe("No ha sido posible firmar el fichero de datos '" + LoggerUtil.getCleanUserHomePath(signConfig.getProperty(URI_STR)) + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
            addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.13") + ": " + e.getMessage(), LoggerUtil.getCleanUserHomePath(signConfig.getProperty(URI_STR)), null); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
        }

    }

    /** Realiza la operaci&oacute;n de contrafirma masiva.
     * @param type
     *        Tipo de firma masiva (contrafirma de &aacute;rbol o de nodos
     *        hoja).
     * @param files
     *        Ficheros a contrafirmar.
     * @param outDir
     *        Directorio de salida.
     * @param originalFormat
     *        Respetar formato de firma original.
     * @param keyEntry
     *        Clave de firma.
     * @param signConfig
     *        Configuraci&oacute;n de firma.
     * @return Indica si la operaci&oacute;n finaliz&oacute; correctamente.
     * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de
     * 		   los datos.
     */
    private boolean massiveCounterSignOperation(final MassiveType type,
                                                final File[] files,
                                                final File outDir,
                                                final boolean originalFormat,
                                                final PrivateKeyEntry keyEntry,
                                                final Properties signConfig) throws IOException {
        boolean allOK = true;
        final CounterSignTarget target = type == MassiveType.COUNTERSIGN_ALL ? CounterSignTarget.TREE : CounterSignTarget.LEAFS;
        AOSigner signer = this.defaultSigner;
        for (final File file : files) {
            if (originalFormat) {
                try {
                    signer = DirectorySignatureHelper.getAppropiatedSigner(file);
                }
                catch (final Exception e) {
                	String filePath = ""; //$NON-NLS-1$
                	if (file != null) {
                		filePath = file.getPath();
                	}
                    addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.15") + ": " + e.getMessage(), filePath, null); //$NON-NLS-1$ //$NON-NLS-2$
                    allOK = false;
                    continue;
                }
            }

            // Solo podemos contrafirmar un fichero de firma en el mismo formato
            // en el que este
            byte[] signData;
            if (DirectorySignatureHelper.isSign(signer, file)) {
                try (
            		final InputStream is = new FileInputStream(file);
        		) {
                    // Si se nos pide que respetemos el formato original el
                    // signer puede cambiar de una iteracion
                    // a otra. Si no es necesario respetar el formato, el signer
                    // siempre sera el por defecto establecido.
                	final byte[] data = AOUtil.getDataFromInputStream(is);
                	is.close();
                    signData = signer.countersign(
                		data,
                		this.algorithm,
                		target,
                		null,
                		keyEntry.getPrivateKey(),
                		keyEntry.getCertificateChain(),
                		signConfig
            		);
                }
                catch (final FileNotFoundException e) {
                    LOGGER.severe("No se ha encontrado el fichero '" + LoggerUtil.getCleanUserHomePath(file.getPath()) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.24"), file.getPath(), null); //$NON-NLS-1$
                    allOK = false;
                    continue;
                }
                catch (final Exception e) {
                    LOGGER.severe("No ha sido posible contrafirmar el fichero '" + LoggerUtil.getCleanUserHomePath(file.getPath()) + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
                    addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.15"), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                    allOK = false;
                    continue;
                }
    			catch (final OutOfMemoryError e) {
    				LOGGER.severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
    				addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.8"), LoggerUtil.getCleanUserHomePath(file.getPath()), null); //$NON-NLS-1$
                    allOK = false;
                    continue;
    			}
            }
            else {
                LOGGER.severe(
            		"El fichero '" + file //$NON-NLS-1$
                         + "' no es un fichero de firma en formato '" //$NON-NLS-1$
                             + signConfig.getProperty(FORMAT_KEY)
                                 + "'" //$NON-NLS-1$
        		);
                addLogRegistry(Level.SEVERE,
                    MassiveSignMessages.getString("DirectorySignatureHelper.16"), LoggerUtil.getCleanUserHomePath(file.getPath()), null //$NON-NLS-1$
                );
                allOK = false;
                continue;
            }

            // Guardamos la firma en disco
			final String signFilePath = saveSignToDirectory(file.getPath(), signData, outDir, signer, ".countersign"); //$NON-NLS-1$
            if (signFilePath == null) {
                allOK = false;
                continue;
            }
            LOGGER.info("El fichero se ha contrafirmado correctamente: " + LoggerUtil.getCleanUserHomePath(file.getPath())); //$NON-NLS-1$
            addLogRegistry(Level.INFO, MassiveSignMessages.getString("DirectorySignatureHelper.20"), file.getPath(), signFilePath); //$NON-NLS-1$
        }
        return allOK;
    }

    /** Salva un fichero de firma en un directorio. El fichero ser&aacute;
     * salvado con el mismo nombre que el fichero original m&aacute;s la
     * extensi&oacute;n que le corresponda segun el tipo de firma. <br>
     * Adicionalmente, se le agregar&iacute;a una part&iacute;cula intermedia
     * entre el nombre de fichero y la extensi&oacute;n de firma, normalmente
     * ".signed", ".cosign" y ".countersign" en los casos de firma, cofirma y
     * contrafirma, respectivamente. <br>
     * Si, una vez determinado el nombre de fichero, se encuentra que ya existe
     * un fichero con este nombre, se agregar&aacute; una cifra entre
     * par&eacute;ntesis (partiendo desde cero y en orden creciente) que permita
     * almacenar el fichero sin sobreescribir ning&uacute;n otro. <br>
     * La estructura del nombre de fichero es:
     * <p>
     * <code>nombre_fichero+particula+cifra+extension_firma</code>
     * </p>
     * Por ejemplo:
     * <p>
     * <code>nombre_fichero.txt.cosign(1).csig</code>
     * </p>
     * @param filename Nombre del fichero original.
     * @param signData Contenido del fichero de firma.
     * @param outDirectory Directorio de salida.
     * @param signer Objeto con el que se realiza la firma.
     * @param inText Part&iacute;cula de texto intermedia (".signed", ".cosign" y
     *               ".countersign" habitualmente).
     * @return Devuelve la ruta del fichero de salida con la firma. */
    private String saveSignToDirectory(final String filename, final byte[] signData, final File outDirectory, final AOSigner signer, final String inText) {

        final String relativePath = getRelativePath(filename);
        final String signFilename = new File(outDirectory, relativePath).getName();
        final File parentFile = new File(outDirectory, relativePath).getParentFile();

        // Nos aseguramos de que exista la estructura de directorios apropiada
        if (!parentFile.exists()) {
            boolean createdParent = false;
            try {
                createdParent = parentFile.mkdirs();
            }
            catch (final Exception e) {
                LOGGER.severe("Error al crearse la estructura de directorios del fichero '" + LoggerUtil.getCleanUserHomePath(filename) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.21"), null, filename); //$NON-NLS-1$
                return null;
            }
            if (!createdParent) {
                LOGGER.severe("No se pudo crear la estructura de directorios del fichero '" + LoggerUtil.getCleanUserHomePath(filename) + "'"); //$NON-NLS-1$ //$NON-NLS-2$
                addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.22"), null, filename); //$NON-NLS-1$
                return null;
            }
        }

        // Buscamos que no exista un fichero con el nombre que nos interesa y,
        // en caso de existir, buscamos
        // otro a base de agregar e incrementar las cifras entre
        // par&eacute;ntesis.
        int ind = 0;
        File finalFile = new File(parentFile, signer.getSignedName(signFilename, inText != null ? inText : "")); //$NON-NLS-1$
        while (finalFile.exists() && !this.overwriteFiles) {
            finalFile = new File(parentFile, signer.getSignedName(signFilename, (inText != null ? inText : "") + "(" + ++ind + ")"));  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
        }

		if (!isParent(parentFile, finalFile)) {
			LOGGER.severe("No se pudo verificar que se fuese a guardar en un subdirectorio del directorio padre el fichero " + LoggerUtil.getCleanUserHomePath(filename)); //$NON-NLS-1$
            addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.23"), null, filename); //$NON-NLS-1$
            return null;
		}

        // Almacenamos el fichero
        try (
    		final OutputStream fos = new FileOutputStream(finalFile);
		) {
            fos.write(signData);
            fos.flush();
        }
        catch (final Exception e) {
            LOGGER.severe("No se pudo crear la estructura de directorios del fichero '" + LoggerUtil.getCleanUserHomePath(filename) + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
            addLogRegistry(Level.SEVERE, MassiveSignMessages.getString("DirectorySignatureHelper.22"), null, finalFile.getPath()); //$NON-NLS-1$
        }

        // Almacenamos el nombre de fichero con la firma
        this.signedFilenames.add(finalFile.getAbsolutePath());

        return finalFile.getAbsolutePath();
    }

	 /**
	  * Comprueba que el fichero {@code parentFile} es un directorio padre de la
	  * ruta de {@code childFile}.
	  * @param parentDir Directorio padre.
	  * @param childFile Fichero/directorio hijo.
	  * @return {@code true} cuando el directorio forma parte de la ruta de directorio,
	  * {@code false} en caso contrario.
	  */
	 private static boolean isParent(final File parentDir, final File childFile) {

		 File parent;
		 File intermediateDir;

		 try {
			 parent = parentDir.getCanonicalFile();
			 intermediateDir = childFile.getCanonicalFile();
		 }
		 catch (final Exception e) {
			 return false;
		 }
		 while (intermediateDir != null && !intermediateDir.equals(parent)) {
			 intermediateDir = intermediateDir.getParentFile();
		 }
		 return intermediateDir != null;
	 }

    /** Recupera un manejador de firma compatible para el fichero de firma
     * introducido. Si no se encuentra uno o no se encuentra el fichero se lanza
     * una excepci&oacute;n.
     * @param file Fichero de firma.
     * @return Manejador de firma.
     * @throws IOException Cuando hay errores de entrada / salida */
    private static AOSigner getAppropiatedSigner(final File file) throws IOException {
        final AOSigner signer;
        signer = determineType(file);
        if (signer == null) {
            throw new IllegalArgumentException("No se ha encontrado un manejador de firma valido para el fichero '" + file.getName() + "'");  //$NON-NLS-1$//$NON-NLS-2$
        }
        return signer;
    }

    /** Comprueba si el fichero <code>file</code> es un fichero de firma aceptado
     * por el manejador <code>signer</code>. En caso de no encontrar el fichero,
     * agrega una entrada al logger indic&aacute;ndolo y se lanza una
     * excepci&oacute;n.
     * @param signer Manejador de firma.
     * @param file Fichero a analizar.
     * @return Devuelve <code>true</code> si el fichero es una firma compatible
     *         con el signer indicado, <code>false</code> en caso contrario.
     * @throws IOException Cuando no se pudo leer el fichero. */
    private static boolean isSign(final AOSigner signer, final File file) throws IOException {
    	try (
			final InputStream is = new FileInputStream(file);
		) {
	        return signer.isSign(AOUtil.getDataFromInputStream(is));
    	}
    }

    /** Comprueba si el fichero <code>file</code> es un fichero v&aacute;lido
     * para su firma mediante el manejador <code>signer</code>. En caso de no
     * encontrar el fichero, agrega una entrada al logger indic&aacute;ndolo y
     * se lanza una excepci&oacute;n.
     * @param signer Manejador de firma.
     * @param file Fichero a analizar.
     * @return Devuelve <code>true</code> si el fichero puede firmarse con el
     *         signer indicado, <code>false</code> en caso contrario.
     * @throws IOException Cuando no se pudo leer el fichero. */
    private static boolean isValidDataFile(final AOSigner signer, final File file) throws IOException {
    	final byte[] data;
    	try (
			final InputStream is = new FileInputStream(file);
		) {
    		data = AOUtil.getDataFromInputStream(is);
    	}
        try {
        	return signer.isValidDataFile(data);
        }
        catch(final OutOfMemoryError e) {
        	throw new IOException("El fichero es demasiado grande: " + e, e); //$NON-NLS-1$
        }
    }

    /** Permite activar y desactivar la generaci&oacute;n del fichero de log.
     * @param activeLog
     *        Si es <code>true</code> activa el log, <code>false</code> lo
     *        desactiva. */
    public void setActiveLog(final boolean activeLog) {
        this.activeLog = activeLog;
    }

    /** Establece la ruta del fichero de log. Si no se configura, se
     * utilizar&aacute; el directorio de salida de las firmas y el nombre de
     * fichero "result.log".
     * @param path
     *        Ruta del fichero de log. */
    public void setLogPath(final String path) {
        this.logPath = path == null || path.trim().length() == 0 ? null : path;
	}

    /** Esteblece el manejador de registro.
     * @param handler Manejador de registro */
	public void setLogHandler(final LogHandler handler) {
		this.logHandler = handler;
	}

    /** Recupera la ruta del fichero con el log de la operaci&oacute;n. Si no se
     * ha establecido una ruta, se devolver&aacute;a {@code null} y el log se
     * alamacenar&aacute;n en el directorio de salida de las firmas con el
     * nombre "result.log".
     * @return Ruta del fichero de log. */
    public String getLogPath() {
        return this.logPath;
    }

    /** Verifica si se debe generar un fichero de log con los resultados de la
     * operaci&oacute;n.
     * @return <code>true</code> si est&aacute; activada la generaci&oacute;n de
     *         un fichero de registro, <code>false</code> en caso contrario */
    public boolean isActiveLog() {
        return this.activeLog;
    }

    /** Recupera el path relativo del path de fichero absoluto introducido. Si se
     * ejecut&oacute; la operaci&oacute;n de firma masiva a partir de un
     * directorio de entrada, la funci&oacute;n devolver&aacute; el path
     * relativo del fichero desde ese directorio. Si se ejecuto a partir de un
     * listado de ficheros, se reproducir&aacute; el path completo de los
     * ficheros sin indicar la unidad de disco o directorio ra&iacute;z.
     * @param path
     *        Ruta absoluta del fichero.
     * @return Camino relativo del fichero. */
    private String getRelativePath(final String path) {
        if (this.inDir != null) {
            return path.substring(this.inDir.length());
        }
        final int pos = path.indexOf(File.separator);
        if (pos != -1) {
            return path.substring(pos + 1);
        }
        return path;
    }

    /** Establece un filtro de ficheros para limitar los objetivos de la firma
     * masiva. Si se introduce <code>null</code> no se realizar&aacute; un
     * filtrado de ficheros.
     * @param fileFilter
     *        Filtro de fichero. */
    public void setFileFilter(final FileFilter fileFilter) {
        this.fileFilter = fileFilter;
    }

    /** Filtro de ficheros v&aacute;lido tanto para realizar un filtrado
     * com&uacute;n como para usar en un <code>FileChooser</code>.
     * @return Filtro de ficheros. */
    public FileFilter getFileFilter() {
        return this.fileFilter;
    }

    /** Devuelve el manejador de firma que se utilizar&aacute; para realizar las
     * operaciones de firma.
     * @return Manejador de firma. */
    public AOSigner getDefaultSigner() {
        return this.defaultSigner;
    }

    /** Indica si durante el guardado de firmas se deben sobrescribir los
     * ficheros previos que se encuentren con el mismo nombre.
     * @param overwirte Sobrescribir ficheros. */
    public void setOverwritePreviuosFileSigns(final boolean overwirte) {
        this.overwriteFiles = overwirte;
    }

    /** Agrega un registro a un fichero de log. Si es nulo el registro de log se
     * muestra un mensaje por consola.
     * @param typeLog Tipo de mensaje.
     * @param msg Mensaje
     * @param inputData Datos que se enviaron para firmar
     * @param outputSign Firma resultante */
    private void addLogRegistry(final Level typeLog,
    		                    final String msg,
    		                    final String inputData,
    		                    final String outputSign) {
        if (this.activeLog) {
            if (msg == null) {
                LOGGER.warning("Se ha intentado insertar un registro nulo en el log"); //$NON-NLS-1$
                return;
            }
            if (this.logHandler != null) {
            	try {
					this.logHandler.addLog(typeLog.intValue(), msg, inputData, outputSign);
				} catch (final IOException e) {
					LOGGER.warning("No se ha podido insertar un registro en el log de la operacion: " + msg + ": " + e.toString()); //$NON-NLS-1$ //$NON-NLS-2$
				}
            }
            if (typeLog == Level.WARNING) {
                this.warnCount++;
            }
            if (typeLog == Level.SEVERE) {
                this.errorCount++;
            }
        }
    }

    /** Inicializa el fichero de log para la firma masiva.
     * @param outputStream Flujo para la escritura del registro
     * @return Manejador del registro */
    protected static LogHandler initDefaultLogHandler(final OutputStream outputStream) {
    	return new DefaultLogHandler(outputStream);
    }

    /** Cierra el fichero de <i>log</i>. */
    private void closeLogRegistry() {

        if (this.logHandler != null) {

        	final Properties params = new Properties();
    		params.setProperty("warningsCount", Integer.toString(this.warnCount)); //$NON-NLS-1$
    		params.setProperty("errorsCount", Integer.toString(this.errorCount)); //$NON-NLS-1$

        	try {
                this.logHandler.close(params);
            }
            catch (final Exception e) {
                LOGGER.warning("El fichero de log no se ha cerrado correctamente: " + e); //$NON-NLS-1$
            }
        }
        this.warnCount = 0;
        this.errorCount = 0;
    }

    /** Obtiene el tipo de firma de un fichero. Si no se puede identificar, se
     * devolver&aacute; {@code null}.
     * @param file
     *        Fichero que deseamos analizar.
     * @return Resultado del an&aacute;lisis.
     * @throws IOException Cuando ocurre alg&uacute;n error en la lectura del fichero */
    private static AOSigner determineType(final File file) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("Se ha introducido un fichero de firma nulo"); //$NON-NLS-1$
        }
        if (!file.exists() || !file.isFile()) {
            throw new FileNotFoundException("El archivo indicado no existe o no es un fichero de datos"); //$NON-NLS-1$
        }
        if (!file.canRead()) {
            throw new IOException("No tiene permisos de lectura sobre el fichero indicado"); //$NON-NLS-1$
        }

        try (
    		final InputStream fis = new FileInputStream(file);
		) {
            return AOSignerFactory.getSigner(AOUtil.getDataFromInputStream(fis));
        }
        catch (final Exception e) {
            throw new IOException("Error al leer el fichero: " + e, e); //$NON-NLS-1$
        }
    }

    /** Recupera el listado de los nombres de ficheros de firma generados.
     * @return Listado de nombres de fichero. */
    public String[] getSignedFilenames() {
        return this.signedFilenames.toArray(new String[0]);
    }

    /** Indica si unos datos son compatibles con alguno de los formatos de firma para
     * documentos espec&iacute;ficos (PDF, ODF u OOXML). Es obligatorio que el manejador
     * de firma de cada formato este disponible para su uso.
     * @param data Datos que se desean revisar.
     * @return Manejador de firma compatible con los datos indicados o {@code null} si
     * no se encontr&oacute; ninguno.
     * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de los datos. */
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

    /** Genera la huella digital de los datos con el algoritmo indicado por
     * {@code DEFAULT_MESSAGE_DIGEST_ALGORITHM}.
     * @param data Datos de la que generar la huella.
     * @return Huella digital. */
    private static byte[] digest(final byte[] data) {
    	if (md == null) {
    		try {
				md = MessageDigest.getInstance(DEFAULT_MESSAGE_DIGEST_ALGORITHM);
			}
    		catch (final NoSuchAlgorithmException e) {
				LOGGER.severe("Se ha utilizado internamente un algoritmo de huella digital no soportado: " + e); //$NON-NLS-1$
				throw new IllegalArgumentException("Algoritmo no soportado", e); //$NON-NLS-1$
			}
    	}
    	return md.digest(data);
    }
}
