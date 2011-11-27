/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.massive;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.util.AOBase64;
import es.gob.afirma.core.signers.AOSignerFactory;


/** M&oacute;dulo para el soporte de multifirmas m&aacute;sivas. Permite
 * configurar una operaci&oacute;n de firma y ejecutarla sobre datos, hashes y
 * ficheros.</br> Se crea un log de la operaci&oacute;n masiva en donde cada
 * entrada se corresponde con el resultado de la ejecuci&oacute;n de una
 * operaci&oacute;n. */
public final class MassiveSignatureHelper {
    
    private static final String XADES_SIGNER = "es.gob.afirma.signers.xades.AOXAdESSigner"; //$NON-NLS-1$
    private static final String XMLDSIG_SIGNER = "es.gob.afirma.signers.xml.xmldsig.AOXMLDSigSigner"; //$NON-NLS-1$
    
    private static final String REG_FIELD_SEPARATOR = " - "; //$NON-NLS-1$
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Configuracion de la operaci&oacute;n masiva. */
    private MassiveSignConfiguration massiveConfiguration = null;

    /** Logger de las operaciones de firma masiva. */
    private List<String> log = null;

    /** Manejador de firma para el formato configurado por defecto. */
    private AOSigner defaultSigner = null;

    /** Indica si el m&oacute;dulo est&aacute; inicializado. */
    private boolean isInitialized;
    
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

        this.massiveConfiguration = configuration;

        // Creamos el manejador de firma por defecto
        this.defaultSigner = AOSignerFactory.getSigner(this.massiveConfiguration.getDefaultFormat());
        if (this.defaultSigner == null) {
            throw new AOException("Formato de firma no soportado: " + this.massiveConfiguration.getDefaultFormat()); //$NON-NLS-1$
        } 
        
        this.isInitialized = true;
    }

    /** Comprueba si el modulo de firma masiva esta inicializado.
     * @return Devuelve <code>true</code> si est&aacute; inicializado, <code>false</code> en caso contrario. */
    public boolean isInitialized() {
        return this.isInitialized;
    }

    /** Establece el tipo de operaci&oacute;n (firma, cofirma, contrafirma del
     * &aacute;rbol de firma o contrafirma de nodos hojas) que debe realizar el
     * m&oacute;dulo de firma masiva. Si se indica <code>null</code> se
     * establece la configuraci&oacute;n por defecto (firma).
     * @param massiveOperation
     *        Tipo de operaci&oacute;n.
     * @see MassiveType */
    public void setMassiveOperation(MassiveType massiveOperation) {
        this.massiveConfiguration.setMassiveOperation(
            (massiveOperation != null ? massiveOperation : MassiveType.SIGN)
        );
    }

    /** Libera la configuraci&oacute;n de la operaci&oacute;n masiva e inhabilita
     * su uso. El m&oacute;dulo aun conservar&aacute;a la informaci&oacute;n de
     * log. */
    public void release() {
        this.massiveConfiguration = null;
        this.defaultSigner = null;
        this.isInitialized = false;
    }

    /** Realiza la firma de datos.
     * @param b64Data
     *        Datos en base 64 que se desean firmar.
     * @return Resultado de la firma en Base64. */
    public String signData(final String b64Data) {

        if (!this.isInitialized) {
            throw new IllegalArgumentException("El modulo de firma masiva no ha sido inicializado"); //$NON-NLS-1$
        }

        if (b64Data == null) {
            LOGGER.severe("No se han introducido datos para firmar"); //$NON-NLS-1$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.0")); //$NON-NLS-1$
            return null;
        }

        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone(); // Configuracion
        config.setProperty("headLess", Boolean.toString(true));  //$NON-NLS-1$
        final byte[] data = AOBase64.decode(b64Data); // Datos a
                                                                // firmar
        String operation = null; // Para aclarar mensajes por consola
        byte[] signData = null; // Firma resultante

        // Ejecutamos la operacion que corresponda
        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
                operation = "sign"; //$NON-NLS-1$
                signData = signDataFromData(this.defaultSigner, data, null, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COSIGN)) { // Cofirma
                operation = "cosign"; //$NON-NLS-1$
                signData = cosign(this.defaultSigner, data, config);
            }
            else if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.COUNTERSIGN_ALL)) { // Contraforma del arbol completo
                operation = "countersign tree"; //$NON-NLS-1$
                signData = countersignTree(this.defaultSigner, data, config);
            }
            else { // Contrafirma de los nodos hoja
                operation = "countersign tree leafs"; //$NON-NLS-1$
                signData = countersignLeafs(this.defaultSigner, data, config);
            }
        }
        catch (final AOFormatFileException e) {
            LOGGER.severe("Los datos introducidos no tienen un formato valido: " + e); //$NON-NLS-1$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.1")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("Error al " + operation + " los datos introducidos: " + e.getMessage());  //$NON-NLS-1$//$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.2") + REG_FIELD_SEPARATOR + operation + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }

        this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.3")); //$NON-NLS-1$

        return AOBase64.encode(signData, false);
    }

    /** Realiza la firma de un hash. La cofirma y contrafirma de hashes no esta
     * soportada.
     * @param b64Hash
     *        Hash en base 64 que se desea firmar.
     * @return Resultado de la firma en Base64. */
    public String signHash(final String b64Hash) {

        if (!this.isInitialized) {
            throw new IllegalArgumentException("El modulo de firma masiva no ha sido inicializado"); //$NON-NLS-1$
        }

        if (b64Hash == null) {
            LOGGER.severe("No se ha introducido un hash para firmar"); //$NON-NLS-1$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.4")); //$NON-NLS-1$
            return null;
        }

        // Transformamos los datos
        final byte[] hash = AOBase64.decode(b64Hash);

        // Solo para aclarar los posibles mensajes por consola, almacenaremos
        String operation = "sign"; //$NON-NLS-1$

        // Firma resultante
        byte[] signData = null;

        // Ejecutamos la operacion que corresponda
        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone();
        config.setProperty("headLess", Boolean.toString(true)); //$NON-NLS-1$
        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
                operation = "sign"; //$NON-NLS-1$
                signData = signDataFromHash(this.defaultSigner, hash, config);
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
            LOGGER.severe("Error al operar sobre el hash '" + b64Hash + "', '" + operation + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.5") + REG_FIELD_SEPARATOR + operation + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }

        this.addLog("Operaci\u00F3n sobre hash: Correcta"); //$NON-NLS-1$

        return AOBase64.encode(signData, false);
    }

    /** Realiza la operaci&oacute;n de multifirma sobre un fichero.
     * @param fileUri
     *        Path del fichero que se desea firmar.
     * @return Multifirma del fichero. */
    public String signFile(final String fileUri) {

        if (!this.isInitialized) {
            LOGGER.severe("El modulo de firma masiva no ha sido inicializado"); //$NON-NLS-1$
            throw new IllegalArgumentException("El modulo de firma masiva no ha sido inicializado"); //$NON-NLS-1$
        }

        if (fileUri == null) {
            LOGGER.severe("No se ha introducido un fichero para firmar"); //$NON-NLS-1$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.6")); //$NON-NLS-1$
            return null;
        }

        // Creamos la URI del fichero
        URI uri = null;
        try {
            uri = AOUtil.createURI(fileUri);
        }
        catch (final Exception e) {
            LOGGER.severe("La URI '" + fileUri + "' no posee un formato valido: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.7") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }

        // Creamos el flujo de datos del fichero
        InputStream is = null;
        try {
            is = AOUtil.loadFile(uri);
        }
        catch (final FileNotFoundException e) {
            LOGGER.severe("No ha sido posible encontrar el fichero '" + fileUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.8") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("No es posible acceder al contenido del fichero '" + fileUri + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.9") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }

        // Leemos el contenido del fichero
        byte[] data = null;
        try {
            data = AOUtil.getDataFromInputStream(is);
        }
        catch (final Exception e) {
            LOGGER.severe("No es posible leer el contenido del fichero '" + fileUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.11") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }
        if (data == null) {
            LOGGER.severe("El fichero '" + fileUri + "' esta vacio"); //$NON-NLS-1$ //$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.12") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }

        // Liberamos el fichero de recursos
        try {
            is.close();
            is = null;
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido liberar el fichero '" + fileUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Ejecutamos la operacion que corresponda
        byte[] signData = null;
        final Properties config = (Properties) this.massiveConfiguration.getExtraParams().clone();
        config.setProperty("headLess", Boolean.toString(true));  //$NON-NLS-1$

        try {
            if (this.massiveConfiguration.getMassiveOperation().equals(MassiveType.SIGN)) { // Firma
                signData = signDataFromData(this.defaultSigner, data, uri, config);
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
            LOGGER.severe("El fichero '" + fileUri + "' no tiene un formato valido: " + e.getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.13") + REG_FIELD_SEPARATOR + fileUri); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.severe("Error al realizar la operacion "  //$NON-NLS-1$
                    + this.massiveConfiguration.getMassiveOperation()
                    + " sobre el fichero '" //$NON-NLS-1$
                    + fileUri
                    + "': " //$NON-NLS-1$
                    + e.getMessage());
            this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.14") + REG_FIELD_SEPARATOR + e.getMessage()); //$NON-NLS-1$
            return null;
        }

        this.addLog(MassiveSignMessages.getString("MassiveSignatureHelper.15")); //$NON-NLS-1$

        return AOBase64.encode(signData, false);
    }

    /** Firma datos con el signer indicado.
     * @param signer
     *        Manejador con el que firmar los datos.
     * @param data
     *        Datos a firmar.
     * @param uri
     *        Uri de los datos a firmar (opcional seg&uacute;n formato de
     *        firma).
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de firma. */
    private byte[] signDataFromData(final AOSigner signer, final byte[] data, final URI uri, final Properties config) throws AOException {

        // Configuramos y ejecutamos la operacion
        config.setProperty("mode", this.massiveConfiguration.getMode()); //$NON-NLS-1$
        config.setProperty("format", this.massiveConfiguration.getDefaultFormat()); //$NON-NLS-1$
        if (uri != null) {
            config.setProperty("uri", uri.toString()); //$NON-NLS-1$
        }

        // Deteccion del MIMEType, solo para XAdES y XMLDSig
        if ((signer.getClass().getName().equals(XADES_SIGNER)) || (signer.getClass().getName().equals(XMLDSIG_SIGNER))) {
            final MimeHelper mimeHelper = new MimeHelper(data);
            final String mimeType = mimeHelper.getMimeType();
            if (mimeType != null) {
                try {
                    config.setProperty("mimeType", mimeType); //$NON-NLS-1$
                    config.setProperty("oid", MimeHelper.transformMimeTypeToOid(mimeType)); //$NON-NLS-1$
                }
                catch (final Exception e) {
                    LOGGER
                          .warning("No se ha podido detectar el MIME-Type, se utilizara el por defecto y este aspecto no se indicara en el registro de firma masiva: " + e); //$NON-NLS-1$
                }
            }
        }

        final byte[] signData = signer.sign(
                data,
                this.massiveConfiguration.getAlgorithm(),
                this.massiveConfiguration.getKeyEntry(),
                config);

        if (signData == null) {
            throw new AOException("No se generaron datos de firma"); //$NON-NLS-1$
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
     *         Cuando ocurre un error durante la operaci&oacute;n de firma. */
    private byte[] signDataFromHash(final AOSigner signer, final byte[] data, final Properties config) throws AOException {

        // Configuramos y ejecutamos la operacion
        config.setProperty("mode", this.massiveConfiguration.getMode()); //$NON-NLS-1$
        config.setProperty("format", this.massiveConfiguration.getDefaultFormat()); //$NON-NLS-1$
        config.setProperty("precalculatedHashAlgorithm", AOSignConstants.getDigestAlgorithmName(this.massiveConfiguration.getAlgorithm())); //$NON-NLS-1$

        // Introduccion MIMEType "hash/algo", solo para XAdES y XMLDSig
        if ((signer.getClass().getName().equals(XADES_SIGNER)) || (signer.getClass().getName().equals(XMLDSIG_SIGNER))) {
            final String mimeType = "hash/" + AOSignConstants.getDigestAlgorithmName(this.massiveConfiguration.getAlgorithm()).toLowerCase(); //$NON-NLS-1$
            try {
                config.setProperty("mimeType", mimeType); //$NON-NLS-1$
                config.setProperty("oid", MimeHelper.transformMimeTypeToOid(mimeType)); //$NON-NLS-1$
            }
            catch (final Exception e) {
                LOGGER
                      .warning("Error al indicar el MIME-Type, se utilizara el por defecto y este aspecto no se indicara en el registro de firma masiva: " + e); //$NON-NLS-1$
            }
        }

        final byte[] signData = signer.sign(data, this.massiveConfiguration.getAlgorithm(), this.massiveConfiguration.getKeyEntry(), config);
        if (signData == null) {
            throw new AOException("No se generaron datos de firma"); //$NON-NLS-1$
        }
        return signData;
    }

    /** Cofirma datos con el manejador configurado o con el m&aacute;s apropiado
     * si se indic&oacute; que se buscase.
     * @param signer
     *        Manejador con el que cofirmar los datos.
     * @param sign
     *        Firma con los datos a cofirmar.
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de firma. */
    private byte[] cosign(final AOSigner signer, final byte[] sign, final Properties config) throws AOException {

        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        final AOSigner validSigner = this.getValidSigner(signer, sign);

        // Configuramos y ejecutamos la operacion
        config.setProperty("mode", this.massiveConfiguration.getMode()); //$NON-NLS-1$

        final byte[] signData = validSigner.cosign(sign, this.massiveConfiguration.getAlgorithm(), this.massiveConfiguration.getKeyEntry(), config);
        if (signData == null) {
            throw new AOException("No se generaron datos de firma"); //$NON-NLS-1$
        }
        return signData;
    }

    /** Contrafirma todos los nodos de firma de los datos de firma introducidos
     * usando el manejador configurado o el m&aacute;s apropiado si se
     * indic&oacute; que se buscase.
     * @param signer
     *        Manejador con el que contrafirmar.
     * @param sign
     *        Firma a contrafirmar.
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma. */
    private byte[] countersignTree(final AOSigner signer, final byte[] sign, final Properties config) throws AOException {
        return countersignOperation(signer, sign, CounterSignTarget.TREE, config);
    }

    /** Contrafirma las hojas de la estructura de firma introducida usando el
     * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
     * se buscase.
     * @param signer
     *        Manejador con el que contrafirmar.
     * @param sign
     *        Firma a contrafirmar.
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma. */
    private byte[] countersignLeafs(final AOSigner signer, final byte[] sign, final Properties config) throws AOException {
        return countersignOperation(signer, sign, CounterSignTarget.LEAFS, config);
    }

    /** Contrafirma los nodos indicados de una firma electr&oacute;nica usando el
     * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
     * se buscase.
     * @param signer
     *        Manejador con el que contrafirmar.
     * @param sign
     *        Firma a contrafirmar.
     * @param target
     *        Nodos objetivos para la contrafirma.
     * @param config
     *        Configuraci&oacute;n general para la operaci&oacute;n.
     * @return Firma electr&oacute;nica con el formato dado por el manejador de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante la operaci&oacute;n de
     *         contrafirma. */
    private byte[] countersignOperation(final AOSigner signer, final byte[] sign, final CounterSignTarget target, final Properties config) throws AOException {

        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        final AOSigner validSigner = this.getValidSigner(signer, sign);

        final byte[] signData = validSigner.countersign(sign, this.massiveConfiguration.getAlgorithm(), target, null, this.massiveConfiguration.getKeyEntry(), config);
        if (signData == null) {
            throw new AOException("No se generaron datos de firma"); //$NON-NLS-1$
        }
        return signData;
    }

    /** Recupera el signer apropiado para la cofirma o contrafirma de la firma
     * introducida. Comprueba si se ha establecido que se respete el formato por
     * defecto introducido o si se debe buscar el formato m&aacute;s apropiado
     * seg&uacute;n el tipo de firma.
     * @param signer
     *        Manejador de firma.
     * @param signData
     *        Firma para la que deseamos obtener un manejador.
     * @return Manejador compatible con la firma introducida.
     * @throws AOException
     *         Si la firma introducida no se corresponde con ning&uacute;n
     *         formato soportado o se obliga a usar el manejador por defecto
     *         y este no la soporta. */
    private AOSigner getValidSigner(final AOSigner signer, final byte[] signData) throws AOException {
        // Tomamos el signer adecuado para la operacion o el obligatorio si se
        // especifico
        AOSigner validSigner = signer;
        if (!this.massiveConfiguration.isOriginalFormat()) {
            if (!signer.isSign(signData)) {
                throw new AOException("La firma introducida no se corresponde con el formato de firma especificado"); //$NON-NLS-1$
            }
        }
        else {
            validSigner = AOSignerFactory.getSigner(signData);
            if (validSigner == null) {
                throw new AOException("La firma introducida no se corresponde con ning\u00FAn formato soportado"); //$NON-NLS-1$
            }
        }
        return validSigner;
    }

    /** Agrega una entrada al log de la operacion de multifirma masiva global.
     * @param message
     *        Entrada del log. */
    private void addLog(final String message) {
        if (this.log == null) {
            this.log = new ArrayList<String>();
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

    /** Almacena los datos necesarios para realizar una operaci&oacute;n masiva
     * de firma. */
    public static class MassiveSignConfiguration {

        private final X509Certificate certificate;
        private final PrivateKeyEntry keyEntry;

        private MassiveType massiveOperation = null;
        private String algorithm = AOSignConstants.DEFAULT_SIGN_ALGO;
        private String mode = AOSignConstants.DEFAULT_SIGN_MODE;
        private String defaultFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
        private boolean originalFormat = true;
        private Properties extraParams;

        /** Crea un <i>JavaBean</i> con los par&aacute;metros necesarios para las
         * operaciones de firma masiva.
         * @param keyEntry
         *        Clave privada para las firmas
         * @param certificate
         *        Certificado X.509 firmante */
        public MassiveSignConfiguration(final PrivateKeyEntry keyEntry, final X509Certificate certificate) {
            this.keyEntry = keyEntry;
            this.certificate = certificate;
            this.extraParams = new Properties();
        }

        /** Recupera la operaci&oacute;n masiva configurada.
         * @return Tipo de operaci&oacute;n masiva. */
        public MassiveType getMassiveOperation() {
            return this.massiveOperation;
        }

        /** Establece la operaci&oacute;n masiva que deber&aacute; ejecutarse.
         * @param massiveOperation
         *        Tipo de operaci&oacute;n masiva. */
        public void setMassiveOperation(final MassiveType massiveOperation) {
            this.massiveOperation = massiveOperation;
        }

        /** Recupera el algoritmo de firma configurado.
         * @return Algoritmo de firma. */
        public String getAlgorithm() {
            return this.algorithm;
        }

        /** Estable el algoritmo de firma.
         * @param algorithm
         *        Algoritmo de firma. */
        public void setAlgorithm(final String algorithm) {
            this.algorithm = algorithm;
        }

        /** Recupera el modo de firma configurado.
         * @return Modo de firma. */
        public String getMode() {
            return this.mode;
        }

        /** Estable el modo de firma.
         * @param mode
         *        Modo de firma. */
        public void setMode(final String mode) {
            this.mode = mode;
        }

        /** Recupera el formato de firma configurado por defecto.
         * @return Formato de firma. */
        public String getDefaultFormat() {
            return this.defaultFormat;
        }

        /** Estable el formato de firma por defecto (para cuando no se desee
         * respetar el original o se realiza una firma masiva).
         * @param defaultFormat
         *        Formato de firma. */
        public void setDefaultFormat(final String defaultFormat) {
            this.defaultFormat = defaultFormat;
        }

        /** Indica si se ha configurado que las multifirmas respeten el formato
         * de firma original.
         * @return Devuelve {@code true} si se ha configurado que se respete el
         *         formato de firma, {@code false} en caso contrario. */
        public boolean isOriginalFormat() {
            return this.originalFormat;
        }

        /** Estable si debe utilizarse un formato de firma original en le caso de
         * las multifirmas masivas.
         * @param originalFormat
         *        Respetar formato original de firma. */
        public void setOriginalFormat(final boolean originalFormat) {
            this.originalFormat = originalFormat;
        }

        /** Recupera el certificado de firma.
         * @return Certificado de firma. */
        public Certificate getCertificate() {
            return this.certificate;
        }

        /** Recupera entrada de la clave de firma.
         * @return Entrada de la clave de firma. */
        public PrivateKeyEntry getKeyEntry() {
            return this.keyEntry;
        }

        /** Establece par&aacute;metros adicionales para la configuraci&oacute;n
         * de la operaci&oacute;n masiva.
         * @param extraParams
         *        Par&aacute;metros adicionales. */
        public void setExtraParams(final Properties extraParams) {
            if (extraParams != null) {
                this.extraParams = (Properties) extraParams.clone();
            }
            else {
                this.extraParams.clear();
            }
        }

        /** Recupera los par&aacute;metros adicionales configurados para la
         * operaci&oacute;n masiva.
         * @return Par&aacute;metros adicionales. */
        public Properties getExtraParams() {
            return this.extraParams;
        }
    }

}
