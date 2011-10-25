/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cades;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.ExtractMimeType;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Manejador de firmas binarias CADES.
 * <p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt><b>mode</b></dt>
 *   <dd>Modo de firma a usar (Expl&iacute;cita = <code>explicit</code> o Impl&iacute;cita = <code>implicit</code>)</dd>
 *  <dt><b>policyIdentifier</b></dt>
 *   <dd>Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.</dd>
 *  <dt><b>policyIdentifierHash</b></dt>
 *   <dd>
 *    Huella digital del documento de pol&iacute;tica de firma (normlamente del mismo fichero en formato ASN.1 procesable).
 *    Si no se indica, y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible universalmente se usar&aacute; <code>0</code> 
 *   </dd>
 *  <dt><b>policyIdentifierHashAlgorithm</b></dt>
 *   <dd>Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>
 *  <dt><b>precalculatedHashAlgorithm</b></dt>
 *   <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 *  <dt><b>signingCertificateV2</b></dt>
 *   <dd>
 *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo 
 *    <i>Signing Certificate</i> de CAdES. Si no se establece
 *    o se hace a <code>false</code> se utilizara la versi&oacute;n 1
 *   </dd>
 * </dl>
 * @version 0.3 */
public final class AOCAdESSigner implements AOSigner {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    private static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    public byte[] sign(byte[] data, String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$

        X509Certificate[] xCerts = new X509Certificate[0];
        final Certificate[] certs = keyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            xCerts = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = keyEntry.getCertificate();
            if (cert instanceof X509Certificate) {
                xCerts = new X509Certificate[] {
                    (X509Certificate) cert
                };
            }
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, xCerts);

        try {
            boolean omitContent = false;
            if (mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null) {
                omitContent = true;
            }
                        
            return new GenCAdESEPESSignedData().generateSignedData(csp,
                                                                   omitContent,
                                                                   new AdESPolicy(extraParams),
                                                                   signingCertificateV2,
                                                                   keyEntry,
                                                                   messageDigest);

        }
        catch (final Exception e) {
            throw new AOException("Error generando la firma CAdES", e); //$NON-NLS-1$
        }
    }

    /** Cofirma datos en formato CAdES. Para realizar la
     * cofirma se necesitan los datos originales (que este m&eacute;todo
     * firmar&aacute; normalmente) y la firma sobre la que se realiza la cofirma
     * (a los que se agregar&aacute; el resultado de la nueva firma).
     * <p><b>IMPORTANTE: Requiere la presencia de <code>es.gob.afirma.signers.cades.multi.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param data Datos que deseamos a cofirmar.
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la cofirma.
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCoSigner").newInstance()).cosign(data, sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final AOException e) {
            throw e;
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se pueden realizar cofirmas CAdES", e); //$NON-NLS-1$
        }
    }

    /** Cofirma datos en formato CAdES. Para realizar la
     * cofirma se necesita el documento en el que se encuentra la firma sobre la
     * que se realiza la cofirma (a los que se agregar&aacute; el resultado de
     * la nueva firma).
     * <p><b>IMPORTANTE: Requiere la presencia de <code>es.gob.afirma.signers.cades.multi.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la cofirma
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCoSigner").newInstance()).cosign(sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final AOException e) {
            throw e;
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se pueden realizar cofirmas CAdES", e); //$NON-NLS-1$
        }
    }

    /** Contrafirma nodos de firma concretos de una firma electr&oacute;nica.<br/>
     * Los nodos que se deben firmar se indican en <code>targetType</code> y
     * pueden ser:
     * <ul>
     *  <li>Todos los nodos del &aacute;rbol de firma</li>
     *  <li>Los nodos hoja del &aacute;rbol de firma</li>
     *  <li>Los nodos de firma cuyas posiciones se especifican en <code>target</code></li>
     *  <li>Los nodos de firma realizados por los firmantes cuyo <i>Common Name</i> se indica en <code>target</code></li>
     * </ul>
     * <p><b>IMPORTANTE: Requiere la presencia de <code>es.gob.afirma.signers.cades.multi.AOCAdESCounterSigner</code> en el CLASSPATH</b></p>
     * @param sign Firma CAdES o CMS con los nodos a contrafirmar
     * @param algorithm Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param targetType Tipo de objetivo de la contrafirma
     * @param targets Informaci&oacute;n complementario seg&uacute;n el tipo de objetivo de la contrafirma
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la contrafirma
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        try {
            return ((AOCounterSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCounterSigner").newInstance()).countersign(sign, algorithm, targetType, targets, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final AOException e) {
            throw e;
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se pueden realizar contrafirmas CAdES", e); //$NON-NLS-1$
        }
    }

    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        try {
            return new ReadNodesTree().readNodesTree(sign, asSimpleSignInfo);
        }
        catch (final Exception ex) {
            LOGGER.severe("No se ha podido obtener el arbol de firmantes de la firma, se devolvera null: " + ex); //$NON-NLS-1$
        }
        return null;
    }

    public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return new CAdESValidator().isCAdESSignedData(data);
    }

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** Obtiene el tipo de datos declarado en una firma mediante su Mime Type. Si
     * no se conoce el tipo de dato se devolver&aacute; <code>null</code>.
     * Seg&uacute;n el formato de firma puede haber un tipo de datos por
     * defecto: application/octect-stream,...
     * @param signData
     *        Firma electr&oacute;nica.
     * @return Mime Type de los datos contenidos en la firma.
     * @throws AOUnsupportedSignFormatException
     *         Cuando la firma no est&eacute; soportada por el manejador
     *         proporcionado. */
    public String getDataMimeType(final byte[] signData) throws AOUnsupportedSignFormatException {

        // Comprobamos que sea una firma valida
        try {
            this.isSign(signData);
        }
        catch (final Exception e1) {
            throw new AOUnsupportedSignFormatException("No es un tipo de firma valido", e1); //$NON-NLS-1$
        }

        // Extraemos el mimetype y transformamos el OID a mimeType
        return MimeHelper.transformOidToMimeType(new ExtractMimeType().extractMimeType(signData));

    }

    public byte[] getData(final byte[] signData) throws AOInvalidFormatException {
        if (signData == null) {
            throw new IllegalArgumentException("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
        }
        if (!CAdESValidator.isCAdESValid(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new ObtainContentSignedData().obtainData(signData);
    }

    public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".csig"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    public AOSignInfo getSignInfo(final byte[] signData) throws AOException {
        if (signData == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_CADES);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo

    }
}
