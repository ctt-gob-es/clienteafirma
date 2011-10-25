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
 *   <dd>Identificadora de la pol&iacute;tica de firma (normalmente un OID que identifica &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable)</dd>
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

    public byte[] cosign(final byte[] data, final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCoSigner").newInstance()).cosign(data, sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(AOException e) {
            throw e;
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se pueden realizar cofirmas CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCoSigner").newInstance()).cosign(sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(AOException e) {
            throw e;
        }
        catch(final Exception e) {
            throw new UnsupportedOperationException("No se pueden realizar cofirmas CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] countersign(final byte[] sign,
                              String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              Properties extraParams) throws AOException {
        try {
            return ((AOCounterSigner)AOUtil.classForName("es.gob.afirma.signers.cades.multi.AOCAdESCounterSigner").newInstance()).countersign(sign, algorithm, targetType, targets, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(AOException e) {
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
        return new CAdESValidator().isCADESSignedData(data);
    }

    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se realiza la verificaci&oacute;n sobre los los siguientes tipos de CMS
     * reconocidos:
     * <ul>
     * <li>Data</li>
     * <li>Signed Data</li>
     * <li>Digested Data</li>
     * <li>Encrypted Data</li>
     * <li>Enveloped Data</li>
     * <li>Signed and Enveloped Data</li>
     * </ul>
     * @param data
     *        Datos que deseamos comprobar.
     * @return La validez del archivo cumpliendo la estructura. */
    public boolean isCADESValid(final byte[] data) {
        // si se lee en el CMSDATA, el inputstream ya esta leido y en los demas
        // siempre sera nulo
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        // Comprobamos si su contenido es de tipo DATA
        boolean valido = new CAdESValidator().isCADESData(data);
        // Comprobamos si su contenido es de tipo SIGNEDDATA
        if (!valido) {
            valido = new CAdESValidator().isCADESSignedData(data);
        }
        // Comprobamos si su contenido es de tipo DIGESTDATA
        if (!valido) {
            valido = new CAdESValidator().isCADESDigestedData(data);
        }
        // Comprobamos si su contenido es de tipo ENCRYPTEDDATA
        if (!valido) {
            valido = new CAdESValidator().isCADESEncryptedData(data);
        }
        // Comprobamos si su contenido es de tipo ENVELOPEDDATA
        if (!valido) {
            valido = new CAdESValidator().isCADESEnvelopedData(data);
        }
        // Comprobamos si su contenido es de tipo SIGNEDANDENVELOPED
        if (!valido) {
            valido = new CAdESValidator().isCADESSignedAndEnvelopedData(data);
        }
        return valido;
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
        if (!this.isCADESValid(signData)) {
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
