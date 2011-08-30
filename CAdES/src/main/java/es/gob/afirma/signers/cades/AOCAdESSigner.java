/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.cades;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.ExtractMimeType;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Manejador de firmas binarias CADES.
 * <p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 * <dt>mode</dt>
 * <dd>Modo de firma a usar (Expl&iacute;cita = <code>explicit</code> o Impl&iacute;cita = <code>implicit</code>)</dd>
 * <dt>policyIdentifier</dt>
 * <dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 * <dt>policyQualifier</dt>
 * <dd>OID calificador de la pol&iacute;tica de firma</dd>
 * <dt>precalculatedHashAlgorithm</dt>
 * <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 * <dt>signingCertificateV2</dt>
 * <dd>Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i> de CAdES. Si no se establece
 * o se hace a <code>false</code> se utilizara la versi&oacute;n 1</dd>
 * </dl>
 * @version 0.3 */
public final class AOCAdESSigner implements AOSigner {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


    public byte[] sign(byte[] data, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "true")); //$NON-NLS-1$ //$NON-NLS-2$

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
            String policyQualifier = null;
            // Nos puede venir como URN o como OID
            try {
                policyQualifier =
                        extraParams.getProperty("policyQualifier").replace("urn:oid:", "").replace("URN:oid:", "").replace("Urn:oid:", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
            }
            catch (final Exception e) {
                // Se ignora, podemos no tener politica
            }

            return new GenCAdESEPESSignedData().generateSignedData(csp,
                                                                   omitContent,
                                                                   extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                                   policyQualifier,
                                                                   signingCertificateV2,
                                                                   keyEntry,
                                                                   messageDigest);

        }
        catch (final Exception e) {
            throw new AOException("Error generando la firma CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] data, final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false")); //$NON-NLS-1$ //$NON-NLS-2$

        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

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
            String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$

            // Si la firma que nos introducen es SignedData
            //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
            final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
            if (signedData) {

                final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$
                final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

                return new CAdESCoSigner().coSigner(csp,
                                                    sign,
                                                    omitContent,
                                                    extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                    policyQualifier,
                                                    signingCertificateV2,
                                                    keyEntry,
                                                    messageDigest);
            }

            return new CAdESCoSignerEnveloped().coSigner(csp,
                                                         sign,
                                                         extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                         policyQualifier,
                                                         signingCertificateV2,
                                                         keyEntry,
                                                         messageDigest);

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CAdES", e); //$NON-NLS-1$
        }
    }

    public byte[] cosign(final byte[] sign, String algorithm, final PrivateKeyEntry keyEntry, Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false")); //$NON-NLS-1$ //$NON-NLS-2$

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

        // algoritmo de firma.
        final String typeAlgorithm = algorithm;
        // Array de certificados
        X509Certificate[] aCertificados = new X509Certificate[0];
        final Certificate[] certs = keyEntry.getCertificateChain();
        if (certs != null && (certs instanceof X509Certificate[])) {
            aCertificados = (X509Certificate[]) certs;
        }
        else {
            final Certificate cert = keyEntry.getCertificate();
            if (cert instanceof X509Certificate) {
                aCertificados = new X509Certificate[] {
                                                       (X509Certificate) cert
                };
            }
        }

        final String policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$

        // Si la firma que nos introducen es SignedData
        //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
        if (signedData) {
            try {
                return new CAdESCoSigner().coSigner(typeAlgorithm,
                                                    aCertificados,
                                                    new ByteArrayInputStream(sign),
                                                    extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                    policyQualifier,
                                                    signingCertificateV2,
                                                    keyEntry,
                                                    null // null porque no nos pueden dar un hash
                                                         // en este metodo, tendría que ser en el
                                                         // que incluye datos
                );
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
            }
        }
        // Signed And Enveloped.

        try {
            return new CAdESCoSignerEnveloped().coSigner(typeAlgorithm,
                                                         aCertificados,
                                                         new ByteArrayInputStream(sign),
                                                         extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
                                                         policyQualifier,
                                                         signingCertificateV2,
                                                         keyEntry,
                                                         null // null porque no nos pueden dar un hash en este
                                                              // metodo, tendría que ser en el que incluye datos
            );
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
        }

    }

    public byte[] countersign(final byte[] sign,
                              String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              Properties extraParams) throws AOException {

        if (extraParams == null) {
            extraParams = new Properties();
        }
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false")); //$NON-NLS-1$ //$NON-NLS-2$

        if (algorithm.equalsIgnoreCase("RSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;
        }
        else if (algorithm.equalsIgnoreCase("DSA")) { //$NON-NLS-1$
            algorithm = AOSignConstants.SIGN_ALGORITHM_SHA1WITHDSA;
        }

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

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(sign, algorithm, xCerts);

        // Recuperamos la polictica de firma si se indico
        String policyQualifier = null;
        String policyIdentifier = null;
        if (extraParams.containsKey("policyQualifier")) { //$NON-NLS-1$
            policyQualifier = extraParams.getProperty("policyQualifier"); //$NON-NLS-1$
            policyIdentifier = extraParams.getProperty("policyIdentifier"); //$NON-NLS-1$
        }

        // Datos firmados.
        byte[] dataSigned = null;

        // Si la firma que nos introducen es SignedData
        //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        final boolean signedData = new ValidateCADES().isCADESSignedData(sign);
        if (signedData) {
            try {
                // CASO DE FIRMA DE ARBOL
                if (targetType == CounterSignTarget.Tree) {
                    final int[] nodes = {
                        0
                    };

                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Tree,
                                                                   nodes,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyQualifier,
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE HOJAS
                else if (targetType == CounterSignTarget.Leafs) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Leafs,
                                                                   nodes,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyQualifier,
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS
                else if (targetType == CounterSignTarget.Nodes) {
                    int[] nodesID = new int[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        nodesID[i] = ((Integer) targets[i]).intValue();
                    }
                    nodesID = new ReadNodesTree().simplyArray(nodesID);
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Nodes,
                                                                   nodesID,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyQualifier,
                                                                   signingCertificateV2);
                }
                // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
                else if (targetType == CounterSignTarget.Signers) {

                    // clase que lee los nodos de un fichero firmado (p7s, csig,
                    // sig)
                    final String[] signers = new String[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        signers[i] = (String) targets[i];
                    }
                    final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
                    dataSigned =
                            new CAdESCounterSigner().counterSigner(csp,
                                                                   sign,
                                                                   CounterSignTarget.Signers,
                                                                   nodes2,
                                                                   keyEntry,
                                                                   policyIdentifier,
                                                                   policyQualifier,
                                                                   signingCertificateV2);

                }

                return dataSigned;

            }
            catch (final Exception e) {
                throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
            }
        }
        // Signed and enveloped

        try {
            // CASO DE FIRMA DE ARBOL
            if (targetType == CounterSignTarget.Tree) {
                final int[] nodes = {
                    0
                };

                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Tree,
                                                                        nodes,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyQualifier,
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE HOJAS
            else if (targetType == CounterSignTarget.Leafs) {
                final int[] nodes = {
                    0
                };
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Leafs,
                                                                        nodes,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyQualifier,
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE NODOS
            else if (targetType == CounterSignTarget.Nodes) {
                int[] nodesID = new int[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    nodesID[i] = ((Integer) targets[i]).intValue();
                }
                nodesID = new ReadNodesTree().simplyArray(nodesID);
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Nodes,
                                                                        nodesID,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyQualifier,
                                                                        signingCertificateV2);
            }
            // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
            else if (targetType == CounterSignTarget.Signers) {

                // clase que lee los nodos de un fichero firmado (p7s, csig,
                // sig)
                final String[] signers = new String[targets.length];
                for (int i = 0; i < targets.length; i++) {
                    signers[i] = (String) targets[i];
                }
                final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
                dataSigned =
                        new CAdESCounterSignerEnveloped().counterSigner(csp,
                                                                        sign,
                                                                        CounterSignTarget.Signers,
                                                                        nodes2,
                                                                        keyEntry,
                                                                        policyIdentifier,
                                                                        policyQualifier,
                                                                        signingCertificateV2);

            }

            return dataSigned;

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Contrafirma CAdES", e); //$NON-NLS-1$
        }

    }

    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        final ReadNodesTree Rn = new ReadNodesTree();
        try {
            return Rn.readNodesTree(sign, asSimpleSignInfo);
        }
        catch (final Exception ex) {
            LOGGER.severe("No se ha podido obtener el albol de firmantes de la firma, se devolvera null: " + ex); //$NON-NLS-1$
        }
        return null;
    }

    public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return new ValidateCADES().isCADESSignedData(data);
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
        boolean valido = new ValidateCADES().isCADESData(data);
        // Comprobamos si su contenido es de tipo SIGNEDDATA
        if (!valido) {
            valido = new ValidateCADES().isCADESSignedData(data);
        }
        // Comprobamos si su contenido es de tipo DIGESTDATA
        if (!valido) {
            valido = new ValidateCADES().isCADESDigestedData(data);
        }
        // Comprobamos si su contenido es de tipo ENCRYPTEDDATA
        if (!valido) {
            valido = new ValidateCADES().isCADESEncryptedData(data);
        }
        // Comprobamos si su contenido es de tipo ENVELOPEDDATA
        if (!valido) {
            valido = new ValidateCADES().isCADESEnvelopedData(data);
        }
        // Comprobamos si su contenido es de tipo SIGNEDANDENVELOPED
        if (!valido) {
            valido = new ValidateCADES().isCADESSignedAndEnvelopedData(data);
        }
        return valido;
    }

    /** M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se permite la verificaci&oacute;n de los siguientes tipos de firma:
     * <ul>
     * <li>Data</li>
     * <li>Signed Data</li>
     * <li>Digested Data</li>
     * <li>Encrypted Data</li>
     * <li>Enveloped Data</li>
     * <li>Signed and Enveloped Data</li>
     * </ul>
     * @param signData
     *        Datos que se desean comprobar.
     * @param type
     *        Tipo de firma que se quiere verificar.
     * @return La validez del archivo cumpliendo la estructura. */
    public static boolean isCADESValid(final byte[] signData, final String type) {
        if (type.equals(AOSignConstants.CMS_CONTENTTYPE_DATA)) {
            return new ValidateCADES().isCADESData(signData);
        }
        else if (type.equals(AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA)) {
            return new ValidateCADES().isCADESSignedData(signData);
        }
        else if (type.equals(AOSignConstants.CMS_CONTENTTYPE_DIGESTEDDATA)) {
            return new ValidateCADES().isCADESDigestedData(signData);
        }
        else if (type.equals(AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA)) {
            return new ValidateCADES().isCADESEncryptedData(signData);
        }
        else if (type.equals(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)) {
            return new ValidateCADES().isCADESEnvelopedData(signData);
        }
        else if (type.equals(AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)) {
            return new ValidateCADES().isCADESSignedAndEnvelopedData(signData);
        }
        LOGGER.warning("Tipo de contenido CADES no reconocido"); //$NON-NLS-1$
        return false;
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

    public void setDataObjectFormat(final String description, final String objectIdentifier, final String mimeType, final String encoding) {
        // No permitimos el cambio del tipo de dato. CMS/CAdES establece que
        // siempre sera de tipo DATA
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

    public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException, AOException {
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
