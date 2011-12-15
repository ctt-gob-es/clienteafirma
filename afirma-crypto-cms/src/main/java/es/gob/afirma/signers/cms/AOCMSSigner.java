/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.cms;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.GenSignedData;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Manejador de firmas binarias CMS. Par&aacute;metros adicionales aceptados
 * para las operaciones de firma:<br>
 * <dl>
 * <dt>mode</dt>
 * <dd>Modo de firma a usar (Expl&iacute;cita o Impl&iacute;cita)</dd>
 * <dt>applySystemDate</dt>
 * <dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario
 * <dt>precalculatedHashAlgorithm</dt>
 * <dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 * </dl>
 * @version 0.1 */
public final class AOCMSSigner implements AOSigner {

    private String dataType = null;
    private final Map<String, byte[]> atrib = new HashMap<String, byte[]>();
    private final Map<String, byte[]> uatrib = new HashMap<String, byte[]>();
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** {@inheritDoc} */
    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$

        byte[] messageDigest = null;
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$

        try {
            final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;
            return new GenSignedData().generateSignedData(csp,
                                                          omitContent,
                                                          Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true")), //$NON-NLS-1$ //$NON-NLS-2$
                                                          this.dataType,
                                                          keyEntry,
                                                          this.atrib,
                                                          this.uatrib,
                                                          messageDigest);
        }
        catch (final Exception e) {
            throw new AOException("Error generando la firma PKCS#7", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public byte[] cosign(final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$

        byte[] messageDigest = null;
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$

        final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

        // Si la firma que nos introducen es SignedData
        if (ValidateCMSSignedData.isCMSSignedData(sign)) {
            try {
                return new CoSigner().coSigner(csp, sign, omitContent, this.dataType, keyEntry, this.atrib, this.uatrib, messageDigest);
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma PKCS#7", e); //$NON-NLS-1$
            }
        }
        throw new AOException("Los datos no se corresponden con una firma CMS valida"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public byte[] cosign(final byte[] sign, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws AOException {

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        // Si la firma que nos introducen es SignedData
        if (ValidateCMSSignedData.isCMSSignedData(sign)) {
            // Cofirma de la firma usando unicamente el fichero de firmas.
            try {
                // No habra messageDigest porque no nos pueden dar un hash
                // en este metodo, tendria que ser en el que incluye datos
                return new CoSigner().coSigner(algorithm, (X509Certificate[])keyEntry.getCertificateChain(), sign, this.dataType, keyEntry, this.atrib, this.uatrib, null);
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma PKCS#7", e); //$NON-NLS-1$
            }
        }
        throw new AOException("Los datos no se corresponden con una firma CMS valida"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(sign, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        // Datos firmados.
        byte[] dataSigned = null;

        // Si la firma que nos introducen es SignedData

        if (ValidateCMSSignedData.isCMSSignedData(sign)) {
            try {
                // CASO DE FIRMA DE ARBOL
                if (targetType == CounterSignTarget.TREE) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned = new CounterSigner().counterSigner(csp, sign, CounterSignTarget.TREE, nodes, keyEntry, this.dataType, this.atrib, this.uatrib);
                }
                // CASO DE FIRMA DE HOJAS
                else if (targetType == CounterSignTarget.LEAFS) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned = new CounterSigner().counterSigner(csp, sign, CounterSignTarget.LEAFS, nodes, keyEntry, this.dataType, this.atrib, this.uatrib);
                }
                // CASO DE FIRMA DE NODOS
                else if (targetType == CounterSignTarget.NODES) {
                    int[] nodesID = new int[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        nodesID[i] = ((Integer) targets[i]).intValue();
                    }
                    nodesID = new ReadNodesTree().simplyArray(nodesID);
                    dataSigned = new CounterSigner().counterSigner(csp, sign, CounterSignTarget.NODES, nodesID, keyEntry, this.dataType, this.atrib, this.uatrib);
                }
                // CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
                else if (targetType == CounterSignTarget.SIGNERS) {

                    // clase que lee los nodos de un fichero firmado (p7s)
                    final String[] signers = new String[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        signers[i] = (String) targets[i];
                    }
                    final ReadNodesTree rn2 = new ReadNodesTree();
                    final int[] nodes2 = rn2.readNodesFromSigners(signers, sign);
                    dataSigned = new CounterSigner().counterSigner(csp, sign, CounterSignTarget.SIGNERS, nodes2, keyEntry, this.dataType, this.atrib, this.uatrib);

                }
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Contrafirma PKCS#7", e); //$NON-NLS-1$
            }
        } else {
            throw new AOException("Los datos no se corresponden con una firma CMS valida");     //$NON-NLS-1$
        }
        
        return dataSigned;
    }

    /** {@inheritDoc} */
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        final ReadNodesTree rn = new ReadNodesTree();
        try {
            return rn.readNodesTree(sign, asSimpleSignInfo);
        }
        catch (final Exception ex) {
            LOGGER.severe(ex.toString());
        }
        return null;
    }

    /** {@inheritDoc} */
    public boolean isSign(final byte[] signData) {
        if (signData == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        return ValidateCMSSignedData.isCMSSignedData(signData);
    }

    /** {@inheritDoc} */
    public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** A&ntilde;ade un atributo firmado al formato de firma seleccionado. Este
     * formato debe reconocer el OID especificado, siendo el atributo value su
     * valor como cadena de texto.
     * @param oid
     *        Object Identifier. Identificador del objeto a introducir.
     * @param value
     *        Valor asignado */
    public void addSignedAttribute(final String oid, final byte[] value) {
        this.atrib.put(oid, value);
    }

    /** A&ntilde;ade un atributo no firmado al formato de firma seleccionado.
     * @param oid
     *        Object Identifier. Identificador del atributo a introducir.
     * @param value
     *        Valor asignado */
    public void addUnsignedAttribute(final String oid, final byte[] value) {
        this.uatrib.put(oid, value);
    }

    /** {@inheritDoc} */
    public byte[] getData(final byte[] signData) throws AOException {
        if (signData == null) {
            throw new IllegalArgumentException("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
        }
        if (!ValidateCMSSignedData.isCMSSignedData(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new ObtainContentSignedData().obtainData(signData);
    }

    /** {@inheritDoc} */
    public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".csig";  //$NON-NLS-1$//$NON-NLS-2$
    }

    /** {@inheritDoc} */
    public AOSignInfo getSignInfo(final byte[] signData) throws AOException {

        if (signData == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }

        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }

        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_CMS);
        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo

        return signInfo;
    }
}
