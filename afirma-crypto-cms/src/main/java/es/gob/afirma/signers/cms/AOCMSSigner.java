/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cms;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;
import es.gob.afirma.signers.pkcs7.SCChecker;

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
    private final Map<String, byte[]> atrib = new HashMap<>();
    private final Map<String, byte[]> uatrib = new HashMap<>();

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** {@inheritDoc} */
    @Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final java.security.cert.Certificate[] certChain,
			           final Properties xParams) throws AOException, IOException {

    	new SCChecker().checkSpongyCastle();

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty(AOCMSExtraParams.PRECALCULATED_HASH_ALGORITHM);

        byte[] messageDigest = null;
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm);

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        final String mode = extraParams.getProperty(AOCMSExtraParams.MODE, AOSignConstants.DEFAULT_SIGN_MODE);

        final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;
        try {
			return new GenSignedData().generateSignedData(
				csp,
			    omitContent,
			    Boolean.parseBoolean(extraParams.getProperty(AOCMSExtraParams.APPLY_SYSTEM_DATE, "true")), //$NON-NLS-1$
			    this.dataType,
			    key,
			    certChain,
			    this.atrib,
			    this.uatrib,
			    messageDigest
		    );
		}
        catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error en el algoritmo de firma: " + e, e); //$NON-NLS-1$
		}
        catch (final CertificateException e) {
        	throw new AOException("Error en el certificado de firma: " + e, e); //$NON-NLS-1$
		}

    }

    /** {@inheritDoc} */
    @Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final java.security.cert.Certificate[] certChain,
			             final Properties xParams) throws AOException, IOException {

    	new SCChecker().checkSpongyCastle();

        final Properties extraParams = xParams != null ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty(AOCMSExtraParams.PRECALCULATED_HASH_ALGORITHM);

        byte[] messageDigest = null;
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm);

        // tipos de datos a firmar.
        if (this.dataType == null) {
            this.dataType = PKCSObjectIdentifiers.data.getId();
        }

        final String mode = extraParams.getProperty(AOCMSExtraParams.MODE, AOSignConstants.DEFAULT_SIGN_MODE);

        final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

        // Si la firma que nos introducen es SignedData
        if (ValidateCMSSignedData.isCMSSignedData(sign)) {
            try {
                return new CoSigner().coSigner(csp, sign, omitContent, this.dataType, key, certChain, this.atrib, this.uatrib, messageDigest);
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma PKCS#7", e); //$NON-NLS-1$
            }
        }
        throw new AOException("Los datos no se corresponden con una firma CMS valida"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final java.security.cert.Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {

    	new SCChecker().checkSpongyCastle();

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
				return new CoSigner().coSigner(
					algorithm,
					(X509Certificate[])certChain,
					sign,
					this.dataType,
					key,
					this.atrib,
					this.uatrib,
					null
				);
			}
            catch (final Exception e) {
            	throw new AOException("Error generando la Cofirma PKCS#7", e); //$NON-NLS-1$
			}
        }
        throw new AOException("Los datos no se corresponden con una firma CMS valida"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties extraParams) throws AOException, IOException {

    	new SCChecker().checkSpongyCastle();

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(sign, algorithm);

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
                    dataSigned = new CounterSigner().counterSigner(
                		csp,
                		sign,
                		CounterSignTarget.TREE,
                		nodes,
                		key,
                		certChain,
                		this.atrib,
                		this.uatrib
            		);
                }
                // CASO DE FIRMA DE HOJAS
                else if (targetType == CounterSignTarget.LEAFS) {
                    final int[] nodes = {
                        0
                    };
                    dataSigned = new CounterSigner().counterSigner(
                		csp,
                		sign,
                		CounterSignTarget.LEAFS,
                		nodes,
                		key,
                		certChain,
                		this.atrib,
                		this.uatrib
            		);
                }
                // CASO DE FIRMA DE NODOS
                else if (targetType == CounterSignTarget.NODES) {
                    int[] nodesID = new int[targets.length];
                    for (int i = 0; i < targets.length; i++) {
                        nodesID[i] = ((Integer) targets[i]).intValue();
                    }
					nodesID = ReadNodesTree.simplyArray(nodesID);
                    dataSigned = new CounterSigner().counterSigner(
                		csp,
                		sign,
                		CounterSignTarget.NODES,
                		nodesID,
                		key,
                		certChain,
                		this.atrib,
                		this.uatrib
            		);
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
                    dataSigned = new CounterSigner().counterSigner(
                		csp,
                		sign,
                		CounterSignTarget.SIGNERS,
                		nodes2,
                		key,
                		certChain,
                		this.atrib,
                		this.uatrib
            		);

                }
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Contrafirma PKCS#7", e); //$NON-NLS-1$
            }
        }
        else {
            throw new AOException("Los datos no se corresponden con una firma CMS valida"); //$NON-NLS-1$
        }

        return dataSigned;
    }
    
    /** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
    	new SCChecker().checkSpongyCastle();
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
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) 
			throws AOInvalidFormatException, IOException{
    	return getSignersStructure(sign, null, asSimpleSignInfo);
    }
    
	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] signData, final Properties params) throws IOException {
        if (signData == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }

        return ValidateCMSSignedData.isCMSSignedData(signData);
    }

    /** {@inheritDoc} */
    @Override
	public boolean isSign(final byte[] signData) throws IOException {
        return isSign(signData, null);
    }

    /** {@inheritDoc} */
    @Override
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
     * @param oid OID del objeto a introducir.
     * @param value Valor asignado */
    public void addSignedAttribute(final String oid, final byte[] value) {
        this.atrib.put(oid, value);
    }

    /** A&ntilde;ade un atributo no firmado al formato de firma seleccionado.
     * @param oid OID del atributo a introducir.
     * @param value Valor asignado */
    public void addUnsignedAttribute(final String oid, final byte[] value) {
        this.uatrib.put(oid, value);
    }
    
    /** {@inheritDoc} */
	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws IOException, AOException {
        if (sign == null) {
            throw new IllegalArgumentException(
        		"Se han introducido datos nulos para su comprobacion" //$NON-NLS-1$
    		);
        }
        if (!ValidateCMSSignedData.isCMSSignedData(sign)) {
            throw new AOInvalidFormatException(
        		"Los datos introducidos no se corresponden con un objeto de firma" //$NON-NLS-1$
    		);
        }
		return ObtainContentSignedData.obtainData(sign);
    }

    /** {@inheritDoc} */
    @Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
        return getData(signData, null);
    }

    /** {@inheritDoc} */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".csig";  //$NON-NLS-1$//$NON-NLS-2$
    }
    
    /** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException, IOException {
        if (data == null) {
            throw new IllegalArgumentException(
        		"No se han introducido datos para analizar" //$NON-NLS-1$
    		);
        }

        if (!isSign(data)) {
            throw new AOInvalidFormatException(
        		"Los datos introducidos no se corresponden con un objeto de firma" //$NON-NLS-1$
    		);
        }

        final AOSignInfo signInfo = new AOSignInfo(AOSignConstants.SIGN_FORMAT_CMS);

        // Aqui podria venir el analisis de la firma buscando alguno de los
        // otros datos de relevancia que se almacenan en el objeto AOSignInfo

        return signInfo;
    }

    /** {@inheritDoc}
     * @throws IOException Si no es posible leer la firma. */
    @Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException, IOException {
        return getSignInfo(signData, null);
    }

}
