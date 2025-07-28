/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades.asic;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;

/** Firmador CAdES ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOCAdESASiCSSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

    /** Firma datos en formato CAdES devolviendo el resultado empaquetado como ASiC-S.
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificados del firmante.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams-asic-s.html">detalle</a>).<br>
     *                Adicionalmente, se pueden usar tambi&eacute;n los <a href="doc-files/extraparams.html">par&aacute;metros
     *                definidos para las firmas CAdES normales</a> menos el par&aacute;metro <code>mode</code>, que aunque se
     *                estableca no tendr&aacute; ning&uacute;n efecto, ya que un contenedor ASiC contendra siempre, y de forma
     *                separada, datos y firma.
     * @return Firma en formato CAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si hay problemas en el tratamiento de datos. */
	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException,
			                                            IOException {

		final Properties extraParams = xParams != null ? xParams : new Properties();

		// solo para firmas simples CAdES ASiC-S
		extraParams.put(CAdESExtraParams.MODE, "explicit"); //$NON-NLS-1$

		final byte[] signature = new AOCAdESSigner().sign(data, algorithm, key, certChain, extraParams);

		return ASiCUtil.createSContainer(
			signature,
			data,
			ASiCUtil.ENTRY_NAME_BINARY_SIGNATURE,
			extraParams.getProperty(CAdESASiCExtraParams.ASICS_FILENAME)
		);
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			                               final boolean asSimpleSignInfo) throws AOInvalidFormatException,
			                                                                      IOException {
		return getSignersStructure(sign, null, asSimpleSignInfo);
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params,
											final boolean asSimpleSignInfo) throws AOInvalidFormatException,
																					IOException {
		return new AOCAdESSigner().getSignersStructure(
				ASiCUtil.getASiCSBinarySignature(sign),
				asSimpleSignInfo
			);
	}

	@Override
	public boolean isSign(final byte[] signData, final Properties params) throws IOException {
		final byte[] sign;
		try {
			sign = ASiCUtil.getASiCSBinarySignature(signData);
		}
		catch(final Exception e) {
			return false;
		}
		return new AOCAdESASiCSSigner().isSign(sign);
	}

	@Override
	public boolean isSign(final byte[] is) throws IOException {
		return isSign(is, null);
	}

	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException, IOException {
		// Devolveremos solo los primeros datos que encontremos
		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(sign);
		final String signedDataName = signedData.keySet().iterator().next();
		return signedData.get(signedDataName);
	}

	@Override
	public byte[] getData(final byte[] signData) throws AOInvalidFormatException, IOException {
		return getData(signData, null);
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException, IOException {
		return new AOCAdESSigner().getSignInfo(ASiCUtil.getASiCSBinarySignature(data));
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException, IOException {
		return getSignInfo(signData, null);
	}

    /** Cofirma en formato CAdES los datos encontrados en un contenedor ASiC-S que ya tuviese una firma CAdES o CMS,
     * a&ntilde;adiendo esta nueva firma a la del contenedor (se sustituye la antigua por la el nuevo fichero con todas las firmas).
     * No se pueden cofirmar con esta clase contenedores ASiC-S que ya contengan firmas XML.
     * <p>
     *  Nota sobre cofirmas cruzadas entre PKCS#7/CMS y CAdES:<br>
     *  Las cofirmas de un documento dan como resultado varias firmas a un mismo nivel sobre este mismo documento,
     *  es decir, que ninguna firma envuelve a la otra ni una prevalece sobre la otra.
     *  A nivel de formato interno, esto quiere decir que cuando cofirmamos un documento ya firmado previamente,
     *  esta firma previa no se modifica. Si tenemos en cuenta que CAdES es en realidad un subconjunto de CMS, el
     *  resultado de una cofirma CAdES sobre un documento firmado previamente con CMS (o viceversa), son dos firmas
     *  independientes, una en CAdES y otra en CMS.<br>
     *  Dado que todas las firmas CAdES son CMS pero no todas las firmas CMS son CAdES, el resultado global de la firma
     *  se adec&uacute;a al est&aacute;ndar mas amplio, CMS en este caso.
     *  Otro efecto de compatibilidad de formatos de las cofirmas con varios formatos en un unico documento es la ruptura
     *  de la compatibilidad con PKCS#7, ya que, aunque las firmas generadas por el cliente mediante CMS son compatibles
     *  con PKCS#7, las generadas con CAdES no lo son, por lo que, en el momento que se introduzca una estructura CAdES,
     *  se pierde la compatibilidad PKCS#7 en el global de la firma.
     * </p>
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.asic.AOCAdESASiCSCoSigner</code> en el CLASSPATH</b></p>
     * @param data Se ignora este par&aacute;metro, los datos se cojen del contenedor ASiC-S.
     * @param sign Contenedor ASiC-S (con un fichero de firmas CAdES o CMS).
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512</i></li>
     * </ul>
     * @param key Clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams-asic-s.html">detalle</a>).<br>
     *                    Adicionalmente, se pueden usar tambi&eacute;n los <a href="doc-files/extraparams.html">par&aacute;metros
     *                    definidos para las firmas CAdES normales</a> menos el par&aacute;metro <code>mode</code>, que aunque se
     *                    estableca no tendr&aacute; ning&uacute;n efecto, ya que un contenedor ASiC contendra siempre, y de forma
     *                    separada, datos y firma.
     * @return Contenedor ASiC-S con su fichero de firmas conteniendo la nueva firma.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si hay problemas en el tratamiento de datos. */
	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {
        try {
			return ((AOCoSigner) Class.forName("es.gob.afirma.signers.multi.cades.asic.AOCAdESASiCSCoSigner").getConstructor().newInstance()).cosign( //$NON-NLS-1$
				data,
				sign,
				algorithm,
				key,
				certChain,
				extraParams
			);
		}
        catch (final InstantiationException e) {
        	throw new AOException("Error al instanciar la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalAccessException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final ClassNotFoundException e) {
        	throw new AOException("No se ha encontrado la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalArgumentException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final InvocationTargetException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final NoSuchMethodException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC por falta de un constructor por defecto sin parametros: " + e, e); //$NON-NLS-1$
		}
        catch (final SecurityException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC por motivos de seguridad: " + e, e); //$NON-NLS-1$
		}
	}

    /** Cofirma en formato CAdES los datos encontrador en un contenedor ASiC-S que ya tuviese una firma CAdES o CMS,
     * a&ntilde;adiendo esta nueva firma a la del contenedor (se sustituye la antigua por la el nuevo fichero con todas las firmas).
     * No se pueden cofirmar con esta clase contenedores ASiC-S que ya contengan firmas XML.
     * <p>
     *  Nota sobre cofirmas cruzadas entre PKCS#7/CMS y CAdES:<br>
     *  Las cofirmas de un documento dan como resultado varias firmas a un mismo nivel sobre este mismo documento,
     *  es decir, que ninguna firma envuelve a la otra ni una prevalece sobre la otra.
     *  A nivel de formato interno, esto quiere decir que cuando cofirmamos un documento ya firmado previamente,
     *  esta firma previa no se modifica. Si tenemos en cuenta que CAdES es en realidad un subconjunto de CMS, el
     *  resultado de una cofirma CAdES sobre un documento firmado previamente con CMS (o viceversa), son dos firmas
     *  independientes, una en CAdES y otra en CMS.<br>
     *  Dado que todas las firmas CAdES son CMS pero no todas las firmas CMS son CAdES, el resultado global de la firma
     *  se adec&uacute;a al est&aacute;ndar mas amplio, CMS en este caso.
     *  Otro efecto de compatibilidad de formatos de las cofirmas con varios formatos en un unico documento es la ruptura
     *  de la compatibilidad con PKCS#7, ya que, aunque las firmas generadas por el cliente mediante CMS son compatibles
     *  con PKCS#7, las generadas con CAdES no lo son, por lo que, en el momento que se introduzca una estructura CAdES,
     *  se pierde la compatibilidad PKCS#7 en el global de la firma.
     * </p>
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.asic.AOCAdESASiCSCoSigner</code> en el CLASSPATH</b></p>
     * @param sign Contenedor ASiC-S (con un fichero de firmas CAdES o CMS).
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar
     * @param extraParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams-asic-s.html">detalle</a>).<br>
     *                    Adicionalmente, se pueden usar tambi&eacute;n los <a href="doc-files/extraparams.html">par&aacute;metros
     *                    definidos para las firmas CAdES normales</a> menos el par&aacute;metro <code>mode</code>, que aunque se
     *                    estableca no tendr&aacute; ning&uacute;n efecto, ya que un contenedor ASiC contendra siempre, y de forma
     *                    separada, datos y firma.
     * @return Contenedor ASiC-S con su fichero de firmas conteniendo la nueva firma.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si hay problemas en el tratamiento de datos. */
	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException, IOException {
		return cosign(null, sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties extraParams) throws AOException,
			                                                       IOException {
        try {
			return ((AOCounterSigner) Class.forName("es.gob.afirma.signers.multi.cades.asic.AOCAdESASiCSCounterSigner").getConstructor().newInstance()).countersign( //$NON-NLS-1$
				sign,
				algorithm,
				targetType,
				targets,
				key,
				certChain,
				extraParams
			);
		}
        catch (final InstantiationException e) {
        	throw new AOException("Error al instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalAccessException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final ClassNotFoundException e) {
        	throw new AOException("No se ha encontrado la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalArgumentException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final InvocationTargetException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC: " + e, e); //$NON-NLS-1$
		}
        catch (final NoSuchMethodException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC por falta de un constructor por defecto sin parametros: " + e, e); //$NON-NLS-1$
		}
        catch (final SecurityException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES ASiC por motivos de seguridad: " + e, e); //$NON-NLS-1$
		}
	}


}
