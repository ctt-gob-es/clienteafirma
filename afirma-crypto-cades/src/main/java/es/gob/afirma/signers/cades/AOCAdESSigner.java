/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;
import es.gob.afirma.signers.pkcs7.SCChecker;

/** Manejador de firmas binarias CADES.
 * Soporta CAdES-BES, CAdES-EPES, CAdES-T y CAdES B-Level. Implementa los m&eacute;todos declarados
 * en el interfaz <code>AOSigner</code>.
 * <p>Un posible ejemplo de uso ser&iacute;a el siguiente:</p>
 * <pre>
 *
 *   // Establecemos los parametros adicionales
 *   final Properties extraParams = new Properties();
 *   extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_IMPLICIT);
 *   extraParams.setProperty(CAdESExtraParams.POLICY_IDENTIFIER, "urn:oid:2.16.724.1.3.1.1.2.1.8");
 *   extraParams.setProperty(CAdESExtraParams.POLICY_IDENTIFIER_HASH, "V8lVVNGDCPen6VELRD1Ja8HARFk=");
 *   extraParams.setProperty(CAdESExtraParams.POLIY_IDENTIFIER_HAS_HALGORITHM, "urn:oid:1.3.14.3.2.26");
 *
 *   // Usamos un PKCS#12 / PFX para obtener el certificado y su clave privada
 *   final InputStream fis = new FileInputStream("cert.pfx");
 *   KeyStore ks = KeyStore.getInstance("PKCS12");
 *   ks.load(fis, "contrasena".toCharArray());
 *   final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection("contrasena".toCharArray()));
 *   final X509Certificate cert = (X509Certificate) ks.getCertificate("alias");
 *
 *   // Realizamos la firma CAdES
 *   final AOSigner signer = new AOCAdESSigner();
 *   final byte[] firma = signer.sign("Texto a firmar".getBytes(), "SHA256withRSA", pke, extraParams);
 *
 * </pre>
 * @version 0.4 */
public final class AOCAdESSigner implements AOSigner {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Firma datos en formato CAdES.
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificaci&oacute;n.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Firma en formato CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] sign(final byte[] data,
                       final String algorithm,
                       final PrivateKey key,
                       final Certificate[] certChain,
                       final Properties xParams) throws AOException {


    	if (certChain == null || certChain.length < 1) {

    	    throw new IllegalArgumentException("La cadena de certificados debe contener al menos un elemento"); //$NON-NLS-1$
    	}

        final Properties extraParams = getExtraParams(xParams);

    	checkAlgorithm(algorithm, extraParams);

    	new SCChecker().checkSpongyCastle();

    	noticeIncompatibleConfig(algorithm, extraParams);

    	// Determinamos la configuracion de la firma a partir de los datos introducidos,
    	// el algoritmo y los parametros adicionales
        final CAdESParameters cadesConfig = CAdESParameters.load(data, algorithm, extraParams);

        byte[] cadesSignedData;
        try {
			cadesSignedData = GenCAdESEPESSignedData.generateSignedData(
					algorithm,
					key,
					certChain,
					cadesConfig);
        }
        catch (final AOCancelledOperationException e) {
        	throw e;
        }
        catch (final AOException e) {
            throw e;
        }
        catch (final Exception e) {
            throw new AOException("Error al generar la firma CAdES: " + e, e); //$NON-NLS-1$
        }

        // Si corresponde, aplicamos un sello de tiempo
        cadesSignedData = applyTimeStamp(cadesSignedData, extraParams);

        return cadesSignedData;
    }

	/** Cofirma datos en formato CAdES a&ntilde;adiendo la nueva firma a una CAdES o CMS ya existente. Para realizar la
     * cofirma se necesitan los datos originales (que este m&eacute;todo
     * firmar&aacute; normalmente) y la firma sobre la que se realiza la cofirma
     * (a los que se agregar&aacute; el resultado de la nueva firma).
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
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param data Datos que deseamos a cofirmar.
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificaci&oacute;n del certificado de firma.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>).
     * @return Firma CAdES.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso.
     * @throws IOException Si hay problemas en el tratamiento de datos. */
    @Override
	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException, IOException {


    	final Properties extraParams = getExtraParams(xParams);

    	checkAlgorithm(algorithm, extraParams);

    	new SCChecker().checkSpongyCastle();

        try {
			return ((AOCoSigner)Class.forName("es.gob.afirma.signers.multi.cades.AOCAdESCoSigner").getConstructor().newInstance()).cosign( //$NON-NLS-1$
				data,
				sign,
				algorithm,
				key,
				certChain,
				extraParams
			);
		}
        catch (final InstantiationException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalAccessException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por acceso ilegal: " + e, e); //$NON-NLS-1$
		}
        catch (final ClassNotFoundException e) {
        	throw new AOException("No se ha encontrado la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalArgumentException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final InvocationTargetException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por error en la invocacion al constructor: " + e, e); //$NON-NLS-1$
		}
        catch (final NoSuchMethodException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por falta de un constructor por defecto sin parametros: " + e, e); //$NON-NLS-1$
		}
        catch (final SecurityException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por falta de permisos: " + e, e); //$NON-NLS-1$
		}
    }

    /** Cofirma una firma CAdES o CMS existente en formato CAdES. Para realizar la
     * cofirma se necesita el documento en el que se encuentra la firma sobre la
     * que se realiza la cofirma (a los que se agregar&aacute; el resultado de
     * la nueva firma).
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
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificaci&oacute;n del certificado de firma.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKey key,
                         final java.security.cert.Certificate[] certChain,
                         final Properties xParams) throws AOException {

    	final Properties extraParams = getExtraParams(xParams);

    	checkAlgorithm(algorithm, extraParams);

    	new SCChecker().checkSpongyCastle();

        try {
			return ((AOCoSigner)Class.forName("es.gob.afirma.signers.multi.cades.AOCAdESCoSigner").getConstructor().newInstance()).cosign( //$NON-NLS-1$
				sign, algorithm, key, certChain, extraParams
			);
		}
        catch (final IOException e) {
        	throw new AOException("Error en tratamiento de datos para la cofirma: " + e, e); //$NON-NLS-1$
		}
        catch (final InstantiationException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalAccessException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por acceso ilegal: " + e, e); //$NON-NLS-1$
		}
        catch (final ClassNotFoundException e) {
        	throw new AOException("No se ha encontrado la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalArgumentException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES: " + e, e); //$NON-NLS-1$
		}
        catch (final InvocationTargetException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por error en la invocacion al constructor: " + e, e); //$NON-NLS-1$
		}
        catch (final NoSuchMethodException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por falta de un constructor por defecto sin parametros: " + e, e); //$NON-NLS-1$
		}
        catch (final SecurityException e) {
        	throw new AOException("No se ha podido instanciar la clase de cofirmas CAdES por falta de permisos: " + e, e); //$NON-NLS-1$
		}
    }

    /** Contrafirma nodos de firma concretos de una firma electr&oacute;nica.
     * Los nodos que se deben firmar se indican en <code>targetType</code> y
     * pueden ser:
     * <ul>
     *  <li>Todos los nodos del &aacute;rbol de firma (<code>CounterSignTarget.TREE</code>)</li>
     *  <li>Los nodos hoja del &aacute;rbol de firma (<code>CounterSignTarget.LEAFS</code>)</li>
     *  <li>Los nodos de firma cuyas posiciones se especifican en <code>target</code> (<code>CounterSignTarget.NODES</code>)</li>
     *  <li>Los nodos de firma realizados por los firmantes cuyo <i>Common Name</i> (CN X.500) se indica en <code>target</code> (<code>CounterSignTarget.SIGNERS</code>)</li>
     * </ul>
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner</code> en el CLASSPATH</b></p>
     * @param sign Firma CAdES o CMS con los nodos a contrafirmar
     * @param algorithm Algoritmo a usar para la firma.
     * @param targetType Tipo de objetivo de la contrafirma
     * @param targets Informaci&oacute;n complementario seg&uacute;n el tipo de objetivo de la contrafirma
     * @param key Clave privada a usar para firmar.
     * @param certChain Cadena de certificaci&oacute;n del certificado de firma.
     * @param xParams Par&aacute;metros adicionales para la firma (<a href="doc-files/extraparams.html">detalle</a>)
     * @return Contrafirma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    @Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties xParams) throws AOException {

    	final Properties extraParams = getExtraParams(xParams);

    	checkAlgorithm(algorithm, extraParams);

    	new SCChecker().checkSpongyCastle();

        try {
			return ((AOCounterSigner)Class.forName("es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner").getConstructor().newInstance()).countersign( //$NON-NLS-1$
				sign, algorithm, targetType, targets, key, certChain, extraParams
			);
		}
        catch (final IOException e) {
			throw new AOException("Error en el tratamiemto de datos durante la contrafirma: " + e, e); //$NON-NLS-1$
		}
        catch (final InstantiationException e) {
			throw new AOException("Error al instanciar el contrafirmador: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalAccessException e) {
			throw new AOException("No ha permisos para invocar al contrafirmador: " + e, e); //$NON-NLS-1$
		}
        catch (final ClassNotFoundException e) {
			throw new AOException("No se ha encontrado el contrafirmador: " + e, e); //$NON-NLS-1$
		}
        catch (final IllegalArgumentException e) {
        	throw new AOException("No se ha podido instanciar el contrafirmador: " + e, e); //$NON-NLS-1$
		}
        catch (final InvocationTargetException e) {
        	throw new AOException("No se ha podido instanciar el contrafirmador: " + e, e); //$NON-NLS-1$
		}
        catch (final NoSuchMethodException e) {
        	throw new AOException("No se ha podido instanciar el contrafirmador por falta de un constructor por defecto sin parametros: " + e, e); //$NON-NLS-1$
		}
        catch (final SecurityException e) {
        	throw new AOException("No se ha podido instanciar el contrafirmador por motivos de seguridad: " + e, e); //$NON-NLS-1$
		}

    }

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
    	new SCChecker().checkSpongyCastle();
    	if (!CAdESValidator.isCAdESValid(sign, false)) {
    		throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma CAdES"); //$NON-NLS-1$
    	}
    	try {
            return new ReadNodesTree().readNodesTree(sign, asSimpleSignInfo);
        }
        catch (final Exception ex) {
            LOGGER.severe("No se ha podido obtener el arbol de firmantes de la firma, se devolvera null: " + ex); //$NON-NLS-1$
        }
        return null;
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma
     * electr&oacute;nica CAdES.
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el CommonName (CN X.500)
     * del titular del certificado usado para cada firma u objetos de tipo <code>AOSimpleSignInfo</code> con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, dependiendo del
     * valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br>
     * Los propios datos se consideran el nodo ra&iacute;z, las firmas y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param asSimpleSignInfo Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres comunes de los
     *        titulares de los certificados usados para cada firma.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de
     *         error.
     * @throws AOInvalidFormatException Cuando los datos introducidos no son una firma CAdES.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma */
    @Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException, IOException {
    	return getSignersStructure(sign, null, asSimpleSignInfo);
    }

    /** Indica si un dato es una firma compatible con los m&eacute;todos de firma, cofirma y contrafirma de esta clase.
     * @param data Datos que deseamos comprobar.
     * @param params Par&aacute;metros de firma
     * @return <code>true</code> si el dato es una firma reconocida por esta clase (&uacute;nicamente CAdES), <code>false</code> en caso contrario. */
	@Override
	public boolean isSign(final byte[] data, final Properties params){
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        new SCChecker().checkSpongyCastle();
		return CAdESValidator.isCAdESSignedData(data, true);
    }

    /** Indica si un dato es una firma compatible con los m&eacute;todos de firma, cofirma y contrafirma de esta clase.
     * @param data Datos que deseamos comprobar.
     * @return <code>true</code> si el dato es una firma reconocida por esta clase (&uacute;nicamente CAdES), <code>false</code> en caso contrario. */
    @Override
	public boolean isSign(final byte[] data) {
    	return isSign(data, null);
    }

    /** Comprueba si unos datos sos susceptibles de ser firmados por esta clase.
     * Dado que las firmas CAdES pueden firmar cualquier dato binario, el resultado siempre ser&aacute;
     * <code>true</code> excepto si se proporciona <code>null</code>
     * @param data Datos que deseamos comprobar.
     * @return <code>true</code> si el dato es v&aacute;aacute;lido para
     *         firmar, <code>false</code> en caso contrario. */
    @Override
	public boolean isValidDataFile(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    /** Recupera los datos originalmente firmados de la firma proporcionada.
     * En caso de no contener la firma los datos firmados, se devuelve <code>null</code>.
     * @param signData Firma CAdES o CMS.
     * @return Datos originalmente firmados o null si la firma no contiene estos datos.
     * @throws AOInvalidFormatException Si no se ha introducido un fichero de firma v&aacute;lido o no
     *                                  ha podido leerse la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma.
     * @throws IllegalArgumentException Si la firma introducida es nula. */
    @Override
	public byte[] getData(final byte[] signData) throws AOInvalidFormatException, IOException {
        return getData(signData, null);
    }

	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException, IOException {
        if (sign == null) {
            throw new IllegalArgumentException("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
        }
        new SCChecker().checkSpongyCastle();
        if (!CAdESValidator.isCAdESValid(sign, false)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
		return ObtainContentSignedData.obtainData(sign);
    }

    /** Devuelve el nombre de fichero de firma predeterminado que se recomienda usar para
     * un fichero firmado en formato CAdES con nombre original igual al proporcionado.
     * En este caso el resultado ser&aacute; siempre el nombre original con la extensi&oacute;n adicional <i>.csig</i>, m&aacute;s un
     * sufijo adicional (opcional) previo a esta extensi&oacute;n.
     * @param originalName Nombre del fichero original que se firma
     * @param inText Sufijo a agregar al nombre de fichero devuelto, inmediatamente anterior a la extensi&oacute;n <i>.csig</i>
     * @return Nombre apropiado para el fichero de firma. */
    @Override
	public String getSignedName(final String originalName, final String inText) {
        return originalName + (inText != null ? inText : "") + ".csig"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Obtiene la informaci&oacute;n general de un objeto de firma.
     * En este caso la informaci&oacute;n devuelta se limita a un objeto <code>AOSignInfo</code> si se
     * proporciona una firma CAdES.
     * con el formato establecido a <code>AOSignConstants.SIGN_FORMAT_CADES</code>
     * @param signData Firma sobre la que se desea obtener informaci&oacute;n.
     * @return Informaci&oacute;n sobre la firma electr&oacute;nica proporcionada
     * @throws AOInvalidFormatException
     *         Cuando la firma introducida no es un objeto de firma
     *         reconocido por este manejador.
     * @throws AOInvalidFormatException Si los datos proporcionados no se corresponden con una firma CAdES
     * @throws IllegalArgumentException Si La firma introducida es nula. */
    @Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException {
        return getSignInfo(signData, null);
    }

	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOInvalidFormatException {
        if (data == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }
        if (!isSign(data)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_CADES);
    }

    private static void checkAlgorithm(final String algorithm, final Properties extraParams) throws AOException {
    	if (algorithm == null) {
    		throw new IllegalArgumentException("El algoritmo de firma no puede ser nulo"); //$NON-NLS-1$
    	}
    	if (algorithm.toUpperCase(Locale.US).startsWith("MD")) { //$NON-NLS-1$
    		throw new AOException("CAdES no permite huellas digitales MD2 o MD5 (Decision 130/2011 CE)"); //$NON-NLS-1$
    	}

    	final String profile = extraParams.getProperty(CAdESExtraParams.PROFILE);

		// Comprobacion del perfil de firma con la configuracion establecida
		if (AOSignConstants.SIGN_PROFILE_BASELINE.equalsIgnoreCase(profile)) {
			if (AOSignConstants.isSHA1SignatureAlgorithm(algorithm)) {
				LOGGER.warning("El algoritmo '" + algorithm + "' no esta recomendado para su uso " //$NON-NLS-1$ //$NON-NLS-2$
						+ "en las firmas baseline por no cumplir los requisitos de seguridad actuales"); //$NON-NLS-1$
			}
		}
    }

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }

    /**
     * Informa a traves de mensajes de consola si se han establecido par&aacute;metros de
     * configuraci&oacute;n que se ignoraran por ser incompatibles.
     * @param algorithm Algoritmo de firma.
     * @param extraParams Configuracion establecida.
     */
    private static void noticeIncompatibleConfig(final String algorithm, final Properties extraParams) {

        if (extraParams != null && extraParams.containsKey(CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM) &&
        		extraParams.containsKey(CAdESExtraParams.MODE)) {
        	LOGGER.warning("Se ignorara el parametro '" + CAdESExtraParams.MODE + //$NON-NLS-1$
        			"' por haberse proporcionado tambien el parametro '" + CAdESExtraParams.PRECALCULATED_HASH_ALGORITHM + //$NON-NLS-1$
        			"'. La firma sera explicita."); //$NON-NLS-1$
        	extraParams.remove(CAdESExtraParams.MODE);
        }

 	   if (algorithm != null && extraParams != null && AOSignConstants.isSHA2SignatureAlgorithm(algorithm) &&
 			   extraParams.containsKey(CAdESExtraParams.SIGNING_CERTIFICATE_V2)) {
		   LOGGER.warning("Se ignorara la propiedad '" + CAdESExtraParams.SIGNING_CERTIFICATE_V2 + //$NON-NLS-1$
				   "' porque las firmas SHA2 siempre usan SigningCertificateV2"); //$NON-NLS-1$
		   extraParams.remove(CAdESExtraParams.SIGNING_CERTIFICATE_V2);
	   }
    }

    /**
     * Aplica un sello de tiempo a la firma si se ha configurado y si se encuentran
     * las dependencias necesarias para ello.
     * @param signature Firma CAdES.
     * @param extraParams Configuraci&oacute;n de la operaci&oacute;n.
     * @return La firma con el sello de tiempo o, si no se configur&oacute; o no se
     * encontraron las dependencias necesarias, la misma firma que se recibi&oacute;.
     */
    private static byte[] applyTimeStamp(final byte[] signature, final Properties extraParams) {

        // Si encontramos las clases de sello de tiempo, comprobamos si se ha solicitado y
        // se hace en tal caso

        Object tsaParams;
        Class<?> tsaParamsClass;
        try {
        	tsaParamsClass = Class.forName("es.gob.afirma.signers.tsp.pkcs7.TsaParams"); //$NON-NLS-1$
            final Constructor<?> tsaParamsConstructor = tsaParamsClass.getConstructor(Properties.class);
        	tsaParams = tsaParamsConstructor.newInstance(extraParams);
        }
        catch(final ClassNotFoundException e) {
        	LOGGER.info("No se han encontrado las bibliotecas de sello de tiempo, por lo que no se comprobara si se requeria agregar"); //$NON-NLS-1$
        	return signature;
        }
        catch(final Exception e) {
        	// No se configuro la actualizacion de la firma
        	return signature;
        }

        byte[] upgradedSignature = null;
        try {
        	final Class<?> cmsTimestamperClass = Class.forName("es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper"); //$NON-NLS-1$
        	final Constructor<?> cmsTimestamperConstructor = cmsTimestamperClass.getConstructor(tsaParamsClass);
        	final Object cmsTimestamper = cmsTimestamperConstructor.newInstance(tsaParams);

        	final Method getTsaHashAlgorithmMethod = cmsTimestamperClass.getMethod("getTsaHashAlgorithm"); //$NON-NLS-1$
        	final String tsaHashAlgorithm = (String) getTsaHashAlgorithmMethod.invoke(tsaParams);

        	final Method addTimestampMethod = cmsTimestamperClass.getMethod("addTimestamp", //$NON-NLS-1$
        			byte[].class,
        			String.class,
        			GregorianCalendar.class);
        	upgradedSignature = (byte[]) addTimestampMethod.invoke(cmsTimestamper,
        			signature,
        			tsaHashAlgorithm,
        			new GregorianCalendar());
        }
        catch (final Exception e) {
        	LOGGER.log(Level.SEVERE, "No se ha podido aplicar el sello de tiempo", e); //$NON-NLS-1$
        }

        return upgradedSignature != null ? upgradedSignature : signature;
	}

}
