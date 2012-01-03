/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.ReadNodesTree;

/** Manejador de firmas binarias CADES.
 * Soporta CAdES-BES, CAdES-EPES y CAdES-T. &Uacute;nicamente expone los m&eacute;todos declarados en el interfaz implementado <code>AOSigner</code>.
 * <p>Un posible ejemplo de uso ser&iacute;a el siguiente:</p>
 * <pre>
 * 
 *   // Establecemos los parametros adicionales
 *   final Properties extraParams = new Properties();
 *   extraParams.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);
 *   extraParams.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8");
 *   extraParams.setProperty("policyIdentifierHash", "tSbjbefbEoLcD06K/IR8FtuhhVE=");
 *   extraParams.setProperty("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1");
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
 *   final byte[] firma = signer.sign("Texto a firmar".getBytes(), "SHA1withRSA", pke, extraParams);
 * 
 * </pre>
 * @version 0.3 */
public final class AOCAdESSigner implements AOSigner {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Indica si por defecto se debe insertar el atributo SigningCertificateV2 en la firma. */
    private static final boolean DEFAULT_USE_SIGNING_CERTIFICATE_V2 = true;

    /** Firma datos en formato CAdES.<br/>
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluir&aacute;n dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique 
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible 
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.     
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo de huella digital (a usar para la firma) cuando esta se proporciona precalculada. Cuando se usan modos de firma
     *    <i>expl&iacute;citos</i>, en los que los datos no se incluyen en la firma, es posible trabajar sin proporcionarlos, indicando
     *    &uacute;nicamente su huella digital en el par&aacute;metro <code>data</code> y el algoritmo usado para su c&aacute;lculo.<br>
     *    <b>
     *     Siempre que se de valor a este par&aacute;metro se supondr&aacute; que los datos proporcionados en el par&aacute;metro
     *     <code>data</code> son la huella digital de los datos a firmar, y no los datos a firmar en si. 
     *    </b>
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo 
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    o se hace a <code>false</code> se utilizara la versi&oacute;n 1
     *   </dd>
     * </dl>
     * @return Firma en formato CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] sign(final byte[] data, 
                       final String algorithm, 
                       final PrivateKeyEntry keyEntry, 
                       final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", Boolean.toString(DEFAULT_USE_SIGNING_CERTIFICATE_V2))); //$NON-NLS-1$

        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(data, algorithm, (X509Certificate[]) keyEntry.getCertificateChain());

        try {
            boolean omitContent = false;
            if (mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null) {
                omitContent = true;
            }
                        
			return GenCAdESEPESSignedData.generateSignedData(
                   csp,
                   omitContent,
                   new AdESPolicy(extraParams),
                   signingCertificateV2,
                   keyEntry,
                   messageDigest,
                   Boolean.parseBoolean(extraParams.getProperty("padesMode", "false")) //$NON-NLS-1$ //$NON-NLS-2$
            );

        }
        catch (final Exception e) {
            throw new AOException("Error generando la firma CAdES", e); //$NON-NLS-1$
        }
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
     *  se adec&uacute;a al est&aacute;ndar m‡s amplio, CMS en este caso.
     *  Otro efecto de compatibilidad de formatos de las cofirmas con varios formatos en un œnico documento es la ruptura 
     *  de la compatibilidad con PKCS#7, ya que, aunque las firmas generadas por el cliente mediante CMS son compatibles 
     *  con PKCS#7, las generadas con CAdES no lo son, por lo que, en el momento que se introduzca una estructura CAdES, 
     *  se pierde la compatibilidad PKCS#7 en el global de la firma.
     * </p>
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param data Datos que deseamos a cofirmar.
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams 
     * Par&aacute;metros adicionales para la cofirma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
     * <dl>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluiran dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique 
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible 
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.     
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo de huella digital (a usar para la firma) cuando esta se proporciona precalculada. Cuando se usan modos de firma
     *    <i>expl&iacute;citos</i>, en los que los datos no se incluyen en la firma, es posible trabajar sin proporcionarlos, indicando
     *    &uacute;nicamente su huella digital en el par&aacute;metro <code>data</code> y el algoritmo usado para su c&aacute;lculo.<br>
     *    <b>
     *     Siempre que se de valor a este par&aacute;metro se supondr&aacute; que los datos proporcionados en el par&aacute;metro
     *     <code>data</code> son la huella digital de los datos a firmar, y no los datos a firmar en si. 
     *    </b>
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo 
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    o se hace a <code>false</code> se utilizara la versi&oacute;n 1
     *   </dd>
     * </dl>
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] data, 
                         final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.multi.cades.AOCAdESCoSigner").newInstance()).cosign(data, sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final Exception e) {
            throw new AOException("Error general en la cofirma: " + e, e); //$NON-NLS-1$
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
     *  se adec&uacute;a al est&aacute;ndar m‡s amplio, CMS en este caso.
     *  Otro efecto de compatibilidad de formatos de las cofirmas con varios formatos en un œnico documento es la ruptura 
     *  de la compatibilidad con PKCS#7, ya que, aunque las firmas generadas por el cliente mediante CMS son compatibles 
     *  con PKCS#7, las generadas con CAdES no lo son, por lo que, en el momento que se introduzca una estructura CAdES, 
     *  se pierde la compatibilidad PKCS#7 en el global de la firma.
     * </p>
     * <p><b>IMPORTANTE: Este m&eacute;todo requiere la presencia de <code>es.gob.afirma.signers.multi.cades.AOCAdESCoSigner</code> en el CLASSPATH</b></p>
     * @param sign Firma CAdES o CMS de los datos que se quiere cofirmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams 
     * Par&aacute;metros adicionales para la cofirma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
     * <dl>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluiran dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique 
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible 
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.     
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo 
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    o se hace a <code>false</code> se utilizara la versi&oacute;n 1
     *   </dd>
     * </dl>
     * @return Firma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] cosign(final byte[] sign, 
                         final String algorithm, 
                         final PrivateKeyEntry keyEntry, 
                         final Properties extraParams) throws AOException {
        try {
            return ((AOCoSigner)AOUtil.classForName("es.gob.afirma.signers.multi.cades.AOCAdESCoSigner").newInstance()).cosign(sign, algorithm, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final Exception e) {
            throw new AOException("Error general en la cofirma: " + e, e); //$NON-NLS-1$
        }
    }

    /** Contrafirma nodos de firma concretos de una firma electr&oacute;nica.<br/>
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
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param targetType Tipo de objetivo de la contrafirma
     * @param targets Informaci&oacute;n complementario seg&uacute;n el tipo de objetivo de la contrafirma 
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param extraParams 
     * Par&aacute;metros adicionales para la contrafirma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>extraParams</code>:</p>
     * <dl>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluiran dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique 
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible 
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.     
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo 
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    o se hace a <code>false</code> se utilizara la versi&oacute;n 1
     *   </dd>
     * </dl>
     * @return Contrafirma CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKeyEntry keyEntry,
                              final Properties extraParams) throws AOException {
        try {
            return ((AOCounterSigner)AOUtil.classForName("es.gob.afirma.signers.multi.cades.AOCAdESCounterSigner").newInstance()).countersign(sign, algorithm, targetType, targets, keyEntry, extraParams); //$NON-NLS-1$
        }
        catch(final Exception e) {
            throw new AOException("Error general en la contrafirma: " + e, e); //$NON-NLS-1$
        }
    }

    /** Recupera el &aacute;rbol de nodos de firma de una firma
     * electr&oacute;nica CAdES.
     * Los nodos del &aacute;rbol ser&aacute;n cadena de texto con el CommonName (CN X.500)
     * del titular del certificado usado para cada firma u objetos de tipo <code>AOSimpleSignInfo</code> con la
     * informaci&oacute;n b&aacute;sica de las firmas individuales, dependiendo del
     * valor del par&aacute;metro <code>asSimpleSignInfo</code>. Los nodos se
     * mostrar&aacute;n en el mismo orden y con la misma estructura con el que
     * aparecen en la firma electr&oacute;nica.<br/>
     * Los propios datos se consideran el nodo ra&iacute;z, las firmas y cofirmas
     * pender&aacute;n directamentede de este.
     * @param sign Firma electr&oacute;nica de la que se desea obtener la estructura.
     * @param asSimpleSignInfo Si es <code>true</code> se devuelve un &aacute;rbol con la
     *        informaci&oacute;n b&aacute;sica de cada firma individual
     *        mediante objetos <code>AOSimpleSignInfo</code>, si es <code>false</code> un &aacute;rbol con los nombres comunes de los
     *        titulares de los certificados usados para cada firma.
     * @return &Aacute;rbol de nodos de firma o <code>null</code> en caso de
     *         error. */
    public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) {
        try {
            return new ReadNodesTree().readNodesTree(sign, asSimpleSignInfo);
        }
        catch (final Exception ex) {
            LOGGER.severe("No se ha podido obtener el arbol de firmantes de la firma, se devolvera null: " + ex); //$NON-NLS-1$
        }
        return null;
    }

    /** Indica si un dato es una firma compatible con los m&eacute;todos de firma, cofirma y contrafirma de esta clase.
     * @param data Datos que deseamos comprobar.
     * @return <code>true</code> si el dato es una firma reconocida por esta clase (&uacute;nicamente CAdES), <code>false</code> en caso contrario. */
    public boolean isSign(final byte[] data) {
        if (data == null) {
            LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
		return CAdESValidator.isCAdESSignedData(data);
    }

    /** Comprueba si unos datos sos susceptibles de ser firmados por esta clase.
     * Dado que las firmas CAdES pueden firmar cualquier dato binario, el resultado siempre ser&aacute;
     * <code>true</code> excepto si se proporciona <code>null</code>
     * @param data Datos que deseamos comprobar.
     * @return <code>true</code> si el dato es v&aacute;aacute;lido para
     *         firmar, <code>false</code> en caso contrario. */
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
     * @return Datos originalmente firmados.
     * @throws AOInvalidFormatException
     *         Si no se ha introducido un fichero de firma v&aacute;lido o no
     *         ha podido leerse la firma.
     * @throws AOException Si ocurre cualquier error durante la recuperaci&oacute;n de los
     *         datos.
     * @throws IllegalArgumentException Si la firma introducida es nula. */
    public byte[] getData(final byte[] signData) throws AOInvalidFormatException {
        if (signData == null) {
            throw new IllegalArgumentException("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
        }
        if (!CAdESValidator.isCAdESValid(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
		return ObtainContentSignedData.obtainData(signData);
    }

    /** Devuelve el nombre de fichero de firma predeterminado que se recomienda usar para
     * un fichero firmado en formato CAdES con nombre original igual al proporcionado.
     * En este caso el resultado ser&aacute; siempre el nombre original con la extensi&oacute;n adicional <i>.csig</i>, m&aacute;s un
     * sufijo adicional (opcional) previo a esta extensi&oacute;n.
     * @param originalName Nombre del fichero original que se firma
     * @param inText Sufijo a agregar al nombre de fichero devuelto, inmediatamente anterior a la extensi&oacute;n <i>.csig</i>
     * @return Nombre apropiado para el fichero de firma. */
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
    public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException {
        if (signData == null) {
            throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
        }
        if (!isSign(signData)) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
        }
        return new AOSignInfo(AOSignConstants.SIGN_FORMAT_CADES);
    }
}
