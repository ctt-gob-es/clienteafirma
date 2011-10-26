/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

/**
 *	M&oacute;dulo de generaci&oacute;n de firmas digitales CAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de CAdES:</p>
 *  <p align="center"><table border="1" cellpadding="5">
 *   <tr>
 *    <td>CAdES-BES</td>
 *    <td>CAdES-EPES</dt>
 *    <td>CAdES-T</td>
 *    <td>CAdES-C</td>
 *    <td>CAdES-X</td>
 *    <td>CAdES-XL</td>
 *    <td>CAdES-A</td>
 *   </tr>
 *   <tr>
 *    <td bgcolor="green">Si</td>
 *    <td bgcolor="green">Si</td>
 *    <td bgcolor="green">Si<sup>*</sup></td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *   </tr>
 *  </table></p>
 *  <p>
 *   <sup>*</sup> La generaci&oacute;n de CAdES-T puede realizarse a&ntilde;adiendo un sello de tiempo una vez generada la
 *   firma mediante la clase <code>es.gob.afirma.signers.pkcs7.CMSTimestamper</code>, perteneciente al m&oacute;dulo PKCS#7 
 *   del Cliente (<i>es.gob.afirma.signers.pkcs7</i>), para lo cual se necesita conexi&oacute;n
 *   con una autoridad de sellado de tiempo (TSA).
 *  </p>
 *  <p align="center"><img src="doc-files/package-info-1.png"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo n&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.</li>
 *   <li>Dependencia con BouncyCastle 1.46 o superior (Proveedor + TSP + <i>Mail</i>).</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas de primer nivel:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo CAdES Multifirmas (<i>afirma-crypto-cades-multi</i>) del Cliente.
 *    La presencia de este m&oacute;dulo es opcional, &uacute;nicamente siendo necesaria 
 *    su presencia para la realizaci&oacute;n de contrafirmas o cofirmas CAdES.
 *   </li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior y con Android 3 o superior.<br> 
 *   Para compatibilidad con Android 2.x es necesario sustituir BouncyCastle por SpongyCastle. No hay llamadas
 *   al Proveedor de BouncyCastle por nombre ni instanciaciones din&aacute;micas de clases de BouncyCastle, por
 *   lo que una simple sustituci&oacute;n de <code>import org.bouncycastle.</code> por <code>org.spongycastle.</code>
 *   es suficiente.
 *  </p>
 *  <p>Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.signers.cades;