/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/**
 *	M&oacute;dulo de generaci&oacute;n de firmas digitales CAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de CAdES:</p>
 *  <table border="1" cellpadding="5" summary="Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de CAdES">
 *   <tr>
 *    <td>CAdES-BES</td>
 *    <td>CAdES-EPES</td>
 *    <td>CAdES-T</td>
 *    <td>CAdES-C</td>
 *    <td>CAdES-X</td>
 *    <td>CAdES-XL</td>
 *    <td>CAdES-A</td>
 *   </tr>
 *   <tr>
 *    <td style="background-color: green;">Si</td>
 *    <td style="background-color: green;">Si</td>
 *    <td style="background-color: green;">Si<sup>*</sup></td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *   </tr>
 *  </table>
 *  <p>
 *   <sup>*</sup> La generaci&oacute;n de CAdES-T puede realizarse a&ntilde;adiendo un sello de tiempo una vez generada la
 *   firma mediante la clase <code>es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper</code>, perteneciente al m&oacute;dulo PKCS#7 TSP
 *   del Cliente (<i>afirma-crypto-core-pkcs7-tsp</i>), para lo cual se necesita conexi&oacute;n
 *   con una autoridad de sellado de tiempo (TSA).
 *  </p>
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.</li>
 *   <li>Dependencia con SpongyCastle 1.54 o superior (Proveedor + TSP + <i>Mail</i>).</li>
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
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6 o superior y con Android 4 o superior.<br>
 *  </p>
 *  <p>Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.signers.cades;