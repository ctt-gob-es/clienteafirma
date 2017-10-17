/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/** M&oacute;dulo de generaci&oacute;n de firmas digitales PAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de PAdES:</p>
 *  <table border="1" cellpadding="5" summary="Tabla de compatibilidad respecto a variantes de PAdES">
 *   <tr>
 *    <td>PAdES-BES</td>
 *    <td>PAdES-EPES</td>
 *    <td>PAdES-T</td>
 *    <td>PAdES-C</td>
 *    <td>PAdES-X</td>
 *    <td>PAdES-XL</td>
 *    <td>PAdES-A</td>
 *   </tr>
 *   <tr>
 *    <td style="background-color: green;">Si<sup>1</sup></td>
 *    <td style="background-color: green;">Si<sup>1</sup></td>
 *    <td style="background-color: green;">Si<sup>1</sup> <sup>2</sup></td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *   </tr>
 *  </table>
 *  <p>
 *   <sup>1</sup> No se soporta la firma ni de ficheros adjuntos a los documentos PDF ni de ficheros empotrados en los documentos PDF.<br>
 *   <sup>2</sup> La generaci&oacute;n de los sellos de tiempo para PAdES-T necesita conexi&oacute;n
 *   con una autoridad de sellado de tiempo (TSA).
 *  </p>
 *  <p>
 *   Por defecto se generan firmas PAdES-BASIC, para lo que se incrusta una firma CAdES en el PDF utilizando
 *   <i>ETSI.CAdES.detached</i> como valor del sub-filtro de la firma.
 *  </p>
 *  <p>Los datos de firma electr&oacute;nica empotrados dentro de la estructura PDF equivalen a una firma CAdES.</p>
 *  <p>
 *   En general, no se soportan documentos PDF cifrados con certificados, con algoritmo AES256 o con cualquier otro medio introducido en
 *   versiones de Adobe Acrobat posteriores a la 9.
 *  </p>
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias ditectas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo CAdES (<i>afirma-crypto-cades</i>) del Cliente.</li>
 *   <li>Dependencia con SpongyCastle 1.54 o superior.</li>
 *   <li>Dependencia con iText 2.1.7<sup>*</sup></li>
 *  </ul>
 *  <p>
 *   <sup>*</sup> No se utilizan versiones m&aacute;s actuales de iText por incompatibilidades de licencias. Las funcionalidades
 *   de firma trif&aacute;sica PAdES requieren una vers&oacute;n modificada de iText 2.1.7 espec&iacute;fica del proyecto Cliente.
 *  </p>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo N&uacute;cleo UI JSE (<i>afirma-ui.core.jse</i>) del Cliente.
 *    Es posible prescindir de este m&oacute;dulo si se restringe el uso de interfaces gr&aacute;ficas desde los
 *    m&eacute;todos de firma.
 *   </li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 6 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.signers.pades;