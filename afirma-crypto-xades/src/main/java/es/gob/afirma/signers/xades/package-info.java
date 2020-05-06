/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/**
 *	M&oacute;dulo de generaci&oacute;n de firmas digitales XAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de XAdES:</p>
 *  <table border="1" cellpadding="5" summary="Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de XAdES">
 *   <tr>
 *    <td>XAdES-BES</td>
 *    <td>XAdES-EPES</td>
 *    <td>XAdES-T</td>
 *    <td>XAdES-C</td>
 *    <td>XAdES-X</td>
 *    <td>XAdES-XL</td>
 *    <td>XAdES-A</td>
 *    <td>XAdES-B-LEVEL</td>
 *   </tr>
 *   <tr>
 *    <td style="background-color: green;">Si</td>
 *    <td style="background-color: green;">Si</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *    <td style="background-color: red;">No</td>
 *   </tr>
 *  </table>
 *  <p style="text-align: center;"><br><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.</li>
 *   <li>Dependencia con JXAdES revisi&oacute;n 5138 o superior.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6 o superior.<br>
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.<br>
 *  </p>
 */
package es.gob.afirma.signers.xades;
