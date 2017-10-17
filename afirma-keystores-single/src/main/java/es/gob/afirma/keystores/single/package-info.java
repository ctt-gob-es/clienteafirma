/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

/** M&oacute;dulo del Cliente Afirma de gesti&oacute;n de almacenes de certificados en formato X.509 (binario puro o Base64) y PKCS#7.<br>
 * Se trata realmente de un proveedor criptogr&aacute;fico JCE que a&ntilde;ade un nuevo tipo de <code>KeyStore</code> con el nombre <code>PKCS7</code>.
 * Este puede ser usado como si fuese cualquier otro <code>KeyStore</code> del sistema.
 *  <p style="text-align: center;"><br><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior.<br>
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.<br>
 *  </p> */
package es.gob.afirma.keystores.single;