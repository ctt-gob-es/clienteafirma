/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

/**
 *	M&oacute;dulo de generaci&oacute;n de cofirmas y contrafirmas en formato CAdES.
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo CAdES (<i>afirma-crypto-cades</i>) del Cliente.</li>
 *   <li>Dependencia con BouncyCastle 1.46 o superior (Proveedor + <i>Mail</i>).</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.signers.multi.cades;