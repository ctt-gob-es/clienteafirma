/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/** M&oacute;dulo de generaci&oacute;n de firmas digitales Office Open XML (OOXML),
 *  variante de XAdES.
 *  <p>
 *   El m&oacute;dulo genera firmas electr&oacute;nicas en el formato definido en:
 *  </p>
 *  <ul>
 *   <li>Microsoft Office 2007</li>
 *   <li>Microsoft Office 2008 for Mac</li>
 *  </ul>
 *  <p>
 *   Estas firmas son adicionalmente compatibles con otras versiones de Microsoft Office, si bien
 *   no implementan todas las caracter&iacute;sticas soportadas por ellas para mantener la
 *   compatibilidad hacia atr&aacute;s:
 *  </p>
 *  <ul>
 *   <li>Microsoft Office 2010</li>
 *   <li>Microsoft Office 2011 for Mac</li>
 *   <li>Microsoft Office 2013</li>
 *  </ul>
 *  <p style="text-align: center;"><br><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo de firmas XAdES (<i>afirma-crypto-xades</i>) del Cliente.</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias de segundo nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.7.0_45 o superior.
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.<br>
 *  </p>
 */
package es.gob.afirma.signers.ooxml;