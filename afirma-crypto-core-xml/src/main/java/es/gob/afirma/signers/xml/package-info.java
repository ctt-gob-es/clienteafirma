/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/**
 *	M&oacute;dulo de utilidad para la generaci&oacute;n de firmas digitales en formatos derivados de XML Digital Signature (XMLDSig).
 *  No provee ninguna funcionalidad directa a los usuarios (ni siquiera genera firmas en formato XMLDSig),
 *  es un m&oacute;dulo de uso interno por parte de otros m&oacute;dulos.
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.</p> */
package es.gob.afirma.signers.xml;