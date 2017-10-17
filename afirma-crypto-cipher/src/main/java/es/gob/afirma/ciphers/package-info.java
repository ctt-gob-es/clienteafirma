/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

/**
 *	M&oacute;dulo de generaci&oacute;n de cifrados sim&eacute;tricos. Se soportan los mismos formatos
 *  y algoritmos que la implementaci&oacute;n de JCE subyacente, tanto basados en claves como basados
 *  en contrase&ntilde;as.
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas de primer nivel:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo N&uacute;cleo UI JSE (<i>afirma-ui-core-jse</i>) del Cliente.
 *    Este puede sustituirse por cualquier otra clase que implemente el interfaz <code>es.gob.afirma.core.ui.AOUIManager</code>.
 *   </li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.ciphers;