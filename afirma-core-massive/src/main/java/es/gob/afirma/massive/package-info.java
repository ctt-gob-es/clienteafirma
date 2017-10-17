/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/**
 *	M&oacute;dulo de generaci&oacute;n masiva de firmas electr&oacute;nicas en m&uacute;ltiples formatos.
 *  <p style="text-align: center;"><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo de Utilidades (<i>afirma-util</i>) del Cliente.</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas de primer nivel:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo CAdES Multifirmas (<i>afirma-crypto-cades-multi</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con SpongyCastle 1.54 o superior (Proveedor + TSP + <i>Mail</i>).
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con la biblioteca JMIMEMagic.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo de firmas XMLDSig (<i>afirma-crypto-xmlsignature</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo de firmas XAdES (<i>afirma-crypto-xades</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con la biblioteca JXAdES.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo de firmas CAdES (<i>afirma-crypto-cades</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.
 *   </li>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo N&uacute;cleo UI JSE (<i>afirma-ui-core-jse</i>) del Cliente.
 *   </li>
 *  </ul>
 *  Adicionalmente, el m&oacute;dulo se conecta din&aacute;micamente con cualquier clase de firma declarada en el N&uacute;cleo.
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.massive;