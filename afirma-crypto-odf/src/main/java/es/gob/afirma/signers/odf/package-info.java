/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

/** M&oacute;dulo de generaci&oacute;n de firmas digitales Open Document Format (ODF),
 *  variante de XMLDSig (XML Digital Signature).
 *  <p>
 *   El m&oacute;dulo es compatible con LibreOffice y OpenOffice.org 3.2 y superiores,
 *   pudiendo opcionalmente generar documentos firmados en formato OpenOffice.org 3.1.
 *  </p>
 *  <p style="text-align: center;"><br><img src="doc-files/package-info-1.png" alt="Dependencias de subsistemas"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6.0_10 o superior.<br>
 *   Para compatibilidad
 *   con JSE 1.5 es necesario incluir las clases Java contenidas en el "Paquete de compatibilidad
 *   con Java 5" del Ciente e instalar los productos Apache Xalan 2.7.1 o superior y Apache Xerces
 *   2.11.0 o superior como API ENDORSED de Java. La inclusi&oacute;n de estas clases proporciona
 *   compatibilidad con Java 5, pero no con Java 6 en versiones anteriores a la 1.6.0_10.<br>
 *   Consulte la p&aacute;gina
 *   <a href="http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html">http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html</a>
 *   para informaci&oacute;n ampliada sobre los API ENDORSED de Java.
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.<br>
 *  </p>
 */
package es.gob.afirma.signers.odf;