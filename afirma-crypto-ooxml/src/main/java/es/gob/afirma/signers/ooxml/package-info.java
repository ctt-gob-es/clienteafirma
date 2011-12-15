/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

/** M&oacute;dulo de generaci&oacute;n de firmas digitales Office Open XML (OOXML),
 *  variante de XMLDSig (XML Digital Signature).
 *  <p>
 *   El m&oacute;dulo genera firmas electr&oacute,nicas en el formato definido en:
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
 *   <li>Microsoft office 2010</li>
 *   <li>Microsoft Office 2011 for Mac</li>
 *  </ul>
 *  <p align="center"><br><img src="doc-files/package-info-1.png"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo de firmas XMLDSig (<i>afirma-crypto-xmlsignature</i>) del Cliente.</li>
 *   <li>Dependencia con la biblioteca Apache Commons IO (<i>commons_io_afirma.jar</i>).</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias de segundo nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.</li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6.0_10 o superior.<br>
 *   Para compatibilidad
 *   con JSE 1.5 es necesario incluir las clases Java contenidas en el "Paquete de compatibilidad
 *   con Java 5" del Ciente e instalar los productos Apache Xalan 2.7.1 o superior y Apache Xerces
 *   2.11.0 o superior como API ENDORSED de Java.<br>
 *   Consulte la p&aacute;gina 
 *   <a href="http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html">http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html</a>
 *   para informaci&oacute;n ampliada sobre los API ENDORSED de Java.
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.<br> 
 *  </p>
 */
package es.gob.afirma.signers.ooxml;