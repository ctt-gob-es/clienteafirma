/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

/**
 *	M&oacute;dulo de generaci&oacute;n de firmas digitales XAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de XAdES:</p>
 *  <p align="center"><table border="1" cellpadding="5">
 *   <tr>
 *    <td>XAdES-BES</td>
 *    <td>XAdES-EPES</dt>
 *    <td>XAdES-T</td>
 *    <td>XAdES-C</td>
 *    <td>XAdES-X</td>
 *    <td>XAdES-XL</td>
 *    <td>XAdES-A</td>
 *   </tr>
 *   <tr>
 *    <td bgcolor="green">Si</td>
 *    <td bgcolor="green">Si</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *   </tr>
 *  </table></p>
 *  <p align="center"><br><img src="doc-files/package-info-1.png"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias directas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo XML (<i>afirma-crypto-core-xml</i>) del Cliente.</li>
 *   <li>Dependencia con JXAdES revisi&oacute;n 5138 o superior.</li>
 *  </ul>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas de primer nivel:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo Núcleo UI JSE (<i>afirma-ui-core-jse</i>) del Cliente.
 *    Este puede sustituirse por cualquier otra clase que implemente el interfaz <code>es.gob.afirma.core.ui.AOUIManager</code>.
 *   </li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6 o superior.<br>
 *   Para compatibilidad
 *   con JSE 1.5 es necesario incluir las clases Java contenidas en el "Paquete de compatibilidad
 *   con Java 5" del Ciente e instalar los productos Apache Xalan 2.7.1 o superior y Apache Xerces
 *   2.11.0 o superior como API ENDORSED de Java.<br>
 *   Consulte la p&aacute;gina 
 *   <a href="http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html">http://docs.oracle.com/javase/1.5.0/docs/guide/standards/index.html</a>
 *   para informaci&oacute;n ampliada sobre los API ENDORSED de Java.
 *  </p>
 *  <p>
 *   Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.<br> 
 *  </p>
 */
package es.gob.afirma.signers.xades;