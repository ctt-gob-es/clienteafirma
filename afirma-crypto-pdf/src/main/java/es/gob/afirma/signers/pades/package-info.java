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
 *	M&oacute;dulo de generaci&oacute;n de firmas digitales PAdES.
 *  <p>Tabla de compatibilidad respecto a generaci&oacute;n en cliente de variantes de PAdES:</p>
 *  <p align="center"><table border="1" cellpadding="5">
 *   <tr>
 *    <td>PAdES-BES</td>
 *    <td>PAdES-EPES</dt>
 *    <td>PAdES-T</td>
 *    <td>PAdES-C</td>
 *    <td>PAdES-X</td>
 *    <td>PAdES-XL</td>
 *    <td>PAdES-A</td>
 *   </tr>
 *   <tr>
 *    <td bgcolor="green">Si<sup>1</sup></td>
 *    <td bgcolor="green">Si<sup>1</sup></td>
 *    <td bgcolor="green">Si<sup>1</sup> <sup>2</sup></td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *    <td bgcolor="red">No</td>
 *   </tr>
 *  </table></p>
 *  <p>
 *   <sup>1</sup> No se soporta la firma ni de ficheros adjuntos a los documentos PDF ni de ficheros empotrados en los documentos PDF.<br>
 *   <sup>2</sup> La generaci&oacute;n de los sellos de tiempo para PAdES-T necesita conexi&oacute;n
 *   con una autoridad de sellado de tiempo (TSA).
 *  </p>
 *  <p>
 *   No se soporta la generaci&oacite;n del modo PAdES-BASIC (firmas CMS), siempre se incrusta una firma CAdES en el PDF utilizando 
 *   <i>ETSI.CAdES.detached</i> como valor del sub-filtro de la firma.
 *  </p> 
 *  <p>Los datos de firma electr&oacute;nica empotrados dentro de la estructura PDF equivalen a una firma CAdES con Signing Certificate V2.</p>
 *  <p>
 *   En general, no se soportan documentos PDF cifrados con certificados, con algoritmo AES256 o con cualquier otro medio introducido en
 *   versiones de Adobe Acrobat posteriores a la 9.
 *  </p>
 *  <p align="center"><img src="doc-files/package-info-1.png"></p>
 *  <p>Este m&oacute;dulo presenta las siguientes dependencias ditectas de primer nivel:</p>
 *  <ul>
 *   <li>Dependencia con el m&oacute;dulo N&uacute;cleo (<i>afirma-core</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo PKCS#7 (<i>afirma-crypto-core-pkcs7</i>) del Cliente.</li>
 *   <li>Dependencia con el m&oacute;dulo CAdES (<i>afirma-crypto-cades</i>) del Cliente.</li>
 *   <li>Dependencia con BouncyCastle 1.46 o superior (Proveedor + TSP + <i>Mail</i>).</li>
 *   <li>Dependencia con iText 2.1.7<sup>*</sup></li> 
 *  </ul>
 *  <p>
 *   <sup>*</sup> No se utilizan versiones m&aacute;s actuales de iText por incompatibilidades de licencias. Las funcionalidades
 *   de firma trif&aacute;sica PAdES requieren una vers&oacute;n modificada de iText 2.1.7 espec&iacute;fica del proyecto Cliente.
 *  </p>
 *  <p>Adicionalmente, se presentan las siguientes dependencias din&aacute;micas:</p>
 *  <ul>
 *   <li>
 *    Dependencia din&aacute;mica con el m&oacute;dulo N&uacute;cleo UI JSE (<i>afirma-ui.core.jse</i>) del Cliente.
 *    Es posible prescindir de este m&oacute;dulo si se restringe el uso de interfaces gr&aacute;ficas desde los
 *    m&eacute;todos de firma.
 *   </li>
 *  </ul>
 *  <p>
 *   Este m&oacute;dulo es compatible con cualquier entorno JSE 1.5 o superior.
 *  </p>
 *  <p>Desde este m&oacute;dulo es posible que se realicen llamadas a interfaces gr&aacute;ficas.</p>
 */
package es.gob.afirma.signers.pades;