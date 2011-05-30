/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

/**
 *	Contiene clases capaces de realizar firmas digitales en distintos formatos.
 *	<p>
 *		<h4>Variaciones respecto a versiones anteriores:</h4>
 *		<ul>
 *			<li>
 *				<b>Firma XMLDSign y XAdES Detached Expl&iacute;cita</b>
 *				<p>En la versi&oacute;n anterior se daba la posibilidad de realizar firmas detached expl&iacute;citas sobre
 *				documentos disponibles en la propia m&aacute;quina del usuario.
 *				En este caso, no se est&aacute; utilizando el par&aacute;metro <code>URI</code> del elemento <code>
 *				Reference</code> para referenciar los datos que se firman, sino que se crea un nuevo nodo
 *				en el documento de firmas, externo a los elementos <code>Signature</code>, que contiene un
 *				valor codificado en base64 de los datos que se firman. A este nuevo nodo es al que se apunta
 *				mediante el par&aacute;metro <code>URI</code>.</p>
 *				
 *				<p>En la nueva versi&oacute;n no se considera que la referencia que se haga a datos contenidos en 
 *				el propio documento de firmas sea expl&iacute;cita, ya que el mismo elemento <code>Reference</code>
 *				que indica los datos que se firman contiene en el elemento <code>DigestValue</code> el valor
 *				codificado en base64 de los datos firmados. Por lo tanto, el c&aacute;lculo que se hace en la versi&oacute;n
 *				anterior puede resultar redundante.</p>
 *				
 *				<p>La firma detached expl&iacute;cita es posible en esta versi&oacute;n siempre que los datos que se 
 *				firmen sean accesibles de manera externa a trav&eacute;s de un protocolo HTTP. En este caso el par&aacute;metro
 *				<code>URI</code> contendr&aacute; la direcci&oacute;n en la que se encuentra el documento y la firma que se 
 *				genere ser&aacute; directamente sobre los datos del documento y no sobre un valor calculado previamente.</p>
 *				
 *			</li>
 *			<li>
 *				<b>Firma XMLDSign y XAdES Enveloping Expl&iacute;cita</b>
 *				<p>El caso es similiar al descrito en el punto anterior para las firmas detached expl&iacute;citas.
 *				En la versi&oacute;n anterior se crea un valor codificado en base64 de los datos que se firman y que
 *				posteriormente se incrusta en un elemento <code>Object</code> dentro de <code>Signature</code>.
 *				Se crea entonces una referencia hacia este elemento <code>Object</code>. Con esto se est&aacute; creando
 *				una firma enveloping impl&iacute;cita pero sustituyendo los datos originales por su valor codificado en 
 *				base64.</p>
 *				
 *				<p>Como en el caso anterior, se considera que el valor calculado al que se hace referencia es 
 *				redundante por existir ya en el elemento <code>DigestValue</code> de <code>Reference</code>. Adem&aacute;s,
 *				en ning&uacute;n momento se hace referencia a los datos originales.</p>
 *			</li>
 *		</ul>
 *	</p>
 */
package es.gob.afirma.signers;