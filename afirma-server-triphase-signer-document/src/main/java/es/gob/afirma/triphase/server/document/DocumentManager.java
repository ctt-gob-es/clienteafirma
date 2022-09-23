/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

/** Interfaz para la recuperaci&oacute;n de documentos desde un servidor o repositorio documental.
 * <u>ES OBLIGATORIO</u>, que las clases que implementen esta interfaz dispongan de un constructor que reciba
 * &uacute;nicamente un objeto Properties (<code>java.util.Properties</code>).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface DocumentManager {

	/**
	 * Obtiene un documento en base a su identificador.
	 * Si no es posible recuperar el fichero se debe lanzar una excepci&oacute;n. El mensaje se recibir&aacute;
	 * como parte del mensaje de error en el cliente de firma.
	 * @param dataRef Referencia al documento.
	 * @param certChain Cadena de certificados que se usar&aacute; para realizar la firma
	 * @param prop Par&aacute;metros para la configuraci&oacute;n de la recuperaci&oacute;n del documento.
	 * @return Documento (en binario)
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n.
	 * @throws SecurityException Cuando el documento incumple alguno de los requisitos establecidos para el documento.
	 */
	byte[] getDocument(String dataRef, X509Certificate[] certChain, Properties prop) throws IOException, SecurityException;

	/**
	 * Almacena un documento firmado.
	 * Si no es posible almacenar el fichero se debe lanzar una excepci&oacute;n. El mensaje se recibir&aacute;
	 * como parte del mensaje de error en el cliente de firma.
	 * @param dataRef Referencia al documento firmado.
	 * @param certChain Cadena de certificados de firma.
	 * @param data Datos firmados.
	 * @param prop Par&aacute;metros para la configuraci&oacute;n del guardado del documento.
	 * @return Identificador del nuevo documento codificado en base 64.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con el guardado.
	 */
	String storeDocument(String dataRef, final X509Certificate[] certChain, byte[] data, Properties prop) throws IOException;
}
