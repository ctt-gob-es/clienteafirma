package es.gob.afirma.signers.xades.asic;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.crypto.Data;
import javax.xml.crypto.OctetStreamData;
import javax.xml.crypto.URIReference;
import javax.xml.crypto.URIReferenceException;
import javax.xml.crypto.XMLCryptoContext;

import es.gob.afirma.signers.xml.dereference.CustomUriDereferencer;

/**
 * Dereferenciador en el que se pueden incorporar externamente los datos a los
 * que corresponder ciertas referencias. El dereferenciador tratar&aacute; de
 * resolver la URL dentro del XML, como URL externa y, por ultimo, valiendose
 * de las referencias proporcionadas previamente.
 */
public class UriAndDataDereferencer extends CustomUriDereferencer {

	private final Map<String, byte[]> externalDatas;

	/**
	 * Define los datos a los que se corresponden determinadas referencias.
	 * @param externalReferences Referencias externas y sus datos asociados.
	 */
	public UriAndDataDereferencer(final Map<String, byte[]> externalReferences) {

		this.externalDatas = new HashMap<>();
		if (externalReferences != null) {
			for (final String ref : externalReferences.keySet().toArray(new String[0])) {
				 this.externalDatas.put(ref, externalReferences.get(ref));
			}
		}
	}

	@Override
	public Data dereference(final URIReference domRef, final XMLCryptoContext context) throws URIReferenceException {

		// Inicialmente delegamos en la clase padre, que buscara la referencia dentro del XML y
		// despues la tratara como una referencia externa
		try {
			return super.dereference(domRef, context);
		}
		catch (final Exception e) {
			// Comprobamos si la referencia se refiere a uno de los datos ya proporcionado
			final String ref = domRef.getURI();
			final byte[] data = this.externalDatas.get(ref);
			if (data == null) {
				throw new URIReferenceException("No se ha podido dereferenciar la URI ni hay datos asociados a la misma", e); //$NON-NLS-1$
			}

			return new OctetStreamData(new ByteArrayInputStream(data));
		}
	}
}
