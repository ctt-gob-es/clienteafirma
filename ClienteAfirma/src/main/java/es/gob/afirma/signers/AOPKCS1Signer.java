/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;
import java.util.Properties;
import java.util.logging.Logger;

import javax.activation.MimeType;

import org.ietf.jgss.Oid;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.beans.AOSignInfo;

/**
 * Clase para la firma electr&oacute;nica de ficheros seg&uacute;n la
 * especificaci&oacute;n PKCS#1 v1.5.
 */
public final class AOPKCS1Signer implements AOSigner {

	public byte[] cosign(final byte[] data, final byte[] sign,
			final String algorithm, final PrivateKeyEntry keyEntry,
			final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException(
				"No se pueden hacer cofirmas sin formato (PKCS#1)");
	}

	public byte[] cosign(final byte[] sign, final String algorithm,
			final PrivateKeyEntry keyEntry, final Properties extraParams)
			throws AOException {
		throw new UnsupportedOperationException(
				"No se pueden hacer cofirmas sin formato (PKCS#1)");
	}

	public byte[] countersign(final byte[] sign, final String algorithm,
			final CounterSignTarget targetType, final Object[] targets,
			final PrivateKeyEntry keyEntry, final Properties extraParams)
			throws AOException {
		throw new UnsupportedOperationException(
				"No se pueden hacer contrafirmas sin formato (PKCS#1)");
	}

	public String getSignedName(String originalName, String inText) {
		return "";
	}

	public TreeModel getSignersStructure(byte[] sign, boolean asCertificates) {
		return new TreeModel(new TreeNode("Ra\u00EDz"));
	}

	public boolean isSign(byte[] sign) {
		return false;
	}

	public void setDataObjectFormat(String description, Oid objectIdentifier,
			MimeType mimeType, String encoding) {
	}

	public String getDataMimeType(byte[] sign)
			throws AOUnsupportedSignFormatException {
		return null;
	}

	public byte[] getData(byte[] sign) throws AOInvalidFormatException {
		return null;
	}

	public AOSignInfo getSignInfo(byte[] sign) throws AOInvalidFormatException {
		throw new AOInvalidFormatException(
				"No es posible identificar y recuperar la informacion de una firma PKCS#1");
	}

	public boolean isValidDataFile(byte[] data) {
		if (data == null) {
			Logger.getLogger("es.gob.afirma").warning(
					"Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}

	public byte[] sign(final byte[] data, final String algorithm,
			final PrivateKeyEntry keyEntry, final Properties extraParams)
			throws AOException {

		try {
			Signature s = Signature.getInstance(algorithm);
			s.initSign(keyEntry.getPrivateKey());
			s.update(data);
			return s.sign();
		} catch (final Exception e) {
			throw new AOException(
					"No se ha podido generar la firma PKCS#1 v1.5 de los datos",
					e);
		}
	}
}
