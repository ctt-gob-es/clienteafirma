/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers;

import java.io.InputStream;
import java.security.Signature;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import javax.activation.MimeType;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;

import org.ietf.jgss.Oid;

import es.gob.afirma.beans.AOSignInfo;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;

/**
 * Clase para la firma electr&oacute;nica de ficheros seg&uacute;n la especificaci&oacute;n PKCS#1 v1.5.
 */
public final class AOPKCS1Signer implements AOSigner {

	public byte[] cosign(InputStream file, InputStream signFile,
			String algorithm, PrivateKeyEntry keyEntry,
			X509Certificate cert, Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No se pueden hacer cofirmas sin formato (PKCS#1)");
	}

	public byte[] cosign(InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams)
			throws AOException {
		throw new UnsupportedOperationException("No se pueden hacer cofirmas sin formato (PKCS#1)");
	}

	public byte[] countersign(InputStream signFile, String algorithm,
			CounterSignTarget targetType, Object[] targets,
			PrivateKeyEntry keyEntry, X509Certificate cert, final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No se pueden hacer contrafirmas sin formato (PKCS#1)");
	}

	public String getSignedName(String originalName, String inText) { return ""; }
	public TreeModel getSignersStructure(InputStream sign, boolean asCertificates) { return new DefaultTreeModel(new DefaultMutableTreeNode("Ra\u00EDz")); }
	public boolean isSign(InputStream is) { return false;	}
	public void setDataObjectFormat(String description, Oid objectIdentifier,	MimeType mimeType, String encoding) {}
	public String getDataMimeType(InputStream signData) throws AOUnsupportedSignFormatException {
	    return null;
	}

    public byte[] getData(InputStream signData) throws AOInvalidFormatException {
        return null;
    }

    public AOSignInfo getSignInfo(InputStream signData) throws AOInvalidFormatException {
        throw new AOInvalidFormatException("No es posible identificar y recuperar la informacion de una firma PKCS#1");
    }

	public boolean isValidDataFile(InputStream is) {
		if(is == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}
	
	public byte[] sign(final InputStream file, final String algorithm, final PrivateKeyEntry keyEntry, final X509Certificate cert, final Properties extraParams) throws AOException {
		
		byte[] data = null;
		try {
			data = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e) {
			throw new AOException("No se han podido leer los datos que se deseaban firmar", e);
		}
		
		try {
			Signature s = Signature.getInstance(algorithm);
			s.initSign(keyEntry.getPrivateKey());
			s.update(data);
			return s.sign();
		}
		catch(final Throwable e) {
			throw new AOException("No se ha podido generar la firma PKCS#1 v1.5 de los datos", e);
		}
	}
}
