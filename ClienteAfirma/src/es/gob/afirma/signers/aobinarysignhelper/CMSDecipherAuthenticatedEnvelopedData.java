/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;

/**
 * Clase que descifra el contenido de un fichero en formato AuthenticatedEnvelopedData.
 * de CMS.
 *
 * Se usa para ello una clave del usuario.
 */
public final class CMSDecipherAuthenticatedEnvelopedData {

	/**
	 * &Eacute;ste m&eacute;todo descifra el contenido de un CMS AuthenticatedEnvelopedData.
	 *
	 * @param cmsData        Datos del tipo AuthenticatedEnvelopedData
	 *                       para obtener los datos cifrados.
	 * @param keyEntry       Clave privada del certificado usado para descifrar el contenido.
	 * @return               El contenido descifrado del EnvelopedData.
	 *
	 * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws java.security.cert.CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
	 * @throws AOInvalidRecipientException Cuando se indica un certificado que no est&aacute; entre los destinatarios del sobre.
	 * @throws InvalidKeyException Cuando la clave almacenada en el sobre no es v&aacute;lida.
	 */
	public byte[] dechiperAuthenticatedEnvelopedData(byte[] cmsData, PrivateKeyEntry keyEntry) throws IOException, CertificateEncodingException, AOException, AOInvalidRecipientException, InvalidKeyException {

		//Contendra el contenido a tratar.
		AuthEnvelopedData authEnvelopedData = null;

		Enumeration<?> elementRecipient;
		try {
			ASN1Sequence contentAuthEnvelopedData = Utils.fetchWrappedData(cmsData);

			authEnvelopedData = AuthEnvelopedData.getInstance(contentAuthEnvelopedData);
			elementRecipient = authEnvelopedData.getRecipientInfos().getObjects();
		} 
		catch(final Exception ex){
			throw new AOException("El fichero no contiene un tipo AuthenticatedEnvelopedData", ex);
		}

		X509Certificate userCert = (X509Certificate) keyEntry.getCertificate();
		EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas(userCert, elementRecipient);

		//Obtenemos el contenido cifrado
		EncryptedContentInfo contenidoCifrado = authEnvelopedData.getAuthEncryptedContentInfo();

		// Obtenemos el algoritmo usado para cifrar la clave generada.
		AlgorithmIdentifier algClave = contenidoCifrado.getContentEncryptionAlgorithm();

		// Asignamos la clave de descifrado del contenido.
		KeyAsigned keyAsigned = Utils.assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry, algClave);

		//Desciframos el contenido.
		final byte[] deciphered;
		byte[] contCifrado = contenidoCifrado.getEncryptedContent().getOctets();
		try {
			deciphered = Utils.deCipherContent(contCifrado, keyAsigned.getConfig(), keyAsigned.getCipherKey());
		} 
		catch (InvalidKeyException ex) {
			throw ex;
		}
		catch (Throwable ex) {
			ex.printStackTrace();
			throw new AOException("Error al descifrar los contenidos del sobre digital", ex);
		}
		return deciphered;
	}
}
