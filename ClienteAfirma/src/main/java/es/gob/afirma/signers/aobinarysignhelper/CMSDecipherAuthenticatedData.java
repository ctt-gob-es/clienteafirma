/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;

/**
 * Clase que extrae el contenido de un fichero en formato AuthenticatedData. de
 * CMS.
 * 
 */
public final class CMSDecipherAuthenticatedData {

	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de
	 * mostrarla directamente al usuario.
	 */
	private SecretKey cipherKey;
	private AOCipherAlgorithm claveMac;

	/**
	 * 
	 * @param cmsData
	 *            Datos del tipo EnvelopedData.
	 * @param keyEntry
	 *            Clave privada del certificado usado para descifrar el
	 *            contenido.
	 * @return El contenido de una firma de tipo authenticatedData.
	 * @throws IOException
	 *             Si ocurre alg&uacute;n problema leyendo o escribiendo los
	 *             datos
	 * @throws CertificateEncodingException
	 *             Si se produce alguna excepci&oacute;n con los certificados de
	 *             firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante el proceso de descifrado
	 *             (formato o clave incorrecto,...)
	 * @throws AOInvalidRecipientException
	 *             Cuando se indica un certificado que no est&aacute; entre los
	 *             destinatarios del sobre.
	 * @throws InvalidKeyException
	 *             Cuando la clave almacenada en el sobre no es v&aacute;lida.
	 */
	public byte[] decipherAuthenticatedData(byte[] cmsData,
			PrivateKeyEntry keyEntry) throws IOException,
			CertificateEncodingException, AOException,
			AOInvalidRecipientException, InvalidKeyException {
		byte[] contenido = new byte[0];

		AuthenticatedData authenticated = null;

		Enumeration<?> elementRecipient;
		try {
			ASN1Sequence authenticatedData = Utils.fetchWrappedData(cmsData);

			authenticated = AuthenticatedData.getInstance(authenticatedData);
			elementRecipient = authenticated.getRecipientInfos().getObjects();
		} catch (final Exception ex) {
			throw new AOException(
					"El fichero no contiene un tipo EnvelopedData", ex);
		}

		X509Certificate userCert = (X509Certificate) keyEntry.getCertificate();
		EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas(
				userCert, elementRecipient);

		// Asignamos la clave de descifrado del contenido.
		assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry,
				encryptedKeyDatas.getAlgEncryptedKey());

		String macAlg = authenticated.getMacAlgorithm().getAlgorithm()
				.toString();
		ASN1Set authAttr = authenticated.getAuthAttrs();

		byte[] macGenerada = null;
		try {
			macGenerada = genMac(macAlg, authAttr.getDEREncoded(),
					this.cipherKey);
		} catch (final InvalidKeyException e) {
			throw e;
		} catch (final Exception e) {
			throw new AOException("Error de codificacion", e);
		}

		byte[] macObtenida = authenticated.getMac().getOctets();

		if (java.util.Arrays.equals(macGenerada, macObtenida)) {
			contenido = ((DEROctetString) authenticated
					.getEncapsulatedContentInfo().getContent()).getOctets();
		}

		return contenido;
	}

	private byte[] genMac(String encryptionAlg, byte[] content,
			SecretKey ciphKey) throws Exception {
		// KeyGenerator kg = KeyGenerator.getInstance("HmacSHA512");
		// SecretKey sk = kg.generateKey();
		final Mac mac = Mac.getInstance(claveMac.getName());
		mac.init(ciphKey);
		return mac.doFinal(content);
	}

	/**
	 * Asigna la clave para firmar el contenido del fichero que queremos
	 * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
	 * p&uacute;blica del usuario que hace la firma.
	 * 
	 * @param passCiphered
	 *            Clave cifrada.
	 * @param keyEntry
	 *            Contrase&ntilde;a que se va a usar para descifrar.
	 * @param algClave
	 *            Algoritmo necesario para crear la clave.
	 * @throws AOException
	 *             Cuando no se pudo descifrar la clave con el certificado de
	 *             usuario.
	 */
	private void assignKey(byte[] passCiphered, PrivateKeyEntry keyEntry,
			AlgorithmIdentifier algClave) throws AOException {

		AOCipherAlgorithm algorithm = null;

		// obtenemos el algoritmo usado para cifrar la pass
		for (AOCipherAlgorithm algo : AOCipherAlgorithm.values()) {
			if (algo.getOid().equals(algClave.getAlgorithm().toString())) {
				algorithm = algo;
				break;
			}
		}

		if (algorithm == null)
			throw new AOException(
					"No se ha podido obtener el algoritmo para cifrar la contrasena");

		claveMac = algorithm;

		// Desembolvemos la clave usada para cifrar el contenido
		// a partir de la clave privada del certificado del usuario.
		try {
			byte[] encrypted = passCiphered;
			// final Cipher cipher2 =
			// Cipher.getInstance("RSA/ECB/PKCS1Padding");
			Cipher cipher = createCipher(keyEntry.getPrivateKey()
					.getAlgorithm());
			cipher.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
			this.cipherKey = (SecretKey) cipher.unwrap(encrypted,
					algorithm.getName(), Cipher.SECRET_KEY);
		} catch (final Exception e) {
			throw new AOException(
					"Error al recuperar la clave de cifrado del sobre autenticado",
					e);
		}
	}

	/**
	 * Crea el cifrador usado para cifrar tanto el fichero como la clave usada
	 * para cifrar dicho fichero.
	 * 
	 * @param algName
	 *            algoritmo utilizado para cifrar.
	 * @return Cifrador.
	 * @throws java.security.NoSuchAlgorithmException
	 * @throws javax.crypto.NoSuchPaddingException
	 */
	private Cipher createCipher(String algName)
			throws NoSuchAlgorithmException, NoSuchPaddingException {
		return Cipher.getInstance(algName);
	}
}
