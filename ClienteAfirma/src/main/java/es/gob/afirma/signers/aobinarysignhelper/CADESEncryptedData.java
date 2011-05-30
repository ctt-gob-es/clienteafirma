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

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.misc.AOCryptoUtil;

/**
 * Clase que implementa firma digital CADES EncryptedData basado en PKCS#7/CMS
 * EncryptedData. La Estructura del mensaje es la siguiente:<br>
 * 
 * <pre>
 * <code>
 * 
 *  id-encryptedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 6 }
 * 
 *  EncryptedData ::= SEQUENCE {
 *        version CMSVersion,
 *        encryptedContentInfo EncryptedContentInfo,
 *        unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
 * 
 * </code>
 * </pre>
 * 
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje EncryptedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */
public final class CADESEncryptedData {

	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de
	 * mostrarla directamente al usuario.
	 */
	private SecretKey cipherKey;

	/**
	 * M&eacute;todo principal que genera la firma de tipo EncryptedData.
	 * 
	 * @param file
	 *            Archivo espec&iacute;fico a cifrar.
	 * @param digAlg
	 *            ALgoritmo para realizar el Digest.
	 * @param config
	 *            Configuraci&oacute;n del algoritmo para firmar.
	 * @param pass
	 *            Cadena que se usará paa cifrar los datos.
	 * @param dataType
	 *            Identifica el tipo del contenido a firmar.
	 * 
	 * @return la firma de tipo EncryptedData.
	 * @throws java.security.NoSuchAlgorithmException
	 *             Si no se soporta alguno de los algoritmos de firma o huella
	 *             digital
	 */
	public byte[] genEncryptedData(InputStream file, String digAlg,
			AOCipherConfig config, String pass, Oid dataType)
			throws NoSuchAlgorithmException {

		byte[] codeFile = createArrayFromFile(file);

		// Asignamos la clave de cifrado
		this.cipherKey = Utils.assignKey(config, pass);

		// Datos previos &uacute;tiles
		String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(digAlg);

		// generamos el contenedor de cifrado
		EncryptedContentInfo encInfo = null;
		try {
			// 3. ENCRIPTEDCONTENTINFO
			encInfo = Utils
					.getEncryptedContentInfo(codeFile, config, cipherKey);
		} catch (Exception ex) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error durante el proceso cifrado: " + ex);
		}

		// 4. ATRIBUTOS
		// obtenemos la lista de certificados
		ASN1Set unprotectedAttrs = null;
		unprotectedAttrs = getAttributeSet(new AttributeTable(
				Utils.initContexExpecific(digestAlgorithm, codeFile, dataType,
						null)));

		// construimos el Enveloped Data y lo devolvemos
		return new ContentInfo(PKCSObjectIdentifiers.encryptedData,
				new EncryptedData(encInfo, unprotectedAttrs)).getDEREncoded();

	}

	/*************************************************************************/
	/**************** Metodos auxiliares de cifrado **************************/
	/*************************************************************************/

	/**
	 * M&eacute;todo que transforma un Inputstream en un array de bytes.
	 * 
	 * @param file
	 *            InputStream a ser transformado.
	 * @return fichero en formato array de bytes.
	 */
	private byte[] createArrayFromFile(InputStream file) {
		ByteArrayOutputStream baos = null;
		try {
			InputStream dataReader = file;

			byte[] buffer = new byte[1024];
			int len;
			baos = new ByteArrayOutputStream();

			while (dataReader.available() != 0) {
				len = dataReader.read(buffer);
				baos.write(buffer, 0, len);
			}

			return baos.toByteArray();
		} catch (Exception ex) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error durante el proceso de lectura del fichero: " + ex);
			return null;
		}
	}
}
