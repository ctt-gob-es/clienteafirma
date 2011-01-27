/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.misc;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.xml.crypto.dsig.DigestMethod;

import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;
import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;

/**
 * M&eacute;todos generales de utilidad para criptograf&iacute;a.
 */
public final class AOCryptoUtil {
	
	/**
	 * Obtiene una huella digital. El proveedor utilizado para generar la huella es "SUN y los
	 * algoritmos que acepta:
	 * <ul>
	 * <li>MD2</li>
	 * <li>MD5</li>
	 * <li>SHA-1</li>
	 * <li>SHA-256</li>
	 * <li>SHA-384</li>
	 * <li>SHA-512</li>
	 * </ul>
	 * @param data Datos origen de la huella digital 
	 * @param algorithm Algoritmo a usar para la generaci&oacute;n de la huella digital
	 * @return Huella digital de los datos proporcionados, obtenida mediante el algoritmo indicado
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella digital indicado
	 */
	public static byte[] getMessageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {
		
		final String PROVIDER = "SUN";
		
		MessageDigest md;
		try {
			md = MessageDigest.getInstance(algorithm, PROVIDER);
		}
		catch (NoSuchProviderException e) {
			Logger.getLogger("es.gob.afirma").severe("No se encuentra el proveedor '" + PROVIDER + "': " + e);
			return null;
		}
	    md.update(data);
		return md.digest();
	}
		
	/**
	 * Recupera un manejador de firma v&aacute;lido para el formato especificado. Si el formato
	 * no est&aacute; soportado se devuelve <code>null</code>.
	 * @param format Formato de firma.
	 * @return Manejador de firma especificado. 
	 * @see es.gob.afirma.misc.AOConstants
	 */
	public static AOSigner getSigner(String format) {
		return AOSignerFactory.getInstance().getSigner(format);
	}
	
	/**
	 * Recupera un manejador de firma v&aacute;lido para la firma indicada. Si el formato
	 * no est&aacute; soportado u ocurre alg&uacute;n error se devuelve <code>null</code>.
	 * @param signFile Fichero de firma.
	 * @return Manejador de firma especificado. 
	 * @see es.gob.afirma.misc.AOConstants
	 */
	public static AOSigner getSigner(File signFile) {
		
		int nBytes = 0;
		byte[] buffer = new byte[1024];
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(signFile);
			while((nBytes = fis.read(buffer)) != -1) {
				baos.write(buffer, 0, nBytes);
			}
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
					"No se pudo leer el fichero '" + signFile.getAbsolutePath() + "': " + e
			);
			return null;
		}
		try {
			fis.close();
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
					"No se pudo cerrar el fichero '"+signFile.getAbsolutePath()+"': "+e
			);
		}

		AOSigner signer = getSigner(baos.toByteArray());

		try {
			baos.close();
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el flujo de datos"+e);
		}
		
		return signer;
	}
	
	/**
	 * Recupera un manejador de firma capaz de tratar la firma indicada. En caso de no tener cargado
	 * ning&uacute;n manejador compatible se devolver&aacute; <code>null</code>. 
	 * @param signData Firma electr&oacute;nica
	 * @return Manejador de firma
	 */
	public static AOSigner getSigner(byte[] signData) {
		
		if(signData == null) throw new NullPointerException("No se han indicado datos de firma");
		
		AOSigner result = null;
		// Recorremos los formatos soportados
		for(String signerID : AOSignerFactory.getInstance().getSignersID()) {
			AOSigner signer = AOSignerFactory.getInstance().getSigner(signerID);
			try {
				if(signer.isSign(new ByteArrayInputStream(signData))) {
					result = signer;
					break;
				}
			} catch (Exception e) {
				Logger.getLogger("es.gob.afirma").warning(
					"El manejador "+signer.getClass().getName()+" produjo un error al analizar la informacion de firma: "+e
				);
			}
		}
		return result;
	}
	
	/**
	 * Recupera el manejador de claves asociado a un certificado seg&uacute;n el repositorio en
	 * el que se aloja. 
	 * @param store Almace&eacute;n de claves del certificado.
	 * @return Manejador de claves asociado.
	 */
	public static PasswordCallback getCertificatePC(AOConstants.AOKeyStore store) {
		PasswordCallback pssCallback;
		if(store == AOConstants.AOKeyStore.WINDOWS ||
		   store == AOConstants.AOKeyStore.WINROOT ||
		   store == AOConstants.AOKeyStore.WINADDRESSBOOK ||
		   store == AOConstants.AOKeyStore.WINCA ||
		   store == AOConstants.AOKeyStore.SINGLE ||
		   store == AOConstants.AOKeyStore.MOZILLA ||
		   store == AOConstants.AOKeyStore.MOZ_UNI ||
		   store == AOConstants.AOKeyStore.PKCS11)
			pssCallback = new NullPasswordCallback();
		else 
			pssCallback = new UIPasswordCallback("Contrase\u00F1a del certificado", null);
		
		return pssCallback;
	}

    /**
     * Crea un X509Certificate a partir de un certificado en Base64. 
     * @param b64Cert Certificado en Base64. No debe incluir <i>Bag Attributes</i>
     * @return Certificado X509 o <code>null</code> si no se pudo crear
     */
	public static X509Certificate createCert(String b64Cert) {
		if (b64Cert==null || "".equals(b64Cert)) {
			Logger.getLogger("es.gob.afirma").severe("Se ha proporcionado una cadena nula o vacia, se devolvera null");
			return null;
		}
		X509Certificate cert;		
		try {			 
			InputStream isCert = new ByteArrayInputStream(new BASE64Decoder().decodeBuffer(b64Cert));
			cert = (X509Certificate)CertificateFactory.getInstance("X.509").generateCertificate(isCert);
			isCert.close();
		}
		catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").severe("No se pudo decodificar el certificado en Base64, se devolvera null: " + e);
			return null;
		}
		return cert;
	}
	
	
	/**
	 * Obtiene el nombre de un algoritmo de huella digital a partir de una de las variantes
	 * de este.
	 * @param pseudoName Nombre o variante del nombre del algoritmo de huella digital
	 * @return Nombre del algoritmo de huella digital
	 */
	public static String getDigestAlgorithmName(final String pseudoName) {
		String upperPseudoName = pseudoName.toUpperCase();
		if (upperPseudoName.equals("SHA")   ||
				upperPseudoName.equals(DigestMethod.SHA1.toUpperCase()) ||
				upperPseudoName.startsWith("SHA1")        ||
				upperPseudoName.startsWith("SHA-1"))
				return "SHA1";
		
		if (upperPseudoName.equals(DigestMethod.SHA256.toUpperCase()) ||
				upperPseudoName.startsWith("SHA256")        ||
				upperPseudoName.startsWith("SHA-256"))
			 	return "SHA-256";
		
		if (upperPseudoName.startsWith("SHA384")        ||
				upperPseudoName.startsWith("SHA-384"))
			 	return "SHA-384";
		
		if (upperPseudoName.equals(DigestMethod.SHA512.toUpperCase()) ||
				upperPseudoName.startsWith("SHA512")        ||
				upperPseudoName.startsWith("SHA-512"))
			 	return "SHA-512";
		
		if (upperPseudoName.equals(DigestMethod.RIPEMD160.toUpperCase()) ||
				upperPseudoName.startsWith("RIPEMD160")        ||
				upperPseudoName.startsWith("RIPEMD-160"))
			 	return "RIPEMD160";
		
		if (upperPseudoName.equals("MD5") ||
				upperPseudoName.startsWith("MD5"))
			 	return "MD5";
		
		if (upperPseudoName.equals("MD2") ||
				upperPseudoName.startsWith("MD2"))
			 	return "MD2";
		
		throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + pseudoName);
	}
	

  
	
  /**
   * BASE64Encoder que s&oacute;lo inserta un '\n' al final de cada l&iacute;nea y no el
   * '\r\n' del Base64Encoder de Sun que hace aparecer el '\r' codificado en las firmas XML.
   * Necesario porque java.util.jar hace su propia gesti&oacute;n de l&iacute;neas (ver Manifest.make72Safe()). 
   */
	public static class RawBASE64Encoder extends BASE64Encoder {
		@Override
		protected void encodeLineSuffix(OutputStream aStream) throws IOException {
			pStream.print('\n');
		}
	}
	
//	/**
//	 * Codifica unos datos a base 64. Si ocurre un error durante la lectura de los datos,
//	 * incluyendo desbordamiento de memoria, se devolver&aacute; {@code null}.
//	 * @param dataIs Datos que deseamos transformar.
//	 * @return Cadena en base 64.
//	 * @throws AOException Cuando Ocurre cualquier error.
//	 */
//	public static String getFileBase64Encoded(InputStream dataIs) throws AOException {
//
//		try {
//			final StringBuilder destBuffer = new StringBuilder();
//
//			final byte[] buffer = new byte[16 * 1024];
//			while (dataIs.read(buffer) != -1) {
//				destBuffer.append(new String(BASE64EncoderStream.encode(buffer)));
//			}
//			
//			try { dataIs.close(); } catch (Exception e) {}
//
//			return destBuffer.toString();
//		} catch (Throwable e) {
//			Logger.getLogger("es.gob.afirma").severe("Ocurrio un error durante la transformacion a base 64: "+e);
//			return null;
//		}
//	}
	
	/**
	 * Codifica unos datos a base 64. Si ocurre cualquier error durante la lectura de
	 * los datos, se devolver&aacute; {@code null}.
	 * @param dataIs Datos que deseamos transformar.
	 * @return Cadena en base 64.
	 * @throws AOException Cuando Ocurre cualquier error.
	 */
	public static String getBase64Encoded(InputStream dataIs) throws AOException {

		try {
			return new BASE64Encoder().encode(AOUtil.getDataFromInputStream(dataIs));
		} catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").severe("No se pudo convertir un binario a base 64: "+e);
			return null;
		}
	}
	
//	/**
//	 * Codifica unos datos a base 64. Si ocurre un error durante la lectura de los datos,
//	 * incluyendo desbordamiento de memoria, se devolver&aacute; {@code null}.
//	 * @param dataIs Datos que deseamos transformar.
//	 * @return Cadena en base 64.
//	 * @throws AOException Cuando Ocurre cualquier error.
//	 */
//	public static String getBase64Encoded(InputStream dataIs) throws AOException {
//		
//		// Creamos un fichero temporal en el que almacenaremos la codificacion base 64 de los datos
//		File tempFile;
//		FileOutputStream fos;
//		try {
//			tempFile = File.createTempFile("afirma", null);
//			fos = new FileOutputStream(tempFile);
//		} catch (Exception e) {
//			throw new AOException("No se pudo crear el fichero temporal para la conversion a base 64", e);
//		}
//		
//		int n = 0;
//		byte[] buffer = new byte[4096];
//		BASE64EncoderStream b64es = new BASE64EncoderStream(fos);
//		try {
//			while((n = dataIs.read(buffer)) > 0) {
//				b64es.write(buffer, 0, n);
//			}
//		} catch (Exception e) {
//			throw new AOException("Error durante la conversion a Base 64", e);
//		}
//		
//		try { fos.flush(); } catch (Exception e) {}
//		try { fos.close(); } catch (Exception e) {}
//		try { b64es.close(); } catch (Exception e) {}
//
//		// Leemos el fichero temporal 
//		int length = new Long(tempFile.length()).intValue();
//		FileInputStream fis;
//		MappedByteBuffer in;
//		try {
//			fis = new FileInputStream(tempFile);
//			in = fis.getChannel().map(
//					FileChannel.MapMode.READ_ONLY, 
//					0,
//					length
//			);
//		} catch (Throwable e) {
//			throw new AOException("No se pudo leer el temporal para la transformacion a Base 64", e);
//		}
//		
//		byte[] bufferArray = new byte[length];
//		in.get(bufferArray);
////		// Leemos el contenido en Base64 del fichero temporal
////		StringWriter sw =  new StringWriter(length);
////		for (int i=0;i<length;i++) {
////			sw.write(in.get(i));
////		}
//
//		// Eliminamos el temporal
//		try {
//			fis.close();
//			tempFile.delete();
//		} catch (Exception e) {
//			tempFile.deleteOnExit();
//		}
//		
//		return new String(bufferArray);
//	}

}
