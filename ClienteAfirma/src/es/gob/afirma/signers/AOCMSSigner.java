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


import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.tree.TreeModel;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.beans.AOSignInfo;
import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.signers.aobinarysignhelper.CMSAuthenticatedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSAuthenticatedEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSCompressedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSData;
import es.gob.afirma.signers.aobinarysignhelper.CMSDigestedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSEncryptedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CMSSignedAndEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CoSigner;
import es.gob.afirma.signers.aobinarysignhelper.CoSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.CounterSigner;
import es.gob.afirma.signers.aobinarysignhelper.CounterSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.ExtractMimeType;
import es.gob.afirma.signers.aobinarysignhelper.GenSignedData;
import es.gob.afirma.signers.aobinarysignhelper.ObtainContentSignedData;
import es.gob.afirma.signers.aobinarysignhelper.P7ContentSignerParameters;
import es.gob.afirma.signers.aobinarysignhelper.ReadNodesTree;
import es.gob.afirma.signers.aobinarysignhelper.ValidateCMS;

/**
 * Manejador de firmas binarias CMS.
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt>mode</dt>
 *  	<dd>Modo de firma a usar (Expl&iacute;cita o Impl&iacute;cita)</dd>
 *  <dt>applySystemDate</dt>
 *  	<dd><code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma, <code>false</code> en caso contrario
 *  <dt>precalculatedHashAlgorithm</dt>
 *  	<dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 * </dl>
 * @version 0.1
 */
public final class AOCMSSigner implements AOSigner {

	private AOCipherAlgorithm cipherAlgorithm= null;
	private Oid dataType = null;
	private HashMap<Oid, byte[]> atrib = new HashMap<Oid, byte[]>();
	private HashMap<Oid, byte[]> uatrib = new HashMap<Oid, byte[]>();

	public byte[] sign(InputStream file, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {

		if (extraParams == null) extraParams = new Properties();

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		byte[] messageDigest = null;


		if(precalculatedDigest != null) {
			messageDigest = plainData;
		}

		P7ContentSignerParameters csp = new P7ContentSignerParameters(
				plainData,
				algorithm,
				(X509Certificate[]) keyEntry.getCertificateChain()
		);

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} catch (GSSException ex) {
				Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto: " + ex);
			}
		}

		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);

		try {
			boolean omitContent = mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;
			return new GenSignedData().generateSignedData(
					csp, 
					omitContent, 
					Boolean.parseBoolean(extraParams.getProperty("applySystemDate", "true")),
					dataType, 
					keyEntry, 
					atrib, 
					uatrib, 
					messageDigest
			);
		}
		catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").severe("Ocurrio un error generando la firma PKCS#7: " + e);
			throw new AOException("Ocurrio un error generando la firma PKCS#7", e);
		}
	}

	public byte[] cosign(InputStream file, InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {

		if (extraParams == null) extraParams = new Properties();

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} catch (Exception e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		byte[] plainSign;
		try {
			plainSign = AOUtil.getDataFromInputStream(signFile);
		} catch (Exception e1) {
			throw new AOException("No se han podido leer los datos de firma: "+e1);
		}

		byte[] messageDigest = null;
		if(precalculatedDigest != null) {
			messageDigest = plainData; 
		}

		P7ContentSignerParameters csp = new P7ContentSignerParameters(
				plainData,
				algorithm,
				(X509Certificate[]) keyEntry.getCertificateChain());

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (Throwable ex) {
				Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto: " + ex);
			}
		}

		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);

		final boolean omitContent = mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

		//	 	Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainSign));
		if (signedData){
			try {
				return new CoSigner().coSigner(csp,
						new ByteArrayInputStream(plainSign),
						omitContent,
						dataType,
						keyEntry,
						atrib,
						uatrib,
						messageDigest);
			}
			catch (Throwable e) {
				Logger.getLogger("es.gob.afirma").severe(
						"Ocurrio un error generando la Cofirma PKCS#7: " + e
				);
				throw new AOException("Ocurrio un error generando la Cofirma PKCS#7", e);
			}
		}
		//     	Si la firma que nos introducen es SignedAndEnvelopedData
		else{
			try {
				//El parametro omitContent no tiene sentido en un signed and envelopedData.
				return new CoSignerEnveloped().coSigner(csp, 
						new ByteArrayInputStream(plainSign), 
						dataType, 
						keyEntry, 
						atrib, 
						uatrib, 
						messageDigest);
			}
			catch (Throwable e) {
				Logger.getLogger("es.gob.afirma").severe(
						"Ocurrio un error generando la Cofirma PKCS#7: " + e
				);
				throw new AOException("Ocurrio un error generando la Cofirma PKCS#7", e);
			}
		}
	}

	public byte[] cosign(InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (Throwable ex) {
				Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto: " + ex);
			}
		}

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signFile);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		// Algoritmo de firma.
		String typeAlgorithm = algorithm;
		// Array de certificados
		X509Certificate[] aCertificados=(X509Certificate[]) keyEntry.getCertificateChain();

		//     	Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainData));
		if (signedData){
			// Cofirma de la firma usando unicamente el fichero de firmas.
			try {
				return new CoSigner().coSigner(
						typeAlgorithm, 
						aCertificados, 
						new ByteArrayInputStream(plainData), 
						dataType, 
						keyEntry, 
						atrib, 
						uatrib, 
						null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
				);	
			}
			catch (Throwable e) {
				throw new AOException("Ocurrio un error generando la Cofirma PKCS#7: " + e);
			}
		}
		//     	Si la firma que nos introducen es SignedAndEnvelopedData
		else{
			// Cofirma de la firma usando unicamente el fichero de firmas.
			try {
				return new CoSignerEnveloped().coSigner(
						typeAlgorithm, 
						aCertificados, 
						new ByteArrayInputStream(plainData), 
						dataType, 
						keyEntry, 
						atrib, 
						uatrib, 
						null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
				);	
			}
			catch (Throwable e) {
				throw new AOException("Ocurrio un error generando la Cofirma PKCS#7: " + e);
			}
		}

	}

	public byte[] countersign(final InputStream signFile, String algorithm, final CounterSignTarget targetType, final Object[] targets, final PrivateKeyEntry keyEntry, final X509Certificate cert, final Properties extraParams) throws AOException {

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signFile);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		P7ContentSignerParameters csp = new P7ContentSignerParameters(
				plainData,
				algorithm,
				(X509Certificate[]) keyEntry.getCertificateChain()
		);

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} catch (GSSException ex) {
				Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto: " + ex);
			}
		}

		// Datos firmados.
		byte[] dataSigned = null;

		// 	Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainData));
		if (signedData){
			try {
				// CASO DE FIRMA DE ARBOL
				if(targetType == CounterSignTarget.Tree){
					int[] nodes = {0};
					dataSigned= new CounterSigner().counterSigner(csp,plainData,CounterSignTarget.Tree,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE HOJAS
				else if(targetType == CounterSignTarget.Leafs){
					int[] nodes={0};
					dataSigned= new CounterSigner().counterSigner(csp,plainData,CounterSignTarget.Leafs,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS
				else if(targetType == CounterSignTarget.Nodes){
					int[] nodesID = new int[targets.length];
					for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
					nodesID = new ReadNodesTree().simplyArray(nodesID);
					dataSigned= new CounterSigner().counterSigner(csp, plainData, CounterSignTarget.Nodes, nodesID, keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
				else if(targetType == CounterSignTarget.Signers){

					//clase que lee los nodos de un fichero firmado (p7s)
					String[] signers = new String[targets.length];
					for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
					ReadNodesTree rn2 = new ReadNodesTree();
					int[] nodes2 = rn2.readNodesFromSigners(signers, plainData);
					dataSigned= new CounterSigner().counterSigner(csp,plainData,CounterSignTarget.Signers,nodes2,keyEntry,dataType,atrib,uatrib);

				}
			} 
			catch (Throwable e) {
				throw new AOException("Ocurrio un error generando la Contrafirma PKCS#7", e);
			}
		}
		// Si la firma es SignedAndEnveloped
		else{

			try {
				// CASO DE FIRMA DE ARBOL
				if(targetType == CounterSignTarget.Tree){
					int[] nodes = {0};
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp,plainData,CounterSignTarget.Tree,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE HOJAS
				else if(targetType == CounterSignTarget.Leafs){
					int[] nodes={0};
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp,plainData,CounterSignTarget.Leafs,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS
				else if(targetType == CounterSignTarget.Nodes){
					int[] nodesID = new int[targets.length];
					for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
					nodesID = new ReadNodesTree().simplyArray(nodesID);
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp, plainData, CounterSignTarget.Nodes, nodesID, keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
				else if(targetType == CounterSignTarget.Signers){

					//clase que lee los nodos de un fichero firmado (p7s)
					String[] signers = new String[targets.length];
					for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
					ReadNodesTree rn2 = new ReadNodesTree();
					int[] nodes2 = rn2.readNodesFromSigners(signers, plainData);
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp,plainData,CounterSignTarget.Signers,nodes2,keyEntry,dataType,atrib,uatrib);

				}
			} 
			catch (Throwable e) {
				throw new AOException("Ocurrio un error generando la Contrafirma PKCS#7", e);
			}

		}

		return dataSigned;
	}

	/**
	 * Construye un envoltorio CMS/PKCS#7 con uno de los siguientes tipos de contenidos:
	 * <ul>
	 * <li>Data</li>
	 * <li>Digested Data</li>
	 * <li>Enveloped Data</li>
	 * <li>Signed and Enveloped Data</li>
	 * <li>Signed and Enveloped Data</li>
	 * <li>Authenticated Data</li>
	 * <li>Authenticated and Enveloped Data</li>
	 * </ul>
	 * En el caso de sobres digitales ("Envelop Data" y "Signed and Envelop Data") el algoritmo de cifrado
	 * puede establecerse mediante <code>setCipherAlgorithm(AOCipherAlgorithm)</code>, siendo por defecto AES.
	 * 
	 * @param file                Flujo de lectura de los datos a envolver.
	 * @param signatureAlgorithm  Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
	 * @param type                Tipo de contenido que almacenar&aacute; la estructura.
	 * @param keyEntry            Clave privada para firmar.
	 * @param certsDest           Certificados de los usuarios a los que va destinado el sobre digital.
	 * @return                    Envoltorio CMS/PKCS#7.
	 * @throws AOException  			Cuando ocurre cualquier problema durante el proceso.
	 */
	public byte[] envelop(InputStream file, String signatureAlgorithm, String type, PrivateKeyEntry keyEntry, X509Certificate[] certsDest) throws AOException {

		//Comprobamos que el archivo a tratar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a tratar no puede ser nulo.");
		}

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		P7ContentSignerParameters csp = null;
		if (keyEntry!=null){
			csp = new P7ContentSignerParameters(
					plainData,
					signatureAlgorithm,
					(X509Certificate[]) keyEntry.getCertificateChain());
		}

		// Tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (Throwable ex) {
				//Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}

		// Datos firmados.
		byte[] dataSigned = null;

		//Seleccion del algoritmo de cifrado.
		AOAlgorithmConfig config=null;
		if (this.cipherAlgorithm == null){
			// Por defecto usamos el AES.
			config = new AOAlgorithmConfig(
					AOCipherAlgorithm.AES,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}
		// En caso de usar un algoritmo de cifrado, si no funciona es porque el Provider no lo soporta.
		else{
			config = new AOAlgorithmConfig(
					this.cipherAlgorithm,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}

		try {
			// Busqueda del tipo que nos han solicitado.
			if ((type== null) || (type.equals(""))) type = AOConstants.DEFAULT_BINARY_ENVELOP;

			// Es Data.
			if (type.equals(AOConstants.BINARY_ENVELOP_DATA)){
				dataSigned = new CMSData().genData(csp);
			}
			//Es Digested Data.
			else if(type.equals(AOConstants.BINARY_ENVELOP_DIGESTEDDATA)){
				dataSigned = new CMSDigestedData().genDigestedData(csp, dataType);
			}
			// Es Enveloped Data.
			else if(type.equals(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA)){
				// compruebo que el keyEntry no sea vacio.
				if (keyEntry != null){
					dataSigned = new CMSEnvelopedData().genEnvelopedData(csp, config, certsDest, dataType,uatrib);
				}
				else{
					dataSigned = new CMSEnvelopedData().genEnvelopedData(
							new ByteArrayInputStream(plainData), 
							signatureAlgorithm, 
							config, 
							certsDest, 
							dataType,
							uatrib
					);
				}
			}
			else if (type.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDDATA)) {
				try {
					this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
				} 
				catch (final GSSException ex) {
					Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto del signedData: " + ex);
					throw new AOException("Ocurrio un error al asignar el OID por defecto del signedData", ex);
				}

				dataSigned = authenticatedSign(
						new ByteArrayInputStream(plainData),
						signatureAlgorithm,
						dataType,
						keyEntry,
						certsDest
				);

			}
			else if (type.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDENVELOPEDDATA)) {
				try {
					this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
				} 
				catch (final GSSException ex) {
					Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto del signedData: " + ex);
					throw new AOException("Ocurrio un error al asignar el OID por defecto del signedData", ex);
				}

				dataSigned = authenticatedEnvelopedSign(
						new ByteArrayInputStream(plainData),
						signatureAlgorithm,
						dataType,
						keyEntry,
						certsDest
				);

			}
			else if (type.equals(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA)) {
				try {
					this.dataType = new Oid(PKCSObjectIdentifiers.signedData.getId());
				} catch (GSSException ex) {
					Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto del signedData: " + ex);
					throw new AOException("Ocurrio un error al asignar el OID por defecto del signedData", ex);
				}

				dataSigned = new CMSSignedAndEnvelopedData().genSignedAndEnvelopedData(csp, config, certsDest, dataType, keyEntry, atrib, uatrib);
			}
			else {
				throw new AOException("Tipo de operacion CMS desconocida: " + type);
			}
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error generando el enveloped de CMS: " + e);
			throw new AOException("Ocurrio un error generando el enveloped de CMS", e);
		}
		return dataSigned;
	}

	/**
	 * Cifra un contenido (t&iacute;picamente un fichero) usando para ello una contrase&ntilde;a.<br/>
	 * Los algoritmos y modos de firma disponibles se declaran en {@link AOConstants}.<br/>
	 * Se usar&aacute; por defecto el algoritmo de cifrado "AES".
	 *
	 * La clave usada para cifrar el contenido puede ser tanto un password como una clave
	 * privada del usuario codificada.
	 *
	 * En el caso de que sea una clave codificada en base 64, se usar&aacute; como algoritmos los tipo AES, DES ...
	 * En el caso de que sea un password, se usar&aacute; un algoritmo de tipo PBE.
	 *
	 * Nota: El par&aacute;metro algorithm no es el agoritmo de cifrado,
	 * es para el digestAlgorithm usado en los "Unsigned Attributes".
	 *
	 * @param file              Flujo de lectura de los datos a firmar
	 * @param digestAlgorithm   Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
	 * @param key               Puede ser una clave codificada o una contrase&ntilde;a
	 *                          usada para cifrar el contenido.
	 * @return                  Contenido firmado
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public byte[] encrypt(InputStream file, String digestAlgorithm, String key) throws AOException {

		//Comprobamos que el archivo a cifrar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a cifrar no puede ser nulo.");
		}

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} catch (GSSException ex) {
				Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}

		// Datos firmados.
		byte[] dataSigned = null;

		//Seleccion del algoritmo de cifrado.
		AOAlgorithmConfig config=null;
		if (this.cipherAlgorithm == null){
			// Por defecto usamos el AES.
			config = new AOAlgorithmConfig(
					AOCipherAlgorithm.AES,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}
		/* En caso de usar un algoritmo de cifrado, si no funciona es porque el
           Provider no lo soporta. */
		else{
			config = new AOAlgorithmConfig(
					this.cipherAlgorithm,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}

		try{
			dataSigned = new CMSEncryptedData().genEncryptedData(file, digestAlgorithm, config, key, dataType, uatrib);
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error generando el Encrypted Data de CMS: " + e);
			throw new AOException("Ocurrio un error generando el Encryted Data de CMS", e);
		}

		return dataSigned;

	}

	/**
	 * 
	 * @param file              Flujo de lectura de los datos a comprimir
	 * @return					Contenido comprimido.
	 * @throws AOException		Cuando ocurre cualquier problema durante el proceso
	 */
	public byte[] compressedData(InputStream file) throws AOException{

		byte[] result = null;

		try{
			result = new CMSCompressedData().genCompressedData(file); 
		}
		catch (Exception e){
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error generando el tipo CompressedData: " + e);
			throw new AOException("Ocurrio un error generando el CompressedData", e);
		}
		return result;

	}

	private byte[] authenticatedSign(InputStream file, String signatureAlgorithm, Oid type, PrivateKeyEntry keyEntry, X509Certificate[] certsDest) throws AOException {

		//Comprobamos que el archivo a tratar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a tratar no puede ser nulo.");
		}

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		P7ContentSignerParameters csp = null;
		if (keyEntry!=null){
			csp = new P7ContentSignerParameters(
					plainData,
					signatureAlgorithm,
					(X509Certificate[]) keyEntry.getCertificateChain());
		}

		// Tipos de datos a firmar.
		if (type==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
				type = this.dataType;
			} 
			catch (Throwable ex) {
				//Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}

		//Seleccion del algoritmo de cifrado.
		AOAlgorithmConfig config=null;
		if (this.cipherAlgorithm == null){
			// Por defecto usamos el HMACSHA512.
			config = new AOAlgorithmConfig(
					AOCipherAlgorithm.HMACSHA512,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}
		// En caso de usar un algoritmo de cifrado, si no funciona es porque el Provider no lo soporta.
		else{
			config = new AOAlgorithmConfig(
					this.cipherAlgorithm,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}

		byte[] messageDigest = null;

		try {
			return new CMSAuthenticatedData().genAuthenticatedData(
					csp, 			//ContentSignerParameters parameters, 
					config,			// Configuracion del cipher
					certsDest, 		//certificados destino
					type, 			//Oid dataType
					true,			//boolean applyTimestamp,
					atrib, 			//HashMap<Oid, byte[]> atrib,
					uatrib, 		//HashMap<Oid,byte[]> uatrib,
					messageDigest	//byte[] messageDigest
			);
		}
		catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").severe("Ocurrio un error generando la firma PKCS#7: " + e);
			throw new AOException("Ocurrio un error generando la firma PKCS#7", e);
		}
	}

	private byte[] authenticatedEnvelopedSign(InputStream file, String signatureAlgorithm, Oid type, PrivateKeyEntry keyEntry, X509Certificate[] certsDest) throws AOException {

		//Comprobamos que el archivo a tratar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a tratar no puede ser nulo.");
		}

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		P7ContentSignerParameters csp = null;
		if (keyEntry!=null){
			csp = new P7ContentSignerParameters(
					plainData,
					signatureAlgorithm,
					(X509Certificate[]) keyEntry.getCertificateChain());
		}

		// Tipos de datos a firmar.
		if (type==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
				type = this.dataType;
			} 
			catch (Throwable ex) {
				//Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}

		//Seleccion del algoritmo de cifrado.
		AOAlgorithmConfig config=null;
		if (this.cipherAlgorithm == null){
			// Por defecto usamos el HMACSHA512.
			config = new AOAlgorithmConfig(
					AOCipherAlgorithm.AES,
					AOCipherBlockMode.CBC,
					AOCipherPadding.ISO10126PADDING
			);
		}
		// En caso de usar un algoritmo de cifrado, si no funciona es porque el Provider no lo soporta.
		else{
			config = new AOAlgorithmConfig(
					this.cipherAlgorithm,
					AOCipherBlockMode.CBC,
					AOCipherPadding.PKCS5PADDING
			);
		}

		try {
			return new CMSAuthenticatedEnvelopedData().genAuthenticatedEnvelopedData(
					csp, 			//ContentSignerParameters parameters, 
					config,			// Configuracion del cipher
					certsDest, 		//certificados destino
					type, 			//Oid dataType
					true,			//boolean applyTimestamp,
					atrib, 			//HashMap<Oid, byte[]> atrib,
					uatrib 		//HashMap<Oid,byte[]> uatrib,
			);
		}
		catch (Throwable e) {
			Logger.getLogger("es.gob.afirma").severe("Ocurrio un error generando la firma PKCS#7: " + e);
			throw new AOException("Ocurrio un error generando la firma PKCS#7", e);
		}
	}
	
	/**
	 * Inserta un nuevo firmante dentro de una firma signedAndEnveloped dada.
	 * 
	 * @param signFile				Flujo de entrada de datos que contiene la firma.
	 * @param file					Fichero de firma, necesario para calcular los datos del nuevo firmante.
	 * @param signatureAlgorithm	Algoritmo de firma.
	 * @param keyEntry				Clave privada a usar para firmar.
	 * @return
	 * @throws AOException			Cuando ocurre cualquier problema durante el proceso
	 */
	public byte[] addOriginatorInfo(InputStream signFile, InputStream file, String signatureAlgorithm, PrivateKeyEntry keyEntry ) throws AOException {

		//Comprobamos que el archivo a tratar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a tratar no puede ser nulo.");
		}

		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}

		P7ContentSignerParameters csp = null;
		if (keyEntry!=null){
			csp = new P7ContentSignerParameters(
					plainData,
					signatureAlgorithm,
					(X509Certificate[]) keyEntry.getCertificateChain());
		}

		// Tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (Throwable ex) {
				//Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Ocurrio un error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}

		// Datos firmados.
		byte[] dataSigned = null;

		try {
			
			dataSigned = new CMSSignedAndEnvelopedData().addOriginatorInfo(signFile, csp, keyEntry, dataType, atrib, uatrib);
			
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error generando el enveloped de CMS: " + e);
			throw new AOException("Ocurrio un error generando el enveloped de CMS", e);
		}
		return dataSigned;
	}


	public TreeModel getSignersStructure(InputStream sign, boolean asSimpleSignInfo){

		TreeModel treeModel = null;

		BufferedInputStream bufin = new BufferedInputStream(sign);
		byte[] buffer = new byte[1024];
		int len;
		ByteArrayOutputStream baos = new ByteArrayOutputStream();

		try {
			while (bufin.available() != 0) {
				len = bufin.read(buffer);
				baos.write(buffer, 0, len);
			}
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning (
					"Error al leer los datos a firmar: " + e 
			);
			return null;
		}

		//clase que lee los nodos de un fichero firmado
		ReadNodesTree Rn = new ReadNodesTree();
		try {
			treeModel = Rn.readNodesTree(baos.toByteArray(), asSimpleSignInfo);
		} catch (Throwable ex) {
			Logger.getLogger("es.gob.afirma").severe(ex.toString());
		}
		return treeModel;
	}

	public boolean isSign(InputStream file) {
		if(file == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		// Se leen los datos y se meten en un array.
		// De esta manera, al ser leidos por el inpustream
		// no se pierden los datos originales
		byte[] datos= null;
		try{
			datos = AOUtil.getDataFromInputStream(file);
		}
		catch(Exception e){}

		// Comprobamos la validez
		boolean signed = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(datos));
		if(!signed){
			signed = new ValidateCMS().isCMSSignedAndEnvelopedData(new ByteArrayInputStream(datos));
		}

		return signed;
	}

	public boolean isValidDataFile(InputStream file) {
		if(file == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}

	/**
	 * M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
	 * Se realiza la verificaci&ocute;n sobre los los siguientes tipos de PKCS#7 reconocidos:
	 * <ul>
	 * <li>Data</li>
	 * <li>Signed Data</li>
	 * <li>Digested Data</li>
	 * <li>Encrypted Data</li>
	 * <li>Enveloped Data</li>
	 * <li>Signed and Enveloped Data</li>
	 * <li>Authenticated Data</li>
	 * <li>Authenticated and Enveloped Data</li>
	 * </ul>
	 * @param file  Fichero que deseamos comprobar.
	 * @return La validez del archivo cumpliendo la estructura.
	 */
	public boolean isCMSValid(InputStream file){
		// si se lee en el CMSDATA, el inputstream ya esta leido y en los demás siempre será nulo
		if(file == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}    	
		//leemos los datos
		byte[] plainData = new byte[0];
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (Exception e1) {
			try {
				throw new AOException("No se han podido leer los datos a firmar: "+e1);
			} catch (AOException e) {
				e.printStackTrace();
			}			
		}
		// Comprobamos si su contenido es de tipo DATA
		boolean valido = new ValidateCMS().isCMSData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo SIGNEDDATA
		if (!valido) valido = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo DIGESTDATA
		if (!valido) valido = new ValidateCMS().isCMSDigestedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo ENCRYPTEDDATA
		if (!valido) valido = new ValidateCMS().isCMSEncryptedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo ENVELOPEDDATA
		if (!valido) valido = new ValidateCMS().isCMSEnvelopedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo SIGNEDANDENVELOPED
		if (!valido) valido = new ValidateCMS().isCMSSignedAndEnvelopedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo AUTHENTICATED
		if (!valido) valido = new ValidateCMS().isCMSAuthenticatedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo AUTHENTICATEDENVELOPEDDATA
		if (!valido) valido = new ValidateCMS().isCMSAuthenticatedEnvelopedData(new ByteArrayInputStream(plainData));
		// Comprobamos si su contenido es de tipo COMPRESSEDDATA
		if (!valido) valido = new ValidateCMS().isCMSCompressedData(new ByteArrayInputStream(plainData));
		return valido;
	}

	/**
	 * Comprueba que un fichero sea un tipo de dato envuelto CMS.
	 * @param file Ficheroq ue deseamos comprobar.
	 * @param type Tipo de dato que queremos.
	 * @return Indica si el fichero es una envoltura CMS con el tipo de contenido indicado.
	 */
	public boolean isCMSValid(InputStream file, String type){
		if(type.equals(AOConstants.BINARY_ENVELOP_DATA)) {
			return new ValidateCMS().isCMSData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_SIGNEDDATA)) {
			return new ValidateCMS().isCMSSignedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_DIGESTEDDATA)) {
			return new ValidateCMS().isCMSDigestedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_ENCRYPTEDDATA)) {
			return new ValidateCMS().isCMSEncryptedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA)) {
			return new ValidateCMS().isCMSEnvelopedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA)) {
			return new ValidateCMS().isCMSSignedAndEnvelopedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDDATA)) {
			return new ValidateCMS().isCMSAuthenticatedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDENVELOPEDDATA)) {
			return new ValidateCMS().isCMSAuthenticatedEnvelopedData(file);
		} else if(type.equals(AOConstants.BINARY_ENVELOP_COMPRESSEDDATA)) {
			return new ValidateCMS().isCMSCompressedData(file);
		}
		Logger.getLogger("es.gob.afirma").warning("Tipo de contenido CMS no reconocido");
		return false;
	}

	public String getDataMimeType(InputStream signData) throws AOUnsupportedSignFormatException {

		String numOid ="";
		String oid="";

		// introducimos los datos en plainData
		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signData);
		} 
		catch (Throwable e) {
			throw new NullPointerException("No se han podido leer los datos a firmar: " + e);
		}

		//Comprobamos que sea una firma valida
		try{
			this.isSign(new ByteArrayInputStream(plainData));
		}
		catch(Throwable e1){
			throw new AOUnsupportedSignFormatException("No es un tipo de firma valido.");
		}

		//Extraemos el mimetype
		ExtractMimeType extract = new ExtractMimeType();
		numOid = extract.extractMimeType(new ByteArrayInputStream(plainData));

		//Transformamos el OID a mimeType
		oid = MimeHelper.transformOidToMimeType(numOid);

		return oid;    	
	}

	/**
	 * A&ntilde;ade un atributo firmado al formato de firma seleccionado. Este formato debe reconocer el
	 * OID especificado, siendo el atributo value su valor como cadena de texto.
	 *
	 * @param oid Object Identifier. Identificador del objeto a introducir.
	 * @param value Valor asignado
	 */
	public void addSignedAttribute(org.ietf.jgss.Oid oid, byte[] value){
		atrib.put(oid, value);
	}

	/**
	 * A&ntilde;ade un atributo no firmado al formato de firma seleccionado.
	 *
	 * @param oid Object Identifier. Identificador del atributo a introducir.
	 * @param value Valor asignado
	 */
	public void addUnsignedAttribute (org.ietf.jgss.Oid oid, byte[] value){
		uatrib.put(oid, value);
	}

	/**
	 * Establece el algoritmo de cifrado.
	 *
	 * @param alg Algoritmo utilizado para cifrar.
	 */
	public void setCipherAlgorithm(AOCipherAlgorithm alg){
		this.cipherAlgorithm = alg;
	}

	public void setDataObjectFormat(String description, Oid objectIdentifier, javax.activation.MimeType mimeType, String encoding) {

		// No permitimos el cambio del tipo de dato. CMS/CAdES establece que siempre
		// sera de tipo DATA 
		//this.dataType = objectIdentifier;

	}

	public byte[] getData(InputStream signData) throws AOInvalidFormatException, AOException {

		byte[] devolver;

		if(signData == null) {
			throw new NullPointerException("Se han introducido datos nulos para su comprobacion");
		}

		// Leemos los datos
		byte[] plainData = null;
		try {
			plainData = AOUtil.getDataFromInputStream(signData);
		} 
		catch (Exception e) {
			throw new AOException("No se han podido leer los datos de firma: "+e);
		}

		// validamos que eson datos firmados.
		if (isCMSValid(new ByteArrayInputStream(plainData))){
			devolver = new ObtainContentSignedData().obtainData(new ByteArrayInputStream(plainData));
		}
		else {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
		}

		return devolver;
	}


	public String getSignedName(String originalName, String inText) {
		return originalName + (inText != null ? inText : "") + ".csig";
	};


	public AOSignInfo getSignInfo(InputStream signData) throws AOInvalidFormatException, AOException {

		if(signData == null)
			throw new NullPointerException("No se han introducido datos para analizar");

		byte[] signDataReaded;
		try {
			signDataReaded = AOUtil.getDataFromInputStream(signData);
		} catch (Throwable e) {
			throw new AOException("No se han podido leer los datos de firma: "+e);
		}

		if(!isSign(new ByteArrayInputStream(signDataReaded))) {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
		}

		AOSignInfo signInfo = new AOSignInfo(AOConstants.SIGN_FORMAT_CMS); 
		// Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
		// que se almacenan en el objeto AOSignInfo

		return signInfo;
	}
}
