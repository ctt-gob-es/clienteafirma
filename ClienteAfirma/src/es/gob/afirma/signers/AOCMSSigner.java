/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.signers;


import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.signers.aobinarysignhelper.CMSHelper;
import es.gob.afirma.signers.aobinarysignhelper.CoSigner;
import es.gob.afirma.signers.aobinarysignhelper.CoSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.CounterSigner;
import es.gob.afirma.signers.aobinarysignhelper.CounterSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.ExtractMimeType;
import es.gob.afirma.signers.aobinarysignhelper.GenSignedData;
import es.gob.afirma.signers.aobinarysignhelper.ObtainContentSignedData;
import es.gob.afirma.signers.aobinarysignhelper.P7ContentSignerParameters;
import es.gob.afirma.signers.aobinarysignhelper.ReadNodesTree;
import es.gob.afirma.signers.beans.AOSignInfo;

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

	private Oid dataType = null;
	private Map<Oid, byte[]> atrib = new HashMap<Oid, byte[]>();
	private Map<Oid, byte[]> uatrib = new HashMap<Oid, byte[]>();

	public byte[] sign(final byte[] data, 
			           String algorithm, 
			           final PrivateKeyEntry keyEntry, 
			           Properties extraParams) throws AOException {

		if (extraParams == null) extraParams = new Properties();

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");

		byte[] messageDigest = null;
		if(precalculatedDigest != null) {
			messageDigest = data;
		}
		
    	X509Certificate[] xCerts = new X509Certificate[0];
    	final Certificate[] certs = keyEntry.getCertificateChain();
    	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
    	else {
    		final Certificate cert = keyEntry.getCertificate();
    		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
    	}

		final P7ContentSignerParameters csp = new P7ContentSignerParameters(
				data,
				algorithm,
				xCerts
		);

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} catch (GSSException ex) {
				Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex);
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
		catch (final Throwable e) {
			throw new AOException("Error generando la firma PKCS#7", e);
		}
	}

	public byte[] cosign(final byte[] data, 
			             final byte[] sign, 
			             String algorithm, 
			             final PrivateKeyEntry keyEntry, 
			             Properties extraParams) throws AOException {

		if (extraParams == null) extraParams = new Properties();

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

		final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");

		byte[] messageDigest = null;
		if(precalculatedDigest != null) {
			messageDigest = data; 
		}

    	X509Certificate[] xCerts = new X509Certificate[0];
    	final Certificate[] certs = keyEntry.getCertificateChain();
    	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
    	else {
    		final Certificate cert = keyEntry.getCertificate();
    		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
    	}
		
		final P7ContentSignerParameters csp = new P7ContentSignerParameters(
			data,
			algorithm,
			xCerts
		);

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (final Throwable ex) {
				Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex);
			}
		}

		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);

		final boolean omitContent = mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

		// Si la firma que nos introducen es SignedData
		final boolean signedData = CMSHelper.isCMSValid(sign, AOConstants.CMS_CONTENTTYPE_SIGNEDDATA);
		if (signedData){
			try {
				return new CoSigner().coSigner(csp,
						sign,
						omitContent,
						dataType,
						keyEntry,
						atrib,
						uatrib,
						messageDigest);
			}
			catch (final Throwable e) {
				throw new AOException("Error generando la Cofirma PKCS#7", e);
			}
		}
		//     	Si la firma que nos introducen es SignedAndEnvelopedData
		try {
			//El parametro omitContent no tiene sentido en un signed and envelopedData.
			return new CoSignerEnveloped().coSigner(csp, 
					sign, 
					dataType, 
					keyEntry, 
					atrib, 
					uatrib, 
					messageDigest);
		}
		catch (final Throwable e) {
			throw new AOException("Error generando la Cofirma PKCS#7", e);
		}
	}

	public byte[] cosign(final byte[] sign, 
			             String algorithm, 
			             final PrivateKeyEntry keyEntry, 
			             Properties extraParams) throws AOException {

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
				Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex);
			}
		}

		// Algoritmo de firma.
		String typeAlgorithm = algorithm;
		
		// Array de certificados
    	X509Certificate[] aCertificados = new X509Certificate[0];
    	final Certificate[] certs = keyEntry.getCertificateChain();
    	if (certs != null && (certs instanceof X509Certificate[])) aCertificados = (X509Certificate[]) certs;
    	else {
    		final Certificate cert = keyEntry.getCertificate();
    		if (cert instanceof X509Certificate) aCertificados = new X509Certificate[] { (X509Certificate) cert };
    	}
		
		// Si la firma que nos introducen es SignedData
		if (CMSHelper.isCMSValid(sign, AOConstants.CMS_CONTENTTYPE_SIGNEDDATA)){
			// Cofirma de la firma usando unicamente el fichero de firmas.
			try {
				return new CoSigner().coSigner(
						typeAlgorithm, 
						aCertificados, 
						sign, 
						dataType, 
						keyEntry, 
						atrib, 
						uatrib, 
						null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
				);	
			}
			catch (final Throwable e) {
				throw new AOException("Error generando la Cofirma PKCS#7", e);
			}
		}
		//     	Si la firma que nos introducen es SignedAndEnvelopedData

		// Cofirma de la firma usando unicamente el fichero de firmas.
		try {
			return new CoSignerEnveloped().coSigner(
					typeAlgorithm, 
					aCertificados, 
					sign, 
					dataType, 
					keyEntry, 
					atrib, 
					uatrib, 
					null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
			);	
		}
		catch (final Throwable e) {
			throw new AOException("Error generando la Cofirma PKCS#7", e);
		}
	}

	public byte[] countersign(final byte[] sign, 
			                  String algorithm, 
			                  final CounterSignTarget targetType, 
			                  final Object[] targets, 
			                  final PrivateKeyEntry keyEntry, 
			                  final Properties extraParams) throws AOException {

		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

    	X509Certificate[] xCerts = new X509Certificate[0];
    	final Certificate[] certs = keyEntry.getCertificateChain();
    	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
    	else {
    		final Certificate cert = keyEntry.getCertificate();
    		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
    	}
		
		final P7ContentSignerParameters csp = new P7ContentSignerParameters(
			sign,
			algorithm,
			xCerts
		);

		// tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} catch (GSSException ex) {
				Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto: " + ex);
			}
		}

		// Datos firmados.
		byte[] dataSigned = null;

		// 	Si la firma que nos introducen es SignedData
		
		if (CMSHelper.isCMSValid(sign, AOConstants.CMS_CONTENTTYPE_SIGNEDDATA)) {
			try {
				// CASO DE FIRMA DE ARBOL
				if(targetType == CounterSignTarget.Tree){
					int[] nodes = {0};
					dataSigned= new CounterSigner().counterSigner(csp, sign,CounterSignTarget.Tree,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE HOJAS
				else if(targetType == CounterSignTarget.Leafs){
					int[] nodes={0};
					dataSigned= new CounterSigner().counterSigner(csp, sign, CounterSignTarget.Leafs,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS
				else if(targetType == CounterSignTarget.Nodes){
					int[] nodesID = new int[targets.length];
					for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
					nodesID = new ReadNodesTree().simplyArray(nodesID);
					dataSigned= new CounterSigner().counterSigner(csp, sign, CounterSignTarget.Nodes, nodesID, keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
				else if(targetType == CounterSignTarget.Signers){

					//clase que lee los nodos de un fichero firmado (p7s)
					String[] signers = new String[targets.length];
					for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
					ReadNodesTree rn2 = new ReadNodesTree();
					int[] nodes2 = rn2.readNodesFromSigners(signers, sign);
					dataSigned= new CounterSigner().counterSigner(csp, sign, CounterSignTarget.Signers,nodes2,keyEntry,dataType,atrib,uatrib);

				}
			} 
			catch (final Throwable e) {
				throw new AOException("Error generando la Contrafirma PKCS#7", e);
			}
		}
		// Si la firma es SignedAndEnveloped
		else{

			try {
				// CASO DE FIRMA DE ARBOL
				if(targetType == CounterSignTarget.Tree){
					int[] nodes = {0};
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp, sign, CounterSignTarget.Tree,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE HOJAS
				else if(targetType == CounterSignTarget.Leafs){
					int[] nodes={0};
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp, sign, CounterSignTarget.Leafs,nodes,keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS
				else if(targetType == CounterSignTarget.Nodes){
					int[] nodesID = new int[targets.length];
					for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
					nodesID = new ReadNodesTree().simplyArray(nodesID);
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp, sign, CounterSignTarget.Nodes, nodesID, keyEntry,dataType,atrib,uatrib);
				}
				// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
				else if(targetType == CounterSignTarget.Signers){

					//clase que lee los nodos de un fichero firmado (p7s)
					String[] signers = new String[targets.length];
					for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
					ReadNodesTree rn2 = new ReadNodesTree();
					int[] nodes2 = rn2.readNodesFromSigners(signers, sign);
					dataSigned= new CounterSignerEnveloped().counterSignerEnveloped(csp, sign, CounterSignTarget.Signers,nodes2,keyEntry,dataType,atrib,uatrib);

				}
			} 
			catch (Throwable e) {
				throw new AOException("Error generando la Contrafirma PKCS#7", e);
			}

		}

		return dataSigned;
	}

	public TreeModel getSignersStructure(byte[] sign, boolean asSimpleSignInfo){

		TreeModel treeModel = null;
		ReadNodesTree Rn = new ReadNodesTree();
		try {
			treeModel = Rn.readNodesTree(sign, asSimpleSignInfo);
		} catch (Throwable ex) {
			Logger.getLogger("es.gob.afirma").severe(ex.toString());
		}
		return treeModel;
	}

	public boolean isSign(byte[] signData) {
		if(signData == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}

		// Comprobamos la validez
		boolean signed = CMSHelper.isCMSValid(signData, AOConstants.CMS_CONTENTTYPE_SIGNEDDATA);
		if(!signed){
			signed = CMSHelper.isCMSValid(signData, AOConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
		}

		return signed;
	}

	public boolean isValidDataFile(byte[] data) {
		if(data == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}

	public String getDataMimeType(byte[] signData) throws AOUnsupportedSignFormatException {

		String numOid ="";
		String oid="";

		//Comprobamos que sea una firma valida
		try{
			this.isSign(signData);
		}
		catch(Throwable e1){
			throw new AOUnsupportedSignFormatException("No es un tipo de firma valido.");
		}

		//Extraemos el mimetype
		ExtractMimeType extract = new ExtractMimeType();
		numOid = extract.extractMimeType(signData);

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

	public void setDataObjectFormat(String description, Oid objectIdentifier, javax.activation.MimeType mimeType, String encoding) {

		// No permitimos el cambio del tipo de dato. CMS/CAdES establece que siempre
		// sera de tipo DATA 
		//this.dataType = objectIdentifier;

	}

	public byte[] getData(byte[] signData) throws AOInvalidFormatException, AOException {

		if(signData == null) {
			throw new NullPointerException("Se han introducido datos nulos para su comprobacion");
		}

		if (!CMSHelper.isCMSValid(signData)) {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
		}
		
		return new ObtainContentSignedData().obtainData(signData);
	}


	public String getSignedName(String originalName, String inText) {
		return originalName + (inText != null ? inText : "") + ".csig";
	}


	public AOSignInfo getSignInfo(byte[] signData) throws AOInvalidFormatException, AOException {

		if(signData == null)
			throw new NullPointerException("No se han introducido datos para analizar");

		if(!isSign(signData)) {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
		}

		AOSignInfo signInfo = new AOSignInfo(AOConstants.SIGN_FORMAT_CMS); 
		// Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
		// que se almacenan en el objeto AOSignInfo

		return signInfo;
	}
}
