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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.signers.aobinarysignhelper.CADESData;
import es.gob.afirma.signers.aobinarysignhelper.CADESDigestedData;
import es.gob.afirma.signers.aobinarysignhelper.CADESEPESSignedAndEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CADESEncryptedData;
import es.gob.afirma.signers.aobinarysignhelper.CADESEnvelopedData;
import es.gob.afirma.signers.aobinarysignhelper.CadesCoSigner;
import es.gob.afirma.signers.aobinarysignhelper.CadesCoSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.CadesCounterSigner;
import es.gob.afirma.signers.aobinarysignhelper.CadesCounterSignerEnveloped;
import es.gob.afirma.signers.aobinarysignhelper.ExtractMimeType;
import es.gob.afirma.signers.aobinarysignhelper.GenCadesEPESSignedData;
import es.gob.afirma.signers.aobinarysignhelper.ObtainContentSignedData;
import es.gob.afirma.signers.aobinarysignhelper.P7ContentSignerParameters;
import es.gob.afirma.signers.aobinarysignhelper.ReadNodesTree;
import es.gob.afirma.signers.aobinarysignhelper.ValidateCADES;
import es.gob.afirma.signers.aobinarysignhelper.ValidateCMS;
import es.gob.afirma.signers.beans.AOSignInfo;

/**
 * Manejador de firmas binarias CADES.<p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt>mode</dt>
 *  	<dd>Modo de firma a usar (Expl&iacute;cita = <code>explicit</code> o Impl&iacute;cita = <code>implicit</code>)</dd>
 *  <dt>policyIdentifier</dt>
 *  	<dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 *  <dt>policyQualifier</dt>
 *  	<dd>OID cualificador de la pol&iacute;tica de firma</dd>
 *  <dt>precalculatedHashAlgorithm</dt>
 *  	<dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 *  <dt>signingCertificateV2</dt>
 *      <dd>Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo <i>Signing Certificate</i> de CAdES. Si no se establece o se hace a <code>false</code> se utilizara la versi&oacute;n 1</dd>
 *  <dt>useMIMEencoding</dt>
 *      <dd>Debe establecerse a <code>true</code> si se desea usar codificaci&oacute;n MIME para empotrar el binario en la firma (seg&uacute;n recomendaciones de la CE). Este par&aacute;metro solo tiene efecto si el par&aacute;metro <code>mode</code> se estableci&oacute; a <code>implicit</code></dd>
 *  <dt>MIMEContentID</dt>
 *      <dd>Identificador MIME del contenido a firmar. S&oacute;lo se tiene en cuenta si el par&aacute;metro <code>MIMEContentID</code> est&aacute; establecido a <code>true</code> y el par&aacute;metro <code>mode</code> a <code>implicit</code></dd>
 *  <dt>MIMEContentType</dt>
 *    <dd>Tipo MIME (MIME-Type) del contenido a firmar. S&oacute;lo se tiene en cuenta si el par&aacute;metro <code>MIMEContentID</code> est&aacute; establecido a <code>true</code> y el par&aacute;metro <code>mode</code> a <code>implicit</code></dd>
 *  <dt>MIMEFileName</dt>      
 *    <dd>Nombre original del fichero a firmar. S&oacute;lo se tiene en cuenta si el par&aacute;metro <code>MIMEContentID</code> est&aacute; establecido a <code>true</code> y el par&aacute;metro <code>mode</code> a <code>implicit</code></dd>
 *  <dt>MIMEModificationDate</dt>
 *    <dd>Fecha de la &uacute;ltima modificaci&oacute;n (por defecto en formato "<i>yyyy/MM/dd HH:mm:ss</i>") del contenido a firmar. S&oacute;lo se tiene en cuenta si el par&aacute;metro <code>MIMEContentID</code> est&aacute; establecido a <code>true</code> y el par&aacute;metro <code>mode</code> a <code>implicit</code></dd>
 *  <dt>MIMEModificationDateFormat</dt>
 *    <dd>Formato en el que se ha proporcionado el valor del par&aacute;metro <code></code> (fecha de la &uacute;ltima modificaci&oacute;n del contenido a firmar) si este difiere de "<i>yyyy/MM/dd HH:mm:ss</i>". Misma sintaxis de formato que <code>java.text.SimpleDateFormat</code>. S&oacute;lo se tiene en cuenta si se ha establecido un valor para el par&aacute;metro <code>MIMEModificationDate</code>, <code>MIMEContentID</code> est&aacute; establecido a <code>true</code> y el par&aacute;metro <code>mode</code> a <code>implicit</code></dd>       
 * </dl> 
 * @version 0.3
 */
public final class AOCAdESSigner implements AOSigner {

    /* Propiedades de la clase. */
    private AOCipherAlgorithm cipherAlgorithm = null;
    private Oid dataType = null;
    
    private static final String DEFAULT_MIME_MODIFICATION_DATE_FORMAT = "yyyy/MM/dd HH:mm:ss";
    
    public byte[] sign(byte[] data, 
    		           String algorithm, 
    		           final PrivateKeyEntry keyEntry, 
    		           Properties extraParams) throws AOException {

    	if (extraParams == null) extraParams = new Properties();
    	
    	if (algorithm.equalsIgnoreCase("RSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
    	else if(algorithm.equalsIgnoreCase("DSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
		
        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
    	
        byte[] messageDigest = null;
        
        if(precalculatedDigest != null) {
            messageDigest = data; 
        }
        
		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);
        
    	if (mode.equals(AOConstants.SIGN_MODE_IMPLICIT) &&
		    "true".equalsIgnoreCase(extraParams.getProperty("useMIMEencoding"))) {
    		
    		Date modDateObject = null;
    		final String modDate = extraParams.getProperty("MIMEModificationDate");
    		if (modDate != null) {
	    		try {
	    			modDateObject = new SimpleDateFormat(
    					extraParams.getProperty("useMIMEencoding", DEFAULT_MIME_MODIFICATION_DATE_FORMAT)
					).parse(modDate);
	    		}
	    		catch(final Throwable e) {
	    			Logger.getLogger("es.gob.afirma").warning(
    					"La fecha MIME de ultima modificacion del contenido a firmar proporcionada (" +
    					modDate +
    					") no corresponde al formato establecido, se omitira este dato: " +
    					e
					);
	    		}
    		}	    		
    		data = MimeHelper.getMimeEncodedAsAttachment(
				data, 
				extraParams.getProperty("MIMEContentID"), 
				extraParams.getProperty("MIMEContentType"), 
				extraParams.getProperty("MIMEFileName"), 
				modDateObject
			).getBytes();
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
        if (this.dataType==null) {
            try {
                this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
            }
            catch (final Throwable ex) {
                throw new AOException("Error al asignar el OID por defecto", ex);
            }
        }
        
        try {
            boolean omitContent = false;
        	if (mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null) omitContent = true;
        	Oid policyQualifier = null;
        	try {
        		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
        	}
        	catch(final Throwable e) {}
        	
        	return new GenCadesEPESSignedData().generateSignedData(
    			csp, 
    			omitContent, 
    			extraParams.getProperty("policyIdentifier"), 
    			policyQualifier,
    			signingCertificateV2, 
    			dataType, 
    			keyEntry, 
    			messageDigest
			);
        	
        }
        catch (final Throwable e) {
            throw new AOException("Error generando la firma CAdES", e);
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
		final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
		
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
        if (this.dataType == null) {
            try {
                this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
            }
            catch (Throwable ex) {
                throw new AOException("Error al asignar el OID por defecto", ex);
            }
        }
		
        try {
        	Oid policyQualifier = null;
        	try {
        		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
        	}
        	catch(Throwable e) {}
        	
        	//Si la firma que nos introducen es SignedData
    		boolean signedData = new ValidateCMS().isCMSSignedData(sign);
        	if (signedData){
        		
        		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);
        		final boolean omitContent = mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;
        		
	            return new CadesCoSigner().coSigner(
	        		csp, 
	        		sign, 
	        		omitContent, 
	        		extraParams.getProperty("policyIdentifier"), 
	        		policyQualifier, 
	        		signingCertificateV2, 
	        		dataType, 
	        		keyEntry, 
	        		messageDigest
	    		);
        	}

    		return new CadesCoSignerEnveloped().coSigner(
        		csp, 
        		sign, 
        		extraParams.getProperty("policyIdentifier"), 
        		policyQualifier, 
        		signingCertificateV2, 
        		dataType, 
        		keyEntry, 
        		messageDigest
    		);

        }
        catch (final Throwable e) {
            throw new AOException("Error generando la Cofirma CAdES", e);
        }
    }
    

    public byte[] cosign(final byte[] sign, 
    		             String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             Properties extraParams) throws AOException {
    
    	if (extraParams == null) extraParams = new Properties();
    	final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
    	
		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
         
		    // tipos de datos a firmar.
        if (this.dataType==null){
            try {
                this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
            } 
            catch (final Throwable ex) {
                throw new AOException("Error al asignar el OID por defecto", ex);
            }
        }
        //algoritmo de firma.
        String typeAlgorithm = algorithm;
        // Array de certificados
    	X509Certificate[] aCertificados = new X509Certificate[0];
    	final Certificate[] certs = keyEntry.getCertificateChain();
    	if (certs != null && (certs instanceof X509Certificate[])) aCertificados = (X509Certificate[]) certs;
    	else {
    		final Certificate cert = keyEntry.getCertificate();
    		if (cert instanceof X509Certificate) aCertificados = new X509Certificate[] { (X509Certificate) cert };
    	}
        
    	Oid policyQualifier = null;
    	try {
    		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
    	}
    	catch(final Throwable e) {}
    	
		//     	Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(sign);
    	if (signedData){
	        try {
	            return new CadesCoSigner().coSigner(
	        		typeAlgorithm, 
	        		aCertificados, 
	        		new ByteArrayInputStream(sign), 
	        		extraParams.getProperty("policyIdentifier"), 
	        		policyQualifier, 
	        		signingCertificateV2,
	        		dataType,
	        		keyEntry,
	        		null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
	    		);
	        }
	        catch (Throwable e) {
	            throw new AOException("Error generando la Cofirma CADES", e);
	        }
    	}
    	// Signed And Enveloped.

		try {
            return new CadesCoSignerEnveloped().coSigner(
        		typeAlgorithm, 
        		aCertificados, 
        		new ByteArrayInputStream(sign), 
        		extraParams.getProperty("policyIdentifier"), 
        		policyQualifier, 
        		signingCertificateV2,
        		dataType,
        		keyEntry,
        		null // null porque no nos pueden dar un hash en este metodo, tendría que ser en el que incluye datos
    		);
        }
        catch (final Throwable e) {
            throw new AOException("Error generando la Cofirma CADES", e);
        }

    }

    public byte[] countersign(final byte[] sign, 
    		                  String algorithm, 
    		                  final CounterSignTarget targetType, 
    		                  final Object[] targets, 
    		                  final PrivateKeyEntry keyEntry, 
    		                  Properties extraParams) throws AOException {

    	if (extraParams == null) extraParams = new Properties();
    	final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
    	
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
    		} 
    		catch (final Throwable ex) {
    			throw new AOException("Error al asignar el OID por defecto", ex);
    		}
    	}

    	// Recuperamos la polictica de firma si se indico
    	Oid policyQualifier = null;
    	String policyIdentifier = null;
    	if(extraParams.containsKey("policyQualifier")) {
    		try {
    			policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
    		} catch(final Throwable e) {}
    		policyIdentifier = extraParams.getProperty("policyIdentifier");
    	}

    	// Datos firmados.
    	byte[] dataSigned = null;

    	// Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(sign);
    	if (signedData){
	    	try {
	    		// CASO DE FIRMA DE ARBOL
	    		if(targetType == CounterSignTarget.Tree){
	    			int[] nodes = {0};
	
	    			dataSigned= new CadesCounterSigner().counterSigner(
	    					csp,
	    					sign,
	    					CounterSignTarget.Tree,
	    					nodes,
	    					keyEntry, 
	    					policyIdentifier, 
	    					policyQualifier, 
	    					signingCertificateV2,
	    					dataType
	    			);
	    		}
	    		// CASO DE FIRMA DE HOJAS
	    		else if(targetType == CounterSignTarget.Leafs){
	    			int[] nodes={0};
	    			dataSigned= new CadesCounterSigner().counterSigner(
	    					csp,
	    					sign,
	    					CounterSignTarget.Leafs,
	    					nodes,
	    					keyEntry, 
	    					policyIdentifier, 
	    					policyQualifier, 
	    					signingCertificateV2, 
	    					dataType
	    			);
	    		}
	    		// CASO DE FIRMA DE NODOS
	    		else if(targetType == CounterSignTarget.Nodes){
	    			int[] nodesID = new int[targets.length];
	    			for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
	    			nodesID = new ReadNodesTree().simplyArray(nodesID);
	    			dataSigned= new CadesCounterSigner().counterSigner(
						csp, 
						sign, 
						CounterSignTarget.Nodes, 
						nodesID, 
						keyEntry, 
						policyIdentifier, 
						policyQualifier, 
						signingCertificateV2, 
						dataType
	    			);
	    		}
	    		// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
	    		else if(targetType == CounterSignTarget.Signers){
	
	    			//clase que lee los nodos de un fichero firmado (p7s, csig, sig)
	    			final String[] signers = new String[targets.length];
	    			for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
	    			final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
	    			dataSigned= new CadesCounterSigner().counterSigner(
	    					csp,
	    					sign,
	    					CounterSignTarget.Signers,
	    					nodes2,
	    					keyEntry, 
	    					policyIdentifier, 
	    					policyQualifier, 
	    					signingCertificateV2, 
	    					dataType
	    			);
	
	    		}
	
	    		return dataSigned;
	
	    	} catch (final Throwable e) {
	    		throw new AOException("Error generando la Contrafirma CAdES", e);
	    	}
    	}
    	//Signed and enveloped

		try {
    		// CASO DE FIRMA DE ARBOL
    		if(targetType == CounterSignTarget.Tree){
    			int[] nodes = {0};

    			dataSigned= new CadesCounterSignerEnveloped().counterSigner(
    					csp,
    					sign,
    					CounterSignTarget.Tree,
    					nodes,
    					keyEntry, 
    					policyIdentifier, 
    					policyQualifier, 
    					signingCertificateV2,
    					dataType
    			);
    		}
    		// CASO DE FIRMA DE HOJAS
    		else if(targetType == CounterSignTarget.Leafs){
    			int[] nodes={0};
    			dataSigned= new CadesCounterSignerEnveloped().counterSigner(
    					csp,
    					sign,
    					CounterSignTarget.Leafs,
    					nodes,
    					keyEntry, 
    					policyIdentifier, 
    					policyQualifier, 
    					signingCertificateV2, 
    					dataType
    			);
    		}
    		// CASO DE FIRMA DE NODOS
    		else if(targetType == CounterSignTarget.Nodes){
    			int[] nodesID = new int[targets.length];
    			for(int i=0; i<targets.length; i++) nodesID[i] = ((Integer)targets[i]).intValue();
    			nodesID = new ReadNodesTree().simplyArray(nodesID);
    			dataSigned= new CadesCounterSignerEnveloped().counterSigner(
					csp, 
					sign, 
					CounterSignTarget.Nodes, 
					nodesID, 
					keyEntry, 
					policyIdentifier, 
					policyQualifier, 
					signingCertificateV2, 
					dataType
    			);
    		}
    		// CASO DE FIRMA DE NODOS DE UNO O VARIOS FIRMANTES
    		else if(targetType == CounterSignTarget.Signers){

    			//clase que lee los nodos de un fichero firmado (p7s, csig, sig)
    			final String[] signers = new String[targets.length];
    			for(int i=0; i<targets.length; i++) signers[i] = (String)targets[i];
    			final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, sign);
    			dataSigned = new CadesCounterSignerEnveloped().counterSigner(
    					csp,
    					sign,
    					CounterSignTarget.Signers,
    					nodes2,
    					keyEntry, 
    					policyIdentifier, 
    					policyQualifier, 
    					signingCertificateV2, 
    					dataType
    			);

    		}

    		return dataSigned;

    	} catch (final Throwable e) {
    		throw new AOException("Error generando la Contrafirma CAdES", e);
    	}

    }

    /**
     * M&eacute;todo que realiza el resto de firmas permitidas por CADES.
     * Son las siguientes: <br/>
     * <ul>
     * <li>Data</li>
     * <li>Signed Data</li>
     * <li>Digested Data</li>
     * <li>Enveloped Data</li>
     * <li>Signed and Enveloped Data</li>
     * </ul>
     *
     * Para la generaci&oacute;n de la clave interna se utiliza por defecto el AES.
     *
     * En el caso de que sea tipo "Enveloped data" o "Signed and enveloped data",
     * la clave se generar&aacute; usando el algoritmo pasado como par&aacute;metro.
     * Dicha clave se cifrar&aacute; despu&eacute;s con la clave p&uacute;blica del certificado
     * que identifica al usuario destinatario.
     *
     * Nota: El par&aacute;metro algorithm no es el agoritmo de cifrado,
     * es para el digestAlgorithm usado en los "Unsigned Attributes".
     *
     * @param file                Flujo de lectura de los datos a firmar.
     * @param digestAlgorithm     Algoritmo a usar para la firma (SHA1withRSA, MD5withRSA,...)
     * @param type                Tipo de "envelop" que se quiere hacer.
     * @param keyEntry            Clave privada a usar para firmar.
     * @param certDest            Certificados de los usuarios a los que va destinado el sobre digital.
     * @param extraParams         Par&aacute;metros adicionales
     * @return                    Envoltorio CADES.
     * @throws AOException  Cuando ocurre cualquier problema en el proceso.
     */
    public byte[] envelop(final InputStream file, 
    		              final String digestAlgorithm, 
    		              String type, 
    		              final PrivateKeyEntry keyEntry, 
    		              final X509Certificate[] certDest, 
    		              Properties extraParams) throws AOException {
    	
    	if (extraParams == null) extraParams = new Properties();
    	final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));

        //Comprobamos que el archivo a tratar no sea nulo.
        if (file==null){
            throw new NullPointerException("El archivo a tratar no puede ser nulo.");
        }

        byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (final Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar", e1);
		}
		
        
		P7ContentSignerParameters csp = null;
        if (keyEntry!=null){
        	
        	X509Certificate[] xCerts = new X509Certificate[0];
        	final Certificate[] certs = keyEntry.getCertificateChain();
        	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
        	else {
        		final Certificate cert = keyEntry.getCertificate();
        		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
        	}
        	
            csp = new P7ContentSignerParameters(
        		plainData,
                digestAlgorithm,
                xCerts
            );
            
        }

        // tipos de datos a firmar.
        if (this.dataType==null){
            try {
                this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
            } 
            catch (final Throwable ex) {
            	throw new AOException("Error al asignar el OID por defecto", ex);
            }
        }

        // Datos firmados.
        byte[] dataSigned = null;

        //Seleccion del algoritmo de cifrado.
        AOCipherConfig config=null;
        if (this.cipherAlgorithm == null){
            // Por defecto usamos el AES.
            config = new AOCipherConfig(
                AOCipherAlgorithm.AES,
                AOCipherBlockMode.CBC,
                AOCipherPadding.PKCS5PADDING
            );
        }
        /* En caso de usar un algoritmo de cifrado, si no funciona es porque el
           Provider no lo soporta. */
        else{
            config = new AOCipherConfig(
                this.cipherAlgorithm,
				AOCipherBlockMode.CBC,
				AOCipherPadding.PKCS5PADDING
            );
        }

        try {
            // Busqueda del tipo que nos han solicitado.
            if ((type== null) || (type.equals(""))){
                type = AOConstants.DEFAULT_CMS_CONTENTTYPE;
            }
            // Es Data.
            else if (type.equals(AOConstants.CMS_CONTENTTYPE_DATA)){
                dataSigned = new CADESData().genData(csp);
            }
            //Es Digested Data.
            else if(type.equals(AOConstants.CMS_CONTENTTYPE_DIGESTEDDATA)){
                dataSigned = new CADESDigestedData().genDigestedData(csp,dataType);
            }
            // Es Enveloped Data.
            else if(type.equals(AOConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)){
                 if (keyEntry != null){
                    dataSigned = new CADESEnvelopedData().genEnvelopedData(csp, config, certDest, dataType);
                 }
                 else{
                    dataSigned = new CADESEnvelopedData().genEnvelopedData(plainData, digestAlgorithm, config, certDest, dataType);
                 }
            }
            // Es Signed and Enveloped Data.
            else {
                try {
                    this.dataType = new Oid(PKCSObjectIdentifiers.signedData.getId());
                } 
                catch (final Throwable ex) {
                    throw new AOException("Error al asignar el OID por defecto del SignedData", ex);
                }
            	Oid policyQualifier = null;
            	try {
            		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
            	}
            	catch(Throwable e) {}
                dataSigned = new CADESEPESSignedAndEnvelopedData().genCADESEPESSignedAndEnvelopedData(
            		csp, 
            		config,
            		extraParams.getProperty("policyIdentifier"), 
            		policyQualifier, 
            		signingCertificateV2, 
            		certDest, 
            		dataType, 
            		keyEntry
        		);
            }
        } 
        catch (final Throwable e) {
           throw new AOException("Error generando el enveloped de CADES", e);
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
     * @return Contenido firmado
     * @throws AOException Cuando ocurre cualquier problema durante el proceso
     */
    public byte[] encrypt(final InputStream file, final String digestAlgorithm, final String key) throws AOException {

        //Comprobamos que el archivo a cifrar no sea nulo.
        if (file == null){
            throw new NullPointerException("El archivo a cifrar no puede ser nulo.");
        }

        // tipos de datos a firmar.
        if (this.dataType==null){
            try {
                this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
            } 
            catch (final Throwable ex) {
            	throw new AOException("Error al asignar el OID por defecto", ex);
            }
        }

        //Seleccion del algoritmo de cifrado.
        AOCipherConfig config=null;
        if (this.cipherAlgorithm == null){
            // Por defecto usamos el PBEWITHSHA1ANDDESEDE. El AES en este caso no funciona.
            config = new AOCipherConfig(
                AOCipherAlgorithm.AES,
                AOCipherBlockMode.CBC,
                AOCipherPadding.PKCS5PADDING
            );
        }
        /* En caso de usar un algoritmo de cifrado, si no funciona es porque el
           Provider no lo soporta. */
        else{
            config = new AOCipherConfig(
                this.cipherAlgorithm,
				AOCipherBlockMode.CBC,
				AOCipherPadding.PKCS5PADDING
            );
        }

        try {
            return new CADESEncryptedData().genEncryptedData(
        		file, 
        		digestAlgorithm, 
        		config , 
        		key, 
        		dataType
    		);
        } 
        catch (final Throwable e) {
            throw new AOException("Error generando el enveloped de CADES", e);
        }

    }

	public TreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo){
        final ReadNodesTree Rn = new ReadNodesTree();
        try {
            return Rn.readNodesTree(sign, asSimpleSignInfo);
        } 
        catch (final Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe(
        		"No se ha podido obtener el albol de firmantes de la firma, se devolvera null: " + ex
    		);
        }
        return null;
    }

	public boolean isSign(final byte[] data) {
		if(data == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
    	return new ValidateCADES().isCADESSignedData(data);
    }

	public boolean isValidDataFile(final byte[] data) {
		if(data == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
		return true;
	}

    /**
     * M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se realiza la verificaciÃƒÆ’Ã‚Â³n sobre los los siguientes tipos de CMS reconocidos:
     * <ul>
     *  <li>Data</li>
     *  <li>Signed Data</li>
     *  <li>Digested Data</li>
     *  <li>Encrypted Data</li>
     *  <li>Enveloped Data</li>
     *  <li>Signed and Enveloped Data</li>
     * </ul>
     * @param data  Datos que deseamos comprobar.
     * @return La validez del archivo cumpliendo la estructura.
     */
	public boolean isCADESValid(final byte[] data){
		// si se lee en el CMSDATA, el inputstream ya esta leido y en los demás siempre será nulo
		if(data == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}    	

    	// Comprobamos si su contenido es de tipo DATA
    	boolean valido = new ValidateCADES().isCADESData(data);
    	// Comprobamos si su contenido es de tipo SIGNEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESSignedData(data);
    	// Comprobamos si su contenido es de tipo DIGESTDATA
    	if (!valido) valido = new ValidateCADES().isCADESDigestedData(data);
    	// Comprobamos si su contenido es de tipo ENCRYPTEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESEncryptedData(data);
    	// Comprobamos si su contenido es de tipo ENVELOPEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESEnvelopedData(data);
    	// Comprobamos si su contenido es de tipo SIGNEDANDENVELOPED
    	if (!valido) valido = new ValidateCADES().isCADESSignedAndEnvelopedData(data);
    	return valido;
    }

    /**
     * M&eacute;todo que comprueba que un archivo cumple la estructura deseada.
     * Se permite la verificaci&oacute;n de los siguientes tipos de firma:
     *
     * <ul>
     *  <li>Data</li>
     *  <li>Signed Data</li>
     *  <li>Digested Data</li>
     *  <li>Encrypted Data</li>
     *  <li>Enveloped Data</li>
     *  <li>Signed and Enveloped Data</li>
     * </ul>
     *
     * @param signData  Datos que se desean comprobar.
     * @param type      Tipo de firma que se quiere verificar.
     *
     * @return          La validez del archivo cumpliendo la estructura.
     */
    public static boolean isCADESValid(final byte[] signData, final String type){
        if (type.equals(AOConstants.CMS_CONTENTTYPE_DATA)){
            return new ValidateCADES().isCADESData(signData);
        } else if (type.equals(AOConstants.CMS_CONTENTTYPE_SIGNEDDATA)){
            return new ValidateCADES().isCADESSignedData(signData);
        } else if (type.equals(AOConstants.CMS_CONTENTTYPE_DIGESTEDDATA)){
            return new ValidateCADES().isCADESDigestedData(signData);
        } else if (type.equals(AOConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA)){
            return new ValidateCADES().isCADESEncryptedData(signData);
        } else if (type.equals(AOConstants.CMS_CONTENTTYPE_ENVELOPEDDATA)){
            return new ValidateCADES().isCADESEnvelopedData(signData);
        } else if (type.equals(AOConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA)){
            return new ValidateCADES().isCADESSignedAndEnvelopedData(signData);
        }
        Logger.getLogger("es.gob.afirma").warning("Tipo de contenido CADES no reconocido");
		return false;
    }
    
    /**
     * Obtiene el tipo de datos declarado en una firma mediante su Mime Type. Si no se conoce
     * el tipo de dato se devolver&aacute; <code>null</code>. Seg&uacute;n el formato de firma
     * puede haber un tipo de datos por defecto: application/octect-stream,...
     * @param signData Firma electr&oacute;nica.
     * @return Mime Type de los datos contenidos en la firma.
     * @throws AOUnsupportedSignFormatException Cuando la firma no est&eacute; soportada por el manejador proporcionado.
     */
    public String getDataMimeType(final byte[] signData) throws AOUnsupportedSignFormatException {
		
		//Comprobamos que sea una firma valida
		try {
			this.isSign(signData);
		}
		catch(final Throwable e1){
			throw new AOUnsupportedSignFormatException("No es un tipo de firma valido", e1);
		}
		
		//Extraemos el mimetype y transformamos el OID a mimeType
		return MimeHelper.transformOidToMimeType(
			new ExtractMimeType().extractMimeType(signData)
		);
		
    }
    
	/**
	 * Inserta un nuevo firmante dentro de una firma signedAndEnveloped dada.
	 * @param signFile				Flujo de entrada de datos que contiene la firma.
	 * @param file					Fichero de firma, necesario para calcular los datos del nuevo firmante.
	 * @param signatureAlgorithm	Algoritmo de firma.
	 * @param keyEntry				Clave privada a usar para firmar.
	 * @param extraParams           Par&aacute;metros adiocionales (variables) 
	 * @return Firma original con el nuevo firmante a&ntilde;adido 
	 * @throws AOException			Cuando ocurre cualquier problema durante el proceso
	 */
	public byte[] addOriginatorInfo(InputStream signFile, InputStream file, String signatureAlgorithm, PrivateKeyEntry keyEntry, Properties extraParams ) throws AOException {

		//Comprobamos que el archivo a tratar no sea nulo.
		if (file==null){
			throw new NullPointerException("El archivo a tratar no puede ser nulo.");
		}

		final byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (final Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar", e1);
		}

		P7ContentSignerParameters csp = null;
		if (keyEntry!=null) {
			
	    	X509Certificate[] xCerts = new X509Certificate[0];
	    	final Certificate[] certs = keyEntry.getCertificateChain();
	    	if (certs != null && (certs instanceof X509Certificate[])) xCerts = (X509Certificate[]) certs;
	    	else {
	    		final Certificate cert = keyEntry.getCertificate();
	    		if (cert instanceof X509Certificate) xCerts = new X509Certificate[] { (X509Certificate) cert };
	    	}
	    	
			csp = new P7ContentSignerParameters(
				plainData,
				signatureAlgorithm,
				xCerts
			);
			
		}

		// Tipos de datos a firmar.
		if (this.dataType==null){
			try {
				this.dataType = new Oid(PKCSObjectIdentifiers.data.getId());
			} 
			catch (final Throwable ex) {
				//Logger.getLogger("es.gob.afirma").severe("Error al asignar el OID por defecto en el envoltorio CMS: " + ex);
				throw new AOException("Error al asignar el OID por defecto en el envoltorio CMS", ex);
			}
		}
		
		Oid policyQualifier = null;
    	try {
    		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
	    } catch(Throwable e) {}

	    final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
	    
		// Datos firmados.
		byte[] dataSigned = null;

		try {
			dataSigned = new CADESEPESSignedAndEnvelopedData().addOriginatorInfo(signFile, csp, keyEntry, dataType, extraParams.getProperty("policyIdentifier"),policyQualifier , signingCertificateV2);
			
		} 
		catch (final Throwable e) {
			throw new AOException("Error generando el enveloped de CMS", e);
		}
		return dataSigned;
	}

    /**
     * Establece el algoritmo de cifrado.
     * @param alg Algoritmo utilizado para cifrar.
     */
    public void setCipherAlgorithm(AOCipherAlgorithm alg){
        this.cipherAlgorithm = alg;
    }

    public void setDataObjectFormat(String description, Oid objectIdentifier, javax.activation.MimeType mimeType, String encoding) {
    	// No permitimos el cambio del tipo de dato. CMS/CAdES establece que siempre
    	// sera de tipo DATA 
    	// this.dataType = objectIdentifier;
    }

    public byte[] getData(final byte[] signData) throws AOInvalidFormatException{
    	
		if(signData == null) {
			throw new NullPointerException(
				"Se han introducido datos nulos para su comprobacion"
			);
		}
    	
    	if (!this.isCADESValid(signData)) {
    		throw new AOInvalidFormatException(
				"Los datos introducidos no se corresponden con un objeto de firma"
			);
    	}
    	
   		return new ObtainContentSignedData().obtainData(signData);
    }

    public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".csig";
	}

    public AOSignInfo getSignInfo(final byte[] signData) throws AOInvalidFormatException, AOException {
        if(signData == null) throw new NullPointerException("No se han introducido datos para analizar");
        
        
        if(!isSign(signData)) {
            throw new AOInvalidFormatException(
        		"Los datos introducidos no se corresponden con un objeto de firma"
    		);
        }
        
        return new AOSignInfo(AOConstants.SIGN_FORMAT_CADES); 
        // Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
        
    }
}
