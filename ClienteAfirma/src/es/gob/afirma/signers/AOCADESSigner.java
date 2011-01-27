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
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.tree.TreeModel;

import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
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

/**
 * Manejador de firmas binarias CADES.<p>
 * Par&aacute;metros adicionales aceptados para las operaciones de firma:<br>
 * <dl>
 *  <dt>mode</dt>
 *  	<dd>Modo de firma a usar (Expl&iacute;cita o Impl&iacute;cita)</dd>
 *  <dt>policyIdentifier</dt>
 *  	<dd>URL identificadora de la pol&iacute;tica de firma (normalmente una URL hacia el documento que describe la pol&iacute;tica)</dd>
 *  <dt>policyQualifier</dt>
 *  	<dd>OID cualificador de la pol&iacute;tica de firma</dd>
 *  <dt>precalculatedHashAlgorithm</dt>
 *  	<dd>Algoritmo de huella digital cuando esta se proporciona precalculada</dd>
 *  <dt>signingCertificateV2</dt>
 *      <dd>Debe establecerse a <code>true</code> si desea usarse la versi&oacute;n 2 del atributo <i>Signing Certificate</i> de CAdES. Si no se establece o se hace a <code>false</code> se utilizara la versi&oacute;n 1</dd> 
 * </dl> 
 * @version 0.2
 */
public final class AOCADESSigner implements AOSigner {

    /* Propiedades de la clase. */
    private AOCipherAlgorithm cipherAlgorithm= null;
    private Oid dataType = null;
    
    public byte[] sign(final InputStream file, 
    		           String algorithm, 
    		           final PrivateKeyEntry keyEntry, 
    		           final X509Certificate cert, 
    		           Properties extraParams) throws AOException {

    	if (extraParams == null) extraParams = new Properties();
    	
    	if (algorithm.equalsIgnoreCase("RSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
    	else if(algorithm.equalsIgnoreCase("DSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
		
        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");
        final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
		
		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} 
		catch (final Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}
    	
        byte[] messageDigest = null;
        
        if(precalculatedDigest != null) {
            messageDigest = plainData; 
        }
        
        final P7ContentSignerParameters csp = new P7ContentSignerParameters(
            plainData,
            algorithm,
            (X509Certificate[]) keyEntry.getCertificateChain()
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
        
		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);

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
        catch (Throwable e) {
            throw new AOException("Error generando la firma CAdES", e);
        }
    }

    public byte[] cosign(InputStream file, InputStream signFile, String algorithm, PrivateKeyEntry keyEntry, X509Certificate cert, Properties extraParams) throws AOException {
        
    	if (extraParams == null) extraParams = new Properties();
    	
		if(algorithm.equalsIgnoreCase("RSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		else if(algorithm.equalsIgnoreCase("DSA"))
			algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;
		
		final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm");
		final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
 		
 		byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(file);
		} catch (Throwable e1) {
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
    	
        final P7ContentSignerParameters csp = new P7ContentSignerParameters(
    		plainData,
            algorithm,
            (X509Certificate[]) keyEntry.getCertificateChain()
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
    		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainSign));
        	if (signedData){
        		
        		final String mode = extraParams.getProperty("mode", AOConstants.DEFAULT_SIGN_MODE);
          	final boolean omitContent = mode.equals(AOConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;
        		
	            return new CadesCoSigner().coSigner(
	        		csp, 
	        		new ByteArrayInputStream(plainSign), 
	        		omitContent, 
	        		extraParams.getProperty("policyIdentifier"), 
	        		policyQualifier, 
	        		signingCertificateV2, 
	        		dataType, 
	        		keyEntry, 
	        		messageDigest
	    		);
        	}
        	else{
        		return new CadesCoSignerEnveloped().coSigner(
    	        		csp, 
    	        		new ByteArrayInputStream(plainSign), 
    	        		extraParams.getProperty("policyIdentifier"), 
    	        		policyQualifier, 
    	        		signingCertificateV2, 
    	        		dataType, 
    	        		keyEntry, 
    	        		messageDigest
    	    		);
        	}
        }
        catch (Throwable e) {
            throw new AOException("Error generando la Cofirma CAdES", e);
        }
    }
    

    public byte[] cosign(final InputStream signFile, 
    		             String algorithm, 
    		             final PrivateKeyEntry keyEntry, 
    		             final X509Certificate cert, 
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
        X509Certificate[] aCertificados=(X509Certificate[]) keyEntry.getCertificateChain();
    	Oid policyQualifier = null;
    	try {
    		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
    	}
    	catch(Throwable e) {}
    	
    	byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signFile);
		} 
		catch (Throwable e1) {
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}
    	
		//     	Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainData));
    	if (signedData){
	        try {
	            return new CadesCoSigner().coSigner(
	        		typeAlgorithm, 
	        		aCertificados, 
	        		new ByteArrayInputStream(plainData), 
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
    	else{
    		try {
	            return new CadesCoSignerEnveloped().coSigner(
	        		typeAlgorithm, 
	        		aCertificados, 
	        		new ByteArrayInputStream(plainData), 
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

    }

    public byte[] countersign(final InputStream signFile, String algorithm, final CounterSignTarget targetType, final Object[] targets, final PrivateKeyEntry keyEntry, final X509Certificate cert, Properties extraParams) throws AOException {

    	if (extraParams == null) extraParams = new Properties();
    	final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
    	
    	if(algorithm.equalsIgnoreCase("RSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
    	else if(algorithm.equalsIgnoreCase("DSA"))
    		algorithm = AOConstants.SIGN_ALGORITHM_SHA1WITHDSA;

    	byte[] plainData;
    	try {
    		plainData = AOUtil.getDataFromInputStream(signFile);
    	} catch (Exception e1) {
    		throw new AOException("No se han podido leer los datos a firmar: "+e1);
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
    		} catch(Throwable e) {}
    		policyIdentifier = extraParams.getProperty("policyIdentifier");
    	}

    	// Datos firmados.
    	byte[] dataSigned = null;

    	// Si la firma que nos introducen es SignedData
		boolean signedData = new ValidateCMS().isCMSSignedData(new ByteArrayInputStream(plainData));
    	if (signedData){
	    	try {
	    		// CASO DE FIRMA DE ARBOL
	    		if(targetType == CounterSignTarget.Tree){
	    			int[] nodes = {0};
	
	    			dataSigned= new CadesCounterSigner().counterSigner(
	    					csp,
	    					plainData,
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
	    					plainData,
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
						plainData, 
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
	    			final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, plainData);
	    			dataSigned= new CadesCounterSigner().counterSigner(
	    					csp,
	    					plainData,
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
    	else{
    		try {
	    		// CASO DE FIRMA DE ARBOL
	    		if(targetType == CounterSignTarget.Tree){
	    			int[] nodes = {0};
	
	    			dataSigned= new CadesCounterSignerEnveloped().counterSigner(
	    					csp,
	    					plainData,
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
	    					plainData,
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
						plainData, 
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
	    			final int[] nodes2 = new ReadNodesTree().readNodesFromSigners(signers, plainData);
	    			dataSigned= new CadesCounterSignerEnveloped().counterSigner(
	    					csp,
	    					plainData,
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
			throw new AOException("No se han podido leer los datos a firmar: "+e1);
		}
		
        
		P7ContentSignerParameters csp = null;
        if (keyEntry!=null){
            csp = new P7ContentSignerParameters(
        		plainData,
                digestAlgorithm,
                (X509Certificate[]) keyEntry.getCertificateChain()
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

        try {
            // Busqueda del tipo que nos han solicitado.
            if ((type== null) || (type.equals(""))){
                type = AOConstants.DEFAULT_BINARY_ENVELOP;
            }
            // Es Data.
            else if (type.equals(AOConstants.BINARY_ENVELOP_DATA)){
                dataSigned = new CADESData().genData(csp);
            }
            //Es Digested Data.
            else if(type.equals(AOConstants.BINARY_ENVELOP_DIGESTEDDATA)){
                dataSigned = new CADESDigestedData().genDigestedData(csp,dataType);
            }
            // Es Enveloped Data.
            else if(type.equals(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA)){
                 if (keyEntry != null){
                    dataSigned = new CADESEnvelopedData().genEnvelopedData(csp, config, certDest, dataType);
                 }
                 else{
                    dataSigned = new CADESEnvelopedData().genEnvelopedData(file, digestAlgorithm, config, certDest, dataType);
                 }
            }
            // Es Signed and Enveloped Data.
            else{
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
        if (file==null){
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

        // Datos firmados.
        byte[] dataSigned = null;

        //Seleccion del algoritmo de cifrado.
        AOAlgorithmConfig config=null;
        if (this.cipherAlgorithm == null){
            // Por defecto usamos el PBEWITHSHA1ANDDESEDE. El AES en este caso no funciona.
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
            dataSigned = new CADESEncryptedData().genEncryptedData(
        		file, 
        		digestAlgorithm, 
        		config , 
        		key, 
        		dataType
    		);
        } 
        catch (final Throwable e) {
            throw new AOException("Error generando en el enveloped de CADES", e);
        }

        return dataSigned;

    }

	public TreeModel getSignersStructure(final InputStream sign, final boolean asSimpleSignInfo){

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
        } 
        catch (final Throwable e) {
        	Logger.getLogger("es.gob.afirma").warning (
    			"Error al leer los datos a firmar: " + e 
        	);
        	return null;
        }

       //clase que lee los nodos de un fichero firmado (p7s, csig, sig)
        ReadNodesTree Rn = new ReadNodesTree();
        try {
            treeModel = Rn.readNodesTree(baos.toByteArray(), asSimpleSignInfo);
        } 
        catch (final Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe(ex.toString());
        }
        return treeModel;
    }

	public boolean isSign(final InputStream file) {
		if(file == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return false;
		}
    	return new ValidateCADES().isCADESSignedData(file);
    }

	public boolean isValidDataFile(final InputStream file) {
		if(file == null) {
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
     * @param file  Fichero que deseamos comprobar.
     * @return La validez del archivo cumpliendo la estructura.
     */
	public boolean isCADESValid(final InputStream file){
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
    	boolean valido = new ValidateCADES().isCADESData(new ByteArrayInputStream(plainData));
    	// Comprobamos si su contenido es de tipo SIGNEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESSignedData(new ByteArrayInputStream(plainData));
    	// Comprobamos si su contenido es de tipo DIGESTDATA
    	if (!valido) valido = new ValidateCADES().isCADESDigestedData(new ByteArrayInputStream(plainData));
    	// Comprobamos si su contenido es de tipo ENCRYPTEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESEncryptedData(new ByteArrayInputStream(plainData));
    	// Comprobamos si su contenido es de tipo ENVELOPEDDATA
    	if (!valido) valido = new ValidateCADES().isCADESEnvelopedData(new ByteArrayInputStream(plainData));
    	// Comprobamos si su contenido es de tipo SIGNEDANDENVELOPED
    	if (!valido) valido = new ValidateCADES().isCADESSignedAndEnvelopedData(new ByteArrayInputStream(plainData));
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
     * @param signFile  Archivo con contiene el archivo firmado.
     * @param type      Tipo de firma que se quiere verificar.
     *
     * @return          La validez del archivo cumpliendo la estructura.
     */
    public static boolean isCADESValid(final InputStream signFile, final String type){

        if (type.equals(AOConstants.BINARY_ENVELOP_DATA)){
            return new ValidateCADES().isCADESData(signFile);
        }else if (type.equals(AOConstants.BINARY_ENVELOP_SIGNEDDATA)){
            return new ValidateCADES().isCADESSignedData(signFile);
        }else if (type.equals(AOConstants.BINARY_ENVELOP_DIGESTEDDATA)){
            return new ValidateCADES().isCADESDigestedData(signFile);
        }else if (type.equals(AOConstants.BINARY_ENVELOP_ENCRYPTEDDATA)){
            return new ValidateCADES().isCADESEncryptedData(signFile);
        }else if (type.equals(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA)){
            return new ValidateCADES().isCADESEnvelopedData(signFile);
        }else if (type.equals(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA)){
            return new ValidateCADES().isCADESSignedAndEnvelopedData(signFile);
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
    public String getDataMimeType(final InputStream signData) throws AOUnsupportedSignFormatException {
		
    	// introducimos los datos en plainData
    	final byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signData);
		} 
		catch (final Throwable e) {
		    throw new NullPointerException("No se han podido leer los datos a firmar: " + e);
		}
		
		//Comprobamos que sea una firma valida
		try{
			this.isSign(new ByteArrayInputStream(plainData));
		}
		catch(final Throwable e1){
			throw new AOUnsupportedSignFormatException("No es un tipo de firma valido: " + e1);
		}
		
		//Extraemos el mimetype y transformamos el OID a mimeType
		return MimeHelper.transformOidToMimeType(
			new ExtractMimeType().extractMimeType(new ByteArrayInputStream(plainData))
		);
		
    }
    
	/**
	 * Inserta un nuevo firmante dentro de una firma signedAndEnveloped dada.
	 * 
	 * @param signFile				Flujo de entrada de datos que contiene la firma.
	 * @param file					Fichero de firma, necesario para calcular los datos del nuevo firmante.
	 * @param signatureAlgorithm	Algoritmo de firma.
	 * @param keyEntry				Clave privada a usar para firmar.
	 * @param Properties			Propiedades necesarias para poder agregar un nuevo firmante.
	 * @return
	 * @throws AOException			Cuando ocurre cualquier problema durante el proceso
	 */
	public byte[] addOriginatorInfo(InputStream signFile, InputStream file, String signatureAlgorithm, PrivateKeyEntry keyEntry, Properties extraParams ) throws AOException {

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
		
		Oid policyQualifier = null;
    	try {
    		policyQualifier = new Oid(extraParams.getProperty("policyQualifier"));
	    }catch(Throwable e) {}

	    final boolean signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2", "false"));
	    
		// Datos firmados.
		byte[] dataSigned = null;

		try {
			dataSigned = new CADESEPESSignedAndEnvelopedData().addOriginatorInfo(signFile, csp, keyEntry, dataType, extraParams.getProperty("policyIdentifier"),policyQualifier , signingCertificateV2);
			
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Ocurrio un error generando el enveloped de CMS: " + e);
			throw new AOException("Ocurrio un error generando el enveloped de CMS", e);
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

    public byte[] getData(InputStream signData) throws AOInvalidFormatException{
    	
    	if(signData == null) {
			Logger.getLogger("es.gob.afirma").warning("Se han introducido datos nulos para su comprobacion");
			return new byte[0];
		}
    	
    	//leemos los datos
    	final byte[] plainData;
		try {
			plainData = AOUtil.getDataFromInputStream(signData);
		} 
		catch (final Throwable e1) {
			throw new AOInvalidFormatException("No se han podido leer los datos a firmar",e1);
		}
		
		// validamos que eson datos firmados.
    	if (isCADESValid(new ByteArrayInputStream(plainData))){
    		return new ObtainContentSignedData().obtainData(new ByteArrayInputStream(plainData));
    	}
   		Logger.getLogger("es.gob.afirma").warning(
			"La firma no es valida. No se pueden sacar los datos que contiene."
		);
    	return new byte[0];
    	
    }

    public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".csig";
	}

    public AOSignInfo getSignInfo(final InputStream signData) throws AOInvalidFormatException, AOException {
        if(signData == null) throw new NullPointerException("No se han introducido datos para analizar");
        
        final byte[] signDataReaded;
        try {
            signDataReaded = AOUtil.getDataFromInputStream(signData);
        } 
        catch (final Throwable e) {
            throw new AOException("No se han podido leer los datos de firma: "+e);
        }
        
        if(!isSign(new ByteArrayInputStream(signDataReaded))) {
            throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma");
        }
        
        return new AOSignInfo(AOConstants.SIGN_FORMAT_CADES); 
        // Aqui podria venir el analisis de la firma buscando alguno de los otros datos de relevancia
        // que se almacenan en el objeto AOSignInfo
        
    };
}
