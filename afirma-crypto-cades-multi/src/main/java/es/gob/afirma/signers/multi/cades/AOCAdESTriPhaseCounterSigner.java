/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.PrivateKey;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSimpleSigner;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESValidator;
import es.gob.afirma.signers.multi.cades.CAdESTriPhaseCounterSigner.CAdESPreCounterSignResult;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma trif&aacute;sica CAdES. */
public class AOCAdESTriPhaseCounterSigner implements AOCounterSigner {

	/** {@inheritDoc} */
	@Override
	public byte[] countersign(final byte[] sign,
                              final String algorithm,
                              final CounterSignTarget targetType,
                              final Object[] targets,
                              final PrivateKey key,
                              final java.security.cert.Certificate[] certChain,
                              final Properties xParams) throws AOException, IOException {

        final Properties extraParams = xParams != null ? xParams : new Properties();

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        }
        else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
        }
        else {
        	signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
        }

        String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
        final byte[] data = new AOCAdESSigner().getData(sign);
        if (data != null) {
        	final MimeHelper mimeHelper = new MimeHelper(data);
			contentDescription = mimeHelper.getDescription();
			contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
        }

        // Comprobamos si la firma que nos introducen es SignedData
        if (!CAdESValidator.isCAdESSignedData(sign)) {
        	throw new IllegalArgumentException("Solo se soporta la contrafirma de estructuras SignedData"); //$NON-NLS-1$
        }

        // Comprobamos si es un tipo de contrafirma coportado
    	if (targetType != CounterSignTarget.TREE && targetType != CounterSignTarget.LEAFS) {
    		throw new UnsupportedOperationException("Modo de contrafirma no soportado: " + targetType); //$NON-NLS-1$
    	}


        //***************************************************************************
        //************************** PREFIRMA ***************************************
        //***************************************************************************

        final CAdESPreCounterSignResult preSign;
        try {
        	preSign = new CAdESTriPhaseCounterSigner().preCounterSign(
    		   new P7ContentSignerParameters(
		    		sign,
		    		algorithm
        	   ),
               sign,
               targetType,
               key,
               certChain,
               new AdESPolicy(extraParams),
               signingCertificateV2,
               contentTypeOid,
               contentDescription
            );
        }
        catch (final Exception e) {
            throw new AOException("Error generando la PreContrafirma CAdES: " + e, e); //$NON-NLS-1$
        }

        //***************************************************************************
        //************************** FIN PREFIRMA ***********************************
        //***************************************************************************


        //***************************************************************************
        //************************** FIRMA ******************************************
        //***************************************************************************

        // Tenemos las prefirmas, pasamos a las firmas
        final Properties pre = new Properties();
        pre.load(new ByteArrayInputStream(preSign.toString().getBytes()));

        if (!pre.containsKey(CAdESTriPhaseCounterSigner.KEY_COUNTERSIGN_COUNT) || !pre.containsKey(CAdESTriPhaseCounterSigner.KEY_SIGN)) {
        	throw new AOException("La prefirma no contiene las claves de propiedades obligatorias"); //$NON-NLS-1$
        }

        // Numero de contrafirmas a hacer
        final int csCount = Integer.parseInt(pre.getProperty(CAdESTriPhaseCounterSigner.KEY_COUNTERSIGN_COUNT));

        // Lista para guardar las firmas PKCS#1
        final List<byte[]> pkcs1Signs = new ArrayList<byte[]>(csCount);

        // Firmador PKCS#1
        final AOSimpleSigner p1Signer = new AOPkcs1Signer();

        // Hacemos el lote de firmas PKCS#1
        for (int i=0;i<csCount;i++) {
        	pkcs1Signs.add(
    			i,
    			p1Signer.sign(
	    			Base64.decode(pre.getProperty(CAdESTriPhaseCounterSigner.KEY_SIGNED_DATA + '.' + Integer.toString(i))),
	    			algorithm,
	    			key,
	    			certChain,
	    			extraParams
				)
			);
        }

        // Creamos el Properties de vuelta
        final StringBuilder sb = new StringBuilder(CAdESTriPhaseCounterSigner.KEY_SIGN);
        sb.append(CAdESTriPhaseCounterSigner.EQUALS_SIGN);
        sb.append(pre.getProperty(CAdESTriPhaseCounterSigner.KEY_SIGN));
        sb.append(CAdESTriPhaseCounterSigner.CR);
        sb.append(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN_COUNT);
        sb.append(CAdESTriPhaseCounterSigner.EQUALS_SIGN);
        sb.append(csCount);
        sb.append(CAdESTriPhaseCounterSigner.CR);
        for(int i=0;i<csCount;i++) {
        	sb.append(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN);
        	sb.append('.');
        	sb.append(i);
        	sb.append(CAdESTriPhaseCounterSigner.EQUALS_SIGN);
        	sb.append(Base64.encode(pkcs1Signs.get(i)).replace("\n", "").replace("\r", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        	sb.append(CAdESTriPhaseCounterSigner.CR);
        }
        final String signResult = sb.toString();


        //***************************************************************************
        //************************** FIN FIRMA **************************************
        //***************************************************************************


        //***************************************************************************
        //************************** POSTFIRMA **************************************
        //***************************************************************************

        final Properties signRet = new Properties();
        signRet.load(new ByteArrayInputStream(signResult.getBytes()));

        if (!signRet.containsKey(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN_COUNT) || !signRet.containsKey(CAdESTriPhaseCounterSigner.KEY_SIGN)) {
        	throw new AOException("La firma no contiene las claves de propiedades obligatorias"); //$NON-NLS-1$
        }

        // Ahora vamos sustitutendo los PKCS#1 en la firma
        byte[] signPlacement = Base64.decode(signRet.getProperty(CAdESTriPhaseCounterSigner.KEY_SIGN));
        final int p1Count = Integer.parseInt(signRet.getProperty(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN_COUNT));
        for (int i=0;i<p1Count;i++) {
        	// El PKCS#1 de verdad
        	final byte[] realP1 = Base64.decode(signRet.getProperty(CAdESTriPhaseCounterSigner.KEY_PKCS1_SIGN + '.' + Integer.toString(i)));
        	// El troncho a buscar y sustituir
        	final byte[] hop = new byte[CAdESTriPhaseCounterSigner.PKCS1_DEFAULT_SIZE];
        	Arrays.fill(hop, (byte) Integer.toString(i).toCharArray()[0]);
        	signPlacement = searchAndReplace(signPlacement, hop, realP1);
        }

        return signPlacement;

        //***************************************************************************
        //************************** FIN POSTFIRMA **********************************
        //***************************************************************************
    }

    private static byte[] searchAndReplace(final byte[] array, final byte[] search, final byte[] replace) {
        if (search.length != replace.length) {
			return array;
		}
        int p = searchFor(array, search);
        if (p == -1) {
			throw new IllegalArgumentException("No se ha encontrado la cadena a sustituir"); //$NON-NLS-1$
		}
        final byte[] result = Arrays.copyOf(array, array.length);
        for (final byte element : replace) {
            result[p] = element;
            p++;
        }
        return result;
    }

    private static int searchFor(final byte[] array, final byte[] subArray) {
        if (subArray.length > array.length) {
			return -1;
		}
        final int p = new String(array).indexOf(new String(subArray));
        for (int i = 1; i < subArray.length; i++) {
            if (array[p + i] != subArray[i]) {
				return -1;
			}
        }
        return p;
    }

}
