package es.gob.afirma.signfolder.server.proxy;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;

interface TriPhasePreProcessor {

	// Firma
	byte[] preProcessPreSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams) throws IOException, AOException;
	byte[] preProcessPostSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException;

	// Cofirma
	byte[] preProcessPreCoSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams) throws IOException, AOException;
    byte[] preProcessPostCoSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams) throws NoSuchAlgorithmException, AOException, IOException;

}
