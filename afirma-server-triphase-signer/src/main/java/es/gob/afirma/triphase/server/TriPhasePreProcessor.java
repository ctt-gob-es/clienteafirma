package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;

interface TriPhasePreProcessor {

	// Firma
	byte[] preProcessPreSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams) throws IOException, AOException;
	byte[] preProcessPostSign(byte[] data, String algorithm, X509Certificate cert, Properties extraParams, byte[] session) throws NoSuchAlgorithmException, AOException, IOException;

	// Cofirma
	byte[] preProcessPreCoSign(byte[] sign, String algorithm, X509Certificate cert, Properties extraParams) throws IOException, AOException;
	byte[] preProcessPostCoSign(byte[] sign, String algorithm, X509Certificate cert, Properties extraParams, byte[] session) throws NoSuchAlgorithmException, AOException, IOException;

	// Contrafirma
	byte[] preProcessPreCounterSign(byte[] sign, String algorithm, X509Certificate cert, Properties extraParams, CounterSignTarget targets) throws IOException, AOException;
	byte[] preProcessPostCounterSign(byte[] sign, String algorithm, X509Certificate cert, Properties extraParams, byte[] session, CounterSignTarget targets) throws NoSuchAlgorithmException, AOException, IOException;

}
