package es.gob.afirma.triphase.signer.xades;

import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;

interface KeyHelper {

	PrivateKey getPrivateKey(final PublicKey puK) throws NoSuchAlgorithmException;

}
