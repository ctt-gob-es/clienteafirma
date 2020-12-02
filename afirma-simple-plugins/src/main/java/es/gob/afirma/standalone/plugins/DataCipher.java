package es.gob.afirma.standalone.plugins;

public interface DataCipher {

	String cipher(byte[] data) throws EncryptingException;

	byte[] decipher(byte[] cipheredData) throws EncryptingException;
}
