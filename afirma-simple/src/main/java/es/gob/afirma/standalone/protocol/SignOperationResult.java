package es.gob.afirma.standalone.protocol;

import java.security.KeyStore.PrivateKeyEntry;

/**
 * Clase con el resultado de una operaci&oacute;n de firma (el propio
 * resultado y la referencia al certificado y clave empleados).
 */
class SignOperationResult {

	byte[] result;
	PrivateKeyEntry pke;

	public SignOperationResult(final byte[] result, final PrivateKeyEntry pke) {
		this.result = result;
		this.pke = pke;
	}

	public byte[] getResult() {
		return this.result;
	}

	public PrivateKeyEntry getPke() {
		return this.pke;
	}
}
