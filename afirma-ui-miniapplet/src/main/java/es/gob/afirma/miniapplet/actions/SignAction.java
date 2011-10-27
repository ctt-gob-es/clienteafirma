package es.gob.afirma.miniapplet.actions;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada de firma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public class SignAction implements PrivilegedExceptionAction<byte[]> {

	private AOSigner signer;
	
	private byte[] data;
	
	private String algorithm;
	
	private PrivateKeyEntry keyEntry;
	
	private Properties extraParams;
	
	/**
	 * Crea la acci&oacute;n para la firma de datos.
	 * @param signer Manejador de firma.
	 * @param data Datos que se desean firmar.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la firma.
	 */
	public SignAction(AOSigner signer, byte[] data, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) {
		this.signer = signer;
		this.data = data;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}
	
	@Override
	public byte[] run() throws Exception {
		return this.signer.sign(this.data, this.algorithm, this.keyEntry, this.extraParams);
	}

}
