package es.gob.afirma.miniapplet.actions;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada para realizar una cofirma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public class CoSignAction implements PrivilegedExceptionAction<byte[]> {

	private AOSigner signer;
	
	private byte[] sign;
	
	private byte[] data;
	
	private String algorithm;
	
	private PrivateKeyEntry keyEntry;
	
	private Properties extraParams;
	
	/**
	 * Crea la acci&oacute;n para cofirmar de una firma electr&oacute;nica.
	 * @param signer Manejador de firma.
	 * @param sign Firma que se desea cofirmar.
	 * @param data Datos que se firmaron originalmente.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la contrafirma.
	 */
	public CoSignAction(AOSigner signer, byte[] sign, byte[] data, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) {
		this.signer = signer;
		this.sign = sign;
		this.data = data;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}
	
	@Override
	public byte[] run() throws Exception {
		System.out.println(this.data);
		if (this.data == null) {
			return this.signer.cosign(this.sign, this.algorithm, this.keyEntry, this.extraParams);
		}
		return this.signer.cosign(this.data, this.sign, this.algorithm, this.keyEntry, this.extraParams);
	}
}
