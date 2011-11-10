package es.gob.afirma.miniapplet.actions;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada para realizar una cofirma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class CoSignAction implements PrivilegedExceptionAction<byte[]> {

	private final AOSigner signer;
	private final byte[] sign;
	private final byte[] data;
	private final String algorithm;
	private final PrivateKeyEntry keyEntry;
	private final Properties extraParams;
	
	/**
	 * Crea la acci&oacute;n para cofirmar de una firma electr&oacute;nica.
	 * @param signer Manejador de firma.
	 * @param sign Firma que se desea cofirmar.
	 * @param data Datos que se firmaron originalmente.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la contrafirma.
	 */
	public CoSignAction(final AOSigner signer, 
	                    final byte[] sign, 
	                    final byte[] data, 
	                    final String algorithm, 
	                    final PrivateKeyEntry keyEntry, 
	                    final Properties extraParams) {
		this.signer = signer;
		this.sign = (sign != null ? sign.clone() : null);
		this.data = (data != null ? data.clone() : null);
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}
	
	public byte[] run() throws Exception {
		if (this.data == null) {
			return this.signer.cosign(this.sign, this.algorithm, this.keyEntry, this.extraParams);
		}
		return this.signer.cosign(this.data, this.sign, this.algorithm, this.keyEntry, this.extraParams);
	}
}
