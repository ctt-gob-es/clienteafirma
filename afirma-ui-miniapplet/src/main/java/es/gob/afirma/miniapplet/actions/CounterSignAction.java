package es.gob.afirma.miniapplet.actions;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada para realizar una contrafirma electr&oacute;nica.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public class CounterSignAction implements PrivilegedExceptionAction<byte[]> {

	private AOSigner signer;
	
	private byte[] sign;
	
	private String algorithm;
	
	private PrivateKeyEntry keyEntry;
	
	private Properties extraParams;
	
	/**
	 * Crea la acci&oacute;n para contrafirmar una firma electr&oacute;nica.
	 * @param signer Manejador de firma.
	 * @param sign Firma que se desea contrafirmar.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la contrafirma.
	 */
	public CounterSignAction(AOSigner signer, byte[] sign, String algorithm, PrivateKeyEntry keyEntry, Properties extraParams) {
		this.signer = signer;
		this.sign = sign;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}
	
	@Override
	public byte[] run() throws Exception {
		return this.signer.countersign(this.sign, this.algorithm,
				CounterSignTarget.LEAFS, null, this.keyEntry, this.extraParams);
	}
}
