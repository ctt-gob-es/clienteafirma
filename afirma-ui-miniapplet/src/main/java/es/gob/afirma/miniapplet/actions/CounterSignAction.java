package es.gob.afirma.miniapplet.actions;

import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants.CounterSignTarget;
import es.gob.afirma.core.signers.AOSigner;

/**
 * Acci&oacute;n privilegiada para realizar una contrafirma electr&oacute;nica.
 * Se contrafirmar&aacute;n todos los nodos hoja de la firma, salvo que se
 * especifique lo contrario.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class CounterSignAction implements PrivilegedExceptionAction<byte[]> {

	private static final String COUNTERSIGN_TARGET_KEY = "target"; //$NON-NLS-1$
	
	private static final String COUNTERSIGN_TARGET_TREE = "tree"; //$NON-NLS-1$
	
	private final AOSigner signer;
	private final byte[] sign;
	private final String algorithm;
	private final PrivateKeyEntry keyEntry;
	private final Properties extraParams;
	
	/**
	 * Crea la acci&oacute;n para contrafirmar una firma electr&oacute;nica.
	 * @param signer Manejador de firma.
	 * @param sign Firma que se desea contrafirmar.
	 * @param algorithm Algoritmo de firma.
	 * @param keyEntry Clave privada de firma.
	 * @param extraParams Datos adicionales para la configuraci&oacute;n de la contrafirma.
	 */
	public CounterSignAction(final AOSigner signer, 
	                         final byte[] sign, 
	                         final String algorithm, 
	                         final PrivateKeyEntry keyEntry, 
	                         final Properties extraParams) {
		this.signer = signer;
		this.sign = sign;
		this.algorithm = algorithm;
		this.keyEntry = keyEntry;
		this.extraParams = extraParams;
	}
	
	public byte[] run() throws Exception {
		CounterSignTarget target = CounterSignTarget.LEAFS;
		if (this.extraParams.containsKey(COUNTERSIGN_TARGET_KEY)) {
			final String targetValue = this.extraParams.getProperty(COUNTERSIGN_TARGET_KEY).trim();
			if (COUNTERSIGN_TARGET_TREE.equals(targetValue)) {
				target = CounterSignTarget.TREE;
			}
		}
		
		return this.signer.countersign(this.sign, this.algorithm,
				target, null, this.keyEntry, this.extraParams);
	}
}
