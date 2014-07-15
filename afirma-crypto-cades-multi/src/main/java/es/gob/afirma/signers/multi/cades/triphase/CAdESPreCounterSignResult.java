package es.gob.afirma.signers.multi.cades.triphase;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import es.gob.afirma.core.misc.Base64;

/** Resultado de una PreFirma trif&aacute;sica.
 * B&aacute;sicamente consiste en una colecci&oacute;n (tantas como firmas se quieran de forma trif&aacute;sica
 * hacer dentro unos mismos datos de una sola vez) de pares de datos a firmar y subarray aleatorio que hay que sustituir
 * dentro de la plantilla de firma por el resultado real de la firma PKCS#1 de estos datos, m&aacute;s la propia plantilla
 * de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CAdESPreCounterSignResult {

	private byte[] preCountersign = null;

	/** Par de subarray aleatorio que hay que sustituir dentro de la plantilla de firma por el resultado real de la
	 *  firma PKCS#1 y datos a firmar con PKCS#1. */
	private final Map<byte[], byte[]> tbs = new LinkedHashMap<byte[], byte[]>();

	/** A&ntilde;ade una firma a realizar en cliente.
	 * @param data Datos que el cliente debe firmar en PKCS#1.
	 * @param randomDummyData Datos aleatorios que debe sustituir por la firma real PKCS#1. */
	void addSign(final byte[] data, final byte[] randomDummyData) {
		if (data == null || randomDummyData == null) {
			throw new IllegalArgumentException(
				"Danto datos a firmar como datos aleatorios a sustituir deben ser nulos" //$NON-NLS-1$
			);
		}
		this.tbs.put(randomDummyData, data);
	}

	/** A&ntilde;ade la plantilla de firma sobre la que insertar las firmas PKCS#11 sustitutendo los datos aleatorios.
	 * @param pcs Plantilla de firma. */
	void addSignTemplate(final byte[] pcs) {
		if (pcs == null) {
			throw new IllegalArgumentException("La platilla de firma no puede ser nula"); //$NON-NLS-1$
		}
		this.preCountersign = pcs.clone();
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("<xml>\n"); //$NON-NLS-1$
		sb.append(" <cs>\n"); //$NON-NLS-1$
		sb.append("  "); //$NON-NLS-1$
		sb.append(Base64.encode(this.preCountersign));
		sb.append('\n');
		sb.append(" </cs>\n"); //$NON-NLS-1$
		sb.append(" <css>\n"); //$NON-NLS-1$

		final Set<byte[]> datas = this.tbs.keySet();
		for (final byte[] data : datas) {
			sb.append("  <dcs>\n"); //$NON-NLS-1$
			sb.append("   <d>\n"); //$NON-NLS-1$
			sb.append("    "); //$NON-NLS-1$
			sb.append(Base64.encode(data));
			sb.append('\n');
			sb.append("   </d>\n"); //$NON-NLS-1$
			sb.append("   <dd>\n"); //$NON-NLS-1$
			sb.append("    "); //$NON-NLS-1$
			sb.append(Base64.encode(this.tbs.get(data)));
			sb.append('\n');
			sb.append("   </dd>\n"); //$NON-NLS-1$
			sb.append("  </dcs>\n"); //$NON-NLS-1$
		}

		sb.append(" </css>\n"); //$NON-NLS-1$
		sb.append("<xml>"); //$NON-NLS-1$
		return sb.toString();
	}

}
