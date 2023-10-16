package es.gob.afirma.standalone.configurator.common;

import java.security.MessageDigest;

import es.gob.afirma.core.misc.AOUtil;

public class ConfigDataInfo {

	private static final String HASH_ALGORITHM = "SHA-256"; //$NON-NLS-1$

	private final byte[] data;
	private String hash;

	public ConfigDataInfo(final byte[] data) {
		this.data = data;

		try {
			final byte[] hashValue = MessageDigest.getInstance(HASH_ALGORITHM).digest(this.data);
			this.hash = AOUtil.hexify(hashValue, false);
		} catch (final Exception e) {
			this.hash = ""; //$NON-NLS-1$
		}
	}

	public byte[] getData() {
		return this.data;
	}

	public String getHash() {
		return this.hash;
	}
}
