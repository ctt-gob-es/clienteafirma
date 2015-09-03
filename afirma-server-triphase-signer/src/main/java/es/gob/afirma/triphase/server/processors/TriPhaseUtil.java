package es.gob.afirma.triphase.server.processors;

import java.util.Properties;
import java.util.UUID;

final class TriPhaseUtil {

	private static final String PROP_ID = "SignatureId"; //$NON-NLS-1$

	private TriPhaseUtil() {
		// No instanciable
	}

	static String getSignatureId(final Properties extraParams) {
		if (extraParams == null) {
			return UUID.randomUUID().toString();
		}
		final String id = extraParams.getProperty(PROP_ID);
		if (id == null) {
			return UUID.randomUUID().toString();
		}
		return id;
	}

}
