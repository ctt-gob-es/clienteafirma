package es.gob.afirma.core.misc.protocol;

import java.util.ResourceBundle;

final class ProtocoloMessages {
	private static final String BUNDLE_NAME = "protocolomessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private ProtocoloMessages() {
		// No permitimos la instanciacion
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final Exception e) {
			return '!' + key + '!';
		}
	}
}
