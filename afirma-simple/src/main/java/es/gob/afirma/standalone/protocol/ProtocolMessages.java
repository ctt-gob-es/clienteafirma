package es.gob.afirma.standalone.protocol;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

final class ProtocolMessages {
	private static final String BUNDLE_NAME = "properties/protocolmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private ProtocolMessages() {
		// No permitimos la instanciacion
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
