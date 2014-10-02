package es.gob.afirma.crypto.handwritten;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

class HandwrittenMessages {
	private static final String BUNDLE_NAME = "handwrittenmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private HandwrittenMessages() {
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
