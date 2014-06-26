package es.gob.afirma.crypto.jarverifier;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

final class JarSignatureCertExtractorMessages {

	private static final String BUNDLE_NAME = "JarSignatureCertExtractorMessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private JarSignatureCertExtractorMessages() {
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
