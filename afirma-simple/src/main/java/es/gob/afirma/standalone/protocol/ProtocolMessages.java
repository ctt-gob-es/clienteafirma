/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.logging.Logger;

import es.gob.afirma.core.ui.LanguageManager;

final class ProtocolMessages {
	
	private static final String BUNDLE_NAME = "properties.protocolmessages.protocolmessages"; //$NON-NLS-1$
	private static final String BUNDLE_BASENAME = "protocolmessages"; //$NON-NLS-1$
	private static ResourceBundle RESOURCE_BUNDLE;
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	static {
	    updateLocale();
	}

	private ProtocolMessages() {
		// No permitimos la instanciacion
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			try {
				final Locale baseLocale = LanguageManager.readMetadataBaseLocale(Locale.getDefault());
				final ResourceBundle temporalBundle;

				if (LanguageManager.isDefaultLocale(baseLocale)) {
					temporalBundle = ResourceBundle.getBundle(BUNDLE_NAME, baseLocale);
				} else {
					temporalBundle = setImportedLangResource();
				}

				return temporalBundle.getString(key);

			} catch (final Exception e1) {
				return '!' + key + '!';
			}
		}
	}
	
    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    public static void updateLocale() {
    	if (LanguageManager.isDefaultLocale(Locale.getDefault()) && !LanguageManager.existDefaultLocaleNewVersion()) {
    		RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    	} else {
    		RESOURCE_BUNDLE = setImportedLangResource();
    	}
    }
	
    private static ResourceBundle setImportedLangResource() {
    	final File localeDir = new File(LanguageManager.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
		final File file = new File(localeDir, BUNDLE_BASENAME + "_" + Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry() + ".properties"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        try (InputStreamReader reader = new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8)) {
        	return new PropertyResourceBundle(reader);
        } catch (final FileNotFoundException e) {
			LOGGER.severe("Recurso para protocolmessages no encontrado: "+ e); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.severe("Error al leer el recuso para protocolmessages: "+ e); //$NON-NLS-1$
		}
        return null;
    }
}
