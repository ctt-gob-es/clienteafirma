/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.certificateselection;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.logging.Logger;

import es.gob.afirma.core.ui.LanguageManager;

/** Clase para la obtenci&oacute;n de los recursos textuales del di&aacute;logo de selecci&oacute;n de certificados. */
final class CertificateSelectionDialogMessages {

    private static final String BUNDLE_NAME = "certdialogmessages.certdialogmessages"; //$NON-NLS-1$
    private static final String BUNDLE_BASENAME = "certdialogmessages"; //$NON-NLS-1$
    private static ResourceBundle RESOURCE_BUNDLE;
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private CertificateSelectionDialogMessages() {
        // No permitimos la instanciacion
    }

    /** Recupera el texto identificado con la clave proporcionada.
     * @param key Clave del texto.
     * @return Recurso textual. */
    public static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
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

    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto en la posici&oacute;n 'i' del array
     * proporcionado.
     * @param key
     *        Clave del texto.
     * @param params
     *        Par&aacute;metros que se desean insertar.
     * @return Recuerso textual con las subcadenas sustituidas. */
    public static String getString(final String key, final String... params) {

        String text;
        try {
            text = RESOURCE_BUNDLE.getString(key);
        }
		catch (final Exception e) {
			try {
				final Locale baseLocale = LanguageManager.readMetadataBaseLocale(Locale.getDefault());
				final ResourceBundle temporalBundle;

				if (LanguageManager.isDefaultLocale(baseLocale)) {
					temporalBundle = ResourceBundle.getBundle(BUNDLE_NAME, baseLocale);
				} else {
					temporalBundle = setImportedLangResource();
				}

				text = temporalBundle.getString(key);

			} catch (final Exception e1) {
				return '!' + key + '!';
			}
		}
        
        if (params != null) {
            for (int i = 0; i < params.length; i++) {
                text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            }
        }

        return text;
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
			LOGGER.severe("Recurso para certificatedialogmessages no encontrado: "+ e); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.severe("Error al leer el recuso para certificatedialogmessages: "+ e); //$NON-NLS-1$
		}
        return null;
    }

}
