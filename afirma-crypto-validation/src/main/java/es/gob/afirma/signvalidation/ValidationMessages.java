/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

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

/** Gesti&oacute;n de mensajes del aplicativo. */
public final class ValidationMessages {

    private static final String BUNDLE_NAME = "validationmessages.validationmessages"; //$NON-NLS-1$
    private static final String BUNDLE_BASENAME = "validationmessages"; //$NON-NLS-1$
    private static ResourceBundle bundle;
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    static {
    	updateLocale();
    }

    private ValidationMessages() { /* No permitimos la instanciacion */ }

    /** Obtiene un mensaje.
     * @param key Clave del mensaje
     * @return Mensaje correspondiente a la clave */
    public static String getString(final String key) {
        try {
            return bundle.getString(key);
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

    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto correspondiente segun posicion de los
     * parametros indicados despues de la clave. Las posiciones empiezan a contar
     * por 0. As&iacute;, el texto "%0" se sustituira por el segundo par&aacute;metro
     * de la funci&oacute;n, el texto "%1" por el siguiente,...<br>
     * Introducir m&aacute;s de 10 cadenas distintas para sustituir
     * conllevar&aacute; errores en el resultado.
     * @param key Clave del texto.
     * @param params Par&aacute;metros que se desean insertar.
     * @return Recurso textual con las subcadenas sustituidas. */
    public static String getString(final String key, final String... params) {

        String text;
        try {
            text = bundle.getString(key);
        }
        catch (final Exception e) {
        	if(!LanguageManager.isDefaultLocale(Locale.getDefault())) {
        		try {
					final Locale baseLocale = LanguageManager.readMetadataBaseLocale(Locale.getDefault());
					final ResourceBundle temporalBundle;
					
					if(LanguageManager.isDefaultLocale(baseLocale)) {
						temporalBundle = ResourceBundle.getBundle(BUNDLE_NAME, baseLocale);
					} else {
						temporalBundle = setImportedLangResource();
					}
					
					text = temporalBundle.getString(key);
					
				} catch (final Exception e1) {
					return '!' + key + '!';
				}          	
        	}
        	
			return '!' + key + '!';   
        }

        if (params != null && params.length > 0) {
            for (int i = 0; i < params.length; i++) {
            	if (params[i] != null) {
            		text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            	}
            }
        }

        return text;
    }

    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    public static void updateLocale() {
    	if (LanguageManager.isDefaultLocale(Locale.getDefault()) && !LanguageManager.existDefaultLocaleNewVersion()) {
    		bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    	} else {
    		bundle = setImportedLangResource();
    	}
    }
    
    private static ResourceBundle setImportedLangResource() {
    	final File localeDir = new File(LanguageManager.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
		final File file = new File(localeDir, BUNDLE_BASENAME + "_" + Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry() + ".properties"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        try (InputStreamReader reader = new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8)) {
        	return new PropertyResourceBundle(reader);
        } catch (final FileNotFoundException e) {
			LOGGER.severe("Recurso para validationmessages no encontrado: "+ e); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.severe("Error al leer el recurso para validationmessages: "+ e); //$NON-NLS-1$
		}
        return null;
    }

}
