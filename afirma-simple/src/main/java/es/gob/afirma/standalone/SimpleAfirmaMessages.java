/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.ui.LanguageManager;

/** Gesti&oacute;n de mensajes del aplicativo. */
public final class SimpleAfirmaMessages {

    private static final String BUNDLE_NAME = "properties.simpleafirmamessages.simpleafirmamessages"; //$NON-NLS-1$
    private static final String BUNDLE_BASENAME = "simpleafirmamessages"; //$NON-NLS-1$
    private static ResourceBundle bundle;
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    static {
		changeLocale();
    }

    private SimpleAfirmaMessages() { /* No permitimos la instanciacion */ }

    /**
     * Obtiene un mensaje.
     * @param key Clave del mensaje
     * @return Mensaje correspondiente a la clave
     */
    public static String getString(final String key) {
        try {
            return bundle.getString(key);
        } catch (final Exception ignored) {
            // Fallback a base locale
            try {
	            final Locale baseLocale = LanguageManager.readMetadataBaseLocale(Locale.getDefault());
	            final ResourceBundle tempBundle = LanguageManager.isDefaultLocale(baseLocale) ? ResourceBundle.getBundle(BUNDLE_NAME, baseLocale) : setImportedLangResource();
                return tempBundle.getString(key);
            } catch (final Exception ignored2) {
                // Fallback a es_ES
                final Locale esLocale = new Locale("es", "ES");  //$NON-NLS-1$ //$NON-NLS-2$
                final ResourceBundle esBundle = ResourceBundle.getBundle(BUNDLE_NAME, esLocale);
                try {
                    return esBundle.getString(key);
                } catch (final Exception e) {
                    LOGGER.warning("Falta el recurso para la clave: " + key); //$NON-NLS-1$
                    return "!" + key + "!";//$NON-NLS-1$//$NON-NLS-2$
                }
            }
        }
    }

    /**
     * Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto correspondiente segun posicion de los
     * parametros indicados despues de la clave. Las posiciones empiezan a contar
     * por 0. As&iacute;, el texto "%0" se sustituira por el segundo par&aacute;metro
     * de la funci&oacute;n, el texto "%1" por el siguiente,...<br>
     * Introducir m&aacute;s de 10 cadenas distintas para sustituir
     * conllevar&aacute; errores en el resultado.
     * @param key Clave del texto.
     * @param params Par&aacute;metros que se desean insertar.
     * @return Recurso textual con las subcadenas sustituidas.
     */
    public static String getString(final String key, final String... params) {
        String text;
        try {
            text = bundle.getString(key);
        } catch (final Exception ignored) {
            // Fallback a base locale
            try {
	            final Locale baseLocale = LanguageManager.readMetadataBaseLocale(Locale.getDefault());
	            final ResourceBundle tempBundle = LanguageManager.isDefaultLocale(baseLocale) ? ResourceBundle.getBundle(BUNDLE_NAME, baseLocale) : setImportedLangResource();
                text = tempBundle.getString(key);
            } catch (final Exception ignored2) {
                // Fallback a es_ES
                final Locale esLocale = new Locale("es", "ES"); //$NON-NLS-1$ //$NON-NLS-2$
                final ResourceBundle esBundle = ResourceBundle.getBundle(BUNDLE_NAME, esLocale);
                try {
                    text = esBundle.getString(key);
                } catch (final Exception e) {
                    LOGGER.warning("Falta el recurso para la clave: " + key); //$NON-NLS-1$
                    return "!" + key + "!";//$NON-NLS-1$//$NON-NLS-2$
                }
            }
        }

        if (params != null) {
            for (int i = 0; i < params.length; i++) {
                if (params[i] != null) {
                    text = text.replace("%" + i, params[i]); //$NON-NLS-1$
                }
            }
        }
        return text;
    }

    /**
     * Cambia la localizaci&oacute;n a la establecida por defecto.
     */
    public static void changeLocale() {
        final Locale current = Locale.getDefault();
        ResourceBundle rb = null;
        if (LanguageManager.isDefaultLocale(current) && !LanguageManager.existDefaultLocaleNewVersion()) {
            try {
                rb = ResourceBundle.getBundle(BUNDLE_NAME, current);
            }
            catch (final Exception e) {
                LOGGER.log(Level.WARNING, "No existe el bundle interno para Locale " + current + ", se usara por defecto", e); //$NON-NLS-1$ //$NON-NLS-2$
                rb = ResourceBundle.getBundle(BUNDLE_NAME, Locale.ENGLISH);
            }
        }
        else {
            rb = setImportedLangResource();
            if (rb == null) {
                LOGGER.warning("No se encontro recurso externo para Locale " + current + ", usando default interno");//$NON-NLS-1$//$NON-NLS-2$
                try {
                    rb = ResourceBundle.getBundle(BUNDLE_NAME, new Locale("es", "ES")); //$NON-NLS-1$ //$NON-NLS-2$
                }
                catch (final Exception e) {
                    LOGGER.log(Level.SEVERE,
                        "El bundle para el Locale es_ES tampoco esta disponible", e); //$NON-NLS-1$
                }
            }
        }
        bundle = rb;
    }

    private static ResourceBundle setImportedLangResource() {
    	final File localeDir = new File(LanguageManager.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
		final File file = new File(localeDir, BUNDLE_BASENAME + "_" + Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry() + ".properties"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        try (InputStreamReader reader = new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8)) {
        	return new PropertyResourceBundle(reader);
        } catch (final FileNotFoundException e) {
			LOGGER.severe("Recurso para simpleafirmamessages no encontrado: "+ e); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.severe("Error al leer el recurso para simpleafirmamessages: "+ e); //$NON-NLS-1$
		}
        return null;
    }

}
