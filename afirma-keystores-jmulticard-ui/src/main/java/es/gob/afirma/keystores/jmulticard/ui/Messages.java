/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

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

/** Gestor de textos con soporte multi-idioma. */
public final class Messages {
	
    static {
    	updateLocale();
    }

	private Messages() {
		// No permitimos la instanciacion
	}

    private static final String BUNDLE_NAME = "properties.jmulticarduimessages.jmulticarduimessages"; //$NON-NLS-1$
    private static final String BUNDLE_BASENAME = "jmulticarduimessages"; //$NON-NLS-1$
    private static ResourceBundle RESOURCE_BUNDLE;
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    

    /** Recupera el texto identificado con la clave proporcionada.
     * @param codeString Clave del texto
     * @return Texto identificado con la clave proporcionada */
    public static String getString(final String codeString) {
        try {
            return RESOURCE_BUNDLE.getString(codeString);
        }
        catch (final Exception e) {
        	try {
				final Locale baseLocale = LanguageUtils.readMetadataBaseLocale(Locale.getDefault());
				final ResourceBundle temporalBundle;

				if (LanguageUtils.isDefaultLocale(baseLocale)) {
					temporalBundle = ResourceBundle.getBundle(BUNDLE_NAME, baseLocale);
				} else {
					temporalBundle = setImportedLangResource();
				}

				return temporalBundle.getString(codeString);

			} catch (final Exception e1) {
				return '!' + codeString + '!';
			}
        }
    }
    
    /** Recupera el texto identificado con la clave proporcionada y sustituye la
     * subcadenas "%0" por el texto proporcionado.
     * @param key
     *        Clave del texto.
     * @param text
     *        Texto que se desea insertar.
     * @return Recuerso textual con la subcadena sustituida. */
    static String getString(final String key, final String text) {
        try {
            return RESOURCE_BUNDLE.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
			try {
				final Locale baseLocale = LanguageUtils.readMetadataBaseLocale(Locale.getDefault());
				final ResourceBundle temporalBundle;

				if (LanguageUtils.isDefaultLocale(baseLocale)) {
					temporalBundle = ResourceBundle.getBundle(BUNDLE_NAME, baseLocale);
				} else {
					temporalBundle = setImportedLangResource();
				}

				return temporalBundle.getString(key).replace("%0", text); //$NON-NLS-1$

			} catch (final Exception e1) {
				return '!' + key + '!';
			}
        }
    }
    
    private static ResourceBundle setImportedLangResource() {
    	final File localeDir = new File(LanguageUtils.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
		final File file = new File(localeDir, BUNDLE_BASENAME + "_" + Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry() + ".properties"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        try (InputStreamReader reader = new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8)) {
        	return new PropertyResourceBundle(reader);
        } catch (final FileNotFoundException e) {
			LOGGER.severe("Recurso para jmulticarduimessages no encontrado: "+ e); //$NON-NLS-1$
		} catch (final IOException e) {
			LOGGER.severe("Error al leer el recuso para jmulticarduimessages: "+ e); //$NON-NLS-1$
		}
        return null;
    }
    
    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    private static void updateLocale() {
    	if (LanguageUtils.isDefaultLocale(Locale.getDefault()) && !LanguageUtils.existDefaultLocaleNewVersion()) {
    		RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    	} else {
    		RESOURCE_BUNDLE = setImportedLangResource();
    	}
    }
   

}
