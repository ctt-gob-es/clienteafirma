package es.gob.afirma.ui.utils;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Clase para acceder a los mensajes del properties sin lanzar excepciones de error
 */
public class Messages {

	private static ResourceBundle bundle = ResourceBundle.getBundle("resources/properties/Idioma", Locale.getDefault());
	
	public static String getString(String codeString) {
		try {
			return bundle.getString(codeString);
		} catch (MissingResourceException e) {
			return "##ERROR##Cadena no disponible.";
		}
	}
	
	public static void changeLocale () {
		bundle = ResourceBundle.getBundle("properties/Idioma", Locale.getDefault());
	}
}
