package es.gob.afirma.standalone;

import es.gob.afirma.keystores.KeyStoreUtilities;

/**
 * Clase con funciones de utilidad para configurar el uso de JMulticard.
 */
public class JMulticardUtilities {

	/**
	 * Comprobamos si hay establecida alguna de las variables de sistema que alteran el
	 * uso de JMulticard para las tarjetas CERES y DNIe.
	 * @return {@code true} si hay alguna variable de sistema que modifique el comportamiento
	 * por defecto de JMulticard, {@code false} en caso contrario.
	 */
	public static boolean isJMulticardConfigurateBySystem() {
		return System.getenv(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER_ENV) != null ||
				System.getenv(KeyStoreUtilities.DISABLE_CERES_NATIVE_DRIVER_ENV) != null;
	}

	/**
	 * Configura si JMulticard debe utilizarse normalmente o si debe desactivarse su uso.
	 * @param defaultBehavior {@code true} para configurar el comportamiento por defecto de JMulticard
	 * (se utilizar&aacute; para las tarjetas inteligentes soportadas) o {@code false} para desactivar
	 * el uso de JMulticard.
	 */
	public static void configureJMulticard(final boolean defaultBehavior) {
		if (defaultBehavior) {
        	System.clearProperty(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER);
        	System.clearProperty(KeyStoreUtilities.DISABLE_CERES_NATIVE_DRIVER);
        }
        else {
        	System.setProperty(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER, Boolean.TRUE.toString());
        	System.setProperty(KeyStoreUtilities.DISABLE_CERES_NATIVE_DRIVER, Boolean.TRUE.toString());
        }
	}
}
