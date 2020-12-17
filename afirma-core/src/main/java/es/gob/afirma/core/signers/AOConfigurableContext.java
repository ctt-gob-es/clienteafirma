package es.gob.afirma.core.signers;

/**
 * Interfaz con los m&eacute;todos para gestionar si el objeto permite configurar
 * el estado de su contexto.
 */
public interface AOConfigurableContext {

	/**
	 * Configura el modo seguro.
	 * @param secure {@code true} para configurar el modo seguro, {@code false}
	 * para desactivarlo.
	 */
	void setSecureMode(boolean secure);

	/**
	 * Indica si se encuentra activado el modo seguro.
	 * @return {@code true} si el modo seguro est&aacute; activado, {@code false}
	 * en caso contrario.
	 */
	boolean isSecureMode();
}
