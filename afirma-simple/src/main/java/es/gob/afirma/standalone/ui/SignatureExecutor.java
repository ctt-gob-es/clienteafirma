package es.gob.afirma.standalone.ui;

import java.util.List;

/**
 * Clase para la ejecuci&oacute;n de una operaci&oacute;n de firma.
 */
public interface SignatureExecutor {

	/**
	 * Inicia una operaci&oacute;n de firma.
	 * @param signConfigs Configuraci&oacute;n de la operaci&oacute;n de firma.
	 */
	void initSignTask(List<SignOperationConfig> signConfigs);

	/**
	 * Finaliza la tarea de firma.
	 */
	void finishTask();
}
