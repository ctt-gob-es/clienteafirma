package es.gob.afirma.standalone.ui;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;

/**
 * Clase para la ejecuci&oacute;n de una operaci&oacute;n de firma.
 */
public interface SignatureExecutor {

	/**
	 * Inicia una operaci&oacute;n de firma.
	 * @param signConfigs Configuraci&oacute;n de las operaciones de firma.
	 */
	void initSignTask(List<SignOperationConfig> signConfigs);

	/**
	 * Relanza la tarea de firma con una nueva configuraci&oacute;n. Si ya
	 * se seleccion&oacute; el certificado de firma, se puede indicar la
	 * referencia a la clave privada que se debe utilizar.
	 * @param pke Referencia a la clave privada que utilizar.
	 * @param signConfigs Configuraci&oacute;n de las operaciones de firma.
	 */
	void relaunchTask(PrivateKeyEntry pke, List<SignOperationConfig> signConfigs);

	/**
	 * Finaliza la tarea de firma.
	 */
	void finishTask();
}
