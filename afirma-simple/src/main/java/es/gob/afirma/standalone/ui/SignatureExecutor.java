package es.gob.afirma.standalone.ui;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;

import es.gob.afirma.keystores.AOKeystoreAlternativeException;

/**
 * Clase para la ejecuci&oacute;n de una operaci&oacute;n de firma.
 */
public interface SignatureExecutor {

	/**
	 * Inicia una operaci&oacute;n de firma.
	 * @param signConfigs Configuraci&oacute;n de las operaciones de firma.
	 * @throws AOKeystoreAlternativeException Error al seleccionar el tipo de certificado.
     * @throws IOException Error al leer almac&eacute;n.
	 */
	void initSignTask(List<SignOperationConfig> signConfigs) throws AOKeystoreAlternativeException, IOException;

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
