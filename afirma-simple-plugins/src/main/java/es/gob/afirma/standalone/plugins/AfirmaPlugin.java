package es.gob.afirma.standalone.plugins;

import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Plugin que aporta informaci&oacute;n adicional a la aplicaci&oacute;n.
 */
public abstract class AfirmaPlugin {

	/**
	 * Proporciona la informaci&oacute;n b&aacute;sica de la aplicaci&oacute;n.
	 * @return Informaci&oacute;n del plugin.
	 */
	public abstract PluginInfo getInfo();

	/**
	 * Proceso ejecutado sobre los datos antes de firma.
	 * @param data Datos que se van a firmar.
	 * @param format Formato de firma que se aplicara sobre los datos. Los
	 * posibles formatos de firma se definen en {@link AOSignConstants}.
	 * @return Datos ya procesados que se van a firmar.
	 */
	@SuppressWarnings("static-method")
	public byte [] preSignProcess(byte[] data, String format) {
		return data;
	}
}
