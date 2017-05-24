package es.gob.afirma.standalone.ui.restoreconfig;

/** Interfaz que define los m&eacute;todos necesarios para realizar la restauraci&oacute;n
 *  de la configuraci&oacute;n de navegadores de AutoFirma.
 **/
interface RestoreConfig {

	/** Repara la configuraci&oacute;n del entorno para la ejecuci&oacute;n de AutoFirma.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n. */
	void restore(RestoreConfigPanel configPanel);

}
