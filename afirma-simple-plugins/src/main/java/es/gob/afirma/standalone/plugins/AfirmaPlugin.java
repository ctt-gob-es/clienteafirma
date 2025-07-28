package es.gob.afirma.standalone.plugins;

import java.lang.reflect.Constructor;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Plugin que aporta informaci&oacute;n adicional a la aplicaci&oacute;n.
 */
public abstract class AfirmaPlugin {

	private PluginInfo info = null;

	private SignDataProcessor inlineProcessor = null;

	private ClassLoader classLoader = null;

	/**
	 * Proporciona la informaci&oacute;n b&aacute;sica de la aplicaci&oacute;n.
	 * @return Informaci&oacute;n del plugin.
	 */
	public final PluginInfo getInfo() {
		return this.info;
	}

	/**
	 * Establece la informaci&oacute;n b&aacute;sica de la aplicaci&oacute;n.
	 * @param info Informaci&oacute;n del plugin.
	 */
	public final void setInfo(final PluginInfo info) {
		this.info = info;
	}

	/**
	 * Proporciona la informaci&oacute;n b&aacute;sica de la aplicaci&oacute;n.
	 * @return Informaci&oacute;n del plugin.
	 */
	public final ClassLoader getClassLoader() {
		return this.classLoader;
	}

	/**
	 * Establece el ClassLoader con el que se cargar&aacute;n las clases del plugin.
	 * @param classLoader ClassLoader con el que se cargar&aacute;n las clases del plugin.
	 */
	public final void setClassLoader(final ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	/**
	 * Permite recuperar la configuraci&oacute;n establecida en el plugin.
	 * @return Configuraci&oacute;n del plugin.
	 */
	public final Properties getConfig() {
		return PluginPreferences.getInstance(this).recoverConfig();
	}

	/**
	 * Construye una instancia del panel de configuraci&oacute;n.
	 * @return Panel de configuraci&oacute;n construido.
	 * @throws ReflectiveOperationException Cuando ocurre un error al construir el panel.
	 */
	public final ConfigurationPanel buildConfigurationPanel() throws ReflectiveOperationException {

		final String configPanelClass = getInfo().getConfigPanel();
		if (configPanelClass == null) {
			return null;
		}

		ConfigurationPanel panel;
		try {
			final Class<?> panelClass = Class.forName(
					configPanelClass, true, this.getClass().getClassLoader());
			panel = (ConfigurationPanel) panelClass.newInstance();
		} catch (final Exception e) {
			throw new ReflectiveOperationException("No se pudo crear el panel de configuracion", e); //$NON-NLS-1$
		}
		return panel;
	}

	/**
	 * Proceso ejecutado sobre los datos antes de firma.
	 * @param data Datos que se van a firmar.
	 * @param format Formato de firma que se aplicara sobre los datos. Los
	 * posibles formatos de firma se definen en {@link AOSignConstants}.
	 * @return Datos ya procesados que se van a firmar.
	 * @throws PluginControlledException Cuando se produce un error en el procesado
	 * de los datos.
	 */
	@SuppressWarnings("static-method")
	public byte[] preSignProcess(final byte[] data, final String format)
			throws PluginControlledException {
		return data;
	}

	/**
	 * Proceso ejecutado sobre las firmas generadas.
	 * @param signature Firma electr&oacute;nica generada.
	 * @param format Formato de la firma. Los posibles formatos de firma se
	 * definen en {@link AOSignConstants}.
	 * @param certChain Cadena de certificaci&oacute;n usada en al firma.
	 * @return Firma ya posprocesada.
	 * @throws PluginControlledException Cuando se produce un error en el procesado
	 * de la firma.
	 */
	@SuppressWarnings("static-method")
	public byte[] postSignProcess(final byte[] signature, final String format, final Certificate[] certChain)
			throws PluginControlledException {
		return signature;
	}

	/**
	 * Obtiene el procesador para peticiones en linea asociado al plugin.
	 * @param protocolVersion Versi&oacute;n del protocolo declarado por Autofirma.
	 * @return Procesador de peticiones de firma en l&iacute;nea o {@code null} si el
	 * plugin no lo establece.
	 * @throws PluginControlledException Cuando se produce un error durante la carga del
	 * procesador.
	 */
	public final SignDataProcessor getInlineProcessor(final int protocolVersion) throws PluginControlledException {
		if (this.inlineProcessor == null) {
			final String processorClassName = this.info.getInlineProcessorClassname();
			if (processorClassName != null) {
				try {
					final Class<SignDataProcessor> processorClass =
							(Class<SignDataProcessor>) Class.forName(processorClassName, true, this.getClass().getClassLoader());

					final Constructor<SignDataProcessor> processorConstructor =
							processorClass.getConstructor(Integer.TYPE);

					this.inlineProcessor = processorConstructor.newInstance(protocolVersion);
				}
				catch (final Exception e) {
					throw new PluginControlledException("No se ha podido cargar la clase de procesado en linea", e); //$NON-NLS-1$
				}
			}
		}
		return this.inlineProcessor;
	}

	/**
	 * Proceso ejecutado al instalar el plugin en Autofirma. Esto s&oacute;lo se
	 * ejecutar&aacute; una vez a lo largo de vida del plugin. Este proceso s&oacute;lo
	 * tendr&aacute; permisos de administrador si el usuario ejecut&oacute; Autofirma
	 * con estos.
	 * @throws PluginControlledException Cuando ocurre un error durante el proceso.
	 */
	public void install() throws PluginControlledException {
		// Por defecto, no se hace nada
	}

	/**
	 * Proceso ejecutado al desinstalar el plugin de Autofirma. Este proceso deber&iacute;
	 * eliminar cualquier resto o referencia que se haya dejado en el sistema como parte del
	 * proceso de instalaci&oacute;n o de firma.
	 * @throws PluginControlledException Cuando ocurre un error durante el proceso.
	 */
	public void uninstall() throws PluginControlledException {
		// Por defecto, no se hace nada
	}

	/**
	 * M&eacute;todo ejecutado al finalizar un proceso de firma completo. En este
	 * proceso pueden, por ejemplo, haberse realizado m&uacute;ltiples firmas como
	 * parte de una operaci&oacute;n de firma masiva.
	 * En este m&eacute;todo se puede configurar lo necesario para restaurar el
	 * estado del plugin o liberar recursos antes de una nueva operaci&oacute;n.
	 * @throws PluginControlledException Cuando falla el proceso de reinicio.
	 */
	public void reset() throws PluginControlledException {
		// No se hace nada
	}

	@Override
	public final boolean equals(final Object obj) {
		if (obj != null && obj instanceof AfirmaPlugin) {
			final PluginInfo myInfo = getInfo();
			final PluginInfo objInfo = ((AfirmaPlugin) obj).getInfo();
			return myInfo.getInternalName().equals(objInfo.getInternalName());
		}
		else if (obj != null && obj instanceof PluginInfo) {
			final PluginInfo myInfo = getInfo();
			final PluginInfo objInfo = (PluginInfo) obj;
			return myInfo.getInternalName().equals(objInfo.getInternalName());
		}
		return super.equals(obj);
	}

	@Override
	public final int hashCode() {
		return super.hashCode();
	}

	@Override
	public final String toString() {
		return this.info != null ? this.info.toString() : super.toString();
	}

}
