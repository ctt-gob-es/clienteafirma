package es.gob.afirma.standalone.plugins;

import java.util.Arrays;

/**
 * Acci&oacute;n ejecutable desde l&iacute;nea de comandos.
 */
public abstract class PluginCommandAction {

	private final String command;

	private AfirmaPlugin plugin;

	/**
	 * Construye la acci&oacute;n para un comando concreto.
	 * @param command Comando ante el que se desencadenar&aacute; la acci&oacute;n.
	 */
	public PluginCommandAction(final String command) {
		this.command = command;
	}

	/**
	 * Recupera el comando que desencadena la acci&oacute;n. Este comando no puede coincidir
	 * con otro comando ya existente o puede que nunca se ejecute la acci&oacute;n.
	 * @return Comando.
	 */
	public String getCommand() {
		return this.command;
	}

	/**
	 * Establece el plugin al que pertenece la acci&oacute;n.
	 * @param plugin Plugin al que pertenece la acci&oacute;n.
	 */
	public final void setPlugin(final AfirmaPlugin plugin) {
		this.plugin = plugin;
	}

	/**
	 * Recupera el plugin al que pertenece la acci&oacute;n.
	 * @return Plugin al que pertenece la acci&oacute;n.
	 */
	public final AfirmaPlugin getPlugin() {
		return this.plugin;
	}

	/**
	 * Obtiene un texto de ayuda con la sintaxis y detalles del comando.
	 * @return Texto de ayuda.
	 */
	public abstract String getHelpText();

	/**
	 * Ejecuta la acci&oacute;n con unos argumentos recibidos por l&iacute;nea de comandos.
	 * @param args Argumentos para ejecutar la acci&oacute;n.
	 * @return Texto que se devolver&aacute; por l&iacute;nea de comandos tras la ejecuci&oacute;n
	 * o {@code null} si no se devuelve nada.
	 * @throws IllegalArgumentException Cuando hay un error en el comando de llamada. En este caso,
	 * el mensajede la excepci&oacute;n describir&aacute; el problema.
	 * @throws PluginControlledException Cuando ha ocurrido un error durante la ejecuci&oacute;n de
	 * la operaci&oacute;n. En este caso, el mensaje de la excepci&oacute;n se mostrar&aacute; al
	 * usuario.
	 */
	public final String start(final String[] args)
			throws IllegalArgumentException, PluginControlledException {
		String[] processedArgs;
		if (args == null) {
			processedArgs = null;
		} else if (args.length > 0 && this.command.equalsIgnoreCase(args[0])) {
			processedArgs = Arrays.copyOfRange(args, 1, args.length);
		} else {
			processedArgs = Arrays.copyOf(args, args.length);
		}
		return process(processedArgs);
	}

	/**
	 * Operaci&oacute;n que se deber&aacute; ejecutar al iniciarse el comando.
	 * @param args Argumentos recibidos para la ejecuci&oacute;n del comando (omitiendo el propio
	 * comando).
	 * @return Texto que se devolver&aacute; por l&iacute;nea de comandos tras la ejecuci&oacute;n
	 * o {@code null} si debe imprimir nada.
	 * @throws IllegalArgumentException Cuando hay un error en el comando de llamada. En este caso,
	 * el mensajede la excepci&oacute;n describir&aacute; el problema.
	 * @throws PluginControlledException Cuando ha ocurrido un error durante la ejecuci&oacute;n de
	 * la operaci&oacute;n. En este caso, el mensaje de la excepci&oacute;n se mostrar&aacute; al
	 * usuario.
	 */
	protected abstract String process(String[] args)
			throws IllegalArgumentException, PluginControlledException;
}
