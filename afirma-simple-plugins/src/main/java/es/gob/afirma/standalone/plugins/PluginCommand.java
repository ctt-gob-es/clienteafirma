package es.gob.afirma.standalone.plugins;

public class PluginCommand {

	private final String command;

	private String commandActionClass;

	private String description;

	public PluginCommand(final String command) {
		this.command = command;
	}

	public String getCommand() {
		return this.command;
	}

	public void setCommandActionClass(final String commandActionClass) {
		this.commandActionClass = commandActionClass;
	}

	public String getCommandActionClass() {
		return this.commandActionClass;
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public String getDescription() {
		return this.description;
	}
}
