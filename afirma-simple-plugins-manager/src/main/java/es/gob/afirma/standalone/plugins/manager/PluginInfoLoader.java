package es.gob.afirma.standalone.plugins.manager;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;

import es.gob.afirma.standalone.plugins.GenericMenuOption;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginButton;
import es.gob.afirma.standalone.plugins.PluginCommand;
import es.gob.afirma.standalone.plugins.PluginInfo;

/**
 * Clase para la carga de la informaci&oacute;n de los plugins.
 */
public class PluginInfoLoader {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Obtiene la informaci&oacute;n de un plugin.
	 * @param reader Lector para la obtenci&oacute;n de la informaci&oacute;n del plugin.
	 * @return Informaci&oacute;n del plugin.
	 * @throws PluginException Cuando no se puede leer la informaci&oacute;n del plugin
	 * o la informaci&oacute;n encontrada est&aacute; mal formada.
	 */
	static PluginInfo parseInfo(final InputStreamReader reader) throws PluginException {

		PluginInfo info;
		try (final JsonReader jsonReader = Json.createReader(reader)) {
			final JsonObject mainObject = jsonReader.readObject();

			// Parseamos la informacion del plugin

			info = parseInfoObject(mainObject);

			final Permission[] permissions = parsePermissionsObject(mainObject);
			if (permissions != null) {
				info.setPermissions(permissions);
			}

			final String inlineProcessorClassname = parseInlineProcessor(mainObject);
			info.setInlineProcessorClassname(inlineProcessorClassname);

			final GenericMenuOption menu = parseMenuObject(mainObject);
			info.setMenu(menu);

			final PluginButton[] buttons = parseButtonsObject(mainObject);
			info.setButtons(buttons);

			final PluginCommand[] commands = parseCommandsObject(mainObject);
			info.setCommands(commands);
		}

		return info;
	}

	private static PluginInfo parseInfoObject(final JsonObject mainObject) throws PluginException {
		final JsonObject infoObject = mainObject.getJsonObject("info"); //$NON-NLS-1$

		if (!infoObject.containsKey("name") || !infoObject.containsKey("title")) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new PluginException("El plugin no define la informacion basica necesaria ('name' y 'title')"); //$NON-NLS-1$
		}

		final PluginInfo info  = new PluginInfo(infoObject.getString("name"), infoObject.getString("title")); //$NON-NLS-1$ //$NON-NLS-2$
		info.setVersionCode(infoObject.getInt("version_code", 1)); //$NON-NLS-1$
		info.setVersion(infoObject.getString("version", "1.0")); //$NON-NLS-1$ //$NON-NLS-2$
		info.setDescription(infoObject.getString("description")); //$NON-NLS-1$

		if (infoObject.containsKey("authors")) { //$NON-NLS-1$
			final JsonArray authorsArray = infoObject.getJsonArray("authors"); //$NON-NLS-1$
			final String[] authors = new String[authorsArray.size()];
			for (int i = 0; i < authorsArray.size(); i++) {
				authors[i] = authorsArray.getString(i);
			}
			info.setAuthors(authors);
		}
		if (infoObject.containsKey("contacts")) { //$NON-NLS-1$
			final JsonArray contactsArray = infoObject.getJsonArray("contacts"); //$NON-NLS-1$
			final String[] contacts = new String[contactsArray.size()];
			for (int i = 0; i < contactsArray.size(); i++) {
				contacts[i] = contactsArray.getString(i);
			}
			info.setContacts(contacts);
		}

		if (infoObject.containsKey("configuration_panel")) { //$NON-NLS-1$
			info.setConfigPanel(infoObject.getString("configuration_panel")); //$NON-NLS-1$
		}

		return info;
	}

	/**
	 * Carga y ordena los permisos de un plugin.
	 * @param mainObject JSON con la informaci&oacute;n del plugin.
	 * @return Listado de permisos.
	 */
	private static Permission[] parsePermissionsObject(final JsonObject mainObject) {

		final JsonArray permissionsArray = mainObject.getJsonArray("permissions"); //$NON-NLS-1$
		if (permissionsArray == null) {
			return new Permission[0];
		}

		final Set<Permission> permissionsSet = new HashSet<>();

		for (int i = 0; i < permissionsArray.size(); i++) {
			final JsonString permissionString = (JsonString) permissionsArray.get(i);
			Permission permission;
			try {
				permission = Permission.forName(permissionString.getString());
			}
			catch (final Exception e) {
				LOGGER.warning("Permiso no reconocido: " + permissionString.getString()); //$NON-NLS-1$
				continue;
			}
			permissionsSet.add(permission);
		}

		final Permission[] permissions = permissionsSet.toArray(new Permission[0]);
		Arrays.sort(permissions,
				(o1, o2) -> o1.getOrder() < o2.getOrder()
							? -1
							: o1.getOrder() == o2.getOrder() ? 0 : 1);

		return permissions;
	}


	private static String parseInlineProcessor(final JsonObject mainObject) {

		final JsonString inlineProcessorString = mainObject.getJsonString("inline_processor"); //$NON-NLS-1$

		String inlineProcessor = null;
		if (inlineProcessorString != null && inlineProcessorString.getString() != null) {
			inlineProcessor = inlineProcessorString.getString();
		}
		return inlineProcessor;
	}

	private static GenericMenuOption parseMenuObject(final JsonObject mainObject) throws PluginException {
		final JsonObject menuObject = mainObject.getJsonObject("menu"); //$NON-NLS-1$
		if (menuObject == null) {
			return null;
		}

		if (!menuObject.containsKey("title") || //$NON-NLS-1$
				!menuObject.containsKey("items") && !menuObject.containsKey("action")) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new PluginException("Se han encontrado menus del plugin mal definidos"); //$NON-NLS-1$
		}

		final GenericMenuOption menu = new GenericMenuOption(menuObject.getString("title")); //$NON-NLS-1$
		if (menuObject.containsKey("action")) { //$NON-NLS-1$
			menu.setDialogClass(menuObject.getString("action")); //$NON-NLS-1$
		}
		else {
			final JsonArray items = menuObject.getJsonArray("items"); //$NON-NLS-1$
			parseItemsObject(menu, items);
		}

		return menu;
	}

	private static void parseItemsObject(final GenericMenuOption menu, final JsonArray mainObject) throws PluginException {
		if(mainObject != null && mainObject.size() > 0) {
			for(int i = 0; i < mainObject.size(); i++) {
				final JsonObject subObject = mainObject.getJsonObject(i);
				if (!subObject.containsKey("title") || //$NON-NLS-1$
						!subObject.containsKey("items") && !subObject.containsKey("action")) { //$NON-NLS-1$ //$NON-NLS-2$
					throw new PluginException("Se han encontrado sub-menus del plugin mal definidos"); //$NON-NLS-1$
				}
				final GenericMenuOption subMenu = new GenericMenuOption(subObject.getString("title")); //$NON-NLS-1$
				if(subObject.containsKey("items")) { //$NON-NLS-1$
					final JsonArray subItems = subObject.getJsonArray("items"); //$NON-NLS-1$
					if(subItems != null && subItems.size() > 0) {
						parseItemsObject(subMenu, subItems);
					}
				} else if(subObject.containsKey("action")) { //$NON-NLS-1$
					final JsonString action = subObject.getJsonString("action"); //$NON-NLS-1$
					subMenu.setActionClassName(action.getString());
				}
				menu.addSubMenu(subMenu);
			}
		}
	}

	private static PluginButton[] parseButtonsObject(final JsonObject mainObject) throws PluginException {
		final JsonArray buttonsArray = mainObject.getJsonArray("buttons"); //$NON-NLS-1$
		if (buttonsArray == null || buttonsArray.size() == 0) {
			return null;
		}

		final List<PluginButton> buttons = new ArrayList<>();
		for (int i = 0; i < buttonsArray.size(); i++) {
			final JsonObject buttonObject = (JsonObject) buttonsArray.get(i);
			if (!buttonObject.containsKey("title") && !buttonObject.containsKey("icon")) { //$NON-NLS-1$ //$NON-NLS-2$
				throw new PluginException("No se ha indicado el titulo ni el icono de un boton del plugin"); //$NON-NLS-1$
			}
			if (!buttonObject.containsKey("action")) { //$NON-NLS-1$
				throw new PluginException("No se ha definido una accion para un boton"); //$NON-NLS-1$
			}
			if (!buttonObject.containsKey("window")) { //$NON-NLS-1$
				throw new PluginException("No se ha definido la ventana para un boton"); //$NON-NLS-1$
			}

			final PluginButton button = new PluginButton();
			button.setTitle(buttonObject.getString("title", null)); //$NON-NLS-1$
			button.setIcon(buttonObject.getString("icon", null)); //$NON-NLS-1$
			button.setToolTip(buttonObject.getString("tooltip", null)); //$NON-NLS-1$
			button.setAccesibleDescription(buttonObject.getString("accesible_description", null)); //$NON-NLS-1$
			button.setAfirmaWindow(buttonObject.getString("window")); //$NON-NLS-1$
			button.setActionClassName(buttonObject.getString("action")); //$NON-NLS-1$

			buttons.add(button);
		}

		return buttons.toArray(new PluginButton[buttons.size()]);
	}


	private static PluginCommand[] parseCommandsObject(final JsonObject mainObject) throws PluginException {
		final JsonArray commandsArray = mainObject.getJsonArray("commands"); //$NON-NLS-1$
		if (commandsArray == null || commandsArray.size() == 0) {
			return null;
		}

		final List<PluginCommand> commands = new ArrayList<>();
		for (int i = 0; i < commandsArray.size(); i++) {
			final JsonObject commandObject = (JsonObject) commandsArray.get(i);
			if (!commandObject.containsKey("name")) { //$NON-NLS-1$
				throw new PluginException("No se ha indicado el nombre de un comando del plugin"); //$NON-NLS-1$
			}
			if (!commandObject.containsKey("action")) { //$NON-NLS-1$
				throw new PluginException("No se ha definido una clase de accion para un comando"); //$NON-NLS-1$
			}

			final PluginCommand command = new PluginCommand(commandObject.getString("name")); //$NON-NLS-1$
			command.setCommandActionClass(commandObject.getString("action")); //$NON-NLS-1$
			command.setDescription(commandObject.getString("description")); //$NON-NLS-1$

			commands.add(command);
		}

		return commands.toArray(new PluginCommand[commands.size()]);
	}
}
