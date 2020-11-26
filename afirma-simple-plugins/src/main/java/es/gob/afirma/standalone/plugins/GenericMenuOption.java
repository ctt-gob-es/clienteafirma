package es.gob.afirma.standalone.plugins;

import java.util.ArrayList;
import java.util.List;

public class GenericMenuOption {

	private final String title;
	private String actionClassName;
	private PluginAction action;
	private String dialogClass = null;
	private List<GenericMenuOption> menus = null;

	GenericMenuOption(final String title) {
		this(title, null);
	}

	GenericMenuOption(final String title, final String actionClassName) {
		this.title = title;
		this.actionClassName = actionClassName;
	}

	public String getTitle() {
		return this.title;
	}

	public String getActionClassName() {
		return this.actionClassName;
	}

	public void setActionClassName(final String actionClassName) {
		this.actionClassName = actionClassName;
	}

	public PluginAction getActionAction() {
		return this.action;
	}

	public void setAction(final PluginAction action) {
		this.action = action;
	}

	void addSubMenu(final GenericMenuOption subMenu) {
		if (this.menus == null) {
			this.menus = new ArrayList<>();
		}
		this.menus.add(subMenu);
	}

	public List<GenericMenuOption> getMenus() {
		return this.menus;
	}

	public String getDialogClass() {
		return this.dialogClass;
	}

	void setDialogClass(final String dialogClass) {
		this.dialogClass = dialogClass;
	}
}
