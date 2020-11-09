package es.gob.afirma.standalone.plugins;

import java.util.ArrayList;
import java.util.List;

public class GenericMenuOption {

	private final String title;
	private String action;
	private String dialogClass = null;
	private List<GenericMenuOption> menus = null;

	GenericMenuOption(String title) {
		this(title, null);
	}
	
	GenericMenuOption(String title, String action) {
		this.title = title;
		this.action = action;
	}

	public String getTitle() {
		return this.title;
	}
	
	public String getAction() {
		return action;
	}
	
	public void setAction(String action) {
		this.action = action;
	}

	void addSubMenu(GenericMenuOption subMenu) {
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

	void setDialogClass(String dialogClass) {
		this.dialogClass = dialogClass;
	}
}
