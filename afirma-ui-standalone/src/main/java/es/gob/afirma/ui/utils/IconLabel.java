package es.gob.afirma.ui.utils;

import javax.swing.Icon;
import javax.swing.JLabel;

/**
 * Componente etiqueta que contiene un icono.
 * @author inteco
 *
 */
public class IconLabel extends JLabel{

	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Icono.
	 */
	private Icon icon = null;
	
	/**
	 * Icono original.
	 */
	private Icon originalIcon = null;
	
	

	/**
	 * Constructor.
	 */
	public IconLabel(){
		
	}

	/**
	 * Devuelve el icono asociado.
	 * @return
	 */
	public Icon getIcon() {
		return icon;
	}

	/**
	 * Asigna el icono.
	 * @param icon
	 */
	public void setIcon(Icon icon) {
		this.icon = icon;
	}

	/**
	 * Obtener el icono original.
	 * @return icono original.
	 */
	public Icon getOriginalIcon() {
		return originalIcon;
	}

	/**
	 * Asignar el icono original.
	 * @param originalIcon
	 */
	public void setOriginalIcon(Icon originalIcon) {
		this.originalIcon = originalIcon;
	}
	
}
