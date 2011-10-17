package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Font;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.MetalTheme;

/**
 * Clase utilizada para mostrar un tema en Alto Contraste en la interfaz de la aplicaci&oacute;n.
 * 
 * @author INTECO
 */
public class HighContrastTheme extends MetalTheme {

	/**
	 * Color primario 1
	 */
	private final ColorUIResource primary1 = new ColorUIResource(Color.WHITE);
	/**
	 * Color primario 2
	 */
	private final ColorUIResource primary2 = new ColorUIResource(Color.WHITE);
	/**
	 * Color primario 3
	 */
	private final ColorUIResource primary3 = new ColorUIResource(Color.DARK_GRAY);

	/**
	 * Color secundario 1
	 */
	private final ColorUIResource secondary1 = new ColorUIResource(Color.WHITE);
	/**
	 * Color secundario 2
	 */
	private final ColorUIResource secondary2 = new ColorUIResource(Color.DARK_GRAY);
	/**
	 * Color secundario 3
	 */
	private final ColorUIResource secondary3 = new ColorUIResource(Color.BLACK);

	/**
	 * Fuente para el control. En este caso indica la fuente para los Warnings
	 *  y Ventanas de Cargar/guardar
	 */
	private final FontUIResource controlFont = new FontUIResource("SansSerif", Font.PLAIN, 12);
	/**
	 * Fuente para el sistema. En este caso indica la fuente para los ToolTip
	 */
	private final FontUIResource systemFont = new FontUIResource("Dialog", Font.PLAIN, 12);
	/**
	 * Fuente para la ventana
	 */
	private final FontUIResource windowTitleFont = new FontUIResource("SansSerif", Font.BOLD, 10);
	/**
	 * Fuente para el usuario
	 */
	private final FontUIResource userFont = new FontUIResource("SansSerif", Font.PLAIN, 10);
	/**
	 * Fuente para el tama&ntilde;o peque&ntilde;o
	 */
	private final FontUIResource smallFont = new FontUIResource("Dialog", Font.PLAIN, 9);

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getName()
	 */
	public final String getName() {
		return "Default";
	}

	// these are blue in Metal Default Theme
	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getPrimary1()
	 */
	protected final ColorUIResource getPrimary1() {
		return primary1;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getPrimary2()
	 */
	protected final ColorUIResource getPrimary2() {
		return primary2;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getPrimary3()
	 */
	protected final ColorUIResource getPrimary3() {
		return primary3;
	}

	// these are gray in Metal Default Theme
	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getSecondary1()
	 */
	protected final ColorUIResource getSecondary1() {
		return secondary1;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getSecondary2()
	 */
	protected final ColorUIResource getSecondary2() {
		return secondary2;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getSecondary3()
	 */
	protected final ColorUIResource getSecondary3() {
		return secondary3;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getControlTextFont()
	 */
	public final FontUIResource getControlTextFont() {
		return controlFont;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getSystemTextFont()
	 */
	public final FontUIResource getSystemTextFont() {
		return systemFont;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getUserTextFont()
	 */
	public final FontUIResource getUserTextFont() {
		return userFont;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getMenuTextFont()
	 */
	public final FontUIResource getMenuTextFont() {
		return controlFont;
	}

	/**
	 * TODO: Should this be getTextHighlightColor?
	 * 
	 * @return
	 */
	public final FontUIResource getEmphasisTextFont() {
		return windowTitleFont;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getSubTextFont()
	 */
	public final FontUIResource getSubTextFont() {
		return smallFont;
	}

	/**
	 * @see javax.swing.plaf.metal.MetalTheme#getWindowTitleFont()
	 */
	public final FontUIResource getWindowTitleFont() {
		return windowTitleFont;
	}

	/**
	 * Devuelve el color a mostrar cuando un elemento tiene el foco
	 */
	@Override
	public final ColorUIResource getFocusColor() {
		return new ColorUIResource(Color.ORANGE);
	}

}