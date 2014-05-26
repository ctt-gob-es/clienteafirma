package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Font;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.metal.MetalTheme;

/** Clase utilizada para mostrar un tema en Alto Contraste en la interfaz de la aplicaci&oacute;n.
 * @author INTECO */
public class HighContrastTheme extends MetalTheme {

    /** Fuente para el control. En este caso indica la fuente para los Warnings
     * y Ventanas de Cargar/guardar */
    private final FontUIResource controlFont = new FontUIResource("SansSerif", Font.PLAIN, 12);
    /** Color primario 1 */
    private final ColorUIResource primary1 = new ColorUIResource(Color.WHITE);
    /** Color primario 2 */
    private final ColorUIResource primary2 = new ColorUIResource(Color.WHITE);

    /** Color primario 3 */
    private final ColorUIResource primary3 = new ColorUIResource(Color.LIGHT_GRAY);
    /** Color secundario 1 */
    private final ColorUIResource secondary1 = new ColorUIResource(Color.WHITE);
    /** Color secundario 2 */
    private final ColorUIResource secondary2 = new ColorUIResource(Color.LIGHT_GRAY);

    /** Color secundario 3 */
    private final ColorUIResource secondary3 = new ColorUIResource(Color.BLACK);
    /** Fuente para el tama&ntilde;o peque&ntilde;o */
    private final FontUIResource smallFont = new FontUIResource("Dialog", Font.PLAIN, 9);
    /** Fuente para el sistema. En este caso indica la fuente para los ToolTip */
    private final FontUIResource systemFont = new FontUIResource("Dialog", Font.PLAIN, 12);
    /** Fuente para el usuario */
    private final FontUIResource userFont = new FontUIResource("SansSerif", Font.PLAIN, 10);
    /** Fuente para la ventana */
    private final FontUIResource windowTitleFont = new FontUIResource("SansSerif", Font.BOLD, 10);

    /** @see javax.swing.plaf.metal.MetalTheme#getControlTextFont() */
    @Override
    public final FontUIResource getControlTextFont() {
        return this.controlFont;
    }

    /** TODO: Should this be getTextHighlightColor?
     * @return */
    final FontUIResource getEmphasisTextFont() {
        return this.windowTitleFont;
    }

    /** Devuelve el color a mostrar cuando un elemento tiene el foco */
    @Override
    public final ColorUIResource getFocusColor() {
        return new ColorUIResource(Color.ORANGE);
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getMenuTextFont() */
    @Override
    public final FontUIResource getMenuTextFont() {
        return this.controlFont;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getName() */
    @Override
    public final String getName() {
        return "Default";
    }

    // these are blue in Metal Default Theme
    /** @see javax.swing.plaf.metal.MetalTheme#getPrimary1() */
    @Override
    protected final ColorUIResource getPrimary1() {
        return this.primary1;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getPrimary2() */
    @Override
    protected final ColorUIResource getPrimary2() {
        return this.primary2;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getPrimary3() */
    @Override
    protected final ColorUIResource getPrimary3() {
        return this.primary3;
    }

    // these are gray in Metal Default Theme
    /** @see javax.swing.plaf.metal.MetalTheme#getSecondary1() */
    @Override
    protected final ColorUIResource getSecondary1() {
        return this.secondary1;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getSecondary2() */
    @Override
    protected final ColorUIResource getSecondary2() {
        return this.secondary2;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getSecondary3() */
    @Override
    protected final ColorUIResource getSecondary3() {
        return this.secondary3;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getSubTextFont() */
    @Override
    public final FontUIResource getSubTextFont() {
        return this.smallFont;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getSystemTextFont() */
    @Override
    public final FontUIResource getSystemTextFont() {
        return this.systemFont;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getUserTextFont() */
    @Override
    public final FontUIResource getUserTextFont() {
        return this.userFont;
    }

    /** @see javax.swing.plaf.metal.MetalTheme#getWindowTitleFont() */
    @Override
    public final FontUIResource getWindowTitleFont() {
        return this.windowTitleFont;
    }

}
