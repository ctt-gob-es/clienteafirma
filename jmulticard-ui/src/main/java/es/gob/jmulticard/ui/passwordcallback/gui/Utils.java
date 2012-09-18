/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros 
 * para la realizacion de procesos de autenticacion, firma electronica y validacion 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion 
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha 
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos 
 * e Impulso de la Administracion Electronica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * European Union Public License publicada por la Comision Europea, 
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 * 
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 * 
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import java.awt.Color;
import java.awt.Font;
import java.awt.IllegalComponentStateException;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.Locale;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.JWindow;
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;

/** Clase con utilidades varias */
final class Utils {

    /** Indica si el sistema operativo tiene activada una combinaci&oacute;n de colores de alto contraste. */
    private static final boolean HIGH_CONTRAST;

    static boolean isHighContrast() {
        return HIGH_CONTRAST;
    }

    static {
        final Object highContrast = Toolkit.getDefaultToolkit().getDesktopProperty("win.highContrast.on"); //$NON-NLS-1$
        if (highContrast instanceof Boolean) {
            HIGH_CONTRAST = ((Boolean) highContrast).booleanValue();
        }
        // En Linux usamos siempre una configuracion como si se usase combinacion de colores
        // de alto contraste
        else if ((System.getProperty("os.name") != null) && (System.getProperty("os.name").contains("inux"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            HIGH_CONTRAST = true;
        }
        else {
            HIGH_CONTRAST = false;
        }
    }

    private Utils() {
        // No permitimos la instanciacion
    }

    /** Configura el formato del remarcado del componente al ser seleccionado.
     * @param component El componente seleccionado. */
    static void remarcar(final JComponent component) {

        if (GeneralConfig.isRemarked()) {
            if (component instanceof JButton) {
                final JButton button = (JButton) component;
                button.addFocusListener(new FocusListener() {
                    @Override
                    public void focusLost(final FocusEvent e) {
                        if (button.getParent() instanceof JPanel) {
                            ((JPanel) button.getParent()).setBorder(BorderFactory.createEmptyBorder());
                        }
                    }

                    @Override
                    public void focusGained(final FocusEvent e) {
                        if (GeneralConfig.isHighContrast() || isHighContrast()) {
                            if (button.getParent() instanceof JPanel) {
                                ((JPanel) button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
                            }
                        }
                        else {
                            if (button.getParent() instanceof JPanel) {
                                ((JPanel) button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                            }
                        }
                    }
                });
            }
            if (component instanceof JTextField) {
                final JTextField textField = (JTextField) component;
                textField.addFocusListener(new FocusListener() {
                    @Override
                    public void focusLost(final FocusEvent e) {
                        textField.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
                    }

                    @Override
                    public void focusGained(final FocusEvent e) {
                        if (GeneralConfig.isHighContrast() || isHighContrast()) {
                            textField.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
                        }
                        else {
                            textField.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                        }
                    }
                });
            }
            if (component instanceof JComboBox) {
                final JComboBox comboBox = (JComboBox) component;
                comboBox.addFocusListener(new FocusListener() {
                    @Override
                    public void focusLost(final FocusEvent e) {
                        comboBox.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
                    }

                    @Override
                    public void focusGained(final FocusEvent e) {
                        if (GeneralConfig.isHighContrast() || isHighContrast()) {
                            comboBox.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
                        }
                        else {
                            comboBox.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                        }
                    }
                });
            }
            if (component instanceof JLabel) {
                final JLabel label = (JLabel) component;
                label.addFocusListener(new FocusListener() {
                    @Override
                    public void focusLost(final FocusEvent e) {
                        label.setBorder(BorderFactory.createEmptyBorder());
                    }

                    @Override
                    public void focusGained(final FocusEvent e) {
                        if (GeneralConfig.isHighContrast() || isHighContrast()) {
                            label.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
                        }
                        else {
                            label.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                        }
                    }
                });
            }
            if (component instanceof JCheckBox) {
                final JCheckBox checkBox = (JCheckBox) component;
                checkBox.addFocusListener(new FocusListener() {
                    @Override
                    public void focusLost(final FocusEvent e) {
                        ((JPanel) checkBox.getParent()).setBorder(BorderFactory.createEmptyBorder());
                    }

                    @Override
                    public void focusGained(final FocusEvent e) {
                        if (GeneralConfig.isHighContrast() || isHighContrast()) {
                            ((JPanel) checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
                        }
                        else {
                            ((JPanel) checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
                        }
                    }
                });
            }
        }
    }

    /** Configura el comportamiento de ciertos componentes en Alto Contraste
     * @param component Componente al que aplicar el alto contraste */
    static void setContrastColor(final JComponent component) {
        if (GeneralConfig.isHighContrast()) {
            if (component instanceof JComboBox || component instanceof JPasswordField || component instanceof JTextField) {
                component.setBackground(Color.WHITE);
            }
            else if (component instanceof JCheckBox) {
                component.setForeground(Color.WHITE);
            }
            else if (component instanceof JPanel) {
                if (component.getBorder() instanceof TitledBorder) {
                    ((TitledBorder) component.getBorder()).setTitleColor(Color.WHITE);
                }
                component.setForeground(Color.WHITE);
                component.setBackground(Color.BLACK);
            }
            else {
                component.setForeground(Color.WHITE);
                component.setBackground(Color.BLACK);
            }
        }
    }

    /** Aplica el estilo de fuente negrita
     * @param component Componente al que aplicar el estilo de fuente negrita. */
    static void setFontBold(final JComponent component) {
        // Se comprueba si el componente es de tipo panel con borde
        if (component instanceof JPanel) {
            if (component.getBorder() instanceof TitledBorder) {
                final TitledBorder titledBorder = (TitledBorder) component.getBorder(); // Se obtiene el borde
                // Se comprueba que no sea nulo
                if (titledBorder != null) {
                    // Se comprueba si la configuracion pide que la fuente este en negrita
                    if (GeneralConfig.isFontBold()) {
                        // Se indica que la fuente es negrita
                        titledBorder.setTitleFont(new Font(component.getFont().getName(), Font.BOLD, component.getFont().getSize()));
                    }
                    else {
                        // Se indica que la fuente es texto plano
                        titledBorder.setTitleFont(new Font(component.getFont().getName(), Font.PLAIN, component.getFont().getSize()));
                    }
                }
            } // Comprobacion del tipo de borde
        }
        else {
            // Se comprueba si la configuracion pide que la fuente este en negrita
            if (GeneralConfig.isFontBold()) {
                    // Se indica que la fuente es negrita
                    component.setFont(new Font(component.getFont().getName(), Font.BOLD, component.getFont().getSize()));
            }
            else {
                    // Se indica que la fuente es texto plano
                    component.setFont(new Font(component.getFont().getName(), Font.PLAIN, component.getFont().getSize()));
            }
        }
    }

    /** Metodo que sumbraya el mnemonico correspondiente para texto HTML.
     * @param text Texto en el que hay que subrayar el caracter.
     * @param key Caracter a subrayar.
     * @return Cadena con el texto subrayado. */
    static String remarkMnemonic(final String text, final int key) {
        String newText = text;
        int pos = text.indexOf(key); // Se obtiene el indice del caracter
        if (pos == -1) {// Se busca en minuscula
            final char keyChar = (char) key;
            pos = text.indexOf(String.valueOf(keyChar).toLowerCase(Locale.getDefault()));
        }
        if (pos != -1) {
            // Se subraya
            newText = text.substring(0, pos) + "<u>" + text.charAt(pos) + "</u>" + text.substring(pos + 1); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return newText;
    }

    /** Muestra u oculta un tooltip relacionado con un bot&oacute;n.
     * @param show Boolean que indica si se muestra el tooltip
     * @param tip JWindow que muestra el contenido del tooltip
     * @param boton JButton al que se relaciona el tooltip
     * @param tipText JLabel que muestra el contenido del tooltip */
    static void showToolTip(final boolean show, final JWindow tip, final JButton boton, final JLabel tipText) {
        tipText.setText(boton.getToolTipText());
        tip.setBackground((Color) UIManager.get("ToolTip.background")); //$NON-NLS-1$
        tipText.setBackground((Color) UIManager.get("ToolTip.background")); //$NON-NLS-1$
        tipText.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.BLACK), BorderFactory.createEmptyBorder(0, 3, 0, 3)));
        tipText.setFont((Font) UIManager.get("ToolTip.font")); //$NON-NLS-1$
        tipText.setOpaque(true);
        tip.add(tipText);
        Point p = new Point();
        try {
            p = boton.getLocationOnScreen();
        }
        catch (final IllegalComponentStateException e) {
            Logger.getLogger("es.gob.jmulticard").warning("Error mostrando el tooltip: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        int factor = 0;
        if (boton.getSize().getHeight() > 34) {
            factor = (int) (boton.getSize().getHeight() * 0.5);
        }
        tip.setLocation((int) p.getX(), (int) p.getY() + 30 + factor);
        tip.pack();
        tip.setVisible(show);
    }
}
