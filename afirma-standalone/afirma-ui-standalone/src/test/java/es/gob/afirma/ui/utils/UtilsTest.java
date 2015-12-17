package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.JWindow;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.ui.principal.AccessibilityOptionsPane;

/**
 * Testeo de funcionalidades relativas a la accesibilidad de la clase Utils
 *
 * @author inteco
 *
 */
public class UtilsTest {

    /**
     * Log.
     */
    private static Logger LOGGER = Logger.getLogger(UtilsTest.class.getName());

    /**
     * Comprueba si el componente aplica el estilo de fuente negrita
     * correctamente
     *
     * @param jComboBox
     *            Componente del tipo JComboBox para el que se comprueba el
     *            cambio de estilo de fuente
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean fontBoldJComboBox(final JComboBox jComboBox) {
	Utils.setFontBold(jComboBox);
	if (GeneralConfig.isFontBold()) {
	    if (jComboBox.getFont().getStyle() == Font.BOLD) {
		return true;
	    }
	} else {
	    if (jComboBox.getFont().getStyle() == Font.PLAIN) {
		return true;
	    }
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el estilo de fuente negrita
     * correctamente
     *
     * @param jPanel
     *            Componente del tipo JPanel para el que se comprueba el cambio
     *            de estilo de fuente
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean fontBoldJPanel(final JPanel jPanel) {
	Utils.setFontBold(jPanel);
	if (GeneralConfig.isFontBold()) {
	    if (jPanel.getBorder() != null) {
		if (jPanel.getBorder().getClass().getName()
			.equals("javax.swing.border.TitledBorder")) { //$NON-NLS-1$
		    if (((TitledBorder) jPanel.getBorder()) != null) {
			if (((TitledBorder) jPanel.getBorder()).getTitleFont()
				.getStyle() == Font.BOLD) {
			    return true;
			}
		    }
		}
	    }
	} else {
	    if (jPanel.getBorder() != null) {
		if (jPanel.getBorder().getClass().getName()
			.equals("javax.swing.border.TitledBorder")) { //$NON-NLS-1$
		    if (((TitledBorder) jPanel.getBorder()) != null) {
			if (((TitledBorder) jPanel.getBorder()).getTitleFont()
				.getStyle() == Font.PLAIN) {
			    return true;
			}
		    }
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el estilo de fuente negrita
     * correctamente
     *
     * @param jComboBox
     *            Componente del tipo JComboBox para el que se comprueba el
     *            cambio de estilo de fuente
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean fontBoldJToolBar(final JToolBar jToolBar) {
	Utils.setFontBold(jToolBar);
	if (GeneralConfig.isFontBold()) {
	    for (int i = 0; i < jToolBar.getComponentCount(); i++) {
		if (jToolBar.getComponent(i).getFont().getStyle() == Font.BOLD) {
		    return true;
		}
	    }
	} else {
	    for (int i = 0; i < jToolBar.getComponentCount(); i++) {
		if (jToolBar.getComponent(i).getFont().getStyle() == Font.PLAIN) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jComboBox
     *            Componente del tipo JComboBox para el que se comprueba el
     *            cambio de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJComboBox(final JComboBox jComboBox) {
	Utils.setContrastColor(jComboBox);
	if (GeneralConfig.isHighContrast()) {
	    if (jComboBox.getBackground() == Color.WHITE) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jEditorPane
     *            Componente del tipo JEditorPane para el que se comprueba el
     *            cambio de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJEditorPane(final JEditorPane jEditorPane) {
	Utils.setContrastColor(jEditorPane);
	if (GeneralConfig.isHighContrast()) {
	    if (jEditorPane.getBackground() == Color.BLACK) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jList
     *            Componente del tipo JList para el que se comprueba el cambio
     *            de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJList(final JList jList) {
	Utils.setContrastColor(jList);
	if (GeneralConfig.isHighContrast()) {
	    if (jList.getForeground() == Color.BLACK) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jPanel
     *            Componente del tipo JPanel para el que se comprueba el cambio
     *            de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJPanel(final JPanel jPanel) {
	Utils.setContrastColor(jPanel);
	if (GeneralConfig.isHighContrast()) {
	    if (jPanel.getBorder() != null) {
		if (jPanel.getBorder().getClass().getName()
			.equals("javax.swing.border.TitledBorder")) { //$NON-NLS-1$
		    if (((TitledBorder) jPanel.getBorder()) != null) {
			if (((TitledBorder) jPanel.getBorder()).getTitleColor() != Color.WHITE) {
			    return false;
			}
		    }
		}
	    }
	    if (jPanel.getForeground() == Color.WHITE
		    && jPanel.getBackground() == Color.BLACK) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jPasswordField
     *            Componente del tipo JPasswordField para el que se comprueba el
     *            cambio de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJPasswordField(
	    final JPasswordField jPasswordField) {
	Utils.setContrastColor(jPasswordField);
	if (GeneralConfig.isHighContrast()) {
	    if (jPasswordField.getBackground() == Color.WHITE) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jStatusBar
     *            Componente del tipo JStatusBar para el que se comprueba el
     *            cambio de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJStatusBar(final JStatusBar jStatusBar) {
	Utils.setContrastColor(jStatusBar);
	if (GeneralConfig.isHighContrast()) {
	    if (((JLabel) jStatusBar.getComponent(0)).getForeground() == Color.WHITE) {
		return true;
	    }
	} else {
	    if (((JLabel) jStatusBar.getComponent(0)).getForeground() == Color.BLACK) {
		return true;
	    }
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jTextField
     *            Componente del tipo JTextField para el que se comprueba el
     *            cambio de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJTextField(final JTextField jTextField) {
	Utils.setContrastColor(jTextField);
	if (GeneralConfig.isHighContrast()) {
	    if (jTextField.getBackground() == Color.WHITE) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el componente aplica el alto contraste correctamente
     *
     * @param jTree
     *            Componente del tipo JTree para el que se comprueba el cambio
     *            de color
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean highContrastJTree(final JTree jTree) {
	Utils.setContrastColor(jTree);
	if (GeneralConfig.isHighContrast()) {
	    if (jTree.getForeground() == Color.WHITE) {
		return true;
	    }
	} else {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el gestor de mnem&oacute;nicos libres indica que no hay un
     * mnem&oacute;nico disponible
     *
     * @param mnemonicList
     *            Lista con los mnem&oacute;nicos empleados
     * @param nombreConLetraLibre
     *            Nombre que no tiene letra disponible a la que asignar
     *            mnem&oacute;nicos
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean languageMnemonicFail(final List<Character> mnemonicList,
	    final String nombreConLetraLibre) {
	if (Utils.getLanguageMnemonic(mnemonicList, nombreConLetraLibre) == 0) {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si el gestor de mnem&oacute;nicos libres asigna correctamente
     * un mnem&oacute;nico
     *
     * @param mnemonicList
     *            Lista con los mnem&oacute;nicos empleados
     * @param nombreConLetraLibre
     *            Nombre con una letra disponible a la que asignar
     *            mnem&oacute;nicos
     * @return Boolean indicando si pasa la validaci&oacute;n
     */
    @SuppressWarnings("static-method")
    private boolean languageMnemonicSucces(final List<Character> mnemonicList,
	    final String nombreConLetraLibre) {
	if (Utils.getLanguageMnemonic(mnemonicList, nombreConLetraLibre) == 'd') {
	    return true;
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JButton ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JCheckBox ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJButton(final JButton component,
	    final JCheckBox componentOld) {
	final LineBorder border = (LineBorder) ((JPanel) (component.getParent()))
		.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JCheckBox ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JLabel ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJCheckBox(final JCheckBox component,
	    final JLabel componentOld) {
	final LineBorder border = (LineBorder) ((JPanel) (component.getParent()))
		.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JComboBox ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JTextField ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJComboBox(final JComboBox component,
	    final JTextField componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	final LineBorder borderOld = (LineBorder) componentOld.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (borderOld.getLineColor() == Color.GRAY) {
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (borderOld.getLineColor() == Color.GRAY) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JEditorPane ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JTextPane ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJEditorPane(final JEditorPane component,
	    final JTextPane componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JLabel ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JComboBox ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJLabel(final JLabel component,
	    final JComboBox componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	final LineBorder borderOld = (LineBorder) componentOld.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (borderOld.getLineColor() == Color.GRAY) {
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (borderOld.getLineColor() == Color.GRAY) {
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JList ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JTree ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJList(final JList component,
	    final JTree componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JMenu ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JList ha comprobar si se ha quitado el borde
     * @param fontSize
     *            Integer con el tama&ntilde;o de fuente de inicio del JMenu
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJMenu(final JMenu component,
	    final JList componentOld, final int fontSize) {

	if (component.getFont().getSize() == fontSize + 5) {
	    if (componentOld.getBorder().getClass().getName()
		    .equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		return true;
	    }
	}

	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JRadioButton ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JToggleButton ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJRadioButton(final JRadioButton component,
	    final JToggleButton componentOld) {
	final LineBorder border = (LineBorder) ((JPanel) (component.getParent()))
		.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JList ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JTree ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJSrollPane(final JScrollPane component,
	    final JList componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde del componente
     *
     * @param component
     *            JTextField ha comprobar
     * @return boolean indicando si el borde ha cambiado
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJTextField(final JTextField component) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		return true;
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		return true;
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JTextPane ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JRadioButton ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJTextPane(final JTextPane component,
	    final JRadioButton componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JToggleButton ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JButton ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJToggleButton(final JToggleButton component,
	    final JButton componentOld) {
	final LineBorder border = (LineBorder) ((JPanel) (component.getParent()))
		.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (((JPanel) (componentOld.getParent())).getBorder()
			.getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprueba si ha cambiado el borde de los componentes
     *
     * @param component
     *            JTree ha comprobar si se ha pintado el borde
     * @param componentOld
     *            JEditorPane ha comprobar si se ha quitado el borde
     * @return boolean indicando si los bordes han cambiado correctamente
     */
    @SuppressWarnings("static-method")
    private boolean remarcarJTree(final JTree component,
	    final JEditorPane componentOld) {
	final LineBorder border = (LineBorder) component.getBorder();
	if (GeneralConfig.isHighContrast()) {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.WHITE) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	} else {
	    if (border.getClass().getName()
		    .equals("javax.swing.border.LineBorder") && border.getLineColor() == Color.BLACK) { //$NON-NLS-1$
		// Se comprueba que se ha desactivado el remarcado en alto
		// contraste del elemento que ha perdido el foco
		if (componentOld.getBorder().getClass().getName()
			.equals("javax.swing.border.EmptyBorder")) { //$NON-NLS-1$
		    return true;
		}
	    }
	}
	return false;
    }

    /**
     * Comprobaci&oacute;n de la asignaci&oacute;n autom&aacute;tica de
     * mnem&oacute;nicos
     */
    @Test
    public void testGetLanguageMnemonic() {
	UtilsTest.LOGGER.info("testGetLanguageMnemonic"); //$NON-NLS-1$

	try {
	    // Lista de mnemonicos usados
	    final List<Character> mnemonicList = new ArrayList<>();
	    mnemonicList.add(new Character('a'));
	    mnemonicList.add(new Character('b'));
	    mnemonicList.add(new Character('c'));

	    // Nombre del texto al que asignar mnemonico
	    final String nombreConLetraLibre = new String("abcd"); //$NON-NLS-1$
	    final String nombreSinLetraLibre = new String("abc");		 //$NON-NLS-1$

	    Assert.assertTrue(this.languageMnemonicSucces(mnemonicList,
		    nombreConLetraLibre));
	    Assert.assertTrue(this.languageMnemonicFail(mnemonicList,
		    nombreSinLetraLibre));
	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}
    }

    /**
     * Comprobaci&oacute;n de que los componentes se remarcan al recibir el foco
     */
    @Test
    @Ignore
    public void testRemarcar() {
	UtilsTest.LOGGER.info("testRemarcar"); //$NON-NLS-1$

	try {
	    // Se obtiene la cofiguracion general
	    // Se anade el perfil por defecto
	    GeneralConfig.loadConfig(GeneralConfig.getConfig());
	    final Properties config = GeneralConfig.getConfig();

	    // Se activa la opcion de remarcar elementos con foco
	    config.setProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE,
		    "true"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);

	    final JFrame frame = new JFrame();
	    frame.setLayout(new GridBagLayout());
	    final GridBagConstraints c = new GridBagConstraints();
	    final JTextField jTF = new JTextField();
	    Utils.remarcar(jTF);
	    frame.add(jTF, c);
	    frame.setBounds(200, 200, 500, 500);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JComboBox jCB = new JComboBox();
	    Utils.remarcar(jCB);
	    frame.add(jCB, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JLabel jL = new JLabel();
	    Utils.remarcar(jL);
	    frame.add(jL, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JPanel panelJChB = new JPanel();
	    final JCheckBox jChB = new JCheckBox();
	    panelJChB.add(jChB);
	    Utils.remarcar(jChB);
	    frame.add(panelJChB, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JPanel panelJB = new JPanel();
	    final JButton jB = new JButton();
	    panelJB.add(jB);
	    Utils.remarcar(jB);
	    frame.add(panelJB, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JPanel panelJTB = new JPanel();
	    final JToggleButton jTB = new JToggleButton();
	    panelJTB.add(jTB);
	    Utils.remarcar(jTB);
	    frame.add(panelJTB, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JPanel panelJRB = new JPanel();
	    final JRadioButton jRB = new JRadioButton();
	    panelJRB.add(jRB);
	    Utils.remarcar(jRB);
	    frame.add(panelJRB, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JTextPane jTP = new JTextPane();
	    Utils.remarcar(jTP);
	    frame.add(jTP, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JEditorPane jEP = new JEditorPane();
	    Utils.remarcar(jEP);
	    frame.add(jEP, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JTree jT = new JTree();
	    Utils.remarcar(jT);
	    frame.add(jT, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JList jLi = new JList();
	    Utils.remarcar(jLi);
	    frame.add(jLi, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JScrollPane jSP = new JScrollPane();
	    Utils.remarcar(jSP);
	    frame.add(jSP, c);
	    frame.setVisible(true);

	    c.gridy = c.gridy + 1;
	    final JMenu jM = new JMenu();
	    Utils.remarcar(jM);
	    frame.add(jM, c);
	    frame.setVisible(true);
	    final int fontSize = jM.getFont().getSize();

	    // Se testea la correcta aplicacion del remarcado sin alto contraste
	    jTF.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTextField(jTF));
	    jCB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJComboBox(jCB, jTF));
	    jL.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJLabel(jL, jCB));
	    jChB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJCheckBox(jChB, jL));
	    jB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJButton(jB, jChB));
	    jTB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJToggleButton(jTB, jB));
	    jRB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJRadioButton(jRB, jTB));
	    jTP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTextPane(jTP, jRB));
	    jEP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJEditorPane(jEP, jTP));
	    jT.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTree(jT, jEP));
	    jLi.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJList(jLi, jT));
	    jSP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJSrollPane(jSP, jLi));
	    // Se activa la opcion de alto contraste
	    config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST,
		    "true"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);
	    // Se testea la correcta aplicacion del remarcado con alto contraste
	    jTF.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTextField(jTF));
	    jCB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJComboBox(jCB, jTF));
	    jL.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJLabel(jL, jCB));
	    jChB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJCheckBox(jChB, jL));
	    jB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJButton(jB, jChB));
	    jTB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJToggleButton(jTB, jB));
	    jRB.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJRadioButton(jRB, jTB));
	    jTP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTextPane(jTP, jRB));
	    jEP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJEditorPane(jEP, jTP));
	    jT.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJTree(jT, jEP));
	    jLi.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJList(jLi, jT));
	    jSP.requestFocusInWindow();
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJSrollPane(jSP, jLi));
	    // Se comprueba el menu
	    jM.setSelected(true);
	    try {
		Thread.sleep(500);
	    } catch (final InterruptedException e) {
		e.printStackTrace();
	    }
	    Assert.assertTrue(this.remarcarJMenu(jM, jLi, fontSize));
	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}
    }

    /**
     * Comprobaci&oacute;n del correcto subrayado de los mnem&oacute;nicos para
     * texto HTML
     */
    @SuppressWarnings("static-method")
	@Test
    public void testRemarkMnemonic() {
	UtilsTest.LOGGER.info("testRemarkMnemonic"); //$NON-NLS-1$

	try {
	    Assert.assertTrue(Utils
		    .remarkMnemonic("Test", 'T').equals("<u>T</u>est")); //$NON-NLS-1$ //$NON-NLS-2$
	    Assert.assertTrue(Utils.remarkMnemonic("Test", 'a').equals("Test")); //$NON-NLS-1$ //$NON-NLS-2$
	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}
    }

    /**
     * Comprobaci&oacute;n de que los componentes cambian al modo en alto
     * contraste correctamente
     */
    @Test
    public void testSetContrastColor() {
	UtilsTest.LOGGER.info("testSetContrastColor"); //$NON-NLS-1$

	try {
	    // Se obtiene la cofiguracion general
	    GeneralConfig.loadConfig(GeneralConfig.getConfig());
	    final Properties config = GeneralConfig.getConfig();
	    // Se crean los componentes para testear
	    final JComboBox jComboBox = new JComboBox();
	    final JPasswordField jPasswordField = new JPasswordField();
	    final JTextField jTextField = new JTextField();
	    final JTree jTree = new JTree();
	    final JList jList = new JList();
	    final JPanel jPanel = new JPanel();
	    jPanel.setBorder(BorderFactory.createTitledBorder("Test")); // NOI18N //$NON-NLS-1$
	    final JStatusBar jStatusBar = new JStatusBar();
	    jStatusBar.setLabelWidth((int) Toolkit.getDefaultToolkit()
		    .getScreenSize().getWidth());
	    jStatusBar.setStatus("Test"); //$NON-NLS-1$
	    final JEditorPane jEditorPane = new JEditorPane();
	    // Se activa la opcion de alto contraste
	    config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST,
		    "true"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);
	    // Se comprueba la correcta aplicacion del alto contraste
	    Assert.assertTrue(this.highContrastJComboBox(jComboBox));
	    Assert.assertTrue(this.highContrastJPasswordField(jPasswordField));
	    Assert.assertTrue(this.highContrastJTextField(jTextField));
	    Assert.assertTrue(this.highContrastJTree(jTree));
	    Assert.assertTrue(this.highContrastJList(jList));
	    Assert.assertTrue(this.highContrastJPanel(jPanel));
	    Assert.assertTrue(this.highContrastJStatusBar(jStatusBar));
	    Assert.assertTrue(this.highContrastJEditorPane(jEditorPane));
	    // Se desactiva la opcion de alto contraste
	    config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST,
		    "false"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);
	    // Se comprueba la correcta desactivacion del alto contraste
	    Assert.assertTrue(this.highContrastJComboBox(jComboBox));
	    Assert.assertTrue(this.highContrastJPasswordField(jPasswordField));
	    Assert.assertTrue(this.highContrastJTextField(jTextField));
	    Assert.assertTrue(this.highContrastJTree(jTree));
	    Assert.assertTrue(this.highContrastJList(jList));
	    Assert.assertTrue(this.highContrastJPanel(jPanel));
	    Assert.assertTrue(this.highContrastJStatusBar(jStatusBar));
	    Assert.assertTrue(this.highContrastJEditorPane(jEditorPane));

	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}
    }

    /**
     * Comprobaci&oacute;n de que los componentes cambian al estilo negrita
     * correctamente
     */
    @Test
    public void testSetFontBold() {
	UtilsTest.LOGGER.info("testSetFontBold"); //$NON-NLS-1$

	try {
	    // Se obtiene la cofiguracion general
	    GeneralConfig.loadConfig(GeneralConfig.getConfig());
	    final Properties config = GeneralConfig.getConfig();
	    // Se crean los componentes para testear
	    final JComboBox jComboBox = new JComboBox();
	    final JPanel jPanel = new JPanel();
	    jPanel.setBorder(BorderFactory.createTitledBorder("Test")); // NOI18N //$NON-NLS-1$
	    final JToolBar jToolBar = new JToolBar();
	    final JButton jButton = new JButton("Test"); //$NON-NLS-1$
	    jToolBar.add(jButton);
	    // Se activa la opcion de fuente negrita
	    config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, "true"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);
	    // Se testea la correcta aplicacion del estilo de fuente en negrita
	    Assert.assertTrue(this.fontBoldJComboBox(jComboBox));
	    Assert.assertTrue(this.fontBoldJPanel(jPanel));
	    Assert.assertTrue(this.fontBoldJToolBar(jToolBar));
	    // Se desactiva la opcion de fuente negrita
	    config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE,
		    "false"); //$NON-NLS-1$
	    // Se asigna
	    GeneralConfig.loadConfig(config);
	    // Se testea la correcta desactivacion del estilo de fuente en
	    // negrita
	    Assert.assertTrue(this.fontBoldJComboBox(jComboBox));
	    Assert.assertTrue(this.fontBoldJPanel(jPanel));
	    Assert.assertTrue(this.fontBoldJToolBar(jToolBar));
	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}

    }

    /**
     * Comprobaci&oacute;n del correcto visionado u ocultaci&oacute;n del tool
     * tip
     */
    @SuppressWarnings("static-method")
	@Test
    public void testShowToolTip() {
	UtilsTest.LOGGER.info("testShowToolTip"); //$NON-NLS-1$
	try {
	    boolean show;
	    final JWindow tip = new JWindow();
	    final JButton boton = new JButton();
	    final JLabel tipText = new JLabel();

	    show = true;
	    Utils.showToolTip(show, tip, boton, tipText);
	    Assert.assertTrue(tip.isVisible() == true);
	    show = false;
	    Utils.showToolTip(show, tip, boton, tipText);
	    Assert.assertTrue(tip.isVisible() == false);
	} catch (final java.awt.HeadlessException e) {
	    // Ignoramos este error, pero no otros, para evitar fallos en tests
	    // automaticos en servidor
	}
    }
}