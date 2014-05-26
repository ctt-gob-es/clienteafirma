/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Font;
import java.awt.IllegalComponentStateException;
import java.awt.Point;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
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
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;

/** Utilidades varias */
public final class Utils {

	private Utils() {
		// No permitimos la instanciacion
	}

	 private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
	 * @param filepath Ruta completa al fichero.
	 */
	public static void openFile(final String filepath){
		openFile(new File(filepath));
	}

	/**
     * Abre un fichero en la aplicaci&oacute;n predefinida por el sistema operativo actual.
     * @param file Fichero.
     */
    public static void openFile(final File file){
    	try {
			Desktop.getDesktop().open(file);
		}
		catch (final IOException e) {
			LOGGER.info("Error al intentar abrir el fichero " + file.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			PrincipalGUI.setNuevoEstado(Messages.getString("Validacion.error.valide")); //$NON-NLS-1$
		}
    }

	/**
	 * M&eacute;todo que devuelve un mnem&oacute;nico v&aacute;lido para el lenguaje que recibe como par&aacute;metro.
	 * @param mnemonicList lista de mnem&oacute;nicos que ya han sido utilizados para otros lenguajes.
	 * @param actualLanguage lenguaje para el que se est&aacute; buscando un mnem&oacute;nico
	 * @return mnem&oacute;nico seleccionado o 0 en el caso de que no se haya encontrado ninguno disponible
	 */
	public static char getLanguageMnemonic(final List<Character> mnemonicList, final String actualLanguage){
		//Se recorren las letras del lenguaje actual
		for (int i=0; i< actualLanguage.length(); i++) {
			//Se lee el caracter correspondiente al indice i
			final char caracter = actualLanguage.charAt(i);
			//Se comprueba si se ha utilizado
			if (!mnemonicList.contains(Character.valueOf(caracter))) {
				//se anade a la lista de caracteres utilizados
				mnemonicList.add(Character.valueOf(caracter));
				//Se devuelve
				return caracter;
			}
		}
		//TODO: mejorar para que en el caso de que no encuentre mnemonico pueda cambiar alguno de los anteriores
		return 0;
	}

	/**
	 * Configura el formato del remarcado del componente al ser seleccionado.
	 * @param component El componente seleccionado.
	 */
	public static void remarcar(final JComponent component){

		if (GeneralConfig.isRemarked()){
			if (component instanceof JButton){
				final JButton button = (JButton) component;
				button.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						if (button.getParent() instanceof JPanel){
						((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
						} else if (button.getParent() instanceof JToolBar){
							button.setBorder(BorderFactory.createEmptyBorder());
						}
					}

					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							} else if (button.getParent() instanceof JToolBar){
								button.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							}
						} else {
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
							} else if (button.getParent() instanceof JToolBar){
								button.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
							}

						}
					}
				});
			}
			if (component instanceof JToggleButton){
				final JToggleButton button = (JToggleButton) component;
				button.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						if (button.getParent() instanceof JPanel){
							((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
						}
						else if (button.getParent() instanceof JToolBar){
							button.setBorder(BorderFactory.createEmptyBorder());
						}
					}

					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast()){
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							}
							else if (button.getParent() instanceof JToolBar){
								button.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							}
						}
						else {
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
							}
							else if (button.getParent() instanceof JToolBar) {
								button.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
							}

						}
					}
				});
			}
			if (component instanceof JTextField){
				final JTextField textField = (JTextField) component;
				textField.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						textField.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							textField.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							textField.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JComboBox){
				final JComboBox comboBox = (JComboBox) component;
				comboBox.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						comboBox.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}

					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							comboBox.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							comboBox.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JRadioButton){
				final JRadioButton radioButton = (JRadioButton) component;
				radioButton.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						((JPanel)radioButton.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}

					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							((JPanel)radioButton.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)radioButton.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JLabel){
				final JLabel label = (JLabel) component;
				label.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						label.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							label.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							label.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JCheckBox){
				final JCheckBox checkBox = (JCheckBox) component;
				checkBox.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						((JPanel)checkBox.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}

					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							((JPanel)checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JTextPane){
				final JTextPane textPane = (JTextPane) component;
				textPane.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						textPane.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							textPane.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							textPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JEditorPane){
				final JEditorPane editorPane = (JEditorPane) component;
				editorPane.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						editorPane.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast()){
							editorPane.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							editorPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JTree){
				final JTree tree = (JTree) component;
				tree.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						tree.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast()){
							tree.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							tree.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JList){
				final JList list = (JList) component;
				list.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						list.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							list.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							list.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JMenu){
				final JMenu menu = (JMenu) component;
				menu.addMenuListener(new MenuListener() {
					@Override
                    public void menuSelected(final MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()+5));
					}
					@Override
                    public void menuDeselected(final MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()-5));
					}
					@Override
                    public void menuCanceled(final MenuEvent e) {
						// Vacio
					}
				});
			}
			if (component instanceof JScrollPane){
				final JScrollPane scrollPane = (JScrollPane) component;
				scrollPane.addFocusListener( new FocusListener() {
					@Override
                    public void focusLost(final FocusEvent e) {
						scrollPane.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					@Override
                    public void focusGained(final FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast()){
							scrollPane.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							scrollPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
		}
	}

	/**
	 * Configura el comportamiento de ciertos componentes en Alto Contraste
	 * @param component Componente al que aplicar el alto contraste
	 */
	public static void setContrastColor (final JComponent component){
		if (GeneralConfig.isHighContrast()){
			if (component instanceof JComboBox || component instanceof JPasswordField || component instanceof JTextField){
				component.setBackground(Color.WHITE);
			}
			else if(component instanceof JCheckBox) {
				component.setForeground(Color.WHITE);
			}
			else if(component instanceof JTree){
				component.setForeground(Color.WHITE);
			}
			else if(component instanceof JList){
				component.setForeground(Color.BLACK);
			}
			else if(component instanceof JPanel){
				if (component.getBorder()!=null &&
				   component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder") && //$NON-NLS-1$
				   (TitledBorder)component.getBorder()!=null){
						((TitledBorder)component.getBorder()).setTitleColor(Color.WHITE);
				}
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			}
			else if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.WHITE);
			}
			else if (component instanceof JEditorPane){
				component.setBackground(Color.BLACK);
			}
			else {
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			}
		}
		else {
			if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.BLACK);
			}
		}
	}

	/**
	 * Aplica el estilo de fuente negrita
	 * @param component Componente al que aplicar el estilo de fuente negrita.
	 */
	public static void setFontBold(final JComponent component){
		//Se comprueba si el componente es de tipo panel con borde
		if(component instanceof JPanel){
			if (component.getBorder()!=null && component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")) { //$NON-NLS-1$
				final TitledBorder titledBorder = (TitledBorder)component.getBorder(); //Se obtiene el borde
				//Se comprueba que no sea nulo
				if (titledBorder != null){
					//Se comprueba si la configuracion pide que la fuente este en negrita
					if (GeneralConfig.isFontBold()){
						//Se indica que la fuente es negrita
						titledBorder.setTitleFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
					}
					else {
						//Se indica que la fuente es texto plano
						titledBorder.setTitleFont(new Font(component.getFont().getName(),Font.PLAIN , component.getFont().getSize()));
					}
				} //Comprobacion del tipo de borde
			}
		}
		else {
			//Se comprueba si la configuracion pide que la fuente este en negrita
			if (GeneralConfig.isFontBold()){
				if (component instanceof JToolBar){
					//Se indica que la fuente es negrita
					for (int i=0;i<component.getComponentCount();i++){
						component.getComponent(i).setFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
					}
				} else {
					//Se indica que la fuente es negrita
					component.setFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
				}
			} else {
				if (component instanceof JToolBar){
					//Se indica que la fuente es texto plano
					for (int i=0;i<component.getComponentCount();i++){
						component.getComponent(i).setFont(new Font(component.getFont().getName(),Font.PLAIN , component.getFont().getSize()));
					}
				} else {
					//Se indica que la fuente es texto plano
					component.setFont(new Font(component.getFont().getName(),Font.PLAIN , component.getFont().getSize()));
				}

			}
		}
	}

    /** Metodo que sumbraya el mnemonico correspondiente para texto HTML.
	 * @param text Texto en el que hay que subrayar el caracter.
	 * @param key Caracter a subrayar.
	 * @return Cadena con el texto subrayado. */
	public static String remarkMnemonic( final String text, final int key) {
		String newText = text;
		int pos = text.indexOf(key); //Se obtiene el indice del caracter
		if (pos == -1) {//Se busca en minuscula
			final char keyChar = (char) key;
			pos = text.indexOf(String.valueOf(keyChar).toLowerCase());
		}
		if (pos != -1) {
			//Se subraya
			newText = text.substring(0, pos) + "<u>" + text.charAt(pos) + "</u>" + text.substring(pos + 1); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return newText;
	}

	/**
     * Obtiene un filtro de fichero correspondiente para almacenes de certificados del tipo PCKS#12.
     * @return filtro
     */
    public static FileFilter getRepositoryFileFilterPkcs12() {
    	return new ExtFilter(new String[] {"p12", "pfx"}, Messages.getString("Repository.filefilter.pkcs12")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    /**
     * Obtiene un filtro de fichero correspondiente para almacenes de certificados del tipo PCKS#11.
     * @return filtro
     */
    public static FileFilter getRepositoryFileFilterPkcs11() {
    	return new ExtFilter(new String[] {"dll", "so"}, Messages.getString("Repository.filefilter.pkcs11")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    /**
     * Muestra u oculta un <i>tooltip</i> relacionado con un bot&oacute;n.
     * @param show Boolean que indica si se muestra el tooltip
     * @param tip JWindow que muestra el contenido del tooltip
     * @param boton JButton al que se relaciona el tooltip
     * @param tipText JLabel que muestra el contenido del tooltip
     */
    public static void showToolTip(final boolean show, final JWindow tip, final JButton boton, final JLabel tipText){
    	tipText.setText(boton.getToolTipText());
    	tip.setBackground((Color)UIManager.get("ToolTip.background")); //$NON-NLS-1$
    	tipText.setBackground((Color)UIManager.get("ToolTip.background")); //$NON-NLS-1$
    	tipText.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.BLACK), BorderFactory.createEmptyBorder(0,3,0,3)));
    	tipText.setFont((Font)UIManager.get("ToolTip.font")); //$NON-NLS-1$
    	tipText.setOpaque(true);
    	tip.add(tipText);
    	Point p = new Point();
    	try{
    		p = boton.getLocationOnScreen();
    	}
    	catch(final IllegalComponentStateException e){
    		// Se ignora
    	}
    	int factor = 0;
    	if (boton.getSize().getHeight()>34){
    		factor = (int)(boton.getSize().getHeight()*0.5);
    	}
    	tip.setLocation((int)p.getX(),(int)p.getY()+30+factor);
		tip.pack();
		tip.setVisible(show);
    }
}
