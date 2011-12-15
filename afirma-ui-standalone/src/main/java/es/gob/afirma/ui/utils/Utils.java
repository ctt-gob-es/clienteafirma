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
import java.awt.Component;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.border.TitledBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.main.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase con utilidades varias
 */
public final class Utils {
	
	 private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	/**
	 * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
	 * @param filepath Ruta completa al fichero.
	 */
	public static void openFile(String filepath){
		String os = System.getProperty("os.name").toLowerCase();
		Runtime rt = Runtime.getRuntime();
		try {
			if (os.indexOf( "win" ) >= 0) 
				rt.exec("cmd.exe /C \""+filepath+"\"");
			else if (os.indexOf( "mac" ) >= 0)
				rt.exec( "open " + filepath);
			else {
				//prioritized 'guess' of users' preference
				List<String> browsers = new ArrayList<String>(Arrays.asList("epiphany", "firefox", "mozilla", "konqueror",
						"netscape","opera","links","lynx"));

				StringBuffer cmd = new StringBuffer();
				for (String browser : browsers)
					cmd.append( (browsers.get(0).equals(browser)  ? "" : " || " ) + browser +" \"" + filepath + "\" ");

				rt.exec(new String[] { "sh", "-c", cmd.toString() });
			}
		} catch (IOException e) {
			e.printStackTrace();
			PrincipalGUI.setNuevoEstado(Messages.getString("Validacion.error.valide"));
		}
	}

	/**
     * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
     * @param filepath Ruta completa al fichero.
     */
    public static void openFile(File file){
        try {
            openFile(file.getCanonicalPath());
        } catch (Exception e) {
            openFile(file.getAbsolutePath());
        }
    }
	
	/**
	 * M&eacute;todo que devuelve un mnem&oacute;nico v&aacute;lido para el lenguaje que recibe como par&aacute;metro.
	 * @param listMnemonic lista de mnem&oacute;nicos que ya han sido utilizados para otros lenguajes.
	 * @param actualLanguage lenguaje para el que se est&aacute; buscando un mnem&oacute;nico
	 * @return mnem&oacute;nico seleccionado o 0 en el caso de que no se haya encontrado ninguno disponible
	 */
	public static char getLanguageMnemonic(List<Character> mnemonicList, String actualLanguage){
		//Se recorren las letras del lenguaje actual
		for (int i=0; i< actualLanguage.length(); i++) {
			//Se lee el caracter correspondiente al indice i
			char caracter = actualLanguage.charAt(i);
			//Se comprueba si se ha utilizado
			if (!mnemonicList.contains(caracter)) {
				//se anade a la lista de caracteres utilizados
				mnemonicList.add(caracter);
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
	public static void remarcar(JComponent component){
		
		if (GeneralConfig.isRemarked()){
			if (component instanceof JButton){
				final JButton button = (JButton) component;
				button.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(FocusEvent e) {
						if (button.getParent() instanceof JPanel){
						((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
						} else if (button.getParent() instanceof JToolBar){
							button.setBorder(BorderFactory.createEmptyBorder());
						}
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						if (button.getParent() instanceof JPanel){
							((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
						} else if (button.getParent() instanceof JToolBar){
							button.setBorder(BorderFactory.createEmptyBorder());
						}
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast){
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							} else if (button.getParent() instanceof JToolBar){
								button.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
							}
						} else {
							if (button.getParent() instanceof JPanel){
								((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
							} else if (button.getParent() instanceof JToolBar) {
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
                    public void focusLost(FocusEvent e) {
						textField.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						comboBox.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						((JPanel)radioButton.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						label.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						((JPanel)checkBox.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						textPane.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						editorPane.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						tree.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()|| Main.isOSHighContrast){
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
                    public void focusLost(FocusEvent e) {
						list.setBorder(BorderFactory.createEmptyBorder());
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
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
                    public void menuSelected(MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()+5));
					}
					@Override
                    public void menuDeselected(MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()-5));
					}
					@Override
                    public void menuCanceled(MenuEvent e) {
						// Vacio
					}
				});
			}
			if (component instanceof JScrollPane){
				final JScrollPane scrollPane = (JScrollPane) component;
				scrollPane.addFocusListener( new FocusListener() {
					@Override
                    public void focusLost(FocusEvent e) {
						scrollPane.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					@Override
                    public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
							scrollPane.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							scrollPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
		} else {
			if (component instanceof JButton){
				final JButton button = (JButton) component;
				button.addFocusListener(new FocusListener() {
					@Override
                    public void focusLost(FocusEvent e) {
						if (button.getParent() instanceof JPanel){
							((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
						}
					}
					
					@Override
                    public void focusGained(FocusEvent e) {
						if (button.getParent() instanceof JPanel){
							((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
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
	public static void setContrastColor (JComponent component){
		if (GeneralConfig.isHighContrast()){
			if (component instanceof JComboBox || component instanceof JPasswordField || component instanceof JTextField){
				component.setBackground(Color.WHITE);
			} else if(component instanceof JCheckBox) {
				component.setForeground(Color.WHITE);
			} else if(component instanceof JTree){
				component.setForeground(Color.WHITE);
			} else if(component instanceof JList){
				component.setForeground(Color.BLACK);
			} else if(component instanceof JPanel){
				if (component.getBorder()!=null){
					if (component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){ //$NON-NLS-1$
						if (((TitledBorder)component.getBorder())!=null){
							((TitledBorder)component.getBorder()).setTitleColor(Color.WHITE);
						}
					}
				}
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			} else if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.WHITE);
			} else if (component instanceof JEditorPane){
				component.setBackground(Color.BLACK);
			} else {
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			}
		} else {
			if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.BLACK);
			}
		}
	}
		
	/**
	 * Aplica el estilo de fuente negrita
	 * @param component Componente al que aplicar el estilo de fuente negrita.
	 */
	public static void setFontBold(JComponent component){
		//Se comprueba si el componente es de tipo panel con borde
		if(component instanceof JPanel){
			if (component.getBorder()!=null){
				if (component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){
					TitledBorder titledBorder = (TitledBorder)component.getBorder(); //Se obtiene el borde
					//Se comprueba que no sea nulo
					if (titledBorder != null){
						//Se comprueba si la configuracion pide que la fuente este en negrita
						if (GeneralConfig.isFontBold()){
							//Se indica que la fuente es negrita
							titledBorder.setTitleFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
						} else {
							//Se indica que la fuente es texto plano
							titledBorder.setTitleFont(new Font(component.getFont().getName(),Font.PLAIN , component.getFont().getSize()));
						}
					}
				} //Comprobacion del tipo de borde
			} 			
		} else {
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

	 /** Recupera una de las preferencias establecidas para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param defaultValue
     *        Valor por defecto.
     * @return Devuelve el valor de la preferencia indicada o {@code defaultValue} si no est&aacute;a establecida. */
    public static String getPreference(final String key, final String defaultValue, Preferences preferences) {
        return preferences.get(key, defaultValue);
    }

    /** Establece una preferencia para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param value
     *        Valor asignado. */
    public static void setPreference(final String key, final String value, Preferences preferences) {
        preferences.put(key, value);
    }
    
    /** Muestra un di&aacute;logo para que el usuario seleccione entre los
     * certificados mostrados. Es posible indicar que s&ocuate;lo puede haber un
     * certificado tras recuperarlos del repositorio y aplicar los filtros, en
     * cuyo caso se seleccionar&iacute; autom&aacute;ticamente. Si se pidiese
     * que se seleccione autom&aacute;ticamemte un certificado y hubiese
     * m&aacute;s de uno, se devolver&iacute;a una excepci&oacute;n.
     * @param alias
     *        Alias de los certificados entre los que el usuario debe
     *        seleccionar uno
     * @param ksm
     *        Gestor de los almac&eacute;nes de certificados a los que pertenecen los alias.
     *        Debe ser {@code null} si se quiere usar el m&eacute;todo para seleccionar
     *        otra cosa que no sean certificados X.509 (como claves de cifrado)
     * @param parentComponent
     *        Componente padre (para ls amodalidad)
     * @param checkPrivateKeys
     *        Indica si se debe comprobar que el certificado tiene clave
     *        privada o no, para no mostrar aquellos que carezcan de ella
     * @param checkValidity
     *        Indica si se debe comprobar la validez temporal de un
     *        certificado al ser seleccionado
     * @param showExpiredCertificates
     *        Indica si se deben o no mostrar los certificados caducados o
     *        aun no v&aacute;lidos
     * @param certFilters
     *        Filtros sobre los certificados a mostrar
     * @param mandatoryCertificate
     *        Indica si los certificados disponibles (tras aplicar el
     *        filtro) debe ser solo uno.
     * @return Alias seleccionado por el usuario
     * @throws AOCancelledOperationException
     *         Si el usuario cancela manualmente la operaci&oacute;n
     * @throws AOCertificatesNotFoundException
     *         Si no hay certificados que mostrar al usuario */
    public static String showCertSelectionDialog(final String[] alias,
                                                       final AOKeyStoreManager ksm,
                                                       final Object parentComponent,
                                                       final boolean checkPrivateKeys,
                                                       final boolean checkValidity,
                                                       final boolean showExpiredCertificates,
                                                       final List<CertificateFilter> certFilters,
                                                       final boolean mandatoryCertificate) throws AOCertificatesNotFoundException {
        if (alias == null || alias.length == 0) {
            throw new AOCertificatesNotFoundException("El almac\u00E9n no conten\u00EDa entradas"); //$NON-NLS-1$
        }

        final Map<String, String> aliassesByFriendlyName =
                KeyStoreUtilities.getAliasesByFriendlyName(
                       alias, 
                       ksm, 
                       checkPrivateKeys,
                       showExpiredCertificates, 
                       certFilters
                );

        // Miramos si despues de filtrar las entradas queda alguna o se ha
        // quedado la lista vacia
        if (aliassesByFriendlyName.size() == 0) {
            throw new AOCertificatesNotFoundException("El almacen no contenia entradas validas"); //$NON-NLS-1$
        }

        // Si se ha pedido que se seleccione automaticamente un certificado, se
        // seleccionara
        // si hay mas de un certificado que se ajuste al filtro, se dara a
        // elegir
        if (mandatoryCertificate && aliassesByFriendlyName.size() == 1) {
            return aliassesByFriendlyName.keySet().toArray()[0].toString();
        }

        // Ordenamos el array de alias justo antes de mostrarlo, ignorando entre
        // mayusculas y min�sculas
        final String[] finalOrderedAliases = aliassesByFriendlyName.values().toArray(new String[0]);
        Arrays.sort(finalOrderedAliases, new Comparator<String>() {
            @Override
            public int compare(final String o1, final String o2) {
                if (o1 == null && o2 == null) {
                    return 0;
                }
                else if (o1 == null) {
                    return 1;
                }
                else if (o2 == null) {
                    return -1;
                }
                else{
                    return o1.compareToIgnoreCase(o2);
                }
            }
        });

        final Object o = CustomDialog.showInputDialog(
             (Component)parentComponent, true, Messages.getString("CustomDialog.showInputDialog.certificate.message"), KeyEvent.VK_S,  //$NON-NLS-1$
             Messages.getString("CustomDialog.showInputDialog.certificate.title"), //$NON-NLS-1$
             JOptionPane.PLAIN_MESSAGE,
             finalOrderedAliases,
             null
        );

        final String certName;
        if (o != null) {
            certName = o.toString();
        }
        else {
            throw new AOCancelledOperationException("Operacion de seleccion de certificado cancelada"); //$NON-NLS-1$
        }

        for (final String al : aliassesByFriendlyName.keySet().toArray(new String[aliassesByFriendlyName.size()])) {
            if (aliassesByFriendlyName.get(al).equals(certName)) {
                if (checkValidity && ksm != null) {
                    boolean rejected = false;
                    for (final KeyStore ks : ksm.getKeyStores()) {
                        try {
                            if (!ks.containsAlias(al)) {
                                continue;
                            }
                        }
                        catch (final Exception e) {
                            continue;
                        }

                        String errorMessage = null;
                        try {
                            ((X509Certificate)ks.getCertificate(al)).checkValidity();
                        }
                        catch (final CertificateExpiredException e) {
                            errorMessage = Messages.getString("CustomDialog.showInputDialog.certificate.expiredMessage"); //$NON-NLS-1$
                        }
                        catch (final CertificateNotYetValidException e) {
                            errorMessage = Messages.getString("CustomDialog.showInputDialog.certificate.invalidMessage"); //$NON-NLS-1$
                        }
                        catch (final KeyStoreException e) {
                            errorMessage = Messages.getString("CustomDialog.showInputDialog.certificate.exceptionMessage"); //$NON-NLS-1$
                        }

                        if (errorMessage != null) {
                            LOGGER.warning("Error durante la validacion: " + errorMessage); //$NON-NLS-1$
                            if (CustomDialog.showConfirmDialog(
                                  (Component)parentComponent, true,
                                  errorMessage,
                                  Messages.getString("CustomDialog.warning"), //$NON-NLS-1$
                                  AOUIFactory.YES_NO_OPTION,
                                  AOUIFactory.WARNING_MESSAGE
                            ) == AOUIFactory.YES_OPTION) {
                                return al;
                            }
                            rejected = true;
                        }

                        if (rejected) {
                            throw new AOCancelledOperationException("Se ha reusado un certificado probablemente no valido"); //$NON-NLS-1$
                        }
                    }
                }
                return al;
            }
        }
        return null;
    }
    
    /** Recupera el manejador de claves asociado a un certificado seg&uacute;n el
     * repositorio en el que se aloja.
     * @param store Almace&eacute;n de claves del certificado.
     * @param parent Componente sobre el que se deben visualizar los
     *               di&aacute;logos modales (normalmente un <code>java.awt.Comonent</code>)
     * @return Manejador para la solicitud de la clave. */
    public static PasswordCallback getCertificatePC(final AOKeyStore store, final Object parent) {
        if (store == AOKeyStore.WINDOWS || store == AOKeyStore.WINROOT
            || store == AOKeyStore.WINADDRESSBOOK
            || store == AOKeyStore.WINCA
            || store == AOKeyStore.SINGLE
            || store == AOKeyStore.MOZ_UNI
            || store == AOKeyStore.PKCS11
            || store == AOKeyStore.APPLE) {
                return new NullPasswordCallback();
        }
        return new UIPasswordCallbackAccessibility(Messages.getString("CustomDialog.showInputDialog.certificate.pass"), (Component)parent, Messages.getString("CustomDialog.showInputPasswordDialog.title"), KeyEvent.VK_O, Messages.getString("CustomDialog.showInputPasswordDialog.title")); //$NON-NLS-1$
    }
    
    /**
	 * Método que sumbraya el mnemónico correspondiente para texto HTML.
	 *
	 * @param text	Texto en el que hay que subrayar el carácter.
	 * @param key	Caracter a subrayar.
	 *
	 * @return	Cadena con el texto subrayado.
	 */
	public static String remarkMnemonic( String text, final int key) {
		String newText = text;
		int pos = text.indexOf(key); //Se obtiene el índice del caracter
		if (pos == -1) {//Se busca en minúscula
			char keyChar = (char) key;
			pos = text.indexOf(String.valueOf(keyChar).toLowerCase());
		}
		if (pos != -1) {
			//Se subraya
			newText = text.substring(0, pos) + "<u>" + text.charAt(pos) + "</u>" + text.substring(pos + 1);
		}
		return newText;
	}
	
	/**
     * Obtiene un filtro de fichero correspondiente para almacenes de certificados del tipo PCKS#12 y .
     * @return filtro
     */
    public static final FileFilter getRepositoryFileFilter() {
    	FileFilter fileFilter = new ExtFilter(new String[] {"p12", "pfx"}, Messages.getString("Repository.filefilter")); //$NON-NLS-1$ //$NON-NLS-2$
    	return fileFilter;
    }
}
