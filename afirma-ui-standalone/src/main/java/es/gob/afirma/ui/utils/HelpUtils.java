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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dialog.ModalExclusionType;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.help.DefaultHelpBroker;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.JHelp;
import javax.help.JHelpContentViewer;
import javax.help.JHelpNavigator;
import javax.help.WindowPresentation;
import javax.help.plaf.basic.BasicIndexCellRenderer;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.JWindow;
import javax.swing.text.html.HTMLDocument;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase con utilidades relacionadas con la ayuda de la aplicacion
 */
public class HelpUtils {

	/**
	 * Mapa de componentes y su indice en la ayuda.
	 */
	private static Hashtable<String, Component> components = new Hashtable<String, Component>();
	/**
	 * Controlador de ayuda.
	 */
	private static HelpBroker helpBroker = null;
	/**
	 * Conjunto de información de ayuda.
	 */
	private static HelpSet helpset = null;
	/**
	 * Variable que almacena el icono original del botón de ayuda.
	 */
	public static final ImageIcon IMAGEICONHELP = new ImageIcon(HelpUtils.class.getResource("/resources/images/help.png"));
 
	/**
	 * Devuelve la ayuda.
	 */
	static {
		getHelp();
		
	}
	
	/**
	 * Genera la ayuda
	 * @return	Ventana con el panel de ayuda
	 */
	static HelpBroker getHelp() {

		if (helpBroker == null) {
			try {
				// Cargamos el archivo de datos de la ayuda
				URL hsURL = HelpBroker.class.getResource("/help/help_set-es_ES.hs");
				
				// Creamos la ventana de ayuda
				HelpSet helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
				helpBroker = helpset.createHelpBroker();
				
				helpBroker.initPresentation();
				WindowPresentation wp = ((DefaultHelpBroker)helpBroker).getWindowPresentation();
				JFrame helpwindow = (JFrame) wp.getHelpWindow();
				
				//La ventana de ayuda no debe ser bloqueada por ninguna ventana de la aplicacion
				helpwindow.setModalExclusionType(ModalExclusionType.APPLICATION_EXCLUDE);
				
				// Introducimos el icono en la ventana
				Image icon = Toolkit.getDefaultToolkit().createImage(HelpUtils.class.getClassLoader().getResource("resources/images/afirma_ico.png"));
				helpwindow.setIconImage(icon);	
							
				
			} catch (Exception ex) {
				ex.printStackTrace();
			}
			
			visualize(false);
			
		}

		
		return helpBroker;
	}

	/**
	 * Cambia el idioma de la ayuda
	 * @param language	Idioma al que se debe cambiar
	 */
	public static void change(String language) {
		try {
			// Carga el nuevo archivos de datos para ese idioma
			URL hsURL = HelpBroker.class.getResource("/help/help_set-" + language + ".hs");
			helpset = new HelpSet(HelpBroker.class.getClassLoader(), hsURL);
			helpBroker = helpset.createHelpBroker();
			Enumeration<String> enumeration = components.keys();
			while(enumeration.hasMoreElements()) {
				String key = enumeration.nextElement();
				Component component = components.get(key);
				helpBroker.enableHelpKey(component, key, helpset);
			}			

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * Activa el acceso a la ventana de ayuda por la pulsacion de una tecla
	 * @param component	Componente que se va a mostrar al pulsar la tecla
	 * @param id Identificador de la entrada de la ayuda a la que se desea acceder.
	 */
	public static void enableHelpKey(Component component, String id) {
		
		components.put(id, component);
		getHelp().enableHelpKey(component, id, helpset);
		if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
		} else if (GeneralConfig.isBigFontSize()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
		} else if (GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
		} else {
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
		}
		//Alto contraste
		WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
		Window helpwindow = (Window) wp.getHelpWindow();
		if (Main.isOSHighContrast || GeneralConfig.isHighContrast()){
			checkHelpAccessibility(helpwindow, true);
		} else {
			checkHelpAccessibility(helpwindow, false);
		}
	}	
	
	/**
	 * Visualiza la pagina de ayuda indicada.
	 * @param pagina id de pagina
	 */
	public static void visualize(String pagina){
		getHelp().setCurrentID(pagina);
		if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
		} else if (GeneralConfig.isBigFontSize()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
		} else if (GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
		} else {
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
		}
		
		//Alto contraste
		WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
		Window helpwindow = (Window) wp.getHelpWindow();
		if (Main.isOSHighContrast || GeneralConfig.isHighContrast()){
			checkHelpAccessibility(helpwindow, true);
		} else {
			checkHelpAccessibility(helpwindow, false);
		}
	}

	/**
	 * Visualiza la ayuda en la pagina de "Introduccion".
	 * @param show indica si se mostrara o no
	 */
	public static void visualize(boolean show) {
		
		getHelp().setDisplayed(show);
				
		getHelp().setCurrentID("introduccion");
		if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
		} else if (GeneralConfig.isBigFontSize()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
		} else if (GeneralConfig.isFontBold()){
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
		} else {
			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
		}
		
		//Alto contraste
		WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
		Window helpwindow = (Window) wp.getHelpWindow();
		if (Main.isOSHighContrast || GeneralConfig.isHighContrast()){
			checkHelpAccessibility(helpwindow, true);
		} else {
			checkHelpAccessibility(helpwindow, false);
		}
	}

	/**
	 * Genera el bot&oacute;n de ayuda que apuntar&aacute; a la p&aacute;gina dada.
	 * @param pagina P&aacute;gina a mostrar cuando se puelse el bot&oacute;n de ayuda.
	 * @return bot&oacute;n de ayuda
	 */
	public static JButton helpButton(final String pagina) {

		final JButton botonAyuda = new JButton(IMAGEICONHELP);
		botonAyuda.setToolTipText(Messages.getString("ayudaHTML.contenido"));
		botonAyuda.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		if (!pagina.equals("perfiles.usuario")){
			botonAyuda.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("ayudaHTML.contenido")));
			botonAyuda.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("ayudaHTML.contenido")));
		}
		botonAyuda.setMnemonic(KeyEvent.VK_H); //Se le asigna un mnemonico al boton de ayuda
		botonAyuda.getAccessibleContext().setAccessibleName(botonAyuda.getToolTipText());
		//Se asigna una dimension al boton segun su icono
		Dimension dimension = new Dimension(12,27);
		botonAyuda.setPreferredSize(dimension);
		
		botonAyuda.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		botonAyuda.setContentAreaFilled(false); //area del boton invisible
		
		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();
		
		botonAyuda.addFocusListener(new FocusListener() {
			/**
			 * Evento que se lanza cuando un componente pierde el foco.
			 */
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, botonAyuda, tipText);
			}
			/**
			 * Evento que se lanza cuando un componente tiene el foco.
			 */
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, botonAyuda, tipText);
			}
		});
		
		//Foco para el modo alto contraste
		if (GeneralConfig.isHighContrast()) {
			botonAyuda.addFocusListener(new FocusListener() {
				/**
				 * Evento que se lanza cuando un componente pierde el foco.
				 */
				public void focusLost(FocusEvent e) {
					//Se quita el borde del botón al perder el foco
					botonAyuda.setBorder(BorderFactory.createEmptyBorder());
				}
				/**
				 * Evento que se lanza cuando un componente tiene el foco.
				 */
				public void focusGained(FocusEvent e) {
					//Se muestra un borde en el botón cuando este tiene el foco
					botonAyuda.setBorder(BorderFactory.createLineBorder(Color.ORANGE, 1));
				}
			});
		}

		//Accion para desplegar la pantalla de ayuda
		botonAyuda.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				getHelp().setDisplayed(true);
				getHelp().setCurrentID(pagina);
				if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
					helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
					helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
				} else if (GeneralConfig.isBigFontSize()){
					helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
				} else if (GeneralConfig.isFontBold()){
					helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
				} else {
					helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
				}
				
				//Alto contraste
				WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
				Window helpwindow = (Window) wp.getHelpWindow();
				if (Main.isOSHighContrast || GeneralConfig.isHighContrast()){
					//helpwindow.setBackground(Color.BLACK);
					checkHelpAccessibility(helpwindow, true);
				} else {
					//helpwindow.setBackground(Color.WHITE);
					checkHelpAccessibility(helpwindow, false);
				}
			}
		});
		Utils.remarcar(botonAyuda);

		return botonAyuda;
	}

	/**
	 * Muestra la ayuda por la p&aacute;gina indicada. Si no se indica, se
	 * abrir&aacute;a por la p&aacute;gina principal. 
	 * @param pagina Identificador de p&aacute;gina.
	 */
	public static void showHelp(final String pagina) {
	    getHelp().setDisplayed(true);
	    if (pagina != null) {
	        try {
	            getHelp().setCurrentID(pagina);
	            if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	    		} else if (GeneralConfig.isBigFontSize()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
	    		} else if (GeneralConfig.isFontBold()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	    		} else {
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	    		}
	        } catch (Exception e) {
	        	if (GeneralConfig.isBigFontSize() && GeneralConfig.isFontBold()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), helpBroker.getFont().getStyle(), 16));
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, helpBroker.getFont().getSize()));
	    		} else if (GeneralConfig.isBigFontSize()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 16));	
	    		} else if (GeneralConfig.isFontBold()){
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.BOLD, 11));
	    		} else {
	    			helpBroker.setFont(new Font(helpBroker.getFont().getName(), Font.PLAIN, 11));
	    		}
	            /* No hacemos nada para que se abra por la pagina principal */
	        }
	        
	       //Alto contraste
	        WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
			Window helpwindow = (Window) wp.getHelpWindow();
	        if (Main.isOSHighContrast || GeneralConfig.isHighContrast()){
				checkHelpAccessibility(helpwindow, true);
			} else {
				checkHelpAccessibility(helpwindow, false);
			}
	    }
	}

	/**
	 * Establece el modo alto contraste para el editorPane que recibe como parámetro.
	 * @param editorPane panel de edición.
	 */
	private static void setHighContrastEditorPane(JEditorPane editorPane, boolean activate){
		if (editorPane !=null) {
			if (editorPane.getDocument() instanceof HTMLDocument){
				HTMLDocument h = (HTMLDocument)editorPane.getDocument();
				//Se establece el color de la la letra a blanco
				editorPane.setContentType("text/html");
				String bodyRule ="";
				if (activate) {
					bodyRule = "body { color: \"white\";}";
					editorPane.setBackground(Color.BLACK);
				} else {
					//h.getStyleSheet().
					bodyRule = "body { color: \"black\";}";
					editorPane.setBackground(Color.WHITE);
				}
				
				h.getStyleSheet().addRule(bodyRule);
			}		
		}
	}

	/**
	 * Establece las opciones de accesibilidad para la ayuda.
	 * @param activate Indica si está activado el alto contraste.
	 */
	public static void checkHelpAccessibility(boolean activate){
		WindowPresentation wp = ((DefaultHelpBroker)getHelp()).getWindowPresentation();
		JFrame helpwindow = (JFrame) wp.getHelpWindow();
		checkHelpAccessibility(helpwindow, activate);
	}
	
	/**
	 * Chequeo de la accesibilidad para la ventana de ayuda.
	 * @param frame ventana de ayuda.
	 */
	private static void checkHelpAccessibility(Window frame, boolean activate) {
		JTabbedPane jtp = new JTabbedPane();
		if (frame != null) {
			Component component = null;
			 // recorre componentes del frame
			 for (int i=0; i<frame.getComponentCount(); i++) {
				 component = frame.getComponent(i);
				 if (component == null) {
		              continue;
				 }
				 //Se recorren todos los componentes de la ventana de ayuda hasta llegar al editorPane
				 if (component instanceof JRootPane) {
					 for(Component component2 : ((JRootPane) component).getComponents()) {
						 if(component2 instanceof JLayeredPane) {
							 for(Component component3 : ((JLayeredPane) component2).getComponents()) {
								 if(component3 instanceof JPanel) {
									 for(Component component4 : ((JPanel) component3).getComponents()) {
										 if(component4 instanceof JHelp) {
											 
											 for(Component component5 : ((JHelp) component4).getComponents()) {
												 
												 if(component5 instanceof JSplitPane) {
													 for(Component component6 : ((JSplitPane) component5).getComponents()) {
														 
														 if(component6 instanceof JTabbedPane) {
															jtp = (JTabbedPane)component6;
																 
															 
														 }
														 if(component6 instanceof JHelpContentViewer) {
															 for(Component component7 : ((JHelpContentViewer) component6).getComponents()) {
																 if(component7 instanceof JScrollPane) {
																	 for(Component component8 : ((JScrollPane) component7).getComponents()) {
																		 if(component8 instanceof JViewport) {
																			 for(Component component9 : ((JViewport) component8).getComponents()) {
																				if (component9 instanceof JEditorPane) {
																					//component9.setBackground(Color.BLACK);
																					//Se activa el alto contraste para el editor pane
																					setHighContrastComponentTabbedPane(jtp,(JEditorPane)component9, activate);
																					HelpUtils.setHighContrastEditorPane((JEditorPane)component9, activate);
																				}
																			 }
																		 }
																	 }
																 }
															 }
														 }	 
													 }
												 } 
											 }
										 }
									 }
								 }
							 }
						 }
					 }
				 }
			 }
		}
	}
	/**
	 * Establece el alto contraste para el indice de la ayuda.
	 * @param tabbedPane pestañas del indice.
	 * @param editorPane panel donde se muestra la ayuda.
	 * @param activate indica si el modo alto contraste está activado o no.
	 */
	private static void setHighContrastComponentTabbedPane(JTabbedPane tabbedPane, final JEditorPane editorPane, final boolean activate){
        if (tabbedPane != null) {
            for(Component componentTabbed : tabbedPane.getComponents()) {
                if (componentTabbed instanceof JHelpNavigator) {
                    for(Component panel : ((JHelpNavigator)componentTabbed).getComponents()) {
                        if (panel instanceof JScrollPane) {
                            for(Component component : ((JScrollPane) panel).getComponents()) {
                                 if(component instanceof JViewport) {
                                     for(Component component9 : ((JViewport) component).getComponents()) {
                                    	 if (component9 instanceof JTree) {
                                    		 
                                    		 /*CAMBIAR EL COMPORTAMIENTO DEL RENDER DE LAS CELDAS*/
                                    		 TreeCellRenderer cr = ((JTree) component9).getCellRenderer();
                                    		 
                                    		 if(cr instanceof BasicIndexCellRenderer){
                                    			 ((JTree) component9).setCellRenderer(new BasicIndexCellRenderer() {
													/**
													 * UID.
													 */
													private static final long serialVersionUID = 1L;
													/**
													 * Obtiene una celda con su configuración de alto contraste aplicada.
													 */
													public Component getTreeCellRendererComponent(JTree pTree,Object pValue,boolean pIsSelected,boolean pIsExpanded,boolean pIsLeaf,int pRow,boolean pHasFocus) {
														//DefaultMutableTreeNode node = (DefaultMutableTreeNode) pValue;
														super.getTreeCellRendererComponent(pTree,pValue,pIsSelected,pIsExpanded,pIsLeaf,pRow,pHasFocus);
														if (activate){
															setForeground(Color.WHITE);
															setBackgroundNonSelectionColor(Color.BLACK);
														}
														else{
															setForeground(Color.BLACK);
															setBackgroundNonSelectionColor(Color.WHITE);
														}
														return (this);
													}
												});
                                    		 }
                                    		 else /*if(cr instanceof BasicTOCCellRenderer)*/{
                                    			 ((JTree) component9).setCellRenderer(new DefaultTreeCellRenderer() {
 													/**
													 * UID.
													 */
													private static final long serialVersionUID = 1L;

													public Component getTreeCellRendererComponent(JTree pTree,Object pValue,boolean pIsSelected,boolean pIsExpanded,boolean pIsLeaf,int pRow,boolean pHasFocus) {
 														//DefaultMutableTreeNode node = (DefaultMutableTreeNode) pValue;
 														super.getTreeCellRendererComponent(pTree,pValue,pIsSelected,pIsExpanded,pIsLeaf,pRow,pHasFocus);
 														if(activate){
 															setForeground(Color.WHITE);
 															setBackgroundNonSelectionColor(Color.BLACK);
 														}
 														else{
 															setForeground(Color.BLACK);
 															setBackgroundNonSelectionColor(Color.WHITE);
 														}
 														return (this);
 													}
 												});
                                    		 }
                                    		 
                                    		 if (activate){
                                    			 ((JTree)component9).setBackground(Color.black);
                                    		 } else { 
                                    			 ((JTree)component9).setBackground(Color.WHITE);
                                    		 }
                                             ((JTree)component9).addMouseListener(new MouseListener() {
     											
                                            	/**
                                            	 * Captura de evento de raton. Liberar boton.
                                            	 */
     											@Override
     											public void mouseReleased(MouseEvent e) {
     												// TODO Auto-generated method stub
     												HelpUtils.setHighContrastEditorPane(editorPane, activate);
     											}
     											
     											/**
     											 * Captura de evento de raton. Presionar boton.
     											 */
     											@Override
     											public void mousePressed(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											
     											/**
     											 * Captura de evento de raton. Puntero del ratón fuera del componente.
     											 */
     											@Override
     											public void mouseExited(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											
     											/**
     											 * Captura de evento de raton. Puntero del ratón dentro del componente.
     											 */
     											@Override
     											public void mouseEntered(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											/**
     											 * Captura de evento de raton. Hacer click.
     											 */
     											@Override
     											public void mouseClicked(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     										});
                                            ((JTree)component9).addKeyListener(new KeyListener() {
												
                                            	/**
                                            	 * Tecla presionada.
                                            	 */
												@Override
												public void keyTyped(KeyEvent e) {
													// TODO Auto-generated method stub
													
												}
												/**
												 * Tecla liberada.
												 */
												@Override
												public void keyReleased(KeyEvent e) {
													// TODO Auto-generated method stub
													HelpUtils.setHighContrastEditorPane(editorPane, activate);
												}
												
												/**
												 * Se ha pulsado una tecla.
												 */
												@Override
												public void keyPressed(KeyEvent e) {
													// TODO Auto-generated method stub
													
												}
											});
                                         }
                                     }   
                                 }                                 
                             }
                         } if (panel instanceof JPanel){
                        	 for(Component component : ((JPanel) panel).getComponents()) {
                        		 if (component instanceof JTextField){
                        			 JTextField campo = (JTextField) component;
                        			 campo.addKeyListener(new KeyListener() {

                                     	/**
                                     	 * Tecla presionada.
                                     	 */
										@Override
										public void keyTyped(KeyEvent e) {
											// TODO Auto-generated method stub
											
										}
										/**
										 * Tecla liberada.
										 */
										@Override
										public void keyReleased(KeyEvent e) {
											// TODO Auto-generated method stub
											if (e.getKeyCode()==KeyEvent.VK_ENTER){
												HelpUtils.setHighContrastEditorPane(editorPane, activate);
											}
										}
										/**
										 * Se ha pulsado una tecla.
										 */
										@Override
										public void keyPressed(KeyEvent e) {
											// TODO Auto-generated method stub
											
										}
									});
                        		 }
                        	 }
                         }
                    }
                }
            }
        }
    }

}


