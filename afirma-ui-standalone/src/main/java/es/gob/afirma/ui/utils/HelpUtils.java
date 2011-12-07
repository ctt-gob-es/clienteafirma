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
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.text.html.HTMLDocument;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase con utilidades relacionadas con la ayuda de la aplicacion
 */
public class HelpUtils {
	
	private static Hashtable<String, Component> components = new Hashtable<String, Component>();
	private static HelpBroker helpBroker = null;
	private static HelpSet helpset = null;
//	private static Style hightContrast;
//    private static Style normalContrast;
	/**
	 * Variable que almacena el icono original del botón de ayuda.
	 */
	public static final ImageIcon IMAGEICONHELP = new ImageIcon(HelpUtils.class.getResource("/resources/images/help.png"));
 
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
	
	
	public static void visualize(String pagina){
		//getHelp().setDisplayed(true);
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
	 * Visualiza la ayuda en la pagina de "Introduccion"
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
	/**
	 * @param pagina
	 * @return
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
		
		//Foco para el modo alto contraste
		if (GeneralConfig.isHighContrast()) {
			botonAyuda.addFocusListener(new FocusListener() {
				public void focusLost(FocusEvent e) {
					//Se quita el borde del botón al perder el foco
					botonAyuda.setBorder(BorderFactory.createEmptyBorder());
				}
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
			
			
//			final StyleContext sc = new StyleContext();
//	        hightContrast = sc.addStyle("hightContrast", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
//	       	StyleConstants.setForeground(hightContrast, Color.WHITE);
//	        
//	        //StyleConstants.setBackground(this.linkUnfocusedStyle, new Color(0, 0, 0, 0));
//	        
//	        normalContrast = sc.addStyle("normalContrast", sc.getStyle(StyleContext.DEFAULT_STYLE)); //$NON-NLS-1$
//	        //StyleConstants.setBackground(this.linkFocusedStyle, Color.BLUE); //$NON-NLS-1$
//	        StyleConstants.setForeground(normalContrast, Color.BLACK); //$NON-NLS-1$
//	        
//	        ((HTMLDocument) editorPane.getDocument()).removeStyle("hightContrast");
//	        ((HTMLDocument) editorPane.getDocument()).setCharacterAttributes(
//                    0,
//                    150, 
//                    hightContrast, 
//                    false
//               );
//	        //editorPane.setBackground(Color.BLACK);
			
			
		}
	}

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
	private static void setHighContrastComponentTabbedPane(JTabbedPane tabbedPane, final JEditorPane editorPane, final boolean activate){
//		for (int i = 0; i<tabbedPane.getComponentCount();i++){
//			System.out.println(tabbedPane.getComponent(i).getClass().getName());
//			if (tabbedPane.getComponent(i) instanceof JHelpIndexNavigator){
//				JHelpIndexNavigator jhin = (JHelpIndexNavigator)tabbedPane.getComponent(i);
//				for (int j = 0; j < jhin.getComponentCount();j++){
//					System.out.println(".."+jhin.getComponent(j).getClass().getName());
//					if (jhin.getComponent(j) instanceof JPanel){
//						JPanel p = (JPanel)jhin.getComponent(j);
//						for (int k = 0;k<p.getComponentCount();k++){
//							System.out.println("...."+p.getComponent(k).getClass().getName());
//						}
//					} else if (jhin.getComponent(j) instanceof JScrollPane){
//						JScrollPane sp = (JScrollPane)jhin.getComponent(j);
//						for (int k = 0; k<sp.getComponentCount();k++){
//							System.out.println("...."+sp.getComponent(k).getClass().getName());
//							if (sp.getComponent(k) instanceof JViewport){
//								JViewport vp = (JViewport)sp.getComponent(k);
//								for (int l = 0;l<vp.getComponentCount();l++){
//									System.out.println("......" + vp.getComponent(l).getClass().getName());
//									if (vp.getComponent(l) instanceof JTree){
//										JTree t = (JTree)vp.getComponent(l);
////										if (activate){
////											t.setBackground(Color.black);
////										}
//										t.addMouseListener(new MouseListener() {
//											
//											@Override
//											public void mouseReleased(MouseEvent e) {
//												// TODO Auto-generated method stub
//												HelpUtils.setHighContrastEditorPane(editorPane, activate);
//											}
//											
//											@Override
//											public void mousePressed(MouseEvent e) {
//												// TODO Auto-generated method stub
//												
//											}
//											
//											@Override
//											public void mouseExited(MouseEvent e) {
//												// TODO Auto-generated method stub
//												
//											}
//											
//											@Override
//											public void mouseEntered(MouseEvent e) {
//												// TODO Auto-generated method stub
//												
//											}
//											
//											@Override
//											public void mouseClicked(MouseEvent e) {
//												// TODO Auto-generated method stub
//												
//											}
//										});
//										for (int m = 0; m<t.getComponentCount();m++){
//											System.out.println("........" + t.getComponent(m).getClass().getName());
//											if (t.getComponent(m) instanceof CellRendererPane){
//												CellRendererPane crp = (CellRendererPane)t.getComponent(m);
//												for (int n = 0;n<crp.getComponentCount();n++){
//													System.out.println(".........." + crp.getComponent(n).getClass().getName());
//													if (crp.getComponent(n) instanceof BasicIndexCellRenderer){
//														BasicIndexCellRenderer bicr = (BasicIndexCellRenderer)crp.getComponent(n);
//														System.out.println("............=" + ((JLabel)bicr).getText());
//														bicr.setBackground(Color.GREEN);
//														for (int o = 0; o<bicr.getComponentCount();o++){
//															
//															System.out.println("............" + bicr.getComponent(o).getClass().getName());
//														}
//													} else if (crp.getComponent(n) instanceof com.sun.java.swing.plaf.windows.WindowsTreeUI.WindowsTreeCellRenderer){
//														
//													}
//												}
//											}
//										}
//										
//									}
//								}
//							}
//						}
//					}
//				}
//			} else if (tabbedPane.getComponent(i) instanceof JHelpTOCNavigator){
//				
//			}
//		}
        if (tabbedPane != null) {
            for(Component componentTabbed : tabbedPane.getComponents()) {
                if (componentTabbed instanceof JHelpNavigator) {
                    for(Component panel : ((JHelpNavigator)componentTabbed).getComponents()) {
                        if (panel instanceof JScrollPane) {
                            for(Component component : ((JScrollPane) panel).getComponents()) {
                                 if(component instanceof JViewport) {
                                     for(Component component9 : ((JViewport) component).getComponents()) {
                                    	 if (component9 instanceof JTree) {
                                    		 if (activate){
                                    			 ((JTree)component9).setBackground(Color.black);
                                    		 } else { 
                                    			 ((JTree)component9).setBackground(Color.WHITE);
                                    		 }
                                             ((JTree)component9).addMouseListener(new MouseListener() {
     											
     											@Override
     											public void mouseReleased(MouseEvent e) {
     												// TODO Auto-generated method stub
     												HelpUtils.setHighContrastEditorPane(editorPane, activate);
     											}
     											
     											@Override
     											public void mousePressed(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											
     											@Override
     											public void mouseExited(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											
     											@Override
     											public void mouseEntered(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     											
     											@Override
     											public void mouseClicked(MouseEvent e) {
     												// TODO Auto-generated method stub
     												
     											}
     										});
                                            ((JTree)component9).addKeyListener(new KeyListener() {
												
												@Override
												public void keyTyped(KeyEvent e) {
													// TODO Auto-generated method stub
													
												}
												
												@Override
												public void keyReleased(KeyEvent e) {
													// TODO Auto-generated method stub
													HelpUtils.setHighContrastEditorPane(editorPane, activate);
												}
												
												@Override
												public void keyPressed(KeyEvent e) {
													// TODO Auto-generated method stub
													
												}
											});
//                                              for(Component cell : ((JTree) component9).getComponents()) {
//                                                 if (cell instanceof CellRendererPane) {
//                                                	 System.out
//															.println("qq");
//                                                     cell.setBackground(Color.BLACK);
//                                                     cell.setForeground(Color.WHITE);
//                                                 }
//                                              }
                                         }
//                                        if (component9 instanceof JTree) {
//                                        	
//                                        	if (activate){
//                                        		((JTree)component9).setBackground(Color.BLACK);
//                                        	} else {
//                                        		((JTree)component9).setBackground(Color.WHITE);
//                                        	}
//                                        	JTree arbol = (JTree) component9;
//                                        	arbolito(arbol);
                                        	
//                                             for(Component cell : ((JTree) component9).getComponents()) {
//                                            	 
//                                                if (cell instanceof CellRendererPane) {
//                                                    //cell.setBackground(Color.BLACK);
//                                                    //cell.setForeground(Color.WHITE);
//                                                    for (Component aaa : ((CellRendererPane) cell).getComponents()){
//                                                    	System.out
//																.println(aaa.getClass().getName());
////                                                    	if (aaa instanceof com.sun.java.swing.plaf.windows.WindowsTreeUI.WindowsTreeCellRenderer){
////                                                    		
////                                                    		//aaa.setForeground(Color.WHITE);
////                                                    		//((BasicIndexCellRenderer)aaa).setForeground(Color.WHITE);
////                                                    		
////                                                    	}
//                                                    }
//                                                }
//                                             }
                                            
//                                        }
                                     }   
                                 }
                                 
                             }
                         } if (panel instanceof JPanel){
                        	 for(Component component : ((JPanel) panel).getComponents()) {
                        		 if (component instanceof JTextField){
                        			 JTextField campo = (JTextField) component;
                        			 campo.addKeyListener(new KeyListener() {
										
										@Override
										public void keyTyped(KeyEvent e) {
											// TODO Auto-generated method stub
											
										}
										
										@Override
										public void keyReleased(KeyEvent e) {
											// TODO Auto-generated method stub
											if (e.getKeyCode()==KeyEvent.VK_ENTER){
												HelpUtils.setHighContrastEditorPane(editorPane, activate);
											}
										}
										
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
	
//	private static void arbolito(JTree a){
//		if(a.getCellRenderer() instanceof DefaultTreeCellRenderer){
//			aaa((DefaultTreeCellRenderer)a.getCellRenderer());
//		}
//	}
//	
//	private static void aaa(DefaultTreeCellRenderer d){
//		System.out.println(d.getComponentCount());
//		d.setForeground(Color.GREEN);
//	}
}


