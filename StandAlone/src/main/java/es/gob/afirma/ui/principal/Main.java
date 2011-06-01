/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.principal;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;

import es.gob.afirma.misc.Platform;

/**
 *	Entrada de la aplicacion
 */
public class Main {

	/** Versi&oacute;n de la interfaz gr&aacute;fica de escritorio. */
	public static final String VERSION = "1.3";
	
	static Logger logger = Logger.getLogger(Main.class.getName());	
	
    /**
     * Arranca la interfaz de escritorio del Cliente @firma.
     * @param args Par&aacute;metros de entrada.
     */
    public static void main(String[] args) {
        try {
        	if (Platform.getOS().equals(Platform.OS.LINUX))
        		UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        	else
        		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
        	logger.log(Level.SEVERE, null, ex);
        }
        
        new PrincipalGUI().main();
    }

}
