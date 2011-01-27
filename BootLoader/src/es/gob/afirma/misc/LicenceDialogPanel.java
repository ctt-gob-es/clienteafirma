/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.misc;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Label;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.gob.afirma.install.Messages;

/**
 * Panel con el acuerdo de licencia del cliente de firma @firma versi&oacute;n 3.
 */
public class LicenceDialogPanel {

	/** Componente sobre el que se debe mostrar el acuerdo de licencia. */
	private Component parentComponent = null;
	
	/**
	 * Crea el di&aacute;logo y establece un componente padre sobre el que debe mostrarse.
	 * @param parentComponent Componente sobre el que se mostrar&aacute; el acuerdo de licencia.
	 */
	public LicenceDialogPanel(Component parentComponent) {
		this.parentComponent = parentComponent;
	}
	
	/**
	 * Muestra el texto del acuerdo de licencia.
	 * @return Devuelve <code>true</code> si se acepta el acuerdo de licencia, <code>false</code>
	 * en caso contrario.
	 */
	public boolean showDisclaimer() {

	    // Texto del dialogo
	    Label textLabel = new Label(Messages.getString("LicenceDialogPanel.0")); //$NON-NLS-1$

	    // Leemos el acuerdo de licencia desde un fichero
	    String licenseText;
	    try {
	        licenseText = new String(
	            AOBootUtil.getDataFromInputStream(
	                    this.getClass().getResourceAsStream("/resources/licenses.txt")), //$NON-NLS-1$
	            "UTF-8" //$NON-NLS-1$
	        );
	    } catch (Throwable e) {
	        licenseText = Messages.getString("LicenceDialogPanel.2"); //$NON-NLS-1$
	        Logger.getLogger("es.gob.afirma").warning("Ocurri\u00F3 un error al acceder a las condiciones de la licencia: "+e); //$NON-NLS-1$ //$NON-NLS-2$
	        e.printStackTrace();
	    }

	    // Texto del acuerdo de licencia
	    JTextArea textArea = new JTextArea(licenseText);
	    //textArea.setPreferredSize(new Dimension(150, 400));
	    textArea.setLineWrap(true);
	    textArea.setWrapStyleWord(true);
	    textArea.setEditable(false);
	    
	    // Cuadro con el scroll
	    JScrollPane scrollPane = new JScrollPane(textArea);
	    scrollPane.setPreferredSize(new Dimension(250, 400));
	    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	    // Restricciones del layout
	    GridBagConstraints c = new GridBagConstraints();
	    c.fill = GridBagConstraints.BOTH;

	    // Panel con el contenido del dialogo modal
	    Container licencePanel = new Container();
	    licencePanel.setLayout(new GridBagLayout());

	    c.gridx = 0;
	    c.gridy = 0;
	    c.weightx = 1.0;
	    c.weighty = 1.0;
	    licencePanel.add(textLabel, c);

	    c.gridy = 1;
	    c.weighty = 0.0;
	    licencePanel.add(scrollPane, c);

	    // Mostramos el dialogo e indicamos si se acepto
	    return JOptionPane.OK_OPTION == JOptionPane.showConfirmDialog(
	            this.parentComponent,
	            licencePanel,
	            Messages.getString("LicenceDialogPanel.1"), //$NON-NLS-1$
	            JOptionPane.OK_CANCEL_OPTION,
	            JOptionPane.PLAIN_MESSAGE);
	}
	
	public static void main(String[] args) {
		LicenceDialogPanel dialog = new LicenceDialogPanel(null);
		dialog.showDisclaimer();
	}
}
