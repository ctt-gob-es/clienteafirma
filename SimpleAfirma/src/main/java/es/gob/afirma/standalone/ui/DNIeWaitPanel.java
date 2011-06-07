package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.util.Locale;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;
import org.w3c.dom.Document;

import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.SimpleAfirma;

/**
 * Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class DNIeWaitPanel extends JPanel implements ItemListener {

	private static final long serialVersionUID = -8543615798397861866L;
	
	JButton noDNIButton = null;
	JPanel noDNIPanel = null;
	
	private void createUI(final KeyListener kl, final ActionListener al) {
		this.setBackground(SimpleAfirma.WINDOW_COLOR);
		this.setLayout(new GridBagLayout());
		this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
		
		// Boton para saltar de pantalla
		noDNIPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		noDNIPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
		noDNIButton = new JButton("No deseo usar DNIe");
		//noDNIButton = new JButton(Messages.getString("SimpleAFirma.1"));
		if (al != null) noDNIButton.addActionListener(al);
		noDNIButton.setMnemonic('n');
		noDNIButton.getAccessibleContext().setAccessibleDescription(
			"Pulse este bot\u00F3n para omitir la espera de inserci\u00F3n de DNIe y permitir seleccionar un certificado cualquiera desde el almac\u00E9n de claves y certificados del sistema operativo"
		);
		noDNIButton.getAccessibleContext().setAccessibleName(
			"Bot\u00F3n de omisi\u00F3n de espera para inserci\u00F3n de DNIe"
		);
		if (kl != null) noDNIButton.addKeyListener(kl);
		noDNIButton.requestFocus();
		noDNIPanel.add(noDNIButton);
		
		// Texto informativo
		ResizingTextPanel textPanel = new ResizingTextPanel(
		        Messages.getString("SimpleAFirma.9")
		);
		textPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
		textPanel.setFocusable(false);
        
		// Imagen central
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		Document docum = null;
		try {
			docum = dbf.newDocumentBuilder().parse(
				this.getClass().getResourceAsStream("/resources/lectordnie.svg")
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
				"No se ha podido cargar la imagen explicativa de insercion de DNIe, esta no se mostrara: " + e
			);
			return;
		}
		final JSVGCanvas vectorDNIeHelpPicture = new JSVGCanvas();
		vectorDNIeHelpPicture.setDocument(docum);
		vectorDNIeHelpPicture.setFocusable(false);
		
		// Listado de idiomas disponibles
		JComboBox languagesList = null;
		Locale[] locales = SimpleAfirma.getAvailableLocales();
		if (locales != null && locales.length > 1) {
		    languagesList = new JComboBox(locales);
		    languagesList.setRenderer(new LocaleCellRenderer());
		    languagesList.setSelectedItem(Locale.getDefault());
		    languagesList.addItemListener(this);
		    if (kl != null) languagesList.addKeyListener(kl);
		}
		
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		this.add(vectorDNIeHelpPicture, c);
		c.weighty = 0.0;
		c.insets = new Insets(10, 0, 5, 0);
		c.gridy = 1;
		c.ipady = 60;
        this.add(textPanel, c);
        c.fill = GridBagConstraints.VERTICAL;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.insets = new Insets(0, 0, 0, 0);
        c.gridy = 2;
        c.ipady = 0;
        c.anchor = GridBagConstraints.WEST;
        this.add(noDNIPanel, c);
        if (locales.length > 1) {
            c.gridx = 1;
            c.anchor = GridBagConstraints.EAST;
            this.add(languagesList, c);
        }
	}
	
	/**
	 * Construye un panel de espera a insercci&oacute;n de DNIe.
	 * @param kl KeyListener para la detecci&oacute;n de la tecla ESC para el
	 *           cierre del aplicativo y F1 para mostrar la ayuda
	 * @param al ActionListener para el control de los botones
	 */
	public DNIeWaitPanel(final KeyListener kl, final ActionListener al) {
		super(true);
		createUI(kl, al);
	}

	public void itemStateChanged(ItemEvent e) {
	    if (e.getStateChange() == ItemEvent.SELECTED) {
	        try {
	            SimpleAfirma.setDefaultLocale((Locale) e.getItem());
	        } catch (Exception ex) {
	            Logger.getLogger("es.gob.afirma").warning(
	                    "No se ha podido cambiar el idioma de la interfaz: " + ex
	            );
	            return;
	        }
	    }
	}
	
	private class LocaleCellRenderer extends DefaultListCellRenderer {
        private static final long serialVersionUID = 1L;

        @Override
	    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
	        String language = null;
	        if (value instanceof Locale) {
	            Locale locale = (Locale) value;
	            language = locale.getDisplayName(locale).substring(0, 1).toUpperCase() + 
	                       locale.getDisplayName(locale).substring(1);
	        }
	        return super.getListCellRendererComponent(list, language != null ? language : value, index, isSelected, cellHasFocus);
	    }
	}
}

