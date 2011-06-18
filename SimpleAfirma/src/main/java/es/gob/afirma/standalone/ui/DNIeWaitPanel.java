/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.SimpleAfirma;

/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeWaitPanel extends JPanel {

    private static final long serialVersionUID = -8543615798397861866L;

    private final JButton noDNIButton = new JButton();
    private final JPanel noDNIPanel = new JPanel();

//    private final SimpleAfirma saf;

    private void createUI(final KeyListener kl, final ActionListener al) {
        this.setBackground(SimpleAfirma.WINDOW_COLOR);
        this.setLayout(new GridBagLayout());
        this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        // Boton para saltar de pantalla
        //this.noDNIPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        this.noDNIPanel.setLayout(new GridBagLayout());
        this.noDNIPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
        
        this.noDNIButton.setText(Messages.getString("DNIeWaitPanel.0")); //$NON-NLS-1$
        if (al != null) this.noDNIButton.addActionListener(al);
        this.noDNIButton.setMnemonic('n');
        this.noDNIButton.getAccessibleContext().setAccessibleDescription(Messages.getString("DNIeWaitPanel.1")); //$NON-NLS-1$
        this.noDNIButton.getAccessibleContext().setAccessibleName(Messages.getString("DNIeWaitPanel.2")); //$NON-NLS-1$
        if (kl != null) this.noDNIButton.addKeyListener(kl);
        this.noDNIButton.requestFocus();
        this.noDNIPanel.add(this.noDNIButton);

        // Texto informativo
        ResizingTextPanel textPanel = new ResizingTextPanel(Messages.getString("DNIeWaitPanel.3")); //$NON-NLS-1$
                
        textPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
        textPanel.setFocusable(false);

        // Imagen central
        final JSVGCanvas vectorDNIeHelpPicture = new JSVGCanvas();
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            vectorDNIeHelpPicture.setDocument(dbf.newDocumentBuilder().parse(this.getClass().getResourceAsStream("/resources/lectordnie.svg") //$NON-NLS-1$
                                                 ));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            "No se ha podido cargar la imagen explicativa de insercion de DNIe, esta no se mostrara: " + e //$NON-NLS-1$
            );
        }
        vectorDNIeHelpPicture.setFocusable(false);

        final GridBagConstraints c = new GridBagConstraints();
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
        c.weightx = 1.0;
        c.insets = new Insets(0, 0, 0, 0);
        c.gridy = 2;
        c.ipady = 0;
        this.add(this.noDNIPanel, c);

//        // Listado de idiomas disponibles
//        final Locale[] locales = SimpleAfirma.getAvailableLocales();
//        if (locales != null && locales.length > 1) {
//            final JComboBox languagesList = new JComboBox(locales);
//            languagesList.setRenderer(new LocaleCellRenderer());
//            languagesList.setSelectedItem(Locale.getDefault());
//            languagesList.addItemListener(new ItemListener() {
//                @Override
//                public void itemStateChanged(ItemEvent e) {
//                    if (e.getStateChange() == ItemEvent.SELECTED) {
//                        try {
//                            DNIeWaitPanel.this.saf.setDefaultLocale((Locale) e.getItem());
//                        }
//                        catch (final Exception ex) {
//                            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
//                               "No se ha podido cambiar el idioma de la interfaz: " + ex //$NON-NLS-1$
//                            );
//                            return;
//                        }
//                    }
//                }
//            });
//            if (kl != null) languagesList.addKeyListener(kl);
//            c.fill = GridBagConstraints.NONE;
//            c.gridx = 1;
//            c.gridy = 0;
//            c.anchor = GridBagConstraints.EAST;
//            this.noDNIPanel.add(languagesList, c);
//        }

    }

    /** Construye un panel de espera a insercci&oacute;n de DNIe.
     * @param kl KeyListener para la detecci&oacute;n de la tecla ESC para el
     *        cierre del aplicativo y F1 para mostrar la ayuda
     * @param al ActionListener para el control de los botones
     * @param safirma SimpleAfirma para establecer el <code>Locale</code> seleccionado en el men&uacute; desplegable */
    public DNIeWaitPanel(final KeyListener kl, final ActionListener al, final SimpleAfirma safirma) {
        super(true);
//        this.saf = safirma;
        createUI(kl, al);
    }

//    private static final class LocaleCellRenderer extends DefaultListCellRenderer {
//
//        private static final long serialVersionUID = -6516072256082631760L;
//
//        @Override
//        public Component getListCellRendererComponent(final JList list,
//                                                      final Object value,
//                                                      final int index,
//                                                      final boolean isSelected,
//                                                      final boolean cellHasFocus) {
//            String language = null;
//            if (value instanceof Locale) {
//                final Locale locale = (Locale) value;
//                language = locale.getDisplayName(locale).substring(0, 1).toUpperCase() + locale.getDisplayName(locale).substring(1);
//            }
//            return super.getListCellRendererComponent(list, language != null ? language : value, index, isSelected, cellHasFocus);
//        }
//    }
}
