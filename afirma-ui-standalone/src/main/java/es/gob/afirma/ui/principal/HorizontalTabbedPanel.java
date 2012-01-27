package es.gob.afirma.ui.principal;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Enumeration;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityFrame;

/** Panel que contiene un listado de botones seleccionables y un espacio para la
 * visualizaci&oacute;n de componentes. Cuando el usuario selecciona un bot&oacute;n
 * se retira el contenido del espacio de visualizaci&oacute;n y se agrega el contenido
 * asociado a ese bot&oacute;n. */
final class HorizontalTabbedPanel extends JPanel {
    /** Listener que detecta las pulsaciones en los botones principales y muestra
     * los paneles asociados. */
    private final class SelectButtonListener implements ItemListener {

        private final Container contentPanel;

        private final GridBagConstraints layoutConstraints = new GridBagConstraints();

        private final Container mainContainer;

        public SelectButtonListener(final Container contentPanel, final Container mainContainer) {
            this.contentPanel = contentPanel;
            this.mainContainer = mainContainer;

            this.layoutConstraints.fill = GridBagConstraints.BOTH;
            this.layoutConstraints.weightx = 1.0;
            this.layoutConstraints.weighty = 1.0;
        }

        @Override
        public void itemStateChanged(final ItemEvent ev) {
            if (ev.getStateChange() == ItemEvent.SELECTED) {
                this.mainContainer.removeAll();

                SelectButtonListener.this.mainContainer.add(SelectButtonListener.this.contentPanel, SelectButtonListener.this.layoutConstraints);

                SelectButtonListener.this.mainContainer.repaint(0,
                                                                0,
                                                                SelectButtonListener.this.mainContainer.getWidth(),
                                                                SelectButtonListener.this.mainContainer.getHeight());

                SelectButtonListener.this.mainContainer.validate();

                JAccessibilityFrame.getJAccessibilityFrame(this.mainContainer).callResize();
            }
        }

    }
    private static final long serialVersionUID = 2340734316078849777L;
    /** Grupo de botones que habilitan los distintos paneles. */

    private final ButtonGroup buttonGroup;

    /** Panel para la visualizaci&oacute;n de contenido. */
    private final JPanel contentsPanel;

    /** Indica si todav&iacute;a no se ha insertado ning&uacute;n bot&oacute;n en la lista. */
    private boolean firstButton = true;

    /** UID */
    private int numBotones = 0;

    /** Panel en donde mostrar el listado de botones. */
    private final JPanel toggledButtonsPanel;

    /** Creamos el panel y su distribuci&oacute;n interna. */
    public HorizontalTabbedPanel() {
        super();

        this.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;

        this.buttonGroup = new ButtonGroup();

        this.toggledButtonsPanel = new JPanel();

        final GridBagLayout buttonsPanelLayout = new GridBagLayout();

        c.weightx = 0.15;
        this.toggledButtonsPanel.setLayout(buttonsPanelLayout);
        this.add(this.toggledButtonsPanel, c);

        c.gridx = 1;
        c.weightx = 0.05;
        final JPanel gapPanel = new JPanel();
        gapPanel.setPreferredSize(new Dimension(20, 1));
        this.add(gapPanel, c);

        c.gridx = 2;
        c.weightx = 1.0;

        this.contentsPanel = new JPanel();
        this.contentsPanel.setLayout(new GridBagLayout());
        this.add(this.contentsPanel, c);
    }

    /** Agrega una pareja bot&oacute;n-panel al cuadro.
     * @param button Bot&oacute;n que debe insertarse.
     * @param panel Panel que debe mostrarse al pulsar el bot&oacute;n. */
    public void addTab(final JToggleButton button, final JPanel panel) {
        if (this.firstButton) {
            this.numBotones = 0;
        }
        else {
            this.numBotones++;
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = this.numBotones * 2;
        c.weightx = 1.0;
        c.weighty = 1.0;
        final JPanel buttonPanel = new JPanel(new GridLayout(1, 1));
        buttonPanel.add(button);
        this.toggledButtonsPanel.add(buttonPanel, c);
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.weightx = 1.0;
        c.gridy = (this.numBotones * 2) + 1;
        this.toggledButtonsPanel.add(new JPanel(), c);

        button.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    button.doClick();
                }
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                // No implementado
            }

            @Override
            public void keyTyped(final KeyEvent e) {
                // No implementado
            }
        });
        this.buttonGroup.add(button);

        // panel.setBackground(Color.BLUE);
        // panel.setBounds(5, 5, 30, 30);

        button.addItemListener(new SelectButtonListener(panel, this.contentsPanel));
        button.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                HelpUtils.visualize(button.getName());
                Main.setHelpIndex(button.getName());
            }
        });
        if (this.firstButton) {
            button.doClick();
            this.firstButton = false;
            this.numBotones = 0;
        }
    }

    /** Borra todo el contenido del componente. */
    public void reset() {
        this.buttonGroup.clearSelection();
        final Enumeration<AbstractButton> buttons = this.buttonGroup.getElements();
        while (buttons.hasMoreElements()) {
            this.buttonGroup.remove(buttons.nextElement());
        }
        this.toggledButtonsPanel.removeAll();
        this.contentsPanel.removeAll();
        this.firstButton = true;
    }

}
