/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTiA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

/** Componente di&aacute;logo que solicita confirmaci&oacute;n al usuario. */
public final class ConfirmSmartcardDialog extends AbstractJAccessibilityCustomDialog implements ActionListener {

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Etiqueta con la informaci&ntilde;n de la alerta. */
    private transient InfoLabel infoLabel = null;

    /** Panel de botones. */
    private transient JPanel buttonsPanel = null;

    /** Etiqueta que contiene el icono de la alerta. */
    private transient final IconLabel iconLabel = new IconLabel();

    /** Bot&oacute;n de <b>Aceptar</b>. */
    private static JButton okButton = null;

    static JButton getOkButton() {
        return okButton;
    }

    /** Bot&oacute;n de <b>No</b>. */
    private transient JButton noButton = null;

    JButton getNoButton() {
    	return this.noButton;
    }

    /** Bot&oacute;n de <b>Cancelar</b>. */
    private static JButton cancelButton = null;

    static JButton getCancelButton() {
        return cancelButton;
    }

    /** Respuesta al mensaje. */
    private transient int answer;

    /** Texto para el bot&oacute;n. */
    private static String cancellText = Messages.getString("PrincipalGUI.cancelar"); //$NON-NLS-1$

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    protected ConfirmSmartcardDialog(final JDialog componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final String iconPath) {
        super(componentParent, modal, false);
        initComponents(message, title, iconPath);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    protected ConfirmSmartcardDialog(final Component componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final String iconPath) {
        super(false);
        setModal(modal);
        initComponents(message, title, iconPath);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    private ConfirmSmartcardDialog(final JFrame componentParent,
                             final boolean modal,
                             final String message,
                             final String title,
                             final String iconPath) {
        super(componentParent, modal, false);
        initComponents(message, title, iconPath);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Inicializa los componentes de la alerta.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    protected void initComponents(final String message,
    		                      final String title,
    		                      final String iconPath) {
        // Se obtienen las dimensiones de maximizado
        final int maxWidth = getMaxWidth();
        final int maxHeight = getMaxHeight();

        // Se establece el tamano minimo
        setMinimumSize(new Dimension(getInitialWidth(), getInitialHeight()));
        setPreferredSize(new Dimension(getInitialWidth(), getInitialHeight()));
        // Se establece el tamano maximo
        setMaximumSize(new Dimension(maxWidth, maxHeight));

        // Dimensiones de la ventana en Windows y Linux
        if (GeneralConfig.isMaximized()) {
            // Se maximiza
            this.setBounds(0, 0, maxWidth, maxHeight);
        }
        else if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()) { // Se establece el tamano minimo en base a las opciones activas
		    setMinimumSize(new Dimension(AccesiblityConstants.CUSTOMDIALOG_FONT_INITIAL_WIDTH, AccesiblityConstants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT));
		}

        setTitle(title);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        this.answer = JOptionPane.NO_OPTION;
        // Contenedor del dialogo
        final Container container = getContentPane();
        // Layout del contenedor
        container.setLayout(new GridBagLayout());

        // Panel con los datos del dialogo
        final JPanel mainPanel = new JPanel(new GridBagLayout());

        // Restricciones para el panel de datos
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
        c.insets = new Insets(5, 10, 0, 10);

        // Icono del dialogo
        setIconLabel(iconPath);
        final JPanel iconPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints consIconPanel = new GridBagConstraints();
        consIconPanel.fill = GridBagConstraints.BOTH;

        iconPanel.add(this.iconLabel, consIconPanel);

        c.insets = new Insets(10, 5, 0, 10); // right padding
        c.gridx++;
        c.weightx = 1.0;
        c.weighty = 1.0;

        // Se crea una etiqueta focusable
        this.infoLabel = new InfoLabel(message, false);
        this.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        // Foco a la etiqueta
        this.infoLabel.addAncestorListener(new RequestFocusListener());
        this.infoLabel.setVerticalAlignment(SwingConstants.CENTER); // Se alinea al centro el texto

        // Se anade la etiqueta al panel de informacion general
        mainPanel.add(this.infoLabel, c);

        // Panel de botones
        createMainButtonsPanel();
        final JPanel accessibilityButtonsPanel = createAccessibilityButtonsPanel();

        // Restricciones del contenedor general

        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.BOTH;

        //Se anade el panel de botones relacionados con la accesibilidad
        cons.gridx = 0;
        cons.gridy = 0;
        cons.gridwidth = 2;
        cons.gridheight = 1;
        cons.weighty = 0.0;
        cons.weightx = 0.10;
        container.add(accessibilityButtonsPanel, cons);

        //se anade el panel de informacion
        cons.gridx = 2;
        cons.gridy = 0;
        cons.gridwidth = 6;
        cons.gridheight = 3;
        cons.weighty = 0.65;
        cons.weightx = 0.90;
        container.add(mainPanel, cons);

        //Se anade el icono
        cons.gridx = 0;
        cons.gridy = 1;
        cons.gridwidth = 2;
        cons.gridheight = 3;
        cons.weighty = 0.90;
        cons.weightx = 0.0;
        container.add(iconPanel, cons);

        //Se anade el panel de botones
        cons.gridx = 2;
        cons.gridy = 3;
        cons.gridwidth = 6;
        cons.gridheight = 1;
        cons.weighty = 0.35;
        cons.weightx = 0.0;
        container.add(this.buttonsPanel, cons);

        pack();
    }

    /** Asigna el icono a la etiqueta.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    protected void setIconLabel(final String iconPath) {
    	if (iconPath != null) {
	        // Segun el tipo de mensaje se selecciona el icono
	        ImageIcon icon = new ImageIcon(
	    		ConfirmSmartcardDialog.class.getResource(
					iconPath
				)
			);
	        final Dimension dimensionInicial = new Dimension(100, (int)(100f / icon.getIconWidth() * icon.getIconHeight()));
	        this.iconLabel.setOriginalIcon(icon);
	        this.iconLabel.setOriginalDimension(dimensionInicial);
	        icon = new ImageIcon(icon.getImage().getScaledInstance(dimensionInicial.width, dimensionInicial.height, Image.SCALE_SMOOTH));
	        this.iconLabel.setIcon(icon);
    	}
    }

    /** Panel que contiene los botones principales de las alerta. */
    void createMainButtonsPanel() {
        this.buttonsPanel = new JPanel(new GridBagLayout());

        // Restricciones para el panel de botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.NONE;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.insets = new Insets(0, 10, 0, 10); // right padding
        consButtons.anchor = GridBagConstraints.CENTER;

        // OK button
        final JPanel okPanel = new JPanel();
        okButton = getButton(Messages.getString("PrincipalGUI.aceptar"), KeyEvent.VK_A); //$NON-NLS-1$
        okButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
					getOkButton().doClick();
				}
			}
		});

        okPanel.add(okButton);
        this.buttonsPanel.add(okPanel, consButtons);

        okButton.addActionListener(this);

    }

    /** Muestra un di&aacute;logo con un mensaje que pide la interaci&oacute;n del usuario.
     * @param componentParent Componente padre.
     * @param modal Modal.
     * @param message Mensaje a mostrar.
     * @param title T&iacute;o del di&aacute;logo.
     * @param typeOption Opciones de interaci&oacute;n.
     * @param iconPath Ruta hacia el icono del di&aacute;logo.
     * @return Respuesta del usuario. */
    public static int showConfirmDialog(final Component componentParent,
                                        final boolean modal,
                                        final String message,
                                        final String title,
                                        final int typeOption,
                                        final String iconPath) {

        final ConfirmSmartcardDialog customDialog = ConfirmSmartcardDialog.getInstanceCustomDialog(
    		componentParent,
    		modal,
    		message,
    		title,
    		iconPath
		);
        okButton.setEnabled(true);
        customDialog.getRootPane().setDefaultButton(null);

        // Restricciones
        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.gridy = 0;
        cons.insets = new Insets(0, 0, 0, 10); // right padding

        // Se comprueba el tipo de dialogo
        if (typeOption != JOptionPane.YES_NO_OPTION) {
            throw new UnsupportedOperationException("Solo se soportan dialogos de tipo Si/No"); //$NON-NLS-1$
        }

		// Boton Si
		okButton.setText(Messages.getString("CustomDialog.confirmDialog.yes")); //$NON-NLS-1$
		okButton.setMnemonic(KeyEvent.VK_S);
		// Boton no
		customDialog.noButton = customDialog.getButton(Messages.getString("CustomDialog.confirmDialog.no"), KeyEvent.VK_N); //$NON-NLS-1$
		customDialog.noButton.addKeyListener(new KeyListener() {

			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

			@Override
			public void keyPressed(final KeyEvent ke) {
				if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
					customDialog.getNoButton().doClick();
				}
			}
		});
		final JPanel noPanel = new JPanel();
		noPanel.add(customDialog.noButton);
		customDialog.buttonsPanel.add(noPanel, cons);
		customDialog.noButton.addActionListener(customDialog);

        customDialog.infoLabel.setHorizontalAlignment(SwingConstants.CENTER); // Se centra el texto
        customDialog.pack();
        customDialog.setSize(customDialog.getInitialWidth() + 1, customDialog.getInitialHeight() + 1); // Hacemos un resize para forzar un repintado
        customDialog.setVisible(true);

        return customDialog.getAnswer();
    }

    /** Crea un boton.
     * Si el bot&oacute;n corresponde al de cancelar, se le asigna la tecla <i>esc</i>.
     * @param text Texto del bot&oacute;n.
     * @param mnemonic Atajo.
     * @return Bot&oacute;n. */
    private JButton getButton(final String text, final int mnemonic) {
        final JButton button = new JButton(text);
        button.setMnemonic(mnemonic);
        AccesibilityUtils.remarcar(button);
        AccesibilityUtils.setContrastColor(button);
        AccesibilityUtils.setFontBold(button);
        // Se comprueba si el boton es el de cancelar
        if (text.equalsIgnoreCase(cancellText)) {
            // Se asigna la tecla escape a dicho boton
            final String cancelKey = "cancel"; //$NON-NLS-1$
            getRootPane().getInputMap(
        		JComponent.WHEN_IN_FOCUSED_WINDOW
    		).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
				cancelKey
			);
            getRootPane().getActionMap().put(
        		cancelKey,
        		new ButtonAbstractAction(getCancelButton())
    		);
        }

        return button;
    }

    @Override
    public int getMinimumRelation() {
        return 7;
    }

    /** @return the answer */
    private int getAnswer() {
        return this.answer;
    }

    /** Devuelve una instancia de <code>CustomDialog</code>.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @return Instancia del di&aacute;logo. */
    static ConfirmSmartcardDialog getInstanceCustomDialog(final Component componentParent,
                                                          final boolean modal,
                                                          final String message,
                                                          final String title,
                                                          final String iconPath) {
    	// Se chequea el tipo del componente padre
    	final ConfirmSmartcardDialog customDialog;
        if (componentParent instanceof JDialog) {
            customDialog = new ConfirmSmartcardDialog(
        		(JDialog) componentParent,
        		modal,
        		message,
        		title,
        		iconPath
    		);
        }
        else if (componentParent instanceof JFrame) {
            customDialog = new ConfirmSmartcardDialog(
        		(JFrame) componentParent,
        		modal,
        		message,
        		title,
        		iconPath
    		);
        }
        else {
            customDialog = new ConfirmSmartcardDialog(componentParent, modal, message, title, iconPath);
        }
        return customDialog;
    }

    /** Acci&oacute;n correspondiente a los botones de las alertas. */
    @Override
    public void actionPerformed(final ActionEvent e) {

        if (e.getSource().equals(ConfirmSmartcardDialog.okButton)) {
            this.answer = JOptionPane.YES_OPTION;
        }
        else if (e.getSource().equals(this.noButton)) {
            this.answer = JOptionPane.NO_OPTION;
        }
        else {
            this.answer = JOptionPane.CANCEL_OPTION;
        }
        setVisible(false);
    }
}