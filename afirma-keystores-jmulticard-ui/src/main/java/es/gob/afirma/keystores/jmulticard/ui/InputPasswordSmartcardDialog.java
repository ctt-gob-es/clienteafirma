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
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import es.gob.jmulticard.CancelledOperationException;

/** Componente di&aacute;logo que solicita una contrase&ntilde;a al usuario. */
public final class InputPasswordSmartcardDialog extends AbstractJAccessibilityCustomDialog implements ActionListener {

    /** UID. */
    private static final long serialVersionUID = 1L;

    private static final int PIN_MIN_LENGTH = 4;
    private static final int PIN_MAX_LENGTH = 16;

    /** Bot&oacute;n de aceptar. */
    private static JButton okButton = null;

    static JButton getOkButton() {
        return okButton;
    }

    /** Bot&oacute;n de cancelar. */
    private static JButton cancelButton = null;

    static JButton getCancelButton() {
        return cancelButton;
    }

    /** Texto para el bot&oacute;n */
    private static String cancellText = Messages.getString("PrincipalGUI.cancelar"); //$NON-NLS-1$

    /** Etiqueta con la informaci&ntilde;n de la alerta. */
    private transient InfoLabel infoLabel = null;

    /** Panel de botones. */
    private transient JPanel buttonsPanel = null;

    /** Panel principal. */
    private transient JPanel mainPanel = null;

    /** Campo de texto o campo de contrase&ntilde;a. */
    private transient JSecurePasswordLabel component = null;

    /** Indica si se muestra o no la casilla de "No volver a preguntar" (<i>cacheo</i> del PIN). */
    private transient JCheckBox useCacheCheckBox = null;

    /** Etiqueta que contiene el icono de la alerta. */
    private transient final IconLabel iconLabel = new IconLabel();

    /** Respuesta al mensaje */
    private transient int answer;

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el <i>cacheo</i> del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de <i>cacheo</i> de PIN. */
    private InputPasswordSmartcardDialog(final JDialog componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final String iconPath,
                         final boolean allowUseCache,
                         final boolean defaultUseCache) {
        super(componentParent, modal, true);
        initComponents(message, title, iconPath, allowUseCache, defaultUseCache);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el cacheo del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de cacheo de PIN. */
    private InputPasswordSmartcardDialog(final Component componentParent,
                         final boolean modal,
                         final String message,
                         final String title,
                         final String iconPath,
                         final boolean allowUseCache,
                         final boolean defaultUseCache) {
        super(true);
        setModal(modal);
        initComponents(message, title, iconPath, allowUseCache, defaultUseCache);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Constructor.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el cacheo del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de cacheo de PIN. */
    private InputPasswordSmartcardDialog(final JFrame componentParent,
                             final boolean modal,
                             final String message,
                             final String title,
                             final String iconPath,
                             final boolean allowUseCache,
                             final boolean defaultUseCache) {
        super(componentParent, modal, true);
        initComponents(message, title, iconPath, allowUseCache, defaultUseCache);
        setLocationRelativeTo(componentParent);
        pack();
    }

    /** Inicializa los componentes de la alerta.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el cacheo del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de cacheo de PIN. */
    protected void initComponents(final String message,
    		                      final String title,
    		                      final String iconPath,
    		                      final boolean allowUseCache,
    		                      final boolean defaultUseCache) {
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
        this.mainPanel = new JPanel(new GridBagLayout());

        // Restricciones para el panel de datos
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
        c.insets = new Insets(10, 5, 0, 10); // right padding
        c.weightx = 1.0;
        c.weighty = 1.0;

        // Icono del dialogo
        setIconLabel(iconPath);
        final JPanel iconPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints consIconPanel = new GridBagConstraints();
        consIconPanel.fill = GridBagConstraints.BOTH;

        iconPanel.add(this.iconLabel, consIconPanel);

        // Se crea una etiqueta sencilla
        this.infoLabel = new InfoLabel(message);
        this.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); // Se alinea a la izqda
        this.infoLabel.setVerticalAlignment(SwingConstants.CENTER); // Se alinea al centro el texto

        // Se anade la etiqueta al panel de informacion general
        this.mainPanel.add(this.infoLabel, c);


        // campo de password del dialogo
        this.component = new JSecurePasswordLabel(16);

        c.gridy++;
        c.insets = new Insets(2, 5, 2, 10); // right padding

        // Se anade el campo de texto al panel de informacion general
        this.mainPanel.add(this.component, c);

        // Si corresponde, se agrega la casilla de verificacion para el uso de la cache
        if (allowUseCache) {
        	this.useCacheCheckBox = new JCheckBox(Messages.getString("InputPasswordSmartcardDialog.useCache")); //$NON-NLS-1$
        	this.useCacheCheckBox.setSelected(defaultUseCache);

        	c.gridy++;
        	this.mainPanel.add(this.useCacheCheckBox, c);
        }

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
        cons.gridheight = 2;
        cons.weighty = 0.35;
        cons.weightx = 0.90;
        container.add(this.mainPanel, cons);

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
        cons.weighty = 0.65;
        cons.weightx = 0.0;
        container.add(this.buttonsPanel, cons);

        pack();
    }

    /** Asigna el icono a la etiqueta.
     * @param iconPath Ruta hacia el recurso de fichero de icono. */
    protected void setIconLabel(final String iconPath) {
    	if (iconPath != null) {
	        // Segun el tipo de mensaje se selecciona el icono
	        final ImageIcon icon = new ImageIcon(
	    		InputPasswordSmartcardDialog.class.getResource(
					iconPath
				)
			);
	        final Dimension dimensionInicial = new Dimension(100, (int)(100f / icon.getIconWidth() * icon.getIconHeight()));
	        this.iconLabel.setOriginalIcon(icon);
	        this.iconLabel.setOriginalDimension(dimensionInicial);
	        this.iconLabel.setIcon(
        		new ImageIcon(
    				icon.getImage().getScaledInstance(
						dimensionInicial.width,
						dimensionInicial.height,
						Image.SCALE_SMOOTH
					)
				)
    		);
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
        okButton.addKeyListener(
    		new KeyListener() {

				@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
				@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

				@Override
				public void keyPressed(final KeyEvent ke) {
					if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
						getOkButton().doClick();
					}
					else if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
						getCancelButton().doClick();
					}
				}
			}
		);

        okPanel.add(okButton);
        this.buttonsPanel.add(okPanel, consButtons);

        okButton.addActionListener(this);

    }

    /** Muestra un di&aacute;logo de solicitud de contrase&ntilde;a.
     * @param componentParent Componente padre para la modalidad
     * @param modal <code>true</code> si se desea que el di&aacute;logo sea modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje a mostrar.
     * @param mnemonic Atajo de teclado.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el icono del di&aacute;logo.
     * @return Contrase&ntilde;a introducida por el usuario.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el cacheo del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de cacheo de PIN. */
    public static PasswordResult showInputPasswordDialog(final Component componentParent,
                                                 final boolean modal,
                                                 final String message,
                                                 final int mnemonic,
                                                 final String title,
                                                 final String iconPath,
                                                 final boolean allowUseCache,
                                                 final boolean defaultUseCache) {
    	final InputPasswordSmartcardDialog inputPasswordDialog = InputPasswordSmartcardDialog.getInstanceInputPasswordDialog(
    			componentParent,
    			modal,
    			message,
    			title,
    			iconPath,
    			allowUseCache,
    			defaultUseCache
    	);

        okButton.setEnabled(false);
        inputPasswordDialog.getRootPane().setDefaultButton(null);

        // Configuramos el componente de insercion de contrasenas
        inputPasswordDialog.component.addKeyListener(
    		new KeyListener() {

	            @Override
	            public void keyTyped(final KeyEvent arg0) { /* Vacio */}

	            @Override
	            public void keyReleased(final KeyEvent ke) {
	            	final int length = inputPasswordDialog.getComponent().getPasswordLength();
	                //Control de los botones aceptar/cancelar
	                if (length >= PIN_MIN_LENGTH && length <= PIN_MAX_LENGTH) {
	                    getOkButton().setEnabled(true);
	                	if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
	    					getOkButton().doClick();
	    				}
	                }
	                else {
	                    getOkButton().setEnabled(false);
	                }
	            }

	            @Override
	            public void keyPressed(final KeyEvent ke) {
					if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
						getCancelButton().doClick();
					}
	            }
	        }
		);
        inputPasswordDialog.component.addAncestorListener(new RequestFocusListener());
        AccesibilityUtils.remarcar(inputPasswordDialog.component);
        AccesibilityUtils.setContrastColor(inputPasswordDialog.component);
        AccesibilityUtils.setFontBold(inputPasswordDialog.component);
        inputPasswordDialog.component.getAccessibleContext().setAccessibleName(
    		message.replace(AccesiblityConstants.HTML_SALTO_LINEA, "") + "  ALT + " + mnemonic + ". " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		);

        // Etiqueta principal
        // Se relaciona la etiqueta con el componente
        inputPasswordDialog.infoLabel.setLabelFor(inputPasswordDialog.component);
        // Se asigna un atajo
        inputPasswordDialog.infoLabel.setDisplayedMnemonic(mnemonic);
        // Se muestra el atajo
        final String text = AccesibilityUtils.remarkMnemonic(inputPasswordDialog.infoLabel.getText(), mnemonic);
        inputPasswordDialog.infoLabel.setText(text);

        // Configuracion de accesibilidad de la casilla de cacheo
        if (inputPasswordDialog.useCacheCheckBox != null) {
        	inputPasswordDialog.useCacheCheckBox.addKeyListener(
    			new KeyListener() {

	    			@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
	    			@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

	    			@Override
	    			public void keyPressed(final KeyEvent ke) {
	    				if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
	    					getOkButton().doClick();
	    				}
	    				else if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
	    					getCancelButton().doClick();
	    				}
	    			}
	    		}
			);

        	AccesibilityUtils.remarcar(inputPasswordDialog.useCacheCheckBox);
        	AccesibilityUtils.setContrastColor(inputPasswordDialog.useCacheCheckBox);
        	AccesibilityUtils.setFontBold(inputPasswordDialog.useCacheCheckBox);

            // Se muestra el atajo
        	inputPasswordDialog.useCacheCheckBox.setMnemonic('e');
            AccesibilityUtils.remarkMnemonic(inputPasswordDialog.useCacheCheckBox, 'e');
        }

        // Restricciones del panel de botones
        final GridBagConstraints cons = new GridBagConstraints();
        cons.insets = new Insets(0, 0, 0, 10); // right padding

        // Cancel button
        cancelButton = inputPasswordDialog.getButton(cancellText, KeyEvent.VK_C);
        final JPanel cancelPanel = new JPanel();
        cancelPanel.add(cancelButton);
        inputPasswordDialog.buttonsPanel.add(cancelPanel, cons);
        cancelButton.addActionListener(inputPasswordDialog);
        cancelButton.addKeyListener(
    		new KeyListener() {

				@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
				@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

				@Override
				public void keyPressed(final KeyEvent ke) {
					if (ke.getKeyCode() == KeyEvent.VK_ENTER || ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
						getCancelButton().doClick();
					}
				}
			}
		);

        inputPasswordDialog.infoLabel.setHorizontalAlignment(SwingConstants.LEFT); // Se centra el texto
        inputPasswordDialog.component.setVisible(true); // Se hace visible el campo de texto

        cancelButton.addActionListener(inputPasswordDialog);

        inputPasswordDialog.pack();
        inputPasswordDialog.setSize(inputPasswordDialog.getInitialWidth() + 1, inputPasswordDialog.getInitialHeight() + 1); // Hacemos un resize para forzar un repintado
        inputPasswordDialog.setVisible(true);

        // Control para saber si se ha pulsado el boton cancelar
        if (inputPasswordDialog.getAnswer() == JOptionPane.YES_OPTION) {
        	final char[] finalPin = inputPasswordDialog.component.getPassword();

        	// Por precaucion borramos el PIN y dejamos sus componentes relacionados
        	// listos para ser descartados
        	inputPasswordDialog.component.setText(""); //$NON-NLS-1$
        	inputPasswordDialog.component.setText(null);
        	inputPasswordDialog.component = null;
        	inputPasswordDialog.dispose();
        	System.gc();

        	// Recuperamos el valor del checkbox de usar cache si existe
        	boolean useCache = false;
        	if (inputPasswordDialog.useCacheCheckBox != null) {
        		useCache = inputPasswordDialog.useCacheCheckBox.isSelected();
        	}

            return new PasswordResult(finalPin, useCache);
        }
        throw new CancelledOperationException(
    		"La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
		);
    }

    /** Crea un bot&oacute;n.
     * Si el bot&oacute;n corresponde al de cancelar, se le asigna la tecla escape.
     * @param text Texto del bot&oacute;n.
     * @param mnemonic Atajo.
     * @return Bot&oacute;n creado. */
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
            getRootPane().getActionMap().put(cancelKey, new ButtonAbstractAction(getCancelButton()));
        }

        return button;
    }

    @Override
    public int getMinimumRelation() {
        return 7;
    }

    private int getAnswer() {
        return this.answer;
    }

    JSecurePasswordLabel getComponent() {
        return this.component;
    }

    /** Devuelve una instancia de <code>CustomDialog</code>.
     * @param componentParent Componente padre para la modalidad.
     * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
     *              <code>false</code> en caso contrario.
     * @param message Mensaje del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param iconPath Ruta hacia el recurso de fichero de icono.
     * @param allowUseCache Hace mostrarse la casilla para seleccionar el cacheo del PIN.
     * @param defaultUseCache Valor por defecto de la opci&oacute;n de cacheo de PIN.
     * @return Instancia del di&aacute;logo. */
    static InputPasswordSmartcardDialog getInstanceInputPasswordDialog(final Component componentParent,
                                                                       final boolean modal,
                                                                       final String message,
                                                                       final String title,
                                                                       final String iconPath,
                                                                       final boolean allowUseCache,
                                                                       final boolean defaultUseCache) {
    	// Se chequea el tipo del componente padre
        return new InputPasswordSmartcardDialog(
    		componentParent,
    		modal,
    		message,
    		title,
    		iconPath,
	        allowUseCache,
	        defaultUseCache
        );
    }

    /** Acci&oacute;n correspondiente a los botones de las alertas. */
    @Override
    public void actionPerformed(final ActionEvent e) {

        if (e.getSource().equals(okButton)) {
            this.answer = JOptionPane.YES_OPTION;
        }
        else {
            this.answer = JOptionPane.CANCEL_OPTION;
        }
        setVisible(false);
    }
}