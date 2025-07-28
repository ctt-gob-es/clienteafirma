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
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

/** Componente que define un di&aacute;logo de alerta.
 * @author Inteco. */
abstract class AbstractJAccessibilityCustomDialog extends JDialog {

	/** UID para la serializaci&oacute;n. */
	private static final long serialVersionUID = 1L;

	/** Posici&oacute;n X actual. */
	private static int actualPositionX = -1;

	/** Posici&oacute;n Y actual. */
	private static int actualPositionY = -1;

	/** Ancho actual. */
	private static int actualWidth = -1;

	/** Alto actual. */
	private static int actualHeight = -1;

	/** Indica si el di&aacute;logo requiere un tama&ntilde;o
	 * grande por defecto. */
	private boolean bigSizeDefault = false;

    /** Si se trata de un di&aacute;logo de confirmaci&oacute;n o tiene entradas */
    private transient final boolean isInputDialog;

	/** Constructor con par&aacute;metros.
	 * @param dialog Di&aacute;logo base.
	 * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
	 *              <code>false</code> en caso contrario.
	 * @param isInputDlg <code>true</code> si el di&aacute;logo es de entrada de datos,
	 *                   <code>false</code> en caso contrario. */
	AbstractJAccessibilityCustomDialog(final JDialog dialog, final boolean modal, final boolean isInputDlg){
		super(dialog, modal);
		this.isInputDialog = isInputDlg;
		final ResizingAdaptor adaptador = new ResizingAdaptor(this);
		addComponentListener(adaptador);
		setResizable(false);
	}

	/** Constructor con par&aacuute;metros.
	 * @param frame Componente base.
	 * @param modal <code>true</code> si el di&aacute;logo debe ser modal,
	 *              <code>false</code> en caso contrario.
	 * @param isInputDlg <code>true</code> si el di&aacute;logo es de entrada de datos,
	 *              <code>false</code> en caso contrario. */
	AbstractJAccessibilityCustomDialog(final JFrame frame, final boolean modal, final boolean isInputDlg){
		super(frame, modal);
		this.isInputDialog = isInputDlg;
		final ResizingAdaptor adaptador = new ResizingAdaptor(this);
		addComponentListener(adaptador);
		setResizable(false);
	}

	/** Constructor.
	 * @param isInputDlg Indica si el di&aacute;logo es de entrada de datos. */
	AbstractJAccessibilityCustomDialog(final boolean isInputDlg){
		this.isInputDialog = isInputDlg;
		final ResizingAdaptor adaptador = new ResizingAdaptor(this);
		addComponentListener(adaptador);
		setResizable(false);
	}

	/** Relaci&oacute;n m&iacute;nima que se aplica para la redimensi&oacute;n de los componentes.
	 * Cuanto menor es este n&uacute;mero menor es la redimensi&oacute;n aplicada.
	 * @return int Relaci&oacute;n m&iacute;nima */
	abstract int getMinimumRelation();

	/** Obtiene el componente horizontal de la posici&oacute;n en pantalla del di&aacute;logo.
	 * @return Componente horizontal de la posici&oacute;n del di&aacute;logo. */
	static int getActualPositionX() {
		return actualPositionX;
	}

	/** Establece el componente horizontal de la posici&oacute;n en pantalla del di&aacute;logo.
	 * @param positionX Componente horizontal de la posici&oacute;n del di&aacute;logo. */
	static void setActualPositionX(final int positionX) {
		AbstractJAccessibilityCustomDialog.actualPositionX = positionX;
	}

	/** Obtiene el componente vertical de la posici&oacute;n en pantalla del di&aacute;logo.
	 * @return Componente vertical de la posici&oacute;n del di&aacute;logo. */
	static int getActualPositionY() {
		return actualPositionY;
	}

	/** Establece el componente vertical de la posici&oacute;n en pantalla del di&aacute;logo.
	 * @param positionY Componente vertical de la posici&oacute;n del di&aacute;logo. */
	static void setActualPositionY(final int positionY) {
		AbstractJAccessibilityCustomDialog.actualPositionY = positionY;
	}

	/** Obtiene el ancho actual del di&aacute;logo.
     * @return Ancho actual del di&aacute;logo. */
	static int getActualWidth() {
		return actualWidth;
	}

	/** Establece el ancho del di&aacute;logo.
     * @param width Ancho del di&aacute;logo. */
	static void setActualWidth(final int width) {
		AbstractJAccessibilityCustomDialog.actualWidth = width;
	}

	/** Obtiene el alto actual del di&aacute;logo.
     * @return Alto actual del di&aacute;logo. */
	static int getActualHeight() {
		return actualHeight;
	}

	/** Establece el alto del di&aacute;logo.
     * @param height Alto del di&aacute;logo. */
	static void setActualHeight(final int height) {
		AbstractJAccessibilityCustomDialog.actualHeight = height;
	}

	/** Indica si el di&aacute;logo debe tener un tama&ntilde;o grande por defecto.
	 * @return <code>true</code> si el di&aacute;logo debe tener un tama&ntilde;o
	 *         grande por defecto, <code>false</code> en caso contrario. */
	boolean isBigSizeDefault() {
		return this.bigSizeDefault;
	}

	/** Indica si el di&aacute;logo debe tener un tama&ntilde;o grande por defecto.
	 * @param bigSizeByDefault <code>true</code> si el di&aacute;logo debe tener un
	 *                         tama&ntilde;o grande por defecto, <code>false</code>
	 *                         en caso contrario. */
	void setBigSizeDefault(final boolean bigSizeByDefault) {
		this.bigSizeDefault = bigSizeByDefault;
	}

	/** Devuelve el ancho inicial del di&aacute;logo.
     * @return Ancho inicial del di&aacute;logo. */
    int getInitialWidth(){
    	if(this.isInputDialog){
    		return AccesiblityConstants.CUSTOMDIALOG_INITIAL_WIDTH;
    	}
    	return AccesiblityConstants.CUSTOMCONFIRMATION_INITIAL_WIDTH;
    }

    /** Devuelve el alto inicial del di&aacute;logo.
     * @return Alto inicial del di&aacute;logo. */
    int getInitialHeight(){
    	if(this.isInputDialog){
    		return AccesiblityConstants.CUSTOMDIALOG_INITIAL_HEIGHT;
    	}
    	return AccesiblityConstants.CUSTOMCONFIRMATION_INITIAL_HEIGHT;
    }

    /** Devuelve el ancho m&aacute;ximo del di&aacute;logo.
     * @return Ancho m&aacute;ximo del di&aacute;logo. */
    int getMaxWidth(){
    	if(this.isInputDialog){
    		return AccesiblityConstants.CUSTOMDIALOG_MAX_WIDTH;
    	}
		return AccesiblityConstants.CUSTOMCONFIRMATION_MAX_WIDTH;
    }

    /** Devuelve el alto m&aacute;ximo del di&aacute;logo.
     * @return Alto m&aacute;ximo del di&aacute;logo. */
    int getMaxHeight(){
    	if(this.isInputDialog){
    		return AccesiblityConstants.CUSTOMDIALOG_MAX_HEIGHT;
    	}
    	return AccesiblityConstants.CUSTOMCONFIRMATION_MAX_HEIGHT;
    }


    /** Crea el panel de botones de accesibilidad.
     * @return Panel de botones de accesibilidad. */
    protected JPanel createAccessibilityButtonsPanel() {
    	final JPanel accessibilityButtonsPanel = new JPanel(new GridBagLayout());

        // Para el tooltip
        final JWindow tip = new JWindow();
        final JLabel tipText = new JLabel();

        // Panel que va a contener los botones de accesibilidad
        final JPanel panel = new JPanel(new GridBagLayout());

        // Restricciones para los botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.BOTH;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.weightx = 1.0;
        consButtons.weighty = 1.0;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Restore button
        final JPanel restorePanel = new JPanel();
        final ImageIcon imageIconRestore = new ImageIcon(InputPasswordSmartcardDialog.class.getResource("/images/restore.png")); //$NON-NLS-1$
        final JButton restoreButton = new JButton(imageIconRestore);
        restoreButton.setMnemonic(KeyEvent.VK_R);
        restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
        restoreButton.getAccessibleContext().setAccessibleName(restoreButton.getToolTipText());

        restoreButton.addFocusListener(new FocusListener() {

            /** Evento que se produce cuando el componente pierde el foco. */
            @Override
            public void focusLost(final FocusEvent e) {
                AccesibilityUtils.showToolTip(false, tip, restoreButton, tipText);
            }

            /** Evento que se produce cuando el componente tiene el foco. */
            @Override
            public void focusGained(final FocusEvent e) {
                AccesibilityUtils.showToolTip(true, tip, restoreButton, tipText);
            }
        });
        final Dimension dimension = new Dimension(20, 20);
        restoreButton.setPreferredSize(dimension);
        restoreButton.addKeyListener(
    		new KeyListener() {

				@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
				@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

				@Override
				public void keyPressed(final KeyEvent ke) {
					if (10 == ke.getKeyCode()) {
						restoreButton.doClick();
					}
				}
			}
		);
        restoreButton.setName("restaurar"); //$NON-NLS-1$
        restorePanel.add(restoreButton);

        AccesibilityUtils.remarcar(restoreButton);

        panel.add(restorePanel, consButtons);

        consButtons.gridx = 1;
        consButtons.insets = new Insets(0, 0, 0, 0); // right padding

        // Maximize button
        final JPanel maximizePanel = new JPanel();

        final ImageIcon imageIconMaximize = new ImageIcon(InputPasswordSmartcardDialog.class.getResource("/images/maximize.png")); //$NON-NLS-1$
        final JButton maximizeButton = new JButton(imageIconMaximize);
        maximizeButton.setMnemonic(KeyEvent.VK_M);
        maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
        maximizeButton.getAccessibleContext().setAccessibleName(maximizeButton.getToolTipText());
        maximizeButton.addKeyListener(
    		new KeyListener() {

				@Override public void keyTyped(final KeyEvent arg0) { /* No necesario */ }
				@Override public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

				@Override
				public void keyPressed(final KeyEvent ke) {
					if (10 == ke.getKeyCode()) {
						maximizeButton.doClick();
					}
				}
			}
		);

        maximizeButton.setName("maximizar"); //$NON-NLS-1$
        // Se asigna una dimension por defecto
        maximizeButton.setPreferredSize(dimension);

        AccesibilityUtils.remarcar(maximizeButton);
        maximizePanel.add(maximizeButton);

        maximizeButton.addFocusListener(new FocusListener() {
            /** Evento que se produce cuando el componente pierde el foco. */
            @Override
            public void focusLost(final FocusEvent e) {
                AccesibilityUtils.showToolTip(false, tip, maximizeButton, tipText);
            }

            /** Evento que se produce cuando el componente tiene el foco. */
            @Override
            public void focusGained(final FocusEvent e) {
                AccesibilityUtils.showToolTip(true, tip, maximizeButton, tipText);
            }
        });

        panel.add(maximizePanel, consButtons);

        // Se anade al panel general
        // Restricciones para el panel de botones
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.NONE;
        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.insets = new Insets(0, 0, 0, 0);
        c.anchor = GridBagConstraints.SOUTH;

        accessibilityButtonsPanel.add(panel, c);

        // Asignamos las acciones
        restoreButton.addActionListener(
    		new ActionListener() {
	            /** Acci&oacute;n del bot&oacute;n. */
	            @Override
	            public void actionPerformed(final ActionEvent e) {
	                restaurarActionPerformed(restoreButton, maximizeButton);
	            }
	        }
		);
        maximizeButton.addActionListener(
    		new ActionListener() {
	            /** Acci&oacute;n del bot&oacute;n. */
	            @Override
	            public void actionPerformed(final ActionEvent e) {
	                maximizarActionPerformed(restoreButton, maximizeButton);
	            }
	        }
		);

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        if (GeneralConfig.isMaximized()) {
            // Se deshabilita el boton de maximizado
            maximizeButton.setEnabled(false);
            // Se habilita el boton de restaurar
            restoreButton.setEnabled(true);
        }
        else {
            // Se habilita el boton de maximizado
            maximizeButton.setEnabled(true);
            // Se deshabilita el boton de restaurar
            restoreButton.setEnabled(false);
        }

        return accessibilityButtonsPanel;
    }


    /** Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla
     * menos el tama&ntilde;o de la barra de tareas de Windows. */
    void maximizarActionPerformed(final JButton restoreButton, final JButton maximizeButton) {
        setActualPositionX(getX());
        setActualPositionY(getY());
        setActualWidth(getWidth());
        setActualHeight(getHeight());

        // Se obtienen las dimensiones de maximizado
        final int maxWidth = getMaxWidth();
        final int maxHeight = getMaxHeight();

        // Se hace el resize
        this.setBounds(getInitialX(maxWidth), getInitialY(maxHeight), maxWidth, maxHeight);

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        maximizeButton.setEnabled(false);
        restoreButton.setEnabled(true);
    }

    /** Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado. */
    void restaurarActionPerformed(final JButton restoreButton, final JButton maximizeButton) {
        // Dimensiones de restaurado
        int minWidth = getInitialWidth();
        int minHeight = getInitialHeight();
        // Se comprueba las opciones de accesibilidad activas
        if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold() || isBigSizeDefault()) {
            minWidth = AccesiblityConstants.CUSTOMDIALOG_FONT_INITIAL_WIDTH;
            minHeight = AccesiblityConstants.CUSTOMDIALOG_FONT_INITIAL_HEIGHT;
        }
        // Se establece el tamano minimo
        setMinimumSize(new Dimension(minWidth, minHeight));

        // Se situa el dialogo
        if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1) {
            this.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
        }
        else {
            setBounds(getInitialX(minWidth), getInitialY(minHeight), minWidth, minHeight);
        }

        // Habilitado/Deshabilitado de botones restaurar/maximizar
        maximizeButton.setEnabled(true);
        restoreButton.setEnabled(false);
    }

    /** Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
     * @param width Ancho de la ventana.
     * @return int Posici&oacute;n X */
    private static int getInitialX(final int width) {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        return screenSize.width / 2 - width / 2;
    }

    /** Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
     * resoluci&oacute;n de pantalla.
     * @param height Alto de la ventana.
     * @return int Posici&oacute;n Y. */
    private static int getInitialY(final int height) {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        return screenSize.height / 2 - height / 2;
    }

    /** Acci&oacute;n por defecto de los botones accesibles. */
    protected static final class ButtonAbstractAction extends AbstractAction {
        /** UID. */
        private static final long serialVersionUID = 1L;

        /** Bot&oacute;n asociado a la acci&oacute;n. */
        private transient final JButton button;

        ButtonAbstractAction(final JButton actionButton) {
            this.button = actionButton;
        }

        /** Indica que la accion es la de pulsar el boton cancelar. */
        @Override
        public void actionPerformed(final ActionEvent event) {
        	if (this.button != null) {
        		this.button.doClick();
        	}
        }
    }
}
