package es.gob.afirma.ui.wizardutils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Botonera superior. */
public final class BotoneraSuperior extends JPanel {
	/**
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Dimensiones.
	 */
	private Dimension dimensiones = new Dimension(603, 70);
	/**
	 * Lista de ventanas del wizard.
	 */
	private List<JDialogWizard> ventanas;

	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	JButton getRestoreButton() {
		return this.restoreButton;
	}

	/**
	 * Boton de maximizar.
	 */
	private JButton maximizeButton = null;
	JButton getMaximizeButton() {
		return this.maximizeButton;
	}

	/**
	 * Devuelve la lista de ventanas del wizard.
	 * @return lista de ventanas del wizard.
	 */
	public List<JDialogWizard> getVentanas() {
		return this.ventanas;
	}

	/**
	 * Genera una botonera con la configuracion predefinida
	 * @param ventanas	Listado que contiene todas las ventanas en orden de aparicion
	 */
	public BotoneraSuperior(final List<JDialogWizard> ventanas) {
		this.ventanas = ventanas;
		initParamenters();
	}

	/**
	 * Genera una botonera con unas dimensiones dadas
	 * @param dimensiones	Dimensiones de la botonera
	 */
	public BotoneraSuperior(final Dimension dimensiones) {
		this.dimensiones = dimensiones;
		initParamenters();
	}

	/**
	 * Inicializacion de parametros
	 */
	private void initParamenters() {
		// Configuracion del panel
    	setBorder(BorderFactory.createEtchedBorder());
    	setPreferredSize(this.dimensiones);
        setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
        setBackground(Color.WHITE);
        if (Main.isOSHighContrast()){
        	setOpaque(false);
        }
        setBorder(null);

        createAccessibilityButtonsPanel();
	}

	/**
	 * Cambia el tamano de la ventana al tamano maximo de pantalla menos el tamano de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);

		JAccessibilityDialogWizard.setActualPositionX(j.getX());
		JAccessibilityDialogWizard.setActualPositionY(j.getY());
		JAccessibilityDialogWizard.setActualWidth(j.getWidth());
		JAccessibilityDialogWizard.setActualHeight(j.getHeight());

		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = (int)rect.getWidth();
		final int maxHeight = (int)rect.getHeight();

		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			j.setBounds(0,0, maxWidth, maxHeight);
		} else {
			j.setBounds(0,0, maxWidth, maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
		}

		//Se deshabilita el boton de maximizar puesto que se ha pulsado.
		this.maximizeButton.setEnabled(false);
		this.restoreButton.setEnabled(true);
	}

	/**
	 * Restaura el tamano de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
		if (JAccessibilityDialogWizard.getActualPositionX() != -1 && JAccessibilityDialogWizard.getActualPositionY() != -1 && JAccessibilityDialogWizard.getActualWidth() != -1 && JAccessibilityDialogWizard.getActualHeight() != -1){
			j.setBounds(JAccessibilityDialogWizard.getActualPositionX(), JAccessibilityDialogWizard.getActualPositionY(), JAccessibilityDialogWizard.getActualWidth(), JAccessibilityDialogWizard.getActualHeight());
		}
		else {
			final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			if (Platform.getOS().equals(Platform.OS.LINUX)){
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			}
			else{
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
			}
    		j.setMinimumSize(new Dimension(j.getSize().width, j.getSize().height));
		}
		//Se deshabilita el boton de restaurar puesto que se ha pulsado.
		this.maximizeButton.setEnabled(true);
		this.restoreButton.setEnabled(false);
	}

	/**
	 * Se crea el panel de botones de accesibilidad.
	 */
	private void createAccessibilityButtonsPanel() {

		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();

		//Panel que va a contener los botones de accesibilidad
		final JPanel panel = new JPanel(new GridBagLayout());
		panel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast()){
        	panel.setOpaque(false);
        }
		Utils.setContrastColor(panel);

		//Restricciones para los botones
		final GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		//Restore button
		final JPanel restorePanel = new JPanel();
		final ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

		this.restoreButton.addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, BotoneraSuperior.this.getRestoreButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, BotoneraSuperior.this.getRestoreButton(), tipText);
			}
		});
		final Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);

		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast()){
        	restorePanel.setOpaque(false);
        }
		Utils.setContrastColor(restorePanel);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	@Override
			public void actionPerformed(final ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});

		panel.add(restorePanel, consButtons);

		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		//Maximize button
		final JPanel maximizePanel = new JPanel();

		final ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);

		Utils.remarcar(this.maximizeButton);
		maximizePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast()){
        	maximizePanel.setOpaque(false);
        }
		Utils.setContrastColor(maximizePanel);
		maximizePanel.add(this.maximizeButton);

		this.maximizeButton.addFocusListener(new FocusListener() {

			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, BotoneraSuperior.this.getMaximizeButton(), tipText);
			}

			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, BotoneraSuperior.this.getMaximizeButton(), tipText);
			}
		});

		this.maximizeButton.addActionListener(new ActionListener() {
		    	@Override
				public void actionPerformed(final ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});


		panel.add(maximizePanel, consButtons);

		//Se anade al panel general
		//Restricciones para el panel de botones
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.insets = new Insets(0,0,0,0);
		c.anchor=GridBagConstraints.EAST;
		this.add(panel, c);


		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
    		//Se deshabilita el boton de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el boton de restaurar
    		this.restoreButton.setEnabled(true);
    	}
    	else {
    		//Se habilita el boton de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el boton de restaurar
    		this.restoreButton.setEnabled(false);
    	}

	}
}
