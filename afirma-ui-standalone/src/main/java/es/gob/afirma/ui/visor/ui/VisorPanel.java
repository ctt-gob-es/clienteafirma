/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.visor.ui;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signature.ValidateBinarySignature;
import es.gob.afirma.signature.ValidateXMLSignature;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.visor.DataAnalizerUtil;


/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci
 */
public final class VisorPanel extends JAccessibilityDialogWizard {

    /** Version ID */
    private static final long serialVersionUID = 8309157734617505338L;

    //    JButton maximizar = new JButton();
    //
    //    JButton restaurar = new JButton();

    /**
     * Panel de botones relacionados con la accesibilidad.
     */
    private JPanel accessibilityButtonsPanel = null;

    /**
     * Boton de maximizar.
     */
    JButton maximizeButton = null;

    /**
     * Boton de restaurar.
     */
    JButton restoreButton = null;

    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros.
     * @param signFile Fichero de firma.
     * @param sign Firma.
     */
    public VisorPanel(final File signFile, final byte[] sign) {
        createUI(signFile, sign);
    }

    /**
     * Se crea el panel de botones de accesibilidad.
     */
    private void createAccessibilityButtonsPanel() {
        this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());

        //Para el tooltip
        final JWindow tip = new JWindow();
        final JLabel tipText = new JLabel();

        //Panel que va a contener los botones de accesibilidad
        final JPanel panel = new JPanel(new GridBagLayout());

        //panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
        //panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
        //panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
        //panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
        //panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
        //panel.setBorder(BorderFactory.createCompoundBorder());
        //panel.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK));

        //Restricciones para los botones
        final GridBagConstraints consButtons = new GridBagConstraints();
        consButtons.fill = GridBagConstraints.BOTH;
        consButtons.gridx = 0;
        consButtons.gridy = 0;
        consButtons.weightx = 1.0;
        consButtons.weighty = 1.0;
        consButtons.insets = new Insets(0,0,0,0);  //right padding
        //consButtons.anchor=GridBagConstraints.EAST;

        //Restore button
        final JPanel restorePanel = new JPanel();
        //this.restoreButton = getButton("r", KeyEvent.VK_R );
        final ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
        this.restoreButton = new JButton(imageIconRestore);
        this.restoreButton.setMnemonic(KeyEvent.VK_R );
        this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
        this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

        this.restoreButton.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, VisorPanel.this.restoreButton, tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, VisorPanel.this.restoreButton, tipText);
            }
        });
        final Dimension dimension = new Dimension(20,20);
        this.restoreButton.setPreferredSize(dimension);

        //this.restoreButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
        //this.restoreButton.setContentAreaFilled(false); //area del boton invisible
        this.restoreButton.setName("restaurar"); //$NON-NLS-1$
        Utils.remarcar(this.restoreButton);
        restorePanel.add(this.restoreButton);
        this.restoreButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                restaurarActionPerformed();
            }
        });


        panel.add(restorePanel, consButtons);


        consButtons.gridx = 1;
        //consButtons.weightx = 0.5;
        consButtons.insets = new Insets(0,0,0,0);  //right padding

        //Maximize button
        final JPanel maximizePanel = new JPanel();

        final ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
        this.maximizeButton = new JButton(imageIconMaximize);
        this.maximizeButton.setMnemonic(KeyEvent.VK_M );
        this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
        this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

        //this.maximizeButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
        //this.maximizeButton.setContentAreaFilled(false); //area del boton invisible

        this.maximizeButton.setName("maximizar"); //$NON-NLS-1$
        //Se asigna una dimension por defecto
        this.maximizeButton.setPreferredSize(dimension);

        Utils.remarcar(this.maximizeButton);
        //maximizePanel.add(this.maximizeButton, consMaximizePanel);
        maximizePanel.add(this.maximizeButton);

        this.maximizeButton.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, VisorPanel.this.maximizeButton, tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, VisorPanel.this.maximizeButton, tipText);
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
        //c.insets = new Insets(3,3,0,3);
        c.insets = new Insets(0,0,0,0);
        c.anchor=GridBagConstraints.EAST;
        this.accessibilityButtonsPanel.add(panel, c);


        // Habilitado/Deshabilitado de botones restaurar/maximizar
        if (GeneralConfig.isMaximized()){
            //Se deshabilita el boton de maximizado
            this.maximizeButton.setEnabled(false);
            //Se habilita el boton de restaurar
            this.restoreButton.setEnabled(true);
        } else {
            //Se habilita el boton de maximizado
            this.maximizeButton.setEnabled(true);
            //Se deshabilita el boton de restaurar
            this.restoreButton.setEnabled(false);
        }

    }

    private void createUI(final File signFile, final byte[] sign) {
        this.setLayout(new GridBagLayout());

        openSign(signFile, sign);
    }

    @Override
    public int getMinimumRelation() {
        return 9;
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
            j.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
        }

        //Se deshabilita el boton de maximizar puesto que se ha pulsado.
        this.maximizeButton.setEnabled(false);
        this.restoreButton.setEnabled(true);
    }

    /**
     * Analiza una firma indicada mediante un fichero o como un array de
     * bytes y muestra la informaci&oacute;n extra&iacute;da en un di&aacute;logo.
     * Si se indican ambos par&aacute;metros, se dar&aacute; prioridad al
     * byte array introducido.
     * @param signFile Fichero de firma.
     * @param signature Firma.
     */
    public void openSign(final File signFile, final byte[] signature) {

        if (signFile == null && signature == null) {
            Logger.getLogger("es.gob.afirma").warning("Se ha intentado abrir una firma nula");  //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        byte[] sign = (signature != null) ?  signature.clone() : null;

        if (sign == null) {
            if (signFile != null) {
                try {
                    final FileInputStream fis = new FileInputStream(signFile);
                    sign = AOUtil.getDataFromInputStream(fis);
                    try { fis.close(); } catch (final Exception e) { /* Ignoramos los errores */ }
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el fichero de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }

        SignValidity validity = new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
        if (sign != null) {
            try {
                validity = validateSign(sign);
            } catch (final Exception e) {
                validity = new SignValidity(SIGN_DETAIL_TYPE.KO, null);
            }
        }
        createAccessibilityButtonsPanel();
        final JPanel resultPanel = new SignResultPanel(validity);
        final JPanel dataPanel = new SignDataPanel(signFile, sign, null, null);

        final JPanel bottonPanel = new JPanel(true);
        bottonPanel.setLayout(new FlowLayout(FlowLayout.TRAILING));

        //Espacio entre botones
        final JPanel panelVacio = new JPanel();
        panelVacio.setPreferredSize(new Dimension(300, 10));
        bottonPanel.add(panelVacio);

        final JPanel panelClose = new JPanel(new GridLayout(1, 1));
        final JButton bClose = new JButton(Messages.getString("VisorPanel.1")); //$NON-NLS-1$
        bClose.setMnemonic(KeyEvent.VK_C);
        bClose.setToolTipText(Messages.getString("VisorPanel.2")); //$NON-NLS-1$
        bClose.getAccessibleContext().setAccessibleName(Messages.getString("VisorPanel.1") + ". " +Messages.getString("VisorPanel.3"));  //$NON-NLS-1$ //$NON-NLS-3$

        Utils.remarcar(bClose);
        Utils.setContrastColor(bClose);
        Utils.setFontBold(bClose);

        bClose.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                saveSizePosition();
                VisorPanel.this.dispose();
            }
        });

        panelClose.add(bClose);
        bottonPanel.add(panelClose);

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0;
        c.gridy = 0;
        add(this.accessibilityButtonsPanel,c);
        c.weightx = 1.0;
        c.gridy = c.gridy +1;
        add(resultPanel, c);
        c.weighty = 1.0;
        c.gridy = c.gridy +1;
        c.insets = new Insets(0, 11, 11, 11);
        add(dataPanel, c);
        c.weighty = 0.0;
        c.gridy = c.gridy +1;
        c.insets = new Insets(0, 11, 11, 11);
        add(bottonPanel, c);

        repaint();

    }

    /**
     * Restaura el tamano de la ventana a la posicion anterior al maximizado
     */
    public void restaurarActionPerformed(){

        final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
        if (JAccessibilityDialogWizard.getActualPositionX() != -1 && JAccessibilityDialogWizard.getActualPositionY() != -1 && JAccessibilityDialogWizard.getActualWidth() != -1 && JAccessibilityDialogWizard.getActualHeight() != -1){
            j.setBounds(JAccessibilityDialogWizard.getActualPositionX(), JAccessibilityDialogWizard.getActualPositionY(), JAccessibilityDialogWizard.getActualWidth(), JAccessibilityDialogWizard.getActualHeight());
        } else {
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            if (Platform.getOS().equals(Platform.OS.LINUX)){
                j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
            } else{
                j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
            }
            j.setMinimumSize(new Dimension(j.getSize().width, j.getSize().height));
        }
        //Se deshabilita el boton de restaurar puesto que se ha pulsado.
        this.maximizeButton.setEnabled(true);
        this.restoreButton.setEnabled(false);
    }

    /**
     * Guarda el tamano y posicion de la ventana antes de cerrarse
     */
    public void saveSizePosition(){
        // Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
        if (!GeneralConfig.isMaximized()){
            final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
            PrincipalGUI.wizardActualPositionX = j.getX();
            PrincipalGUI.wizardActualPositionY = j.getY();
            PrincipalGUI.wizardActualWidth = j.getWidth();
            PrincipalGUI.wizardActualHeight = j.getHeight();
        }
    }

    /**
     * Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws Exception Cuando los datos introducidos no se corresponden con una firma.
     */
    private static SignValidity validateSign(final byte[] sign) {
        if (DataAnalizerUtil.isPDF(sign)) {
            return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
        }
        else if (DataAnalizerUtil.isXML(sign)) {
            return ValidateXMLSignature.validate(sign);
        }
        else if(new AOCMSSigner().isSign(sign) || new AOCAdESSigner().isSign(sign)) {
            return ValidateBinarySignature.validate(sign, null);
        }
        return new SignValidity(SIGN_DETAIL_TYPE.KO, null);
    }
}
