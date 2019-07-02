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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JWindow;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.signvalidation.ValidateBinarySignature;
import es.gob.afirma.signvalidation.ValidatePdfSignature;
import es.gob.afirma.signvalidation.ValidateXMLSignature;
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

    /** Panel de botones relacionados con la accesibilidad. */
    private JPanel accessibilityButtonsPanel = null;

    /** Bot&oacute;n de maximizar. */
    private JButton maximizeButton;

    JButton getMaximizeButton() {
    	return this.maximizeButton;
    }

    /** Bot&oacute;n de restaurar. */
    private JButton restoreButton;

    JButton getRestoreButton() {
    	return this.restoreButton;
    }

    /**
     * Panel con los resultados de la validaci&oacute;n.
     */
    private SignResultPanel resultPanel;

    /**
     * Panel con los datos de la firma validada.
     */
    private SignDataPanel dataPanel;

    /**
     * Manejador de las peticiones de carga de datos.
     */
    private PropertyChangeListener loadExternalDataListener = null;

    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros.
     * @param signFile Fichero de firma.
     * @param sign Firma. */
    public VisorPanel(final File signFile, final byte[] sign) {
        this(signFile, sign, null);
    }

    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros.
     * @param signFile Fichero de firma.
     * @param sign Firma.
     * @param dataFile Fichero de datos.
     */
    public VisorPanel(final File signFile, final byte[] sign, final File dataFile) {
    	byte[] signature = sign == null ? null : sign.clone();
        if (signature == null) {
            signature = signFile == null ? null : loadFile(signFile);
        }
        if (signature != null) {
        	final byte[] data = dataFile == null ? null : loadFile(dataFile);
        	createUI();
        	openSign(signFile, signature, dataFile, data);
        }
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
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, VisorPanel.this.getRestoreButton(), tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, VisorPanel.this.getRestoreButton(), tipText);
            }
        });
        final Dimension dimension = new Dimension(20,20);
        this.restoreButton.setPreferredSize(dimension);

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
        maximizePanel.add(this.maximizeButton);

        this.maximizeButton.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                Utils.showToolTip(true, tip, VisorPanel.this.getMaximizeButton(), tipText);
            }

            @Override
            public void focusLost(final FocusEvent e) {
                Utils.showToolTip(false, tip, VisorPanel.this.getMaximizeButton(), tipText);
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

    private void createUI() {

        createAccessibilityButtonsPanel();
        this.resultPanel = new SignResultPanel(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null));
        this.dataPanel = new SignDataPanel();

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
        bClose.getAccessibleContext().setAccessibleName(Messages.getString("VisorPanel.1") + ". " +Messages.getString("VisorPanel.3"));  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

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
        c.gridy = c.gridy + 1;
        add(this.resultPanel, c);
        c.weighty = 1.0;
        c.gridy = c.gridy + 1;
        c.insets = new Insets(0, 11, 11, 11);
        add(this.dataPanel, c);
        c.weighty = 0.0;
        c.gridy = c.gridy + 1;
        c.insets = new Insets(0, 11, 11, 11);
        add(bottonPanel, c);

        repaint();
    }

    /**
     * Analiza una firma indicada mediante un fichero o como un array de
     * bytes y muestra la informaci&oacute;n extra&iacute;da en un di&aacute;logo.
     * Si se indican ambos par&aacute;metros, se dar&aacute; prioridad al
     * byte array introducido.
     * @param signFile Fichero de firma.
     * @param signature Firma.
     * @param dataFile Fichero de datos.
     * @param data Datos.
     */
    public void openSign(final File signFile, final byte[] signature, final File dataFile, final byte[] data) {

    	SignValidity validity = new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
        if (signature != null) {
            try {
                validity = validateSign(signature, data);
            }
            catch (final Exception e) {
                validity = new SignValidity(SIGN_DETAIL_TYPE.KO, null);
            }
        }

    	this.resultPanel.update(validity);
    	this.dataPanel.load(signFile, signature, dataFile);
    	if (this.loadExternalDataListener != null) {
    		this.dataPanel.removePropertyChangeListener(this.loadExternalDataListener);
    	}
    	this.loadExternalDataListener = new LoadExternalDataListener(signFile, signature);
        this.dataPanel.addPropertyChangeListener(this.loadExternalDataListener);

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
            else {
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
            PrincipalGUI.setWizardActualPositionX(j.getX());
            PrincipalGUI.setWizardActualPositionY(j.getY());
            PrincipalGUI.setWizardActualWidth(j.getWidth());
            PrincipalGUI.setWizardActualHeight(j.getHeight());
        }
    }

    /**
     * Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws IOException Cuando ocurre algun error durante la lectura de los datos.
     * @throws Exception Cuando los datos introducidos no se corresponden con una firma.
     */
    private static SignValidity validateSign(final byte[] sign, final byte[] data) throws IOException {
        if (DataAnalizerUtil.isSignedPDF(sign)) {
        	return new ValidatePdfSignature().validate(sign);
        }
        else if (DataAnalizerUtil.isSignedInvoice(sign)) { // Factura electronica
            return new ValidateXMLSignature().validate(sign);
        }
        else if (DataAnalizerUtil.isSignedXML(sign)) {
            return new ValidateXMLSignature().validate(sign);
        }
        else if(DataAnalizerUtil.isSignedBinary(sign)) {
            return ValidateBinarySignature.validate(sign, data);
        }
        else if (DataAnalizerUtil.isSignedODF(sign)) {
            return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.ODF_UNKOWN_VALIDITY);
        }
        else if (DataAnalizerUtil.isSignedOOXML(sign)) {
            return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.OOXML_UNKOWN_VALIDITY);
        }
        return new SignValidity(SIGN_DETAIL_TYPE.KO, null);
    }

    private class LoadExternalDataListener implements PropertyChangeListener {

    	private File signFile = null;
    	private byte[] sign = null;

    	public LoadExternalDataListener(final File signatureFile, final byte[] signatureData) {
    		this.signFile = signatureFile;
    		this.sign = signatureData.clone();
		}

		@Override
		public void propertyChange(final PropertyChangeEvent event) {
			if (event.getNewValue() != null && event.getNewValue() instanceof File) {
				byte[] signature = this.sign;
				if (signature == null) {
					signature = loadFile(this.signFile);
				}
				if (signature != null) {
					final byte[] data = loadFile((File) event.getNewValue());
					openSign(this.signFile, signature, (File) event.getNewValue(), data);
				}
			}
		}
    }


    /**
     * Recupera el contenido de un fichero.
     * @param file Fichero.
     * @return Datos contenidos en el fichero o {@code null} si ocurri&oacute; alg&uacute;n error.
     */
    byte[] loadFile(final File file) {
    	try ( final FileInputStream fis = new FileInputStream(file); ) {
    		return AOUtil.getDataFromInputStream(fis);
		}
        catch(final OutOfMemoryError e) {
        	CustomDialog.showMessageDialog(
    			SwingUtilities.getRoot(this), true, Messages.getString("Firma.msg.error.fichero.tamano"), //$NON-NLS-1$
                Messages.getString("error"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return null;
        }
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el fichero: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
    }
}
