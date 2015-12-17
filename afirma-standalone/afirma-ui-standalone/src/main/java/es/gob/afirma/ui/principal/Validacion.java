/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.ExtFilter;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.SignedFileManager;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.visor.ui.VisorPanel;

/** Clase que muestra el panel de validacion VALIDe */
final class Validacion extends JPanel {

    private static final long serialVersionUID = 1L;

    /** Etiqueta del campo con la ruta del fichero de datos firmado. */
    private final JLabel browseDataLabel = new JLabel();
    JLabel getBrowseDataLabel() {
    	return this.browseDataLabel;
    }

    /** Campo de texto con el fichero de datos firmado. */
    private final JTextField dataFileField = new JTextField();
    JTextField getDataFileField() {
    	return this.dataFileField;
    }

    /** Bot&oacute;n para la b&uacute;squeda del fichero de datos firmado. */
    private final JButton browseDataButton = new JButton();
    JButton getBrowseDataButton() {
    	return this.browseDataButton;
    }

    /** Bot&oacute;n para la validaci&oacute;n de la firma. */
    private final JButton checkSignButton = new JButton();
    JButton getCheckSignButton() {
    	return this.checkSignButton;
    }

    /** Construye el panel y todos sus componentes visuales. */
    public Validacion() {
        initComponents();
    }

    /** Abre un di&aacute;logo de selecci&oacute;n de fichero y muestra el fichero seleccionado
     * en una caja de texto.
     * @param titleDialog T&iacute;tulo de la ventana modal.
     * @param filter Filtro de fichero.
     * @param campoFichero Campo en el que se escribe la ruta del fichero seleccionado */
    void browseActionPerformed(final String titleDialog, final ExtFilter filter, final JTextField campoFichero) {
        final File selectedFile =
            SelectionDialog.showFileOpenDialog(this, titleDialog, null, filter);
        if (selectedFile != null) {
        	campoFichero.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Eliminamos el layout
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();

        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.insets = new Insets(13, 13, 0, 13);

        // Componentes para la seleccion del fichero de firma
        final JLabel browseSignLabel = new JLabel();
        browseSignLabel.setText(Messages.getString("Validacion.buscar")); //$NON-NLS-1$
        browseSignLabel.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.buscar.description")); //$NON-NLS-1$
        Utils.setContrastColor(browseSignLabel);
        Utils.setFontBold(browseSignLabel);
        add(browseSignLabel, c);

        c.gridwidth = 1;
        c.insets = new Insets(0, 13, 0, 0);
        c.weightx = 1.0;
        c.gridy = 1;

        // Campo donde se guarda el nombre del fichero a firmar
        final JTextField signFileField = new JTextField();
        signFileField.setToolTipText(Messages.getString("Validacion.buscar.caja.description")); //$NON-NLS-1$
        signFileField.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                           Messages.getString("Validacion.buscar.caja.description.status"))); //$NON-NLS-1$
        signFileField.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                           Messages.getString("Validacion.buscar.caja.description.status"))); //$NON-NLS-1$
        signFileField.getAccessibleContext().setAccessibleName(browseSignLabel.getText() + " ALT + R."); //$NON-NLS-1$
        signFileField.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.buscar.caja.description")); //$NON-NLS-1$
        signFileField.addAncestorListener(new RequestFocusListener(false));

        signFileField.setEditable(false);

        Utils.remarcar(signFileField);
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            signFileField.setCaret(caret);
        }
        Utils.setFontBold(signFileField);
        add(signFileField, c);

        // Relacion entre etiqueta y campo de texto
        browseSignLabel.setLabelFor(signFileField);
        // Asignacion de mnemonico
        browseSignLabel.setDisplayedMnemonic(KeyEvent.VK_R);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        final JButton browseSignButton = new JButton();
        browseSignButton.setMnemonic(KeyEvent.VK_E);
        browseSignButton.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        browseSignButton.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        browseSignButton.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        browseSignButton.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        browseSignButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                browseActionPerformed(
                		Messages.getString("Validacion.chooser.title"), //$NON-NLS-1$
                		(ExtFilter) SignedFileManager.getCommonSignedFileFilter(),
                		signFileField);

                if (signFileField.getText() != null && signFileField.getText().length() > 0) {
                	try {
                	checkSignFile(
                			signFileField.getText(),
                			Validacion.this.getBrowseDataLabel(),
                			Validacion.this.getDataFileField(),
                			Validacion.this.getBrowseDataButton(),
                			Validacion.this.getCheckSignButton());
                	} catch (final Exception e) {
                		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                				"Ocurrio un error al comprobar el fichero cargado: " + e); //$NON-NLS-1$
                	}

                }
            }

			private void checkSignFile(final String path,
					                   final JLabel label,
					                   final JTextField field,
					                   final JButton button1,
					                   final JButton button2) throws AOException,
					                                                 IOException,
					                                                 URISyntaxException {

		        // Si el fichero es una firma explicita activamos el campo de seleccion
		        // de datos, si es una firma implicita activamos el boton de validacion
				final byte[] signData;
				try ( final InputStream is = AOUtil.loadFile(AOUtil.createURI(path)); ) {
					signData = AOUtil.getDataFromInputStream(is);
				}
		        final AOSigner signer = AOSignerFactory.getSigner(signData);
		        if (signer == null) {
		        	label.setEnabled(false);
		        	field.setEnabled(false);
		        	field.setText(""); //$NON-NLS-1$
		        	button1.setEnabled(false);
		        	button2.setEnabled(false);
		        }
		        else if (signer.getData(signData) == null) {
		        	label.setEnabled(true);
		        	field.setEnabled(true);
		        	button1.setEnabled(true);
		        	chechDataFile();
		        }
		        else {
		        	label.setEnabled(false);
		        	field.setEnabled(false);
		        	field.setText(""); //$NON-NLS-1$
		        	button1.setEnabled(false);
		        	button2.setEnabled(true);
		        }
			}
        });
        Utils.remarcar(browseSignButton);
        Utils.setContrastColor(browseSignButton);
        Utils.setFontBold(browseSignButton);
        panelExaminar.add(browseSignButton);
        add(panelExaminar, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridx = 0;
        c.gridy = 2;

        // Componentes para la seleccion del fichero de datos
        this.browseDataLabel.setText(Messages.getString("Validacion.datos.buscar")); //$NON-NLS-1$
        this.browseDataLabel.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.datos.buscar.description")); //$NON-NLS-1$
        Utils.setContrastColor(this.browseDataLabel);
        Utils.setFontBold(this.browseDataLabel);
        add(this.browseDataLabel, c);

        this.browseDataLabel.setEnabled(false);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridx = 0;
        c.gridy = 3;

        // Campo donde se guarda el nombre del fichero de datos
        this.dataFileField.setToolTipText(Messages.getString("Validacion.datos.buscar.caja.description")); //$NON-NLS-1$
        this.dataFileField.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                           Messages.getString("Validacion.datos.buscar.caja.description.status"))); //$NON-NLS-1$
        this.dataFileField.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                           Messages.getString("Validacion.datos.buscar.caja.description.status"))); //$NON-NLS-1$
        this.dataFileField.getAccessibleContext().setAccessibleName(this.browseDataLabel.getText() + " ALT + O."); //$NON-NLS-1$
        this.dataFileField.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.datos.buscar.caja.description")); //$NON-NLS-1$
        this.dataFileField.addAncestorListener(new RequestFocusListener(false));

        Utils.remarcar(this.dataFileField);
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            this.dataFileField.setCaret(caret);
        }
        Utils.setFontBold(this.dataFileField);
        add(this.dataFileField, c);

        this.dataFileField.setEnabled(false);

        // Relacion entre etiqueta y campo de texto
        this.browseDataLabel.setLabelFor(this.dataFileField);
        // Asignacion de mnemonico
        this.browseDataLabel.setDisplayedMnemonic(KeyEvent.VK_O);


        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar2 = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        this.browseDataButton.setMnemonic(KeyEvent.VK_X);
        this.browseDataButton.setText(Messages.getString("PrincipalGUI.Examinar")); //$NON-NLS-1$
        this.browseDataButton.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); //$NON-NLS-1$
        this.browseDataButton.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        this.browseDataButton.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        this.browseDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                browseActionPerformed(
                		Messages.getString("Validacion.chooser.title"), //$NON-NLS-1$
                		null,
                		Validacion.this.getDataFileField());

                chechDataFile();
            }
        });
        Utils.remarcar(this.browseDataButton);
        Utils.setContrastColor(this.browseDataButton);
        Utils.setFontBold(this.browseDataButton);
        panelExaminar2.add(this.browseDataButton);
        add(panelExaminar2, c);

        this.browseDataButton.setEnabled(false);


        c.gridwidth = 2;
        c.insets = new Insets(0, 13, 0, 13);
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 4;

        // Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
        final JPanel emptyPanel = new JPanel();
        add(emptyPanel, c);

        // Panel con los botones
        final JPanel panelBotones = new JPanel(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.anchor = GridBagConstraints.FIRST_LINE_START; // control de la orientacion de componentes al redimensionar
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.ipadx = 15;
        cons.gridx = 0;

        // Etiqueta para rellenar a la izquierda
        final JLabel label = new JLabel();
        panelBotones.add(label, cons);

        final JPanel panelFirmar = new JPanel(new GridLayout(1, 1));

        // Boton Validar
        this.checkSignButton.setMnemonic(KeyEvent.VK_V);
        this.checkSignButton.setText(Messages.getString("Validacion.btnValidar")); //$NON-NLS-1$
        this.checkSignButton.setToolTipText(Messages.getString("Validacion.btnValidar.description")); //$NON-NLS-1$
        this.checkSignButton.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                             Messages.getString("Validacion.btnValidar.description.status"))); //$NON-NLS-1$
        this.checkSignButton.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                             Messages.getString("Validacion.btnValidar.description.status"))); //$NON-NLS-1$
        this.checkSignButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                validateActionPerformance(signFileField.getText(), Validacion.this.getDataFileField().getText());
            }
        });
        this.checkSignButton.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.btnValidar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(this.checkSignButton);
        Utils.setContrastColor(this.checkSignButton);
        Utils.setFontBold(this.checkSignButton);

        this.checkSignButton.setEnabled(false);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        final JPanel buttonPanel = new JPanel();
        panelFirmar.add(this.checkSignButton);
        buttonPanel.add(panelFirmar, BorderLayout.CENTER);
        panelBotones.add(buttonPanel, cons);

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("validacion"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$
        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.insets = new Insets(13, 13, 13, 13);
        c.weighty = 0.0;
        c.weightx = 1.0;
        c.gridy = 5;

        add(panelBotones, c);
    }

    /** Muestra el di&aacute;logo con la informaci&oacute;n de validaci&oacute;n
     * de una firma.
     * @param signPath Ruta del fichero de firma. */
    void validateActionPerformance(final String signPath, final String dataPath) {
        if (signPath == null || signPath.trim().length() <= 0) {
            CustomDialog.showMessageDialog(
            		SwingUtilities.getRoot(this),
            		true,
            		Messages.getString("Validacion.msg.error.fichero"), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        final File signFile = new File(signPath);
        if (!signFile.exists() || !signFile.isFile()) {
            CustomDialog.showMessageDialog(
            		SwingUtilities.getRoot(this),
                    true,
                    Messages.getString("Validacion.msg.error.nofichero", signPath), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        if (!signFile.canRead()) {
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                    true,
                    Messages.getString("Validacion.msg.error.noLectura", signPath), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        final File dataFile = dataPath != null && dataPath.length() > 0 ? new File(dataPath) : null;
        if (dataFile != null) {
        	if (!dataFile.exists() || !dataFile.isFile()) {
        		CustomDialog.showMessageDialog(
        				SwingUtilities.getRoot(this),
        				true,
        				Messages.getString("Validacion.msg.error.nofichero", dataPath), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        		return;
        	}

        	if (!dataFile.canRead()) {
        		CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
        				true,
        				Messages.getString("Validacion.msg.error.noLectura", dataPath), Messages.getString("error"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        		return;
        	}
        }

        final VisorPanel visorPanel = new VisorPanel(signFile, null, dataFile);
        visorPanel.setTitle(Messages.getString("Visor.window.title")); //$NON-NLS-1$
        visorPanel.setVisible(true);
    }


	void chechDataFile() {
		if (Validacion.this.dataFileField.getText() != null && Validacion.this.dataFileField.getText().length() > 0) {
     		Validacion.this.checkSignButton.setEnabled(true);
        }
	}
}
