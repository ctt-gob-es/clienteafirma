
package es.gob.afirma.standalone.ui.envelopes;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Panel para seleccionar los destinatarios que se quieren incluir en el sobre digital.
 * @author Juliana Marulanda
 */
public class DigitalEnvelopeRecipients extends JPanel {

	private static final long serialVersionUID = 8190414784696825608L;
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JButton nextButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.3")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.4")); //$NON-NLS-1$
	private final JButton backButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.5") ); //$NON-NLS-1$
	private final JButton removeButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.18")); //$NON-NLS-1$
	private final JButton addButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.16")); //$NON-NLS-1$

	final JComboBox<KeyStoreConfiguration> comboBoxRecipients = new JComboBox<>();

	private final List<CertificateDestiny> certificateList = new ArrayList<>();
	List<CertificateDestiny> getCertificateList() {
		return this.certificateList;
	}

	private final JList<String> recipientsList = new JList<>();
	JList<String> getRecipientsList() {
		return this.recipientsList;
	}

	private final JScrollPane scrollPane = new JScrollPane(
		this.recipientsList,
		ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
		ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
	);

	private final EnvelopesTypeResources envelopeType;
	EnvelopesTypeResources getEnvelopeType() {
		return this.envelopeType;
	}

	private final String filePath;
	String getFilePath() {
		return this.filePath;
	}

	private final DigitalEnvelopePresentation dialog;
	DigitalEnvelopePresentation getDialog() {
		return this.dialog;
	}

	private final JPanel panel = new JPanel();
	JPanel getPanel() {
		return this.panel;
	}

	private final JPanel panelCentral = new JPanel();
	JPanel getPanelCentral() {
		return this.panelCentral;
	}

	private final String signAlgorithm;
	String getSignAlgorithm() {
		return this.signAlgorithm;
	}

	/**
	 * Genera un panel de destinatarios de sobres digitales.
	 * @param parent Di&aacute;logo del asistente de ensobrado.
	 * @param file Ruta del fichero a ensobrar.
	 * @param type Tipo de sobre a realizar.
	 * @param algorithm Tipo de algortimo de cifrado. */
	public DigitalEnvelopeRecipients(final DigitalEnvelopePresentation parent,
									 final String file,
									 final EnvelopesTypeResources type,
									 final String algorithm) {
		this.dialog = parent;
		this.filePath = file;
		this.envelopeType = type;
		this.signAlgorithm = algorithm;
		createUI();
	}

	void createUI() {

		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.32") //$NON-NLS-1$
		);

		this.recipientsList.setModel(new DefaultListModel<String>());

		// Commbobox con los tipos de certificado a elegir
		this.comboBoxRecipients.setModel(new DefaultComboBoxModel<>(EnvelopesUtils.getKeyStoresToWrap()));
		this.comboBoxRecipients.setToolTipText(SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.12")); //$NON-NLS-1$);
		this.comboBoxRecipients.addKeyListener(this.dialog);

        // Panel con el contenido
        final GridBagLayout gbLayout = new GridBagLayout();
        this.panelCentral.setBackground(Color.WHITE);
        this.panelCentral.setLayout(gbLayout);
        this.panelCentral.setBorder(BorderFactory.createEmptyBorder());

        // Etiqueta con el texto "Destinatarios..."
        final JLabel label = new JLabel(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.0"), //$NON-NLS-1$
    		SwingConstants.CENTER
        );
        label.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$

        // Label para el Combobox
        final JLabel labelCB = new JLabel(
			SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.11") //$NON-NLS-1$
		);
        labelCB.setLabelFor(this.comboBoxRecipients);

		// Boton de eliminar destinatario
		this.removeButton.setMnemonic('E');
		this.removeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.31") //$NON-NLS-1$
		);
		this.removeButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					removeRecipient();
				}
			}
		);
		this.removeButton.addKeyListener(this.dialog);

		// Boton de anadir
		this.addButton.setMnemonic('D');
		this.addButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.30") //$NON-NLS-1$
		);
		this.addButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					addRecipient();
				}
			}
		);
		this.addButton.addKeyListener(this.dialog);

		// Label del texto donde se almacena el sobre
		final JLabel labelRec = new JLabel(
			SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.17") //$NON-NLS-1$
		);
		labelRec.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$
		labelRec.setLabelFor(this.scrollPane);

		// Lugar donde se muestra el sobre elegido
		this.scrollPane.setBorder(BorderFactory.createLineBorder(Color.black));
		this.scrollPane.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$
		this.scrollPane.addKeyListener(this.dialog);
		this.recipientsList.setFocusable(false);

	    // Boton de siguiente
 		this.nextButton.setMnemonic('S');
 		this.nextButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.6") //$NON-NLS-1$
		);
 		this.nextButton.addActionListener(
 			new ActionListener() {
 				/** {@inheritDoc} */
 				@Override
 				public void actionPerformed(final ActionEvent ae) {
 					getDialog().remove(getPanelCentral());
 					getDialog().remove(getPanel());
 					getDialog().remove(getDialog().getRecipientsPanel());
 					getDialog().setSendersPanel(
 						new DigitalEnvelopeSender(
							getDialog(),
							getFilePath(),
							getEnvelopeType(),
							getCertificateList(),
							getSignAlgorithm()
						)
 					);
 					getDialog().add(getDialog().getSendersPanel());
 				}
 			}
 		);
 		this.nextButton.addKeyListener(this.dialog);

 		// Boton cancelar
		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.7") //$NON-NLS-1$
		);
		this.cancelButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					getDialog().setVisible(false);
					getDialog().dispose();
				}
			}
		);
		this.cancelButton.addKeyListener(this.dialog);

		// Boton de volver
		this.backButton.setMnemonic('A');
		this.backButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.8") //$NON-NLS-1$
		);
		this.backButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				getDialog().remove(getPanelCentral());
				getDialog().remove(getPanel());
				getDialog().remove(getDialog().getRecipientsPanel());
				getDialog().setFilePanel(new DigitalEnvelopeSelectFile(getDialog()));
				getDialog().add(getDialog().getFilePanel());
			}
		});
		this.backButton.addKeyListener(this.dialog);

		this.panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.panel.add(this.cancelButton);
			this.panel.add(this.backButton);
			this.panel.add(this.nextButton);
		}
		else {
			this.panel.add(this.backButton);
			this.panel.add(this.nextButton);
			this.panel.add(this.cancelButton);
		}

		final JPanel emptyPanel = new JPanel();
		emptyPanel.setBackground(Color.WHITE);

        final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.NORTHWEST;
		c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridx = 0;
        c.gridy = 0;

		c.gridwidth = GridBagConstraints.REMAINDER;
		c.insets = new Insets(30, 10, 0, 20);
		this.panelCentral.add(label, c);
        c.gridy++;
		this.panelCentral.add(labelCB, c);
		c.gridwidth = 2;
		c.insets = new Insets(5, 10, 0, 20);
		c.weightx = 0.0;
		c.gridy++;
		c.fill = GridBagConstraints.NONE;
        this.panelCentral.add(this.comboBoxRecipients, c);
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.anchor = GridBagConstraints.LINE_END;
		this.panelCentral.add(this.addButton, c);
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 10, 0, 20);
		c.gridx = 0;
		c.weightx = 1.0;
		c.gridy++;
		this.panelCentral.add(labelRec, c);
		c.insets = new Insets(5, 10, 0, 20);
		c.ipady = 150;
		c.gridy++;
		this.panelCentral.add(this.scrollPane, c);
		c.insets = new Insets(20, 10, 20, 20);
		c.gridx = 1;
		c.ipady = 0;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.gridy++;
		c.fill = GridBagConstraints.NONE;
		this.panelCentral.add(this.removeButton, c);
        c.weighty = 1.0;
		c.gridy++;
		this.panelCentral.add(emptyPanel, c);
        this.dialog.getContentPane().add(this.panelCentral);
        this.dialog.getContentPane().add(this.panel, BorderLayout.PAGE_END);
        this.dialog.revalidate();
        this.dialog.repaint();
		enableButtons(this.recipientsList.getModel().getSize() > 0);
	}

    /**
     * A&ntilde;ade un destinatario del tipo seleccionado.
     */
    void addRecipient() {

    	final DefaultListModel<String> modelList = (DefaultListModel<String>) getRecipientsList().getModel();
    	String[] filter;
    	AOKeyStoreManager keyStoreManager = null;
        final KeyStoreConfiguration kc = (KeyStoreConfiguration) this.comboBoxRecipients.getSelectedItem();

        try {
            final AOKeyStore ao = kc.getType();
            String lib = null;
            if (ao == AOKeyStore.PKCS12 || ao == AOKeyStore.SINGLE) {
                if (ao == AOKeyStore.PKCS12) {
                    filter = new String[] {"p12", "pfx"};  //$NON-NLS-1$//$NON-NLS-2$
                }
                else {
                    filter = new String[] { "cer", "p7b", "p7s"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                }
                final File keystorePath = EnvelopesUtils.addFileSelected(filter, this.comboBoxRecipients, getDialog());
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
            }
            else if (ao == AOKeyStore.PKCS11) {
                filter = new String[] {"dll", "so"};  //$NON-NLS-1$//$NON-NLS-2$
                final File keystorePath = EnvelopesUtils.addFileSelected(filter, this.comboBoxRecipients, getDialog());
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
            }
            keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
        		ao,
        		lib,
        		"default", //$NON-NLS-1$
        		ao.getStorePasswordCallback(getDialog()),
        		getDialog()
            );
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
            return;
        }
        catch (final IOException e) {
        	AOUIFactory.showErrorMessage(
				this.dialog,
				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.23"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.20"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			LOGGER.severe("Error generando o guardando la huella digital"  + e); //$NON-NLS-1$
			return;
	    }
	    catch (final Exception e) {
	        LOGGER.severe("No se ha podido abrir el almacen de certificados: " + e); //$NON-NLS-1$
	        AOUIFactory.showErrorMessage(
				this.dialog,
				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.19"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.20"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
	        return;
	    }
        final CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this.dialog);
	    // Comprobamos que el certificado es correcto
	    if (certDest.getAlias() != null) {
	        boolean copiar = true;
	        for (int i = 0; i < modelList.getSize(); i++) {
	            if (certDest.getAlias().equals(modelList.getElementAt(i))) {
	                copiar = false;
	            }
	        }
	        if (copiar) {
	        	modelList.addElement(certDest.getAlias());
	            this.certificateList.add(certDest);
	        }
	        else {
	        	 LOGGER.severe("Ya existe ese usuario"); //$NON-NLS-1$
	 	        AOUIFactory.showMessageDialog(
     				this.dialog,
     				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.25"), //$NON-NLS-1$
     				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.20"), //$NON-NLS-1$
     				JOptionPane.ERROR_MESSAGE
     			);
	 	        return;
	        }
	    }
	    enableButtons(true);
    }

    /**
    * Elimina un destintatario de la lista.
    */
    void removeRecipient() {
    	final DefaultListModel<String>listaModel = (DefaultListModel<String>) getRecipientsList().getModel();
        for (int i = 0; i < this.certificateList.size(); i++) {
            if (this.certificateList.get(i).getAlias().equals(this.recipientsList.getSelectedValue())) {
            	this.certificateList.remove(this.certificateList.get(i));
                listaModel.remove(this.recipientsList.getSelectedIndex());
                break;
            }
        }

        if (listaModel.isEmpty()) {
            enableButtons(false);
        }
    }

    void enableButtons(final boolean enable) {
    	this.nextButton.setEnabled(enable);
        this.nextButton.setFocusable(enable);
        this.removeButton.setEnabled(enable);
        this.removeButton.setFocusable(enable);
        if (enable) {
        	this.nextButton.requestFocusInWindow();
        }
        else {
        	this.addButton.requestFocusInWindow();
        }
    }
}
