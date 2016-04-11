package es.gob.afirma.standalone.ui.envelopes;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.UnrecoverableEntryException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.standalone.SimpleAfirmaMessages;


public class DigitalEnvelopeSender extends JDialog {

	private static final long serialVersionUID = 7169956308231498090L;
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private final JDialog dialog;
	JDialog getDialog() {
		return this.dialog;
	}
	private BufferedImage icon;
	public BufferedImage getIcon() {
		return this.icon;
	}
	public void setIcon(final BufferedImage iconBI) {
		this.icon = iconBI;
	}

	private final JComboBox<KeyStoreConfiguration> comboBox = new JComboBox<>();
	JComboBox<KeyStoreConfiguration> getComboBox() {
		return this.comboBox;
	}

	private final JButton addButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopeSender.17")); //$NON-NLS-1$
	private final JButton removeButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopeSender.19")); //$NON-NLS-1$
	private final JButton nextButton = new JButton(SimpleAfirmaMessages.getString("MenuDigitalEnvelope.21")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.15")); //$NON-NLS-1$
	private final JButton backButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.16") ); //$NON-NLS-1$

	JButton getBackButton() {
		return this.backButton;
	}

	JButton getRemoveButton() {
		return this.removeButton;
	}

	private final List<CertificateDestiny> certificateRecipientsList;
	private final List<CertificateDestiny> certificateList = new ArrayList<>();
	private final JList<String> senderList = new JList<>();
	JList<String> getSendersList() {
		return this.senderList;
	}

	private final JScrollPane scrollPane = new JScrollPane(
		this.senderList,
		ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
		ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
	);

	final JPanel panel = new JPanel();
	JPanel getPanel() {
		return this.panel;
	}
	private final EnvelopesTypeResources envelopeType;
	EnvelopesTypeResources getEnvelopeType() {
		return this.envelopeType;
	}

	private final String filePath;

	private PrivateKeyEntry privateKeyEntry;
	String getFilePath() {
		return this.filePath;
	}

	/**
	 * Constructor de la clase.
	 * @param parent
	 * @param fileSelected
	 */
	public DigitalEnvelopeSender(final JDialog parent,
								final String file,
								final EnvelopesTypeResources type,
								final List<CertificateDestiny> certificateRecipientsList) {
		this.dialog = parent;
		this.filePath = file;
		this.envelopeType = type;
		this.certificateRecipientsList = certificateRecipientsList;
		this.privateKeyEntry = null;
		createUI();
	}

	void createUI() {

		this.senderList.setModel(new DefaultListModel<String>());

        // Panel con el contenido
        final JPanel panelCentral = new JPanel();
        final GridBagLayout gbLayout = new GridBagLayout();
        panelCentral.setBackground(Color.WHITE);
        panelCentral.setLayout(gbLayout);
        panelCentral.setBorder(BorderFactory.createEmptyBorder());
        panelCentral.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.0") //$NON-NLS-1$
    	);

        // Etiqueta con el texto "Remitente..."
        final JLabel label = new JLabel(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.0"), //$NON-NLS-1$
    		SwingConstants.CENTER
        );
        label.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$

        // Eleccion del remitente
        final JLabel labelCombo = new JLabel(SimpleAfirmaMessages.getString("DigitalEnvelopeSender.12")); //$NON-NLS-1$
        labelCombo.setLabelFor(this.comboBox);
        this.comboBox.setModel(new DefaultComboBoxModel<>(EnvelopesUtils.getKeyStoresToSign()));
 		this.comboBox.setSelectedItem(
 			SimpleAfirmaMessages.getString("DigitalEnvelopeSender.13") //$NON-NLS-1$
 		);
 		this.comboBox.setEnabled(true);

 		// Boton de anadir
		this.addButton.setMnemonic('A');
		this.addButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopeSender.20") //$NON-NLS-1$
		);
		this.addButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					addSender(
						getComboBox(),
						getRemoveButton(),
						(DefaultListModel<String>) getSendersList().getModel()
					);
				}
			}
		);
		this.addButton.setEnabled(true);

		// Area de texto con el remitente
		final JLabel labelText = new JLabel(SimpleAfirmaMessages.getString("DigitalEnvelopeSender.18")); //$NON-NLS-1$
		labelText.setLabelFor(this.scrollPane);
		this.scrollPane.setBorder(BorderFactory.createLineBorder(Color.black));
        this.scrollPane.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$

        // Verificamos que el text field no este vacio
 		if (this.senderList.getModel().getSize() > 0) {
 			this.removeButton.setEnabled(true);
 		}
 		else {
 			this.removeButton.setEnabled(false);
 		}

        // Boton de eliminar remitente
		this.removeButton.setMnemonic('E');
		this.removeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopeSender.21") //$NON-NLS-1$
		);
		this.removeButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					removeSender((DefaultListModel<String>) getSendersList().getModel());
				}
			}
		);

		 // Boton de siguiente
 		this.nextButton.setMnemonic('S');
 		this.nextButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.17") //$NON-NLS-1$
		);
 		this.nextButton.addActionListener(
 			new ActionListener() {
 				/** {@inheritDoc} */
 				@Override
 				public void actionPerformed(final ActionEvent ae) {
 					if (createEnvelope()) {
 						getDialog().remove(panelCentral);
 						getDialog().remove(getPanel());
	 					getBackButton().setEnabled(true);
	 					new DigitalEnvelopeEnd(getDialog());
 					}
 				}
 			}
 		);

 	// Boton cancelar
		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.18") //$NON-NLS-1$
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

		// Boton de volver
		this.backButton.setMnemonic('A');
		this.backButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.19") //$NON-NLS-1$
		);
		this.backButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				getDialog().remove(panelCentral);
				getDialog().remove(DigitalEnvelopeSender.this.panel);
				new DigitalEnvelopeRecipients(getDialog(), getFilePath(), getEnvelopeType());
			}
		});
		this.nextButton.setEnabled(true);
		this.cancelButton.setEnabled(true);
		this.backButton.setEnabled(true);

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
		panelCentral.add(label, c);
		c.gridy++;
		panelCentral.add(labelCombo, c);
		c.gridwidth = 2;
		c.insets = new Insets(0, 10, 0, 20);
		c.weightx = 0.0;
		c.gridy++;
		c.fill = GridBagConstraints.NONE;
		panelCentral.add(this.comboBox, c);
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.anchor = GridBagConstraints.LINE_END;
		panelCentral.add(this.addButton, c);
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 10, 0, 20);
		c.gridx = 0;
		c.weightx = 1.0;
		c.gridy++;
		panelCentral.add(labelText, c);
		c.insets = new Insets(0, 10, 0, 20);
		c.ipady = 120;
		c.gridy++;
		panelCentral.add(this.scrollPane, c);
		c.insets = new Insets(20, 10, 20, 20);
		c.ipady = 0;
		c.weightx = 0.0;
		c.gridx = 1;
		c.gridy++;
		c.fill = GridBagConstraints.NONE;
		panelCentral.add(this.removeButton, c);
		c.weighty = 1.0;
		c.gridy++;
		panelCentral.add(emptyPanel, c);

		this.dialog.getContentPane().add(panelCentral);
		this.dialog.getContentPane().add(this.panel, BorderLayout.PAGE_END);
		this.dialog.revalidate();
        this.dialog.repaint();
	}

    /** A&ntilde;ade un destinatario del origen seleccionado en el combo
     * @param comboDestinatarios
     * @param eliminar
     * @param label
     */
    void addSender(final JComboBox<KeyStoreConfiguration> comboRecipients, final JButton remove, final DefaultListModel<String> modelList) {

    	String[] filter;
    	AOKeyStoreManager keyStoreManager = null;
        final KeyStoreConfiguration kc = (KeyStoreConfiguration) comboRecipients.getSelectedItem();
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
                final File keystorePath = EnvelopesUtils.addFileSelected(filter, comboRecipients, getIcon(), getDialog());
                if (keystorePath == null) {
                    throw new AOCancelledOperationException();
                }
                lib = keystorePath.getAbsolutePath();
            }
            else if (ao == AOKeyStore.PKCS11) {
                filter = new String[] {"dll", "so"};  //$NON-NLS-1$//$NON-NLS-2$
                final File keystorePath = EnvelopesUtils.addFileSelected(filter, comboRecipients, getIcon(), getDialog());
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
			Logger.getLogger("es.gob.afirma").log( //$NON-NLS-1$
				Level.SEVERE, "Error generando o guardando la huella digital", e//$NON-NLS-1$
			);
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
			Logger.getLogger("es.gob.afirma").log( //$NON-NLS-1$
				Level.SEVERE, "Error generando o guardando la huella digital", e//$NON-NLS-1$
			);
	        return;
	    }

	    final CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this.dialog);

	    // Comprobamos que el certificado es correcto
	    if (certDest.getAlias() != null && !certDest.equals("")) { //$NON-NLS-1$
	        boolean copiar = true;
	        for (int i = 0; i < modelList.getSize(); i++) {
	            if (certDest.getAlias().equals(modelList.getElementAt(i))) {
	                copiar = false;
	            }
	        }
	        if (copiar) {
	        	modelList.addElement(certDest.getAlias());
	            this.certificateList.add(certDest);
	            remove.setEnabled(true);
	        }
	        else {
	        	 LOGGER.severe("Ya existe ese usuario"); //$NON-NLS-1$
	 	        AOUIFactory.showMessageDialog(
     				this.dialog,
     				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.20"), //$NON-NLS-1$
     				SimpleAfirmaMessages.getString("DigitalEnvelopeRecipients.25"), //$NON-NLS-1$
     				JOptionPane.WARNING_MESSAGE
     			);
	        }
	    }
	    // Preguntamos por la contrasena del certificado
        if (!this.certificateList.isEmpty()) {
            try {
                this.privateKeyEntry = keyStoreManager.getKeyEntry(certDest.getAlias());
            }
            catch (final UnrecoverableEntryException e) {
            	LOGGER.severe("Error de constasena: " + e); //$NON-NLS-1$
                // Control de la excepcion generada al introducir mal la contrasena para el certificado
                AOUIFactory.showMessageDialog(
            		this.dialog,
            		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.22"), //$NON-NLS-1$
            		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.23"),  //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
                return;
            }
            catch (final AOCancelledOperationException e) {
            	LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
            	clear();
            	return;
            } catch (final Exception e1) {
            	LOGGER.info("Error recuperando la clave privada: " + e1); //$NON-NLS-1$
            	clear();
            	return;
            }
        }
        this.addButton.setEnabled(false);
    }


	/** Elimina un remitente de la lista de remitentes
     * @param comboRepositorios Combo con el repositorio / almacen
     * @param eliminar Boton para eliminar un remitente de la lista
     * @param anadir Boton para anadir un remitente a la lista */
    void removeSender(final DefaultListModel<String> listaModel) {
        for (int i = 0; i < this.certificateList.size(); i++) {
            if (this.certificateList.get(i).getAlias().equals(this.senderList.getSelectedValue())) {
                this.certificateList.remove(this.certificateList.get(i));
                listaModel.remove(this.senderList.getSelectedIndex());
                break;
            }
        }

        if (listaModel.isEmpty()) {
            clear();
        }

        // Borramos las posibles claves del certificado
        this.privateKeyEntry = null;
    }

    /**
     * Resetea los campos
     */
    private void clear() {
        this.certificateList.remove(this.certificateList.get(0));
        ((DefaultListModel<String>) this.senderList.getModel()).remove(0);
        this.removeButton.setEnabled(false);
    }

	boolean createEnvelope() {
		if (this.envelopeType == EnvelopesTypeResources.AUTHENTICATED
				|| this.envelopeType == EnvelopesTypeResources.SIGNED) {

            final DefaultListModel<String> listModel = (DefaultListModel<String>) this.senderList.getModel();
            if (listModel.isEmpty()) {
            	LOGGER.info("No anadido remitente"); //$NON-NLS-1$
	            AOUIFactory.showMessageDialog(
	        		getDialog(),
	        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.24"), //$NON-NLS-1$
	        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.25"), //$NON-NLS-1$
	                JOptionPane.ERROR_MESSAGE
	            );
                return false;
            }
        }

        final X509Certificate[] certs = new X509Certificate[this.certificateRecipientsList.size()];
        for (int i = 0; i < this.certificateRecipientsList.size(); i++) {
            certs[i] = (X509Certificate) this.certificateRecipientsList.get(i).getCertificate();
        }

        final AOCMSEnveloper enveloper = new AOCMSEnveloper();
        byte[] contentData = null;
        byte[] envelopedData = null;
        try {
        	contentData = EnvelopesUtils.readFile(this.filePath);
        }
        catch(final OutOfMemoryError e) {
        	LOGGER.info("Error de memoria: " + e); //$NON-NLS-1$
        	AOUIFactory.showMessageDialog(
        		getDialog(),
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.26"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.27"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
        } catch (final IOException e) {
        	LOGGER.info("Error leyendo el fichero: " + e); //$NON-NLS-1$
        	AOUIFactory.showMessageDialog(
        		getDialog(),
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.28"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.29"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
		}

       	try {
            if (this.envelopeType == EnvelopesTypeResources.AUTHENTICATED) {
            	envelopedData = enveloper.createCMSAuthenticatedEnvelopedData(
					contentData,
					this.privateKeyEntry,
					new AOCipherConfig(
						AOCipherAlgorithm.AES,
						null, // BlockMode (sin uso)
						null  // Padding (sin uso)
					),
					certs,
					null
				);
            }
            else if (this.envelopeType == EnvelopesTypeResources.SIGNED) {
            	envelopedData = enveloper.createCMSSignedAndEnvelopedData(
					contentData,
					this.privateKeyEntry,
					new AOCipherConfig(
						AOCipherAlgorithm.AES,
						null, // BlockMode (sin uso)
						null  // Padding (sin uso)
					),
					certs,
					null
				);
            }
            else if (this.envelopeType == EnvelopesTypeResources.SIMPLE) {
				envelopedData = enveloper.createCMSEnvelopedData(
					contentData,
					this.privateKeyEntry,
					new AOCipherConfig(
						AOCipherAlgorithm.AES,
						null, // BlockMode (sin uso)
						null  // Padding (sin uso)
					),
					certs,
					null
				);
			}
       	}
        catch (final Exception e) {
			LOGGER.severe("No se ha posido crear el sobre: " + e); //$NON-NLS-1$
			AOUIFactory.showMessageDialog(
        		getDialog(),
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.30"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.31"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
        }

       	File savedFile;
		try {
			savedFile = AOUIFactory.getSaveDataToFile(
			    envelopedData,
			    SimpleAfirmaMessages.getString("DigitalEnvelopeSender.32"), //$NON-NLS-1$
			    null,
			    new File(this.filePath).getName() + ".enveloped", //$NON-NLS-1$
			    null,
			    null,
			    this.dialog
			);
		} catch (final IOException e) {
			LOGGER.severe("No se ha posido guardar el sobre: " + e); //$NON-NLS-1$
			AOUIFactory.showMessageDialog(
        		getDialog(),
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.33"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.31"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return false;
		}
        // Si el usuario cancela el guardado de los datos, no nos desplazamos a la ultima pantalla
        if (savedFile == null) {
            return false;
        }
		return true;
	}
}
