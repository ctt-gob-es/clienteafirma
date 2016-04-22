package es.gob.afirma.standalone.ui.envelopes;

import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * @author Mariano Mart&iacute;nez
 * Di&aacute;logo para abrir sobres digitales.
 */
public class OpenDigitalEnvelopeDialog extends JDialog implements KeyListener{

	private static final long serialVersionUID = -5949140119173965513L;
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final int PREFERRED_WIDTH = 600;
	private static final int PREFERRED_HEIGHT = 200;

	private final SimpleAfirma saf;

	private final JTextField selectedFilePath = new JTextField();
	void setSelectedFilePath(final String path) {
		this.selectedFilePath.setText(path);
	}

	private final JButton examineFileButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.2")); //$NON-NLS-1$
	private final JButton openButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.3")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.4")); //$NON-NLS-1$

	/**
	 * Crea el di&aacute;logo y lo hace visible.
	 * @param parent Frame padre del di&aacute;logo.
	 * @param sa Instancia de SimpleAfirma para utilizar el almac&eacute;n de la aplicaci&oacute;n.
	 */
	public static void startOpenDigitalEnvelopeDialog(final Frame parent, final SimpleAfirma sa) {
		final OpenDigitalEnvelopeDialog ode = new OpenDigitalEnvelopeDialog(parent, sa);
		ode.setSize(PREFERRED_WIDTH, PREFERRED_HEIGHT);
		ode.setResizable(false);
		ode.setLocationRelativeTo(parent);
		ode.setVisible(true);
	}

	/** Crea el panel de apertura de un sobre digital.
	 * @param parent Componente padre del di&aacute;logo.
	 * @param sa Instancia de SimpleAfirma para utilizar el almac&eacute;n de la aplicaci&oacute;n.
	 **/
	public OpenDigitalEnvelopeDialog(final Frame parent, final SimpleAfirma sa) {
		super(parent);
		this.saf =  sa;
		createUI();
	}

	public void createUI() {

		setTitle(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.0")); //$NON-NLS-1$

		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.2") //$NON-NLS-1$
		);

		// Icono de la ventana
		setIconImage(AutoFirmaUtil.getDefaultDialogsIcon());

		final JLabel infoLabel = new JLabel(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.6") //$NON-NLS-1$
		);

		// Eleccion fichero a desensobrar
		final JLabel envelopeFilesLabel = new JLabel(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.5") //$NON-NLS-1$
		);
		envelopeFilesLabel.setLabelFor(this.selectedFilePath);
		this.selectedFilePath.setEditable(false);
		this.selectedFilePath.setFocusable(false);

		// Boton de examinar
		this.examineFileButton.setMnemonic('X');
		this.examineFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.7") //$NON-NLS-1$
		);
		this.examineFileButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					final File envFile;
					try {
						envFile = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString("OpenDigitalEnvelope.11"), //$NON-NLS-1$
							null,
							null,
							new String[] {SimpleAfirmaMessages.getString("OpenDigitalEnvelope.8")}, //$NON-NLS-1$
							SimpleAfirmaMessages.getString("OpenDigitalEnvelope.8"), //$NON-NLS-1$
							false,
							false,
							null,
							OpenDigitalEnvelopeDialog.this
						)[0];
					}
					catch (final AOCancelledOperationException e) {
						LOGGER.warning(
							"Operacion cancelada por el usuario: " + e //$NON-NLS-1$
						);
						return;
					}
					if (!envFile.canRead()) {
						LOGGER.warning(
							"No ha podido cargarse el fichero para envolver: " //$NON-NLS-1$
						);
						AOUIFactory.showErrorMessage(
							OpenDigitalEnvelopeDialog.this,
							SimpleAfirmaMessages.getString("OpenDigitalEnvelope.12"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("OpenDigitalEnvelope.13"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}
					setSelectedFilePath(envFile.getAbsolutePath());
					enableOpenbutton();
				}
			}
		);
		this.examineFileButton.setEnabled(true);
		this.examineFileButton.addKeyListener(this);

		// Boton de examinar
		this.openButton.setMnemonic('A');
		this.openButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.9") //$NON-NLS-1$
		);
		this.openButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent arg0) {
					if (open()) {
						setVisible(false);
						dispose();
					}
				}
			}
		);
		this.openButton.addKeyListener(this);

		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.10") //$NON-NLS-1$
		);
		this.cancelButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					setVisible(false);
					dispose();
				}
			}
		);
		this.cancelButton.addKeyListener(this);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(this.cancelButton);
			panel.add(this.openButton);
		}
		else {
			panel.add(this.openButton);
			panel.add(this.cancelButton);
		}

		setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;
        add(infoLabel, c);
        c.gridy++;
        c.gridy++;
        add(envelopeFilesLabel, c);
        c.gridy++;
        c.gridy++;
        c.gridwidth = 2;
        c.insets = new Insets(5, 20, 0, 25);
        add(this.selectedFilePath, c);
        c.weightx = 0.0;
        c.gridwidth = GridBagConstraints.REMAINDER;
        add(this.examineFileButton, c);
        c.insets = new Insets(20, 20, 0, 20);
        c.gridy++;
        c.weightx = 1.0;
        c.anchor = GridBagConstraints.PAGE_END;
        add(panel, c);
		enableOpenbutton();
	}

	void enableOpenbutton() {
		if (this.saf.isKeyStoreReady()
				&& !this.selectedFilePath.getText().trim().isEmpty()) {
			this.openButton.setEnabled(true);
		}
		else {
			this.openButton.setEnabled(false);
		}
	}

	/**
	 * Abre el sobre digital seleccionado si es posible.
	 * @return Devuelve <code>true</code> si se ha podido abrir el sobre correctamente, <code>false</code> en caso contrario.
	 */
	boolean open() {
		final PrivateKeyEntry pke;
        try {
            pke = getPrivateKeyEntry();
        }
        catch (final AOCancelledOperationException e) {
        	LOGGER.info("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
            return false;
        }
        catch(final AOCertificatesNotFoundException e) {
        	LOGGER.severe("El almacen no contiene ningun certificado que se pueda usar para firmar: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this,
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.14"), //$NON-NLS-1$,
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.15"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
        }
        catch (final Exception e) {
        	LOGGER.severe("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this,
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.16"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.15"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
    	}

		byte[] data = null;

        final AOCMSEnveloper enveloper = new AOCMSEnveloper();
        try {
			data = enveloper.recoverData(
				EnvelopesUtils.readFile(this.selectedFilePath.getText()),
				pke
			);
		} catch (final InvalidKeyException e1) {
			LOGGER.severe("La clave indicada no pertenece a ninguno de los destinatarios del envoltorio" + e1); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this,
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.17"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.15"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
		} catch (final Exception e1) {
			LOGGER.severe("Error desensobrando el fichero: " + e1); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this,
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.18"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("OpenDigitalEnvelope.15"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return false;
		}

		try {
			AOUIFactory.getSaveDataToFile(
			    data,
			    SimpleAfirmaMessages.getString("DigitalEnvelopeSender.32"), //$NON-NLS-1$
			    null,
			    new File(this.selectedFilePath.getText()).getName().split(".enveloped")[0], //$NON-NLS-1$
			    null,
			    null,
			    this
			);
		} catch (final IOException e) {
			LOGGER.severe("No se ha posido guardar el sobre: " + e); //$NON-NLS-1$
			AOUIFactory.showMessageDialog(
        		this,
        		SimpleAfirmaMessages.getString("OpenDigitalEnvelope.19"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("OpenDigitalEnvelope.15"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
		}
		return true;
	}

	private PrivateKeyEntry getPrivateKeyEntry() throws AOCertificatesNotFoundException, KeyStoreException, NoSuchAlgorithmException, UnrecoverableEntryException {
		final AOKeyStoreManager ksm = this.saf.getAOKeyStoreManager();
    	final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
			ksm,
			this,
			true,             // Comprobar claves privadas
			false,            // Mostrar certificados caducados
			true,             // Comprobar validez temporal del certificado
			null, 				// Filtros
			false             // mandatoryCertificate
		);
    	dialog.show();
    	ksm.setParentComponent(this);
    	return ksm.getKeyEntry(
			dialog.getSelectedAlias()
		);
	}

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			this.setVisible(false);
			dispose();
		}
	}
}