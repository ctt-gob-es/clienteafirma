package es.gob.afirma.plugin.certvalidation;

import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.security.cert.X509Certificate;
import java.util.concurrent.CancellationException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.SwingWorker;

import es.gob.afirma.plugin.certvalidation.validation.CertificateVerificable;
import es.gob.afirma.plugin.certvalidation.validation.CertificateVerifierFactory;
import es.gob.afirma.plugin.certvalidation.validation.ValidationResult;
import es.gob.afirma.standalone.plugins.OutputData;
import es.gob.afirma.standalone.plugins.SignatureProcessAction;

/**
 * Acci&oacute;n de validaci&oacute;n del certificado de firma utilizado en la &uacute;ltima
 * firma electr&oacute;nica.
 */
public class ValidateCertAction extends SignatureProcessAction {

	static final Logger LOGGER = Logger.getLogger(ValidateCertAction.class.getName());

	JDialog waitDialog = null;

	@Override
	public void processSignatures(final OutputData[] outputs, final X509Certificate signingCert, final Window parent) {
		if (signingCert == null) {
			JOptionPane.showMessageDialog(
					parent,
					Messages.getString("ValidateCertAction.10"), //$NON-NLS-1$
					Messages.getString("ValidateCertAction.11"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE);
			return;
		}

		this.waitDialog = new JDialog(parent, Messages.getString("ValidateCertAction.13"), ModalityType.APPLICATION_MODAL); //$NON-NLS-1$

		final SwingWorker<?, ?> validationWorker = executeCertValidation(signingCert, parent);

		showWaitDialog(parent, validationWorker);
	}

	private void showWaitDialog(final Window parent, final SwingWorker<?, ?> worker) {

		this.waitDialog.setLayout(new GridBagLayout());

		final JLabel textLabel = new JLabel(Messages.getString("ValidateCertAction.14")); //$NON-NLS-1$
		textLabel.setMinimumSize(new Dimension(250, 15));
		final JProgressBar progressBar = new JProgressBar();
		progressBar.setIndeterminate(true);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		c.insets = new Insets(11,  11,  0,  11);
		this.waitDialog.add(textLabel, c);
		c.gridy++;
		c.insets = new Insets(6,  11,  11,  11);
		this.waitDialog.add(progressBar, c);

		this.waitDialog.pack();
		this.waitDialog.setLocationRelativeTo(parent);
		this.waitDialog.setVisible(true);

		if (worker != null) {
			worker.cancel(true);
		}
	}

	void hideWaitDialog() {
		this.waitDialog.dispose();
	}

	private SwingWorker<ValidationResult, Void> executeCertValidation(final X509Certificate cert, final Window parent) {
		final SwingWorker<ValidationResult, Void> validationWorker = new SwingWorker<ValidationResult, Void>(){
			@Override
			protected ValidationResult doInBackground() throws Exception {



				// Obtenemos el validador correspondiente para el tipo de certificado que queremos validar
				CertificateVerificable cfv = null;
				try {
					cfv = CertificateVerifierFactory.getCertificateVerifier(cert);
				}
				catch(final Exception e) {
					LOGGER.log(Level.SEVERE, "No se pudo obtener un validador para el certificado", e); //$NON-NLS-1$
					hideWaitDialog();
					JOptionPane.showMessageDialog(
							parent,
							Messages.getString("ValidateCertAction.12"), //$NON-NLS-1$
							Messages.getString("ValidateCertAction.11"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE);
					return null;
				}

				// Validamos el certificado
				if (cfv == null) {
					hideWaitDialog();
					JOptionPane.showMessageDialog(
							parent,
							Messages.getString("ValidateCertAction.12"), //$NON-NLS-1$
							Messages.getString("ValidateCertAction.11"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE);
					return null;
				}

				// Si se cancelo la tarea, terminamos
				if (isCancelled()) {
					return null;
				}

				final ValidationResult vr = cfv.validateCertificate(cert);

				// Si se cancelo la tarea, terminamos
				if (isCancelled()) {
					return null;
				}
				return vr;
			}

			@Override
			protected void done() {
				hideWaitDialog();

				final ValidationResult vr;
				try {
					vr = get();
				}
				catch (final CancellationException e) {
					LOGGER.log(Level.INFO, "Tarea cancelada por el usuario: " + e); //$NON-NLS-1$
					return;
				}
				catch (final Exception e) {
					LOGGER.log(Level.SEVERE, "Tarea interrumpida", e); //$NON-NLS-1$
					return;
				}

				final String validationMessage;
				int validationMessageType = JOptionPane.ERROR_MESSAGE;
				switch(vr) {
				case VALID:
					validationMessage = Messages.getString("ValidateCertAction.0"); //$NON-NLS-1$
					validationMessageType = JOptionPane.INFORMATION_MESSAGE;
					break;
				case EXPIRED:
					validationMessage = Messages.getString("ValidateCertAction.1"); //$NON-NLS-1$
					break;
				case REVOKED:
					validationMessage = Messages.getString("ValidateCertAction.2"); //$NON-NLS-1$
					break;
				case NOT_YET_VALID:
					validationMessage = Messages.getString("ValidateCertAction.3"); //$NON-NLS-1$
					break;
				case CA_NOT_SUPPORTED:
					validationMessage = Messages.getString("ValidateCertAction.4"); //$NON-NLS-1$
					break;
				case CORRUPT:
					validationMessage = Messages.getString("ValidateCertAction.5"); //$NON-NLS-1$
					break;
				case SERVER_ERROR:
					validationMessage = Messages.getString("ValidateCertAction.6"); //$NON-NLS-1$
					break;
				case UNKNOWN:
					validationMessage = Messages.getString("ValidateCertAction.7"); //$NON-NLS-1$
					break;
				default:
					validationMessage = Messages.getString("ValidateCertAction.8");  //$NON-NLS-1$
				}
				JOptionPane.showMessageDialog(
					parent,
					validationMessage,
					Messages.getString("ValidateCertAction.9"),  //$NON-NLS-1$
					validationMessageType
				);
			}
		};

		validationWorker.execute();

		return validationWorker;
	}
}
