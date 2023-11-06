package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.FileInputStream;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class ValidationInfoPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 1627010163673368615L;

	private String accesibleDescription;
	final JLabel signsDetailsLbl = new JLabel();

	public ValidationInfoPanel(final SignOperationConfig signConfig, final Color bgColor) {
		createUI(signConfig, bgColor);
	}

	private void createUI(final SignOperationConfig signConfig, final Color bgColor) {

		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
			setBackground(bgColor);
		}

        // Validez de la firma (si era una firma)
        final String validitySignatureText = signConfig.getInvalidSignatureText();
        JLabel validitySignatureLabel = null;
        final List<SignValidity> validityList = signConfig.getSignValidity();
        if (validitySignatureText != null) {
        	final BufferedImage errorIcon;
    		if (validityList.get(0).getValidity() == SignValidity.SIGN_DETAIL_TYPE.UNKNOWN
    				|| validityList.get(0).getValidity() == SignValidity.SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER) {
        		errorIcon = ImageLoader.loadImage("unknown_icon.png"); //$NON-NLS-1$
        	}
        	else {
        		errorIcon = ImageLoader.loadImage("ko_icon.png"); //$NON-NLS-1$
        	}
    		validitySignatureLabel = new JLabel(validitySignatureText, new ImageIcon(errorIcon), SwingConstants.LEFT);
        	this.accesibleDescription += validitySignatureText;
        	validitySignatureLabel.setForeground(Color.RED);
        } else if (validityList.get(0).getValidity() == SignValidity.SIGN_DETAIL_TYPE.OK) {
        	final BufferedImage okIcon;
        	okIcon = ImageLoader.loadImage("ok_icon.png"); //$NON-NLS-1$
        	validitySignatureLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.156"), new ImageIcon(okIcon), SwingConstants.LEFT); //$NON-NLS-1$
        	this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.156"); //$NON-NLS-1$
        }

       	this.signsDetailsLbl.setText(SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
    	// Este gestor se encargara de controlar los eventos de foco y raton
        final LabelLinkManager labelLinkManager = new LabelLinkManager(this.signsDetailsLbl);
        byte [] signData = null;
		try (FileInputStream fl = new FileInputStream(signConfig.getDataFile())) {
			signData = new byte[(int)signConfig.getDataFile().length()];
			fl.read(signData);
		} catch (final Exception e) {
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("ValidationInfoDialog.5"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					null
					);
		}

        labelLinkManager.setLabelLinkListener(new ValidationErrorsLabelLinkImpl(
        		signData, validityList
        ));
        this.signsDetailsLbl.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
        		+ SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		add(Box.createRigidArea(new Dimension(0, 4)));
		add(validitySignatureLabel);
		add(Box.createRigidArea(new Dimension(0, 4)));
		add(this.signsDetailsLbl);
	}

	public String getAccesibleDescription() {
		return this.accesibleDescription;
	}
}
