package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class DocumentInfoPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 1627010163673368615L;

	private String accesibleDescription;

	public DocumentInfoPanel(final SignOperationConfig signConfig, final Color bgColor) {
		createUI(signConfig, bgColor);
	}

	private void createUI(final SignOperationConfig signConfig, final Color bgColor) {

		if (!LookAndFeelManager.HIGH_CONTRAST) {
			setBackground(bgColor);
		}

		// Tipo de documento
		final JLabel descLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.46", signConfig.getFileType().getFileDescription())); //$NON-NLS-1$
		this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.46", signConfig.getFileType().getFileDescription()); //$NON-NLS-1$

		// Fecha de modificacion
		final Date fileLastModified = new Date(signConfig.getDataFile().lastModified());
		final JLabel dateLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.47",  //$NON-NLS-1$
        				DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT)
        				.format(fileLastModified))
        		);
		this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.47",  //$NON-NLS-1$
				DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT)
				.format(fileLastModified));

		// Tamano
		final String fileSize = NumberFormat.getNumberInstance().format(signConfig.getDataFile().length() / 1024);
        final JLabel sizeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
        this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        // Validez de la firma (si era una firma)
        final String invalidSignatureText = signConfig.getInvalidSignatureText();
        JLabel invalidSignatureErrorLabel = null;
        if (invalidSignatureText != null) {
        	final BufferedImage errorIcon;
        	final SignValidity validity = signConfig.getSignValidity();
    		if (validity.getValidity() == SignValidity.SIGN_DETAIL_TYPE.UNKNOWN) {
        		errorIcon = ImageLoader.loadImage("unknown_icon.png"); //$NON-NLS-1$
        	}
        	else {
        		errorIcon = ImageLoader.loadImage("ko_icon.png"); //$NON-NLS-1$
        	}
        	invalidSignatureErrorLabel = new JLabel(invalidSignatureText, new ImageIcon(errorIcon), SwingConstants.LEFT);
        	this.accesibleDescription += invalidSignatureText;
        	invalidSignatureErrorLabel.setForeground(Color.RED);
        }

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        add(Box.createRigidArea(new Dimension(0, 4)));
		add(descLabel);
		add(Box.createRigidArea(new Dimension(0, 4)));
		add(dateLabel);
		add(Box.createRigidArea(new Dimension(0, 4)));
		add(sizeLabel);
		if (invalidSignatureErrorLabel != null) {
			add(Box.createRigidArea(new Dimension(0, 4)));
			add(invalidSignatureErrorLabel);
		}
	}

	public String getAccesibleDescription() {
		return this.accesibleDescription;
	}
}
