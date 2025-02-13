package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.text.DefaultCaret;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.signdetails.CAdESSignAnalyzer;
import es.gob.afirma.standalone.signdetails.FacturaESignAnalyzer;
import es.gob.afirma.standalone.signdetails.PAdESSignAnalyzer;
import es.gob.afirma.standalone.signdetails.SignAnalyzer;
import es.gob.afirma.standalone.signdetails.SignDetailsFormatter;
import es.gob.afirma.standalone.signdetails.XAdESSignAnalyzer;

public class ValidationInfoDialog extends JDialog {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = -6677845022747809410L;

	private static final int DEFAULT_WINDOW_WIDTH = 780;
	private static final int DEFAULT_WINDOW_HEIGHT = 550;

	private JScrollPane scrollPane;
	private final JEditorPane validationInfo = new JEditorPane();
	private final List<SignValidity> generalValidation;

	public ValidationInfoDialog(final JFrame parent, final byte [] signData, final List<SignValidity> generalValidation) {
		super(parent, true);
		this.generalValidation = generalValidation;
		createUI(signData);
	}

	void createUI(final byte [] signData) {

		setTitle(SimpleAfirmaMessages.getString("ValidationInfoDialog.0")); //$NON-NLS-1$
		setIconImages(DesktopUtil.getIconImages());
		setResizable(true);
		setPreferredSize(new Dimension(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT));

		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.weighty = 0.0;
		c.gridy = 0;
		c.insets = new Insets(11,  11,  0,  11);

		final JLabel validationDescLbl = new JLabel(SimpleAfirmaMessages.getString("ValidationInfoDialog.1")); //$NON-NLS-1$
		this.add(validationDescLbl, c);

		c.insets = new Insets(5,  11,  0,  11);
		c.gridy++;
		c.weighty = 2.0;


		SignAnalyzer analyzer = null;
		try {
			analyzer = getSignAnalyzer(signData);
		} catch (final Exception e1) {
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("ValidationInfoDialog.17"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					null
					);
		}
		final String signsinfo = SignDetailsFormatter.formatToHTML(analyzer, this.generalValidation);

		this.validationInfo.setContentType("text/html"); //$NON-NLS-1$
		this.validationInfo.setText(signsinfo);
		this.validationInfo.setEditable(false);

		final DefaultCaret caret = new DefaultCaret();
	    caret.setBlinkRate(500);
	    caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
	    this.validationInfo.setCaret(caret);

		this.scrollPane = new JScrollPane(this.validationInfo);
		this.scrollPane.getVerticalScrollBar().setValue(this.scrollPane.getVerticalScrollBar().getMinimum());
		final JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
		// Colocamos los componentes en el panel
		final GridBagConstraints c1 = new GridBagConstraints();
		c1.fill = GridBagConstraints.BOTH;
		c1.weightx = 1.0;
		c1.weighty = 2.0;
		c1.gridx = 0;
		c1.gridy = 0;
		panel.add(this.scrollPane, c1);
		this.add(panel, c);

		final JButton closeDialogButton = new JButton(SimpleAfirmaMessages.getString("TrustedCertificatesDialog.28")); //$NON-NLS-1$
		closeDialogButton.addActionListener(e -> dispose());
		c.weighty = 0;
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.SOUTHEAST;
		c.insets = new Insets(11,  11,  11,  11);
        c.gridy++;
        this.add(closeDialogButton, c);

        pack();

        final Window ancestor = SwingUtilities.getWindowAncestor(this);
		setLocationRelativeTo(ancestor);
	}

	private static SignAnalyzer getSignAnalyzer(final byte[] signData) throws Exception {
		final SignAnalyzer analyzer = null;
		// Comprobamos si es un fichero PDF
		if (DataAnalizerUtil.isPDF(signData)) {
			return new PAdESSignAnalyzer(signData);
		}
		// Comprobamos si es una factura electronica
		else if (DataAnalizerUtil.isFacturae(signData)) {
			return new FacturaESignAnalyzer(signData);
		}
		// Comprobamos si es un fichero de firma CAdES o XAdES (los PDF y facturas pasaran por las
		// condiciones anteriores)
		else {
			AOSigner signer;
			try {
				signer = AOSignerFactory.getSigner(signData);
			} catch (final IOException e) {
				LOGGER.log(Level.WARNING, "Error al leer la informacion de la firma", e); //$NON-NLS-1$
				throw e;
			}
			if (signer != null) {
				if (signer instanceof AOXAdESSigner) {
					return new XAdESSignAnalyzer(signData);
				}
				return new CAdESSignAnalyzer(signData);
			}
		}

		return analyzer;
	}


}
