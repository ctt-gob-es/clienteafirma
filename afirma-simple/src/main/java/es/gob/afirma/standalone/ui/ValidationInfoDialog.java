package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.signdetails.SignAnalyzer;
import es.gob.afirma.standalone.signdetails.SignDetailsFormatter;
import es.gob.afirma.standalone.signdetails.XAdESSignAnalyzer;

public class ValidationInfoDialog extends JDialog {

	private static final long serialVersionUID = -6677845022747809410L;

	private static final int DEFAULT_WINDOW_WIDTH = 780;
	private static final int DEFAULT_WINDOW_HEIGHT = 550;

	private JScrollPane scrollPane;
	private final JEditorPane errorsInfo = new JEditorPane();
	private final SignValidity generalValidation;

	public ValidationInfoDialog(final JFrame parent, final byte [] signData, final SignValidity generalValidation) {
		super(parent, true);
		this.generalValidation = generalValidation;
		createUI(signData);
	}

	void createUI(final byte [] signData) {

		setTitle(SimpleAfirmaMessages.getString("ValidationInfoDialog.0")); //$NON-NLS-1$
		setIconImages(AutoFirmaUtil.getIconImages());
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
			analyzer = new XAdESSignAnalyzer(signData);
		} catch (final Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		final String signsinfo = SignDetailsFormatter.formatToHTML(analyzer, this.generalValidation);

		this.errorsInfo.setContentType("text/html"); //$NON-NLS-1$
		this.errorsInfo.setText(signsinfo);
		this.errorsInfo.setEditable(false);
		this.scrollPane = new JScrollPane(this.errorsInfo);
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


}
