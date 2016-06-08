package es.gob.afirma.standalone.ui.pdf;

import java.awt.Container;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

/** Di&aacute;logo de selecci&oacute;n de campo de firma PDF visible.
 * @author Mariano Mart&iacute;nez. */
public final class PdfEmptySignatureFieldsChooserDialog extends JDialog implements SignPdfUiPanelListener{

	private static final long serialVersionUID = 4904673758490090781L;

	private static final int PREFERRED_WIDTH = 500;
	private static final int PREFERRED_HEIGHT = 615;

	private final SignPdfDialogListener listener;
	SignPdfDialogListener getListener() {
		return this.listener;
	}

	private final byte[] pdf;

	private final SignatureField field;
	SignatureField getField() {
		return this.field;
	}

	/** Inicia el proceso de creaci&oacute;n de di&aacute;logo de selecci&oacute;n del campo de firma PDF visible.
	 * @param pdf PDF de entrada.
	 * @param parent Componente padre para la modalidad.
	 * @param field Campos de firma seleccionado.
	 * @param signPdfDialogListener Clase a la que hay que notificar la selecci&oacute;n de propiedades de firma visible PDF. */
	public static void startPdfEmptySignatureFieldsChooserDialog(final byte[] pdf,
																 final Frame parent,
																 final SignatureField field,
																 final SignPdfDialogListener signPdfDialogListener) {

		final PdfEmptySignatureFieldsChooserDialog tsd = new PdfEmptySignatureFieldsChooserDialog(
			pdf,
			parent,
			field,
			signPdfDialogListener
		);
		tsd.setSize(PREFERRED_WIDTH, PREFERRED_HEIGHT);
		tsd.setResizable(false);
		tsd.setLocationRelativeTo(parent);
	}

	private PdfEmptySignatureFieldsChooserDialog(final byte[] pdf,
												 final Frame parentFrame,
												 final SignatureField fld,
												 final SignPdfDialogListener spul) {
		super(parentFrame);
		this.listener = spul;
		this.pdf = pdf;
		this.field = fld;
		setTitle(SignPdfUiMessages.getString("SignPdfDialog.3")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);

		SwingUtilities.invokeLater(
			() -> createUI()
		);
	}

	void createUI() {
		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;

        setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfFieldChooser.1") //$NON-NLS-1$
		);

		addWindowListener(
			new java.awt.event.WindowAdapter() {
			    @Override
			    public void windowClosing(final java.awt.event.WindowEvent windowEvent) {
			    	positionCancelled();
			    }
			}
		);

		try {
			final Properties p = new Properties();
			p.put("signatureField", getField().getName()); //$NON-NLS-1$
			p.put("signaturePage", Integer.toString(getField().getPage())); //$NON-NLS-1$
			p.put("signaturePositionOnPageLowerLeftX", Integer.toString(getField().getSignaturePositionOnPageLowerLeftX())); //$NON-NLS-1$
			p.put("signaturePositionOnPageLowerLeftY", Integer.toString(getField().getSignaturePositionOnPageLowerLeftY())); //$NON-NLS-1$
			p.put("signaturePositionOnPageUpperRightX", Integer.toString(getField().getSignaturePositionOnPageUpperRightX())); //$NON-NLS-1$
			p.put("signaturePositionOnPageUpperRightY", Integer.toString(getField().getSignaturePositionOnPageUpperRightY())); //$NON-NLS-1$
			nextPanel(p, getFragmentImage(getField()));
		}
		catch (final Exception e1) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No ha sido posible cargar la previsualizacion del acrocampo: " + e1//$NON-NLS-1$
			);
			AOUIFactory.showErrorMessage(
                  this,
                  SignPdfUiMessages.getString("SignPdfFieldChooser.2"), //$NON-NLS-1$
                  SignPdfUiMessages.getString("SignPdfFieldChooser.3"), //$NON-NLS-1$
                  JOptionPane.ERROR_MESSAGE
            );
			setVisible(false);
			getListener().propertiesCreated(new Properties());
			dispose();
		}
	}

	BufferedImage getFragmentImage(final SignatureField sf) throws IOException {

		final List<BufferedImage> pages = Pdf2ImagesConverter.pdf2Images(this.pdf);
		final BufferedImage page = pages.get(sf.getPage() - 1);

		final int uxr = sf.getSignaturePositionOnPageUpperRightX();
		final int uyr = sf.getSignaturePositionOnPageUpperRightY();
		final int lxl = sf.getSignaturePositionOnPageLowerLeftX();
		final int lyl = sf.getSignaturePositionOnPageLowerLeftY();

		final BufferedImage imSign = page.getSubimage(lxl, uyr, uxr - lxl, uyr - lyl);

		return imSign;
	}

	void nextPanel(final Properties p, final BufferedImage im) throws IOException {
		if (im != null) {
			getContentPane().removeAll();
			final GridBagConstraints constraints = new GridBagConstraints();
			constraints.fill = GridBagConstraints.BOTH;
			constraints.weightx = 1.0;
			constraints.weighty = 1.0;
			constraints.insets = new Insets(0, 0, 0, 0);
			getContentPane().add(new SignPdfUiPanelPreview(this, p, im), constraints);
			setVisible(true);
		}
		else {
			throw new IOException("Error creando la imagen para previsualizar"); //$NON-NLS-1$
		}
	}

	@Override
	public void positionSelected(final Properties extraParams) {
		setVisible(false);
		this.listener.propertiesCreated(extraParams);
		dispose();
	}

	@Override
	public void positionCancelled() {
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	/** Muestra un di&aacute;logo gr&aacute;fico para la selecci&oacute;n de un campo de firma en un PDF.
	 * @param emptySignatureFields Listado de campos de firma del PDF.
	 * @return Campo de firma seleccionado. */
	public static SignatureField selectField(final List<SignatureField> emptySignatureFields) {

		final JComboBox<Object> combo = new JComboBox<>(emptySignatureFields.toArray());

		final String[] options = {
			SignPdfUiMessages.getString("SignPdfFieldChooser.4"), //$NON-NLS-1$
			SignPdfUiMessages.getString("SignPdfFieldChooser.6"), //$NON-NLS-1$
			SignPdfUiMessages.getString("SignPdfFieldChooser.5") //$NON-NLS-1$
		};

		final String title = SimpleAfirmaMessages.getString("SignPanelSignTask.1"); //$NON-NLS-1$
		final int selection = JOptionPane.showOptionDialog(
			null,
			combo,
			title,
			JOptionPane.DEFAULT_OPTION,
			JOptionPane.QUESTION_MESSAGE,
			null,
			options,
			options[0]
		);

		if (selection == 0) {
			return (SignatureField) combo.getSelectedItem();
		}
		else if (selection == 1) {
			return new SignatureField(0, 0, 0, 0, 0, "Create"); //$NON-NLS-1$
		}
		return null;
	}
}
