package es.gob.afirma.crypto.handwritten;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.crypto.handwritten.net.DownloadListener;
import es.gob.afirma.crypto.handwritten.net.Downloader;
import es.gob.afirma.crypto.handwritten.pdf.PdfBuilder;

/** Ejecutor de procesos de firma biom&eacute;trica
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BioSignerRunner implements SignaturePadListener {

	private static final String ERROR_ICON = "/error.png"; //$NON-NLS-1$
	private static final String CHECK_ICON = "/check.png"; //$NON-NLS-1$
	private static final String SIGN_ICON = "/sign.png"; //$NON-NLS-1$
	private static final String LOCK_ICON = "/lock.png"; //$NON-NLS-1$

	// Opciones para el dialogo de cancelacion
	private static int REPEAT_SIGN = 0;
	private static int REPEAT_ALL_PROCESS = 1;
	private static int CANCEL_ALL_PROCESS = 2;

	private static final int BUTTON_BOX_GAP = 10;
	private static final int BUTTON_PANEL_BORDER = 10;
	private static final int BUTTON_HEIGHT = 75;
	private static final int BUTTON_WIDTH = 500;


	private static final int MIN_WIDTH = 300;
	private static final int MIN_HEIGHT_CONSTANT = 90;

	private final SignTask signTask;
	SignTask getSignTask() {
		return this.signTask;
	}

	private int signCount;
	int decreaseSignCount() {
		this.signCount = this.signCount -1;
		return this.signCount;
	}

	private JButton opSignButton;
	void enableOpSignButton() {
		if (this.opSignButton != null) {
			this.opSignButton.setEnabled(true);
		}
	}
	void setOpSignButton(final JButton b) {
		this.opSignButton = b;
	}

	/** Asociaci&oacute;n de los identificadores de firma con su posici&oacute;n en la
	 * lista de firmas. */
	private final Map<String, Integer> signIdAssociation = new ConcurrentHashMap<String, Integer>();
	Map<String, Integer> getAssociationMap () {
		return this.signIdAssociation;
	}

	/** Resultados de las firmas que se van haciendo. */
	private final Map<String, SignatureResult> sigResults = new ConcurrentHashMap<String, SignatureResult>();

	private final List<String> signIdList = new ArrayList<String>();

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JFrame frame = new JFrame();
	Frame getMainFrame() {
		return this.frame;
	}

	private final List<JButton> buttonList = new ArrayList<JButton>();
	List<JButton> getButtonList() {
		return this.buttonList;
	}

	/** Crea el ejecutor de procesos de firma biom&eacute;trica.
	 * @param xml Contiene los datos de la tarea de firma.
	 * @throws IOException Si hay errores en el tratamiento de datos. */
	public BioSignerRunner(final String xml) throws IOException {

		if (xml == null) {
			throw new IllegalArgumentException("El XML de entrada no puede ser nulo"); //$NON-NLS-1$
		}
		try {
			this.signTask = SignTask.getInstance(xml);
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"El XML no se ha podido tratar, es probable que no sea correcto: " + e, //$NON-NLS-1$
				e
			);
		}
		this.signCount = getSignTask().getBioSigns().size();

		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI(getSignTask());
				}
			}
		);
	}

	private JButton createButton(final int ordinal, final String img) {

		final JButton btn = new JButton();
		this.buttonList.add(btn);

		final String signerData;


		if(getSignTask().getBioSigns().size() < ordinal) {
			signerData = HandwrittenMessages.getString("BioSignerRunner.20"); //$NON-NLS-1$
			btn.setEnabled(false);
			setOpSignButton(btn);
		}
		else {
			signerData = getSignTask().getBioSigns().get(ordinal-1).getSignerData().toString();
		}

		btn.setText(buttonTxt(signerData, ordinal, img));

		btn.getAccessibleContext().setAccessibleDescription(
			HandwrittenMessages.getString("BioSignerRunner.0") + signerData//$NON-NLS-1$
		);
		btn.setPreferredSize(new Dimension(BUTTON_WIDTH, BUTTON_HEIGHT));

		btn.setMnemonic(Integer.toString(ordinal).toCharArray()[0]);
		btn.setHorizontalAlignment(SwingConstants.LEFT);

		btn.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					final JButton b = (JButton) ae.getSource();
					b.setEnabled(false);
					try {
						launchSign(ordinal-1);
					}
					catch(final Exception e) {
						b.setText(buttonTxt(signerData, ordinal, ERROR_ICON));
						//TODO: Mostrar dialog de error
						return;
					}
				}
			}
		);

		return btn;
	}


	static String buttonTxt(final String signerData, final int ordinal, final String img) {
		return 	"<html>" + //$NON-NLS-1$
				"<table style=\"width:100%\">" + //$NON-NLS-1$
				"<tr>" + //$NON-NLS-1$
					"<td>" + //$NON-NLS-1$
						"<img src=" + //$NON-NLS-1$
							new ImageIcon(BioSignerRunner.class.getResource(img)) +
						">" + //$NON-NLS-1$
					"</td>" + //$NON-NLS-1$
					"<td>" + //$NON-NLS-1$
					"<b style=\"text-align: left\">" + //$NON-NLS-1$
						"<u>" + //$NON-NLS-1$
							Integer.toString(ordinal) +
						"</u>" + //$NON-NLS-1$
						". " + signerData + //$NON-NLS-1$
					"</b>" + //$NON-NLS-1$
				"</td>" + //$NON-NLS-1$
				"</tr>" + //$NON-NLS-1$
			"</table>"; //$NON-NLS-1$
	}


	void launchSign(final int ordinal) {

		SingleBioSignData sign = null;

		// Asociamos el identificador de firma al ordinal
		if(ordinal >= getSignTask().getBioSigns().size()) {
			//Firma del funcionario
			// TODO: firmar con el cliente
			LOGGER.info("Firma del funcionario"); //$NON-NLS-1$
			//manageSignatures();
		}
		else {
			sign = getSignTask().getBioSigns().get(ordinal);
			this.signIdAssociation.put(
				sign.getId(),
				Integer.valueOf(ordinal)
			);
		}

		// Captura desde tableta
		if (sign != null) {

			// Abrimos la pantalla de firma en la tableta
			try{
				LOGGER.info("Abrimos la pantalla de firma"); //$NON-NLS-1$
				final BioSigner bs = new BioSigner();
				// Si hay plantilla HTML
				if (sign.getHtmlTemplate() != null) {
					bs.sign(
						getMainFrame(),
						sign.getId(),
						BioSignerRunner.this,
						sign.getHtmlTemplate(),
						sign.getSignatureArea()
					);
				}
				// Si no es imagen o null
				else {
					bs.sign(
						getMainFrame(),
						sign.getId(),
						BioSignerRunner.this,
						sign.getJpegTemplate(),
						sign.getSignatureArea()
					);
				}

			}
			catch (final Exception ex) {
				abortTask(null, ex);
			}
			//TODO: Guardar resultado de la firma
		}
		else {
			// Firma del funcionario
		}

	}

	// Crea el panel de botones de firma
	void createUI(final SignTask st) {

		this.frame.setTitle(HandwrittenMessages.getString("BioSignerRunner.19")); //$NON-NLS-1$
		this.frame.getAccessibleContext().setAccessibleDescription(
			HandwrittenMessages.getString("BioSignerRunner.1") //$NON-NLS-1$
		);

		try {
			this.frame.setIconImage(ImageIO.read(BioSignerRunner.class.getResource("/logo_cliente_96x96.png"))); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No ha sido posible establecer el icono del dialogo: " + e //$NON-NLS-1$
			);
		}
		this.frame.setLayout(new GridLayout());

		// Creamos el panel donde incluiremos los botones
		final JPanel buttonPanel = new JPanel();
		buttonPanel.getAccessibleContext().setAccessibleDescription(
			HandwrittenMessages.getString("BioSignerRunner.2") //$NON-NLS-1$
		);
		// Establecemos el layout para colocar los botones
		buttonPanel.setLayout(new GridLayout(0, 1, BUTTON_BOX_GAP, BUTTON_BOX_GAP));

		buttonPanel.setBorder(
			BorderFactory.createEmptyBorder(
				BUTTON_PANEL_BORDER,
				BUTTON_PANEL_BORDER,
				BUTTON_PANEL_BORDER,
				BUTTON_PANEL_BORDER
			)
		);

		// Anadimos los botones
		final List<SingleBioSignData> signs = st.getBioSigns();
		for (int i=1; i<=signs.size(); i++) {
			buttonPanel.add(createButton(i, SIGN_ICON));
		}

		if(st.isCompleteWithCriptoSign()) {
			// Se necesita firma del funcionario
			// Anadimos boton de firma del funcionario
			buttonPanel.add(createButton(signs.size() + 1, LOCK_ICON));
		}

		this.frame.setMinimumSize(new Dimension(MIN_WIDTH, MIN_HEIGHT_CONSTANT * signs.size()));
		buttonPanel.setMaximumSize(new Dimension(800, 120 * signs.size()));

		this.frame.add(buttonPanel);
		this.frame.pack();
		this.frame.setLocationRelativeTo(null);
	}

	/** Muestra la ventana con las tareas de firma. */
	public void show() {
		this.frame.setVisible(true);
	}

	private void abortTask(final String signatureId, final Throwable t) {

		LOGGER.warning("El proceso de firma ha sido abortado: " + t); //$NON-NLS-1$

		String errorMsg;
		if(t instanceof SignaturePadConnectionException) {
			errorMsg = HandwrittenMessages.getString("BioSignerRunner.3"); //$NON-NLS-1$
		}
		else if (signatureId == null) {
			errorMsg = HandwrittenMessages.getString("BioSignerRunner.11"); //$NON-NLS-1$
		}
		else {
			final SignerInfoBean signer = getSignTask().getBioSigns().get(
				getAssociationMap().get(signatureId).intValue()
			).getSignerData();

			final String signerName = signer.getSignerName();
			errorMsg = HandwrittenMessages.getString("BioSignerRunner.21", signerName); //$NON-NLS-1$
		}

		AOUIFactory.showErrorMessage(
			getMainFrame(),
			errorMsg,
			HandwrittenMessages.getString("BioSignerRunner.12"), //$NON-NLS-1$
			JOptionPane.ERROR_MESSAGE
		);

		closeApp();
	}

	private void cancelTask(final String signatureId) {

		if (signatureId == null) {
			LOGGER.warning("Firma cancelada"); //$NON-NLS-1$
			abortTask(null, null);
			return;
		}

		final int signerPosition = getAssociationMap().get(signatureId).intValue();

		final String signerName = getSignTask().getBioSigns().get(
			signerPosition
		).getSignerData().getSignerName();

		final String errorMsg = HandwrittenMessages.getString("BioSignerRunner.13", signerName); //$NON-NLS-1$

		// Variable para indicar si se muestra el dialogo con las tres opciones
		boolean showOpDlg = true;

		while(showOpDlg){

			final int n = JOptionPane.showOptionDialog(
				this.frame,
			    errorMsg,
			    HandwrittenMessages.getString("BioSignerRunner.4"), //$NON-NLS-1$
			    JOptionPane.CANCEL_OPTION,
			    JOptionPane.WARNING_MESSAGE,
			    null,
			    new Object[] {
					HandwrittenMessages.getString("BioSignerRunner.5"), //$NON-NLS-1$
		            HandwrittenMessages.getString("BioSignerRunner.6"), //$NON-NLS-1$
		            HandwrittenMessages.getString("BioSignerRunner.7") //$NON-NLS-1$
				},
			    null
		    );

			if (n == REPEAT_SIGN) {
				LOGGER.info("Repetir la firma"); //$NON-NLS-1$
				final JButton b = getButtonList().get(signerPosition);
				b.setEnabled(true);
				b.setText( buttonTxt(signerName, signerPosition + 1,SIGN_ICON));
				showOpDlg = false;
			}
			else if (n == REPEAT_ALL_PROCESS) {

				LOGGER.info("Se ha solicitado repetir todo el proceso tras una cancelacion de una firma"); //$NON-NLS-1$

				// Borrar todas las firmas ya realizadas
				this.sigResults.clear();

				// Volver a activar los botones.
				final List<SingleBioSignData> signersList = getSignTask().getBioSigns();
				for(int i = 0; i < signersList.size(); i++) {
					getButtonList().get(i).setEnabled(true);
					getButtonList().get(i).setText(
						buttonTxt(
							signersList.get(i).getSignerData().toString(),
							i + 1,
							SIGN_ICON
						)
					);
				}

				// El boton del operador debe aparecer desactivado
				if(getSignTask().isCompleteWithCriptoSign()) {
					getButtonList().get(getButtonList().size() - 1).setEnabled(false);
				}

				showOpDlg = false;
			}
			else if (n == CANCEL_ALL_PROCESS){
				LOGGER.info("Se ha solicitado cancelar todo el proceso tras una cancelacion de una firma"); //$NON-NLS-1$

				if (JOptionPane.YES_OPTION == AOUIFactory
					.showConfirmDialog(
						getMainFrame(),
						HandwrittenMessages.getString("BioSignerRunner.8"), //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.9"), //$NON-NLS-1$
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE
				)) {
					//TODO: canelar todo el proceso
					AOUIFactory.showMessageDialog(
						getMainFrame(),
						HandwrittenMessages.getString("BioSignerRunner.10"), //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.14"), //$NON-NLS-1$
						JOptionPane.OK_CANCEL_OPTION
					);
					closeApp();
					showOpDlg = false;
				}
			}
		}
	}

	private void closeApp() {
		// Borrar todas las firmas ya realizadas
		this.sigResults.clear();
		// TODO: Cerrar aplicacion
		getMainFrame().setVisible(false);
		getMainFrame().dispose();
	}

	@Override
	public void signatureCancelled(final String signatureId) {
		LOGGER.info("Firma cancelada"); //$NON-NLS-1$
		cancelTask(signatureId);
	}

	@Override
	public void signatureAborted(final Throwable e, final String signatureId) {
		LOGGER.info("Firma abortada"); //$NON-NLS-1$
		abortTask(signatureId, e);
	}

	@Override
	public void signatureFinished(final SignatureResult sr) {

		LOGGER.info("Firma realizada con exito con identificador: " + sr.getSignatureId()); //$NON-NLS-1$

		// Guardamos la firma
		this.sigResults.put(sr.getSignatureId(), sr);
		this.signIdList.add(sr.getSignatureId());

		// Deshabilitamos el boton y lo marcamos como correcto
		final int signerPosition = getAssociationMap().get(sr.getSignatureId()).intValue();
		final SignerInfoBean signer = getSignTask().getBioSigns().get(
			signerPosition
		).getSignerData();



		getButtonList().get(signerPosition).setText(
			buttonTxt(
				signer.getSignerName(),
				signerPosition + 1,
				CHECK_ICON
			)
		);

		final int count = decreaseSignCount();

		// Si se han terminado todas las firmas biometricas...
		if (count == 0) {
			try {
				generatePdf(sr.getSignatureId());
			}

			catch (final Exception e) {
				LOGGER.warning("Error generando el documento PDF firmado. " + e); //$NON-NLS-1$
				signatureAborted(e, sr.getSignatureId());
				return;
			}

			if(getSignTask().isCompleteWithCriptoSign()) {
				enableOpSignButton();
			}
		}

		// Si el contador es menor que cero quiere decir que la firma es del funcionario
		else if(count < 0) {
			// Si el fichero necesita ser firmado por un funcionario, activamos el boton de firma y abrimos el cliente @firma

			//TODO: Tramite realizado por el funcionario
			LOGGER.info("Firma del funcionario"); //$NON-NLS-1$

		}

	}

	Map<SignerInfoBean, SignatureResult> buildSrList() {

		final Map<SignerInfoBean, SignatureResult> srList = new ConcurrentHashMap<SignerInfoBean, SignatureResult>(this.sigResults.size());

		final Set<String> signResultKey = this.sigResults.keySet();

		for(final String signString : signResultKey) {
			final SignatureResult sr = this.sigResults.get(signString);
			// Posicion en la que se encuentra la informacion del firmante
			final int signerPosition = getAssociationMap().get(sr.getSignatureId()).intValue();
			srList.put(
				getSignTask().getBioSigns().get(
					signerPosition
				).getSignerData(),
				sr
			);
		}

		return srList;
	}

	private void generatePdf(final String signId) throws IOException {

		Downloader dwn = new Downloader(getMainFrame(), new DownloadListener() {

			@Override
			public void downloadError(final Throwable t) {
				System.out.println("Error"); //$NON-NLS-1$
				signatureAborted(t,signId);
			}

			@Override
			public void downloadComplete(final byte[] data) {

				try {

					byte[] pdf = PdfBuilder.buildPdf(
						buildSrList(),
						data,
						getSignTask().getBioSigns(),
						getSignTask().getCert()
					);

					final java.io.OutputStream fos = new FileOutputStream(File.createTempFile("KAKA", ".pdf")); //$NON-NLS-1$ //$NON-NLS-2$
					fos.write(pdf);
					fos.flush();
					fos.close();
					//return pdf;
				}
				catch(final Exception e) {
					signatureAborted(e, signId);
				}

			}
		});
		dwn.downloadFile(getSignTask().getRetrieveUrl().toString());


	}

}
