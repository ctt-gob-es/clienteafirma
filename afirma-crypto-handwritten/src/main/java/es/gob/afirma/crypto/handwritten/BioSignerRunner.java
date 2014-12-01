package es.gob.afirma.crypto.handwritten;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.ConnectException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
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

import com.WacomGSS.STU.Tablet;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.crypto.handwritten.net.DownloadListener;
import es.gob.afirma.crypto.handwritten.net.Downloader;
import es.gob.afirma.crypto.handwritten.pdf.PdfBuilder;
import es.gob.afirma.crypto.handwritten.wacom.PadUtils;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.signers.pades.AOPDFSigner;

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

	private static final String PDF_EXTENSION = ".pdf"; //$NON-NLS-1$

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

	byte[] pdfSignedByBioSigns = null;
	byte[] getPdfSignedByBioSigns() {
		return this.pdfSignedByBioSigns;
	}


	/** Crea el ejecutor de procesos de firma biom&eacute;trica.
	 * @param xml Contiene los datos de la tarea de firma.
	 * @throws IOException Si hay errores en el tratamiento de datos. */
	public BioSignerRunner(final String xml) throws IOException {

		LOGGER.info("Contenido de la cadena XML : " + xml); //$NON-NLS-1$

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
					LOGGER.info("Inicio de la creacion de la interfaz de firma..."); //$NON-NLS-1$
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
						LOGGER.warning("Error en el proceso de firma."); //$NON-NLS-1$
						AOUIFactory.showErrorMessage(
							getMainFrame(),
							HandwrittenMessages.getString("BioSignerRunner.23"), //$NON-NLS-1$
							HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						closeApp();
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

	/**
	 * Inicio del proceso firma.
	 * @param ordinal posici&oacute;n que ocupa en la lista el usuario seleccionado.*/
	void launchSign(final int ordinal) {

		SingleBioSignData sign = null;

		// Asociamos el identificador de firma al ordinal
		// Si el ordinal seleccionado es mayor que el numero de firmantes, entonces se inicia el proceso de firma del funcionario
		if(ordinal >= getSignTask().getBioSigns().size()) {
			signEmployeeProcess();
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

				// Obtenemos el modelo de la tableta conectada
				Tablet tablet = PadUtils.getTablet();
				String model = PadUtils.getTabletModel(tablet);
				tablet.disconnect();

				LOGGER.info("Modelo de la tableta conectada: " + model); //$NON-NLS-1$

				// Obtenemos la plantilla HTML correspondiente a la tableta conectada
				String htmlTemplate = TabletTemplateData.getTemplateHtml(sign.getTabletTemplates(), model);
				// Obtenemos la plantilla JPEG correspondiente a la tableta conectada
				byte[] jpegTemplate = TabletTemplateData.getTemplateJPEG(sign.getTabletTemplates(), model);

				// Si hay plantilla HTML
				if (htmlTemplate != null) {
					bs.sign(
						getMainFrame(),
						sign.getId(),
						BioSignerRunner.this,
						htmlTemplate,
						sign.getSignatureArea(),
						this.signTask.showTabletButtons()
					);
				}

				// Si no es imagen o null
				else if (jpegTemplate != null){
					bs.sign(
						getMainFrame(),
						sign.getId(),
						BioSignerRunner.this,
						jpegTemplate,
						sign.getSignatureArea(),
						this.signTask.showTabletButtons()
					);
				}
				else {
					LOGGER.severe("No se ha definido una plantilla para la tableta conectada. Modelo de tableta: " + model); //$NON-NLS-1$
					AOUIFactory
					.showErrorMessage(
						getMainFrame(),
						HandwrittenMessages.getString("BioSignerRunner.34") + model, //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);

					closeApp();

					return;
				}
			}
			catch (final Exception ex) {
				abortTask(null, ex);
				return;
			}
		}

	}

	private void signEmployeeProcess() {
		// Inicio del proceso de firma del funcionario
		LOGGER.info("Inicio del proceso de firma del funcionario"); //$NON-NLS-1$

		// Obtenemos el gestor del almac&eacute;n de claves (KeyStoreManager)
		final AOKeyStoreManager ksm;
		try {
			if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.WINDOWS,
					null,
					"Windows", //$NON-NLS-1$
					AOKeyStore.WINDOWS.getStorePasswordCallback(getMainFrame()),
					getMainFrame()
				);
			}
			else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.APPLE,
						null,
						"Apple", //$NON-NLS-1$
						AOKeyStore.APPLE.getStorePasswordCallback(getMainFrame()),
						getMainFrame()
					);
			}
			else {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.MOZ_UNI,
						null,
						"NSS", //$NON-NLS-1$
						AOKeyStore.MOZ_UNI.getStorePasswordCallback(getMainFrame()),
						getMainFrame()
					);
			}
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
					getMainFrame(),
					HandwrittenMessages.getString("BioSignerRunner.25"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			return;
		}

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, true, false);
		String alias;
		try {
			alias = dialog.show();
		}
		catch (AOCertificatesNotFoundException e) {
			LOGGER.warning("Error, no hay certificados para firmar. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					getMainFrame(),
					HandwrittenMessages.getString("BioSignerRunner.26"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			closeApp();
			return;
		}

		// Obtenemos la entrada de la clave privada
		final PrivateKeyEntry signKey;
		try {
			signKey = ksm.getKeyEntry(alias, NullPasswordCallback.getInstance());
		}
		catch (Exception e) {
			LOGGER.warning("Error accediendo a la clave de firma. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					getMainFrame(),
					HandwrittenMessages.getString("BioSignerRunner.27"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			closeApp();
			return;
		}

		//TODO: Borrar estas lineas, solo se utilizan para la demos de SACYL
		Properties p = new Properties();
		p.put("signaturePage", "1");
		p.put("signaturePositionOnPageLowerLeftX", "250");
		p.put("signaturePositionOnPageLowerLeftY", "205");
		p.put("signaturePositionOnPageUpperRightX", "400");
		p.put("signaturePositionOnPageUpperRightY", "255");
		p.put("signatureRubricImage", "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");

		// PDF final
		final byte[] finalPDFToSave;
		try {
			finalPDFToSave = new AOPDFSigner().sign(
				getPdfSignedByBioSigns(),
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				signKey.getPrivateKey(),
				signKey.getCertificateChain(),
				p
			);
		}
		catch (Exception e) {
			LOGGER.warning("Error en la firma del funcionario. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					getMainFrame(),
					HandwrittenMessages.getString("BioSignerRunner.28"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			closeApp();
			return;
		}

		// Guardamos el PDF a disco(importante en este caso no haber guardado el intermedio)
		closePDF(finalPDFToSave, getMainFrame());

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
		else if(t instanceof ConnectException) {
			errorMsg = HandwrittenMessages.getString("BioSignerRunner.22"); //$NON-NLS-1$
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

	void closeApp() {
		// Borrar todas las firmas ya realizadas
		this.sigResults.clear();
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
				generatePdf();
			}

			catch (final Exception e) {
				LOGGER.warning("Error generando el documento PDF firmado. " + e); //$NON-NLS-1$
				signatureAborted(e, sr.getSignatureId());
				return;
			}
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

	private void generatePdf() throws IOException {

		final Downloader dwn = new Downloader(getMainFrame(), new DownloadListener() {

			@Override
			public void downloadError(final Throwable t) {
				signatureAborted(t,null);
			}

			@Override
			public void downloadComplete(final byte[] data) {

				CertificateFactory cf;
				X509Certificate cert = null;

				try {
					cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
					cert = (X509Certificate) cf.generateCertificate(
						new ByteArrayInputStream(Base64.decode(getSignTask().getCert()))
					);
				}
				catch (final CertificateException e) {
					LOGGER.warning("Error generando certificado X.509 : " + e); //$NON-NLS-1$
					downloadError(e);
					return;
				}
				catch (final IOException e) {
					LOGGER.warning("Error decodificando certificado X.509 : " + e); //$NON-NLS-1$
					downloadError(e);
					return;
				}

				try {
					final byte[] pdf = PdfBuilder.buildPdf(
						buildSrList(),
						data,
						getSignTask().getBioSigns(),
						cert,
						getSignTask().getCSV()
					);

					// Si necesitamos la firma del funcionario, guardamos la estructura del pdf para
					// insertar la firma del funcionario y posteriormente guardarla a disco y
					// habilitamos el boton de firmado al funcionario
					if(getSignTask().isCompleteWithCriptoSign()) {
						BioSignerRunner.this.pdfSignedByBioSigns = pdf;
						enableOpSignButton();
					}
					else {
						// Guardamos el PDF a disco.
						closePDF(pdf, getMainFrame());
					}

				}
				catch(final Exception e) {
					signatureAborted(e, null);
					return;
				}

			}
		});

		dwn.downloadFile(getSignTask().getRetrieveUrl().toString());


	}

	/** M&eacute;do para cerrar un PDF: crear el fichero y guardarlo a disco.
	 * @param data datos del PDF. */
	void closePDF(final byte[] data, final Frame parent) {

		if(this.signTask.getSaveDirectory() == null && this.signTask.getSaveUrl() == null) {
			LOGGER.warning("Error, no se ha especificado ni un directorio ni una URL para almacenar el PDF firmado."); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					parent,
					HandwrittenMessages.getString("BioSignerRunner.32"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			return;

		}
		// Directorio local para almacenar el PDF firmado.
		if(this.signTask.getSaveDirectory() != null) {
			try {
				final File f =
					new File (
							this.signTask.getSaveDirectory() +
							this.signTask.getSignedFileName() +
							PDF_EXTENSION
						);
				if(f.exists()) {
					final FileOutputStream fos = new FileOutputStream(f);
					fos.write(data);
					fos.close();
				}
				else {
					LOGGER.warning("El directorio especificado para almacenar el documento firmado no existe."); //$NON-NLS-1$
				}
			} catch (IOException e) {
				LOGGER.warning("Error, no se ha podido almacenar el PDF firmado en el directorio definido: " + e); //$NON-NLS-1$
				AOUIFactory
					.showErrorMessage(
						parent,
						HandwrittenMessages.getString("BioSignerRunner.33"), //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
			}

		}

		// URL en la que almacenar el PDF.
		if(this.signTask.getSaveUrl() != null) {
			final String url =
					this.signTask.getSaveUrl() +
					"?" + this.signTask.getSaveIdPostParam() + "=" + this.signTask.getSaveId() +  //$NON-NLS-1$//$NON-NLS-2$
					"&" + this.signTask.getSaveUrlPostParam() + "=" + Base64.encode(data, true); //$NON-NLS-1$ //$NON-NLS-2$

			LOGGER.info("Se enviara el PDF a la direccion:\n" + url);   //$NON-NLS-1$
			try {
				final byte[] resbytes = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(url);
				if (resbytes == null) {
					throw new IOException("La respuesta del servidor es nula"); //$NON-NLS-1$
				}
				final String response = new String(resbytes);
				LOGGER.info("Respuesta del servidor tras el envio del PDF firmado: " + response); //$NON-NLS-1$
				if (!"OK".equals(response.trim())) { //$NON-NLS-1$
					throw new IOException("La respuesta del servidor no es OK: " + response); //$NON-NLS-1$
				}
			}
			catch (Exception e) {
				LOGGER.warning("Error enviando el PDF al servidor: " + e); //$NON-NLS-1$
				AOUIFactory
					.showErrorMessage(
						parent,
						HandwrittenMessages.getString("BioSignerRunner.31"), //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
				return;
			}
		}

		AOUIFactory
			.showMessageDialog(
				parent,
				HandwrittenMessages.getString("BioSignerRunner.29"), //$NON-NLS-1$
				HandwrittenMessages.getString("BioSignerRunner.30"), //$NON-NLS-1$
				JOptionPane.INFORMATION_MESSAGE
			);

		// Cerramos la aplicacion
		parent.setVisible(false);
		parent.dispose();
	}

	}
