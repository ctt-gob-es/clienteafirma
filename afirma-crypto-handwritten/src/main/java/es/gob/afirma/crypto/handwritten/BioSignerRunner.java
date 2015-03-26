package es.gob.afirma.crypto.handwritten;

import java.awt.Container;
import java.awt.Dimension;
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

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import netscape.javascript.JSObject;

import com.WacomGSS.STU.Tablet;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.crypto.handwritten.net.DownloadListener;
import es.gob.afirma.crypto.handwritten.net.Downloader;
import es.gob.afirma.crypto.handwritten.pdf.PdfBuilder;
import es.gob.afirma.crypto.handwritten.wacom.PadUtils;
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

	private final JPanel panel = new JPanel();
	JPanel getMainFrame() {
		return this.panel;
	}

	private final List<JButton> buttonList = new ArrayList<JButton>();
	List<JButton> getButtonList() {
		return this.buttonList;
	}

	byte[] pdfSignedByBioSigns = null;
	byte[] getPdfSignedByBioSigns() {
		return this.pdfSignedByBioSigns;
	}

	private JSObject response = null;

	JDialog dialog;

	/** Crea el ejecutor de procesos de firma biom&eacute;trica.
	 * @param xml Contiene los datos de la tarea de firma.
	 * @param finalResponse JSObject definido para enviar la respuesta obtenida al finalizar el proceso.
	 * @throws IOException Si hay errores en el tratamiento de datos. */
	public BioSignerRunner(final String xml, final JSObject finalResponse) throws IOException {

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

		this.response = finalResponse;

		LOGGER.info("Writable directory: " + getSignTask().getWrtDirectory()); //$NON-NLS-1$ç

		PadUtils.setLogPath(getSignTask().getWrtDirectory());

		//Alnmacenamos las dlls de WACOM en el directorio definido en el XML.
		PadUtils.setLibraryPath(getSignTask().getWrtDirectory());

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
			signerData = getSignTask().getBioSigns().get(ordinal-1).getSignerData().dataToBtn();
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
						LOGGER.severe("Error en el proceso de firma: " + e); //$NON-NLS-1$
						getMainFrame().setSize(new Dimension(100,100));
						AOUIFactory.showErrorMessage(
							getMainFrame(),
							HandwrittenMessages.getString("BioSignerRunner.23"), //$NON-NLS-1$
							HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						closeApp(HandwrittenMessages.getString("BioSignerRunner.23")); //$NON-NLS-1$
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
			signEmployeeProcess(getSignTask().getCompleteCriptoSignExtraParams());
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
				final Tablet tablet = PadUtils.getTablet();
				final String model = PadUtils.getTabletModel(tablet);
				tablet.disconnect();

				LOGGER.info("Modelo de la tableta conectada: " + model); //$NON-NLS-1$

				// Obtenemos la plantilla HTML correspondiente a la tableta conectada
				final String htmlTemplate = TabletTemplateData.getTemplateHtml(sign.getTabletTemplates(), model);
				// Obtenemos la plantilla JPEG correspondiente a la tableta conectada
				final byte[] jpegTemplate = TabletTemplateData.getTemplateJPEG(sign.getTabletTemplates(), model);

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
					getMainFrame().setSize(new Dimension(100,100));
					AOUIFactory
					.showErrorMessage(
						getMainFrame(),
						HandwrittenMessages.getString("BioSignerRunner.34") + model, //$NON-NLS-1$
						HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);

					closeApp(HandwrittenMessages.getString("BioSignerRunner.34") + model); //$NON-NLS-1$

					return;
				}
			}
			catch (final Exception ex) {
				abortTask(null, ex);
				return;
			}
		}

	}

	private void signEmployeeProcess(final Properties extraParams) {
		// Inicio del proceso de firma del funcionario
		LOGGER.info("Inicio del proceso de firma del funcionario"); //$NON-NLS-1$

		final PrivateKeyEntry signKey;
		if (this.signTask.hasCompleteCriptoSignPkcs12Params()) {
			signKey = BioSignerRunnerKeyHelper
				.getKeyFromPkcs12(
					this.signTask.getCompleteCriptoSignPkcs12(),
					this.signTask.getCompleteCriptoSignPkcs12Password(),
					this.signTask.getCompleteCriptoSignPkcs12Alias(),
					this.panel
			);
		}
		else{
			signKey = BioSignerRunnerKeyHelper.getKey(this.panel);
		}

		if (signKey == null) {
			closeApp("Error en el proceso de firma del funcionario"); //$NON-NLS-1$
			return;
		}

		// PDF final
		final byte[] finalPDFToSave;
		try {
			finalPDFToSave = new AOPDFSigner().sign(
				getPdfSignedByBioSigns(),
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				signKey.getPrivateKey(),
				signKey.getCertificateChain(),
				extraParams
			);
		}
		catch (final Exception e) {
			LOGGER.warning("Error en la firma del funcionario. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					getMainFrame(),
					HandwrittenMessages.getString("BioSignerRunner.28"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			closeApp(HandwrittenMessages.getString("BioSignerRunner.28")); //$NON-NLS-1$
			return;
		}

		// Guardamos el PDF a disco(importante en este caso no haber guardado el intermedio)
		closePDF(finalPDFToSave, getMainFrame());

	}

	// Crea el panel de botones de firma
	void createUI(final SignTask st) {

		//this.panel.setTitle(HandwrittenMessages.getString("BioSignerRunner.19")); //$NON-NLS-1$
		this.panel.getAccessibleContext().setAccessibleDescription(
			HandwrittenMessages.getString("BioSignerRunner.1") //$NON-NLS-1$
		);

		//this.panel.setIconImage(ImageIO.read(BioSignerRunner.class.getResource("/logo_cliente_96x96.png"))); //$NON-NLS-1$

		this.panel.setLayout(new GridLayout());

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

		this.panel.setSize(new Dimension(MIN_WIDTH, MIN_HEIGHT_CONSTANT * signs.size()));
		buttonPanel.setMaximumSize(new Dimension(800, 120 * signs.size()));

		this.panel.add(buttonPanel);
		//this.panel.pack();
		//this.panel.setLocationRelativeTo(null);
	}

	/** Muestra la ventana con las tareas de firma. */
	public void show() {
		final JOptionPane j = new JOptionPane(
			this.panel,
			JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE
		);
		j.setOptions(new Object[]{});
		this.dialog = j.createDialog(null, HandwrittenMessages.getString("BioSignerRunner.19")); //$NON-NLS-1$
		this.dialog.setSize(new Dimension(450, 100 * (this.signTask.getBioSigns().size() + 1)));
		this.dialog.setVisible(true);
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
			this.panel,
			errorMsg,
			HandwrittenMessages.getString("BioSignerRunner.12"), //$NON-NLS-1$
			JOptionPane.ERROR_MESSAGE
		);

		closeApp(errorMsg);
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
				this.panel,
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
					closeApp(HandwrittenMessages.getString("BioSignerRunner.10")); //$NON-NLS-1$
					showOpDlg = false;
				}
			}
		}
	}

	void closeApp(final String res) {
		// Borrar todas las firmas ya realizadas
		this.sigResults.clear();

		// Cerramos la aplicacion
		this.dialog.dispose();

		// Devolvemos un resultado al javascript
		LOGGER.info("Fin del proceso"); //$NON-NLS-1$
		if(this.response != null) {
			this.response.call("response", new Object[] {res}); //$NON-NLS-1$
		}
		else {
			LOGGER.severe("No existe un JSObject. " + res); //$NON-NLS-1$
		}
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
				e.printStackTrace();
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

	private void generatePdf() {

		final Downloader dwn = new Downloader(this.panel, new DownloadListener() {

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
		if(getSignTask().getRetrieveUrl() == null) {
			LOGGER.severe("No se ha indicado la url del documento a firmar. "); //$NON-NLS-1$
			signatureAborted(new IllegalStateException("No se ha indicado la url del documento a firmar. "), null); //$NON-NLS-1$
			return;
		}
		try {
			dwn.downloadFile(getSignTask().getRetrieveUrl());
		}
		catch(final Exception e) {
			LOGGER.severe("Error en la descarga del documento a firmar: " + e); //$NON-NLS-1$
			signatureAborted(e, null);
			return;
		}


	}

	/** M&eacute;do para cerrar un PDF: crear el fichero y guardarlo a disco.
	 * @param data datos del PDF. */
	void closePDF(final byte[] data, final Container parent) {

		String res = null;

		if(this.signTask.getSaveDirectory() == null && this.signTask.getSaveUrl() == null) {
			LOGGER.warning("Error, no se ha especificado ni un directorio ni una URL para almacenar el PDF firmado."); //$NON-NLS-1$
			res = HandwrittenMessages.getString("BioSignerRunner.32"); //$NON-NLS-1$
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
				final File f = new File(
					this.signTask.getSaveDirectory() +
					this.signTask.getSignedFileName() +
					PDF_EXTENSION
				);
				final FileOutputStream fos = new FileOutputStream(f);
				fos.write(data);
				fos.flush();
				fos.close();

				res = HandwrittenMessages.getString("BioSignerRunner.35") + " " + f.getAbsolutePath(); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final Exception e) {
				LOGGER.warning("Error, no se ha podido almacenar el PDF firmado en el directorio definido: " + e); //$NON-NLS-1$
				res = HandwrittenMessages.getString("BioSignerRunner.33"); //$NON-NLS-1$
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
			LOGGER.info("URL de guardado de documento: " + this.signTask.getSaveUrl().toString()); //$NON-NLS-1$
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
				if(res != null) {
					res = res + "\n" + new String(resbytes); //$NON-NLS-1$
				}
				else {
					res = new String(resbytes);
				}

				LOGGER.info(res);
				LOGGER.info("Respuesta del servidor tras el envio del PDF firmado: " + res); //$NON-NLS-1$

			}
			catch (final Exception e) {
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

		closeApp(res);
	}


	}
