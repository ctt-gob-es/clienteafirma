package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;
import java.util.zip.Deflater;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import com.WacomGSS.STU.ITabletHandler;
import com.WacomGSS.STU.STUException;
import com.WacomGSS.STU.Tablet;
import com.WacomGSS.STU.Protocol.Capability;
import com.WacomGSS.STU.Protocol.DevicePublicKey;
import com.WacomGSS.STU.Protocol.EncodingMode;
import com.WacomGSS.STU.Protocol.EncryptionStatus;
import com.WacomGSS.STU.Protocol.InkingMode;
import com.WacomGSS.STU.Protocol.PenData;
import com.WacomGSS.STU.Protocol.PenDataEncrypted;
import com.WacomGSS.STU.Protocol.PenDataEncryptedOption;
import com.WacomGSS.STU.Protocol.PenDataOption;
import com.WacomGSS.STU.Protocol.PenDataTimeCountSequence;
import com.WacomGSS.STU.Protocol.PenDataTimeCountSequenceEncrypted;
import com.WacomGSS.STU.Protocol.ProtocolHelper;

import es.gob.afirma.crypto.handwritten.JseUtil;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePad;
import es.gob.afirma.crypto.handwritten.SignaturePadConnectionException;
import es.gob.afirma.crypto.handwritten.SignaturePadException;
import es.gob.afirma.crypto.handwritten.SignaturePadListener;
import es.gob.afirma.crypto.handwritten.SignatureResult;
import es.gob.afirma.crypto.handwritten.wacom.UiUtils.ButtonsOnScreen;

/** Tableta de firma Wacom USB.
 * Basado en <i>SignatureDialog</i> del SDK de Wacom.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class WacomSignaturePad extends SignaturePad implements ITabletHandler,
                                                                     ImageProvider,
                                                                     PadButtonsListener {
	private final Dimension availableScreenSize;

	/** Botones emulados. */
	private final Button[] btns;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = -5861903537003625868L;

	/** Indica si se comprimir&aacute;n o no las im&aacute;genes que se env&iacute;an al dispositivo de captura
	 * para mejorar la velocidad de transmisi&oacute;n. */
	private static final boolean USE_ZLIB_COMPRESSION = true;

	private static final String BACKGROUND_IMAGE_PATH = "/wacom/Wacom_STU430.png"; //$NON-NLS-1$

	private Tablet tablet;

	private final Capability capability;

	private java.awt.Rectangle sigArea = null;

	private final JPanel panel;

	private boolean useColor = false;

	/** Imagen que se muestra en la pantalla replicada. */
	private BufferedImage bitmap = null;

	// The isDown flag is used like this:
	// 0 = up
	// +ve = down, pressed on button number
	// -1 = down, inking
	// -2 = down, ignoring
	private int isDown;

	// Array of data being stored. This can be subsequently used as desired.
	private final List<PenData> penData = new ArrayList<PenData>();

	@Override
	public int getAvailableWidth() {
		return (int) this.availableScreenSize.getWidth();
	}

	@Override
	public int getAvailableHeight() {
		return (int) this.availableScreenSize.getHeight();
	}

	@Override
	public PenData[] getPenData() {
		if (this.penData != null) {
			final PenData[] arrayPenData = new PenData[0];
			return this.penData.toArray(arrayPenData);
		}
		return null;
	}

	private Point2D.Float tabletToCaptureDevice(final PenData penData1) {
		return new Point2D.Float(
			(float) penData1.getX() * this.capability.getScreenWidth()  / this.capability.getTabletMaxX(),
			(float) penData1.getY() * this.capability.getScreenHeight() / this.capability.getTabletMaxY()
		);
	}

	@Override
	public void clickFromClientToCaptureDevice(final Point pt) {
		final Point2D.Float devicePoint = new Point2D.Float(
			(float) pt.getX() * this.capability.getScreenWidth()  / this.panel.getWidth(),
			(float) pt.getY() * this.capability.getScreenHeight() / this.panel.getHeight()
		);
		for (final Button btn : WacomSignaturePad.this.btns) {
			if (btn.contains(devicePoint)) {
				btn.performClick();
				break;
			}
		}
	}

	@Override
	public Image getImage() {
		return this.bitmap;
	}

	/** Modo en el que se env&iacute;a la imagen al dispositivo de captura. */
	private EncodingMode encodingMode;

	/** Mapa de bits (imagen) que se env&iacute;a al dispositivo de captura. */
	private byte[] bitmapData;

	private boolean encrypted = false;

	private Point2D.Float tabletToClient(final PenData penData1) {
		return new Point2D.Float(
			(float) penData1.getX() * this.panel.getWidth()  / this.capability.getTabletMaxX(),
			(float) penData1.getY() * this.panel.getHeight() / this.capability.getTabletMaxY()
		);
	}

	private BufferedImage getCurrentCroppedImage() {
		final BufferedImage im = new BufferedImage(
			this.panel.getWidth(),
			this.panel.getHeight(),
			BufferedImage.TYPE_INT_RGB
		);
		this.panel.paint(im.getGraphics());
		return im.getSubimage(
			this.sigArea.x * this.panel.getWidth() / this.bitmap.getWidth() + 1 ,
			this.sigArea.y * this.panel.getHeight() / this.bitmap.getHeight() + 2,
			this.sigArea.width * this.panel.getWidth() / this.bitmap.getWidth() -1,
			this.sigArea.height * this.panel.getHeight() / this.bitmap.getHeight() -1
		);
	}

	@Override
	public synchronized void pressOkButton() {
		this.setVisible(false);
		for (final SignaturePadListener sl : this.signatureListeners) {
			try {
				sl.signatureFinished(
					new SignatureResult(
						PadUtils.penDataArrayToIso19794(getPenData()),
						null,
						JseUtil.bufferedImage2Jpeg(getCurrentCroppedImage())
					)
				);
			}
			catch (final Exception e) {
				dispose();
				throw new IllegalStateException(
					"No se ha podido obtener la imagen de rubrica de la firma: " + e, e //$NON-NLS-1$
				);
			}
		}

		// Eliminamos los listeners
		this.signatureListeners.clear();

		// Vaciamos la pantalla
		dispose();
	}

	@Override
	public synchronized void pressClearButton() throws STUException {
		clearScreen();
	}

	@Override
	public synchronized void pressCancelButton() {
		this.setVisible(false);
		this.penData.clear();

		// Vaciamos la pantalla
		dispose();

		for (final SignaturePadListener sl : this.signatureListeners) {
			sl.signatureCancelled();
		}

		// Eliminamos los listeners
		this.signatureListeners.clear();
	}

	private void clearScreen() throws STUException {
		this.tablet.writeImage(this.encodingMode, this.bitmapData);
		this.penData.clear();
		this.isDown = 0;
		this.panel.repaint();
	}

	@Override
	public void dispose() {
		if (this.tablet != null) {
			try {
				this.tablet.setInkingMode(InkingMode.Off);
				if (this.encrypted) {
					this.tablet.endCapture();
					this.encrypted = false;
				}
				this.tablet.setClearScreen();
			}
			catch (final STUException t) {
				LOGGER.warning("Error cerrando el dipositivo de captura de firmas: " + t); //$NON-NLS-1$
			}
			this.tablet.disconnect();
			this.tablet = null;
		}
		super.dispose();
	}

	/** Crea una tableta de firma Wacom USB.
	 * @param frame Componente padre (para la modalidad)
	 * @param bgSurfaceImage Imagen a mostrar de fondo en la pantalla de la tableta.
	 * @param signatureArea Area de firma dentro de la pantalla de la tableta.
	 * @throws SignaturePadConnectionException */
	public WacomSignaturePad(final Frame frame) throws SignaturePadException {

		super(frame, true);

		setTitle(Messages.getString("SignatureDialog.3")); //$NON-NLS-1$
		try {
			setIconImage(ImageIO.read(WacomSignaturePad.class.getResource("/logo_cliente_96x96.png"))); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning(
				"No ha sido posible establecer el icono del dialogo: " + e //$NON-NLS-1$
			);
		}

		// Buscamos la primera tableta conectada al sistema y la inicializamos
		this.tablet = PadUtils.getTablet();

		// Obtenemos las capacidades de la tableta
		this.capability = PadUtils.getTabletCapability(this.tablet);

		// Se deshabilita el color si no esta establecido el controlador bulk para evitar lentitud
		this.useColor = PadUtils.getColorEnabled(this.tablet, this.capability);

		// Dimensiones totales de la pantalla de captura de la tableta, que se usan en distintos sitios
		// del constructor
		final int screenWidth = this.capability.getScreenWidth();
		final int screenHeight = this.capability.getScreenHeight();

		// Creamos los botones que se ven en la pantalla de la tableta y en el espejo en pantalla
		final ButtonsOnScreen bos = UiUtils.createButtons(
			// Dependiendo del modelo los botones varian de sitio
			this.tablet,
			this.useColor,
			screenWidth,
			screenHeight,
			this
		);

		// Los botones determinan el espacio util para captura de la pantalla de la tableta
		this.availableScreenSize = bos.getAvailableScreenSize();

		this.btns = bos.getButtons();

		// Creamos la replica de la pantalla de captura
		// El tamano se establece atendiendo a la resolucion de la pantalla
		this.panel = UiUtils.getPadMirrorPanel(
			this,
			this.capability.getTabletMaxX(),
			this.capability.getTabletMaxY(),
			this.getToolkit().getScreenResolution()
		);

		// La situamos acorde a la imagen de fondo
		this.panel.setLocation(
			153,
			211
		);

		this.setLayout(null);
		this.setBounds(10, 10, 718, 653);
		this.setResizable(false);

		// Pintamos la imagen de fondo (imagen de la tableta fisica, por estetica)
		final JLabel bgImage = new JLabel(new ImageIcon(WacomSignaturePad.class.getResource(BACKGROUND_IMAGE_PATH)));
		bgImage.setBounds(0, 0, 718, 653);

		this.add(this.panel);
		this.add(bgImage);

		// Calculamos el modo de codificacion para la imagen a enviar a la tableta
		this.encodingMode = PadUtils.getTabletEncodingMode(this.useColor, this.tablet);

		// Anadimos el manejador que recibe los datos de la firma.
		this.tablet.addTabletHandler(this);
	}

	public void init(final byte[] imageTemplate, final Rectangle signatureArea) throws SignaturePadException, IOException {
		init(JseUtil.jpeg2BufferedImage(imageTemplate, this.useColor), signatureArea);
	}

	/**
	 *
	 * @param bgSurfaceImage
	 * @param signatureArea
	 * @throws SignaturePadException Cuando ocurre cualquier problema durante el proceso.
	 */
	public void init(final Image bgSurfaceImage, final Rectangle signatureArea) throws SignaturePadException {

		// Establecemos el area de firma
		if (signatureArea != null) {
			this.sigArea = new java.awt.Rectangle(
				signatureArea.x,
				signatureArea.y,
				signatureArea.width,
				signatureArea.height
			);
		}
		else {
			this.sigArea = new java.awt.Rectangle(
				0,
				0,
				this.availableScreenSize.width,
				this.availableScreenSize.height
			);
		}
		PadUtils.setTabletSignatureArea(this.tablet, this.sigArea);

		// Dimensionamos el mapa de bits de la pantalla y pintamos los botones.
		// Se reutiliza el mismo para la tableta real y para el espejo
		this.bitmap = UiUtils.getBitmapWithButtons(
			this.capability.getScreenWidth(),
			this.capability.getScreenHeight(),
			this.btns,
			bgSurfaceImage,
			signatureArea
		);

		// Convertimos la imagen a formato Wacom nativo.
		this.bitmapData = PadUtils.convertBitmapToWacomNative(this.bitmap, this.useColor);

		// Si es blanco y negro comprimimos la imagen para que cargue mas rapido
		if (!this.useColor && USE_ZLIB_COMPRESSION) {
			final Deflater deflater = new Deflater(9);
			deflater.setInput(this.bitmapData);
			deflater.finish();
			final byte[] outputBuffer = new byte[this.bitmapData.length];
			final int finalLength = deflater.deflate(outputBuffer);
			deflater.end();
			this.bitmapData = Arrays.copyOf(outputBuffer, finalLength);
			this.encodingMode = EncodingMode.EncodingMode_1bit_ZLib;
		}

		try {

			// Borramos pantalla...
			clearScreen();

			if (ProtocolHelper.supportsEncryption(this.tablet.getProtocol())) {
				this.tablet.startCapture(0xc0ffee);
				this.encrypted = true;
			}

			// ... Y habilitamos el lapiz
			this.tablet.setInkingMode(InkingMode.On);
		}
		catch (final STUException t) {
			t.printStackTrace();
			if (this.tablet != null) {
				this.tablet.disconnect();
				this.tablet = null;
			}
			throw new SignaturePadException(t);
		}
	}

	@Override
	public void onGetReportException(final STUException e) {
		this.tablet.disconnect();
		this.tablet = null;
		this.penData.clear();
		this.setVisible(false);
		for (final SignaturePadListener sl : this.signatureListeners) {
			sl.signatureAborted(e);
		}
	}

	@Override
	public void onUnhandledReportData(final byte[] data) {
		// Vacio
	}

	@Override
	public void onPenData(final PenData penData1) {
//		final count = 0;
//		System.out.println(this.count++ + " Ready: " + penData1.getRdy() + "\tClass: " + penData1.getClass().getName() + "\tContact: " + penData1.getSw() + "\tPresion: " + penData1.getPressure());

		final Point2D pt = tabletToCaptureDevice(penData1);

		int btn = 0; // will be +ve if the pen is over a button.

		for (int i = 0; i < this.btns.length; ++i) {
			if (this.btns[i].contains(pt)) {
				btn = i + 1;
				break;
			}
		}

		final boolean isDown1 = penData1.getSw() != 0;

		// This code uses a model of four states the pen can be in:
		// down or up, and whether this is the first sample of that state.

		if (isDown1) {
			if (this.isDown == 0) {
				// transition to down
				if (btn > 0) {
					// We have put the pen down on a button.
					// Track the pen without inking on the client.
					this.isDown = btn;
				}
				else {
					// We have put the pen down somewhere else.
					// Treat it as part of the signature.
					this.isDown = -1;
				}
			}
			else {
				// already down, keep doing what we're doing!
				// draw
				if (!this.penData.isEmpty() && this.isDown == -1) {
					// Draw a line from the previous down point to this down point.
					// This is the simplist thing you can do; a more sophisticated program
					// can perform higher quality rendering than this!
					final Graphics2D gfx = (Graphics2D) this.panel.getGraphics();
					drawInk(gfx, this.penData.get(this.penData.size() - 1), penData1);
					gfx.dispose();
				}

			}

			// The pen is down, store it for use later.
			if (this.isDown == -1) {
				this.penData.add(penData1);
			}
		}
		else {
			if (this.isDown != 0) {
				// transition to up
				if (btn > 0) {
					// The pen is over a button

					if (btn == this.isDown) {
						// The pen was pressed down over the same button as is
						// was lifted now.
						// Consider that as a click!
						this.btns[btn - 1].performClick();
					}
				}
				this.isDown = 0;
			}
			else {
				// El lapiz esta una levantado
			}

			// Add up data once we have collected some down data.
			if (!this.penData.isEmpty()) {
				this.penData.add(penData1);
			}
		}

	}

	@Override
	public void drawInk(final Graphics2D gfx, final PenData pd0, final PenData pd1) {
		if (this.sigArea.contains(tabletToCaptureDevice(pd0))) {
			gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			gfx.setColor(new Color(0, 0, 64, 255));
			gfx.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
			gfx.draw(
				new Line2D.Float(
					tabletToClient(pd0),
					tabletToClient(pd1)
				)
			);
		}
	}

	@Override
	public void onPenDataOption(final PenDataOption penDataOption) {
		onPenData(penDataOption);
	}

	@Override
	public void onPenDataEncrypted(final PenDataEncrypted penDataEncrypted) {
		onPenData(penDataEncrypted.getPenData1());
		onPenData(penDataEncrypted.getPenData2());
	}

	@Override
	public void onPenDataEncryptedOption(final PenDataEncryptedOption penDataEncryptedOption) {
		onPenData(penDataEncryptedOption.getPenDataOption1());
		onPenData(penDataEncryptedOption.getPenDataOption2());
	}

	@Override
	public void onPenDataTimeCountSequence(final PenDataTimeCountSequence penDataTimeCountSequence) {
		onPenData(penDataTimeCountSequence);
	}

	@Override
	public void onPenDataTimeCountSequenceEncrypted(final PenDataTimeCountSequenceEncrypted penDataTimeCountSequenceEncrypted) {
		onPenData(penDataTimeCountSequenceEncrypted);
	}

	@Override
	public void onEncryptionStatus(final EncryptionStatus encryptionStatus) {
		// Vacio
	}

	@Override
	public void onDevicePublicKey(final DevicePublicKey devicePublicKey) {
		// Vacio
	}


}
