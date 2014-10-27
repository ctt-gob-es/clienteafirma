package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.util.logging.Logger;

import javax.swing.JPanel;

import com.WacomGSS.STU.NotConnectedException;
import com.WacomGSS.STU.STUException;
import com.WacomGSS.STU.Tablet;
import com.WacomGSS.STU.UsbDevice;

import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePadConnectionException;

final class UiUtils {

	private UiUtils() {
		// No instanciable
	}

	static JPanel getPadMirrorPanel(final ImageProvider ip,
			                        final int maxX,
			                        final int maxY,
			                        final int screenResolution) {
		return new DeviceCaptureScreenPanel(
			ip,
			new Dimension(
				maxX * screenResolution / 2540,
				maxY * screenResolution / 2540
			)
		);
	}

	/** Botones para la pantalla de la tableta y espacio libre que queda en esta tras pintar los botones. */
	static final class ButtonsOnScreen {
		private final Button[] btns;
		private final Dimension dim;

		ButtonsOnScreen(final Button[] b, final Dimension d) {
			this.btns = b;
			this.dim = d;
		}

		Button[] getButtons() {
			return this.btns;
		}

		Dimension getAvailableScreenSize() {
			return this.dim;
		}
	}


	/** M&eactute;todo para crear los botones de la interfaz de firma.
	 * @param tablet Tableta de firma.
	 * @param useColor True si la tableta presenta interfaz gráfico con color, false en caso contrario.
	 * @param screenWidth Ancho de pantalla.
	 * @param screenHeight Alto de los botones.
	 * @param pbl Listener con los m&ecute;todos para utilizar cuando se pulsa los botones de la interfaz.
	 * @param count array de tres posiciones con la que se evita realizar m&acute;s de una llamada a la funciones correspondiente al listener del bot&oacute;n pulsado.
	 * @return devuelve la interfaz de botones para la tableta de firma
	 * */
	static ButtonsOnScreen createButtons(final Tablet tablet,
			                      final boolean useColor,
			                      final int screenWidth,
			                      final int screenHeight,
			                      final PadButtonsListener pbl,
			                      final int[] count) throws SignaturePadConnectionException {

		final Button[] btns = new Button[3];
		btns[0] = new Button(Messages.getString("SignatureDialog.0"), useColor); //$NON-NLS-1$
		btns[1] = new Button(Messages.getString("SignatureDialog.1"), useColor); //$NON-NLS-1$
		btns[2] = new Button(Messages.getString("SignatureDialog.2"), useColor); //$NON-NLS-1$


		final short productId;
		try {
			productId = tablet.getProductId();
		}
		catch (final NotConnectedException e) {
			throw new SignaturePadConnectionException(
				"Error obteniendo el numero de producto de la tableta: " + e, e //$NON-NLS-1$
			);
		}

		final Dimension d;

		if (productId != UsbDevice.ProductId_300) {

			// Los botones en la parte inferior de la pantalla.

			final int w2 = screenWidth / 3;
			final int w3 = screenWidth / 3;
			final int w1 = screenWidth - w2 - w3;
			final int y = screenHeight * 6 / 7;
			final int h = screenHeight - y;

			btns[0].setBounds(new java.awt.Rectangle(0, y, w1, h));
			btns[1].setBounds(new java.awt.Rectangle(w1, y, w2, h));
			btns[2].setBounds(new java.awt.Rectangle(w1 + w2, y, w3, h));

			d = new Dimension(screenWidth, screenHeight - h);

		}
		else {

			// La STU-300 es muy estrecha, para no perder tamano de pantalla
			// ponemos los botones en el lateral.

			final int x = screenWidth * 3 / 4;
			final int w = screenWidth - x;

			final int h2 = screenHeight / 3;
			final int h3 = screenHeight / 3;
			final int h1 = screenHeight - h2 - h3;

			btns[0].setBounds(new java.awt.Rectangle(x, 0, w, h1));
			btns[1].setBounds(new java.awt.Rectangle(x, h1, w, h2));
			btns[2].setBounds(new java.awt.Rectangle(x, h1 + h2, w, h3));

			d = new Dimension(screenWidth - w, screenHeight);
		}

		// Actualmente ocurren decenas clicks, realizando decenas de llamadas al evento presionado.
		// Con el contador solo permitiremos realizar un unico click

		btns[0].addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent evt) {
					//  Sentencia if para que se ejecute una unica vez el metodo
					if(count[0] == 0) {
						count[0]++;
						pbl.pressOkButton();
					}
				}
			}
		);

		btns[1].addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent evt) {
					try {
						pbl.pressClearButton();
					}
					catch (final STUException e1) {
						Logger.getLogger("es.gob.afirma").warning("Error en el evento de pulsar el boton repetir: " + e1); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		);

		btns[2].addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent evt) {
					if(count[1] == 0) {
						count[1]++;
						pbl.pressCancelButton();
					}
				}
			}
		);


		return new ButtonsOnScreen(btns, d);

	}

	static BufferedImage getBitmapWithButtons(final int screenWidth,
			                                  final int screenHeight,
			                                  final Button[] btns,
			                                  final Image bgSurfaceImage,
			                                  final Rectangle signatureArea) {
		final BufferedImage bitmap = new BufferedImage(
			screenWidth,
			screenHeight,
			BufferedImage.TYPE_INT_RGB
		);

		final Graphics2D gfx = bitmap.createGraphics();
		gfx.setColor(Color.WHITE);
		gfx.fillRect(0, 0, bitmap.getWidth(), bitmap.getHeight());

		if (bgSurfaceImage != null) {
			gfx.drawImage(bgSurfaceImage, 0, 0, null);
		}

		if (signatureArea != null) {
			gfx.setColor(Color.BLACK);
			gfx.drawRect(
				signatureArea.x,
				signatureArea.y,
				signatureArea.width,
				signatureArea.height
			);
		}

		// Pintamos los botones
		for (final Button btn : btns) {
			btn.paint(gfx);
		}

		gfx.dispose();

		return bitmap;
	}
}
