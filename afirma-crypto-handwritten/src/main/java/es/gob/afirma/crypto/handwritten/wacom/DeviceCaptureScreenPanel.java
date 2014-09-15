package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JPanel;

import com.WacomGSS.STU.Protocol.PenData;

final class DeviceCaptureScreenPanel extends JPanel {

	private static final long serialVersionUID = 4179574305135267207L;

	private final ImageProvider imageProvider;

	DeviceCaptureScreenPanel(final ImageProvider ip, final Dimension size) {
		if (ip == null) {
			throw new IllegalArgumentException(
				"El proveedor de la imagen de firma y pantalla no puede ser nulo" //$NON-NLS-1$
			);
		}
		if (size == null) {
			throw new IllegalArgumentException(
				"El tamano del panel de firma no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.imageProvider = ip;
		this.addMouseListener(
			new MouseAdapter() {
				@Override
				public void mouseClicked(final MouseEvent e) {
					ip.clickFromClientToCaptureDevice(e.getPoint());
				}
			}
		);
		this.setSize(size);
	}

	@Override
	public void paintComponent(final Graphics gfx) {
		super.paintComponent(gfx);
		if (this.imageProvider.getImage() != null) {
			final Image rescaled = this.imageProvider.getImage().getScaledInstance(
				getWidth(),
				getHeight(),
				Image.SCALE_SMOOTH
			);
			gfx.drawImage(rescaled, 0, 0, null);
			drawInk((Graphics2D) gfx);
		}
	}

	private void drawInk(final Graphics2D gfx) {
		final PenData[] pd = this.imageProvider.getPenData();
		for (int i = 1; i < pd.length; ++i) {
			if (pd[i - 1].getSw() != 0 && pd[i].getSw() != 0) {
				this.imageProvider.drawInk(gfx, pd[i - 1], pd[i]);
			}
		}
	}

}
