package es.gob.afirma.crypto.handwritten;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import com.WacomGSS.STU.Protocol.PenData;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.crypto.handwritten.wacom.WacomSignaturePad;

/** Ejemplo de invocaci&oacute;n. */
public class DemoButtons extends JFrame {

	private static final long serialVersionUID = -2946927576518582468L;

	BufferedImage signatureImage;

	void startButtonPress() throws Throwable {
		try {
				final WacomSignaturePad signatureDialog = new WacomSignaturePad(
					this,
					new SignerInfoBean("Tomas", "Garcia-Meras", "Capote", "12345678Z") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				);
				signatureDialog.init(
					JseUtil.html2Image(
						"<html><body><p>Texto a mostrar en la pantalla</p><img src=\"data:image/png;base64," + Base64.encode(AOUtil.getDataFromInputStream(JseUtil.class.getResourceAsStream("/logo_aeat.gif"))) + "\"></body></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
						800,
						411
					),
					new Rectangle(
						20, 200, 700, 100
					)
				);
				signatureDialog.addSignatureListener(new SignaturePadListener() {

					@Override
					public void signatureFinished(final SignatureResult sr) {
						System.out.println("Firma OOOKKKK!!!!!"); //$NON-NLS-1$
						try {
							final File f = File.createTempFile("TEMP_", ".jpeg"); //$NON-NLS-1$ //$NON-NLS-2$
							final OutputStream fos = new FileOutputStream(f);
							fos.write(sr.getSignatureJpegImage());
							fos.flush();
							fos.close();
							System.exit(0);
						}
						catch (final IOException e) {
							e.printStackTrace();
						}

					}

					@Override
					public void signatureCancelled() {
						System.out.println("Firma cancelada!!!!!"); //$NON-NLS-1$
					}

					@Override
					public void signatureAborted(final Throwable e) {
						e.printStackTrace();
						JOptionPane.showMessageDialog(
							DemoButtons.this,
							"Error: " + e, //$NON-NLS-1$
							"Error (onGetReportException)", //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);

					}
				});
				signatureDialog.setVisible(true);

				System.out.println("Alto: " + signatureDialog.getAvailableHeight()); //$NON-NLS-1$
				System.out.println("Ancho: " + signatureDialog.getAvailableWidth()); //$NON-NLS-1$

				final PenData[] penData = signatureDialog.getPenData();
				if (penData != null && penData.length > 0) {
					// collected data!
					// this.signatureImage = createImage(penData, signatureDialog.getCapability(), signatureDialog.getInformation());
				}
				signatureDialog.dispose();
		}
		catch (final Exception e) {
			JOptionPane.showMessageDialog(this,
              e,
              "Error (STU)", //$NON-NLS-1$
              JOptionPane.ERROR_MESSAGE
			);
		}
	}

	DemoButtons() {
		this.setTitle("Wacom STU SDK - Java Sample"); //$NON-NLS-1$
		this.setLayout(new BorderLayout());

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout());

		final JButton btn = new JButton("GetSignature"); //$NON-NLS-1$
		btn.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent evt) {
					try {
						startButtonPress();
					}
					catch (final Throwable e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		);
		panel.add(btn);

		final JPanel image = new JPanel() {

			private static final long serialVersionUID = -566273349113170227L;

			@Override
			public void paintComponent(final Graphics gfx) {
				super.paintComponent(gfx);
				if (DemoButtons.this.signatureImage != null) {
					final double newHeight = (double) DemoButtons.this.signatureImage.getHeight() / DemoButtons.this.signatureImage.getWidth() * this.getWidth();
					final Image rescaled = DemoButtons.this.signatureImage.getScaledInstance(this.getWidth(), (int) newHeight, Image.SCALE_AREA_AVERAGING);
					gfx.drawImage(rescaled, 0, (int) (this.getHeight() / 2 - newHeight / 2), null);
				}
			}
		};

		image.setBorder(
			new TitledBorder(
				new LineBorder(
					new Color(0, 0, 0)
				),
				"Image",  //$NON-NLS-1$
				TitledBorder.LEADING,
				TitledBorder.TOP,
				null,
				Color.BLACK
			)
		);
		image.setPreferredSize(new Dimension(300, 200));

		this.add(panel, BorderLayout.NORTH);
		//    this.add(image, BorderLayout.SOUTH);
		this.pack();
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	static void runProgram() {
		final DemoButtons sample = new DemoButtons();
		sample.setVisible(true);
	}

	/** Main.
	 * @param args
	 * @throws Exception */
	public static void main(final String[] args) throws Exception {
	  EventQueue.invokeLater(
		  new Runnable() {
			  @Override
			  public void run() {
				  runProgram();
			  }
		  }
	  );
	}
}
