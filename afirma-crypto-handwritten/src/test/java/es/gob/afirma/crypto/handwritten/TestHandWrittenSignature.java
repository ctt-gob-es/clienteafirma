package es.gob.afirma.crypto.handwritten;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import javax.swing.JFrame;

import old.es.gob.afirma.crypto.handwritten.JseUtil;
import old.es.gob.afirma.crypto.handwritten.jopensignature.JOpenSignatureSignaturePadManager;
import old.es.gob.afirma.crypto.handwritten.jopensignature.stepover.StepOverSignaturePadManager;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.crypto.handwritten.PdfProcessor.PdfProcessorListener;

/** Pruebas de firma manuscrita.
 * @author Tomas Garc&iacute;a-Mer&aacute;s */
public final class TestHandWrittenSignature {

	/** Pruebas de tableta de firma.
	 * En un contexto JUnit fallan las llamadas nativas con un cierre abrupto de la JVM
	 * @param args
	 * @throws Exception */
	public static void main(final String[] args) throws Exception {
		final byte[] pdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("AppCampus_Tomas_Garcia-Meras.pdf")); //$NON-NLS-1$

		final SignaturePadManager padManager = new StepOverSignaturePadManager(
			JOpenSignatureSignaturePadManager.getSigningDevicesNames()[0]
		);

//		final SignaturePadManager padManager = new TopazSignaturePadManager(
//			TopazSignaturePadManager.getSigningDevicesNames()[0]
//		);

//		final SignaturePadManager padManager = new WacomSignaturePadManager(
//			WacomSignaturePadManager.getSigningDevicesNames()[0]
//		);

		final String htmlLayout = "<html><body><p>Tom&aacute;s Garc&iacute;a-Mer&aacute;s (12345678Z)</p>" + //$NON-NLS-1$
		"<p>Liquidaci&oacute;n positiva en 325.432 €<br>N&uacute;mero de expediente: 5478</p></body></html>"; //$NON-NLS-1$

		System.out.println(padManager.getDeviceScreenWidth());
		System.out.println(padManager.getDeviceScreenHeight());

//		padManager.startHandWrittenSignature(
//			null,
//			JseUtil.html2JpegImage(
//				htmlLayout,
//				padManager.getDeviceScreenWidth(),
//				padManager.getDeviceScreenHeight()
//			),
//			new SimpleSignatureListener() {
//
//				@Override
//				public void signProcessFinished(final byte[] jpegImage) {
//					// TODO Auto-generated method stub
//
//				}
//
//				@Override
//				public void signProcessFailed(final Throwable e) {
//					// TODO Auto-generated method stub
//
//				}
//
//				@Override
//				public void signProcessCancelled() {
//					// TODO Auto-generated method stub
//
//				}
//
//			}
//		);

		final JFrame frame = new JFrame("FRAMEEE"); //$NON-NLS-1$
		frame.setLayout(null);
		frame.setBounds(10,10,900,900);
		final java.awt.Component canvas = (java.awt.Component)padManager.getSignatureCanvas();
		frame.add(canvas);
		canvas.setLocation(0, 0);
		frame.setVisible(true);

		PdfProcessor.processPdf(
			new Signatory("Tomas", "Garcia-Meras", "12345678Z"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			new PadToBeUsedDetails(
				padManager,
				JseUtil.html2JpegImage(
					htmlLayout,
					padManager.getDeviceScreenWidth(),
					padManager.getDeviceScreenHeight()
				),
				null
			),
			new PdfToBeSignedDetails(
				pdf,
				null,
				1
			),
			new PdfProcessorListener() {

				@Override
				public void processFinished(final byte[] pdf1) {
					try {
						final File f = File.createTempFile("TEMPO_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
						final OutputStream fos = new FileOutputStream(f);
						fos.write(pdf1);
						fos.flush();
						fos.close();
						System.out.println(f.getAbsolutePath());
					}
					catch(final Exception e) {
						e.printStackTrace();
					}
				}

				@Override
				public void processFailed(final Throwable e) {
					System.out.println("FALLO ************************"); //$NON-NLS-1$
					e.printStackTrace();
				}

				@Override
				public void processCanceled() {
					System.out.println("CANCELACION ************************"); //$NON-NLS-1$
				}
			}
		);

	}
}
