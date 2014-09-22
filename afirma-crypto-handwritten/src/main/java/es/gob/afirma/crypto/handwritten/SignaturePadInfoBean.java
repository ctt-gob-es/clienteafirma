package es.gob.afirma.crypto.handwritten;

/** Informaci&oacute;n sobre una tableta de captura de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignaturePadInfoBean {

	private final String vendor;
	private final String model;
	private final String firmware;
	private final int reportRate;
	private final int screenWidth;
	private final int screenHeight;
	private final int maxPressure;
	private final int resolution;

	/** Crea la informaci&oacute;n sobre una tableta de captura de firmas.
	 * @param vid Nombre del fabricante.
	 * @param pid Modelo de la tableta.
	 * @param fw Versi&oacute;n de firmware.
	 * @param rate Frecuencia de captura (m&aacute;xima).
	 * @param width Ancho de la pantalla de captura.
	 * @param height Alto de la pantalla de captura.
	 * @param pressure M&aacute;ximo nivel de presi&oacute;n capturado.
	 * @param res resoluci&oacute;n de la superficie de captura. */
	public SignaturePadInfoBean(final String vid,
			                    final String pid,
			                    final String fw,
			                    final int rate,
			                    final int width,
			                    final int height,
			                    final int pressure,
			                    final int res) {
		this.vendor = vid;
		this.model = pid;
		this.firmware = fw;
		this.reportRate = rate;
		this.screenHeight = height;
		this.screenWidth = width;
		this.maxPressure = pressure;
		this.resolution = res;
	}

	@Override
	public String toString() {
		return new StringBuilder()
			.append("fabricante=") //$NON-NLS-1$
			.append(this.vendor)
			.append("; modelo=") //$NON-NLS-1$
			.append(this.model)
			.append("; firmware=") //$NON-NLS-1$
			.append(this.firmware)
			.append("; frecuencia=") //$NON-NLS-1$
			.append(this.reportRate)
			.append("; ancho=") //$NON-NLS-1$
			.append(this.screenWidth)
			.append("; alto=") //$NON-NLS-1$
			.append(this.screenHeight)
			.append("; presion=") //$NON-NLS-1$
			.append(this.maxPressure)
			.append("; resolucion=") //$NON-NLS-1$
			.append(this.resolution)
				.toString();
	}

}
