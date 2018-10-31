package es.gob.afirma.standalone.ui;

import java.awt.image.BufferedImage;

/**
 * Informaci&oacute;n recopilada sobre un fichero.
 */
public class DataFileInfo {

	private String extension = null;
	private String description = null;
	private int size = 0;
	private byte[] data = null;
	private BufferedImage icon = null;
	private boolean executable = false;

	/**
	 * Recupera la extensi&oacute;n que com&uacute;nmente muestra este tipo de fichero.
	 * @return Extesi&oacute;n de fichero o {@code null} si no se conoce.
	 */
	public String getExtension() {
		return this.extension;
	}

	/**
	 * Establece la extensi&oacute;n que com&uacute;nmente muestra este tipo de fichero.
	 * @param extension Extesi&oacute;n de fichero.
	 */
	public void setExtension(String extension) {
		this.extension = extension;
	}

	/**
	 * Recupera la descripci&oacute; del tipo de fichero.
	 * @return Descripcion del tipo de fichero o {@code null} si no se conoce.
	 */
	public String getDescription() {
		return this.description;
	}

	/**
	 * Establece la descripci&oacute;n del tipo de fichero.
	 * @param description Descripci&oacute;n del tipo de fichero.
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * Recupera el tama&ntilde;o de los datos.
	 * @return Tama&ntilde;o del tipo de los datos o 0 si no se estableci&oacute;.
	 */
	public int getSize() {
		return this.size;
	}

	/**
	 * Recupera el tama&ntilde;o de los datos.
	 * @param size Tama&ntilde;o de los datos.
	 */
	public void setSize(int size) {
		this.size = size;
	}

	/**
	 * Recupera el contenido del fichero.
	 * @return Contenido del fichero.
	 */
	public byte[] getData() {
		return this.data;
	}

	/**
	 * Establece el contenido del fichero.
	 * @param data Contenido del fichero.
	 */
	public void setData(byte[] data) {
		this.data = data;
	}

	/**
	 * Recupera la imagen del icono con el que se visualiza el fichero.
	 * @return Icono del fichero.
	 */
	public BufferedImage getIcon() {
		return this.icon;
	}

	/**
	 * Establece la imagen del icono con el que se visualiza el fichero.
	 * @param icon Icono del fichero.
	 */
	public void setIcon(final BufferedImage icon) {
		this.icon = icon;
	}

	/**
	 * Indica si se identifican los datos como ejecutables.
	 * @return {@code true} si los datos son ejecutables, {@code false}
	 * si no lo son o si se desconoce el tipo.
	 */
	public boolean isExecutable() {
		return this.executable;
	}

	/**
	 * Establece si los datos son ejecutables o no.
	 * @param executable {@code true} si los datos son ejecutables, {@code false}
	 * si no lo son.
	 */
	public void setExecutable(final boolean executable) {
		this.executable = executable;
	}
}
