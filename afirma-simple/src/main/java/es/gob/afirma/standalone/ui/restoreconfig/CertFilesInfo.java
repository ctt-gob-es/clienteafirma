package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.File;

/**
 * Clase con las referencias de los ficheros de los certificados y almacenes de la
 * configuraci&oacute;n SSL de Autofirma.
 */
public class CertFilesInfo {

	/**
	 * Fichero del certificado SSL.
	 */
	private File sslCertFile;

	/**
	 * Fichero del certificado de la autoridad de confianza con el que se gener&oacute;
	 * el certificado SSL.
	 */
	private File sslRootFile;

	/**
	 * Fichero del almac&eacute;n con la clave y el certificado SSL.
	 */
	private File sslKeyStoreFile;

	/**
	 * Obtiene el fichero del certificado SSL.
	 * @return Fichero del certificado SSL.
	 */
	public File getSslCertFile() {
		return this.sslCertFile;
	}

	/**
	 * Establece el fichero del certificado SSL.
	 * @param sslCertFile Fichero del certificado SSL.
	 */
	public void setSslCertFile(final File sslCertFile) {
		this.sslCertFile = sslCertFile;
	}

	/**
	 * Obtiene el fichero del certificado de CA con el que se genero el certificado SSL.
	 * @return Fichero del certificado de CA.
	 */
	public File getSslRootFile() {
		return this.sslRootFile;
	}

	/**
	 * Establece el fichero del certificado de CA con el que se genero el certificado SSL.
	 * @param sslRootFile Fichero del certificado de CA.
	 */
	public void setSslRootFile(final File sslRootFile) {
		this.sslRootFile = sslRootFile;
	}

	/**
	 * Obtiene el fichero del almac&eacute;n del certificado SSL.
	 * @return Fichero del almac&eacute;n del certificado SSL.
	 */
	public File getSslKeyStoreFile() {
		return this.sslKeyStoreFile;
	}

	/**
	 * Establece el fichero del almac&eacute;n del certificado SSL.
	 * @param sslKeyStoreFile Fichero del almac&eacute;n del certificado SSL.
	 */
	public void setSslKeyStoreFile(final File sslKeyStoreFile) {
		this.sslKeyStoreFile = sslKeyStoreFile;
	}


}
