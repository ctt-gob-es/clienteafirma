package es.gob.afirma.signers.pades.enhancer.afirmaplatform;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.signers.pades.SignEnhancer;

/**
 * Clase para la mejora de las firmas PDF a trav&eacute;s del servicio de actualizaci&oacute;n de
 * la plataforma @firma.<br/>
 * Deben proporcionarse como opciones:
 * <ul>
 * <li>appName: Nombre de la aplicaci&oacute;n que solicita la mejora (debe estar dado de
 * alta en la plataforma y cumplir los requisitos establecidos).</li>
 * <li>signType: Tipo de firma: "A": Archivo longevo, "T": Con sello de tiempo,...</li>
 * </ul>
 */
public class PDFEnhancer implements SignEnhancer {

	/**
	 * Valor clave para la configuraci&oacute;n del nombre de aplicaci&oacute;n que accede
	 * a la plataforma @firma.
	 */
	public static final String APPLICATION_NAME_OPTION = "appName"; //$NON-NLS-1$

	/**
	 * Valor clave para la configuraci&oacute;n del tipo de firma extendida que se desea generar.
	 */
	public static final String SIGN_TYPE_OPTION = "signType"; //$NON-NLS-1$

	@Override
	public byte[] enhance(final byte[] signature, final Properties options) throws IOException {

		return AFirmaPlatformPdfEnhancer.upgradeSign(
				signature,
				options.getProperty(APPLICATION_NAME_OPTION),
				options.getProperty(SIGN_TYPE_OPTION));
	}



}
