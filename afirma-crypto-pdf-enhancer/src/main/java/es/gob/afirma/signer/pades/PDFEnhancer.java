package es.gob.afirma.signer.pades;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.platform.wsclientoriginal.TestClient;
import es.gob.afirma.signers.padestri.server.SignEnhancer;

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
public class PDFEnhancer implements SignEnhancer{

	private static final String APPLICATION_NAME_OPTION = "appName"; //$NON-NLS-1$

	private static final String SIGN_TYPE_OPTION = "signType"; //$NON-NLS-1$

	public byte[] enhance(final byte[] signature, final Properties options) throws IOException {

        //TODO: Sustituir por cliente de plataforma "limpio"
		return TestClient.upgradeSign(
				signature,
				options.getProperty(APPLICATION_NAME_OPTION),
				options.getProperty(SIGN_TYPE_OPTION));
        //return TestClient.upgradeSign(signature, "dipucr.sigem", "A"); //$NON-NLS-1$ //$NON-NLS-2$
	}



}
