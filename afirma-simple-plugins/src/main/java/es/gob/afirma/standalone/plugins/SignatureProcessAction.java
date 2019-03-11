package es.gob.afirma.standalone.plugins;

import java.awt.Window;
import java.security.cert.X509Certificate;

/**
 * Acci&oacute;n para el procesado de firmas. Este proceso se invoca desde las pantallas:
 * <ul>
 * <li>Resultado de firma simple.</li>
 * <li>Resultado de firma masiva.</li>
 * <li>Visualizaci&oacute;n de firma.</li>
 * </ul>
 * La informaci&oacute;n que se proporciona a las llamadas a esta interfaz puede variar
 * seg&uacute;n la pantalla desde la que se invoque. As&iacute;, cuando
 * Este proceso no puede implicar ning&uacute;n tipo de cambio sobre las firmas procesadas.
 */
public class SignatureProcessAction extends PluginAction {

	/**
	 * Procesa las firmas resultantes de una operaci&oacute;n de firma o de la que se
	 * visualizan los datos.
	 * @param signatures Firmas cargadas en la aplicaci&oacute;n. Solo habr&aacute; un
	 * elemento cuando se firme un &uacute;nico documento o cuando &uacute;nicamente se
	 * visualice la informaci&oacute;n de una firma. Los datos contenidos en este objeto
	 * pueden variar segun el proceso previo realizado por la aplicaci&oacute;n:
	 * @param signingCert Certificaci&oacute;n utilizado para firmar. Este
	 * valor puede ser {@code null} cuando se invoque el m&eacute;todo por raz&oacute;n
	 * distinta a que se acabe de generar la firma.
	 * @param parent Ventana padre sobre la que poder mostrar di&aacute;ogos
	 * gr&aacute;ficos.
	 */
	public void processSignatures(final OutputData[] signatures, final X509Certificate signingCert, final Window parent) {
		super.start(parent);
	}
}
