package es.gob.afirma.core.misc;

import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;

/** Prueba del gestor de registro.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestLogManager {

	/** Prueba de la obtenci&oacute;n del registro.
	 * @throws Exception SI hay cualquier problema durante la prueba */
	@SuppressWarnings("static-method")
	@Test
	public void testLogRetrieve() throws Exception {
		LogManager.install(App.OTHER, null);
		final Logger logger = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
		logger.info("Info"); //$NON-NLS-1$
		logger.warning("Warning"); //$NON-NLS-1$
		logger.severe("Severe"); //$NON-NLS-1$
		System.out.println(LogManager.getLogFile());
	}

}
