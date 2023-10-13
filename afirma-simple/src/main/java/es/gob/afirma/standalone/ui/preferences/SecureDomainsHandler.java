package es.gob.afirma.standalone.ui.preferences;

import java.util.logging.Logger;

import es.gob.afirma.standalone.HttpManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

public class SecureDomainsHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String COMMA_SEPARATOR = ","; //$NON-NLS-1$

	private static final String CRLF = "\r"; //$NON-NLS-1$
	private static final String LINE_BREAK = "\n"; //$NON-NLS-1$

	private final SecureDomainsPanel view;

	public SecureDomainsHandler(final SecureDomainsPanel view) {
		this.view = view;
	}

	/**
	 * Carga la informaci&oacute;n actualmente configurada en la vista.
	 */
	void loadViewData() {
		final String secureDomains = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST);
		if (secureDomains != null) {
			this.view.getSecureDomainsListTA().setText(secureDomains.replaceAll(COMMA_SEPARATOR, LINE_BREAK));
		}
	}

	/**
	 * Guarda la informaci&oacute;n que se indica en la vista.
	 * @throws ConfigurationException
	 */
	void saveViewData() throws ConfigurationException {

		final String textAreaContent = this.view.getSecureDomainsListTA().getText();

		final String secureDomains = textAreaContent.trim()
				.replace(CRLF, COMMA_SEPARATOR).replace(LINE_BREAK, COMMA_SEPARATOR);

		if (secureDomains.isEmpty()) {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST);
		}
		else {
			PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST, secureDomains);
		}

		// Configuramos los dominios seguros para que tenga efecto inmediato
		HttpManager.setSecureDomains(secureDomains);

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			LOGGER.severe("Error al guardar las preferencias de sitios seguros: " + e); //$NON-NLS-1$
		}
	}
}
