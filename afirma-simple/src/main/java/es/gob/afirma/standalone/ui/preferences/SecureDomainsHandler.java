package es.gob.afirma.standalone.ui.preferences;

import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

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

		final String secureDomains = textAreaContent
				.replace(CRLF, COMMA_SEPARATOR).replace(LINE_BREAK, COMMA_SEPARATOR);

		checkCorrectDomainFormat(secureDomains);

		PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST, secureDomains);

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			LOGGER.severe("Error al guardar las preferencias de sitios seguros: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Comprueba que el formato de los dominios indicados sea el correcto.
	 * @param domainsText Texto con todos los dominios.
	 * @throws ConfigurationException Error en caso de formato incorrecto en la lista de dominios.
	 */
	private static void checkCorrectDomainFormat(final String domainsText) throws ConfigurationException {
		if (domainsText.isEmpty()) {
			throw new ConfigurationException(SimpleAfirmaMessages.getString("SecureDomainsDialog.5")); //$NON-NLS-1$
		}

		final String [] domainsArray = domainsText.split(COMMA_SEPARATOR);

		final String regex = "^((?!-)[A-Za-z0-9-*]{1,63}(?<!-)\\.)+[A-Za-z*]{1,6}"; //$NON-NLS-1$

	    final Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);

		for (final String domain : domainsArray) {
		    final Matcher matcher = pattern.matcher(domain);
		    final boolean correctFormat = matcher.find();
		    if (!correctFormat) {
		    	throw new ConfigurationException("El dominio " + domain + " tiene un formato incorrecto"); //$NON-NLS-1$ //$NON-NLS-2$
		    }
		}
	}
}
