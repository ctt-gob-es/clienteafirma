package es.gob.afirma.standalone.ui.preferences;

import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class SecureDomainsHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String COMMA_SEPARATOR = ","; //$NON-NLS-1$

	private static final String LINE_BREAK_SEPARATOR = "\n"; //$NON-NLS-1$

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
			this.view.getSecureDomainsListTA().setText(secureDomains.replaceAll(COMMA_SEPARATOR, LINE_BREAK_SEPARATOR));
		}
	}

	/**
	 * Guarda la informaci&oacute;n que se indica en la vista.
	 * @throws ConfigurationException
	 */
	void saveViewData() throws ConfigurationException {

		String secureDomains = ""; //$NON-NLS-1$
		final String textAreaContent = this.view.getSecureDomainsListTA().getText();

		checkCorrectDomainFormat(textAreaContent);

		if (textAreaContent.indexOf(LINE_BREAK_SEPARATOR) != -1) {
			secureDomains = textAreaContent.replaceAll(LINE_BREAK_SEPARATOR, COMMA_SEPARATOR);
		} else{
			secureDomains = textAreaContent;
		}

		PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST, secureDomains);

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			LOGGER.severe("Error al guardar las preferencias de sitios seguros: " + e); //$NON-NLS-1$
		}
	}

	private static void checkCorrectDomainFormat(final String domainsText) throws ConfigurationException {
		if (domainsText.indexOf(COMMA_SEPARATOR) != -1
				&& domainsText.indexOf(LINE_BREAK_SEPARATOR) != -1) {
    		throw new ConfigurationException(SimpleAfirmaMessages.getString("SecureDomainsDialog.4")); //$NON-NLS-1$
		} else if (domainsText.isEmpty()) {
			throw new ConfigurationException(SimpleAfirmaMessages.getString("SecureDomainsDialog.5")); //$NON-NLS-1$
		}

		String [] domainsArray;
		if (domainsText.indexOf(COMMA_SEPARATOR) != -1) {
			domainsArray = domainsText.split(COMMA_SEPARATOR);
		} else {
			domainsArray = domainsText.split(LINE_BREAK_SEPARATOR);
		}

		final String regex = "^((?!-)[A-Za-z0-9-*]{1,63}(?<!-)\\.)+[A-Za-z*]{1,6}"; //$NON-NLS-1$

	    final Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);

		for (final String domain : domainsArray) {
		    final Matcher matcher = pattern.matcher(domain);
		    final boolean correctFormat = matcher.find();
		    if (!correctFormat) {
		    	throw new ConfigurationException("El dominio " + domain + "tiene un formato incorrecto"); //$NON-NLS-1$ //$NON-NLS-2$
		    }
		}
	}
}
