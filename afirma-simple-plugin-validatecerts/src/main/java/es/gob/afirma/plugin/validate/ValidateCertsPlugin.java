package es.gob.afirma.plugin.validate;

import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.PluginInfo;

/**
 * Plugin para permitir la validaci&oacute;n de certificados.
 */
public class ValidateCertsPlugin extends AfirmaPlugin {

	private static final String INTERNAL_NAME = "validate_certs"; //$NON-NLS-1$
	private static final int VERSION_CODE = 1;

	@Override
	public PluginInfo getInfo() {

		final PluginInfo info = new PluginInfo(INTERNAL_NAME, Messages.getString("ValidateCertsInfo.0")); //$NON-NLS-1$
		info.setDescription(Messages.getString("ValidateCertsInfo.1")); //$NON-NLS-1$
		info.setVersionCode(VERSION_CODE);
		info.setVersion(Messages.getString("ValidateCertsInfo.2")); //$NON-NLS-1$
		info.setAuthors(new String[] {Messages.getString("ValidateCertsInfo.3")}); //$NON-NLS-1$

		return info;
	}

}
