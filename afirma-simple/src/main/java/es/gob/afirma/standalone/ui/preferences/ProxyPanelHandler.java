package es.gob.afirma.standalone.ui.preferences;

import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.event.ItemEvent;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.ProxyConfig;
import es.gob.afirma.standalone.ProxyConfig.ConfigType;
import es.gob.afirma.standalone.ProxyUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager.PreferencesSource;

public class ProxyPanelHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String URL_CHECK_CONNECTION = "https://www.google.com"; //$NON-NLS-1$

	private final ProxyPanel view;

	public ProxyPanelHandler(final ProxyPanel view) {
		this.view = view;
	}

	ProxyConfig getProxyConfig() {

		ConfigType proxyType;
		if (this.view.getNoProxyRb().isSelected()) {
			proxyType = ConfigType.NONE;
		}
		else if (this.view.getSystemProxyRb().isSelected()) {
			proxyType = ConfigType.SYSTEM;
		}
		else {
			proxyType = ConfigType.CUSTOM;
		}

		final ProxyConfig config = new ProxyConfig(proxyType);
		config.setHost(this.view.getHostField().getText());
		config.setPort(this.view.getPortField().getText());
		config.setUsername(this.view.getUsernameField().getText());
		config.setPassword(this.view.getPasswordField().getText().toCharArray());
		config.setExcludedUrls(this.view.getExcludedUrlsField().getText());

		return config;
	}


	void registerComponents() {
		this.view.getNoProxyRb().addItemListener(e ->  {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				enableComponentsFromPanel(false, this.view.getManualProxyPanel());
			}
		});
		this.view.getSystemProxyRb().addItemListener(e ->  {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				enableComponentsFromPanel(false, this.view.getManualProxyPanel());
			}
		});
		this.view.getManualProxyRb().addItemListener(e ->  {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				enableComponentsFromPanel(true, this.view.getManualProxyPanel());
			}
		});
		this.view.getDetectProxyButton().addActionListener(e -> {
			detectDefaultProxy();
		});
		this.view.getCheckConnectionButton().addActionListener(e -> {
			final String address = this.view.getHostField().getText();
			final String port = this.view.getPortField().getText();
			checkConnection(address, port);
		});

		// Definimos el formato del campo de puerto del proxy
		final AbstractDocument ad = (AbstractDocument) this.view.getPortField().getDocument();
		ad.setDocumentFilter(
				new DocumentFilter() {
					@Override
					public void insertString(final DocumentFilter.FilterBypass fb, final int offset, final String text, final AttributeSet attr) throws BadLocationException {
						if(fb.getDocument().getLength() + text.length() < 5 && text.matches("\\d+")) { //$NON-NLS-1$
							fb.insertString(offset, text, attr);
						}
					}

					@Override
					public void remove(final DocumentFilter.FilterBypass fb, final int offset, final int length) throws BadLocationException {
						fb.remove(offset, length);
					}

					@Override
					public void replace(final DocumentFilter.FilterBypass fb, final int offset, final int length, final String text, final AttributeSet attrs)throws BadLocationException {
						if(fb.getDocument().getLength() + text.length() < 5 && text.matches("\\d+")) { //$NON-NLS-1$
							fb.insertString(offset, text, attrs);
						}
					}
				}
				);
	}

	/**
	 * Carga la informaci&oacute;n actualmente configurada en la vista.
	 */
	void loadViewData() {

		final String typePreference = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_TYPE);
		ConfigType proxyType;
		try {
			proxyType = ConfigType.valueOf(typePreference);
		}
		catch (final Exception e) {
			proxyType = ConfigType.NONE;
		}

		// XXX: Comprobacion por compatibilidad entre Autofirma 1.7 y anteriores. Si se encuentra configurada
		// la propiedad antigua, ajustamos la configuracion
		if (PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED, PreferencesSource.USER) != null) {
			final boolean selected = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED);
			if (!selected) {
				proxyType = ConfigType.SYSTEM;
			}
			else {
				final String host = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST);
				if (host != null && !host.trim().isEmpty()) {
					proxyType = ConfigType.CUSTOM;
				}
			}
		}

		switch (proxyType) {
		case NONE:
			this.view.getNoProxyRb().setSelected(true);
			enableComponentsFromPanel(false, this.view.getManualProxyPanel());
			break;
		case SYSTEM:
			this.view.getSystemProxyRb().setSelected(true);
			enableComponentsFromPanel(false, this.view.getManualProxyPanel());
			break;
		case CUSTOM:
			this.view.getManualProxyRb().setSelected(true);
			loadCustomConfig();
			break;
		default:
			break;
		}
	}

	void saveViewData() throws ConfigurationException {

		final ProxyConfig config = getProxyConfig();
		if (config.getConfigType() == ConfigType.CUSTOM) {
			final String host = config.getHost();
			final String port = config.getPort();
			if (host == null || host.trim().isEmpty()) {
				LOGGER.warning("El host no puede ser nulo o vacio"); //$NON-NLS-1$
				throw new ConfigurationException(SimpleAfirmaMessages.getString("ProxyDialog.1")); //$NON-NLS-1$
			}
			else if(port == null || port == "") { //$NON-NLS-1$
				LOGGER.warning("El puerto no puede ser nulo, vacio o tener mas de 4 digitos"); //$NON-NLS-1$
				throw new ConfigurationException(SimpleAfirmaMessages.getString("ProxyDialog.3")); //$NON-NLS-1$
			}
			else {
				PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST, host);
				PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT, port);

				// Si no se establece usuario, nos aseguramos de eliminar el actual. Si se establece, lo guardamos.
				if (config.getUsername() == null || config.getUsername().trim().isEmpty()) {
					PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME);
				}
				else {
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME,
							config.getUsername().trim());
				}

				// Si no se establece contrasena, nos aseguramos de eliminar la actual. Si se establece,
				// la guardamos cifrada.
				final char[] password = config.getPassword();
				if (password == null || password.length == 0) {
					PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);
				}
				else {
					try {
						final String cipheredPwd = ProxyUtil.cipherPassword(password);
						if (cipheredPwd != null) {
							PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, cipheredPwd);
						}
					}
					catch (final GeneralSecurityException e) {
						LOGGER.severe("Error cifrando la contrasena del Proxy: " + e); //$NON-NLS-1$
						JOptionPane.showMessageDialog(this.view.getParent(), SimpleAfirmaMessages.getString("ProxyDialog.19")); //$NON-NLS-1$);
						PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, ""); //$NON-NLS-1$
					}
				}

				if (config.getExcludedUrls() != null && !config.getExcludedUrls().trim().isEmpty()) {
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS,
							config.getExcludedUrls().trim().replace(",", "|"));  //$NON-NLS-1$ //$NON-NLS-2$
				}
				else {
					PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS);
				}
			}
		}
		else {
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST);
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT);
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME);
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);
			PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS);
		}
		// Borramos las antiguas propiedades de configuracion
		PreferencesManager.remove(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED);

		// Guardamos el tipo de proxy seleccionado (ninguno, automatico o manual)
		PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_TYPE, config.getConfigType().name());

		try {
			PreferencesManager.flush();
		}
		catch (final Exception e) {
			LOGGER.severe("Error al guardar las preferencias de firma: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Carga la configuraci&oacute;n personal establecida.
	 */
	private void loadCustomConfig() {
		this.view.getHostField().setText(
				PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST));
		this.view.getPortField().setText(
				PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT));
		this.view.getUsernameField().setText(
				PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME));

		final String cipheredPwd = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);

		char[] pwd;
		try {
			pwd = ProxyUtil.decipherPassword(cipheredPwd);
		}
		catch (final Exception e) {
			pwd = null;
			LOGGER.warning("No se pudo descifrar la contrasena del proxy. Se omitira la contrasena: " + e); //$NON-NLS-1$
			JOptionPane.showMessageDialog(this.view, SimpleAfirmaMessages.getString("ProxyDialog.18")); //$NON-NLS-1$);
			// Eliminamos la contrasena mal cifrada de las preferencias
			// Si no la eliminamos, el dialogo de error persistiria aunque tuviesemos sin marcar
			// la casilla de "Usar proxy", obligandonos a establecer una solo para evitar el error
			PreferencesManager.remove(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD
			);
		}

		this.view.getPasswordField().setText(pwd == null ? "" : String.valueOf(pwd)); //$NON-NLS-1$

		final String excludedUrls = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS);
		if (excludedUrls != null) {
			this.view.getExcludedUrlsField().setText(excludedUrls.replace("|", ",")); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private static void enableComponentsFromPanel(final boolean enable, final Container panel) {
		for (final Component c : panel.getComponents()) {
			if (c instanceof Container) {
				c.setEnabled(enable);
				enableComponentsFromPanel(enable, (Container) c);
			}
			else {
				c.setEnabled(enable);
			}
		}
	}

	/** Detecci&oacute;n autom&aacute;tica de proxy. */
	private void detectDefaultProxy() {

		LOGGER.info("Detectando proxies..."); //$NON-NLS-1$


		this.view.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		Proxy proxy = ProxyUtil.getDefaultProxyToUrl(URL_CHECK_CONNECTION);

		if (proxy != null) {
			LOGGER.info("Proxy disponible: " + proxy.toString()); //$NON-NLS-1$
			final SocketAddress address = proxy.address();
			if (address instanceof InetSocketAddress) {
				// Host seleccionado
				this.view.getHostField().setText(
						((InetSocketAddress) address).getHostName()
						);
				// Puerto seleccionado
				this.view.getPortField().setText(
						Integer.toString(((InetSocketAddress) address).getPort())
						);
			}
			else {
				LOGGER.info("El proxy detectado no define una direccion de red"); //$NON-NLS-1$
				proxy = null;
			}
		}
		this.view.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		// Comprobamos si no se puedo identificar el proxy
		if (proxy == null) {
			LOGGER.info("No se han detectado un proxy valido configurado en el sistema"); //$NON-NLS-1$
			AOUIFactory.showMessageDialog(
					this.view,
					SimpleAfirmaMessages.getString("ProxyDialog.14"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("ProxyDialog.11"), //$NON-NLS-1$
					AOUIFactory.ERROR_MESSAGE
					);
		}
	}

	/** Prueba de connexi&oacute;n de red a trav&eacute;s de un hilo de ejecuci&oacute;n.
	 * @param host Direcci&oacute;n del proxy.
	 * @param port Puerto al que conectarse.
	 */
	private void checkConnection(final String host, final String port) {
		new CheckConnectionThread(this.view, host, port).start();
	}

	private static class CheckConnectionThread extends Thread {

		private final Logger SUBLOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

		private final Component parent;
		private final String proxyHost;
		private final String proxyPort;

		public CheckConnectionThread(final Component parent, final String proxyHost, final String proxyPort) {
			this.parent = parent;
			this.proxyHost = proxyHost;
			this.proxyPort = proxyPort;
		}

		@Override
		public void run() {
			this.SUBLOGGER.info("Verificando conexion...."); //$NON-NLS-1$

			if (this.parent != null) {
				this.parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			}
			boolean connectionStatus = false;
			try {
				final Proxy proxy = new Proxy(
						Proxy.Type.HTTP,
						new InetSocketAddress(this.proxyHost, Integer.parseInt(this.proxyPort)));
				final URL url = new URL(URL_CHECK_CONNECTION);

				final HttpURLConnection uc = (HttpURLConnection) url.openConnection(proxy);
				uc.connect();
				connectionStatus = true;
			}
			catch (final Exception e) {
				this.SUBLOGGER.log(Level.WARNING, "Error conectando con el proxy: " + e, e); //$NON-NLS-1$
			    connectionStatus = false;
			}

			if (connectionStatus) {
				this.SUBLOGGER.info("Conexion proxy correcta"); //$NON-NLS-1$
				AOUIFactory.showMessageDialog(
					this.parent,
					SimpleAfirmaMessages.getString("ProxyDialog.9"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("ProxyDialog.8"), //$NON-NLS-1$
					AOUIFactory.INFORMATION_MESSAGE
				);
			}
			else {
				this.SUBLOGGER.info("Conexion proxy incorrecta"); //$NON-NLS-1$
				AOUIFactory.showMessageDialog(
					this.parent,
					SimpleAfirmaMessages.getString("ProxyDialog.10"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("ProxyDialog.8"), //$NON-NLS-1$
					AOUIFactory.ERROR_MESSAGE
				);
			}
			if (this.parent != null) {
				this.parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
		}
	}
}
