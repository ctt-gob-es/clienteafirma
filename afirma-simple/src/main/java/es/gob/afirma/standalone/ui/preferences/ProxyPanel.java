package es.gob.afirma.standalone.ui.preferences;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.net.URL;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import com.github.markusbernhardt.proxy.ProxySearch;
import com.github.markusbernhardt.proxy.ProxySearch.Strategy;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel para el di&aacute;logo de configuraci&oacute; de proxy. */
final class ProxyPanel extends JPanel{

	private static final long serialVersionUID = -5919574790093890970L;
	private static final int PREFERRED_WIDTH = 420;
	private static final int PREFERRED_HEIGHT = 210;

	static final String URL_CHECK_CONNECTION = "http://www.google.com"; //$NON-NLS-1$
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JTextField portTF = new JTextField();

	/** Obtiene el valor del campo de texto definido para el puerto.
	 * @return Valor del campo de texto definido para el puerto.*/
	String getPort() {
		return this.portTF.getText();
	}

	private final JTextField hostTF = new JTextField();

	/** Obtiene el valor del campo de texto definido para el host.
	 * @return Valor del campo de texto definido para el host.*/
	String getHost() {
		return this.hostTF.getText();
	}

	private final JTextField usernameProxy = new JTextField();
	String getUsername() {
		return this.usernameProxy.getText();
	}

	private final JPasswordField passwordProxy = new JPasswordField();
	String getPassword() {
		return this.passwordProxy.getPassword().toString();
	}

	private final JCheckBox proxyCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("ProxyDialog.4") //$NON-NLS-1$
	);

	boolean isProxySelected() {
		return this.proxyCheckBox.isSelected();
	}

	private final JButton checkConnectionBtn = new JButton(SimpleAfirmaMessages.getString("ProxyDialog.8")); //$NON-NLS-1$

	private final JButton autodetectProxyBtn = new JButton(SimpleAfirmaMessages.getString("ProxyDialog.11")); //$NON-NLS-1$

	/** Constructor	 */
	ProxyPanel() {
		setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.insets = new Insets(1,1,10,1);
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridwidth = 3;
		c.gridx = 0;
		c.gridy = 0;

		setMinimumSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("ProxyDialog.12") //$NON-NLS-1$
    	);

		final JLabel infoLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.15")); //$NON-NLS-1$

		this.proxyCheckBox.setMnemonic('U');
		this.proxyCheckBox.setSelected(PreferencesManager.getBoolean(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED,
				false
			)
		);
		this.proxyCheckBox.addItemListener(
			new ItemListener() {
				@Override
				public void itemStateChanged(final ItemEvent e) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						enableComponents(true);
					}
					else {
						enableComponents(false);
					}
				}
			}
		);

		// host
		final JLabel hostLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.5")); //$NON-NLS-1$
		hostLabel.setLabelFor(this.hostTF);

		// Puerto
		final JLabel portLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.7")); //$NON-NLS-1$
		portLabel.setLabelFor(this.portTF);

		//user name
		final JLabel usernameLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.16")); //$NON-NLS-1$
		usernameLabel.setLabelFor(this.usernameProxy);

		//password
		final JLabel passwordLabel = new JLabel(SimpleAfirmaMessages.getString("ProxyDialog.17")); //$NON-NLS-1$
		passwordLabel.setLabelFor(this.passwordProxy);

		final AbstractDocument ad = (AbstractDocument) this.portTF.getDocument();
		ad.setDocumentFilter(
			new DocumentFilter() {
		       @Override
		        public void insertString(final DocumentFilter.FilterBypass fb, final int offset, final String text, final AttributeSet attr) throws BadLocationException
		        {
		            if(fb.getDocument().getLength() + text.length() < 5 && text.matches("\\d+")) { //$NON-NLS-1$
		            	fb.insertString(offset, text, attr);
		            }
		        }

		        @Override
		        public void remove(final DocumentFilter.FilterBypass fb, final int offset, final int length) throws BadLocationException
		        {
		            fb.remove(offset, length);
		        }

		        @Override
		        public void replace(final DocumentFilter.FilterBypass fb, final int offset, final int length, final String text, final AttributeSet attrs)throws BadLocationException
		        {
	                 if(fb.getDocument().getLength() + text.length() < 5 && text.matches("\\d+")) { //$NON-NLS-1$
	                	 fb.insertString(offset, text, attrs);
	                 }
		        }
			}
		);
		this.checkConnectionBtn.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("ProxyDialog.6") //$NON-NLS-1$
    	);
		this.checkConnectionBtn.setMnemonic('V');
		this.checkConnectionBtn.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					LOGGER.info("verificar"); //$NON-NLS-1$
					ProxyPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
					final String host = ProxyPanel.this.getHost();
					if(testConnection(host, ProxyPanel.this.getPort())){
						LOGGER.info("Conexion proxy correcta"); //$NON-NLS-1$
						AOUIFactory.showMessageDialog(
							getParent(),
							SimpleAfirmaMessages.getString("ProxyDialog.9"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("ProxyDialog.8"), //$NON-NLS-1$
							AOUIFactory.INFORMATION_MESSAGE
						);
					}
					else {
						LOGGER.info("Conexion proxy incorrecta"); //$NON-NLS-1$
						AOUIFactory.showMessageDialog(
							getParent(),
							SimpleAfirmaMessages.getString("ProxyDialog.10"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("ProxyDialog.8"), //$NON-NLS-1$
							AOUIFactory.ERROR_MESSAGE
						);
					}
					ProxyPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
			}
		);

		this.autodetectProxyBtn.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("ProxyDialog.13") //$NON-NLS-1$
    	);
		this.autodetectProxyBtn.setMnemonic('A');
		this.autodetectProxyBtn.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(
				final ActionEvent e) {
					LOGGER.info("verificar"); //$NON-NLS-1$
					ProxyPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
					fillWithDefaultProxy();
					ProxyPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				}
			}
		);


		this.add(this.proxyCheckBox, c);
		c.gridy++;
		c.insets = new Insets(10, 30, 0, 0);
		this.add(infoLabel, c);
		c.gridwidth = 1;
		c.weightx = 0.2;
		c.gridy++;
		this.add(hostLabel, c);
		c.gridwidth = GridBagConstraints.LAST_LINE_END;
		c.gridx++;
		c.gridwidth = 2;
		c.weightx = 1.0;
		this.add(this.hostTF,c);
		c.gridwidth = 1;
		c.weightx = 0.2;
		c.insets = new Insets(0, 30, 0, 0);
		c.gridy++;
		c.gridx--;
		this.add(portLabel, c);
		c.gridx++;
		c.gridwidth = 2;
		c.weightx = 1.0;
		this.add(this.portTF, c);
		c.gridwidth = 1;
		c.weightx = 0.2;
		c.insets = new Insets(0, 30, 0, 0);
		c.gridy++;
		c.gridx--;
		this.add(usernameLabel, c);
		c.gridx++;
		c.gridwidth = 2;
		c.weightx = 1.0;
		this.add(this.usernameProxy, c);
		c.gridwidth = 1;
		c.weightx = 0.2;
		c.insets = new Insets(0, 30, 0, 0);
		c.gridy++;
		c.gridx--;
		this.add(passwordLabel, c);
		c.gridx++;
		c.gridwidth = 2;
		c.weightx = 1.0;
		this.add(this.passwordProxy, c);
		c.gridwidth = 1;
		c.gridy++;
		this.add(this.checkConnectionBtn, c);
		c.gridx++;
		this.add(this.autodetectProxyBtn, c);

		this.hostTF.setText(PreferencesManager.get(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST,
				"" //$NON-NLS-1$
			)
		);
		this.portTF.setText(PreferencesManager.get(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT,
				"" //$NON-NLS-1$
			)
		);
		enableComponents(this.proxyCheckBox.isSelected());
	}

	/** Detecci&oacute;n autom&aacute;tica de proxy. */
	void fillWithDefaultProxy() {

		LOGGER.info("Detectando proxies..."); //$NON-NLS-1$
		// Busqueda de proxies configurados en el sistema
		final ProxySearch proxySearch = new ProxySearch();
		proxySearch.addStrategy(Strategy.OS_DEFAULT);
		proxySearch.addStrategy(Strategy.JAVA);
		proxySearch.addStrategy(Strategy.BROWSER);

		final ProxySelector proxySelector = proxySearch.getProxySelector();
		ProxySelector.setDefault(proxySelector);
		if(proxySelector != null) {
			final URI home = URI.create(URL_CHECK_CONNECTION);
			// Listado de proxies disponibles
			final List<Proxy> proxyList = proxySelector.select(home);
			if (proxyList != null && !proxyList.isEmpty()) {
				LOGGER.info("Proxy disponible: " + proxyList.get(0).toString()); //$NON-NLS-1$
				final SocketAddress address = proxyList.get(0).address();
				if (address instanceof InetSocketAddress) {
					// Host seleccionado
					this.hostTF.setText(
						((InetSocketAddress) address).getHostName()
					);
					// Puerto seleccionado
					this.portTF.setText(
						Integer.toString(((InetSocketAddress) address).getPort())
					);
					return;
				}
			}
		}
		LOGGER.info("No se han detectado proxies en el sistema"); //$NON-NLS-1$
		AOUIFactory.showMessageDialog(
			getParent(),
			SimpleAfirmaMessages.getString("ProxyDialog.14"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("ProxyDialog.11"), //$NON-NLS-1$
			AOUIFactory.ERROR_MESSAGE
		);
	}

	/** Test de connexi&oacute;n de red.
	 * @param addr Direcci&oacute;n del proxy.
	 * @param port Puerto al que conectarse.
	 * @return estado de la conexi&oacute;n: <code>true</code> si la conexi&oacute;n es correcta y <code>false</code> en caso contrario.*/
	static boolean testConnection(final String addr, final String port) {

		boolean connectionStatus = false;
		LOGGER.info("Verificando conexion...."); //$NON-NLS-1$

		try {
			final Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(addr,Integer.parseInt(port)));
			final URL url = new URL("http://www.yahoo.com"); //$NON-NLS-1$

			final HttpURLConnection uc = (HttpURLConnection)url.openConnection(proxy);
			uc.connect();
			connectionStatus = true;
		}
		catch (final Exception e) {
		    LOGGER.log(Level.WARNING, "Error conectando con el proxy: " + e, e); //$NON-NLS-1$
		    connectionStatus = false;
		}

		return connectionStatus;
	}

	void enableComponents(final boolean enable) {
		this.autodetectProxyBtn.setEnabled(enable);
		this.checkConnectionBtn.setEnabled(enable);
		this.portTF.setEnabled(enable);
		this.hostTF.setEnabled(enable);
		this.usernameProxy.setEnabled(enable);
		this.passwordProxy.setEnabled(enable);
	}
}
