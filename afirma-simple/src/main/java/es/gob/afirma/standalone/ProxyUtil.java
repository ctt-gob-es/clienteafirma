/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.IOException;
import java.net.Authenticator;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import com.github.markusbernhardt.proxy.ProxySearch;
import com.github.markusbernhardt.proxy.ProxySearch.Strategy;

import es.gob.afirma.standalone.crypto.CypherDataManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Utilidades para el manejo y establecimiento del <i>Proxy</i> de red para las
 * conexiones de la aplicaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ProxyUtil {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static ProxySelector defaultProxySelector = null;

	private ProxyUtil() {
		// No instanciable
	}

	/** Establece la configuraci&oacute;n para el servidor <i>Proxy</i> seg&uacute;n los valores
     * de configuraci&oacute;n encontrados. */
    public static void setProxySettings() {

    	// Guardamos copia del selector de proxy por defecto de Java
    	if (defaultProxySelector == null) {
    		defaultProxySelector = ProxySelector.getDefault();
    	}

    	// Identificamos si se ha configurador el proxy del sistema, uno personalizado o
    	// si no hay proxy y operamos segun corresponda
    	final ProxyConfig.ConfigType proxyType = getProxyType();

    	final ProxySelector proxySelector = getProxySelector(proxyType);
    	if (proxySelector == null) {
			LOGGER.info("No se usara proxy para las conexiones de red"); //$NON-NLS-1$
			return;
		}

    	// Establecemos el selector del proxy
		ProxySelector.setDefault(proxySelector);

		// Este bloque es solo para el log
		try {
			List<Proxy> proxies = proxySelector.select(new URI("http://www.theregister.co.uk")); //$NON-NLS-1$
			if (proxies.isEmpty() || proxies.get(0).address() == null) {
				LOGGER.info("No se usara proxy para las conexiones HTTP"); //$NON-NLS-1$
			}
			else {
		        final InetSocketAddress addr = (InetSocketAddress) proxies.get(0).address();
				LOGGER.info("Se usara proxy para las conexiones HTTP: " + addr.getHostName() + ":" + addr.getPort()); //$NON-NLS-1$ //$NON-NLS-2$
			}
			proxies = proxySelector.select(new URI("https://www.google.com")); //$NON-NLS-1$
			if (proxies.isEmpty() || proxies.get(0).address() == null) {
				LOGGER.info("No se usara proxy para las conexiones HTTPS"); //$NON-NLS-1$
			}
			else {
		        final InetSocketAddress addr = (InetSocketAddress) proxies.get(0).address();
				LOGGER.info("Se usara proxy para las conexiones HTTPS: " + addr.getHostName() + ":" + addr.getPort()); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		catch (final URISyntaxException e) {
			// No debe pasar
			throw new IllegalStateException("La URI de pruebas del proxy es invalida: " + e, e); //$NON-NLS-1$
		}
    }

    private static ProxyConfig.ConfigType getProxyType() {

    	ProxyConfig.ConfigType proxyType;
    	final String proxyTypeString = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_TYPE);
    	if (proxyTypeString == null) {
    		final boolean proxySelected = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED);
    		if (!proxySelected) {
    			proxyType = ProxyConfig.ConfigType.SYSTEM;
    		}
    		else {
    			final String proxyHost = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST);
        		final String proxyPort = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT);
        		if (proxyHost != null && !proxyHost.trim().isEmpty() &&
        				proxyPort != null && !proxyPort.trim().isEmpty()) {
        			proxyType = ProxyConfig.ConfigType.CUSTOM;
        		}
        		else {
        			proxyType = ProxyConfig.ConfigType.NONE;
        		}
    		}
    	}
    	else {
    		try {
    			proxyType = ProxyConfig.ConfigType.valueOf(proxyTypeString);
    		}
    		catch (final Exception e) {
    			LOGGER.warning("El tipo de proxy configurado no era valido. No se usara proxy: " + e); //$NON-NLS-1$
    			proxyType = ProxyConfig.ConfigType.NONE;
			}
    	}

    	return proxyType;
    }

    /**
     * Obtiene la configuracion actualmente seleccionada (que puede no ser la guardada) del proxy.
     * @param proxyType Tipo de configuraci&oacute;n de proxy.
     * @return Selector de proxy a utilizar.
     */
    private static ProxySelector getProxySelector(final ProxyConfig.ConfigType proxyType) {

    	ProxySelector proxySelector;
    	switch (proxyType) {
		case SYSTEM:
			proxySelector = getSystemProxySelector();
			break;
		case CUSTOM:
			proxySelector = getCustomProxySelector();
			break;
		case NONE:
		default:
			proxySelector = getNoProxySelector();
			break;
		}
    	return proxySelector;
    }

	/**
	 * Obtiene el selector de proxy que emula la configuraci&oacute;n de proxy del sistema.
	 * @return Selector de proxy.
	 */
	private static ProxySelector getSystemProxySelector() {

		// IMPORTANTE: En Java 8 se ha encontrado que, cuando hay un ProxySelector
		// con proxies ya establecido, la deteccion automatica no funciona. Para
		// solucionarlo, eliminamos la configuracion por defecto de proxy que hubiese,
		// identificamos la configuracion del sistema (que puede que mas adelante se
		// establezca) y volvemos a establecer la que hubiese

		// Obtenemos la configuracion establecida y la eliminamos guardando copia
		final ProxySelector proxyDefault = ProxySelector.getDefault();
		ProxySelector.setDefault(getNoProxySelector());

		// Busqueda de proxies configurados en el sistema
		final ProxySearch proxySearch = new ProxySearch();
		proxySearch.addStrategy(Strategy.OS_DEFAULT);
		proxySearch.addStrategy(Strategy.BROWSER);

		final ProxySelector newSelector = proxySearch.getProxySelector();

		// Establecemos el ProxySelector original
		ProxySelector.setDefault(proxyDefault);

		return newSelector;
	}

	private static ProxySelector getCustomProxySelector() {

		final String proxyHost = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST);
		final String proxyPort = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT);
		final String proxyUsername = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME);
		final String cipheredProxyPassword = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD);
		String excludedUrls = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS);

		// Configuracion de host y puerto del proxy
		System.setProperty("http.proxyHost", proxyHost); //$NON-NLS-1$
		System.setProperty("http.proxyPort", proxyPort); //$NON-NLS-1$
		System.setProperty("https.proxyHost", proxyHost); //$NON-NLS-1$
		System.setProperty("https.proxyPort", proxyPort); //$NON-NLS-1$
		System.setProperty("ftp.proxHost", proxyHost); //$NON-NLS-1$
		System.setProperty("ftp.proxyPort", proxyPort); //$NON-NLS-1$
		System.setProperty("socksProxyHost", proxyHost); //$NON-NLS-1$
		System.setProperty("socksProxyPort", proxyPort); //$NON-NLS-1$

		// Listado de excepciones
		if (excludedUrls != null && !excludedUrls.trim().isEmpty()) {
			excludedUrls = excludedUrls.trim();
			System.setProperty("http.nonProxyHosts", excludedUrls); //$NON-NLS-1$
			System.setProperty("https.nonProxyHosts", excludedUrls); //$NON-NLS-1$
			System.setProperty("ftp.nonProxyHosts", excludedUrls); //$NON-NLS-1$
			System.setProperty("socksNonProxyHosts", excludedUrls); //$NON-NLS-1$
		}

		// Se configura si es necesario el usuario y contrasena del proxy
		if (proxyUsername != null && !proxyUsername.trim().isEmpty() &&
				cipheredProxyPassword != null && !cipheredProxyPassword.trim().isEmpty()) {
			char[] proxyPassword;
			try {
				proxyPassword = decipherPassword(cipheredProxyPassword);
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudo descifrar la contrasena del proxy. No se configurara el usuario y contrasena: " + e); //$NON-NLS-1$
				proxyPassword = null;
			}

			// Establecemos a nivel general un autenticador que utilice el usuario y contrasena
			// frente al proxy
			Authenticator auth = null;
			if (proxyPassword != null) {
				auth = getPasswordAuthentication(proxyUsername, proxyPassword);
			}
			Authenticator.setDefault(auth);
		}

		return defaultProxySelector;
	}

	/**
	 * Obtiene un selector de proxy que no configura proxy.
	 * @return Selector de proxy que indica que las conexiones se hagan de forma directa.
	 */
	private static ProxySelector getNoProxySelector() {
		return new NoProxySelector();
	}

    private static final char[] PWD_CIPHER_KEY = new char[] {'8', 'W', '{', 't', '2', 'r', ',', 'B'};

    /** Cifra una contrase&ntilde;a.
     * @param password Contrase&ntilde;a cifrada o <code>null</code> si la contrase&ntilde;a proporcionada es
     *                 nula o vac&iacute;a.
     * @return Contrase&tilde;a cifrada en Base64.
     * @throws GeneralSecurityException Cuando se produce un error durante el cifrado. */
    public static String cipherPassword(final char[] password) throws GeneralSecurityException {
    	if (password == null || password.length < 1) {
    		return null;
    	}
    	return CypherDataManager.cipherData(
			String.valueOf(password).getBytes(StandardCharsets.UTF_8),
			String.valueOf(
				PWD_CIPHER_KEY
			).getBytes(StandardCharsets.UTF_8)
		);
    }

    /** Descifra una contrase&ntilde;a
     * @param cipheredPassword Contrase&ntilde;a cifrada en base 64.
     * @return Contrase&ntilde;a o <code>null</code> si la entrada cifrada era nula o vac&iacute;a.
     * @throws GeneralSecurityException Cuando se produce un error durante el descifrado.
     * @throws IOException Cuando los datos introducidos no son un base 64 v&aacute;lido. */
    public static char[] decipherPassword(final String cipheredPassword) throws GeneralSecurityException, IOException {
    	if (cipheredPassword == null  || cipheredPassword.isEmpty()) {
    		return null;
    	}
    	final byte[] p = CypherDataManager.decipherData(
			cipheredPassword.getBytes(StandardCharsets.UTF_8),
			String.valueOf(
				PWD_CIPHER_KEY
			).getBytes(StandardCharsets.UTF_8)
		);
    	return new String(p, StandardCharsets.UTF_8).toCharArray();
    }

    /**
     * Devuelve el proxy configurado en el sistema (sistema operativo, el navegador o Java)
     * para el acceso a una URL concreta.
     * @param url URL para la que se quiere obtener el proxy a utilizar segun la
     * configuraci&oacute;n del sistema.
     * @return Proxy a utilizar para el acceso a la URL.
     */
	public static Proxy getDefaultProxyToUrl(final String url) {

		Proxy proxy = null;
		final ProxySelector proxySelector = getSystemProxySelector();
		if (proxySelector != null) {
			final URI home = URI.create(url);
			// Listado de proxies disponibles
			final List<Proxy> proxyList = proxySelector.select(home);
			if (proxyList != null && !proxyList.isEmpty()) {
				proxy = proxyList.get(0);
			}
		}
		return proxy;
	}

	/**
	 * Establece la logica de autenticaci&oacute;n frente al proxy del sistema.
	 * @param user Usuario del proxy.
	 * @param pwd Contrase&ntilde;a del proxy.
	 * @return Objeto para la autenticaci&oacute;n del proxy.
	 */
	private static Authenticator getPasswordAuthentication(final String user, final char[] pwd) {
		return new Authenticator() {
			@Override
			protected PasswordAuthentication getPasswordAuthentication() {
				if (getRequestorType() == RequestorType.PROXY) {
					return new PasswordAuthentication(user, pwd);
				}
				return super.getPasswordAuthentication();
			}
		};
	}

	/**
	 * Selector que no utiliza proxy.
	 */
	public static class NoProxySelector extends ProxySelector {

		@Override
		public List<Proxy> select(final URI uri) {
			final List<Proxy> proxies = new ArrayList<>();
			proxies.add(Proxy.NO_PROXY);
			return proxies;
		}

		@Override
		public void connectFailed(final URI uri, final SocketAddress sa, final IOException ioe) {
			LOGGER.warning("No se puede acceder a la URL sin proxy: " + ioe); //$NON-NLS-1$
		}
	}
}
