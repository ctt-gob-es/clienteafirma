package es.gob.afirma.core.afirmaui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.KeyStore.LoadStoreParameter;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStore.ProtectionParameter;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.ConfirmationCallback;
import javax.security.auth.callback.TextInputCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import java.security.Provider;
import java.security.Security;
import java.security.Signature;
import java.util.Enumeration;

import org.junit.Test;

import es.gob.jmulticard.jse.provider.DnieProvider;
import es.gob.jmulticard.ui.passwordcallback.gui.CommonPasswordCallback;
import es.gob.jmulticard.ui.passwordcallback.gui.DnieCallbackHandler;

public class TestCallBackHandler {

	@Test
	public void testProviderWithCallbackHandlerProtection() throws Exception {
		Provider p = new DnieProvider();
		Security.addProvider(p);
		KeyStore ks = KeyStore.getInstance("DNI", p);
		ks.load(
			new LoadStoreParameter() {
				@Override
				public ProtectionParameter getProtectionParameter() {
					return new KeyStore.CallbackHandlerProtection(
							new DnieCallbackHandler()
					);
				}
			}
		);
		
		Enumeration<String> aliases = ks.aliases();
		while (aliases.hasMoreElements()) {
			System.out.println(aliases.nextElement());
		}
		Signature s = Signature.getInstance("SHA1withRSA");
		s.initSign(((PrivateKeyEntry)ks.getEntry("CertFirmaDigital", null)).getPrivateKey());
		s.update("Hola".getBytes());
		s.sign();
	}
	
	@Test
	public void testProviderWithPasswordProtection() throws Exception {
		Provider p = new DnieProvider();
		Security.addProvider(p);
		KeyStore ks = KeyStore.getInstance("DNI", p);
		ks.load(
			new LoadStoreParameter() {
				@Override
				public ProtectionParameter getProtectionParameter() {
					return new KeyStore.PasswordProtection(
						new CommonPasswordCallback("Introduzca el PIN de su DNIe", "PIN").getPassword()
					);
				}
			}
		);
		Enumeration<String> aliases = ks.aliases();
		while (aliases.hasMoreElements()) {
			System.out.println(aliases.nextElement());
		}
		Signature s = Signature.getInstance("SHA1withRSA");
		s.initSign(((PrivateKeyEntry)ks.getEntry("CertFirmaDigital", null)).getPrivateKey());
		s.update("Hola".getBytes());
		s.sign();
	}
	
	@Test
	public void testProviderWithKeyStoreBuilderWithCallbackHandlerProtection() throws Exception {
		final KeyStore.Builder kb = KeyStore.Builder.newInstance(
				"DNI",
				new DnieProvider(),
				new KeyStore.CallbackHandlerProtection(
						new DnieCallbackHandler()
				)
			);
		
		Enumeration<String> aliases = kb.getKeyStore().aliases();
		while (aliases.hasMoreElements()) {
			System.out.println(aliases.nextElement());
		}
		Signature s = Signature.getInstance("SHA1withRSA");
		s.initSign(((PrivateKeyEntry) kb.getKeyStore().getEntry("CertFirmaDigital", null)).getPrivateKey());
		s.update("Hola".getBytes());
		s.sign();
	}
	
	@Test
	public void testProviderWithKeyStoreBuilderWithPasswordProtection() throws Exception {
		final KeyStore.Builder kb = KeyStore.Builder.newInstance(
				"DNI",
				new DnieProvider(),
				new KeyStore.PasswordProtection(
						new CommonPasswordCallback("Introduzca el PIN de su DNIe", "PIN").getPassword()
					)
			);
		Enumeration<String> aliases = kb.getKeyStore().aliases();
		while (aliases.hasMoreElements()) {
			System.out.println(aliases.nextElement());
		}
		Signature s = Signature.getInstance("SHA1withRSA");
		s.initSign(((PrivateKeyEntry) kb.getKeyStore().getEntry("CertFirmaDigital", null)).getPrivateKey());
		s.update("Hola".getBytes());
		s.sign();
	}
}
