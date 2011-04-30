/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.awt.Component;
import java.awt.Dimension;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.security.auth.callback.PasswordCallback;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.atosorigin.AOCertVerifier;
import es.gob.afirma.Messages;
import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOCMSSigner;

/**
 * Clase para la firma electr&oacute;nica de cadenas de texto simulando el funcionamiento del m&eacute;todo
 * <code>[window.]crypto.signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])</code>
 * de JavaScript en navegadores Mozilla / Firefox.
 */
public final class SignText {
	
	
	private static final String USER_CANCEL = "error:userCancel";
	private static final String INTERNAL_ERROR = "error:internalError";
	private static final String NO_MATCHING_CERT = "error:noMatchingCert";
	
	private final String[] alias;
	private final AOKeyStoreManager kss;
	private final Component parent;
	private String result = USER_CANCEL;
	private boolean useCAdES = false;
	
	/**
	 * Obtiene el resultado de la operaci&oacute;n <code>SignText</code>.
	 * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta termin&oacute; correctamente se devuelve el objeto firmado
	 *         en formato CMS (por defecto) o CAdES codificado en Base64. En caso contrario devuelve uno de los siguientes c&oacute;digos
	 *         de error:
	 *         <br>
	 *         <ul>
	 *          <li>
	 *           <code>error:noMatchingCert</code> si el usuario no dispone de ning&uacute;n certificado
	 *           emitido por las CA indicadas.
	 *          </li> 
	 *          <li>
	 *           <code>error:userCancel</code> si el usuario cancela la operaci&oacute;n.
	 *          </li>
	 *          <li>
	 *           <code>error:internalError</code> si ocurre cualquier error durante el proceso.
	 *          </li>
	 *         </ul>
	 */
	public String getResult() {
		return result;
	}
	
	/**
	 * Establece si debe usarse CAdES en vez de CMS para la firma.
	 * @param useit <code>true</code> para usar CAdES, <code>false</code> (por defecto) para usar CMS
	 */
	public void setUseCAdES(boolean useit) {
		useCAdES = useit;
	}
	
	/**
	 * Firma una cadena de texto simulando el funcionamiento del m&eacute;todo
	 * <code>[window.]crypto.signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])</code>
	 * de JavaScript en navegadores Mozilla / Firefox.
	 * @param stringToSign El texto a firmar. Si se especifica <code>"ask"</code> en el par&aacute;metro
	 *                     <code>caOption</code> se presenta el texto al usuario en un di&aacute;logo de una
	 *                     forma legible
	 * @param caOption Puede ser una de las siguientes opciones:
	 *                 <br>
	 *                 <ul>
	 *                  <li>
	 *                   <code>"auto"</code> indica que el programa seleccionar&aacute; autom&aacute;ticamente
	 *                   un certificado. Si se ha indicado uno o m&aacute;s nombre de CA mediante los par&aacute;metros
	 *                   <code>caName<i>N</i></code> la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las
	 *                   CA indicadas. 
	 *                  </li>
	 *                  <li>
	 *                   <code>"ask"</code> indica que se debe solicitar al usuario que seleccione un certificado.
	 *                   Si se ha indicado uno o m&aacute;s nombre de CA mediante los par&aacute;metros
	 *                   <code>caName<i>N</i></code> la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las
	 *                   CA indicadas.
	 *                  </li>
	 *                 </ul>
	 * @param caNameN DN de las CA cuyos certificados deben tenerse en cuenta para la selecci&oacute;n de firmante,
	 *                debe proporcionarse un par&aacute;metro por cada CA. Para mayor informaci&oacute;n sobre el
	 *                formato DN consulte <a href="http://www.faqs.org/rfcs/rfc1485.html">String Representation of Distinguished Names</a>.
	 */
	public void signText(final String stringToSign, final String caOption, final String... caNameN) {
		
		if (stringToSign == null || "".equals(stringToSign)) {
			Logger.getLogger("es.gob.afirma").warning(
				"No se ha proporcionado un texto para firmar"
			);
			result = INTERNAL_ERROR;
			return;
		}
		
		if (!("ask".equals(caOption)) && !("auto".equals(caOption))) {
			Logger.getLogger("es.gob.afirma").severe(
				"Valor incorrecto para caOption, debe ser 'ask' u 'auto': " + caOption
			);
			result = INTERNAL_ERROR;
			return;
		}
		
		if (alias == null || alias.length < 1) {
			result = NO_MATCHING_CERT;
			return;
		}
		
		// Gestionamos el filtro de CAs
		
		String issuerFilter = null;
		
		if (caNameN != null && caNameN.length > 0) {
			final StringBuilder filter = new StringBuilder();
			LdapName dn;
			String ca;
			for (int j=0;j<caNameN.length;j++) {
				ca = caNameN[j];
				try {
					dn = new LdapName(ca);
				}
				catch(final Throwable e) {
					Logger.getLogger("es.gob.afirma").warning(
						"DN de CA para filtro mal formado ('" + ca + "'), se ignorara: " + e
					);
					continue;
				}
				if (dn.getRdns().size() > 0) {
					filter.append("(");
					for (int i=0;i<dn.getRdns().size();i++) {
						filter.append(dn.getRdns().get(i).toString());
						if (i != dn.getRdns().size()-1) filter.append(" & ");
					}
					filter.append(")");
				}
				
				if (j != caNameN.length-1) filter.append(" | ");
			}
			issuerFilter = filter.toString();
		}
		Logger.getLogger("es.gob.afirma").info("Se utilizara el siguiente filtro de emisor: " + issuerFilter);
		
		// Obtenemos la lista de nombres descriptivos de los alias de los certificados
		final Hashtable<String, String> aliasesByFriendlyName;
		try {
			aliasesByFriendlyName = KeyStoreUtilities.getAlisasesByFriendlyName(
				alias, 
				kss.getKeyStores(),
				null, // keyUsageFilter 
				true, // checkPrivateKeys
				true, // checkValidity
				true, // showExpiredCertificates
				issuerFilter, 
				null  // subjectFilter
			);
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
				"Error obteniendo los nombres descriptivos de los alias: " + e
			);
			result = INTERNAL_ERROR;
			return;
		}
		
		// Miramos si despues de filtrar las entradas queda alguna o se ha quedado la lista vacia
		if (aliasesByFriendlyName.size() == 0) {
			result = NO_MATCHING_CERT;
			return;
		}

		// En "auto" seleccionamos automaticamente el primero de la lista
		if ("auto".equals(caOption)) {
			final Hashtable<String, String> tmpHash = new Hashtable<String, String>(1);
			final String key = aliasesByFriendlyName.keys().nextElement();
			tmpHash.put(key, aliasesByFriendlyName.get(key));
			createUI(stringToSign, tmpHash);
		}
		else {
			createUI(stringToSign, aliasesByFriendlyName);
		}
		
		return;
	}
	
	private String firmar(final String stringToSign, final PrivateKeyEntry keyEntry, final X509Certificate cert) {
		if (stringToSign == null) {
			Logger.getLogger("es.gob.afirma").severe("El texto a firmar no puede ser nulo");
			return INTERNAL_ERROR;
		}
		if (keyEntry == null) {
			Logger.getLogger("es.gob.afirma").severe("La clave privada de firma no puede ser nula");
			return NO_MATCHING_CERT;
		}
		if (cert == null) {
			Logger.getLogger("es.gob.afirma").severe("El certificado de firma no puede ser nulo");
			return NO_MATCHING_CERT;
		}
 		try {
			if (!useCAdES) return AOCryptoUtil.encodeBase64(
				new AOCMSSigner().sign(
					stringToSign.getBytes(), 
					AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, 
					keyEntry, 
					null
				),
				false
			);
			return AOCryptoUtil.encodeBase64(
				new AOCAdESSigner().sign(
					stringToSign.getBytes(), 
					AOConstants.SIGN_ALGORITHM_SHA1WITHRSA, 
					keyEntry, 
					null
				),
				false
			);
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
				"Error creando la firma: " + e
			);
			return INTERNAL_ERROR;
		}
	}
	
	private final PasswordCallback passCbk;
	
	/**
	 * Crea un objeto para la firma de Texto simulando el funcionamiento de <code>crypto.signText()</code>
	 * @param aliases Alias para mostrar en el men&uacute; desplegable de selecci&oacute;n de certificado
	 * @param ksm Gestor de KeyStores, para convertir los alias en nombres decriptivos
	 * @param parentComponent Componente padre para la modalidad
	 * @param pc PassWord callback si el KeyStore necesita contrase&ntilde;a. Si de especifica <code>null</code> se utilizara un <code>NullPasswordCallback</code>
	 */
	public SignText(final String[] aliases, 
			        final AOKeyStoreManager ksm, 
			        final Component parentComponent,
			        final PasswordCallback pc) {
		alias = aliases;
		kss = ksm;
		parent = parentComponent;
		if (pc == null) passCbk = new NullPasswordCallback();
		else passCbk = pc;
	}

	private void createUI(final String stringToSign, final Hashtable<String, String> aliasesByFriendlyName) {
		
		final JTextArea textArea = new JTextArea(stringToSign);
		textArea.setEditable(false);

		final JScrollPane scrollPane = new JScrollPane(textArea);
		scrollPane.setMinimumSize(new Dimension(400,300));
		scrollPane.setBounds(10,30,500,300);
		
		final JLabel label1 = new JLabel("Se va a firmar la siguiente informaci\u00F3n:");
		label1.setBounds(10,10,500,15);
		label1.setLabelFor(scrollPane);
		
		// ***************************************************
		// ****** Componentes opcionales *********************
		// ***************************************************
		
		int offset = 0;
		
		// Ordenamos el array de alias justo antes de mostrarlo, ignorando entre
		// mayusculas y minusculas
		Object[] finalOrderedAliases = aliasesByFriendlyName.values().toArray();
		Arrays.sort(finalOrderedAliases, new Comparator<Object>() {
			public int compare(Object o1, Object o2) {
				if (o1==null && o2==null) return 0;
				else if (o1==null && o2!=null) return 1;
				else if (o1!=null && o2==null) return -1;
				return o1.toString().compareToIgnoreCase(o2.toString());
			}
		});
		
		final JComboBox comboBox = new JComboBox(finalOrderedAliases);
		comboBox.setBounds(10, 370, 500, 20);
		
		final JLabel label2 = new JLabel("Por favor, seleccione el certificado de firma:");
		label2.setBounds(10, 350, 500, 15);
		label2.setLabelFor(comboBox);
		
		// ***************************************************
		// ****** Fin componentes opcionales *****************
		// ***************************************************

		final JPanel pane = new JPanel();
		pane.setLayout(null);
		pane.add(scrollPane);
		pane.add(label1);
		if (aliasesByFriendlyName.size() > 1) {
			pane.add(comboBox);
		}
		else {
			offset = -25;
			label2.setText("Se utilizar\u00E1 el siguiente certificado: " + aliasesByFriendlyName.keys().nextElement());
		}
		pane.add(label2);
		
		
		final JLabel label3 = new JLabel("Si est\u00E1 conforme en firmar este mensaje pulse el bot\u00F3n Aceptar");
		label3.setBounds(10, 405+offset, 500, 15);
		
		pane.add(label3);
		
		// Dimensionamos el panel con los componentes
		pane.setPreferredSize(new Dimension(520, 440+offset));

		// Mostramos el dialogo 
		if(JOptionPane.showConfirmDialog(parent, pane, "Firma Electr\u00F3nica", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE)
				== JOptionPane.OK_OPTION) {
			try {
				
				//********************************************************************
				//**** DAMOS VUELTA ALIAS Y NOMBRES DESCRIPTIVOS Y FILTRAMOS CADUCADOS
				//********************************************************************
				final String certName = comboBox.getSelectedItem().toString();
				
				for(String al : aliasesByFriendlyName.keySet().toArray(new String[aliasesByFriendlyName.size()])) {
					if(aliasesByFriendlyName.get(al).equals(certName)) {
						if (kss != null) {
							AOCertVerifier cv = new AOCertVerifier();
							for (KeyStore ks : kss.getKeyStores()) {
								try {
									if (ks.containsAlias(al)) {
										try {
											cv.checkCertificate(new java.security.cert.Certificate[] { ks.getCertificate(al) }, false);
										}
										catch(Throwable t) {
											Logger.getLogger("es.gob.afirma").warning(t.getMessage());
											if (JOptionPane.showConfirmDialog(
												parent, 
												cv.getErrorMessage() + 
												Messages.getString("AOUIManager.8"),  //$NON-NLS-1$
												Messages.getString("AOUIManager.5"),  //$NON-NLS-1$
												JOptionPane.YES_NO_OPTION, 
												JOptionPane.WARNING_MESSAGE
											) == JOptionPane.YES_OPTION) {
												result = firmar(
													stringToSign, 
													kss.getKeyEntry(al, passCbk), 
													kss.getCertificate(al)
												);
											}
											return;
										}
										
										result = firmar(
												stringToSign, 
												kss.getKeyEntry(al, passCbk), 
												kss.getCertificate(al)
											);
									}
								}
								catch(Throwable e) {}
							}
						}
					}
				}

			}
			catch(Throwable t) {
				Logger.getLogger("es.gob.afirma").severe(
					"Error durante el proceso de firma de texto: " + t
				);
				JOptionPane.showMessageDialog(parent, "Error durante el proceso de firma", "Error", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
