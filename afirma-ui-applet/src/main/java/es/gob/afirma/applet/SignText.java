/* This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.awt.Dimension;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.security.auth.callback.PasswordCallback;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;


/** Clase para la firma electr&oacute;nica de cadenas de texto simulando el
 * funcionamiento del m&eacute;todo <code>[window.]crypto.signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])</code> de
 * JavaScript en navegadores Mozilla / Firefox. */
final class SignText {

    private static final String USER_CANCEL = "error:userCancel"; //$NON-NLS-1$
    private static final String INTERNAL_ERROR = "error:internalError"; //$NON-NLS-1$
    private static final String NO_MATCHING_CERT = "error:noMatchingCert"; //$NON-NLS-1$

    private final String[] alias;
    private final AOKeyStoreManager kss;
    private final Component parent;
    private String result = USER_CANCEL;
    private boolean useCAdES = false;

    /** Obtiene el resultado de la operaci&oacute;n <code>SignText</code>.
     * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta
     *         termin&oacute; correctamente se devuelve el objeto firmado en
     *         formato CMS (por defecto) o CAdES codificado en Base64. En caso
     *         contrario devuelve uno de los siguientes c&oacute;digos de error: <br>
     *         <ul>
     *         <li>
	 *         <code>error:noMatchingCert</code> si el usuario no dispone de ning&uacute;n certificado emitido por las CA indicadas.</li>
     *         <li>
	 *         <code>error:userCancel</code> si el usuario cancela la operaci&oacute;n.</li>
     *         <li>
	 *         <code>error:internalError</code> si ocurre cualquier error durante el proceso.</li>
     *         </ul> */
    String getResult() {
        return this.result;
    }

    /** Establece si debe usarse CAdES en vez de CMS para la firma.
     * @param useit
     *        <code>true</code> para usar CAdES, <code>false</code> (por
     *        defecto) para usar CMS */
    void setUseCAdES(final boolean useit) {
        this.useCAdES = useit;
    }

    /** Firma una cadena de texto simulando el funcionamiento del m&eacute;todo
     * <code>[window.]crypto.signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])</code> de JavaScript en navegadores Mozilla /
     * Firefox.
     * @param stringToSign
     *        El texto a firmar. Si se especifica <code>"ask"</code> en el
     *        par&aacute;metro <code>caOption</code> se presenta el texto al
     *        usuario en un di&aacute;logo de una forma legible
     * @param caOption
     *        Puede ser una de las siguientes opciones: <br>
     *        <ul>
     *        <li>
	 *            <code>"auto"</code> indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. Si se ha indicado uno o
     *        m&aacute;s nombre de CA mediante los par&aacute;metros <code>caName<i>N</i></code> la selecci&oacute;n autom&aacute;tica se limita a los
     *        certificado emitidos por las CA indicadas.</li>
     *        <li>
	 *            <code>"ask"</code> indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado uno o m&aacute;s nombre
     *        de CA mediante los par&aacute;metros <code>caName<i>N</i></code> la selecci&oacute;n autom&aacute;tica se limita a los certificado
     *        emitidos por las CA indicadas.</li>
     *        </ul>
     * @param caNameN
     *        DN de las CA cuyos certificados deben tenerse en cuenta para
     *        la selecci&oacute;n de firmante, debe proporcionarse un
     *        par&aacute;metro por cada CA. Para mayor informaci&oacute;n
     *        sobre el formato DN consulte <a
     *        href="http://www.faqs.org/rfcs/rfc1485.html">String
     *        Representation of Distinguished Names</a>. */
    void signText(final String stringToSign, final String caOption, final String... caNameN) {

        if (stringToSign == null || "".equals(stringToSign)) { //$NON-NLS-1$
            Logger.getLogger("es.gob.afirma").warning("No se ha proporcionado un texto para firmar"); //$NON-NLS-1$ //$NON-NLS-2$
            this.result = INTERNAL_ERROR;
            return;
        }

        if (!"ask".equals(caOption) && !"auto".equals(caOption)) { //$NON-NLS-1$ //$NON-NLS-2$
            Logger.getLogger("es.gob.afirma").severe("Valor incorrecto para caOption, debe ser 'ask' u 'auto': " + caOption); //$NON-NLS-1$ //$NON-NLS-2$
            this.result = INTERNAL_ERROR;
            return;
        }

        if (this.alias == null || this.alias.length < 1) {
            this.result = NO_MATCHING_CERT;
            return;
        }

        // Gestionamos el filtro de CAs

        String issuerFilter = null;

        if (caNameN != null && caNameN.length > 0) {
            final StringBuilder filter = new StringBuilder();
            LdapName dn;
            String ca;
            for (int j = 0; j < caNameN.length; j++) {
                ca = caNameN[j];
                try {
                    dn = new LdapName(ca);
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").warning("DN de CA para filtro mal formado ('" + ca + "'), se ignorara: " + e);  //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
                    continue;
                }
                if (dn.getRdns().size() > 0) {
                    filter.append("(");  //$NON-NLS-1$
                    for (int i = 0; i < dn.getRdns().size(); i++) {
                        filter.append(dn.getRdns().get(i).toString());
                        if (i != dn.getRdns().size() - 1) {
                            filter.append(" & ");  //$NON-NLS-1$
                        }
                    }
                    filter.append(")");  //$NON-NLS-1$
                }

                if (j != caNameN.length - 1) {
                    filter.append(" | ");  //$NON-NLS-1$
                }
            }
            issuerFilter = filter.toString();
        }
        Logger.getLogger("es.gob.afirma").info("Se utilizara el siguiente filtro de emisor: " + issuerFilter);  //$NON-NLS-1$ //$NON-NLS-2$

        // Obtenemos la lista de nombres descriptivos de los alias de los
        // certificados
        final Map<String, String> aliasesByFriendlyName;
        try {
            aliasesByFriendlyName =
                    KeyStoreUtilities.getAliasesByFriendlyName(this.alias, // aliases
                                                                this.kss, // KeyStores
                                                                true, // checkPrivateKeys
                                                                true, // showExpiredCertificates
                                                                Collections.singletonList((CertificateFilter) new RFC2254CertificateFilter(null,
                                                                                                                                           issuerFilter)) // filtros
                    );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error obteniendo los nombres descriptivos de los alias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            this.result = INTERNAL_ERROR;
            return;
        }

        // Miramos si despues de filtrar las entradas queda alguna o se ha
        // quedado la lista vacia
        if (aliasesByFriendlyName.size() == 0) {
            this.result = NO_MATCHING_CERT;
            return;
        }

        // En "auto" seleccionamos automaticamente el primero de la lista
        if ("auto".equals(caOption)) { //$NON-NLS-1$
            final Map<String, String> tmpHash = new HashMap<String, String>(1);
            final String key = aliasesByFriendlyName.keySet().iterator().next();
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
            Logger.getLogger("es.gob.afirma").severe("El texto a firmar no puede ser nulo"); //$NON-NLS-1$ //$NON-NLS-2$
            return INTERNAL_ERROR;
        }
        if (keyEntry == null) {
            Logger.getLogger("es.gob.afirma").severe("La clave privada de firma no puede ser nula"); //$NON-NLS-1$ //$NON-NLS-2$
            return NO_MATCHING_CERT;
        }
        if (cert == null) {
            Logger.getLogger("es.gob.afirma").severe("El certificado de firma no puede ser nulo"); //$NON-NLS-1$ //$NON-NLS-2$
            return NO_MATCHING_CERT;
        }
        try {


                final AOSigner signer = AOSignerFactory.getSigner(
                        this.useCAdES ? AOSignConstants.SIGN_FORMAT_CADES : AOSignConstants.SIGN_FORMAT_CMS);

                return Base64.encode(
                    signer.sign(
                        stringToSign.getBytes(),
                        AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
                        keyEntry.getPrivateKey(),
                        keyEntry.getCertificateChain(),
                        null
                    )
                );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Error creando la firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            return INTERNAL_ERROR;
        }
    }

    private final PasswordCallback passCbk;

    /** Crea un objeto para la firma de Texto simulando el funcionamiento de <code>crypto.signText()</code>
     * @param aliases
     *        Alias para mostrar en el men&uacute; desplegable de
     *        selecci&oacute;n de certificado
     * @param ksm
     *        Gestor de KeyStores, para convertir los alias en nombres
     *        decriptivos
     * @param parentComponent
     *        Componente padre para la modalidad
     * @param pc
     *        PassWord callback si el KeyStore necesita contrase&ntilde;a.
     *        Si de especifica <code>null</code> se utilizara un <code>NullPasswordCallback</code> */
    SignText(final String[] aliases, final AOKeyStoreManager ksm, final Component parentComponent, final PasswordCallback pc) {
        this.alias = aliases == null ? null : aliases.clone();
        this.kss = ksm;
        this.parent = parentComponent;
        if (pc == null) {
            this.passCbk = NullPasswordCallback.getInstance();
        }
        else {
            this.passCbk = pc;
        }
    }

    private void createUI(final String stringToSign, final Map<String, String> aliasesByFriendlyName) {

        final JTextArea textArea = new JTextArea(stringToSign);
        textArea.setEditable(false);

        final JScrollPane scrollPane = new JScrollPane(textArea);
        scrollPane.setMinimumSize(new Dimension(400, 300));
        scrollPane.setBounds(10, 30, 500, 300);

        final JLabel label1 = new JLabel(AppletMessages.getString("SignText.0")); //$NON-NLS-1$
        label1.setBounds(10, 10, 500, 15);
        label1.setLabelFor(scrollPane);

        // ***************************************************
        // ****** Componentes opcionales *********************
        // ***************************************************

        int offset = 0;

        // Ordenamos el array de alias justo antes de mostrarlo, ignorando entre
        // mayusculas y minusculas
        final Object[] finalOrderedAliases = aliasesByFriendlyName.values().toArray();
        Arrays.sort(finalOrderedAliases, new Comparator<Object>() {
            @Override
			public int compare(final Object o1, final Object o2) {
                if (o1 == null && o2 == null) {
                    return 0;
                }
                else if (o1 == null && o2 != null) {
                    return 1;
                }
                else if (o1 != null && o2 == null) {
                    return -1;
                }
                else if (o1 == null) { // Este caso no se va a dar jamas
                	return 0;
                }
                return o1.toString().compareToIgnoreCase(o2.toString());
            }
        });

        final JComboBox comboBox = new JComboBox(finalOrderedAliases);
        comboBox.setBounds(10, 370, 500, 20);

        final JLabel label2 = new JLabel(AppletMessages.getString("SignText.1")); //$NON-NLS-1$
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
            label2.setText(AppletMessages.getString("SignText.2") + aliasesByFriendlyName.keySet().iterator().next()); //$NON-NLS-1$
        }
        pane.add(label2);

        final JLabel label3 = new JLabel(AppletMessages.getString("SignText.3")); //$NON-NLS-1$
        label3.setBounds(10, 405 + offset, 500, 15);

        pane.add(label3);

        // Dimensionamos el panel con los componentes
        pane.setPreferredSize(new Dimension(520, 440 + offset));

        // Mostramos el dialogo
        if (AOUIFactory.showConfirmDialog(
	    		this.parent,
	    		pane,
	    		AppletMessages.getString("SignApplet.25"),  //$NON-NLS-1$
	    		AOUIFactory.OK_CANCEL_OPTION,
	    		AOUIFactory.PLAIN_MESSAGE
		) == AOUIFactory.OK_OPTION) {
            try {

                // ********************************************************************
                // **** DAMOS VUELTA ALIAS Y NOMBRES DESCRIPTIVOS Y FILTRAMOS CADUCADOS
                // ********************************************************************
                final String certName = comboBox.getSelectedItem().toString();

                for (final String al : aliasesByFriendlyName.keySet().toArray(new String[aliasesByFriendlyName.size()])) {
                    if (aliasesByFriendlyName.get(al).equals(certName) && this.kss != null) {
                        final X509Certificate tmpCert = this.kss.getCertificate(al);
                        if (tmpCert != null) {
                            String errorMessage = null;
                            try {
                            	tmpCert.checkValidity();
                            }
                            catch (final CertificateExpiredException e) {
                                errorMessage = AppletMessages.getString("SignText.5"); //$NON-NLS-1$
                            }
                            catch (final CertificateNotYetValidException e) {
                                errorMessage = AppletMessages.getString("SignText.6"); //$NON-NLS-1$
                            }
                            catch (final Exception e) {
                                errorMessage = AppletMessages.getString("SignText.9"); //$NON-NLS-1$
                            }

                            if (errorMessage != null) {
                                Logger.getLogger("es.gob.afirma").warning(errorMessage); //$NON-NLS-1$
                                if (AOUIFactory.showConfirmDialog(
                                		this.parent,
                                		errorMessage,
                                        AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
                                        AOUIFactory.YES_NO_OPTION,
                                        AOUIFactory.WARNING_MESSAGE
                                ) != AOUIFactory.YES_OPTION) {
                                    return;
                                }
                            }

                            this.kss.setEntryPasswordCallBack(this.passCbk);
                            this.result = firmar(
                                stringToSign,
                                this.kss.getKeyEntry(al),
                                this.kss.getCertificate(al)
                            );
                        }

                    }
                }

            }
            catch (final Exception t) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de firma de texto: " + t); //$NON-NLS-1$ //$NON-NLS-2$
                AOUIFactory.showMessageDialog(
            		this.parent,
            		AppletMessages.getString("SignApplet.101"), //$NON-NLS-1$
            		AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
            		JOptionPane.ERROR_MESSAGE
        		);
            }
        }
    }
}
