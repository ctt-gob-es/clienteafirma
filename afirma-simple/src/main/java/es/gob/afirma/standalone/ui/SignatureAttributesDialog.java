package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Properties;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class SignatureAttributesDialog {

	private static final String PROP_POLICY_ID = "policyIdentifier"; //$NON-NLS-1$
	private static final String PROP_POLICY_HASH = "policyIdentifierHash"; //$NON-NLS-1$
	private static final String PROP_POLICY_HASH_ALGO = "policyIdentifierHashAlgorithm"; //$NON-NLS-1$
	private static final String PROP_POLICY_DESCRIPTION = "policyDescription"; //$NON-NLS-1$
	private static final String PROP_POLICY_QUALIFIER = "policyQualifier"; //$NON-NLS-1$
	private static final String PROP_PLACE_STREET = "signatureProductionStreetAddress"; //$NON-NLS-1$
	private static final String PROP_PLACE_CITY = "signatureProductionCity"; //$NON-NLS-1$
	private static final String PROP_SIGN_REASON = "signReason"; //$NON-NLS-1$
	private static final String PROP_CONTACT_INFO= "signerContact"; //$NON-NLS-1$
	private static final String PROP_PLACE_PROVINCE = "signatureProductionProvince"; //$NON-NLS-1$
	private static final String PROP_PLACE_PC = "signatureProductionPostalCode"; //$NON-NLS-1$
	private static final String PROP_PLACE_COUNTRY = "signatureProductionCountry"; //$NON-NLS-1$
	private static final String PROP_ROLES = "signerClaimedRoles"; //$NON-NLS-1$

	private static final String PROP_PROFILE = "profile"; //$NON-NLS-1$
	private static final String PROFILE_B_LEVEL = "B-B-Level"; //$NON-NLS-1$

	/** Valores que hay que comprobar para saber si en la configuraci&oacute;n de firma
	 * se encuentran atributos de firma adicionales a los datos, el certificado y la
	 * hora de firma. */
	private static final String[] SIGNED_ATTR_FLAGS = new String[] {
			PROP_POLICY_ID,
			PROP_PLACE_STREET,
			PROP_PLACE_CITY,
			PROP_PLACE_PROVINCE,
			PROP_PLACE_PC,
			PROP_PLACE_COUNTRY,
			PROP_ROLES,
			PROP_CONTACT_INFO,
			PROP_SIGN_REASON
	};

	private static String accessibleDescription = ""; //$NON-NLS-1$

	public static JDialog newInstance(final Component parent, final SignOperationConfig config) {

		accessibleDescription = ""; //$NON-NLS-1$

		final JOptionPane optionPane = new JOptionPane();
		optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		optionPane.setMessage(createInfoPanel(config));

		final JDialog dialog = optionPane.createDialog(
				getParentComponent(parent),
				SimpleAfirmaMessages.getString("SignatureAttributesDialog.0")); //$NON-NLS-1$
		dialog.setIconImages(DesktopUtil.getIconImages());

		return dialog;
	}

	private static Component createInfoPanel(final SignOperationConfig config) {

		final JPanel panel = new JPanel(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;

		c.anchor = GridBagConstraints.LINE_START;
		c.gridy = 0;
		c.gridx = 0;

		c.gridwidth = 2;

		// Si tiene  atributos de firma adicionales definidos, mostramos el detalle.
		if (hasAdditionalSignedAttributes(config)) {
			panel.add(new JLabel(SimpleAfirmaMessages.getString("SignatureAttributesDialog.1")), c); //$NON-NLS-1$
			accessibleDescription += SimpleAfirmaMessages.getString("SignatureAttributesDialog.1"); //$NON-NLS-1$

			final Properties p = config.getExtraParams();
			if (p.containsKey(PROP_POLICY_ID)) {

				addHeader(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.2")); //$NON-NLS-1$
				addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.3"), p.getProperty(PROP_POLICY_ID)); //$NON-NLS-1$

				if (p.containsKey(PROP_POLICY_HASH)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.4"), p.getProperty(PROP_POLICY_HASH)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_POLICY_HASH_ALGO)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.5"), p.getProperty(PROP_POLICY_HASH_ALGO)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_POLICY_DESCRIPTION)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.6"), p.getProperty(PROP_POLICY_DESCRIPTION)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_POLICY_QUALIFIER)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.7"), p.getProperty(PROP_POLICY_QUALIFIER)); //$NON-NLS-1$
				}
			}

			if (p.containsKey(PROP_PLACE_CITY) || p.containsKey(PROP_PLACE_PROVINCE) ||
					p.containsKey(PROP_PLACE_PC) || p.containsKey(PROP_PLACE_COUNTRY) ||
					PROFILE_B_LEVEL.equalsIgnoreCase(p.getProperty(PROP_PROFILE)) && p.containsKey(PROP_PLACE_STREET)) {
				addHeader(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.8")); //$NON-NLS-1$

				if (PROFILE_B_LEVEL.equalsIgnoreCase(p.getProperty(PROP_PROFILE)) && p.containsKey(PROP_PLACE_STREET)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.9"), p.getProperty(PROP_PLACE_STREET)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_PLACE_CITY)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.10"), p.getProperty(PROP_PLACE_CITY)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_PLACE_PROVINCE)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.11"), p.getProperty(PROP_PLACE_PROVINCE)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_PLACE_PC)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.12"), p.getProperty(PROP_PLACE_PC)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_PLACE_COUNTRY)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.13"), p.getProperty(PROP_PLACE_COUNTRY)); //$NON-NLS-1$
				}
			}

			if (p.containsKey(PROP_SIGN_REASON) || p.containsKey(PROP_CONTACT_INFO)) {
				addHeader(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.19")); //$NON-NLS-1$

				if (p.containsKey(PROP_SIGN_REASON)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.17"), p.getProperty(PROP_SIGN_REASON)); //$NON-NLS-1$
				}
				if (p.containsKey(PROP_CONTACT_INFO)) {
					addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.18"), p.getProperty(PROP_CONTACT_INFO)); //$NON-NLS-1$
				}
			}

			if (p.containsKey(PROP_ROLES)) {
				addHeader(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.14")); //$NON-NLS-1$
				addData(panel, c, SimpleAfirmaMessages.getString("SignatureAttributesDialog.15"), p.getProperty(PROP_ROLES)); //$NON-NLS-1$
			}
		}
		// Si no hay atributos de firma adicionales, mostramos la informacion basica
		else {
			panel.add(new JLabel(SimpleAfirmaMessages.getString("SignatureAttributesDialog.16")), c); //$NON-NLS-1$
			accessibleDescription += SimpleAfirmaMessages.getString("SignatureAttributesDialog.16"); //$NON-NLS-1$
		}

		return panel;
	}

	/**
	 * Comprueba si en una configuraci&oacute;n de firma hay atributos de firma adicionales.
	 * @param config Configuraci&oacute;n de firma.
	 * @return {@code true} si hay atributos adicionales de firma, {@code false} en caso contrario.
	 */
	private static boolean hasAdditionalSignedAttributes(final SignOperationConfig config) {
		for (final String flag : SIGNED_ATTR_FLAGS) {
			if (config.getExtraParams().containsKey(flag)) {
				return true;
			}
		}
		return false;
	}

	public static void addHeader(final Container panel, final GridBagConstraints c, final String text) {

		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy++;
		c.insets = new Insets(8, 0, 2, 0);
		final JLabel label = new JLabel(text);
		label.setFont(label.getFont().deriveFont(Font.BOLD));
		panel.add(label, c);
		accessibleDescription += text;
	}

	public static void addData(final Container panel, final GridBagConstraints c, final String data, final String value) {

		c.gridwidth = 1;
		c.weightx = 0.0;
		c.gridy++;
		c.gridx = 0;
		c.insets = new Insets(2, 15, 0, 0);
		panel.add(new JLabel(data), c);
		c.weightx = 1.0;
		c.gridx = 1;
		panel.add(new JLabel(value), c);
		accessibleDescription += data + " " + value; //$NON-NLS-1$
	}


	/**
	 * Obtiene el componente padre de m&aacute;ximo nivel de un componente.
	 * @param component Componente del que se desea obtener el padre.
	 * @return Padre &uacute;ltimo del componente o &eacute;l mismo si no tiene padre.
	 */
	private static Component getParentComponent(final Component component) {

		Component parent = component;
		while (parent != null && parent.getParent() != null && parent != parent.getParent()) {
			parent = parent.getParent();
		}
		return parent;
	}

	public static String getAccessibleDescription() {
		return accessibleDescription;
	}

}
