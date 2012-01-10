/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.core.jse;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.URL;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import com.sun.jndi.toolkit.dir.SearchFilter;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.NameCertificateBean;

/** Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica Windows 7. */
@SuppressWarnings("restriction")
final class CertificateSelectionPanel extends JPanel implements ListSelectionListener {
	
	/** Serial version */ 
	private static final long serialVersionUID = 6288294705582545804L;
	
	private static final String VERDANA_FONT_NAME = "Verdana"; //$NON-NLS-1$

	private static final Font TITLE_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, 14); 
	
	private static final Font TEXT_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, 12); 
	
	/** Altura de un elemento de la lista de certificados. */
	private static final int CERT_LIST_ELEMENT_HEIGHT = 86;
	
	private JList<CertificateLine> certList;
	
	private String selectedValue = null;
	
	CertificateSelectionPanel(final NameCertificateBean[] el) {
		this.createUI((el == null) ? new NameCertificateBean[0] : el);
	}
	
	private void createUI(final NameCertificateBean[] el) {

		this.setLayout(new GridBagLayout());
		
		this.setBackground(Color.WHITE);
		this.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
		
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(13, 15, 8, 15);
		c.weightx = 1.0;
		c.weighty = 0.0;
		c.gridy = 0;
		
		final JLabel mainMessage = new JLabel(JSEUIMessages.getString("CertificateSelectionPanel.0")); //$NON-NLS-1$
		mainMessage.setFont(TITLE_FONT);
		mainMessage.setForeground(Color.decode("0x0033BC")); //$NON-NLS-1$
		this.add(mainMessage, c);
		
		if (el.length == 1) {
			c.insets = new Insets(0, 15, 4, 15);
			c.gridy++;

			final JTextPane textMessage = new JTextPane();
			textMessage.setText(JSEUIMessages.getString("CertificateSelectionPanel.1")); //$NON-NLS-1$
			textMessage.setFont(TEXT_FONT);
			textMessage.setBorder(null);
			textMessage.setPreferredSize(new Dimension(370, 40));
			this.add(textMessage, c);
		}
		
		c.insets = new Insets(4, 15, 8, 15);
		c.gridy++;
		
		this.add(new JSeparator(), c);
		
		c.insets = new Insets(8, 18, 13, 18);
		c.weighty = 1.0;
		c.gridy++;
		
		CertificateLine certLine;
		final Vector<CertificateLine> certLines = new java.util.Vector<CertificateSelectionPanel.CertificateLine>();
		for (final NameCertificateBean nameCert : el) {
		    try {
		    	certLine = createCertLine(nameCert.getName(), nameCert.getCertificate() );
		    } 
		    catch(final Exception e) {
		        continue;
		    }
			certLine.setPreferredSize(new Dimension(0, CERT_LIST_ELEMENT_HEIGHT));
			certLines.add(certLine);
		}

		this.certList = new JList<CertificateLine>();
		this.certList.setCellRenderer(new CertListCellRendered());
		this.certList.setListData(certLines);
		this.certList.setVisibleRowCount(Math.max(Math.min(4, certLines.size()), 1));
		if (certLines.size() > 0) {
			this.certList.setSelectedIndex(0);
		}
		
		final JScrollPane sPane = new JScrollPane(
				this.certList,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER
		);
		
		this.certList.addListSelectionListener(this);
		final CertLinkMouseListener listener = new CertLinkMouseListener();
		this.certList.addMouseMotionListener(listener);
		this.certList.addMouseListener(listener);
		
		this.certList.setSelectedIndex(0);
		this.selectedValue = this.certList.getSelectedValue().toString();
		
		sPane.setBorder(null);
		sPane.setPreferredSize(new Dimension(435, CERT_LIST_ELEMENT_HEIGHT * this.certList.getVisibleRowCount()));
		
		this.add(sPane, c);
	}
	
	/** Selecciona la lista de certificados. */
    void selectCertificateList() {
		this.certList.requestFocusInWindow();
	}
	
	/** Recupera el nombre descriptor del certificado seleccionado.
	 * @return Nombre del certificado seleccionado. */
	String getSelectedCertificate() {
		return this.selectedValue;
	}
	
	/** Agrega un gestor de eventos de rat&oacute;n a la lista de certificados para poder
	 * gestionar a trav&eacute;s de &eacute;l eventos especiales sobre la lista.
	 * @param listener Manejador de eventos de rat&oacute;n. */
	void addCertificateListMouseListener(MouseListener listener) {
		this.certList.addMouseListener(listener);
	}
	
	private static CertificateLine createCertLine(final String friendlyName, final X509Certificate cert) {
		final CertificateLine certLine = new CertificateLine(friendlyName, cert);
		certLine.setFocusable(true);
		return certLine;
	}

	private static class CertificateLine extends JPanel {
		
		/** Serial Version */
		private static final long serialVersionUID = 5012625058876812352L;

		private static final Font SUBJECT_FONT = new Font(VERDANA_FONT_NAME, Font.BOLD, 14); 
		
		private static final Font DETAILS_FONT = new Font(VERDANA_FONT_NAME, Font.PLAIN, 11); 
		
		private JLabel propertiesLink = null;
		
		private String friendlyName;
		private X509Certificate cert;
		
		CertificateLine(final String friendlyName, final X509Certificate cert) {
			this.friendlyName = friendlyName;
			this.cert = cert;
			createUI();
		}

		/**
		 * Indica si un certificado pertenece a un DNIe.
		 * @param certificate Certificado.
		 * @return Devuelve {@code true} si el certificado pertenece a un DNIe, {@code false}
		 * en caso contrario.
		 */
		private static boolean isDNIeCert(final X509Certificate certificate) {
			
			try {
				final Attributes attrs = new BasicAttributes(true);
				for (final Rdn rdn : new LdapName(certificate.getIssuerDN().toString()).getRdns()) {
					attrs.put(rdn.getType(), rdn.getValue());
				}
				return new SearchFilter(
						"(&(cn=AC DNIE *)(ou=DNIE)(o=DIRECCION GENERAL DE LA POLICIA)(c=ES))") //$NON-NLS-1$
				.check(attrs);
			} 
			catch (final Exception e) {
				return false;
			}
		}
		
		X509Certificate getCertificate() {
			return this.cert;
		}
		
		/** {@inheritDoc} */
		@Override
		public String toString() {
			return this.friendlyName;
		}
			
		private void createUI() {
			setLayout(new GridBagLayout());

			setBackground(Color.WHITE);
			
			final GridBagConstraints c = new GridBagConstraints();
			c.gridx = 1;
			c.gridy = 1;
			c.gridheight = 4;

			final URL urlIcon = (isDNIeCert(this.cert) ?
					AOUtil.getCleanClassLoader().getResource("resources/dnieicon.png") : //$NON-NLS-1$
						AOUtil.getCleanClassLoader().getResource("resources/certicon.png")); //$NON-NLS-1$
			final JLabel icon = (urlIcon != null) ?
					new JLabel(new ImageIcon(urlIcon)) : 
						new JLabel();

			c.insets = new Insets(2, 2, 2, 5);
			add(icon, c);

			c.fill = GridBagConstraints.HORIZONTAL;
			c.weightx = 1.0;
			c.gridx++;
			c.gridheight = 1;
			c.insets = new Insets(5, 0, 0, 5); 
				
			final JLabel alias = new JLabel(this.friendlyName);
			alias.setFont(SUBJECT_FONT);
			add(alias, c);
			
			c.gridy++;
			c.insets = new Insets(0, 0, 0, 5); 
						
			final JLabel issuer = new JLabel(JSEUIMessages.getString("CertificateSelectionPanel.2") + AOUtil.getCN(this.cert.getIssuerDN().toString())); //$NON-NLS-1$
			issuer.setFont(DETAILS_FONT);
			add(issuer, c);
			
			c.gridy++;
			
			final JLabel dates = new JLabel(
					JSEUIMessages.getString("CertificateSelectionPanel.3", //$NON-NLS-1$
					new String[] { formatDate(this.cert.getNotBefore()),
						formatDate(this.cert.getNotAfter()) }));
			dates.setFont(DETAILS_FONT);
			add(dates, c);
			
			c.gridy++;
			
			this.propertiesLink = new JLabel(
			        "<html><u>" + //$NON-NLS-1$
			        JSEUIMessages.getString("CertificateSelectionPanel.5") + //$NON-NLS-1$
			        "</u></html>"); //$NON-NLS-1$
			this.propertiesLink.setFont(DETAILS_FONT);
			add(this.propertiesLink, c);
		}
		
		/**
		 * Devuelve la fecha con formato.
		 * @param date Fecha.
		 * @return Texto que representativo de la fecha.
		 */
		private static String formatDate(Date date) {
			return new SimpleDateFormat("dd/MM/yyyy").format(date); //$NON-NLS-1$
		}
		
		/**
		 * Recupera el rect&aacute;ngulo ocupado por el enlace para la carga del certificado.
		 * @return Recuadro con el enlace.
		 */
		Rectangle getCertificateLinkBounds() {
			return this.propertiesLink.getBounds();
		}
	}
	
	/**
	 * Renderer para mostrar la informaci&oacute;n de un certificado.
	 */
	private static final class CertListCellRendered implements ListCellRenderer<CertificateLine> {

		CertListCellRendered() {
			/* Limitamos la visibilidad del constructor */
		}
		
		/** {@inheritDoc} */
		public Component getListCellRendererComponent(JList<? extends CertificateLine> list, CertificateLine value,
				int index, boolean isSelected, boolean cellHasFocus) {

			final CertificateLine line = value;
			if (isSelected) {
				line.setBackground(Color.decode("0xD9EAFF")); //$NON-NLS-1$
				line.setBorder(BorderFactory.createCompoundBorder(
						BorderFactory.createLineBorder(Color.WHITE, 1),
						BorderFactory.createLineBorder(Color.decode("0x84ACDD"), 1))); //$NON-NLS-1$

			} 
			else {
				line.setBackground(Color.WHITE);
				line.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 4));
			}
			
			return line;
		}

	}
	
	/** {@inheritDoc} */
	public void valueChanged(ListSelectionEvent e) {
		this.selectedValue = this.certList.getSelectedValue().toString();
	}
	
	/**
	 * Manejador de eventos de raton para la lista de certificados.
	 */
	private final class CertLinkMouseListener implements MouseListener, MouseMotionListener {

		private boolean entered = false;
		
		CertLinkMouseListener() {
			/* Contructor por defecto */
		}
		
		/** {@inheritDoc} */
		public void mouseClicked(final MouseEvent me) {
			if (me.getClickCount() == 1 &&
					((CertificateLine)((JList<?>) me.getSource()).getSelectedValue()).getCertificateLinkBounds().contains(me.getX(), me.getY() % CERT_LIST_ELEMENT_HEIGHT)) {
				try {
					CertificateUtils.openCert(
							CertificateSelectionPanel.this,
							((CertificateLine)((JList<?>) me.getSource()).getSelectedValue()).getCertificate());
				} 
				catch (final AOCancelledOperationException e) {
					/* No hacemos nada */
				}
			} 
		}
		
		/** {@inheritDoc} */
		public void mouseMoved(final MouseEvent me) {
			if (((CertificateLine)((JList<?>) me.getSource()).getSelectedValue()).getCertificateLinkBounds().contains(me.getX(), me.getY() % CERT_LIST_ELEMENT_HEIGHT)) {
				if (!this.entered) {
					((JList<?>) me.getSource()).setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
					this.entered = true;
				}
			} 
			else if (this.entered) {
				((JList<?>) me.getSource()).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				this.entered = false;
			}
		}
		
		/** {@inheritDoc} */
		public void mouseDragged(MouseEvent e) {
			/* No hacemos nada */
		}

		/** {@inheritDoc} */
		public void mouseReleased(MouseEvent me) {
			/* No hacemos nada */
		}
		
		/** {@inheritDoc} */
		public void mousePressed(MouseEvent me) {
			/* No hacemos nada */
		}
		
		/** {@inheritDoc} */
		public void mouseExited(MouseEvent me) {
			/* No hacemos nada */
		}
		
		/** {@inheritDoc} */
		public void mouseEntered(MouseEvent me) {
			/* No hacemos nada */
		}
	}
}
