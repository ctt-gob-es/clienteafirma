/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;

import es.gob.afirma.cert.signvalidation.SignValider;
import es.gob.afirma.cert.signvalidation.SignValiderFactory;
import es.gob.afirma.cert.signvalidation.SignValidity;
import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.cert.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;

/** Visor de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 * @author Carlos Gamuci. */
public final class VisorPanel extends JPanel implements KeyListener {

    /** Version ID. */
    private static final long serialVersionUID = 8309157734617505338L;

    private final VisorFirma visorFirma;

    VisorFirma getVisorFirma() {
        return this.visorFirma;
    }

    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros.
     * @param signFile Fichero de firma.
     * @param sign Firma.
     * @param vf VisorFirma para las acciones de los botones
     * @param allowReload <code>true</code> si se desea dar a usuario la opci&oacute;n de ver otras firmas en el
     *                    visor carg&aacute;ndolas mediante un bot&oacute;n, <code>false</code> en caso contrario. */
    public VisorPanel(final File signFile, final byte[] sign, final VisorFirma vf, final boolean allowReload) {
        super(true);
        this.visorFirma = vf;
        createUI(signFile, sign, allowReload);
    }

    private void createUI(final File signFile, final byte[] sign, final boolean addReloadButton) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        openSign(signFile, sign, addReloadButton);
    }

    private void openSign(final File signFile, final byte[] signature, final boolean addReloadButton) {

        if (signFile == null && signature == null) {
            Logger.getLogger("es.gob.afirma").warning("Se ha intentado abrir una firma nula");  //$NON-NLS-1$ //$NON-NLS-2$
            return;
        }

        byte[] sign = signature != null ?  signature.clone() : null;

        if (sign == null && signFile != null) {
            try ( final FileInputStream fis = new FileInputStream(signFile); ) {
                sign = AOUtil.getDataFromInputStream(fis);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            		"No se ha podido cargar el fichero de firma: " + e //$NON-NLS-1$
        		);
            }
        }

        SignValidity validity = new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
        if (sign != null) {
            try {
                validity = validateSign(sign);
            }
            catch (final Exception e) {
            	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        			"No se ha podido comprobar la validez de la firma: " + e //$NON-NLS-1$
    			);
                validity = new SignValidity(SIGN_DETAIL_TYPE.KO, null);
            }
        }

        final X509Certificate cert = getCertificate(sign);

        final JPanel resultPanel = new SignResultPanel(validity, this);
        final JPanel dataPanel = new SignDataPanel(
    		signFile,
    		sign,
    		null,
    		cert, // Certificado
    		this
		);

        final JPanel bottonPanel = new JPanel(true);
        bottonPanel.setLayout(new FlowLayout(FlowLayout.TRAILING));

        if (addReloadButton) {
            final JButton openSign = new JButton(SimpleAfirmaMessages.getString("VisorPanel.1")); //$NON-NLS-1$
            openSign.setMnemonic('V');
            bottonPanel.add(openSign);
            openSign.addKeyListener(this);
            openSign.addActionListener(
        		e -> {
				    if (VisorPanel.this.getVisorFirma() != null) {
				        VisorPanel.this.getVisorFirma().loadNewSign();
				    }
				}
    		);
        }

        final JButton closeVisor = new JButton(SimpleAfirmaMessages.getString("VisorPanel.0")); //$NON-NLS-1$
        closeVisor.setMnemonic('C');
        closeVisor.addKeyListener(this);
        closeVisor.addActionListener(
    		e -> {
			    if (VisorPanel.this.getVisorFirma() != null) {
			        VisorPanel.this.getVisorFirma().closeApplication(0);
			    }
			}
		);
        bottonPanel.add(closeVisor);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            bottonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.insets = new Insets(11, 11, 11, 11);
        add(resultPanel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 11, 11, 11);
        add(dataPanel, c);
        c.weighty = 0.0;
        c.gridy = 2;
        c.insets = new Insets(0, 11, 11, 11);
        add(bottonPanel, c);

        repaint();

    }

    private static X509Certificate getCertificate(final byte[] sign) {
		if (sign == null) {
			return null;
		}
		final AOTreeModel tree;
		try {
			if (new AOFacturaESigner().isSign(sign)) {
				tree = new AOFacturaESigner().getSignersStructure(sign, true);
			}
			else if (new AOXAdESSigner().isSign(sign)) {
				tree = new AOXAdESSigner().getSignersStructure(sign, true);
			}
			else if (new AOXMLDSigSigner().isSign(sign)) {
				tree = new AOXMLDSigSigner().getSignersStructure(sign, true);
			}
			else if (new AOPDFSigner().isSign(sign)) {
				tree = new AOPDFSigner().getSignersStructure(sign, true);
			}
			else if (new AOCAdESSigner().isSign(sign)) {
				tree = new AOCAdESSigner().getSignersStructure(sign, true);
			}
			else if (new AOCMSSigner().isSign(sign)) {
				tree = new AOCMSSigner().getSignersStructure(sign, true);
			}
			else if (new AOOOXMLSigner().isSign(sign)) {
				tree = new AOOOXMLSigner().getSignersStructure(sign, true);
			}
			else if (new AOODFSigner().isSign(sign)) {
				tree = new AOODFSigner().getSignersStructure(sign, true);
			}
			else {
				return null;
			}
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No se ha podido obtener el certificado de la firma: " + e //$NON-NLS-1$
			);
			return null;
		}
		// Solo se muestra la informacion del certificado si hay una unica firma
		if (AOTreeModel.getChildCount(tree.getRoot()) != 1) {
			return null;
		}
		final AOTreeNode node = (AOTreeNode) AOTreeModel.getChild(tree.getRoot(),0);
		final AOSimpleSignInfo ssi = (AOSimpleSignInfo) node.getUserObject();
		if (ssi == null) {
			return null;
		}
		final X509Certificate[] certs = ssi.getCerts();
		if (certs == null || certs.length < 1) {
			return null;
		}
		return certs[0];
	}

	/** Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
    public static SignValidity validateSign(final byte[] sign) throws IOException {
    	final SignValider sv = SignValiderFactory.getSignValider(sign);
        if (sv != null) {
        	return sv.validate(sign);
        }
        else if(DataAnalizerUtil.isSignedODF(sign)) {
			return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.ODF_UNKOWN_VALIDITY);
		}
		else if(DataAnalizerUtil.isSignedOOXML(sign)) {
			return new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.OOXML_UNKOWN_VALIDITY);
		}
		return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_SIGNATURE_FORMAT);
    }

	@Override
	public void keyPressed(final KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ESCAPE && VisorPanel.this.visorFirma != null) {
			VisorPanel.this.visorFirma.closeApplication(0);
		}
	}

	@Override
	public void keyReleased(final KeyEvent e) {
		// Vacio
	}

	@Override
	public void keyTyped(final KeyEvent e) {
		// Vacio
	}

}
