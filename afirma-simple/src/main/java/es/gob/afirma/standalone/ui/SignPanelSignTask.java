package es.gob.afirma.standalone.ui;

import java.awt.Cursor;
import java.io.File;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.BadPdfPasswordException;
import es.gob.afirma.signers.pades.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.pdf.PdfEmptySignatureFieldsChooserDialog;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

final class SignPanelSignTask extends SwingWorker<Void, Void> {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final SignPanel signPanel;
	private final List<? extends CertificateFilter> certFilters;

	private final CommonWaitDialog waitDialog;
	CommonWaitDialog getWaitDialog() {
		return this.waitDialog;
	}

	private PrivateKeyEntry getPrivateKeyEntry() throws AOCertificatesNotFoundException,
	                                                    KeyStoreException,
	                                                    NoSuchAlgorithmException,
	                                                    UnrecoverableEntryException {

		final AOKeyStoreManager ksm = this.signPanel.getSimpleAfirma().getAOKeyStoreManager();
    	final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
			ksm,
			this.signPanel,
			true,             // Comprobar claves privadas
			false,            // Mostrar certificados caducados
			true,             // Comprobar validez temporal del certificado
			this.certFilters, // Filtros
			false             // mandatoryCertificate
		);
    	dialog.show();
    	ksm.setParentComponent(this.signPanel);
    	return ksm.getKeyEntry(
			dialog.getSelectedAlias()
		);
	}


	SignPanelSignTask(final SignPanel parentSignPanel,
			          final List<? extends CertificateFilter> certificateFilters,
			          final CommonWaitDialog signWaitDialog) {
        super();
        this.signPanel = parentSignPanel;
        this.certFilters = certificateFilters;
        this.waitDialog = signWaitDialog;
    }

    void doSignature(final AOSigner currentSigner, final Properties initialExtraParams) {

        this.signPanel.setCursor(new Cursor(Cursor.WAIT_CURSOR));

        if (currentSigner == null || this.signPanel.getDataToSign() == null || this.signPanel.getSimpleAfirma() == null) {
            return;
        }

        final PrivateKeyEntry pke;
        try {
            pke = getPrivateKeyEntry();
        }
        catch (final AOCancelledOperationException e) {
            return;
        }
        catch(final AOCertificatesNotFoundException e) {
        	LOGGER.severe("El almacen no contiene ningun certificado que se pueda usar para firmar: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.29"), //$NON-NLS-1$,
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return;
        }
        catch (final Exception e) {
        	LOGGER.severe("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.56"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        	return;
    	}
        finally {
            this.signPanel.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }

        // ************************************************************************************
        // ****************** PROCESO DE FIRMA ************************************************
        final Properties prefProps = ExtraParamsHelper.loadExtraParamsForSigner(currentSigner);
        if (initialExtraParams != null){
        	prefProps.putAll(initialExtraParams);
        }

        // Anadimos las propiedades del sistema, habilitando asi que se puedan indicar opciones de uso con -D en linea
        // de comandos
        final Properties p = new Properties();
        p.putAll(prefProps);
        p.putAll(System.getProperties());

        final String signatureAlgorithm = PreferencesManager.get(
    		PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, "SHA512withRSA" //$NON-NLS-1$
		);

        final byte[] signResult;
        try {
            if (this.signPanel.isCosign()) {
                signResult = currentSigner.cosign(
            		this.signPanel.getDataToSign(),
            		signatureAlgorithm,
                    pke.getPrivateKey(),
                    pke.getCertificateChain(),
                    p
                );
            }
            else {
                signResult = currentSigner.sign(
            		this.signPanel.getDataToSign(),
            		signatureAlgorithm,
            		pke.getPrivateKey(),
                    pke.getCertificateChain(),
                    p
                );
            }
        }
        catch(final AOCancelledOperationException e) {
            return;
        }
        catch(final AOFormatFileException e) {
        	LOGGER.warning("La firma o el documento no son aptos para firmar: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.102"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final PdfIsCertifiedException e) {
        	LOGGER.warning("PDF no firmado por estar certificado: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.27"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final BadPdfPasswordException e) {
        	LOGGER.warning("PDF protegido con contrasena mal proporcionada: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.23"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final PdfHasUnregisteredSignaturesException e) {
        	LOGGER.warning("PDF con firmas no registradas: " + e); //$NON-NLS-1$
        	AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.28"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final Exception e) {
            LOGGER.log(Level.SEVERE, "Error durante el proceso de firma: " + e, e); //$NON-NLS-1$
            AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.65"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final OutOfMemoryError ooe) {
            LOGGER.severe("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$
            AOUIFactory.showErrorMessage(
                this.signPanel,
                SimpleAfirmaMessages.getString("SignPanel.1"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        finally {
            this.signPanel.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
        // ****************** FIN PROCESO DE FIRMA ********************************************
        // ************************************************************************************

        final String newFileName = currentSigner.getSignedName(
    		this.signPanel.getCurrentFile().getName(),
    		"_signed" //$NON-NLS-1$
		);
        String[] filterExtensions;
        String filterDescription;


        if (currentSigner instanceof AOPDFSigner) {
            filterExtensions = new String[] {
                "pdf" //$NON-NLS-1$
            };
            filterDescription = SimpleAfirmaMessages.getString("SignPanel.72"); //$NON-NLS-1$
        }
        else if (currentSigner instanceof AOXAdESSigner || currentSigner instanceof AOFacturaESigner) {
            filterExtensions = new String[] {
                "xsig", "xml" //$NON-NLS-1$ //$NON-NLS-2$
            };
            filterDescription = SimpleAfirmaMessages.getString("SignPanel.76"); //$NON-NLS-1$
        }
        else if (currentSigner instanceof AOOOXMLSigner) {
        	if (newFileName.toLowerCase().endsWith("docx")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "docx" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.91"); //$NON-NLS-1$
        	}
        	else if (newFileName.toLowerCase().endsWith("pptx")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "pptx" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.92"); //$NON-NLS-1$
        	}
        	else if (newFileName.toLowerCase().endsWith("xslx")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "xslx" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.93"); //$NON-NLS-1$
        	}
        	else {
        		filterExtensions = new String[] {
                    "ooxml" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.94"); //$NON-NLS-1$
        	}
        }
        else if (currentSigner instanceof AOODFSigner) {
        	if (newFileName.toLowerCase().endsWith("odt")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "odt" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.96"); //$NON-NLS-1$
        	}
        	else if (newFileName.toLowerCase().endsWith("odp")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "odp" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.97"); //$NON-NLS-1$
        	}
        	else if (newFileName.toLowerCase().endsWith("ods")) { //$NON-NLS-1$
                filterExtensions = new String[] {
                    "ods" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.98"); //$NON-NLS-1$
        	}
        	else {
        		filterExtensions = new String[] {
                    "odf" //$NON-NLS-1$
                };
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.99"); //$NON-NLS-1$
        	}
        }
        else {
            filterExtensions = new String[] {
                "csig", "p7s" //$NON-NLS-1$ //$NON-NLS-2$
            };
            filterDescription = SimpleAfirmaMessages.getString("SignPanel.80"); //$NON-NLS-1$
        }

        final String fDescription = filterDescription;
        final String[] fExtensions = filterExtensions;

        final File fd;
        try {
        	final File currentDataFile = this.signPanel.getCurrentFile();
	    	fd = AOUIFactory.getSaveDataToFile(
    			signResult,
    			SimpleAfirmaMessages.getString("SignPanel.81"), //$NON-NLS-1$
    			currentDataFile != null ? currentDataFile.getParent() : null,
    			newFileName,
    			fExtensions,
    			fDescription,
				this.signPanel.getWindow()
			);
        }
        catch(final IOException e) {
            LOGGER.severe(
                "No se ha podido guardar el resultado de la firma: " + e //$NON-NLS-1$
            );
            AOUIFactory.showErrorMessage(
                  this.signPanel,
                  SimpleAfirmaMessages.getString("SignPanel.88"), //$NON-NLS-1$
                  SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                  JOptionPane.ERROR_MESSAGE
            );
            return;
        }
        catch(final AOCancelledOperationException e) {
        	return;
        }
        finally {
        	this.signPanel.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }

        this.signPanel.getSimpleAfirma().loadResultsPanel(
    		signResult,
    		fd.getAbsolutePath(),
    		(X509Certificate) pke.getCertificate()
		);
    }

    @Override
    public Void doInBackground() {

        final AOSigner currentSigner = this.signPanel.getSigner();

        // Si es un PDF miramos si nos han pedido firma visible
    	final boolean doVisibleSignature;
    	if (this.signPanel.getFilePanel() instanceof SignPanelFilePanel) {
    		doVisibleSignature = ((SignPanelFilePanel)this.signPanel.getFilePanel()).isVisibleSignature();
    	}
    	else {
    		doVisibleSignature = false;
    	}

        if (currentSigner instanceof AOPDFSigner) {
            if (doVisibleSignature) {
            	final List<SignatureField> emptySignatureFields = PdfUtil.getPdfEmptySignatureFields(this.signPanel.getDataToSign());
            	final String oldMessage = this.waitDialog.getMessage();
            	this.waitDialog.setMessage(SimpleAfirmaMessages.getString("SignPanelSignTask.0")); //$NON-NLS-1$
            	if (!emptySignatureFields.isEmpty()) {
            		final SignatureField field = PdfEmptySignatureFieldsChooserDialog.selectField(emptySignatureFields);
            		if (field != null) {
            			if (field.getName().equals("Create")) { //$NON-NLS-1$
            				try {
            					SignPdfDialog.getVisibleSignatureExtraParams(
            						currentSigner.isSign(this.signPanel.getDataToSign()),
            						this.signPanel.getDataToSign(),
            						this.signPanel.getWindow(),
            						new SignPdfDialogListener() {
            							@Override
            							public void propertiesCreated(final Properties extraParams) {
            								// Solo hacemos la firma si hay propiedades visibles
            								if (extraParams != null && !extraParams.isEmpty()) {
            									SignPanelSignTask.this.getWaitDialog().setMessage(oldMessage);
            									doSignature(currentSigner, extraParams);
            								}
            					        	if (getWaitDialog() != null) {
            					        		getWaitDialog().dispose();
            					        	}
            							}
            						}
            					);
            				}
            				catch (final Exception e) {
            					LOGGER.severe(
            		                "No se han podido cargar los parametros de la firma visible: " + e //$NON-NLS-1$
            		            );
            					AOUIFactory.showErrorMessage(
            		                  this.signPanel,
            		                  SimpleAfirmaMessages.getString("SignPanel.88"), //$NON-NLS-1$
            		                  SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
            		                  JOptionPane.ERROR_MESSAGE
            		            );
            				}
                    		return null;
            			}
						PdfEmptySignatureFieldsChooserDialog.startPdfEmptySignatureFieldsChooserDialog(
							this.signPanel.getDataToSign(),
							this.signPanel.getWindow(),
							field,
							new SignPdfDialogListener() {
								@Override
								public void propertiesCreated(final Properties extraParams) {
									// Solo hacemos la firma si hay propiedades visibles
									if (extraParams != null && !extraParams.isEmpty()) {
										SignPanelSignTask.this.getWaitDialog().setMessage(oldMessage);
										doSignature(currentSigner, extraParams);
									}
						        	if (getWaitDialog() != null) {
						        		getWaitDialog().dispose();
						        	}
								}
							}
						);
						return null;
            		}
            		if (getWaitDialog() != null) {
		        		getWaitDialog().dispose();
		        	}
            		return null;
            	}
        		try {
					SignPdfDialog.getVisibleSignatureExtraParams(
						currentSigner.isSign(this.signPanel.getDataToSign()),
						this.signPanel.getDataToSign(),
						this.signPanel.getWindow(),
						new SignPdfDialogListener() {
							@Override
							public void propertiesCreated(final Properties extraParams) {
								// Solo hacemos la firma si hay propiedades visibles
								if (extraParams != null && !extraParams.isEmpty()) {
									SignPanelSignTask.this.getWaitDialog().setMessage(oldMessage);
									doSignature(currentSigner, extraParams);
								}
					        	if (getWaitDialog() != null) {
					        		getWaitDialog().dispose();
					        	}
							}
						}
					);
				} catch (final Exception e) {
					LOGGER.severe(
		                "No se han podido cargar los parametros de la firma visible: " + e //$NON-NLS-1$
		            );
					AOUIFactory.showErrorMessage(
		                  this.signPanel,
		                  SimpleAfirmaMessages.getString("SignPanel.88"), //$NON-NLS-1$
		                  SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
		                  JOptionPane.ERROR_MESSAGE
		            );
				}
        		return null;
            }
        }

        // En cualquier otro caso
    	doSignature(currentSigner, null);
    	if (this.waitDialog != null) {
    		this.waitDialog.dispose();
    	}
        return null;
    }
}
