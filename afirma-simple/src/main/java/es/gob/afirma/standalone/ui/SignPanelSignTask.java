/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.CustomRuntimeConfigNeededException;
import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.RuntimeConfigNeededException.RequestType;
import es.gob.afirma.core.RuntimePasswordNeededException;
import es.gob.afirma.core.keystores.AOCancelledSMOperationException;
import es.gob.afirma.core.keystores.AuthenticationException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.LockedKeyStoreException;
import es.gob.afirma.core.keystores.PinException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.filters.EncodedCertificateFilter;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.IncorrectPageException;
import es.gob.afirma.signers.pades.InvalidSignaturePositionException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.manager.PermissionChecker;
import es.gob.afirma.standalone.plugins.manager.PluginException;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;
import es.gob.afirma.standalone.ui.SignOperationConfig.CryptoOperation;

final class SignPanelSignTask extends SwingWorker<Void, Void> {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** ExtraParam que configura que no aparezcan di&aacute;logos gr&aacute;ficos durante la firma. */
	private static final String EXTRAPARAM_HEADLESS = "headless"; //$NON-NLS-1$

	private final Component parent;
	private final List<SignOperationConfig> signConfigs;
	private final AOKeyStoreManager ksm;
	private final List<? extends CertificateFilter> certFilters;
	private PrivateKeyEntry selectedPke;
	private final CommonWaitDialog waitDialog;
	private final SignatureExecutor signExecutor;
	private final SignatureResultViewer resultViewer;

	private boolean needRelaunch = false;

	private final static List<String> invalidPageNumberFilesList = new ArrayList<>();


	SignPanelSignTask(final Component parent,
					  final List<SignOperationConfig> signConfigs,
					  final AOKeyStoreManager ksm,
			          final List<? extends CertificateFilter> certificateFilters,
			          final CommonWaitDialog signWaitDialog,
			          final SignatureExecutor signExecutor,
			          final SignatureResultViewer resultViewer) {
        this.parent = parent;
        this.signConfigs = signConfigs;
        this.ksm = ksm;
        this.certFilters = certificateFilters != null ? new ArrayList<>(certificateFilters) : null;
        this.selectedPke = null;
        this.waitDialog = signWaitDialog;
        this.signExecutor = signExecutor;
        this.resultViewer = resultViewer;
    }

	SignPanelSignTask(final Component parent,
			final List<SignOperationConfig> signConfigs,
			final PrivateKeyEntry pke,
			final CommonWaitDialog signWaitDialog,
			final SignatureExecutor signExecutor,
			final SignatureResultViewer resultViewer) {
		this.parent = parent;
		this.signConfigs = signConfigs;
		this.ksm = null;
		this.certFilters = null;
		this.selectedPke = pke;
		this.waitDialog = signWaitDialog;
		this.signExecutor = signExecutor;
		this.resultViewer = resultViewer;
	}


	@Override
    public Void doInBackground() {

        // Para cualquier otro formato de firma o PAdES no visible, firmaremos
    	// directamente
		doSignature();

        return null;
    }

	CommonWaitDialog getWaitDialog() {
		return this.waitDialog;
	}

	@Override
	protected void done() {
		if (this.signExecutor != null) {
			if (this.needRelaunch) {
				this.signExecutor.relaunchTask(this.selectedPke, this.signConfigs);
			}
			else {
				this.signExecutor.finishTask();
			}
		}
	}

    void doSignature() {

    	this.needRelaunch = false;

        if (this.signConfigs == null || this.signConfigs.isEmpty()) {
            return;
        }

        if (this.selectedPke == null) {
        	try {
        		this.selectedPke = getPrivateKeyEntry(this.certFilters, false);
        	}
        	catch (final AOCancelledOperationException e) {
        		return;
        	}
        	catch(final AOCertificatesNotFoundException e) {
        		LOGGER.severe("El almacen no contiene ningun certificado que se pueda usar para firmar: " + e); //$NON-NLS-1$
        		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.29"), e); //$NON-NLS-1$
        		return;
        	}
        	catch (final Exception e) {
        		LOGGER.severe("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e); //$NON-NLS-1$
        		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.56"), e); //$NON-NLS-1$
        		return;
        	}
        }

        // Si se va a firmar mas de un documento, se debera pedir de antemano la carpeta
        // de salida para ir almacenando las firmas en ella a medida que se generan
        final boolean onlyOneFile = this.signConfigs.size() == 1;
        File outDir = null;
        if (!onlyOneFile) {
        	outDir = selectOutDir();
        	if (outDir == null) {
        		return;
        	}
        }

        // Revisamos los directorios de entrada para calcular la ruta base de entrada,
        // que sera la que se ignore para el calculo de la ruta relativa con respecto
        // al directorio de salida en el que se guardaran los ficheros
        String inputBasePath = ""; //$NON-NLS-1$
        int inputBasePathLength = Integer.MAX_VALUE;
        for (final SignOperationConfig signConfig : this.signConfigs) {
        	if (signConfig.getDataFile().getParentFile() != null
        			&& signConfig.getDataFile().getParentFile().getAbsolutePath().length() < inputBasePathLength) {
        		inputBasePath = signConfig.getDataFile().getParentFile().getAbsolutePath();
        		inputBasePathLength = inputBasePath.length();
        	}
        }

        // Realizamos la firma de cada documento
        byte[] signResult = null;
        for (final SignOperationConfig signConfig : this.signConfigs) {

        	// Evitamos agregar nuevas firmas a los documentos con firmas no validas
        	if (signConfig.getSignValidity() != null
        			&& signConfig.getSignValidity().get(0).getValidity() == SIGN_DETAIL_TYPE.KO
        			&& !PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES)) {
        		LOGGER.severe("La entrada es una firma invalida. Se omitira"); //$NON-NLS-1$
        		continue;
        	}

        	final AOSigner currentSigner = signConfig.getSigner();

            // Anadimos las propiedades del sistema, habilitando asi que se puedan indicar
        	// opciones de uso con -D en linea de comandos y, sobre todo ello y si se firma
        	// mas de un fichero, se indica que no se muestren dialogos que puedan bloquear
        	// el proceso
        	signConfig.addExtraParams(System.getProperties());

        	// Si se esta firmando mas de un documento, evitamos que se muestren dialogos,
        	// mientras que si es solo uno y requiere la intervencion del usuario, se le
        	// pedira lo que se necesite y se cancelara la operacion si no se proporciona

            if (!onlyOneFile) {
            	signConfig.addExtraParam(EXTRAPARAM_HEADLESS, Boolean.TRUE.toString());
            }
            else if (signConfig.getSignValidity() != null
            		&& signConfig.getSignValidity().get(0).getValidity() == SignValidity.SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER
            		&& signConfig.getSignValidity().get(0).getErrorException() != null
            		&& signConfig.getSignValidity().get(0).getErrorException() instanceof RuntimeConfigNeededException) {

            	// Se requiere confirmacion por parte del usuario
            	final RuntimeConfigNeededException e = (RuntimeConfigNeededException) signConfig.getSignValidity().get(0).getErrorException();
        		if (e.getRequestType() == RequestType.CONFIRM) {
        			final int result = AOUIFactory.showConfirmDialog(this.parent, SimpleAfirmaMessages.getString(e.getRequestorText()),
        					SimpleAfirmaMessages.getString("SignPanel.153"), JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE); //$NON-NLS-1$
        			if (result == JOptionPane.YES_OPTION) {
        				if (e instanceof CustomRuntimeConfigNeededException) {
    						((CustomRuntimeConfigNeededException) e).prepareOperationWithConfirmation(signConfig.getExtraParams());
    					}
    					else {
            				signConfig.addExtraParam(e.getParam(), Boolean.TRUE.toString());
    					}
        			} else {
        				return;
        			}
        		}
        		// Se requiere ua contrasena por parte del usuario
        		else if (e.getRequestType() == RequestType.PASSWORD) {
        			char[] p;
        			try {
        				p = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(e.getRequestorText()), this.parent);
        			}
        			catch (final AOCancelledOperationException ce) {
        				p = null;
        			}
        			if (p != null) {
        				this.needRelaunch = true;
        				if (e instanceof RuntimePasswordNeededException) {
        					((RuntimePasswordNeededException) e).configure(signConfig.getExtraParams(), p);
        				}
        				else {
            				signConfig.addExtraParam(e.getParam(), new String(p));
        				}
        			}
        			else {
        				return;
        			}
        		}
    			else {
    				LOGGER.severe("No se puede gestionar la solicitud de datos necesaria para completar la firma: " + e); //$NON-NLS-1$
    				showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.154", e.getMessage()), e); //$NON-NLS-1$
    			}
            }

            byte[] data;
            try {
            	data = loadData(signConfig.getDataFile());
            }
            catch (final Exception e) {
            	LOGGER.severe("Error cargando el fichero a firmar: " + e); //$NON-NLS-1$
            	if (onlyOneFile) {
            		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.123"), e); //$NON-NLS-1$
            		return;
            	}
            	continue;
			}

        	final byte[] dataToSign = pluginsPreProcess(
        		data,
        		signConfig.getSignatureFormatName()
    		);

        	String signatureAlgorithm;
        	final String keyType = this.selectedPke.getPrivateKey().getAlgorithm();
        	String digestAlgorithm = signConfig.getDigestAlgorithm();
        	if (digestAlgorithm == null) {
        		digestAlgorithm = PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM);
        	}
        	try {
        		signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(digestAlgorithm, keyType);
        	}
        	catch (final Exception e) {
        		LOGGER.severe("El tipo de clave del certificado no esta soportado: " + e); //$NON-NLS-1$
        		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.123", keyType), null); //$NON-NLS-1$
        		return;
        	}

        	// Ejecutamos la operacion de firma apropiada
            try {
                signResult = signData(
            		dataToSign,
            		currentSigner,
            		signConfig,
            		signatureAlgorithm,
            		this.selectedPke,
            		onlyOneFile,
            		this.parent
        		);
            }
            catch (final AOCancelledOperationException e) {
                return;
            }
        	// Si la operacion que se cancelo fue con una tarjeta, refrescamos el almacen
        	// para restablecer el estado inicial
            catch (final AOCancelledSMOperationException e) {
           		refreshKeyStore();
           		return;
			}
            catch (final PinException e) {
            	LOGGER.log(Level.WARNING, "El PIN utilizado es incorrecto", e); //$NON-NLS-1$
            	// Identificamos el certificado que se utilizo
            	final Certificate selectedCert = this.selectedPke.getCertificate();
            	// Recargamos el almacen para evitar que el driver JMulticard quede en mal estado
        		refreshKeyStore();
            	// Preparamos un filtro para utilizar el mismo certificado
            	List<? extends CertificateFilter> filters;
				try {
					final String certEncoded = Base64.encode(selectedCert.getEncoded());
					filters = Collections.singletonList(new EncodedCertificateFilter(certEncoded));
				} catch (final CertificateEncodingException e1) {
					LOGGER.log(Level.SEVERE, "Error en la codificacion del certificado. No se selecionara automaticamente", e); //$NON-NLS-1$
					filters = null;
				}
				// Seleccionamos el certificado
				try {
					this.selectedPke = getPrivateKeyEntry(filters, true);
				}
				catch (final Exception e2) {
					LOGGER.log(Level.SEVERE, "Ocurrio un error al extraer la clave privada del certificiado seleccionado", e2); //$NON-NLS-1$
					showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.56"), e); //$NON-NLS-1$
					return;
				}
				// Volvemos a ejecutar la firma
            	doSignature();
            	return;
            }
            catch (final LockedKeyStoreException e) {
            	LOGGER.log(Level.SEVERE, "El almacen de claves o o el certificado esta bloqueado", e); //$NON-NLS-1$
            	// Recargamos el almacen para evitar que el driver JMulticard quede en mal estado
            	refreshKeyStore();
            	showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.161"), e); //$NON-NLS-1$
            	return;
            }
            catch (final AuthenticationException e) {
            	LOGGER.log(Level.SEVERE, "Error al firmar por un problema de acceso al almacen o la clave del certificado", e); //$NON-NLS-1$
            	showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.162"), e); //$NON-NLS-1$
            	return;
            }
            catch (final RuntimeConfigNeededException e) {
            	LOGGER.warning("No se puede completar la firma sin intervencion del usuario: " + e); //$NON-NLS-1$
            	if (onlyOneFile) {
            		// Se requiere confirmacion por parte del usuario
            		if (e.getRequestType() == RequestType.CONFIRM) {
            			final int result = AOUIFactory.showConfirmDialog(this.parent, SimpleAfirmaMessages.getString(e.getRequestorText()),
            					SimpleAfirmaMessages.getString("SignPanel.153"), JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE); //$NON-NLS-1$
            			if (result == JOptionPane.YES_OPTION) {
            				this.needRelaunch = true;
            				if (e instanceof CustomRuntimeConfigNeededException) {
        						((CustomRuntimeConfigNeededException) e).prepareOperationWithConfirmation(signConfig.getExtraParams());
        					}
        					else {
                				signConfig.addExtraParam(e.getParam(), Boolean.TRUE.toString());
        					}
            			}
            		}
            		// Se requiere ua contrasena por parte del usuario
            		else if (e.getRequestType() == RequestType.PASSWORD) {
            			char[] p;
            			try {
            				p = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(e.getRequestorText()), this.parent);
            			}
            			catch (final AOCancelledOperationException ce) {
            				p = null;
            			}
            			if (p != null) {
            				this.needRelaunch = true;
            				if (e instanceof RuntimePasswordNeededException) {
            					((RuntimePasswordNeededException) e).configure(signConfig.getExtraParams(), p);
            				}
            				else {
	            				signConfig.addExtraParam(e.getParam(), new String(p));
            				}
            			}
            		}
        			else {
        				LOGGER.severe("No se puede gestionar la solicitud de datos necesaria para completar la firma: " + e); //$NON-NLS-1$
        				showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.154", e.getMessage()), e); //$NON-NLS-1$
        			}
            		return;
            	}
            	continue;
			}
            catch (final AOFormatFileException e) {
            	LOGGER.warning("La firma o el documento no son aptos para firmar: " + e); //$NON-NLS-1$
            	if (onlyOneFile) {
            		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.154", e.getMessage()), e); //$NON-NLS-1$
            		return;
            	}
            	continue;
            }
            catch(final SingleSignatureException e) {
            	LOGGER.warning("Error en una firma del lote, se continua con la siguiente: " + e); //$NON-NLS-1$
            	continue;
            }
            catch(final Exception e) {
                LOGGER.log(Level.SEVERE, "Error durante el proceso de firma: " + e, e); //$NON-NLS-1$
            	if (onlyOneFile) {
            		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.65"), e); //$NON-NLS-1$
            		return;
            	}
            	continue;
            }
            catch(final OutOfMemoryError ooe) {
                LOGGER.severe("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$
            	if (onlyOneFile) {
            		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.1"), ooe); //$NON-NLS-1$
            		return;
            	}
            	continue;
            }
        	catch (final Error e) {
        		LOGGER.log(Level.SEVERE, "Error interno durante el proceso de firma", e); //$NON-NLS-1$
        		if (onlyOneFile) {
            		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.65"), e); //$NON-NLS-1$
            		return;
            	}
            	continue;
    		}

			signResult = pluginsPostProcess(signResult, signConfig.getSignatureFormatName(), this.selectedPke.getCertificateChain());

            // En caso de definirse directorio de salida, se guarda la firma
            if (outDir != null) {
            	final String defaultFilename = signConfig.getSigner().getSignedName(
            			signConfig.getDataFile().getName(), "_signed"); //$NON-NLS-1$

            	String relativePath = ""; //$NON-NLS-1$
            	if (signConfig.getDataFile() != null && !inputBasePath.isEmpty()) {
            		relativePath = signConfig.getDataFile().getParentFile().getAbsolutePath().substring(
            				inputBasePath.length()) + File.separator;
            	}

            	final File defaultOutFile = new File(outDir, relativePath + defaultFilename);
            	final boolean overwrite = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_MASSIVE_OVERWRITE);

            	File outFile;
            	try {
            		outFile = saveDataToFile(signResult, defaultOutFile, overwrite);
            	}
            	catch (final Exception e) {
            		LOGGER.log(Level.WARNING, "Error al guardar una de las firmas generadas", e); //$NON-NLS-1$
            		continue;
				}
            	signConfig.setSignatureFile(outFile);
            }

        }

        // Terminado el proceso, notificamos al plugin que puede reiniciarse si es preciso
        pluginsReset();

        // Si solo habia una firma, decidimos ahora donde se guarda
        if (onlyOneFile) {
        	final File signatureFile;
        	final SignOperationConfig signConfig = this.signConfigs.get(0);
        	try {
        		signatureFile = saveSignature(
    				signResult, signConfig.getSigner(),
    				signConfig.getDataFile(),
    				this.parent
				);
        	}
        	catch(final AOCancelledOperationException e) {
        		return;
        	}
        	catch(final Exception e) {
        		LOGGER.log(Level.SEVERE, "No se ha podido guardar el resultado de la firma", e); //$NON-NLS-1$
        		showErrorMessage(SimpleAfirmaMessages.getString("SignPanel.88"), e); //$NON-NLS-1$
        		return;
        	}

        	signConfig.setSignatureFile(signatureFile);

        	this.resultViewer.showResultsInfo(
        			signResult,
        			signConfig,
        			(X509Certificate) this.selectedPke.getCertificate()
	    		);
        }
        else {
        	this.resultViewer.showResultsInfo(
    			this.signConfigs,
    			outDir,
    			(X509Certificate) this.selectedPke.getCertificate()
			);
        }

    	if (!invalidPageNumberFilesList.isEmpty()) {
    		String invalidPageNumberFiles = ""; //$NON-NLS-1$
    		for (final String fileName : invalidPageNumberFilesList) {
    			invalidPageNumberFiles += fileName + "\n"; //$NON-NLS-1$
    		}

			AOUIFactory.showMessageDialog(
					this,
					SimpleAfirmaMessages.getString("SignResultPanel.34") + "\n" + invalidPageNumberFiles, //$NON-NLS-1$ //$NON-NLS-2$
	                SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
	                JOptionPane.WARNING_MESSAGE
	        );

			invalidPageNumberFilesList.clear();
    	}
    }

    /**
     * Actualiza el almac&eacute;n de claves cargado.
     */
    private void refreshKeyStore() {
    	this.selectedPke = null;
    	try {
			this.ksm.refresh();
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Error al actualizar el almacen de claves", e); //$NON-NLS-1$
		}
	}

	/**
     * Ejecuta una operaci&oacute;n de firma.
     * @param data Datos a firmar.
     * @param signer Manejador de firma con el que realizar la operaci&oacute;n.
     * @param signConfig Configuraci&oacute;n de la firma.
     * @param algorithm Algoritmo de firma.
     * @param pke Referencia al certificado y clave de firma.
     * @param onlyOneFile {@code true} si s&oacute;lo se carga un fichero en el panel,
     * {@code false} si se carga m&aacute;s de uno.
     * @param parent Componente padre.
     * @return Firma electr&oacute;nica generada.
     * @throws AOException Cuando se produce un error relacionado a la generaci&oacute;n del formato de firma.
     * @throws IOException Cuando se produce un error relacionado a la lectura de los datos a firmar.
     * @throws SingleSignatureException Cuando se produce un error en una firma dentro de un proceso masivo.
     */
    private static byte[] signData(final byte[] data,
    						       final AOSigner signer,
    						       final SignOperationConfig signConfig,
    						       final String algorithm,
    						       final PrivateKeyEntry pke,
    						       final boolean onlyOneFile,
    						       final Component parent) throws AOException,
                                                                  IOException,
                                                                  SingleSignatureException {
    	final CryptoOperation cop = signConfig.getCryptoOperation();
    	final Properties extraParams = signConfig.getExtraParams();
    	byte[] signResult;
    	try {
    		switch (cop) {
    		case COSIGN:
    			signResult = signer.cosign(
					data,
					algorithm,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					extraParams
				);
    			break;

    		case COUNTERSIGN_LEAFS:
    			signResult = signer.countersign(
					data,
					algorithm,
					CounterSignTarget.LEAFS,
					null,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					extraParams
				);
    			break;

    		case COUNTERSIGN_TREE:
    			signResult = signer.countersign(
					data,
					algorithm,
					CounterSignTarget.TREE,
					null,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					extraParams
				);
    			break;

    		default:	// Firma
    			signResult = signer.sign(
					data,
					algorithm,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					extraParams
				);
    		}
    	}
    	catch(final IncorrectPageException e) {
        	LOGGER.warning("El documento no dispone de las paginas donde estampar la firma visible: " + e); //$NON-NLS-1$
        	final Properties newExtraParams = (Properties) extraParams.clone();
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_PAGE);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_PAGES);
        	signConfig.setExtraParams(newExtraParams);
        	if (!invalidPageNumberFilesList.contains(signConfig.getDataFile().getName())) {
            	invalidPageNumberFilesList.add(signConfig.getDataFile().getName());
        	}
        	signResult = signData(data, signer, signConfig, algorithm, pke, onlyOneFile, parent);
        }
    	catch(final InvalidSignaturePositionException e) {
        	LOGGER.warning("No es posible estampar la firma visible ya que la posicion indicada se encuentra fuera de rango " + e); //$NON-NLS-1$
        	final Properties newExtraParams = (Properties) extraParams.clone();
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_PAGE);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_PAGES);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX);
        	newExtraParams.remove(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY);
        	signConfig.setExtraParams(newExtraParams);
        	if (!invalidPageNumberFilesList.contains(signConfig.getDataFile().getName())) {
            	invalidPageNumberFilesList.add(signConfig.getDataFile().getName());
        	}
        	signResult = signData(data, signer, signConfig, algorithm, pke, onlyOneFile, parent);
        }

    	return signResult;
    }

    /** Guarda una firma en disco permitiendo al usuario seleccionar el fichero
     * de salida mediante un di&aacute;logo.
     * @param signature Datos a guardar.
     * @param signer Manejador de firma utilizado.
     * @param dataFile Fichero de datos firmado.
     * @param parent Componente padre.
     * @return Fichero guardado.
     * @throws IOException Cuando ocurre un error durante el guardado. */
    private static File saveSignature(final byte[] signature,
    		                          final AOSigner signer,
    		                          final File dataFile,
    		                          final Component parent) throws IOException {

    	final String newFileName = signer.getSignedName(
			dataFile.getName(),
			"_signed" //$NON-NLS-1$
		);

    	final List<GenericFileFilter> filters = new ArrayList<>();

    	GenericFileFilter defaultFilter = null;

    	if (signer instanceof AOPDFSigner) {
    		defaultFilter = new GenericFileFilter (new String[] {"pdf"}, SimpleAfirmaMessages.getString("SignPanel.72")); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	else if (signer instanceof AOXAdESSigner || signer instanceof AOFacturaESigner) {
    		defaultFilter = new GenericFileFilter(new String[] {"xsig", "xml"}, SimpleAfirmaMessages.getString("SignPanel.76"));  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
    	}
    	else if (signer instanceof AOOOXMLSigner) {
    		if (newFileName.toLowerCase().endsWith("docx")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"docx"}, SimpleAfirmaMessages.getString("SignPanel.91")); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    		else if (newFileName.toLowerCase().endsWith("pptx")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"pptx"}, SimpleAfirmaMessages.getString("SignPanel.92"));  //$NON-NLS-1$//$NON-NLS-2$
    		}
    		else if (newFileName.toLowerCase().endsWith("xlsx")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"xlsx"}, SimpleAfirmaMessages.getString("SignPanel.93"));  //$NON-NLS-1$//$NON-NLS-2$
    		}
    		else {
    			defaultFilter = new GenericFileFilter(new String[] {"ooxml"}, SimpleAfirmaMessages.getString("SignPanel.94"));  //$NON-NLS-1$//$NON-NLS-2$
    		}
    	}
    	else if (signer instanceof AOODFSigner) {
    		if (newFileName.toLowerCase().endsWith("odt")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"odt"}, SimpleAfirmaMessages.getString("SignPanel.96")); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    		else if (newFileName.toLowerCase().endsWith("odp")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"odp"}, SimpleAfirmaMessages.getString("SignPanel.97")); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    		else if (newFileName.toLowerCase().endsWith("ods")) { //$NON-NLS-1$
    			defaultFilter = new GenericFileFilter(new String[] {"ods"}, SimpleAfirmaMessages.getString("SignPanel.98"));  //$NON-NLS-1$//$NON-NLS-2$
    		}
    		else {
    			defaultFilter = new GenericFileFilter(new String[] {"odf"}, SimpleAfirmaMessages.getString("SignPanel.99")); //$NON-NLS-1$ //$NON-NLS-2$
    		}
    	}
    	else {
    		defaultFilter = new GenericFileFilter(new String[] {"csig", "p7s"}, SimpleAfirmaMessages.getString("SignPanel.80")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    	}
		filters.add(defaultFilter);

    	return AOUIFactory.getSaveDataToFile(
    				signature,
    				SimpleAfirmaMessages.getString("SignPanel.81"), //$NON-NLS-1$
    				dataFile.getParent(),
    				newFileName,
    				filters,
    				defaultFilter,
    				parent
    				);
    }

	private PrivateKeyEntry getPrivateKeyEntry(final List<? extends CertificateFilter> filters,
			final boolean mandatoryCertificate) throws AOCertificatesNotFoundException,
	                                                    KeyStoreException,
	                                                    NoSuchAlgorithmException,
	                                                    UnrecoverableEntryException {

    	final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
			this.ksm,
			this.parent,
			true,             // Comprobar claves privadas
			PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS), // Mostrar certificados caducados
			true,             // Comprobar validez temporal del certificado
			filters,
			mandatoryCertificate
			);
    	dialog.show();

    	final CertificateContext context = dialog.getSelectedCertificateContext();

    	// Obtenemos el almacen del certificado seleccionado (que puede no ser el mismo
    	// que se indico originalmente por haberlo cambiado desde el dialogo de seleccion)
    	final KeyStoreManager currentKsm = context.getKeyStoreManager();
    	currentKsm.setParentComponent(this.parent);

    	return currentKsm.getKeyEntry(context.getAlias());
	}

	/**
	 * Carga en memoria el contenido de un fichero.
	 * @param file Fichero a cargar.
	 * @return Contenido del fichero.
	 * @throws IOException Cuando ocurre un error durante la carga.
	 */
	private static byte[] loadData(final File file) throws IOException {
		int n = 0;
		final byte[] buffer = new byte[4096];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try (FileInputStream fis = new FileInputStream(file)) {
			while ((n = fis.read(buffer)) > 0) {
				baos.write(buffer, 0, n);
			}
		}
		return baos.toByteArray();
	}

    /**
     * Permite al usuario seleccionar un directorio para el guardado de los ficheros.
     * Por defecto, se usar&aacute; el de los ficheros a firmar seleccionados.
     * @return Directorio de salida o {@code null} si no se ha seleccionado ninguno.
     */
    private File selectOutDir() throws AOCancelledOperationException {
    	//Usamos como directorio por defecto el mismo de los datos
    	final String currentDir = this.signConfigs.get(0).getDataFile().getParent();

    	final File[] dirs;
    	try {
    		dirs = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("SignPanelSignTask.3"), //$NON-NLS-1$
				currentDir,
				null,
				null,
				null,
				true,
				false,
				DesktopUtil.getDefaultDialogsIcon(),
				this.parent
			);
    	}
    	catch (final AOCancelledOperationException e) {
    		return null;
    	}
    	return dirs != null ? dirs[0] : null;
	}

    /** Guarda datos en un directorio con un nombre concreto.
	 * @param data Datos a guardar.
	 * @param defaultOutFile Fichero de salida por defecto.
	 * @param overwrite Si se permite sobreescribir el fichero de salida.
	 * @return Fichero en el que se guardan los datos.
	 * @throws IOException Cuando se produce un error durante el guardado. */
	private static File saveDataToFile(final byte[] data, final File defaultOutFile, final boolean overwrite) throws IOException {
		File outFile = defaultOutFile;
		if (!overwrite) {
			int i = 1;
			while (outFile.isFile()) {
				final String defaultFilename = defaultOutFile.getName();
				final int extPos = defaultFilename.lastIndexOf('.');
				final String filename = defaultFilename.substring(0, extPos) + '(' + i + ')' + defaultFilename.substring(extPos);
				outFile = new File(defaultOutFile.getParentFile(), filename);
				i++;
			}
		}

		if (!defaultOutFile.getParentFile().isDirectory() && !defaultOutFile.getParentFile().mkdirs()) {
			throw new IOException("No se pudo crear el directorio de salida de la firma"); //$NON-NLS-1$
		}

		try (FileOutputStream fos = new FileOutputStream(outFile)) {
			fos.write(data, 0, data.length);
		}
		return outFile;
	}


	/**
	 * Muestra un di&aacute;logo con un mensaje de error.
	 * @param message Mensaje de error.
	 * @param t Causa del error.
	 */
    static void showErrorMessage(final String message, final Throwable t) {
		AOUIFactory.showErrorMessage(
			message,
			SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			JOptionPane.ERROR_MESSAGE,
			t
		);
    }

    /** Muestra un di&aacute;logo para indicar al usuario que se ha encontrado una dificultad durante
     * la operaci&oacute;n y consultarle si desea continuar o no con ella.
     * @param parent Componente padre.
     * @param message Mensaje de advertencia.
     * @return Si el usuario acepto el di&aacute;logo.
     */
    static boolean showConfirmRequest(final Component parent, final String message) {
		final int response = AOUIFactory.showConfirmDialog(
			parent,
			message,
			SimpleAfirmaMessages.getString("SimpleAfirma.8"), //$NON-NLS-1$
			JOptionPane.YES_NO_OPTION,
			JOptionPane.WARNING_MESSAGE
		);
		return response == JOptionPane.YES_OPTION;
    }

    private static byte[] pluginsPreProcess(final byte[] data, final String format) {

    	final PluginsManager pluginsManager = SimpleAfirma.getPluginsManager();
    	final List<AfirmaPlugin> plugins;
		try {
			plugins = pluginsManager.getPluginsLoadedList();
		}
		catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido cargar el listado de plugins instalados", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
                SimpleAfirmaMessages.getString("SignPanel.113"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE,
                e
            );
			return data;
		}

		byte[] processedData = data;
    	for (final AfirmaPlugin plugin : plugins) {
    		final PluginInfo info = plugin.getInfo();
    		if (PermissionChecker.check(info, Permission.PRESIGN)) {
    			try {
    				processedData = plugin.preSignProcess(processedData, format);
    			}
    			catch (final PluginControlledException e) {
    				LOGGER.log(Level.WARNING, "El plugin " + plugin + " lanzo una excepcion controlada", e); //$NON-NLS-1$ //$NON-NLS-2$
    				AOUIFactory.showErrorMessage(
    						e.getMessage(),
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    						JOptionPane.WARNING_MESSAGE,
    						e
    						);
    			}
    			catch (Exception | Error e) {
    				LOGGER.log(Level.SEVERE, "Ocurrio un error grave al preprocesar los datos con el plugin " + plugin + //$NON-NLS-1$
    						". Se continuara el proceso con el resto de plugins", e); //$NON-NLS-1$
    				AOUIFactory.showErrorMessage(
    						SimpleAfirmaMessages.getString("SignPanel.111", plugin.toString(), e.getMessage()), //$NON-NLS-1$
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    						JOptionPane.ERROR_MESSAGE,
    						e
    						);
    			}
    		}
    	}

    	return processedData;
    }

    private static byte[] pluginsPostProcess(final byte[] signature, final String format, final Certificate[] certChain) {

    	final PluginsManager pluginsManager = SimpleAfirma.getPluginsManager();
    	final List<AfirmaPlugin> plugins;
		try {
			plugins = pluginsManager.getPluginsLoadedList();
		}
		catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido cargar el listado de plugins instalados", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
                SimpleAfirmaMessages.getString("SignPanel.114"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE,
                e
            );
			return signature;
		}

    	byte[] processedSignature = signature;
    	for (final AfirmaPlugin plugin : plugins) {
    		final PluginInfo info = plugin.getInfo();
    		if (PermissionChecker.check(info, Permission.POSTSIGN)) {
    			try {
    				processedSignature = plugin.postSignProcess(processedSignature, format, certChain);
    			}
    			catch (final PluginControlledException e) {
    				LOGGER.log(Level.WARNING, "El plugin " + plugin + " lanzo una excepcion controlada", e); //$NON-NLS-1$ //$NON-NLS-2$
    				AOUIFactory.showErrorMessage(
    						e.getMessage(),
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    						JOptionPane.WARNING_MESSAGE,
    						e
    						);
    			}
    			catch (Exception | Error e) {
    				LOGGER.log(Level.SEVERE, "Ocurrio un error grave al postprocesar la firma con el plugin " + plugin + //$NON-NLS-1$
    						". Se continuara el proceso con el resto de plugins", e); //$NON-NLS-1$
    				AOUIFactory.showErrorMessage(
    						SimpleAfirmaMessages.getString("SignPanel.112", plugin.toString(), e.getMessage()), //$NON-NLS-1$
    						SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    						JOptionPane.ERROR_MESSAGE,
    						e
    						);
    			}
    		}
    	}

    	return processedSignature;
    }

    private static void pluginsReset() {

    	final PluginsManager pluginsManager = SimpleAfirma.getPluginsManager();
    	final List<AfirmaPlugin> plugins;
		try {
			plugins = pluginsManager.getPluginsLoadedList();
		}
		catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se ha podido cargar el listado de plugins instalados", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
                SimpleAfirmaMessages.getString("SignPanel.114"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE,
                e
            );
			return;
		}

    	for (final AfirmaPlugin plugin : plugins) {
    		try {
    			plugin.reset();
    		}
    		catch (final PluginControlledException e) {
    			LOGGER.log(Level.WARNING, "El plugin " + plugin + " lanzo una excepcion controlada", e); //$NON-NLS-1$ //$NON-NLS-2$
    			AOUIFactory.showErrorMessage(
					e.getMessage(),
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE,
					e
				);
    		}
    		catch (Exception | Error e) {
    			LOGGER.log(Level.SEVERE, "Ocurrio un error grave al reiniciar la firma con el plugin " + plugin, e); //$NON-NLS-1$
    			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("SignPanel.115", plugin.toString(), e.getMessage()), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e
				);
    		}
    	}
    }
}
