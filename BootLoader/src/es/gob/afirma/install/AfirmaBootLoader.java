/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.install;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.InstaladorClienteEntryPoints;
import es.gob.afirma.misc.AOBootUtil;
import es.gob.afirma.misc.AOInstallParameters;

/**
 * Clase encargada de instalar el cliente de firma del Ministerio de la Presidencia, tanto
 * las librer&iacute;as de las que depende como el propio applet cliente (cliente + librerias
 * de compatibilidad con afirma 5).
 */
public final class AfirmaBootLoader extends JApplet implements InstaladorClienteEntryPoints {

	private static final long serialVersionUID = -2570412953683244702L;

	/** Directorio de instalacion del cliente afirma. */
	private static final String DEFAULT_AFIRMA_DIR = "afirma.5"; //$NON-NLS-1$

	/** JAR principal del cliente afirma (Imprescindible). */
	public static final String MAIN_JAR = "afirma5_coreV3.jar"; //$NON-NLS-1$
	
	/** JAR de compatibilidad con Java 5 del cliente afirma (Opcional). */
	public static final String COMPATIBILITY_JAR = "afirma_5_java_5.jar"; //$NON-NLS-1$
	
	/** Direcci&oacute;n remota del applet. */
	private URL codeBase = null;
	
	private boolean iniciado = false;
	
	/** Instalador del cliente. */
	private static Installer installer = null;
		
	public boolean isIniciado() {
		Logger.getLogger("es.gob.afirma").info("Invocando isIniciado()"); //$NON-NLS-1$ //$NON-NLS-2$
		return iniciado;
	}
	
	public boolean setBaseDownloadURL(final String url) {
		Logger.getLogger("es.gob.afirma").info("Invocando setBaseDownloadURL(String)"); //$NON-NLS-1$ //$NON-NLS-2$
		try {
			codeBase = new URL(url);
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"La URL establecida es incorrecta, se usara la original (" + codeBase.toString() + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return false;
		}
		return true;
	}
	
	@Override
	public void init() {

		this.codeBase = this.getCodeBase();
		iniciado = true;

		// El directorio de instalacion se nos puede pasar como un parametro del applet
		final String dir=getParameter("installDirectory"); //$NON-NLS-1$

		if (dir!=null && !dir.trim().equals("") //$NON-NLS-1$
				&& !dir.equalsIgnoreCase("null") //$NON-NLS-1$
				&& !dir.equalsIgnoreCase("undefined")) { //$NON-NLS-1$
			AOInstallParameters.setInstallationDirectory(new File(dir).getName());
		} else {
			AOInstallParameters.setInstallationDirectory(DEFAULT_AFIRMA_DIR);
		}
		
		// Comprobamos la accion a realizar si encontramos version antiguas del cliente @firma
		int action = -1;
		final String oldVersionsAction = getParameter("oldVersionsAction"); //$NON-NLS-1$
		if (oldVersionsAction!=null && !oldVersionsAction.trim().equals("") //$NON-NLS-1$
				&& !oldVersionsAction.equalsIgnoreCase("null") //$NON-NLS-1$
				&& !oldVersionsAction.equalsIgnoreCase("undefined")) //$NON-NLS-1$
			try {
				action = Integer.parseInt(oldVersionsAction);
			} catch (Exception e) {
				Logger.getLogger("es.gob.afirma").warning("La accion indicada a realizar frente a versiones antiguas del cliente no es valida: "+oldVersionsAction); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Configuramos el instalador
			final AOInstallParameters params = new AOInstallParameters();
			if(action != -1) params.oldVersionsAction = action;

			installer = new Installer(this, params);

			Logger.getLogger("es.gob.afirma").info("BootLoader de @firma iniciado"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public String instalar() {
		// Realizamos la instalacion
		return instalar(Installer.BUILD_LITE);
	}
	
	public String instalar(final String build) {
		return instalar(build, null, null);
	}
	
	public String instalar(final String build, final String jsMethodName, final Object jsMethodParams) {
		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Invocando instalar(" + build + ", " +  jsMethodName + ", " + jsMethodParams + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$  //$NON-NLS-4$
		
		// Realizamos la instalacion
		final String buildToInstall = (build != null ? build : Installer.BUILD_LITE);
		AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {

						// Indica si la instalacion finalizo correctamente 
						boolean installOk = true;
						
						// Mostramos el acuerdo de licencia y, si se acepta, se instala el cliente
						if(installer.prepareInstall(build, AfirmaBootLoader.this)) {
							installOk = AfirmaBootLoader.this.installCore(buildToInstall, false);
							if(installOk)
								installOk = AfirmaBootLoader.this.installDependencies(buildToInstall, false);

							// Informamos del resultado de la instalacion
							if(installOk) {
								JOptionPane.showMessageDialog(
										AfirmaBootLoader.this, 
										"La instalaci\u00F3n ha finalizado correctamente.",
										"Informaci\u00F3n",
										JOptionPane.INFORMATION_MESSAGE
								);
							} else {
								JOptionPane.showMessageDialog(
										AfirmaBootLoader.this, 
										"La instalaci\u00F3n ha finalizado con errores.",
										"Informaci\u00F3n",
										JOptionPane.ERROR_MESSAGE
								);
								return;
							}
						}
						else {
							// Si se cancela la instalacion se evita ejecutar cualquier accion JavaScript
							return;
						}
						
						// Si se especifico alguna operacion JavaScript posterior a la instalacion, la
						// ejecutamos y, si se produce algun error al invocarla, se le pide al usuario que
						// reinicie su navegador
						if (jsMethodName != null && !"".equals(jsMethodName)) {
							
							Object[] jsPreparedParams = AOBootUtil.prepareJSParams(jsMethodParams);
							
							try {
								netscape.javascript.JSObject.getWindow(AfirmaBootLoader.this).call(jsMethodName, jsPreparedParams);
							}
							catch(final Throwable e) {
								Logger.getLogger("es.gob.afirma").severe(
										"No se ha podido realizar la llamada al metodo JavaScript '" + jsMethodName + "': " + e
								);
								JOptionPane.showMessageDialog(
										AfirmaBootLoader.this, 
										"Por favor, reinicie su navegador Web.",
										"Informaci\u00F3n",
										JOptionPane.WARNING_MESSAGE
								);
								return;
							}
						}
					}
				});
				return true;
			}
		});
		
		return getInstallationDirectory();
	}
	
	public boolean desinstalar() {
		Logger.getLogger("es.gob.afirma").info("Invocando desinstalar()"); //$NON-NLS-1$ //$NON-NLS-2$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
			    return installer.uninstall();
			}
		});
	}
	
	public boolean desinstalarAntiguas() {
		Logger.getLogger("es.gob.afirma").info("Invocando desinstalarAntiguas()"); //$NON-NLS-1$ //$NON-NLS-2$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				String userHome = null;
				try {
					userHome = System.getProperty("user.home") + File.separator; //$NON-NLS-1$
				} catch (Exception e) {
					Logger.getLogger("es.gob.afirma").severe("No se ha podido localizar el directorio de usuario, se abortara la operacion: "+e); //$NON-NLS-1$ //$NON-NLS-2$
					return false;
				}

				final File oldInstallationDir = new File(userHome +".clienteFirmaArrobaFirma5"); //$NON-NLS-1$
				if(oldInstallationDir.exists() && oldInstallationDir.isDirectory()) {
					return installer.uninstallOldVersion(oldInstallationDir.getAbsolutePath());
				}
				return true;
			}
		});
	}
	
	public String getInstallationDirectory() {
		Logger.getLogger("es.gob.afirma").info("Invocando getInstallationDirectory()"); //$NON-NLS-1$ //$NON-NLS-2$
		return AccessController.doPrivileged(new PrivilegedAction<String>() {
            public String run() {
                return AOInstallParameters.getHomeApplication();
            }}
        );
	}
	
	public boolean isInstalado() {
	    Logger.getLogger("es.gob.afirma").info("Invocando isInstalado()"); //$NON-NLS-1$ //$NON-NLS-2$
	    return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
	        public Boolean run() {
	            return installer.isAlreadyInstalled(Installer.BUILD_LITE);
	        }
	    });
	}

	public boolean isInstalado(final String build) {
	    Logger.getLogger("es.gob.afirma").info("Invocando isInstalado(String)"); //$NON-NLS-1$ //$NON-NLS-2$
	    return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
	        public Boolean run() {
	            String buildToEval = build;
	            if (build == null || build.equals("") || build.equalsIgnoreCase("base")) //$NON-NLS-1$ //$NON-NLS-2$
	                buildToEval = Installer.BUILD_LITE;

	            return installer.isAlreadyInstalled(buildToEval);
	        }
	    });
	}
	
	public boolean isDependenciesInstalled(final String build) {
		Logger.getLogger("es.gob.afirma").info("Invocando isDependenciesInstalled(String)"); //$NON-NLS-1$ //$NON-NLS-2$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				return installer.isDependenciesInstalled(build);
			}
		});
	}
	
	public String getAllJars() {
		Logger.getLogger("es.gob.afirma").info("Invocando getAllJars()"); //$NON-NLS-1$ //$NON-NLS-2$
		return MAIN_JAR;
	}

	public boolean actualizar() {
		Logger.getLogger("es.gob.afirma").info("Invocando actualizar()"); //$NON-NLS-1$ //$NON-NLS-2$
		return actualizar(null, null);
	}
	
	public boolean actualizar(final String jsMethodName, final Object jsMethodParams) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {

				// Determinamos la construccion instalada y la actualizamos
				final String build = AOBootUtil.getBuildVersion(getInstallationDirectory() + File.separator + MAIN_JAR);
				if(build == null) {
					instalar(Installer.BUILD_LITE);
					return true;
				}
				
				// Si ya esta actualizado no hacemos nada
				if(installer.isUpdated(AfirmaBootLoader.this.codeBase)) {
					JOptionPane.showMessageDialog(AfirmaBootLoader.this, Messages.getString("Installer.4"), Messages.getString("Installer.2"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
					return true;
				}
				
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						boolean updateOK = 
							installer.update(
									AfirmaBootLoader.this.codeBase,
									build);

						// Si la actualizacion finalizo correctamente informamos de ello
						if(updateOK && installer.isUpdated(AfirmaBootLoader.this.codeBase)) {
							JOptionPane.showMessageDialog(
									AfirmaBootLoader.this,
									Messages.getString("Installer.6"), //$NON-NLS-1$
									Messages.getString("Installer.2"), //$NON-NLS-1$
									JOptionPane.INFORMATION_MESSAGE
							);
						}
						else {
							JOptionPane.showMessageDialog(
									AfirmaBootLoader.this,
									Messages.getString("Installer.7"), //$NON-NLS-1$
									Messages.getString("Installer.2"), //$NON-NLS-1$
									JOptionPane.ERROR_MESSAGE
							);
						}

						// Si se ha definido una accion posterior, la ejecutamos
						if (jsMethodName != null && !"".equals(jsMethodName)) {
							
							Object[] jsPreparedParams = AOBootUtil.prepareJSParams(jsMethodParams);
							try {
								netscape.javascript.JSObject.getWindow(AfirmaBootLoader.this).call(jsMethodName, jsPreparedParams);
							}
							catch(final Throwable e) {
								Logger.getLogger("es.gob.afirma").severe(
										"No se ha podido realizar la llamada al metodo JavaScript '" + jsMethodName + "': " + e
								);
								JOptionPane.showMessageDialog(
										AfirmaBootLoader.this, 
										"Informaci\u00F3n", 
										"Por favor, reinicie su navegador Web.",
										JOptionPane.WARNING_MESSAGE
								);
							}
						}
					}
				});
				return true;
			}
		});
	}
	
	public boolean isActualizado() {
	    Logger.getLogger("es.gob.afirma").info("Invocando isActualizado()"); //$NON-NLS-1$ //$NON-NLS-2$
	    return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
	        public Boolean run() {

	            // Comprobamos que este al menos instalada la construccion basica del cliente
	            if(!installer.isAlreadyInstalled(Installer.BUILD_LITE)) {
	                return false;
	            }

	            return installer.isUpdated(AfirmaBootLoader.this.codeBase);
	        }
	    });
	}

    public String getVersion() {
        InputStream is = AfirmaBootLoader.class.getResourceAsStream("/version.properties"); //$NON-NLS-1$
        final String idVersion = AOBootUtil.getVersion(is);
        try {is.close();} catch (Exception e) {}
        return idVersion;
    }

    public String getClientVersion() {
        final String version = AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                return installer.getVersion();
            }
        });
        return version != null ? version : "0.0.0"; //$NON-NLS-1$
    }

		public boolean isInstallationNeeded() {
			Logger.getLogger("es.gob.afirma").info("Invocando isInstallationNeeded()"); //$NON-NLS-1$ //$NON-NLS-2$
			return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
				public Boolean run() {
					String osName = null;
					String javaVersion = null;
					String javaArch = null;
					try {
						osName = System.getProperty("os.name").trim(); //$NON-NLS-1$
						javaVersion = System.getProperty("java.version").trim(); //$NON-NLS-1$
						javaArch = System.getProperty("sun.arch.data.model").trim(); //$NON-NLS-1$
					} catch (Throwable e) {
						Logger.getLogger("es.gob.afirma").severe("Error al identificar si es necesaria la instalacion del cliente, se indicara que si: "+e); //$NON-NLS-1$ //$NON-NLS-2$
						return true;
					}
					
					// Si ejecutamos el cliente con Windows y tenemos un Java 1.6u12 o superior
					// de 32bits, entonces no es necesario instalar nada
					if(osName.toLowerCase().contains("windows") &&
							javaVersion.compareTo("1.6.0_12") >= 0 &&
							javaArch.trim().equals("32")) {
						return false;
					}
					
					// Si tenemos un Java 6 y un sistema operativo distinto de Windows
					if(javaVersion.compareTo("1.6.0_12") >= 0 && !osName.toLowerCase().contains("windows")) {
						return false;
					}

					return true;
				}
			});
			
		}

		public String installCore(final String build) {

			// Realizamos la instalacion
			final String buildToInstall = (build != null ? build : Installer.BUILD_LITE);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
						public Boolean run() {
							return AfirmaBootLoader.this.installCore(buildToInstall, true);
						}
					});
				}
			});
			return getInstallationDirectory();
		}
		
		/**
		 * Opcionalmente, muestra al usuario un di&aacute;logo informando de que va a instalarse el
		 * cliente Afirma y el acuerdo de licencia. Si se acepta el acuerdo o se indica que no se muestre,
		 * se instala el n&uacute;cleo y se devuelve el resultado de la instalaci&oacute;. Si no se acepta
		 * el acuerdo o se produce alg&uacute;n error se devuelve {@code false}, si finaliz&oacute;
		 * correctamente se devuelve {@code true}.
		 * @param build Construcci&oacute;n del cliente que desea instalarse.
		 * @param solicitConfirm Indica si se debe informar o no de la instalaci&oacute;n al usuario.  
		 * @return Resultado de la operaci&oacute;n.
		 */
		private boolean installCore(final String build, final boolean solicitConfirm) {
			
			try {
			
			// Realizamos la instalacion
			if(!solicitConfirm || installer.prepareInstall(build, AfirmaBootLoader.this)) {
				return installer.installCore(AfirmaBootLoader.this.codeBase, build);
			}
			
			} catch (Exception e) {
				e.printStackTrace();
			}
			
			return false;
		}
		
		public String installDependencies(final String build) {
			return installDependencies(build, null, null);
		}
		
		public String installDependencies(final String build, final String jsMethodName, final Object jsMethodParams) {

			// Realizamos la instalacion
			final String buildToInstall = (build != null ? build : Installer.BUILD_LITE);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
						public Void run() {
							AfirmaBootLoader.this.installDependencies(buildToInstall, true);
							
							if (jsMethodName != null && !"".equals(jsMethodName)) {
								
								Object[] jsPreparedParams = AOBootUtil.prepareJSParams(jsMethodParams);
								try {
									netscape.javascript.JSObject.getWindow(AfirmaBootLoader.this).call(jsMethodName, jsPreparedParams);
								}
								catch(final Throwable e) {
									Logger.getLogger("es.gob.afirma").severe(
											"No se ha podido realizar la llamada al metodo JavaScript '" + jsMethodName + "': " + e
									);
									JOptionPane.showMessageDialog(
											AfirmaBootLoader.this, 
											"Por favor, reinicie su navegador Web.",
											"Informaci\u00F3n",
											JOptionPane.WARNING_MESSAGE
									);
								}
							}
							
							return null;
						}
					});
				}
			});
			return getInstallationDirectory();
		}
		
		/**
		 * Opcionalmente, muestra al usuario un di&aacute;logo informando de que va a instalarse el
		 * cliente Afirma y el acuerdo de licencia. Si se acepta el acuerdo o se indica que no se muestre,
		 * se instalan las dependencias del cliente y se devuelve el resultado de la instalaci&oacute;.
		 * Si no se acepta el acuerdo o se produce alg&uacute;n error se devuelve {@code false}, si
		 * finaliz&oacute; correctamente se devuelve {@code true}.
		 * @param build Construcci&oacute;n del cliente que desea instalarse.
		 * @param solicitConfirm Indica si se debe informar o no al usuario sobre la instalaci&oacute;n.  
		 * @return Resultado de la operaci&oacute;n.
		 */
		private boolean installDependencies(final String build, final boolean solicitConfirm) {

			// Realizamos la instalacion
			if(!solicitConfirm || installer.prepareInstall(build, AfirmaBootLoader.this)) {
				return installer.installDependencies(AfirmaBootLoader.this.codeBase, build);
			}
			
			return false;
		}
}
