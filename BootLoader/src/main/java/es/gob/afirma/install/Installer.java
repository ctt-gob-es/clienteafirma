/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009-2011 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.install;

import java.awt.Component;
import java.io.File;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.misc.LicenceDialogPanel;
import es.gob.afirma.misc.Platform;

/**
 * Instalador del cliente AFirma.
 * @version 2.0
 */
final class Installer {
	
	/** Directorio de instalaci&oacute;n. */
	public final static String INSTALL_DIR = ".cafirma"; //$NON-NLS-1$
	
	/** Construcci&oacute;n LITE del cliente Afirma. */
	public static final String LITE = "LITE"; //$NON-NLS-1$
	
	/**
	 * Distintos sistemas operativos entre los que distingue el instalador del cliente.
	 */
	enum OS {
		WINDOWS,	// Sistemas operativos Windows
		LINUX,		// Sistemas operativos Linux
		SOLARIS,	// Sistemas operativos Solaris
		MACINTOSH,	// Sistemas operativos Mac OS X
		OTHER		// Otro distinto a los anteriores
	}

	/** Componente padre sobre el que se situar&aacute;n los mensajes del instalador. */
	private Component parentComponent = null;

	/** Componente para la comprobaci&oacute;n e instalaci&oacute;n de las dependencias
	 * de entorno del Cliente. */
	private final CheckAndInstallMissingParts enviromentInstaller;

	/**
	 * Crea el objeto instalador tomando un componente padre como referencia para mostrar los mensajes
	 * modales y unos par&aacute;metros de instalaci&oacute;n.
	 * @param parentComponent Componente padre, puede ser nulo.
	 * @param params Parametros de instalaci&oacute;n.
	 * @exception NullPointerException Si no se han indicado los par&aacute;metros de la instalaci&oacute;n.
	 */
	Installer(final Component parentComponent, final URL codeBase, final String build) {
		
		// Establecemos el componente padre sobre el que se mostraran los dialogos del instalador
		// (puede ser nulo).
		this.parentComponent = parentComponent;

		// Manejador para la comprobacion e instalacion de las dependencias del cliente
		// propias del entorno (Manejadores de repositorios, bibliotecas de terceros,...)
		this.enviromentInstaller = new CheckAndInstallMissingParts(
			Platform.getOS(),
			Platform.getJavaVersion(),
			build,
			codeBase
		);

	}

	/**
	 * Desinstala el cliente de firma al completo.
	 * @return Devuelve <code>true</code> si la desinstalaci&oacute;n finaliz&oacute; correctamente,
	 * <code>false</code> en caso contrario.
	 */
	boolean uninstall() {
		final File afirmaDir = new File(Platform.getUserHome() + File.separator + Installer.INSTALL_DIR);		
		
		return AccessController.doPrivileged(new PrivilegedAction<Boolean>() {
			public Boolean run() {
				if (!afirmaDir.exists()) {
					Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
						"El directorio de instalacion no existe, se omitira la operacion" //$NON-NLS-1$
					);
					return true;
				}
				try {
					fileDelete(afirmaDir);
					JOptionPane.showMessageDialog(
						Installer.this.parentComponent,
						Messages.getString("Installer.11"), //$NON-NLS-1$
						Messages.getString("Installer.12"), //$NON-NLS-1$
						JOptionPane.INFORMATION_MESSAGE
					);
					return true;
				}
				catch(final Throwable e) {
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se ha podido eliminar el directorio de instalacion: " + e //$NON-NLS-1$
					);
					JOptionPane.showMessageDialog(
						Installer.this.parentComponent,
						Messages.getString("Installer.13"), //$NON-NLS-1$
						Messages.getString("Installer.12"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					return false;
				}
			}
			private void fileDelete(final File srcFile) throws Throwable {
		        if (srcFile.isDirectory()) {
		    		for (File f : srcFile.listFiles()) {
		    			fileDelete(f);
		    		}
		            srcFile.delete();
		        } 
		        else srcFile.delete();
			}	
		});
		
	}

	/**
	 * Informa al usuario que se va a proceder a instalar el cliente Afirma y, a continuaci&oacute;n,
	 * muestra al usuario el acuerdo de licencia. La funci&oacute;n devuelve {@code true} o {@code false}
	 * seg&uacute;n se acepte o no el di&aacute;logo.
	 * @param build Construcci&oacute;n que se de desea instalar.
	 * @param parentComponent Componente padre sobre el que se muestran los di&aacute;logos.
	 * @return Indica si se ha aceptado o no el acuerdo de licencia.
	 */
	private boolean prepareInstall() {
		JOptionPane.showMessageDialog(parentComponent, Messages.getString("Installer.0"), Messages.getString("Installer.1"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
		final boolean accepted = new LicenceDialogPanel(parentComponent).showDisclaimer(); 
		if (accepted) {
			Logger.getLogger("es.gob.afirma").info("Se ha aceptado el acuerdo de licencia"); //$NON-NLS-1$ //$NON-NLS-2$
		} 
		else {
			Logger.getLogger("es.gob.afirma").info("No se ha aceptado el acuerdo de licencia, no se instalara el Cliente"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return accepted;
	}
	
	void install() {
		
		boolean allOK = true;
		boolean licenciaMostrada = false;
		
		try {
			if (enviromentInstaller.isEndorsedApacheXMLSecNeeded()) {
				if (!prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando Apache XML Security..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installEndorsedApacheXMLSec();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando Apache XML Security, la ejecucion sobre Java 7 puede fallar: " + e //$NON-NLS-1$
			);
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isEndorsedJava5AFirmaDependenciesNeeded()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando dependencias de @firma para Java 5..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installEndorsedJava5AFirmaDependencies();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando las dependencias para Java 5, la ejecucion sobre Java 5 puede fallar: " + e //$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isEndorsedXalanNeeded()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando Apache XALAN..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installEndorsedXalan();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando Apache Xalan, la ejecucion sobre Java 5 puede fallar: " + e//$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isNSSNeeded()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando NSS..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installNSS();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando NSS, la ejecucion sobre Firefox puede fallar: " + e //$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isNSSConfigurationNeeded()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				enviromentInstaller.configureNSS(parentComponent);
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error configurando NSS, la ejecucion sobre Firefox puede fallar: " + e //$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			JOptionPane.showMessageDialog(
				Installer.this.parentComponent,
				Messages.getString(Messages.getString("Installer.18")), //$NON-NLS-1$
				Messages.getString("Installer.26"), //$NON-NLS-1$
				JOptionPane.WARNING_MESSAGE
			);
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isSunMSCAPINeeded()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando SunMSCAPI..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installSunMSCAPI();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando SunMSCAPI, la ejecucion sobre Java 64 bits o Java 5 puede fallar: " + e //$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			allOK = false;
		}
		
		try {
			if (enviromentInstaller.isSunPKCS11Needed()) {
				if (!licenciaMostrada && !prepareInstall()) return;
				licenciaMostrada = true;
				Logger.getLogger("es.gob.afirma").info("Instalando SunPKCS11..."); //$NON-NLS-1$ //$NON-NLS-2$
				enviromentInstaller.installSunPKCS11();
			}
		}
		catch(final Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error instalando SunPKCS11, la ejecucion sobre Java 64 bits puede fallar: " + e //$NON-NLS-1$
			);
			if (AfirmaBootLoader.DEBUG) e.printStackTrace();
			allOK = false;
		}
		
		if (licenciaMostrada && allOK) JOptionPane.showMessageDialog(
			parentComponent, 
			Messages.getString("Installer.23"), //$NON-NLS-1$
			Messages.getString("Installer.24"), //$NON-NLS-1$
			JOptionPane.INFORMATION_MESSAGE
		);
		else if (licenciaMostrada) JOptionPane.showMessageDialog(
			parentComponent, 
			Messages.getString("Installer.25"),  //$NON-NLS-1$
			Messages.getString("Installer.26"), //$NON-NLS-1$
			JOptionPane.ERROR_MESSAGE
		);
		
	}

}
