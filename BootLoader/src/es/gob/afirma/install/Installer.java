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

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JOptionPane;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOBootUtil;
import es.gob.afirma.misc.AOInstallParameters;
import es.gob.afirma.misc.LicenceDialogPanel;

/**
 * Instalador del cliente AFirma.
 * @version 2.0
 */
final class Installer {

    /** Construcci&oacute;n LITE del cliente Afirma. */
    static final String BUILD_LITE = "LITE"; //$NON-NLS-1$

    /** Construcci&oacute;n MEDIA del cliente Afirma. Incluye la construcci&oacute;n LITE. */
    static final String BUILD_MEDIA = "MEDIA"; //$NON-NLS-1$

    /** Construcci&oacute;n COMPLETA del cliente Afirma. Incluye la construcci&oacute;n MEDIA. */
    static final String BUILD_COMPLETA = "COMPLETA"; //$NON-NLS-1$

    /** Nombre del directorio dentro de la carpeta de usuario en donde se instalaban las versiones 2.4
     * y anteriores del cliente. */
    private final static String OLD_INSTALLATION_DIR = ".clienteFirmaArrobaFirma5"; //$NON-NLS-1$ 

    /** JAR para con el n&uacute;cleo del cliente. */
    private static final String AFIRMA_CORE_JAR = "afirma5_coreV3.jar"; //$NON-NLS-1$
    
    private static final String PACK200_SUFIX = ".pack.gz";
    
    /** JAR con las clases de compatibilidad del cliente con Java 5 (S&oacute;lo Java 5). */
    private static final String AFIRMA_JAVA5_JAR = "afirma_5_java_5.jar"; //$NON-NLS-1$
    
    /** Archivo zip con las librerias Xalan necesarias para la firma XML en JDK 5. */
    private static final String XALAN_LIBRARY_ZIP = "xalan.zip"; //$NON-NLS-1$
    
    /** Nombre del fichero con los datos de version del cliente. */
    private static final String VERSION_PROPERTIES = "version.properties"; //$NON-NLS-1$
    
    /** Nombre del sistema operativo en donde se realiza la ejecuci&oacute;n. */
    private static String osName = null;

    /** Versi&oacute;n de java que se est&aacute; utilizando. */ 
    private static String javaVersion = null;

    /** Componente padre sobre el que se situar&aacute;n los mensajes del instalador. */
    private Component parentComponent = null;

    ///** Directorio de instalaci&oacute;n (s&oacute;lo el nombre, no el path). */
    //private String installDir = null;

    /** Acci&oacute;n a tomar con respecto a las versiones antiguas del cliente instaladas. */ 
    private int oldVersionsAction = AOInstallParameters.ACTION_ASK;

    /**
     * Crea el objeto instalador tomando un componente padre como referencia para mostrar los mensajes
     * modales y unos par&aacute;metros de instalaci&oacute;n.
     * @param parentComponent Componente padre, puede ser nulo.
     * @param params Parametros de instalaci&oacute;n.
     * @exception NullPointerException Si no se han indicado los par&aacute;metros de la instalaci&oacute;n.
     */
    Installer(final Component parentComponent, final AOInstallParameters params) {

        if(params == null) {
            throw new NullPointerException("Los parametros de instalacion no pueden ser nulos"); //$NON-NLS-1$
        }

        // Tomamos la accion a realizar con las versiones antiguas del cliente
        this.oldVersionsAction = params.oldVersionsAction;

        // Establecemos el componente padre sobre el que se mostraran los dialogos del instalador
        // (puede ser nulo).
        this.parentComponent = parentComponent;
    }

    /**
     * Indica si las dependencias requeridas por la construccion concreta estan ya instaladas en el sistema.
     * @param build Construcci&oacute;n cuya instalaci&oacute;n se quiere comprobar.
     * @return Devuelve {@code true} si las dependenicas est&aacute;n instaladas,
     * {@code false} en caso contrario.
     */
    boolean isDependenciesInstalled(final String build) {

        if(build == null) {
            Logger.getLogger("es.gob.afirma").warning("No se ha indicado una construccion"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        boolean installOk;
        if(build.equalsIgnoreCase(Installer.BUILD_LITE)) {
            installOk = this.isBuildLiteDependenciesInstalled();
        } 
        else if(build.equalsIgnoreCase(Installer.BUILD_MEDIA)) {
            installOk = this.isBuildMediaDependenciesInstalled();
        } 
        else if(build.equalsIgnoreCase(Installer.BUILD_COMPLETA)) {
            installOk = this.isBuildCompletaDependenciesInstalled();
        } 
        else {
            Logger.getLogger("es.gob.afirma").warning("La construccion introducida no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
            installOk = false;
        }
        return installOk;
    }

    /**
     * Indica si hay instalada una construcci&oacute;n igual o superior a la indicada del cliente AFirma.
     * Para ello hace una comprobaci&oacute;n de todos los ficheros requeridos por las construcciones
     * inferiores y los requeridos por la propia construcci&oacute;n. Estos ficheros podr&aacute;n variar
     * seg&uacute;n la versi&oacute;n de Java y del sistema operativo. En caso de faltar alguno, se
     * considera que la instalaci&oacute;n es no v&aacute;lida.
     * @param build Construcci&oacute;n cuya instalaci&oacute;n se quiere comprobar.
     * @return Devuelve <code>true</code> si la construcci&oacute;n est&aacute; ya instalada,
     * <code>false</code> en caso contrario
     */
    boolean isAlreadyInstalled(final String build) {

        if(build == null) {
            Logger.getLogger("es.gob.afirma").warning("No se ha indicado una construccion"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        // Comprobamos que el nucleo del cliente este correctamente instalado
        if(!isCoreInstalled(build)) return false;

        boolean installOk;
        if(build.equalsIgnoreCase(Installer.BUILD_LITE)) {
            installOk = this.isBuildLiteDependenciesInstalled();
        } 
        else if(build.equalsIgnoreCase(Installer.BUILD_MEDIA)) {
            installOk = this.isBuildMediaDependenciesInstalled();
        } 
        else if(build.equalsIgnoreCase(Installer.BUILD_COMPLETA)) {
            installOk = this.isBuildCompletaDependenciesInstalled();
        } 
        else {
            Logger.getLogger("es.gob.afirma").warning("La construccion introducida no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
            installOk = false;
        }
        return installOk;
    }
    
    /**
     * Comprueba que est&eacute;n instaladas las dependencias exigidas por la contrucci&oacute;n
     * LITE del cliente.
     * @return Devuelve <code>true</code> si las dependencias est&aacute;n instaladas,
     * <code>false</code> en caso contrario.
     */
    private boolean isBuildLiteDependenciesInstalled() {

        // Comprobamos que esten los requisitos de Java 5
        if (isJava5() && !isJava5DependenciesInstalled(BUILD_LITE)) return false;

        return true;
    }

    /**
     * Comprueba que est&eacute;n instaladas las dependencias exigidas por la contrucci&oacute;n
     * MEDIA del cliente.
     * @return Devuelve <code>true</code> si las dependencias est&aacute;n instaladas,
     * <code>false</code> en caso contrario.
     */
    private boolean isBuildMediaDependenciesInstalled() {

        // Comprobamos la instalacion de la dependencias de la construccion anterior
        if(!isBuildLiteDependenciesInstalled()) return false;

        // Comprobacion del paquete de compatibilidad con Java 5 para las firmas XML
        if (isJava5() && !isJava5DependenciesInstalled(BUILD_MEDIA)) return false;

        return true;
    }

    /**
     * Comprueba que est&eacute;n instaladas las dependencias exigidas por la contrucci&oacute;n
     * COMPLETA del cliente.
     * @return Devuelve <code>true</code> si las dependencias est&aacute;n instaladas,
     * <code>false</code> en caso contrario.
     */
    private boolean isBuildCompletaDependenciesInstalled() {

        // Comprobamos la instalacion de la dependencias de la construccion anterior
        if(!isBuildMediaDependenciesInstalled()) return false;

        return true;    
    }

    /**
     * Comprueba que el n&uacute;cleo de la aplciaci&oacute;n est&eacute; correctamente instalado.
     * @param build Construcci&oacute;n m&iacute;nima exigida del n&uacute;clero del cliente.
     * @return Devuelve <code>true</code> si est&aacute; instalado correctamente,
     * <code>false</code> en caso contrario.
     */
    private boolean isCoreInstalled(final String build) {

        // Comprobamos que exista el fichero
        final String file = AOInstallParameters.getHomeApplication() + File.separator + AFIRMA_CORE_JAR;
        if (!AOBootUtil.existFile(file)) {
            Logger.getLogger("es.gob.afirma").warning("No existe el fichero " + file); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        
        final String buildVersion = AOBootUtil.getBuildVersion(file);
        if(buildVersion == null) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo recuperar la version del nucleo del cliente, se considera no valida su instalacion"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        
        // Comparamos si el valor de la construccion instalada es mayor o igual que el de la
        // construccion exigida
        return getBuildValue(buildVersion) >= getBuildValue(build);
    }

    /**
     * Devuelve el valor asociado a una construcci&oacute;n cuanto m&aacute;s completa sea mayor
     * ser&aacute; su valor. Si se introduce un indicador de construcci&oacute;n no v&aacute;lido
     * se devolver&aacute; -1. 
     * @param build Identificador de la construcci&oacute;n.
     * @return Valor de la construcci&oacute;n.
     */
    private int getBuildValue(final String build) {
        if(build == null) return -1;
        if(build.equalsIgnoreCase(BUILD_LITE)) return 1;
        if(build.equalsIgnoreCase(BUILD_MEDIA)) return 2;
        if(build.equalsIgnoreCase(BUILD_COMPLETA)) return 3;
        return -1;
    }

    /**
     * Comprueba que las dependencias para la compatibilidad con Java 5 est&eacute;n instaladas.
     * @param build Construcci&oacute;n para la que hay que instalar las dependencias.
     * @return Devuelve <code>true</code> si est&aacute;n instaladas correctamente,
     * <code>false</code> en caso contrario.
     */
    private boolean isJava5DependenciesInstalled(String build) {

    	// Si se va a usar la construccion MEDIA o superior son necesarios los modulos para firmas XML
    	if(getBuildValue(build) >= getBuildValue(BUILD_MEDIA)) {
    		// Comprobamos que exista el fichero
    		final String file = Installer.getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR;
    		if (!AOBootUtil.existFile(file)) {
    			Logger.getLogger("es.gob.afirma").warning("No existe el fichero " + file); //$NON-NLS-1$ //$NON-NLS-2$
    			return false;
    		}

    		if(!isXalanInstalled()) return false;
    	}
    	
    	// En Java 5 siempre sera necesaria la instalacion de SunMSCAPI
    	if(!isMsCapiLibsInstalled()) return false;
    	
    	return true;
    }
    
    /**
     * Comprueba que est&eacute;n instaladas todas las dependencias necesarias para el uso
     * del almac&eacute;n de certificados de Windows.
     * @return Devuelve <code>true</code> si las dependencias est&aacute;n instaladas,
     * <code>false</code> en caso contrario.
     */
    private boolean isMsCapiLibsInstalled() {

        // Ficheros que hay que comprobar
        final Vector<String> libsNames = new Vector<String>();
        final String javaHome = this.getJavaHome();
        if(javaHome == null) {
            return false;
        }

        if(isCurrentOS(OS.WINDOWS)) {
            // Comprobamos "msvcr71.dll", para lo que deberemos mirar en el registro el directorio
            // con las librerias de sistema. Para esto sera necesario disponer de la DLL de deploy.
            // La biblioteca "msvcr71.dll" es una dependencia necesaria en para la version de
            // "sunmscapi.dll" distribuida.
            libsNames.add(javaHome + "lib" + File.separator + "ext" + File.separator + "sunmscapi.jar"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            //libsNames.add(javaHome + "bin" + File.separator + "msvcr71.dll"); //$NON-NLS-1$ //$NON-NLS-2$
            libsNames.add(javaHome + "bin" + File.separator + "sunmscapi.dll"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Comprobamos que existan todos los ficheros
        for(final String lib : libsNames) {
            if (!AOBootUtil.existFile(lib)) {
                Logger.getLogger("es.gob.afirma").warning("No existe el fichero " + lib); //$NON-NLS-1$ //$NON-NLS-2$
                return false;
            }
        }

        return true;
    }

    /**
     * Actualiza el n&uacute;cleo del cliente de firma y las dependencias nativas directas.
     * @param installFilesCodeBase URL base desde la que obtener los ficheros necesarios para
     * la actualizaci&oacute;n.
     * @param build Construcci&oacute;n que se desea actualizar.
     * @return Devuelve <code>true</code> si el cliente se actualiz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    boolean update(final URL installFilesCodeBase, final String build) {

    	// Obtenemos la version del cliente publicada
    	final Properties remoteVersion = getRemoteVersionProperties(installFilesCodeBase);
    	
    	if(isUpdated(installFilesCodeBase, remoteVersion)) {
    		JOptionPane.showMessageDialog(parentComponent, Messages.getString("Installer.4"), Messages.getString("Installer.2"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
    		return true;
    	}
    	
    	// Informamos de que vamos a actualizar el cliente para que el usuario sea consciente del motivo
    	// de la demora
    	JOptionPane.showMessageDialog(parentComponent,
    			Messages.getString("Installer.15")+ " " +  //$NON-NLS-1$//$NON-NLS-2$
    				remoteVersion.getProperty("version.mayor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
    				remoteVersion.getProperty("version.minor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
    				remoteVersion.getProperty("version.build")+ //$NON-NLS-1$
    				".", //$NON-NLS-1$
        		Messages.getString("Installer.2"), //$NON-NLS-1$
        		JOptionPane.INFORMATION_MESSAGE);
    	
    	 boolean updateOK = true;
    	 
    	 // Actualizamos los nucleos del cliente
    	 if( build != null && (build.equalsIgnoreCase(Installer.BUILD_LITE) ||
    			 build.equalsIgnoreCase(Installer.BUILD_MEDIA) ||
    			 build.equalsIgnoreCase(Installer.BUILD_COMPLETA))) {
    		 try {
    			 updateOK = installCore(installFilesCodeBase, build);
    		 } 
    		 catch(final SecurityException e) {
    			 Logger.getLogger("es.gob.afirma").severe("No dispone de los permisos necesarios para realizar esta operacion: "+e); //$NON-NLS-1$ //$NON-NLS-2$
    			 updateOK = false;
    		 } 
    		 catch(final Throwable e) {
    			 Logger.getLogger("es.gob.afirma").severe("Ocurrio un error grave durante la actualizacion: "+e); //$NON-NLS-1$ //$NON-NLS-2$
    			 updateOK = false;
    		 }
    	 }
    	 else {
             Logger.getLogger("es.gob.afirma").severe("La construccion indicada no esta reconocida, no se actualizara el cliente"); //$NON-NLS-1$ //$NON-NLS-2$
             updateOK = false;
    	 }
    	 
    	 return updateOK;
    }
    
    /**
     * Procesa las versiones antiguas del cliente que pueda haber instaladas en el sistema. Esto es,
     * en caso de localizar alguna, actual seg&uacute;n la acci&oacute;n que se indicase: borrar,
     * respetar o preguntar al usuario.
     * @param oldVersionAction Acci&oacute;n a realizar.
     */
    private void processOldVersions(int oldVersionAction) {
    	
        // Si existen versiones antiguas del cliente las borramos, le preguntamos al usuario o
        // la respetamos (no hacemos nada) segun se nos indicase al cargar el instalador.
        if(existsOldVersions()) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                "Se han detectado versiones antiguas del cliente @firma" //$NON-NLS-1$
            );
            if (oldVersionAction == AOInstallParameters.ACTION_DELETE) {
            	if (uninstallOldVersion(getOldInstallationDir())) {
            		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            				"Se han eliminado las versiones antiguas del cliente" //$NON-NLS-1$
            		);
            	} 
            	else {
            		Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            				"No se pudieron eliminar por completo las versiones antiguas del cliente" //$NON-NLS-1$
            		);
            	}
            } else if (oldVersionAction == AOInstallParameters.ACTION_ASK) {
                int result = JOptionPane.showConfirmDialog(
                    Installer.this.parentComponent,
                    Messages.getString("Installer.8"), //$NON-NLS-1$
                    Messages.getString("Installer.1"), //$NON-NLS-1$
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.QUESTION_MESSAGE
                );
                if(result == JOptionPane.YES_OPTION) {
                    if(!uninstallOldVersion(getOldInstallationDir())) {
                        Logger.getLogger("es.gob.afirma").warning("No se pudieron eliminar por completo las versiones antiguas del cliente."); //$NON-NLS-1$ //$NON-NLS-2$
                        this.showErrorMessage(
                            Messages.getString("Installer.9") //$NON-NLS-1$
                        );
                    }
                }
            } 
            else if(oldVersionAction != AOInstallParameters.ACTION_RESPECT) {
                Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                    "La accion elegida sobre las versiones antiguas del cliente no es valida, se respetaran las versiones ya instaladas" //$NON-NLS-1$
                );
            }
        }
    }

    boolean uninstallOldVersion(final String oldVersionDir) {
    	/*
    	 * Las bibliotecas antiguas del Cliente Afirma pueden provocar errores en el acceso
    	 * al almacen interno de los nuevos Mozilla
    	 */
    	if (isCurrentOS(OS.WINDOWS)) {
    		String systemRootDir = AOBootUtil.getSystemRoot();
    		if (systemRootDir != null) {
    			String system32Dir = systemRootDir + File.separator + "system32";
    			try { new File(system32Dir, "nss3.dll").delete(); 		} catch (Exception e) {}
    			try { new File(system32Dir, "softokn3.dll").delete(); } catch (Exception e) {}
    			try { new File(system32Dir, "libnspr4.dll").delete(); } catch (Exception e) {}
    			try { new File(system32Dir, "libplc4.dll").delete(); 	} catch (Exception e) {}
    			try { new File(system32Dir, "libplds4.dll").delete(); } catch (Exception e) {}
    			try { new File(system32Dir, "nssckbi.dll").delete(); 	} catch (Exception e) {}
    			try { new File(system32Dir, "smime3.dll").delete(); 	} catch (Exception e) {}
    			try { new File(system32Dir, "ssl3.dll").delete();			} catch (Exception e) {}
    			try { new File(system32Dir, "jss3.dll").delete();			} catch (Exception e) {}
    			try { new File(system32Dir, "Afirma5Library2d4.dll").delete(); } catch (Exception e) {}
    		}
    	}
    	
    	return AOBootUtil.deleteDir(new File(getOldInstallationDir()));
    }
    
    /**
     * Indica si existe el directorio en donde se instalan las versiones 2.4 y anteriores del cliente.
     * @return Devuelve <code>true</code> si existe el directorio antiguo de instalaci&oacute;n,
     * <code>false</code> en caso contrario. 
     */
    private boolean existsOldVersions() {
        File oldInstallationDir = new File(this.getOldInstallationDir());
        return oldInstallationDir.exists() && oldInstallationDir.isDirectory();
    }

    /**
     * Recupera la ruta de con las instalaciones de las antiguas versiones del cliente de firma
     * (versiones 2.4 y anteriores).
     * @return Ruta del directorio con las instalaciones.
     */
    private String getOldInstallationDir() {
        return this.getUserHome() + OLD_INSTALLATION_DIR;
    }

    /**
     * Comprueba que est&eacute;n disponibles para su instalaci&oacute;n el plugin de firmas XML y 
     * sus posibles dependencias.
     * @param codeBase URL base en la que buscar el plugin.
     * @return Devuelve <code>true</code> si est&aacute; disponible el plugin, <code>false</code> en
     * caso contrario.
     */
    private boolean isXMLModuleDependeciesAvailables(URL codeBase) {

        boolean dependenciesCoverted = true;
        if(isJava5()) {
            dependenciesCoverted = AOBootUtil.existRemoteFile(codeBase, XALAN_LIBRARY_ZIP);
        }
        
        return dependenciesCoverted;
    }

    /**
     * Instala el plugin XML y sus posibles dependecias.
     * @param codeBase Ruta base de los ficheros necesarios para la instalaci&oacute;n.
     * @return Devuelve <code>true</code> si se instala correctamente el plugin o este ya estaba
     * instalado, <code>false</code> en caso contrario.
     */
    private boolean installXMLModuleDependencies(URL codeBase) {

        boolean installOK = true;

        // Si se requieren dependencias, se comprueba si existen y se instalan si procede
        if(isJava5() && !isXalanInstalled()) {
            installOK = this.installXalan(codeBase);
        }

        return installOK;
    }

    /**
     * Comprueba que los paquetes XALAN est&eacute;n instalados en el directorio endorsed del JRE
     * configurado.
     * @return Devuelve <code>true</code> si XALAN est&aacute; instalado, <code>false</code>
     * en caso contrario.
     */
    private boolean isXalanInstalled() {

        boolean installOK = true;
        String endorsedDir = getEndorsedDir();
        if(endorsedDir == null) {
            installOK = false;
        }
        else {
            installOK = new File(endorsedDir + File.separator + "serializer.jar").exists() || //$NON-NLS-1$
            new File(endorsedDir + File.separator + "xalan.jar").exists() || //$NON-NLS-1$
            new File(endorsedDir + File.separator + "xercesImpl.jar").exists() || //$NON-NLS-1$
            new File(endorsedDir + File.separator + "xml-apis.jar").exists(); //$NON-NLS-1$
        }

        return installOK;
    }

    /**
     * Comprueba que el paquete de compatibilidad con Java 5 est&eacute; instalado en el directorio
     * endorsed del JRE configurado.
     * @return Devuelve {@code true} si el paquete de compatibilidad est&aacute; instalado, {@code false}
     * en caso contrario.
     */
    private boolean isCompatibilityPackInstalled() {
    	boolean installOK = true;
      String endorsedDir = getEndorsedDir();
      if(endorsedDir == null) {
          installOK = false;
      }
      else {
          installOK = new File(endorsedDir + File.separator + "afirma_5_java_5.jar").exists(); //$NON-NLS-1$
      }

      return installOK;
    }
    
    /** Directorio endorsed de la distribuci&oacute;n Java. */
    static String endorsedDir = null;

    /**
     * Recupera el directorio "endorsed" de la JRE usada para la instalaci&oacute;n o <code>null</code>
     * si no se pudo determinar ning&uacute;n directorio.
     * @return Directorio "endorsed".
     */
    static String getEndorsedDir() {

        if(endorsedDir != null) {
            return endorsedDir; 
        }

        String endorsedDirsString = System.getProperty("java.endorsed.dirs"); //$NON-NLS-1$
        String[] endorsedDirs;
        if(endorsedDirsString != null && (endorsedDirs = endorsedDirsString.split(System.getProperty("path.separator"))).length > 0) { //$NON-NLS-1$
            endorsedDir = endorsedDirs[0];
        }

        // Si no se ha asignado, acudimos al directorio por defecto ($JAVA_HOME/lib/endorsed)
        if(endorsedDir == null) {

            // Recuperamos el directorio de Java
            String javaHome = null;
            try {
                javaHome = AOBootUtil.getJavaHome();
            } catch (Throwable e) {
                Logger.getLogger("es.gob.afirma").severe("No se tienen permisos para localizar y leer " + //$NON-NLS-1$ //$NON-NLS-2$
                        "el directorio de Java, conceda los permisos necesarios o actualice a Java 6 o " + //$NON-NLS-1$
                        "superior: "+e); //$NON-NLS-1$
                JOptionPane.showMessageDialog(null, Messages.getString("Installer.16"), Messages.getString("Installer.10"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$//$NON-NLS-2$
            }
            if(javaHome != null)
                endorsedDir = javaHome + File.separator + "lib" + File.separator + "endorsed"; //$NON-NLS-1$ //$NON-NLS-2$
        }

        return endorsedDir;
    }

    /**
     * Comprueba que un fichero exista y sea accesible en el directorio de instalaci&oacute;n
     * del cliente.
     * @param jarFilename Nombre del fichero.
     * @return Devuelve <code>true</code> si el fichero existe y es accesible, <code>false</code>
     * en caso contrario. 
     */
    static boolean isJarInstalled(final String jarFilename) {
        File jarFile = new File(AOInstallParameters.getHomeApplication() + File.separator + jarFilename);
        return jarFile.exists() && jarFile.isFile() && jarFile.canRead();
    }
    
    /**
     * Recupera un fichero fichero ZIP situado en la ruta especificada.
     * @param codeBase Ruta al directorio en donde se encuentra el fichero.
     * @param filename Path del fichero.
     * @return Fichero Zip.
     * @throws Exception No se pudo acceder al Zip u ocurri&oacute; un error al crear una
     * copia temporal en local.
     */
    private static ZipFile getZipFile(final URL codeBase, final String filename) throws Exception {
        ZipFile zipFile = null;
        if(codeBase == null) {
            zipFile = new ZipFile(filename);
        } 
        else {
            zipFile = new ZipFile(createLocalTempFile(codeBase, filename));
        }		
        return zipFile;
    }

    /**
     * Crea una copia de un fichero en el disco local para poder procesarlo.
     * @param path URL en donde se encuentra el fichero.
     * @param filename Nombre del fichero que deseamos copiar en local.
     * @return Fichero en local.
     */
    private static File createLocalTempFile(final URL path, final String filename) throws AOException {

        File tmpFile = null;
        InputStream is = null;
        FileOutputStream fos = null;
        try {
            String pathString = path.toString();
            if(pathString.endsWith("/") || pathString.endsWith("\\")) pathString = pathString.substring(0, pathString.length()-1); //$NON-NLS-1$ //$NON-NLS-2$
            tmpFile = File.createTempFile("afirmaTmp_", filename); //$NON-NLS-1$

            URI uri = AOBootUtil.createURI(pathString+"/"+filename); //$NON-NLS-1$
            if(uri.getScheme().toLowerCase().equals("file")) { //$NON-NLS-1$
                try {
                    is = new FileInputStream(pathString.substring(7)+"/"+filename); //$NON-NLS-1$
                } catch (Exception e) {
                    is = uri.toURL().openStream();
                }
            } else {
                is = uri.toURL().openStream();
            }
            fos = new FileOutputStream(tmpFile);
            int nBytes = -1;
            byte[] buffer = new byte[1024];
            while((nBytes = is.read(buffer)) != -1) {
                fos.write(buffer, 0, nBytes);
            }
        } catch (Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Ocurrio un error durante la generacion de fichero temporal '" //$NON-NLS-1$ //$NON-NLS-2$
                    +filename+"' para la instalacion del cliente"); //$NON-NLS-1$
            throw new AOException("No se ha podido crear el fichero temporal en local: "+e); //$NON-NLS-1$
        } finally {
            if(is != null) try {is.close();} catch (Exception e) {}
            if(fos != null) try {fos.close();} catch(Exception e) {}
        }
        return tmpFile;
    }

    /**
     * Descomprime un fichero Zip en un directorio. Si el directorio de destino no existe,
     * se crear&aacute;. En caso de ocurrir un error durante la descompresi&oacute;n de un
     * fichero concreto, la operaci&oacute;n no se detendr&aacute; y se tratar&aacute; de
     * descomprimir el resto.<br>
     * Si se introduce un directorio nulo o vac&iacute;o se descomprimir&aacute; en el directorio
     * actual.
     * @param zipFile Fichero zip
     * @param destDirectory Ruta del directorio en donde deseamos descomprimir
     * @throws AOException Cuando ocurre cualquier error durante la descompresi&oacute;n de ficheros
     * @throws IOException El nombre de directorio indicado coincide con el de un fichero
     */
    private void unzip(ZipFile zipFile, String destDirectory) throws AOException, IOException {

        if(zipFile == null) throw new NullPointerException("El fichero Zip no puede ser nulo"); //$NON-NLS-1$
        if(destDirectory == null || destDirectory.length() < 0) destDirectory = "."; //$NON-NLS-1$

        // Si no existe el directorio de destino, lo creamos
        File destDir = new File(destDirectory);
        if(!destDir.exists()) destDir.mkdirs();
        else if(destDir.isFile()) throw new IOException("Ya existe un fichero con el nombre indicado para el directorio de salida"); //$NON-NLS-1$

        // Agregamos el separador al final si no estaba
        if(!destDirectory.endsWith(File.separator)) destDirectory += File.separator;

        // Descomprimimos los ficheros
        byte[] buffer = new byte[1024];
        for(Enumeration<? extends ZipEntry> zipEntries = zipFile.entries(); zipEntries.hasMoreElements();) {
            ZipEntry entry = zipEntries.nextElement();
            try {
                // Creamos el arbol de directorios para el fichero
                File outputFile = new File(destDirectory + entry.getName());
                if(!outputFile.getParentFile().exists()) outputFile.getParentFile().mkdirs();

                // Descomprimimos el fichero
                InputStream zeis = zipFile.getInputStream(entry);
                FileOutputStream fos = new FileOutputStream(outputFile);
                int nBytes;
                while((nBytes = zeis.read(buffer)) != -1) fos.write(buffer, 0, nBytes);
                fos.close();
            }
            catch (Exception e) {
                throw new AOException(
                        "Ocurrio un error durante la instalacion, no se pudo instalar la biblioteca '" //$NON-NLS-1$
                        + entry.getName() + "': " + e //$NON-NLS-1$
                );
            }
        }
    }

    /**
     * Comprueba si la instalaci&oacute;n se esta llevando a cabo sobre una plataforma Java 5.
     * @return Devuelve <code>true</code> si es Java 5, <code>false</code> en caso contrario.
     */
    static boolean isJava5() {

        if(javaVersion == null) {
            javaVersion = System.getProperty("java.version"); //$NON-NLS-1$
        }
        return javaVersion.startsWith("1.5"); //$NON-NLS-1$
    }

    /** Sistema operativo en donde se ejecuta la instalaci&oacute;n. */
    private static OS currentOS = null;

    /**
     * Consulta si estamos ejecutando la instalaci&oacute;n en el sistema operativo indicado.
     * @param os Sistema operativo en el que queremos comprobar si se esta ejecutando la instalaci&oacute;n.
     * @return Devuelve <code>true</code> si es ese SO, <code>false</code> en caso contrario.
     */
    private static boolean isCurrentOS(OS os) {
        if(currentOS == null) {
            osName = System.getProperty("os.name"); //$NON-NLS-1$

            // Identificamos el sistema operativo en donde ejecutamos el instalador
            if(osName.contains("inux")) currentOS = OS.LINUX; //$NON-NLS-1$
            else if (osName.startsWith("Mac OS X")) currentOS = OS.MACINTOSH; //$NON-NLS-1$
            else if (osName.contains("olaris") || osName.startsWith("SunOS")) currentOS = OS.SOLARIS; //$NON-NLS-1$ //$NON-NLS-2$
            else if (osName.contains("indows")) currentOS = OS.WINDOWS; //$NON-NLS-1$
            else currentOS = OS.OTHER;
        }

        return currentOS == os;
    }

    /**
     * Distintos sistemas operativos entre los que distingue el instalador del cliente.
     */
    enum OS {
        WINDOWS,	// Sistemas operativos Windows
        LINUX,		// Sistemas operativos Linux
        SOLARIS,	// Sistemas operativos Solaris
        MACINTOSH,	// Sistemas operativos de Applet
        OTHER		// Otro distinto a los anteriores
    }

    /**
     * Instala el n&uacute;cleo del cliente en el directorio de instalaci&oacute;n del cliente. 
     * @param installFilesCodeBase Ruta en donde se encuentra el n&uacute;cleo del cliente.
     * @param build Construcci&oacute;n que se desea instalar.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    boolean installCore(final URL installFilesCodeBase, final String build) {

        // Identificamos si vamos a instalar la version Java 5 o la Java 6 (o superior)
        String javaPref = "j6"; //$NON-NLS-1$
        if(isJava5()) javaPref = "j5"; //$NON-NLS-1$
        
        // Descargamos el nucleo de la aplicacion
        Logger.getLogger("es.gob.afirma").info("Se va a instalar el nucleo del cliente"); //$NON-NLS-1$ //$NON-NLS-2$
        
        Logger.getLogger("es.gob.afirma").info("Se intentara instalar la version PACK200 del nucleo");
        try {
            AOBootUtil.copyRemoteFile(
                AOBootUtil.createURLFile(
            		installFilesCodeBase, 
            		build + "_" + javaPref + "_" + AFIRMA_CORE_JAR + PACK200_SUFIX
        		),
                AOInstallParameters.getHomeApplication(),
                AFIRMA_CORE_JAR + PACK200_SUFIX
            );
            unpack(AOInstallParameters.getHomeApplication() + "/" + AFIRMA_CORE_JAR + PACK200_SUFIX);
        }
        catch(final Throwable e) {
        	Logger.getLogger("es.gob.afirma").warning(
    			"No se ha podido instalar el nucleo en formato PACK200, se intentara en formato JAR: " + e
			);
            try {
                AOBootUtil.copyRemoteFile(
                    AOBootUtil.createURLFile(installFilesCodeBase, build + "_" + javaPref + "_" + AFIRMA_CORE_JAR), //$NON-NLS-1$ //$NON-NLS-2$
                    AOInstallParameters.getHomeApplication(),
                    AFIRMA_CORE_JAR
                );
            }
            catch (final SecurityException ex) {
                throw ex;
            }
            catch (final Throwable ex) {
                Logger.getLogger("es.gob.afirma").severe("No se pudo descargar el nucleo del cliente: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
                return false;
            }
        }
        
        return true;
    }

    /**
     * Instala en el directorio endorsed de la JRE en uso el pack de compatibilidad de firma
     * del cliente para la generacion de firmas XML con Java 5. 
     * @param installFilesCodeBase Ruta en donde se encuentran los ficheros que se deben instalar.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    private boolean installCompatibilityPack(final URL installFilesCodeBase) {

        boolean installOK = true;

        // Recuperamos el directorio de java
        String endorsedDir = getEndorsedDir();
        if(endorsedDir == null) return false;

        Logger.getLogger("es.gob.afirma").info("Se va a instalar el paquete de compatibilidad de firmas XML con Java 5"); //$NON-NLS-1$ //$NON-NLS-2$
        
        Logger.getLogger("es.gob.afirma").info("Se intentara instalar la version PACK200 del paquete de compatibilidad con Java 5");
        try {
        	AOBootUtil.copyRemoteFile(
        			AOBootUtil.createURLFile(
        					installFilesCodeBase, 
        					AfirmaBootLoader.COMPATIBILITY_JAR + PACK200_SUFIX
        			),
        			endorsedDir
        	);
        	unpack(endorsedDir + "/" + AfirmaBootLoader.COMPATIBILITY_JAR + PACK200_SUFIX);
        }
        catch(final Throwable e) {
        	Logger.getLogger("es.gob.afirma").warning(
        			"No se ha podido instalar el paquete de compatibilidad con Java 5 en formato PACK200, se intentara en formato JAR: " + e
        	);
        	//Borramos primeramente al pack de compatibilidad si se en Pack200 si se llego a copiar
        	File compPack200File = new File(endorsedDir + "/" + AfirmaBootLoader.COMPATIBILITY_JAR + PACK200_SUFIX);
        	if(compPack200File.exists()) {
        		try { compPack200File.delete(); } catch (Exception ex) {}
        	}
        	
        	// Compiamos la version sin pack 200
            try {
                AOBootUtil.copyRemoteFile(AOBootUtil.createURLFile(installFilesCodeBase, AfirmaBootLoader.COMPATIBILITY_JAR), endorsedDir);
            }
            catch (final SecurityException ex) {
                throw ex;
            }
            catch(final Throwable ex) {
                Logger.getLogger("es.gob.afirma").warning("Error durante la instalacion del paquete de compatibilidad: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
                installOK = false;
            }
        }

        return installOK;
    }

    /**
     * Instala las librerias "mscapi" para el acceso al repositorio de Windows. 
     * @param installFilesCodeBase Ruta en donde se encuentran los ficheros que se deben instalar.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    private boolean installMSCapi(final URL installFilesCodeBase) {
    	
    	// Si ya estaba instalado no hacemos nada
    	if (isMsCapiLibsInstalled()) return true;

        final String mscapiJarName = "sunmscapi.jar"; //$NON-NLS-1$
        String mscapiZipName = "mscapi.zip"; //$NON-NLS-1$
        // Miramos si es x64 para instalar un SunMSCAPI x64 propio
        if ("x64".equalsIgnoreCase(System.getProperty("os.arch")) &&
        	 "64".equalsIgnoreCase(System.getProperty("sun.arch.data.model"))) {
        	mscapiZipName = "mscapix64.zip";
        }
        // Miramos si es IA64 para instalar un SunMSCAPI x64 propio
        if ("ia64".equalsIgnoreCase(System.getProperty("os.arch")) &&
           	 "64".equalsIgnoreCase(System.getProperty("sun.arch.data.model"))) {
           	mscapiZipName = "mscapiia64.zip";
        }
        boolean installOK = true;

        // Recuperamos el directorio de java
        final String javaHome = getJavaHome();
        if(javaHome == null) return false;
        
        // Descomprimimos las librerias de CAPI de Windows en el directorio habilitado para este
        // objetivo.
        Logger.getLogger("es.gob.afirma").info("Se va a actualizar la libreria de MSCAPI"); //$NON-NLS-1$ //$NON-NLS-2$
        try {
            unzip(getZipFile(installFilesCodeBase, mscapiZipName), javaHome + "bin"); //$NON-NLS-1$
        }
        catch (final SecurityException e) {
            throw e;
        }
        catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("Error durante la instalacion de las bibliotecas nativas de SunMSCAPI: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            installOK = false;
        }

        // Copiamos el JAR de MS CAPI en el directorio de extensiones de Windows.
        try {
        	AOBootUtil.copyRemoteFile(
    			AOBootUtil.createURLFile(installFilesCodeBase, mscapiJarName), 
    			javaHome + "lib" + File.separator + "ext"
			);
        }
        catch(final SecurityException e) {
            throw e;
        }
        catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("Ocurrio un error durante la instalacion de la extension MS CAPI de Java: "+e); //$NON-NLS-1$ //$NON-NLS-2$
            installOK = false;
        }

        // Si usamos un JRE inferior al 6 descomprimimos, si no existe, la libreria 7.1 del Runtime de Microsoft,
        // dependencia que necesita la libreria MS CAPI
        if (isJava5()) {
            final String msvcr71ZipName = "msvcr71.zip"; //$NON-NLS-1$
            if(!(new File(javaHome + "bin" + File.separator + "msvcr71.dll")).exists()) { //$NON-NLS-1$ //$NON-NLS-2$
                try {
                    unzip(getZipFile(installFilesCodeBase, msvcr71ZipName), javaHome + "bin"); //$NON-NLS-1$
                }
                catch(final SecurityException e) {
                    throw e;
                }
                catch(final Throwable e) {
                    Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                        "Error intentando copiar " + msvcr71ZipName + ", se omitira y se continuara con la instalacion: " + e //$NON-NLS-1$ //$NON-NLS-2$
                    );
                    installOK = false;
                }
            }
        }

        return installOK;
    }

    /**
     * Instala las bibliotecas Xalan para la generaci&oacute;n de firmas XML desde Java 5.
     * @param installFilesCodeBase Ruta en donde se encuentra el fichero Zip con la libreria que se desea instalar.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    private boolean installXalan(final URL installFilesCodeBase){

        // Descomprimimos los ficheros de Xalan en el directorio endorsed de Java
        Logger.getLogger("es.gob.afirma").info("Se van a instalar las bibliotecas Xalan para la generacion de firmas XML desde Java 5"); //$NON-NLS-1$ //$NON-NLS-2$
        
        final String tempDirPath = AOInstallParameters.getHomeApplication() + File.separator + "temp_xalan";
        Logger.getLogger("es.gob.afirma").info("Se intentara instalar la version PACK200 de Xalan");
        try {
        	unzip(getZipFile(installFilesCodeBase, XALAN_LIBRARY_ZIP), tempDirPath);
        	// Desempaquetamos los ficheros Pack200 del directorio temporal en el
            // directorio Endorsed de Java
            File tempDir = new File(tempDirPath);
            for(File file : tempDir.listFiles()) {
                String filename = file.getName();
                if(filename.endsWith(".pack.gz")) {
                	unpack(file.getAbsolutePath(), getEndorsedDir() + "/" + filename.substring(0, filename.lastIndexOf(".pack.gz")));
                }
                // Eliminamos el fichero tras desempaquetarlo
                file.delete();
            }
            // Eliminamos el directorio temporal y su contenido
            tempDir.delete();
        }
        catch(final Throwable e) {
        	Logger.getLogger("es.gob.afirma").warning("No se ha podido instalar la version PACK200 de Xalan, se intentara la version JAR: " + e);
            try {
                unzip(getZipFile(installFilesCodeBase, XALAN_LIBRARY_ZIP), getEndorsedDir());
            }
            catch(final SecurityException ex) {
                throw ex;
            }
            catch(final Throwable ex) {
                Logger.getLogger("es.gob.afirma").warning("Error durante la instalacion de las librerias Xalan: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
                return false;
            }
        }
        return true;
    }

    /**
     * Desinstala el cliente de firma al completo.
     * @return Devuelve <code>true</code> si la desinstalaci&oacute;n finaliz&oacute; correctamente,
     * <code>false</code> en caso contrario.
     */
    boolean uninstall() {
    	
        final boolean result = AOBootUtil.deleteDir(new File(AOInstallParameters.getHomeApplication()));
        if(result) {
            JOptionPane.showMessageDialog(
                    Installer.this.parentComponent,
                    Messages.getString("Installer.11"), //$NON-NLS-1$
                    Messages.getString("Installer.12"), //$NON-NLS-1$
                    JOptionPane.INFORMATION_MESSAGE
            );
            // Procesar versiones antiguas del cliente
            this.processOldVersions(this.oldVersionsAction);
        } 
        else {
            JOptionPane.showMessageDialog(
                    Installer.this.parentComponent,
                    Messages.getString("Installer.13"), //$NON-NLS-1$
                    Messages.getString("Installer.12"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
        }
        return result;
    }

    /**
     * Recupera el directorio de instalacion de Java finalizado con la cadena separaci&oacute;n
     * de directorios. Si no puede recuperar el directorio de instalaci&oacute;n de Java, devuelve
     * <code>null</code>.
     * @return Directorio de instalaci&oacute;n de Java.
     */
    private String getJavaHome() {
        String javaHome = null;
        try {
            javaHome = AOBootUtil.getJavaHome();
        } 
        catch(final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("No se tienen permisos para localizar y leer el directorio de Java, conceda los permisos necesarios."); //$NON-NLS-1$ //$NON-NLS-2$
            showErrorMessage(Messages.getString("Installer.14")); //$NON-NLS-1$
        }
        if(javaHome != null && !javaHome.endsWith(File.separator)) {
            javaHome += File.separator;
        }
        return javaHome;
    }

    /**
     * Recupera el directorio de usuario finalizado con la cadena de separaci&oacute;n de directorios.
     * @return Directorio de usuario.
     */
    private String getUserHome() {
        String userHome = null;
        try {
            userHome = System.getProperty("user.home") + File.separator; //$NON-NLS-1$
        } 
        catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("No se tienen permisos para localizar y leer el directorio de usuario, conceda los permisos necesarios."); //$NON-NLS-1$ //$NON-NLS-2$
            showErrorMessage(Messages.getString("Installer.17")); //$NON-NLS-1$
        }
        if(!userHome.endsWith(File.separator)) {
            userHome += File.separator;
        }
        return userHome;
    }

    /**
     * Recupera la versi&oacute;n del cliente de firma. Esta es la versi&oacute;n del n&uacute;cleo
     * del cliente y no tiene que coincidir con la version de los plugins que integre.<br/>
     * El formato de la versi&oacute;n ser&aacute; siempre:<br/>
     * <code>X.Y.Z BUILD</code><br/>
     * En donde <code>X</code>, <code>X</code> y <code>X</code> son la versi&oacute;n, subversi&oacute;n
     * y contrucci&oacute;n del cliente y debe tener uno o m&aacute;s d&iacute;gitos; y 
     * <code>BUILD</code> es la construcci&oacute;n del cliente instalado (LITE, MEDIA o COMPLETA).<br/>
     * Si no se detecta la versi&oacute;n se devuelve la cadena "0.0.0".
     * @return Versi&oacute;n del cliente.
     */
    String getVersion() {
        return AOBootUtil.getVersionFromZip(
    		AOInstallParameters.getHomeApplication() + File.separator + AFIRMA_CORE_JAR
        );
    }
    
    /**
     * Muestra al usuario un di&aacute;logo de error en una ventana modal.
     * @param message Texto de error.
     */
    private void showErrorMessage(final String message) {
        JOptionPane.showMessageDialog(Installer.this.parentComponent, message, Messages.getString("Installer.10"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
    }

    /**
     * Instalamos las dependencias de la construcci&oacute;n LITE del cliente.
     * @param codeBase Ruta en donde se encuentran los ficheros instalables del cliente.
     * @param build Construcc&iacute;n de la que hay que instalar las dependencias.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute;
     * correctamente, <code>false</code> en caso contrario.
     */
    boolean installDependencies(final URL codeBase, final String build) {
    	
    	boolean installOk = false;
    	if(BUILD_LITE.equals(build)) {
    		installOk = installLiteBuildDependencies(codeBase);
    	} else if(BUILD_MEDIA.equals(build)) {
    		installOk = installMediaBuildDependencies(codeBase);
    	} else if(BUILD_COMPLETA.equals(build)) {
    		installOk = installCompletaBuildDependencies(codeBase);
    	}
    	
    	return installOk;
    }
    
    /**
     * Instalamos las dependencias de la construcci&oacute;n LITE del cliente.
     * @param codeBase Ruta en donde se encuentran los ficheros instalables del cliente.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute;
     * correctamente, <code>false</code> en caso contrario.
     */
    private boolean installLiteBuildDependencies(final URL codeBase) {

        boolean installOK = true;

        // En el caso de Windows instalamos las librerias correspondientes para manejar su repositorio 
        if (isCurrentOS(OS.WINDOWS)) {

            // Instalamos SunMSCAPI, esto es necesario para Java 5 y Java 6 64 Bits (las versiones de este ultimo
            // que no traigan el proveedor en 64 bits)
            if(!installMSCapi(codeBase)) installOK = false;
        }

        return installOK;
    }

    /**
     * Instalamos las dependencias de la construcci&oacute;n MEDIA del cliente.
     * @param codeBase Ruta en donde se encuentran los ficheros instalables del cliente.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute;
     * correctamente, <code>false</code> en caso contrario.
     */
    private boolean installMediaBuildDependencies(final URL codeBase) {

        boolean installOK = true;

        // Instalamos las dependencias de la construccion LITE del cliente
        if(!installLiteBuildDependencies(codeBase)) installOK = false;

        // Java 5 requiere ciertos modulos y clases adicionales para el uso del modulo de firmas XML
        if(isJava5() && ! isCompatibilityPackInstalled()) {
            if(!installCompatibilityPack(codeBase)) installOK = false;
        }

        // Instalamos el modulo de firmas XML 
        if(this.isXMLModuleDependeciesAvailables(codeBase)) {
            if(!installXMLModuleDependencies(codeBase)) {
                Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al instalar las dependencias necesarias para las firmas XML"); //$NON-NLS-1$ //$NON-NLS-2$
                installOK = false;
            }
        } else {
            Logger.getLogger("es.gob.afirma").severe("No se encontraron en remoto los ficheros necesarios para la instalacion del modulo de firmas XML"); //$NON-NLS-1$ //$NON-NLS-2$
            installOK = false;
        }

        return installOK;
    }

    /**
     * Instalamos las dependencias de la construcci&oacute;n COMPLETA del cliente.
     * @param codeBase Ruta en donde se encuentran los ficheros instalables del cliente.
     * @return Devuelve <code>true</code> si la instalaci&oacute;n finaliz&oacute;
     * correctamente, <code>false</code> en caso contrario.
     */
    private boolean installCompletaBuildDependencies(final URL codeBase) {

        boolean installOK = true;

        // Instalamos la construccion MEDIA del cliente
        if(!installMediaBuildDependencies(codeBase)) installOK = false;

        // Instalacion de los ficheros adicionales
        
        return installOK;
    }
    
    /**
     * Comprueba si la versi&oacute;n instalada del cliente de firma es igual o superior a la publicada
     * en el directorio indicado. 
     * @param installFilesCodeBase Directorio con los instalables del cliente de firma.
     * @return Devuelve <code>true</code> si el cliente est&aacute; actualizado,
     * <code>false</code> en caso contrario.
     */
    boolean isUpdated(final URL installFilesCodeBase) {
    	
    	if(installFilesCodeBase == null) {
    		throw new NullPointerException("La URL con los ficheros de instalacion no puede ser nula"); //$NON-NLS-1$
    	}

        // Tomamos los datos de version del cliente remoto, si no podemos obtenerlos
        // se considerara actualizado el cliente
        Properties remoteVersion = getRemoteVersionProperties(installFilesCodeBase);
        if(remoteVersion == null) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo detectar la version remota, se considerara actualizado el cliente"); //$NON-NLS-1$ //$NON-NLS-2$
            return true;
        }
    	
        return isUpdated(installFilesCodeBase, remoteVersion);
    }
    
    /**
     * Comprueba si la versi&oacute;n instalada del cliente de firma es igual o superior a la publicada
     * en el directorio indicado. 
     * @param installFilesCodeBase Directorio con los instalables del cliente de firma.
     * @return Devuelve <code>true</code> si el cliente est&aacute; actualizado,
     * <code>false</code> en caso contrario.
     */
    private boolean isUpdated(final URL installFilesCodeBase, final Properties remoteVersion) {
    	
    	if(remoteVersion == null) {
    		throw new NullPointerException("La configuracion del cliente remoto no puede ser nula"); //$NON-NLS-1$
    	}
    	
        // Tomamos el JAR principal de la version instalada del cliente para extraer
        // de el properties de version
        final ZipFile coreZip;
        try {
            coreZip = new ZipFile(AOInstallParameters.getHomeApplication() + File.separator + AFIRMA_CORE_JAR);
        } 
        catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("No se puede detectar la version del cliente instalado, se considerara NO actualizado"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        final ZipEntry propEntry = coreZip.getEntry(VERSION_PROPERTIES);
        if(propEntry == null) {
            Logger.getLogger("es.gob.afirma").warning("No se ha encontrado la version del cliente instalado, se considera NO actualizado"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }

        // Tomamos los datos de version del cliente instalado
        final Properties localVersion = new Properties();
        try {
            localVersion.load(coreZip.getInputStream(propEntry));
        } catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido identificar la version del cliente instalado, se considera NO actualizado"); //$NON-NLS-1$ //$NON-NLS-2$
            return false;
        }
        
        try {
            coreZip.close();
        } 
        catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cerrar un flujo de datos del cliente instalado"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        
        // Si no existe el fichero remoto con los numeros de version, la instalada
        // se considera actualizada
        if(!AOBootUtil.existRemoteFile(installFilesCodeBase, VERSION_PROPERTIES)) {
            Logger.getLogger("es.gob.afirma").warning("El fichero remoto con las versiones no existe, se considerara actualizado el cliente"); //$NON-NLS-1$ //$NON-NLS-2$
            return true;
        }
        
        // Informo por consola de las versiones 
        Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
                "Version local: "+ //$NON-NLS-1$
                localVersion.getProperty("version.mayor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
                localVersion.getProperty("version.minor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
                localVersion.getProperty("version.build")+"; "+ //$NON-NLS-1$ //$NON-NLS-2$
                "Version remota: "+ //$NON-NLS-1$
                remoteVersion.getProperty("version.mayor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
                remoteVersion.getProperty("version.minor")+"."+ //$NON-NLS-1$ //$NON-NLS-2$
                remoteVersion.getProperty("version.build")); //$NON-NLS-1$
        
        // Comparamos las versiones
        if(Integer.parseInt(remoteVersion.getProperty("version.mayor")) > //$NON-NLS-1$
            Integer.parseInt(localVersion.getProperty("version.mayor")) || //$NON-NLS-1$
            Integer.parseInt(remoteVersion.getProperty("version.minor")) > //$NON-NLS-1$
            Integer.parseInt(localVersion.getProperty("version.minor")) || //$NON-NLS-1$
            Integer.parseInt(remoteVersion.getProperty("version.build")) > //$NON-NLS-1$
            Integer.parseInt(localVersion.getProperty("version.build"))) { //$NON-NLS-1$
                return false;
        }
        
        return true;
    }
    
    /**
     * Obtiene el properties con la configuraci&oacute;n de la versi&oacute;n remota del cliente.
     * @param installFilesCodeBase URL base con la versi&oacute;n remota del cliente.
     * @return El properties con la configuraci&oacute;n remota o <code>null</code> si no se
     * produce algun error.
     */
    private Properties getRemoteVersionProperties(final URL installFilesCodeBase) {
    	InputStream is = null;
    	final Properties remoteVersion = new Properties();
    	try {
    		String uriPath = installFilesCodeBase.toString().replace('\\', '/');
    		if(!uriPath.endsWith("/")) uriPath += "/"; //$NON-NLS-1$ //$NON-NLS-2$
    		
    		is = AOBootUtil.loadFile(AOBootUtil.createURI(uriPath + VERSION_PROPERTIES), null, false);
    		//is = AOBootUtil.loadFile(URI.create(uriPath + VERSION_PROPERTIES), null, false);
    		remoteVersion.load(is);
    	} catch (final Throwable e) {
    		Logger.getLogger("es.gob.afirma").warning("No se ha podido identificar la version del cliente remoto: "+e); //$NON-NLS-1$ //$NON-NLS-2$
    		return null;
    	} finally {
    		if(is != null) try { is.close(); } catch (Throwable e) { }
    	}

    	return remoteVersion;
    }

    /**
     * Desempaqueta un fichero Pack200 para obtener el fichero JAR equivalente. El fichero
     * JAR resultante se almacenar&aacute; en el mismo directorio que el original y tendr&aacute;
     * por nombre el mismo sin la extension ".pack.gz" o ".pack" y terminado en ".jar".
     * @param pack200Filename Ruta del fichero Pack200.
     * @throws AOException Cuando ocurre un error durante el desempaquetado.
     */
    private void unpack(final String pack200Filename) throws AOException {
        
        // Obtenemos el nombre del fichero de salida
        String jarFilename = pack200Filename + ".jar"; 
        if(pack200Filename.endsWith(".pack") || pack200Filename.endsWith(".pack.gz")) {
            jarFilename = pack200Filename.substring(0, pack200Filename.lastIndexOf(".pack")); 
            if(!jarFilename.endsWith(".jar")) jarFilename += ".jar";
        }
        
        // Desempaquetamos con el nombre de salida generado
        unpack(pack200Filename, jarFilename);
    }
    
    /**
     * Desempaqueta un fichero Pack200 para obtener el fichero JAR equivalente.
     * @param pack200Filename Ruta del fichero Pack200.
     * @param targetJarFilename Ruta del fichero de salida.
     * @throws AOException Cuando ocurre un error durante el desempaquetado.
     */
    private void unpack(final String pack200Filename, final String targetJarFilename) throws AOException {
        try {
            AOBootUtil.unpack200gunzip(
                AOBootUtil.loadFile(
                    AOBootUtil.createURI(pack200Filename),
                    null,
                    false
                ),
                new FileOutputStream(targetJarFilename)
            );
        } 
        catch(final AOException e) {
            throw e;
        } 
        catch(final Throwable e) {
            throw new AOException("Error al desempaquetar el fichero '" + pack200Filename + "'", e);
        }
        
        // Eliminamos la version empaquetada
        new File(pack200Filename).delete();
    }

    /**
     * Informa al usuario que se va a proceder a instalar el cliente Afirma y, a continuaci&oacute;n,
     * muestra al usuario el acuerdo de licencia. La funci&oacute;n devuelve {@code true} o {@code false}
     * seg&uacute;n se acepte o no el di&aacute;logo.
     * @param build Construcci&oacute;n que se de desea instalar.
     * @param parentComponent Componente padre sobre el que se muestran los di&aacute;logos.
     * @return Indica si se ha aceptado o no el acuerdo de licencia.
     */
    boolean prepareInstall(String build, Component parentComponent) {
    	
    	JOptionPane.showMessageDialog(parentComponent, Messages.getString("Installer.0")+build, Messages.getString("Installer.1"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

      if(new LicenceDialogPanel(parentComponent).showDisclaimer()) {
          Logger.getLogger("es.gob.afirma").info("Se ha aceptado el acuerdo de licencia"); //$NON-NLS-1$ //$NON-NLS-2$
          return true;
      } 

      Logger.getLogger("es.gob.afirma").info("No se ha aceptado el acuerdo de licencia, no se instalara el Cliente"); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
}
