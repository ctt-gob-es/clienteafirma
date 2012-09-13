/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros 
 * para la realizacion de procesos de autenticacion, firma electronica y validacion 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion 
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha 
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos 
 * e Impulso de la Administracion Electronica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la 
 * European Union Public License publicada por la Comision Europea, 
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 * 
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 * 
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.jmulticard.jse.provider;

import java.io.FileDescriptor;
import java.net.InetAddress;
import java.security.AccessController;
import java.security.Permission;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/** Proveedor criptogr&aacute;fico JCA para DNIe.
 * Crea dos servicios:
 * <dl>
 * <dt><code>KeyStore</code></dt>
 * <dd><i>DNI</i></dd>
 * <dt><code>Signature</code></dt>
 * <dd><i>SHA1withRSA</i>, <i>SHA256withRSA</i>, <i>SHA384withRSA</i>, <i>SHA512withRSA</i></dd>
 * </dl>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DnieProvider extends Provider {

    private static final String SHA512WITH_RSA = "SHA512withRSA"; //$NON-NLS-1$

    private static final String SHA384WITH_RSA = "SHA384withRSA"; //$NON-NLS-1$

    private static final String SHA256WITH_RSA = "SHA256withRSA"; //$NON-NLS-1$

    private static final String SHA1WITH_RSA = "SHA1withRSA"; //$NON-NLS-1$

    private static final String ES_GOB_JMULTICARD_CARD_DNIE_DNIE_PRIVATE_KEY = "es.gob.jmulticard.jse.provider.DniePrivateKey"; //$NON-NLS-1$

    private static final long serialVersionUID = -1046745919235177156L;

    private static final String INFO = "Proveedor para el DNIe"; //$NON-NLS-1$
    private static final double VERSION = 0.1d;
    private static final String NAME = "DNIeJCAProvider"; //$NON-NLS-1$

    /** Crea un proveedor JCA para DNI Electr&oacute;nico (DNIe). */
    public DnieProvider() {
        super(NAME, VERSION, INFO);

        AccessController.doPrivileged(new PrivilegedAction<Void>() {
            @Override
            public Void run() {
                if (!(System.getSecurityManager() instanceof DnieSecurityManager)) {
                    System.setSecurityManager(new DnieSecurityManager(System.getSecurityManager()));
                }
                return null;
            }
        });

        // KeyStore
        put("KeyStore.DNI", "es.gob.jmulticard.jse.provider.DnieKeyStoreImpl"); //$NON-NLS-1$ //$NON-NLS-2$

        // Motores de firma
        put("Signature.SHA1withRSA", "es.gob.jmulticard.jse.provider.DnieSignatureImpl$Sha1"); //$NON-NLS-1$ //$NON-NLS-2$
        put("Signature.SHA256withRSA", "es.gob.jmulticard.jse.provider.DnieSignatureImpl$Sha256"); //$NON-NLS-1$ //$NON-NLS-2$
        put("Signature.SHA384withRSA", "es.gob.jmulticard.jse.provider.DnieSignatureImpl$Sha384"); //$NON-NLS-1$ //$NON-NLS-2$
        put("Signature.SHA512withRSA", "es.gob.jmulticard.jse.provider.DnieSignatureImpl$Sha512"); //$NON-NLS-1$ //$NON-NLS-2$

        // Claves soportadas
        put("Signature.SHA1withRSA SupportedKeyClasses", DnieProvider.ES_GOB_JMULTICARD_CARD_DNIE_DNIE_PRIVATE_KEY); //$NON-NLS-1$
        put("Signature.SHA256withRSA SupportedKeyClasses", DnieProvider.ES_GOB_JMULTICARD_CARD_DNIE_DNIE_PRIVATE_KEY); //$NON-NLS-1$
        put("Signature.SHA384withRSA SupportedKeyClasses", DnieProvider.ES_GOB_JMULTICARD_CARD_DNIE_DNIE_PRIVATE_KEY); //$NON-NLS-1$
        put("Signature.SHA512withRSA SupportedKeyClasses", DnieProvider.ES_GOB_JMULTICARD_CARD_DNIE_DNIE_PRIVATE_KEY); //$NON-NLS-1$

        // Alias de los nombres de algoritmos de firma
        put("Alg.Alias.Signature.1.2.840.113549.1.1.5", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.5", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.1.3.14.3.2.29", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHAwithRSA", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-1withRSA", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA1withRSAEncryption", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-1withRSAEncryption", DnieProvider.SHA1WITH_RSA); //$NON-NLS-1$

        put("Alg.Alias.Signature.1.2.840.113549.1.1.11", DnieProvider.SHA256WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.11", DnieProvider.SHA256WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-256withRSA", DnieProvider.SHA256WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-256withRSAEncryption", DnieProvider.SHA256WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA256withRSAEncryption", DnieProvider.SHA256WITH_RSA); //$NON-NLS-1$

        put("Alg.Alias.Signature.1.2.840.113549.1.1.12", DnieProvider.SHA384WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.12", DnieProvider.SHA384WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-384withRSA", DnieProvider.SHA384WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-384withRSAEncryption", DnieProvider.SHA384WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA384withRSAEncryption", DnieProvider.SHA384WITH_RSA); //$NON-NLS-1$

        put("Alg.Alias.Signature.1.2.840.113549.1.1.13", DnieProvider.SHA512WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.OID.1.2.840.113549.1.1.13", DnieProvider.SHA512WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-512withRSA", DnieProvider.SHA512WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA-512withRSAEncryption", DnieProvider.SHA512WITH_RSA); //$NON-NLS-1$
        put("Alg.Alias.Signature.SHA512withRSAEncryption", DnieProvider.SHA512WITH_RSA); //$NON-NLS-1$
    }

    private static final class DnieSecurityManager extends SecurityManager {

        private final SecurityManager sm;

        DnieSecurityManager(final SecurityManager sm) {
        	super();
            this.sm = sm;
        }

        /** {@inheritDoc} */
        @Override
        public void checkAccept(final String host, final int port) {
            if (this.sm != null) {
                this.sm.checkAccept(host, port);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkAccess(final Thread t) {
            if (this.sm != null) {
                this.sm.checkAccess(t);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkAccess(final ThreadGroup g) {
            if (this.sm != null) {
                this.sm.checkAccess(g);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkAwtEventQueueAccess() {
            if (this.sm != null) {
                this.sm.checkAwtEventQueueAccess();
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkConnect(final String host, final int port) {
            if (this.sm != null) {
                this.sm.checkConnect(host, port);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkConnect(final String host, final int port, final Object context) {
            if (this.sm != null) {
                this.sm.checkConnect(host, port, context);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkCreateClassLoader() {
            if (this.sm != null) {
                this.sm.checkCreateClassLoader();
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkDelete(final String file) {
            if (this.sm != null) {
                this.sm.checkDelete(file);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkExec(final String cmd) {
            if (this.sm != null) {
                this.sm.checkExec(cmd);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkExit(final int status) {
            if (this.sm != null) {
                this.sm.checkExit(status);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkLink(final String lib) {
            if (this.sm != null) {
                this.sm.checkLink(lib);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkListen(final int port) {
            if (this.sm != null) {
                this.sm.checkListen(port);
            }
        }

        private static final List<String> ALLOWED_MEMBER_ACCESS_PREFIXES = new ArrayList<String>(4);

        static {
            ALLOWED_MEMBER_ACCESS_PREFIXES.add("es.gob.jmulticard.asn1.der"); //$NON-NLS-1$
            ALLOWED_MEMBER_ACCESS_PREFIXES.add("es.gob.jmulticard.ui.passwordcallback"); //$NON-NLS-1$
            ALLOWED_MEMBER_ACCESS_PREFIXES.add("es.gob.jmulticard.jse.provider.DnieProvider"); //$NON-NLS-1$
            ALLOWED_MEMBER_ACCESS_PREFIXES.add("es.gob.jmulticard.jse.provider.DnieKeyStoreImpl"); //$NON-NLS-1$
            ALLOWED_MEMBER_ACCESS_PREFIXES.add("es.gob.jmulticard.jse.provider.DnieSignatureImpl"); //$NON-NLS-1$
        }

        /** {@inheritDoc} */
        @Override
        public void checkMemberAccess(final Class<?> clazz, final int which) {
            super.checkMemberAccess(clazz, which);
            if (clazz.getName().startsWith("es.gob.jmulticard")) { //$NON-NLS-1$
                for (final String classPrefix : ALLOWED_MEMBER_ACCESS_PREFIXES) {
                    if (clazz.getName().startsWith(classPrefix)) {
                        if (this.sm != null) {
                            this.sm.checkMemberAccess(clazz, which);
                        }
                        return;
                    }
                }
                throw new SecurityException("No se permite el acceso por reflexion a esta clase: " + clazz); //$NON-NLS-1$
            }
            if (this.sm != null) {
                this.sm.checkMemberAccess(clazz, which);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkMulticast(final InetAddress maddr) {
            if (this.sm != null) {
                this.sm.checkMulticast(maddr);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkMulticast(final InetAddress maddr, final byte ttl) {
            if (this.sm != null) {
                this.sm.checkPermission(new java.net.SocketPermission(maddr.getHostAddress(), "accept,connect")); //$NON-NLS-1$
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPackageAccess(final String pkg) {
            super.checkPackageAccess(pkg);
            if (this.sm != null) {
                this.sm.checkPackageAccess(pkg);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPackageDefinition(final String pkg) {
            super.checkPackageDefinition(pkg);
            if (pkg != null && pkg.startsWith("es.gob.jmulticard")) { //$NON-NLS-1$
                throw new SecurityException("No se permite la creacion de clases en este paquete"); //$NON-NLS-1$
            }
            if (this.sm != null) {
                this.sm.checkPackageDefinition(pkg);
            }
        }

        private static final Set<String> DENIED_PREMISSIONS_NAMES = new HashSet<String>(8);

        static {
            DENIED_PREMISSIONS_NAMES.add("setPolicy"); //$NON-NLS-1$
            DENIED_PREMISSIONS_NAMES.add("clearProviderProperties.DNIeJCAProvider"); //$NON-NLS-1$
            DENIED_PREMISSIONS_NAMES.add("putProviderProperty.DNIeJCAProvider"); //$NON-NLS-1$
            DENIED_PREMISSIONS_NAMES.add("removeProviderProperty.DNIeJCAProvider"); //$NON-NLS-1$
            DENIED_PREMISSIONS_NAMES.add("readDisplayPixels"); //$NON-NLS-1$
            DENIED_PREMISSIONS_NAMES.add("setSecurityManager"); //$NON-NLS-1$
        }

        /** {@inheritDoc} */
        @Override
        public void checkPermission(final Permission perm) {
            if (DENIED_PREMISSIONS_NAMES.contains(perm.getName())) {
                throw new SecurityException("Operacion no permitida: " + perm); //$NON-NLS-1$
            }

            if (this.sm != null) {
                this.sm.checkPermission(perm);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPermission(final Permission perm, final Object context) {
            if (this.sm != null) {
                this.sm.checkPermission(perm, context);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPrintJobAccess() {
            if (this.sm != null) {
                this.sm.checkPrintJobAccess();
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPropertiesAccess() {
            if (this.sm != null) {
                this.sm.checkPropertiesAccess();
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkPropertyAccess(final String key) {
            if (this.sm != null) {
                this.sm.checkPropertyAccess(key);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkRead(final FileDescriptor fd) {
            if (this.sm != null) {
                this.sm.checkRead(fd);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkRead(final String file) {
            if (this.sm != null) {
                this.sm.checkRead(file);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkRead(final String file, final Object context) {
            if (this.sm != null) {
                this.sm.checkRead(file, context);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkSecurityAccess(final String target) {
            if (this.sm != null) {
                this.sm.checkSecurityAccess(target);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkSetFactory() {
            if (this.sm != null) {
                this.sm.checkSetFactory();
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkSystemClipboardAccess() {
            if (this.sm != null) {
                this.sm.checkSystemClipboardAccess();
            }
        }

        /** {@inheritDoc} */
        @Override
        public boolean checkTopLevelWindow(final Object window) {
            if (this.sm != null) {
                return this.sm.checkTopLevelWindow(window);
            }
            return true;
        }

        /** {@inheritDoc} */
        @Override
        public void checkWrite(final FileDescriptor fd) {
            if (this.sm != null) {
                this.sm.checkWrite(fd);
            }
        }

        /** {@inheritDoc} */
        @Override
        public void checkWrite(final String file) {
            if (this.sm != null) {
                this.sm.checkWrite(file);
            }
        }
    }
}