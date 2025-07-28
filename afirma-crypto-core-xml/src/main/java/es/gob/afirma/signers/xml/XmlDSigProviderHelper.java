package es.gob.afirma.signers.xml;

import java.lang.reflect.InvocationTargetException;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.NoSuchMechanismException;
import javax.xml.crypto.dsig.XMLSignatureFactory;

public class XmlDSigProviderHelper {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String APACHE_PROVIDER_CLASS = "org.apache.jcp.xml.dsig.internal.dom.XMLDSigRI"; //$NON-NLS-1$

    private static final String SUN_PROVIDER_CLASS = "org.jcp.xml.dsig.internal.dom.XMLDSigRI"; //$NON-NLS-1$

    private static Provider defaultProvider;

    private static boolean configured = false;

    /** Configura el proveedor de firmas XMLDSig para el entorno de ejecuci&oacute;n de Java en uso. */
    public static void configureXmlDSigProvider() {
    	configureXmlDSigProvider(false, true);
    }

    /**
     * Configura el proveedor de firmas XMLDSig para el entorno de ejecuci&oacute;n de Java en uso.
     * @param forced {@code true} para forzar a que se configure incluso si ya se configur&oacute;
     * anteriormente, {@code false} en caso contrario.
     * @param apachePreferred {@code true} para indicar que se configure un proveedor de Apache
     * Santuario externo (que se habra agregado como dependencia), {@code false} para indicar el
     * Apache Santuario interno de Java.
     */
    public static void configureXmlDSigProvider(final boolean forced, final boolean apachePreferred) {

    	// Omitimos la configuracion si ya se realizo previamente
    	if (configured && !forced) {
    		return;
    	}

		try {
			defaultProvider = XMLSignatureFactory.getInstance("DOM").getProvider(); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.warning("No se encontro un proveedor por defecto para la generacion de firmas XML: " + e); //$NON-NLS-1$
		}

    	// Correccion al problema insertado a partir de Apache Santuario 2.0.7 (Java 8 y Java 11)
    	//
    	// Establecemos la propiedad de Apache Santuario necesaria para que no se agreguen saltos
    	// de linea en los Base64 generados, ya que de hacerlo se utiliza "\r\n" y el "\r" aparece como
    	// "&#13;" al final de cada linea en las firmas XML. Las firmas generadas serian validas pero
    	// darian problemas al promocionarlas a formatos longevos.
    	// Referencias:
    	// - https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8177334
    	// - https://issues.apache.org/jira/browse/SANTUARIO-482
    	//
    	// Esta correccion depende del proveedor de seguridad utilizado, que sera
    	// el de Apache o el por defecto.

    	// Java 8u272 introduce el uso de Apache Santuario 2.1.1. Con esta version,
    	// pueden omitirse los saltos de linea con la siguiente propiedad.
    	System.setProperty("com.sun.org.apache.xml.internal.security.lineFeedOnly", "true"); //$NON-NLS-1$ //$NON-NLS-2$

    	// A partir de Apache Santuario 2.1.2 se implementa la siguiente propiedad, tambien disponible
    	// con Java 8u281 (Apache Santuario 2.1.4) y 11.0.5 (Apache Santuario 2.1.3)
    	System.setProperty("org.apache.xml.security.ignoreLineBreaks", "true"); //$NON-NLS-1$ //$NON-NLS-2$

    	// Si damos preferencia al proveedor de Apache, nos aseguramos de que este el primero de la lista
    	final String providerName = apachePreferred ? APACHE_PROVIDER_CLASS : SUN_PROVIDER_CLASS;
    	try {
    		installProvider(providerName);
	    }
	    catch (final Throwable e) {
	    	LOGGER.log(Level.SEVERE, "No se pudo dar preferencia al proveedor configurado", e); //$NON-NLS-1$
	    }

    	try {
    		final Provider currentProvider = XMLSignatureFactory.getInstance("DOM").getProvider(); //$NON-NLS-1$
    		LOGGER.info("Se usara el proveedor '" + currentProvider.getName() + "': " + currentProvider.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	catch (final NoSuchMechanismException e) {
    		LOGGER.warning("No hay proveedor instalado para XMLDSig. Se reinstala el proveedor por defecto: " + e); //$NON-NLS-1$
    		Security.addProvider(defaultProvider);
    	}
    	catch (final Exception e) {
    		LOGGER.log(Level.SEVERE, "Error en la verificacion de los proveedores XML", e); //$NON-NLS-1$
    	}

    	// Marcamos la configuracion como finalizada para no repetirla en siguientes ejecuciones
    	configured = true;
    }

    /**
     * Instala un proveedor de seguridad.
     * @param clazz Clase del proveedor de seguridad.
     * @throws ClassNotFoundException Cuando no se encuentra la clase.
     * @throws InstantiationException Cuando no se puede instanciar la clase.
     * @throws IllegalAccessException Cuando no se tiene acceso a la clase.
     * @throws IllegalArgumentException Cuando no existe un constructor vac&iacute;o en la clase.
     * @throws InvocationTargetException Si el constructor por defecto es abstracto.
     * @throws NoSuchMethodException Si no se encuentra un contructor por defecto.
     * @throws SecurityException No se tiene acceso al constructor por defecto.
     */
    private static void installProvider(final String clazz) throws ClassNotFoundException,
    		InstantiationException, IllegalAccessException, IllegalArgumentException,
    		InvocationTargetException, NoSuchMethodException, SecurityException {

    	// Comprobamos que no este instalado ya
    	Provider provider = null;
    	final Provider[] providers = Security.getProviders();
		for (int i = 0; provider == null && i < providers.length; i++) {
			if (clazz.equals(providers[i].getClass().getName())) {
				provider = providers[i];
			}
		}

		if (provider != null) {
			Security.removeProvider(provider.getName());
			LOGGER.info("Damos preferencia al proveedor " + provider.getName() + ": " + provider.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
		}
		else {
			final Class<?> classProvider = Class.forName(clazz);
			provider =  (Provider) classProvider.getDeclaredConstructor().newInstance();
			LOGGER.info("Instalamos con la maxima preferencia el proveedor " + provider.getName() + ": " + provider.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
		}
		Security.insertProviderAt(provider, 1);
    }
}
