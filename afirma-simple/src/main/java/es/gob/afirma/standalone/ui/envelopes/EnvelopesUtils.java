package es.gob.afirma.standalone.ui.envelopes;

import java.awt.Component;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class EnvelopesUtils {

	private EnvelopesUtils() {
		// No permitimos la instanciacion
	}

	/** Recupera los almacenes compatibles con el sistema y preparados
     * para contener certificados de firma.
     * @return Listado de almacenes. */
    public static KeyStoreConfiguration[] getKeyStoresToSign() {

        final List<KeyStoreConfiguration> stores = new ArrayList<>();

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.WINDOWS, null, null));
        }
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.APPLE, null, null));
        }

        stores.add(new KeyStoreConfiguration(AOKeyStore.MOZ_UNI, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.DNIEJAVA, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS12, null, null));

        return stores.toArray(new KeyStoreConfiguration[0]);
    }

    /** Recupera los almacenes compatibles con el sistema y preparados
     * para contener certificados para envoltura de datos.
     * @return Listado de almacenes. */
    public static KeyStoreConfiguration[] getKeyStoresToWrap() {

        final List<KeyStoreConfiguration> stores = new ArrayList<>();

        stores.add(new KeyStoreConfiguration(AOKeyStore.SINGLE, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS12, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS11, null, null));

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.WINADDRESSBOOK, null, null));
        }

        return stores.toArray(new KeyStoreConfiguration[0]);
    }

    /**
     * Obtiene el fichero elegido por el usuario dependiendo de la extension que haya elegido en el combobox.
     * @return El nombre del fichero seleccionado.
     */
    static File addFileSelected(final String[] extension,
    					 final JComboBox<KeyStoreConfiguration> comboDestinatarios,
    					 final BufferedImage icon,
    					 final Component parent) {

		final File file;
		try {
			if (extension[0] != null) {
				Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
					"Boton de anadir -> " + extension[0] //$NON-NLS-1$
				);
			}
			file = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("MenuDigitalEnvelope.14"), //$NON-NLS-1$
				null,
				null,
				extension,
				comboDestinatarios.getSelectedItem().toString(),
				false,
				false,
				icon,
				parent
			)[0];
		}
		catch (final AOCancelledOperationException e) {
			Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Accion cancelada por el usuario" + e //$NON-NLS-1$
			);
			return null;
		}
		if (!file.canRead()) {
			AOUIFactory.showErrorMessage(icon,
				SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
		}
		return file;
    }

    /** Lectura de fichero.
     * @param filepath
     * @return
     * @throws java.io.FileNotFoundException
     * @throws IOException */
    static byte[] readFile(final String filepath) throws IOException {
        try ( final InputStream fileIn = AOUtil.loadFile(AOUtil.createURI(filepath)); ) {
            return AOUtil.getDataFromInputStream(fileIn);
        }
        catch (final URISyntaxException e) {
        	throw new IOException("Ruta hacia el fichero de datos invalida: " + filepath, e); //$NON-NLS-1$
		}
    }
}
