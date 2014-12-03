package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Logger;

final class MozillaKeyStoreUtilitiesUnix {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String LIB_NSPR4_SO = "/lib/libnspr4.so"; //$NON-NLS-1$

	private static final String SOFTOKN3_SO = "libsoftokn3.so"; //$NON-NLS-1$

	private MozillaKeyStoreUtilitiesUnix() {
		// No instanciable
	}

	static String getSystemNSSLibDirUnix() throws FileNotFoundException {

		String nssLibDir = null;

		// *********************************************************************
		// *********************************************************************
		// Compobamos antes el caso especifico de NSS partido entre /usr/lib y
		// /lib, que se da en Fedora
		if (new File("/usr/lib/" + SOFTOKN3_SO).exists() && new File(LIB_NSPR4_SO).exists()) { //$NON-NLS-1$
			try {
				System.load(LIB_NSPR4_SO);
				nssLibDir = "/usr/lib"; //$NON-NLS-1$
			}
			catch (final Exception e) {
				nssLibDir = null;
				LOGGER.warning(
						"Descartamos el NSS situado entre /lib y /usr/lib porque no puede cargarse adecuadamente: " + e //$NON-NLS-1$
				);
			}
			if (nssLibDir != null) {
				return nssLibDir;
			}
		}
		// *********************************************************************
		// *********************************************************************

		final String[] paths =
			new String[] {
				"/usr/lib/firefox", //$NON-NLS-1$
				"/usr/lib/firefox-" + searchLastFirefoxVersion("/usr/lib/"), //$NON-NLS-1$ //$NON-NLS-2$
				"/opt/firefox", //$NON-NLS-1$
				"/opt/firefox-" + searchLastFirefoxVersion("/opt/"), //$NON-NLS-1$ //$NON-NLS-2$
				"/lib", //$NON-NLS-1$
				"/usr/lib", //$NON-NLS-1$
				"/usr/lib/nss", //$NON-NLS-1$
				"/usr/lib/i386-linux-gnu/nss", /* En algunos Ubuntu */ //$NON-NLS-1$
				"/opt/fedora-ds/clients/lib", //$NON-NLS-1$
				"/opt/google/chrome", /* NSS de Chrome cuando no hay NSS de Mozilla de la misma arquietctura */ //$NON-NLS-1$
				"/usr/lib/thunderbird", /* Si hay Thunderbird pero no Firefox */ //$NON-NLS-1$
				"/usr/lib64" /* NSS cuando solo hay Firefox de 64 en el sistema */ //$NON-NLS-1$
		};

		for (final String path : paths) {
			String tailingLib = "/libnspr4.so"; //$NON-NLS-1$
			if (new File(path + "/mozsqlite3.so").exists()) { //$NON-NLS-1$
				tailingLib = "/mozsqlite3.so"; //$NON-NLS-1$
			}
			else if (new File(path + "/libmozsqlite3.so").exists()) { //$NON-NLS-1$
				tailingLib = "/libmozsqlite3.so"; //$NON-NLS-1$
			}
			if (new File(path + "/" + SOFTOKN3_SO).exists() && new File(path + tailingLib).exists()) { //$NON-NLS-1$
				try {
					System.load(path + tailingLib);
					nssLibDir = path;
				}
				catch (final Exception e) {
					nssLibDir = null;
					LOGGER.warning(
							"Descartamos el NSS situado en '" + path //$NON-NLS-1$
							+ "' porque no puede cargarse adecuadamente: " //$NON-NLS-1$
							+ e
					);
				}
				if (nssLibDir != null) {
					return nssLibDir;
				}
			}
		}

		throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en UNIX"); //$NON-NLS-1$
	}

	/** Busca la &uacute;ltima versi&oacute;n de Firefox instalada en un sistema
	 * Linux o Solaris
	 * @param startDir Directorio de inicio para la b&uacute;squeda
	 * @return &Uacute;ltima versi&oacute;n instalada en el sistema */
	private static String searchLastFirefoxVersion(final String startDir) {
		final File directoryLib = new File(startDir);
		if (directoryLib.isDirectory()) {
			final String filenames[] = directoryLib.list();
			final List<String> firefoxDirectories = new ArrayList<String>();
			for (final String filename : filenames) {
				if (filename.startsWith("firefox-")) { //$NON-NLS-1$
					firefoxDirectories.add(filename.replace("firefox-", "")); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
			if (firefoxDirectories.isEmpty()) {
				return ""; //$NON-NLS-1$
			}
			for (int i = 0; i < firefoxDirectories.size(); i++) {
				try {
					Integer.getInteger(firefoxDirectories.get(i));
				}
				catch (final Exception e) {
					firefoxDirectories.remove(i);
				}
			}
			if (firefoxDirectories.size() == 1) {
				return firefoxDirectories.get(0);
			}
			Collections.sort(firefoxDirectories, new Comparator<String>() {
				/** {@inheritDoc} */
				@Override
				public int compare(final String o1, final String o2) {
					return o1.compareTo(o2);
				}
			});
			return firefoxDirectories.get(0);
		}
		return ""; //$NON-NLS-1$
	}

}
