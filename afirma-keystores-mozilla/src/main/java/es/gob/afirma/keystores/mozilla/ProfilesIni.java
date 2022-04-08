package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * Almacena la configuraci&oacute;n de perfiles de Mozilla Firefox.
 */
class ProfilesIni {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final static String PROFILES_ATTR_NAME = "name="; //$NON-NLS-1$
    private final static String PROFILES_ATTR_RELATIVE = "isrelative="; //$NON-NLS-1$
    private final static String PROFILES_ATTR_PATH = "path="; //$NON-NLS-1$
    private final static String PROFILES_ATTR_DEFAULT = "default="; //$NON-NLS-1$
    private static final String PROFILES_ATTR_VERSION = "version="; //$NON-NLS-1$
    private static final String PROFILES_ATTR_START_WITH_LAST_PROFILE = "startwithlastprofile="; //$NON-NLS-1$
    private static final String PROFILES_ATTR_LOCKED = "locked="; //$NON-NLS-1$

	private final List<FirefoxProfile> profilesList;
	private GeneralInfo generalInfo;
	private StateInfo stateInfo;

	/**
	 * Crea el objeto a parte del fichero de perfiles de Mozilla (profiles.ini).
	 * @param profilesIniFile Fichero de perfiles de Mozilla.
	 * @throws IOException Cuando ocurre un error en la lectura o forma del fichero de perfil.
	 */
	ProfilesIni(final File profilesIniFile) throws IOException {
		this.profilesList = new ArrayList<>();
		this.generalInfo = new GeneralInfo();
		this.stateInfo = null;

		//TODO: Aqui no se puede utilizar la clase BoundedBufferedReader como
		// "BufferedReader" porque marca posiciones del texto para poder deshacer
		// la lectura de los finales de linea. Para poder usarla aqui, habria que
		// rehacer esa clase para que no requiriese el marcado.
		try (
				final InputStream is = new FileInputStream(profilesIniFile);
				final BufferedReader in = new BufferedReader(
						new InputStreamReader(is, StandardCharsets.UTF_8));
				) {
			String line;
			while ((line = in.readLine()) != null) {

				// Buscamos un nuevo bloque
				line = line.trim();
				if (!line.startsWith("[")) { //$NON-NLS-1$
					continue;
				}

				// Marcamos para volver aqui si la siguiente linea tambien
				// fuese un inicio de seccion
				in.mark(256);

				// Bloque de perfil
				if (line.toLowerCase().startsWith("[profile")) { //$NON-NLS-1$
					try {
						this.profilesList.add(readProfile(in, profilesIniFile.getParentFile()));
					}
					catch (final Exception e) {
						LOGGER.warning("No se ha podido leer la informacion de uno de los perfiles de Firefox: " + e); //$NON-NLS-1$
					}
				}
				// Bloque de configuracion general
				else if (line.equalsIgnoreCase("[General]")) { //$NON-NLS-1$
					try {
						this.generalInfo = readGeneralInfo(in);
					}
					catch (final Exception e) {
						LOGGER.warning("No se ha podido leer la seccion general del fichero de perfiles de Firefox: " + e); //$NON-NLS-1$
					}
				}
				// Bloque de estado (a partir de Firefox 69)
				else if (line.toLowerCase().startsWith("[install")) { //$NON-NLS-1$
					// El fichero puede declarar mas de un bloque de este tipo. Si hay mas de uno,
					// utilizamos el declare el perfil bloqueado
					try {
						final StateInfo newStateInfo = readStateInfo(in);
						if (this.stateInfo == null || !this.stateInfo.isLockedDeclared()) {
							this.stateInfo = newStateInfo;
						}
					}
					catch (final Exception e) {
						LOGGER.warning("No se ha podido leer la seccion " + line + " con el perfil activo de Firefox: " + e); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
	}


	public List<FirefoxProfile> getProfilesList() {
		return this.profilesList != null ? new ArrayList<>(this.profilesList) : null;
	}

	public GeneralInfo getGeneralInfo() {
		return this.generalInfo;
	}

	public StateInfo getStateInfo() {
		return this.stateInfo;
	}

	/**
	 * Lee el bloque de propiedades correspondiente a un perfil de Firefox.
	 * @param in Lector de entrada del profiles.ini.
	 * @param baseDir Directorio base del que se lee el perfil.
	 * @return Informaci&oacute;n del perfil de Firefox.
	 * @throws IOException Cuando se produce alg&uacute;n error en la lectura.
	 */
	private static FirefoxProfile readProfile(final BufferedReader in, final File baseDir) throws IOException {

		final FirefoxProfile profile = new FirefoxProfile();

		String line;
		while ((line = in.readLine()) != null && !line.trim().isEmpty() && !line.trim().startsWith("[")) { //$NON-NLS-1$

			// Marcamos la posicion para volver si luego pisamos otra seccion
			in.mark(256);

			line = line.trim();
			if (line.toLowerCase().startsWith(PROFILES_ATTR_NAME)) {
				profile.setName(line.substring(PROFILES_ATTR_NAME.length()));
			}
			else if (line.toLowerCase().startsWith(PROFILES_ATTR_RELATIVE)) {
				profile.setRelative(
						line.substring(PROFILES_ATTR_RELATIVE.length()).equals("1") //$NON-NLS-1$
						);
			}
			else if (line.toLowerCase().startsWith(PROFILES_ATTR_PATH)) {
				profile.setPath(
						line.substring(PROFILES_ATTR_PATH.length())
						);
			}
			else if (line.toLowerCase().startsWith(PROFILES_ATTR_DEFAULT)) {
				profile.setDefault(
						line.substring(PROFILES_ATTR_DEFAULT.length()).equals("1") //$NON-NLS-1$
						);
			}
		}

		// Comprobamos si hemos pisado la cabecera de otra seccion, en cuyo caso, reseteamos
		// la posicion
		if (line != null && line.trim().startsWith("[")) { //$NON-NLS-1$
			try {
				in.reset();
			}
			catch (final Exception e) {
				LOGGER.warning("Se ha encontrado problemas en la forma del 'profiles.ini' de " //$NON-NLS-1$
						+ "Mozilla. Es posible que no se seleccione el perfil adecuado: " + e); //$NON-NLS-1$
			}
		}

		// Debemos encontrar al menos el nombre y la ruta del perfil
		if (profile.getName() == null || profile.getPath() == null) {
			throw new IllegalStateException("No se ha encontrado la informacion obligatoria del perfil"); //$NON-NLS-1$
		}

		// Componemos la ruta absoluta del perfil
		profile.setAbsolutePath(profile.isRelative() ?
				new File(baseDir, profile.getPath()).getAbsolutePath() :
					profile.getPath());

		// Comprobamos si existe el fichero de bloqueo
		profile.setLocked(
				new File(profile.getAbsolutePath(), "lock").exists() || // En UNIX //$NON-NLS-1$
				Files.isSymbolicLink(new File(profile.getAbsolutePath(), "lock").toPath()) || // En UNIX y Firefox 69 o superiores //$NON-NLS-1$
				new File(profile.getAbsolutePath(), "parent.lock").exists() // En Windows //$NON-NLS-1$
				);

		return profile;
	}

	private static GeneralInfo readGeneralInfo(final BufferedReader in) throws NumberFormatException, IOException {

		final GeneralInfo info = new GeneralInfo();

		String line;
		while ((line = in.readLine()) != null && !line.trim().isEmpty() && !line.trim().startsWith("[")) { //$NON-NLS-1$

			// Marcamos la posicion para volver si luego pisamos otra seccion
			in.mark(256);

			line = line.trim();
			if (line.toLowerCase().startsWith(PROFILES_ATTR_VERSION)) {
				info.setVersion(Integer.parseInt(line.substring(PROFILES_ATTR_VERSION.length())));
			}
			else if (line.toLowerCase().startsWith(PROFILES_ATTR_START_WITH_LAST_PROFILE)) {
				info.setStartWithLastProfile(
						line.substring(PROFILES_ATTR_START_WITH_LAST_PROFILE.length()).equals("1") //$NON-NLS-1$
						);
			}
		}

		// Comprobamos si hemos pisado la cabecera de otra seccion, en cuyo caso, reseteamos
		// la posicion
		if (line != null && line.trim().startsWith("[")) { //$NON-NLS-1$
			in.reset();
		}

		return info;
	}

	private static StateInfo readStateInfo(final BufferedReader in) throws IOException  {

		final StateInfo info = new StateInfo();

		String line;
		while ((line = in.readLine()) != null && !line.trim().isEmpty() && !line.trim().startsWith("[")) { //$NON-NLS-1$

			// Marcamos la posicion para volver si luego pisamos otra seccion
			in.mark(256);

			line = line.trim();
			if (line.toLowerCase().startsWith(PROFILES_ATTR_DEFAULT)) {
				info.setDefaultProfilePath(line.substring(PROFILES_ATTR_DEFAULT.length()));
			}
			else if (line.toLowerCase().startsWith(PROFILES_ATTR_LOCKED)) {
				info.setLocked(line.substring(PROFILES_ATTR_LOCKED.length()).equals("1")); //$NON-NLS-1$
			}
		}

		// Comprobamos si hemos pisado la cabecera de otra seccion, en cuyo caso, reseteamos
		// la posicion
		if (line != null && line.trim().startsWith("[")) { //$NON-NLS-1$
			in.reset();
		}

		// Debemos encontrar al menos la ruta del perfil activo
		if (info.getDefaultProfilePath() == null) {
			throw new IllegalStateException("No se ha encontrado la informacion sobre el perfil activo"); //$NON-NLS-1$
		}

		return info;
	}

	/** Almacena la configuraci&oacute;n para la identificaci&oacute;n de un
	 * perfil de Mozilla Firefox. */
	static final class FirefoxProfile {

		private String name = null;

		String getName() {
			return this.name;
		}

		void setName(final String n) {
			this.name = n;
		}

		private boolean relative = true;

		boolean isRelative() {
			return this.relative;
		}

		void setRelative(final boolean r) {
			this.relative = r;
		}

		private String path = null;

		String getPath() {
			return this.path;
		}

		void setPath(final String p) {
			this.path = p;
		}

		private String absolutePath = null;

		String getAbsolutePath() {
			return this.absolutePath;
		}

		void setAbsolutePath(final String ap) {
			this.absolutePath = ap;
		}

		private boolean def = false;

		boolean isDefault() {
			return this.def;
		}

		void setDefault(final boolean d) {
			this.def = d;
		}

		private boolean locked = false;

		boolean isLocked() {
			return this.locked;
		}

		void setLocked(final boolean lock) {
			this.locked = lock;
		}

		@Override
		public String toString() {
			return "Perfil de Firefox" + //$NON-NLS-1$
					(this.locked ? " bloqueado" : " no bloqueado") + //$NON-NLS-1$ //$NON-NLS-2$
					(this.def ? " y por defecto " : "") + //$NON-NLS-1$ //$NON-NLS-2$
					(this.absolutePath != null ? " situado en: " + this.absolutePath : ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Almacena la informaci&oacute;n general del fichero "profiles.ini". */
	static final class GeneralInfo {

		private int version = 1;

		private boolean startWithLastProfile = true;

		public int getVersion() {
			return this.version;
		}

		public void setVersion(final int version) {
			this.version = version;
		}

		public boolean isStartWithLastProfile() {
			return this.startWithLastProfile;
		}

		public void setStartWithLastProfile(final boolean startWithLastProfile) {
			this.startWithLastProfile = startWithLastProfile;
		}
	}

	/** Almacena la informaci&oacute;n de estado del fichero "profiles.ini". */
	static final class StateInfo {

		private String defaultProfilePath = null;

		private boolean locked = true;

		private boolean lockedDeclared = false;

		public String getDefaultProfilePath() {
			return this.defaultProfilePath;
		}

		public void setDefaultProfilePath(final String defaultDir) {
			this.defaultProfilePath = defaultDir;
		}

		public boolean isLocked() {
			return this.locked;
		}

		public void setLocked(final boolean locked) {
			this.locked = locked;
			this.lockedDeclared = true;
		}

		public boolean isLockedDeclared() {
			return this.lockedDeclared;
		}
	}
}