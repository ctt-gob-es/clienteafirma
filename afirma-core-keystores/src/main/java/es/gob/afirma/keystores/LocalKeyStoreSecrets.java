/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclEntryPermission;
import java.nio.file.attribute.AclEntryType;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.attribute.UserPrincipal;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Gestiona el secreto local con el que se protege el almac&eacute;n de claves del canal
 * de comunicaci&oacute;n de Autofirma (<code>autofirma.pfx</code>).
 * <p>
 * Requisitos (ASVS 4.0.3 V2.5.1, V6.4.1 y V8, tarea T4):
 * </p>
 * <ul>
 *   <li>El secreto se genera con {@link SecureRandom} (32 bytes en Base64) en el primer
 *       arranque y se guarda en un fichero propio junto al almac&eacute;n.</li>
 *   <li>El fichero se escribe de forma at&oacute;mica: se crea ya con permisos de solo el
 *       usuario (0600 en POSIX, ACL de solo el propietario en Windows) y despu&eacute;s se
 *       escribe el contenido, sin un momento con permisos abiertos.</li>
 *   <li>Los almacenes heredados, protegidos con la contrase&ntilde;a fija de versiones
 *       anteriores, se migran: se vuelven a cifrar con el secreto y se guarda una copia
 *       <code>.bak</code> que se borra al verificar el resultado.</li>
 *   <li>La contrase&ntilde;a nunca se registra ni se incluye en mensajes de error.</li>
 * </ul>
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class LocalKeyStoreSecrets {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Sufijo del fichero de secreto asociado al almac&eacute;n de claves. */
	public static final String SECRET_FILE_SUFFIX = ".secret"; //$NON-NLS-1$

	/** Contrase&ntilde;a con la que se generaban los almacenes del canal local antes de T4.
	 * Solo se usa para migrar un almac&eacute;n heredado y nunca se escribe en disco. */
	static final String LEGACY_PASSWORD = "654321"; //$NON-NLS-1$

	private static final String PKCS12 = "PKCS12"; //$NON-NLS-1$
	private static final int SECRET_BYTES = 32;
	private static final String BACKUP_SUFFIX = ".bak"; //$NON-NLS-1$
	private static final String TEMP_PREFIX = ".afirma-secret-"; //$NON-NLS-1$
	private static final String TEMP_SUFFIX = ".tmp"; //$NON-NLS-1$
	private static final String WRITABLE_PROBE_PREFIX = ".afirma-w-"; //$NON-NLS-1$

	private static final Set<PosixFilePermission> OWNER_ONLY =
			PosixFilePermissions.fromString("rw-------"); //$NON-NLS-1$

	private LocalKeyStoreSecrets() {
		// No instanciable
	}

	/**
	 * Devuelve el fichero de secreto asociado a un almac&eacute;n de claves.
	 * @param keyStoreFile Almac&eacute;n de claves.
	 * @return Fichero de secreto (puede no existir todav&iacute;a).
	 */
	public static File getSecretFile(final File keyStoreFile) {
		if (keyStoreFile == null) {
			return null;
		}
		final File absolute = keyStoreFile.getAbsoluteFile();
		return new File(absolute.getParentFile(), absolute.getName() + SECRET_FILE_SUFFIX);
	}

	/**
	 * Obtiene la contrase&ntilde;a con la que abrir el almac&eacute;n del canal local. Si no
	 * existe el secreto local se genera; si el almac&eacute;n es heredado (contrase&ntilde;a
	 * fija de versiones anteriores) se vuelve a cifrar con el secreto nuevo.
	 * <p>
	 * El llamador es responsable de borrar la contrase&ntilde;a devuelta con
	 * {@link #wipe(char[])} en cuanto termine de usarla.
	 * </p>
	 * @param keyStoreFile Almac&eacute;n de claves del canal local.
	 * @return Contrase&ntilde;a del almac&eacute;n.
	 * @throws IOException Si no se puede leer o escribir el secreto o el almac&eacute;n.
	 * @throws GeneralSecurityException Si el almac&eacute;n no se puede abrir ni migrar.
	 */
	public static char[] resolvePassword(final File keyStoreFile) throws IOException, GeneralSecurityException {

		if (keyStoreFile == null) {
			throw new IOException("No se ha indicado el almacen de claves del canal local"); //$NON-NLS-1$
		}

		final File keyStore = keyStoreFile.getAbsoluteFile();
		if (!keyStore.isFile()) {
			throw new IOException("No se encuentra el almacen de claves del canal local: " + keyStore.getPath()); //$NON-NLS-1$
		}

		final File secretFile = getSecretFile(keyStore);

		// Caso normal: existe el secreto y abre el almacen
		if (secretFile.isFile()) {
			final char[] secret = readSecret(secretFile);
			if (canOpen(keyStore, secret)) {
				hardenFilePermissions(keyStore.toPath(), "almacen", false); //$NON-NLS-1$
				return secret;
			}
			wipe(secret);
		}

		// No hay secreto utilizable. Migrar exige reescribir el almacen: si el directorio
		// no es escribible por el usuario actual (instalacion por maquina en Windows) no
		// se puede migrar y se mantiene la contrasena heredada, avisando del riesgo.
		if (!isDirectoryWritable(keyStore.getParentFile())) {
			final char[] legacy = LEGACY_PASSWORD.toCharArray();
			final boolean legacyOpens = canOpen(keyStore, legacy);
			wipe(legacy);
			if (!legacyOpens) {
				throw new GeneralSecurityException(
					"No se pudo abrir el almacen del canal local con la contrasena heredada ni migrarlo: el directorio no es escribible. " //$NON-NLS-1$
					+ "El fichero puede estar danado o protegido con un secreto desconocido."); //$NON-NLS-1$
			}
			LOGGER.warning(
				"El almacen del canal local no es escribible por el usuario actual (" + keyStore.getPath() + "): " //$NON-NLS-1$ //$NON-NLS-2$
				+ "no se puede migrar a un secreto local y se mantiene la contrasena heredada. Reinstale Autofirma en un directorio del usuario o conceda permiso de escritura."); //$NON-NLS-1$
			return LEGACY_PASSWORD.toCharArray();
		}

		final char[] secret = readOrCreateSecret(secretFile);
		try {
			migrate(keyStore, secret);
		}
		catch (final IOException | GeneralSecurityException e) {
			wipe(secret);
			throw e;
		}
		return secret;
	}

	/** Borra de memoria una contrase&ntilde;a.
	 * @param password Contrase&ntilde;a a borrar. */
	public static void wipe(final char[] password) {
		if (password != null) {
			Arrays.fill(password, '\0');
		}
	}

	/** Lee y valida el fichero de secreto, corrigiendo sus permisos si est&aacute;n abiertos. */
	private static char[] readSecret(final File secretFile) throws IOException {

		final byte[] raw = Files.readAllBytes(secretFile.toPath());
		final String content;
		try {
			content = new String(raw, StandardCharsets.US_ASCII).trim();
		}
		finally {
			Arrays.fill(raw, (byte) 0);
		}

		final char[] secret = content.toCharArray();
		if (!isValidSecret(secret)) {
			wipe(secret);
			throw new IOException(
				"El fichero de secreto del canal local no es valido (" + secretFile.getPath() + "): " //$NON-NLS-1$ //$NON-NLS-2$
				+ "no contiene un secreto Base64 de 32 bytes."); //$NON-NLS-1$
		}

		hardenFilePermissions(secretFile.toPath(), "secreto del canal local", true); //$NON-NLS-1$
		return secret;
	}

	/** Recupera el secreto del fichero o lo genera y escribe de forma at&oacute;mica. */
	private static char[] readOrCreateSecret(final File secretFile) throws IOException {

		if (secretFile.isFile()) {
			return readSecret(secretFile);
		}

		final char[] secret = generateSecret();
		try {
			writeSecretAtomically(secretFile, secret);
		}
		catch (final IOException e) {
			wipe(secret);
			throw e;
		}

		LOGGER.info("Se ha generado el secreto local del canal TLS: " + secretFile.getPath()); //$NON-NLS-1$
		return secret;
	}

	/** Genera un secreto de 32 bytes con {@link SecureRandom}, codificado en Base64. */
	private static char[] generateSecret() {

		final byte[] random = new byte[SECRET_BYTES];
		try {
			new SecureRandom().nextBytes(random);
			return Base64.getEncoder().encodeToString(random).toCharArray();
		}
		finally {
			Arrays.fill(random, (byte) 0);
		}
	}

	/** Comprueba que un secreto es Base64 estricto de 32 bytes. */
	private static boolean isValidSecret(final char[] secret) {

		if (secret == null || secret.length == 0) {
			return false;
		}

		final byte[] decoded;
		try {
			decoded = Base64.getDecoder().decode(new String(secret));
		}
		catch (final IllegalArgumentException e) {
			return false;
		}

		try {
			return decoded.length == SECRET_BYTES;
		}
		finally {
			Arrays.fill(decoded, (byte) 0);
		}
	}

	/**
	 * Escribe el secreto en el fichero de forma at&oacute;mica: el fichero temporal se crea
	 * ya con permisos de solo el usuario, se escribe y se mueve sobre el destino.
	 */
	private static void writeSecretAtomically(final File secretFile, final char[] secret) throws IOException {

		final Path dir = secretFile.getAbsoluteFile().getParentFile().toPath();
		final Path tmp = createOwnerOnlyTempFile(dir);
		try {
			final byte[] data = new byte[secret.length];
			try {
				for (int i = 0; i < secret.length; i++) {
					data[i] = (byte) secret[i];
				}
				try (OutputStream os = Files.newOutputStream(tmp)) {
					os.write(data);
				}
			}
			finally {
				Arrays.fill(data, (byte) 0);
			}
			moveAtomically(tmp, secretFile.toPath());
		}
		finally {
			Files.deleteIfExists(tmp);
		}
	}

	/**
	 * Vuelve a cifrar un almac&eacute;n heredado con el secreto local. Guarda una copia
	 * <code>.bak</code> que se borra solo cuando el resultado se ha verificado.
	 */
	private static void migrate(final File keyStoreFile, final char[] newPassword)
			throws IOException, GeneralSecurityException {

		final KeyStore store = KeyStore.getInstance(PKCS12);
		final char[] legacy = LEGACY_PASSWORD.toCharArray();
		try (InputStream is = Files.newInputStream(keyStoreFile.toPath())) {
			store.load(is, legacy);
		}
		catch (final IOException | GeneralSecurityException e) {
			throw new GeneralSecurityException(
				"No se pudo abrir el almacen del canal local con la contrasena heredada: no se puede migrar. " //$NON-NLS-1$
				+ "El fichero puede estar danado o protegido con un secreto desconocido.", e); //$NON-NLS-1$
		}
		try {
			// En PKCS#12 la clave puede estar cifrada con una contrasena distinta de la del
			// almacen: se vuelve a guardar cada entrada de clave con el secreto local.
			final java.util.Enumeration<String> aliases = store.aliases();
			while (aliases.hasMoreElements()) {
				final String alias = aliases.nextElement();
				if (store.isKeyEntry(alias)) {
					store.setKeyEntry(
						alias,
						store.getKey(alias, legacy),
						newPassword,
						store.getCertificateChain(alias)
					);
				}
			}
		}
		catch (final GeneralSecurityException e) {
			throw new GeneralSecurityException(
				"No se pudieron releer las claves del almacen del canal local para migrarlas.", e); //$NON-NLS-1$
		}
		finally {
			wipe(legacy);
		}

		final Path keyStorePath = keyStoreFile.toPath();
		final Path backup = keyStorePath.resolveSibling(keyStoreFile.getName() + BACKUP_SUFFIX);
		Files.copy(keyStorePath, backup, StandardCopyOption.REPLACE_EXISTING);
		hardenFilePermissions(backup, "copia de seguridad del almacen", false); //$NON-NLS-1$

		final Path tmp = createOwnerOnlyTempFile(keyStorePath.getParent());
		boolean migrated = false;
		try {
			try (OutputStream os = Files.newOutputStream(tmp)) {
				store.store(os, newPassword);
			}
			moveAtomically(tmp, keyStorePath);
			if (!canOpen(keyStoreFile, newPassword)) {
				throw new IOException("El almacen migrado no se puede abrir con el secreto local"); //$NON-NLS-1$
			}
			migrated = true;
		}
		catch (final IOException | GeneralSecurityException e) {
			throw new IOException(
				"No se pudo migrar el almacen del canal local al secreto propio: se restaura la copia original.", e); //$NON-NLS-1$
		}
		finally {
			Files.deleteIfExists(tmp);
			if (!migrated) {
				restoreBackup(backup, keyStorePath);
			}
		}

		hardenFilePermissions(keyStorePath, "almacen", false); //$NON-NLS-1$
		try {
			Files.deleteIfExists(backup);
		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo borrar la copia de seguridad del almacen migrado: " + backup + " (" + e + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}

		LOGGER.info("Se ha migrado el almacen del canal local a un secreto propio del usuario: " + keyStoreFile.getPath()); //$NON-NLS-1$
	}

	/** Restaura la copia de seguridad sobre el almac&eacute;n original. */
	private static void restoreBackup(final Path backup, final Path keyStorePath) {

		try {
			if (Files.exists(backup)) {
				moveAtomically(backup, keyStorePath);
				LOGGER.warning("Se ha restaurado la copia de seguridad del almacen del canal local: " + keyStorePath); //$NON-NLS-1$
			}
		}
		catch (final IOException e) {
			LOGGER.severe("No se pudo restaurar la copia de seguridad del almacen del canal local: " + e); //$NON-NLS-1$
		}
	}

	/** Comprueba si el almac&eacute;n se puede abrir y sus claves se pueden recuperar con una contrase&ntilde;a. */
	private static boolean canOpen(final File keyStoreFile, final char[] password) {

		if (password == null) {
			return false;
		}

		try (InputStream is = Files.newInputStream(keyStoreFile.toPath())) {
			final KeyStore store = KeyStore.getInstance(PKCS12);
			store.load(is, password);
			final java.util.Enumeration<String> aliases = store.aliases();
			while (aliases.hasMoreElements()) {
				final String alias = aliases.nextElement();
				if (store.isKeyEntry(alias) && store.getKey(alias, password) == null) {
					return false;
				}
			}
			return true;
		}
		catch (final IOException | GeneralSecurityException e) {
			return false;
		}
	}

	/** Crea un fichero temporal que ya nace con permisos de solo el usuario. */
	private static Path createOwnerOnlyTempFile(final Path dir) throws IOException {

		try {
			return Files.createTempFile(
				dir, TEMP_PREFIX, TEMP_SUFFIX,
				PosixFilePermissions.asFileAttribute(OWNER_ONLY)
			);
		}
		catch (final UnsupportedOperationException e) {
			// Sistemas sin atributos POSIX (Windows): se crea vacio y se restringe la ACL
			// antes de escribir ningun contenido.
			final Path tmp = Files.createTempFile(dir, TEMP_PREFIX, TEMP_SUFFIX);
			hardenFilePermissions(tmp, "fichero temporal del secreto", true); //$NON-NLS-1$
			return tmp;
		}
	}

	/** Mueve un fichero de forma at&oacute;mica, con reemplazo del destino. */
	private static void moveAtomically(final Path source, final Path target) throws IOException {

		try {
			Files.move(
				source, target,
				StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING
			);
		}
		catch (final AtomicMoveNotSupportedException e) {
			Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
		}
	}

	/** Restringe un fichero a su propietario (0600 en POSIX, ACL de solo el usuario en Windows).
	 * @param path Fichero a restringir.
	 * @param description Descripci&oacute;n para el aviso de permisos corregidos.
	 * @param fatal Si {@code true}, no poder corregir los permisos es un error. */
	private static void hardenFilePermissions(final Path path, final String description, final boolean fatal) throws IOException {

		final PosixFileAttributeView posix = Files.getFileAttributeView(path, PosixFileAttributeView.class);
		if (posix != null) {
			final Set<PosixFilePermission> current = posix.readAttributes().permissions();
			if (!OWNER_ONLY.equals(current)) {
				posix.setPermissions(OWNER_ONLY);
				LOGGER.warning("Se corrigieron los permisos abiertos del " + description + ": " + path); //$NON-NLS-1$ //$NON-NLS-2$
			}
			return;
		}

		final AclFileAttributeView aclView = Files.getFileAttributeView(path, AclFileAttributeView.class);
		if (aclView != null) {
			final UserPrincipal owner = aclView.getOwner();
			if (!hasOnlyOwnerAcl(aclView, owner)) {
				aclView.setAcl(Collections.singletonList(AclEntry.newBuilder()
					.setType(AclEntryType.ALLOW)
					.setPrincipal(owner)
					.setPermissions(EnumSet.allOf(AclEntryPermission.class))
					.build()));
				LOGGER.warning("Se corrigieron los permisos abiertos del " + description + ": " + path); //$NON-NLS-1$ //$NON-NLS-2$
			}
			return;
		}

		if (fatal) {
			throw new IOException("No se pueden verificar ni restringir los permisos del " + description + ": " + path); //$NON-NLS-1$ //$NON-NLS-2$
		}
		LOGGER.warning("No se pueden verificar los permisos del " + description + ": " + path); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Comprueba si la ACL del fichero solo concede permisos a su propietario. */
	private static boolean hasOnlyOwnerAcl(final AclFileAttributeView aclView, final UserPrincipal owner) {

		try {
			for (final AclEntry entry : aclView.getAcl()) {
				if (!owner.equals(entry.principal())) {
					return false;
				}
			}
			return true;
		}
		catch (final IOException e) {
			return false;
		}
	}

	/** Comprueba si el usuario actual puede crear ficheros en un directorio. */
	private static boolean isDirectoryWritable(final File dir) {

		if (dir == null) {
			return false;
		}

		Path probe = null;
		try {
			probe = Files.createTempFile(dir.toPath(), WRITABLE_PROBE_PREFIX, TEMP_SUFFIX);
			return true;
		}
		catch (final IOException | RuntimeException e) {
			return false;
		}
		finally {
			if (probe != null) {
				try {
					Files.deleteIfExists(probe);
				}
				catch (final IOException e) {
					LOGGER.fine("No se pudo borrar la sonda de escritura: " + probe); //$NON-NLS-1$
				}
			}
		}
	}
}
