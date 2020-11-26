package es.gob.afirma.plugin.hash;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveAction;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Acci&oacute;n que, recursivamente, calcular&aacute; el hash de ficheros de un directorio
 * utilizando hilos.
 */
public class CheckHashAction extends RecursiveAction {

	/** Serial Id. */
	private static final long serialVersionUID = 9133180415134836622L;

	/** Listado de ficheros de los que nunca se calculara el hash. */
	private final static Set<String> FILES_TO_AVOID = new HashSet<>(
		Arrays.asList(
			".fseventsd", //$NON-NLS-1$
			".Spotlight-V100", //$NON-NLS-1$
			".Trashes", //$NON-NLS-1$
			"._.Trashes", //$NON-NLS-1$
			".DS_Store", //$NON-NLS-1$
			".desktop", //$NON-NLS-1$
			"thumbs.db", //$NON-NLS-1$
			"$Recycle.Bin" //$NON-NLS-1$
		)
	);

	private static final Logger LOGGER = Logger.getLogger(CheckHashAction.class.getName());

	private final Path basePath;
	private final File dataFile;
	private final boolean recursive;
	private final String digestAlgorithm;
	private final Map<String, byte[]> hashes;
	private final HashReport report;
	private final ForkJoinPool pool;

	public CheckHashAction(final Path basePath, final File dataFile, final HashDocument hashDocument,
			final HashReport report, final ForkJoinPool pool) {

		this.basePath = basePath;
		this.dataFile = dataFile;
		this.recursive = hashDocument.isRecursive();
		this.digestAlgorithm = hashDocument.getAlgorithm();
		this.hashes = hashDocument.getHashes();
		this.report = report;
		this.pool = pool;
	}

	public CheckHashAction(final Path basePath, final File dataFile, final boolean recursive, final String algorithm,
			final Map<String, byte[]> hashes, final HashReport report, final ForkJoinPool pool) {

		this.basePath = basePath;
		this.dataFile = dataFile;
		this.recursive = recursive;
		this.digestAlgorithm = algorithm;
		this.hashes = hashes;
		this.report = report;
		this.pool = pool;
	}

	@Override
	protected void compute() {

		if (this.dataFile.isFile()) {
			if (!this.dataFile.getName().contains("~$") && !FILES_TO_AVOID.contains(this.dataFile.getName())) { //$NON-NLS-1$
				processing();
			}
		}
		else {
			final List<CheckHashAction> tasks = new ArrayList<>();
			for (final File file : this.dataFile.listFiles()) {
				// Solo procesaremos los subdirectorios si es un proceso recursivo
				if (this.recursive || file.isFile()) {
					tasks.add(new CheckHashAction(this.basePath, file, this.recursive, this.digestAlgorithm, this.hashes, this.report, this.pool));
				}
			}
			if (!this.pool.isShutdown()) {
				ForkJoinTask.invokeAll(tasks);
			}
		}
	}

	private void processing() {

		// Comprobamos si habia calculado un hash para este fichero, si no lo habia, se indica en el informe
		final String path = this.basePath.relativize(Paths.get(this.dataFile.getAbsolutePath())).toString();
		final byte[] previousHash = this.hashes.get(path);
		if (previousHash == null) {
			this.report.reportFileWithoutHash(path);
		}
		// Si habia un hash, calculamos el hash del fichero y comprobamos si coincide o no
		else {

			byte[] calculateHash;
			try {
				calculateHash = HashUtil.getFileHash(this.digestAlgorithm, this.dataFile);
			} catch (final NoSuchAlgorithmException e) {
				LOGGER.log(Level.SEVERE, "Se ha indicado un algoritmo de hash no soportado", e); //$NON-NLS-1$
				this.pool.shutdown();
				return;
			} catch (final Exception | Error e) {
				LOGGER.log(Level.SEVERE, "Error al calcular el hash de un fichero", e); //$NON-NLS-1$
				this.pool.shutdown();
				return;
			}

			if (Arrays.equals(previousHash, calculateHash)) {
				this.report.reportMatchingHash(path);
			}
			else {
				this.report.reportNoMatchingHash(path);
			}
			// Eliminamos los datos del fichero del mapa de resultados para reducir
			// su tamano y, posteriormente, saber cuales no se han comprobado
			synchronized (this.hashes) {
				this.hashes.remove(path);
			}
		}
	}
}
