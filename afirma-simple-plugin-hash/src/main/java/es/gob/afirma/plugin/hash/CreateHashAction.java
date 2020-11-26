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
public class CreateHashAction extends RecursiveAction {

	/** Serial Id. */
	private static final long serialVersionUID = 9133180415134836622L;

	private static final Logger LOGGER = Logger.getLogger(CreateHashAction.class.getName());

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

	private final Path basePath;
	private final File dataFile;
	private final boolean recursive;
	private final String digestAlgorithm;
	private final Map<String, byte[]> result;
	private final ForkJoinPool pool;

	public CreateHashAction(final Path basePath, final File dataFile, final boolean recursive, final String digestAlgorithm,
			final Map<String, byte[]> result, final ForkJoinPool pool) {

		this.basePath = basePath;
		this.dataFile = dataFile;
		this.recursive = recursive;
		this.digestAlgorithm = digestAlgorithm;
		this.result = result;
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
			final List<CreateHashAction> tasks = new ArrayList<>();
			for (final File file : this.dataFile.listFiles()) {
				// Solo procesaremos los subdirectorios si es un proceso recursivo
				if (this.recursive || file.isFile()) {
					tasks.add(new CreateHashAction(this.basePath, file, this.recursive, this.digestAlgorithm, this.result, this.pool));
				}
			}
			if (!this.pool.isShutdown()) {
				ForkJoinTask.invokeAll(tasks);
			}
		}
	}

	private void processing() {
		byte[] hash;
		try {
			hash = HashUtil.getFileHash(this.digestAlgorithm, this.dataFile);
			this.result.put(this.basePath.relativize(Paths.get(this.dataFile.getAbsolutePath())).toString(), hash);
		} catch (final NoSuchAlgorithmException e) {
			LOGGER.log(Level.SEVERE, "Se ha indicado un algoritmo de hash no soportado", e); //$NON-NLS-1$
			this.pool.shutdown();
		}  catch (final Exception | Error e) {
			LOGGER.log(Level.SEVERE, "Error al calcular el hash de un fichero", e); //$NON-NLS-1$
			this.pool.shutdown();
		}
    }
}
