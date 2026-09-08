package es.gob.afirma.keystores;


/** Pruebas de precedencia de almacenes en un almacen agregado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPreferredKeyStore {

	/** Prueba de precedencia de almacenes en un almacen agregado CAPI - CERES 100% Java.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {



		final AOKeyStoreManager aksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.WINDOWS,
			null, // Lib
			"CAPI-CERES", // Description //$NON-NLS-1$
			AOKeyStore.WINDOWS.getStorePasswordCallback(null),
			null, // Parent
			false
		);
		AggregatedKeyStoreManager multiKsm = new AggregatedKeyStoreManager(aksm);

		final AOKeyStoreManager ceresKsm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.CERES,
			null, // Lib
			"CERES 100% Java", // Description //$NON-NLS-1$
			AOKeyStore.CERES.getStorePasswordCallback(null),
			null, // Parent
			false
		);

		multiKsm.addKeyStoreManager(ceresKsm);

		final String[] aliases = aksm.getAliases();
		for (final String alias : aliases) {
			System.out.println(alias);
		}
	}

}
