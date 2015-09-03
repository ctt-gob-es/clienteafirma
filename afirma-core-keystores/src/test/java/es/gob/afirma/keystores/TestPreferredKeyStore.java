package es.gob.afirma.keystores;


/** Pruebas de precedencia de almacenes en un almacen agregado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPreferredKeyStore {

	/** Prueba de precedencia de almacenes en un almacen agregado CAPI - CERES 100% Java.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {

		final AggregatedKeyStoreManager aksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.WINDOWS,
			null, // Lib
			"CAPI-CERES", // Description //$NON-NLS-1$
			AOKeyStore.WINDOWS.getStorePasswordCallback(null),
			null // Parent
		);

		final AOKeyStoreManager ceresKsm = new AOKeyStoreManager();
		ceresKsm.init(AOKeyStore.CERES, null, AOKeyStore.CERES.getStorePasswordCallback(null), null, false);
		ceresKsm.setPreferred(true);

		aksm.addKeyStoreManager(ceresKsm);

		final String[] aliases = aksm.getAliases();
		for (final String alias : aliases) {
			System.out.println(alias);
		}

	}

}
