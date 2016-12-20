package es.gob.afirma.signers.batch;

final class TempStoreFactory {

	private static final TempStore TS = new TempStoreFileSystem();

	static TempStore getTempStore() {
		return TS;
	}
}
