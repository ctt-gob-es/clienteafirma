package es.gob.afirma.signers.batch.client;

import java.util.List;

public interface BatchInfo {

	void updateResults(List<BatchDataResult> results);

	String getInfoString();
}
