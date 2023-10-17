package es.gob.afirma.standalone.signdetails;

import java.util.List;

public interface SignAnalyzer {

	String getSignFormat();

	String getDataLocation();

	List<SignDetails> getAllSignDetails();

}
