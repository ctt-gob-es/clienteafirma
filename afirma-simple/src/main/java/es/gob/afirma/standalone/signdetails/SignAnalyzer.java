package es.gob.afirma.standalone.signdetails;

import java.util.List;

import es.gob.afirma.core.util.tree.AOTreeModel;

public interface SignAnalyzer {

	AOTreeModel getSignersTree();

	String getSignFormat();

	String getDataLocation();

	List<SignDetails> getAllSignDetails();

}
