package es.gob.afirma.signers.pkcs7;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;

import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;

/**
 * Conjunto de pruebas para
 */
public class TestGetSignersStructure {

	private static final String RESOURCE_COUNTERSIGN_CADES_BES = "countersign-cades.csig"; //$NON-NLS-1$
	private static final String RESOURCE_CADES_T = "cades-t.csig"; //$NON-NLS-1$
	private static final String RESOURCE_CADES_A = "cades-a.csig"; //$NON-NLS-1$

	/**
	 * Prueba a extraer los nodos de firma de una contrafirma CAdES-BES.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testGetStructureCounterSignCadesBES() throws Exception {

		System.out.println("Test Contrafirma CAdES-BES"); //$NON-NLS-1$
		System.out.println("--------------------------"); //$NON-NLS-1$

		final byte[] data = readResource(RESOURCE_COUNTERSIGN_CADES_BES);

		final ReadNodesTree reader = new ReadNodesTree();
		final AOTreeModel tree = reader.readNodesTree(data, true);

		printTreeBranch((AOTreeNode) tree.getRoot(), 0);

		System.out.println("=========================="); //$NON-NLS-1$
	}

	/**
	 * Prueba a extraer los nodos de firma de una firma CAdES-T.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testGetStructureCadesT() throws Exception {

		System.out.println("Test Firma CAdES-T"); //$NON-NLS-1$
		System.out.println("------------------"); //$NON-NLS-1$

		final byte[] data = readResource(RESOURCE_CADES_T);

		final ReadNodesTree reader = new ReadNodesTree();
		final AOTreeModel tree = reader.readNodesTree(data, true);

		printTreeBranch((AOTreeNode) tree.getRoot(), 0);

		System.out.println("=========================="); //$NON-NLS-1$
	}

	/**
	 * Prueba a extraer los nodos de firma de una firma CAdES-A.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testGetStructureCadesA() throws Exception {

		System.out.println("Test Firma CAdES-A"); //$NON-NLS-1$
		System.out.println("------------------"); //$NON-NLS-1$

		final byte[] data = readResource(RESOURCE_CADES_A);

		final ReadNodesTree reader = new ReadNodesTree();
		final AOTreeModel tree = reader.readNodesTree(data, true);

		printTreeBranch((AOTreeNode) tree.getRoot(), 0);

		System.out.println("=========================="); //$NON-NLS-1$
	}

	private static void printTreeBranch(final AOTreeNode node, final int lv) {

		// Escalonamos el nodo
		for (int i = 0; i < lv; i++) {
			System.out.print("\t"); //$NON-NLS-1$
		}

		// Imprimimos el nodo
		System.out.println(node.getUserObject());

		// Imprimimos los nodos hoja
		for (int i = 0; i < node.getChildCount(); i++) {
			printTreeBranch(node.getChildAt(i), lv + 1);
		}
	}

	private static byte[] readResource(final String filename) throws IOException {
		int n;
		final byte[] buffer = new byte[1024];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try (
			final InputStream is = TestGetSignersStructure.class.getResourceAsStream("/" + filename); //$NON-NLS-1$
		) {
			while ((n = is.read(buffer)) > 0) {
				baos.write(buffer, 0, n);
			}
		}
		return baos.toByteArray();
	}
}
