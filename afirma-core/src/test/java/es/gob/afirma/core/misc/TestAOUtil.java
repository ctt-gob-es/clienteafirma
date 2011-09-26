/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.misc;

import junit.framework.Assert;

import org.junit.Test;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3
 */
public final class TestAOUtil {

    /** Prueba para el getCN(), reconstruido para prescindir de javax.naming.
     * @throws Exception */
    @Test
    public void testGetCN() throws Exception {
        final String principals[][] = {
                {"CN=\"fulanito Menganito\", C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {" CN=\"fulanito Menganito\", C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=\"fulanito, Menganito\", C=ES", "fulanito, Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito , C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN = fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN =fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN = fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN =\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN= \"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {" OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion , C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU = mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, OU=mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, OU = mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU=mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU = mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU=\"mi organizacion\"", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU= \"mi organizacion\"", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"PCN=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=, C=ES", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, PCN=fulanito Menganito", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, P CN=\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, P ,CN=\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\"\"", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\"", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\" ", " "}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN a", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"Hola Mundo!!", "Hola Mundo!!"}, //$NON-NLS-1$ //$NON-NLS-2$
                {null, null}
        };
        
        for (final String principal[] : principals) {
            System.out.println(principal[0] + ": " + principal[1]); //$NON-NLS-1$
            Assert.assertEquals("El metodo getCN() no devuelve el resultado correcto para " + principal[0], AOUtil.getCN(principal[0]), principal[1]); //$NON-NLS-1$
        }
    }
}

