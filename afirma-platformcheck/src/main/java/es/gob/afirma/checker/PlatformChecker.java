/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.checker;

import java.awt.BorderLayout;
import java.awt.Color;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JApplet;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

/** Comprueba el entorno de ejecuci&oacute;n para determinar si es adecuado para la ejecuci&oacute;n
 * del Cliente Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PlatformChecker extends JApplet {

    private static final long serialVersionUID = -7446595885819219127L;

    private static final String OS_NAME = System.getProperty("os.name"); //$NON-NLS-1$
    
    private Object[][] getChecks() {
    	
    	List<Check> checks = new ArrayList<Check>();
    	
    	Check check = checkJavaVersion();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
    	check = checkBC();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
    	check = checkIText();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
    	check = checkSunMSCAPI();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
    	check = checkSunPKCS11();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
    	check = checkOS();
    	if (check.getCode() != 0) {
    		checks.add(check);
    	}
        
    	if (checks.isEmpty()) {
    		return new String[0][2];
    	}
    	
    	final Object[][] ret = new Object[checks.size()][2];
    	for (int i = 0; i< checks.size(); i++) {
    		ret[i][0] = Integer.toString(checks.get(i).getCode());
    		ret[i][1] = checks.get(i).toString();
    	}
        
        return ret;
    }
    
    @Override
    public void init() {
    	
    	this.setBackground(Color.WHITE);
    	
    	Object[][] checkItems = getChecks();
    	if (checkItems.length == 0) {
    		setLayout(new BorderLayout());
    		JLabel okLabel = new JLabel(CheckerMessages.getString("PlatformChecker.0")); //$NON-NLS-1$
    		okLabel.setBackground(Color.WHITE);
            add(okLabel, BorderLayout.PAGE_START);
    		return;
    	}
    	
        JTable table = new JTable(new CheckTableModel(
        		checkItems, new String[] {CheckerMessages.getString("PlatformChecker.1"), CheckerMessages.getString("PlatformChecker.2") })); //$NON-NLS-1$ //$NON-NLS-2$
        table.setFocusable(false);
        table.getColumnModel().getColumn(0).setMaxWidth(85);
        table.getColumnModel().getColumn(1).setMinWidth(500);
        setLayout(new BorderLayout());
        add(table.getTableHeader(), BorderLayout.PAGE_START);
        add(table, BorderLayout.CENTER);
    }
    
    private static class Check {
        private final int code;
        private final String description;
        
        Check(final int cod, final String desc) {
            this.code = cod;
            this.description = desc;
        }
        
        @Override
        public String toString() {
            return this.description;
        }
        
        int getCode() {
            return this.code;
        }
    }
    
    private static final String RECOMMENDED_JAVA = "1.6.0_30"; //$NON-NLS-1$
    private static final String MINIMUM_JAVA = "1.6.0_18"; //$NON-NLS-1$
    
    private Check checkJavaVersion() {
        final String currentJava = System.getProperty("java.version"); //$NON-NLS-1$
        if (MINIMUM_JAVA.compareTo(currentJava) > 0) {
            return new Check(100, CheckerMessages.getString("PlatformChecker.3", new String[] {currentJava, MINIMUM_JAVA})); //$NON-NLS-1$
        }
        if (RECOMMENDED_JAVA.compareTo(currentJava) > 0) {
            return new Check(900, CheckerMessages.getString("PlatformChecker.4", new String[] {currentJava, RECOMMENDED_JAVA})); //$NON-NLS-1$
        }
        return new Check(0, null);
    }
    
    private Check checkSunMSCAPI() {
        if (!OS_NAME.contains("indows")) { //$NON-NLS-1$
            return new Check(0, null);
        }
        try {
            PlatformChecker.class.getClassLoader().loadClass("sun.security.mscapi.SunMSCAPI"); //$NON-NLS-1$
        }
        catch (final Throwable e) {
            return new Check(101, CheckerMessages.getString("PlatformChecker.5")); //$NON-NLS-1$
        }
        return new Check(0, null);
    }
    
    private Check checkSunPKCS11() {
        try {
            PlatformChecker.class.getClassLoader().loadClass("sun.security.pkcs11.SunPKCS11"); //$NON-NLS-1$
        }
        catch (final Throwable e) {
            return new Check(102, CheckerMessages.getString("PlatformChecker.6")); //$NON-NLS-1$
        }
        return new Check(0, null);
    }
    
    private static final String MINIMUM_WINDOWS_VERSION = "5.1"; //$NON-NLS-1$
    private static final String MINIMUM_MACOSX_VERSION = "10.6"; //$NON-NLS-1$
    
    private Check checkOS() {
        final String osVersion = System.getProperty("os.version"); //$NON-NLS-1$
        if (OS_NAME.contains("indows")) { //$NON-NLS-1$
            if (MINIMUM_WINDOWS_VERSION.compareTo(osVersion) > 0) {
                return new Check(103, CheckerMessages.getString("PlatformChecker.7", new String[] {osVersion, MINIMUM_WINDOWS_VERSION}));  //$NON-NLS-1$
            }
        }
        else if (OS_NAME.startsWith("Mac OS X")) { //$NON-NLS-1$
            if (MINIMUM_MACOSX_VERSION.compareTo(osVersion) > 0) {
                return new Check(103, CheckerMessages.getString("PlatformChecker.8", new String[] {osVersion, MINIMUM_MACOSX_VERSION})); //$NON-NLS-1$
            }
        }
        else if (!OS_NAME.contains("inux") || !OS_NAME.contains("SunOS") || !OS_NAME.contains("olaris")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return new Check(103, CheckerMessages.getString("PlatformChecker.9")); //$NON-NLS-1$
        }
        return new Check(0, null);
    }
    
    private static final String MINIMUM_BOUNCYCASTLE_VERSION = "1.46";  //$NON-NLS-1$
    
    private Check checkBC() {
        final String bcVersion;
        try {
            final Class<?> bouncyCastleProviderClass = Class.forName("org.bouncycastle.jce.provider.BouncyCastleProvider"); //$NON-NLS-1$
            final  Object bouncyCastleProviderObject = bouncyCastleProviderClass.newInstance();
            final Class<?> providerClass = Class.forName("java.security.Provider"); //$NON-NLS-1$
            final Method getVersionMethod = providerClass.getDeclaredMethod("getVersion"); //$NON-NLS-1$
            bcVersion = ((Double) getVersionMethod.invoke(bouncyCastleProviderObject)).toString();
        } 
        catch (final Exception e) {
            return new Check(0, null);
        }
        if (MINIMUM_BOUNCYCASTLE_VERSION.compareTo(bcVersion) > 0) {
            return new Check(106, CheckerMessages.getString("PlatformChecker.10", new String[] {bcVersion, MINIMUM_BOUNCYCASTLE_VERSION}));  //$NON-NLS-1$
        }
        return new Check(901, CheckerMessages.getString("PlatformChecker.11")); //$NON-NLS-1$
    }
    
    private static final String ITEXT_VERSION = "2.1.7"; //$NON-NLS-1$
    
    private Check checkIText() {
        final String itextVersion;
        try {
            final Class<?> documentClass = Class.forName("com.lowagie.text.Document"); //$NON-NLS-1$
            final Method getReleaseMethod = documentClass.getDeclaredMethod("getRelease"); //$NON-NLS-1$
            itextVersion = (String) getReleaseMethod.invoke(null);
        } 
        catch (final Exception e) {
            return new Check(0, null);
        }
        if (ITEXT_VERSION.equals(itextVersion)) {
            return new Check(902, CheckerMessages.getString("PlatformChecker.12")); //$NON-NLS-1$
        }
        return new Check(107,
        		CheckerMessages.getString("PlatformChecker.13", new String[] {itextVersion, ITEXT_VERSION})); //$NON-NLS-1$
    }

    /**
     * TableModel personalizado que bloquea la edicion de las celdas de la tabla. 
     */
    private class CheckTableModel extends DefaultTableModel {

    	/** Default Serial UID */
		private static final long serialVersionUID = -4843738929661752061L;

		public CheckTableModel(Object[][] rowData, Object[] columnNames) {
    		super(rowData, columnNames);
		}
    	
    	@Override
    	public boolean isCellEditable(int row, int column) {
    		return false;
    	}
    }
}
