/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.cliente.utilidades.exp;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.exceptions.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
final class SimpleCondition extends AClause implements ConditionConstants {
	
    private String field;
    private Operator operator;
    private String value;
    private Object aux = null;
    private String hashAlg;

    SimpleCondition(final String str) throws AOException {
        Matcher fieldMatcher = fieldConditionPattern.matcher(str);
        Matcher valueMatcher = valueConditionPattern.matcher(str);
        Matcher operatorMatcher = operatorPattern.matcher(str);

        int ini, fin;
        if (fieldMatcher.find()) {
            ini = fieldMatcher.start();
            fin = fieldMatcher.end();
            field = str.substring(ini, fin);
        }
        else throw new AOException("Error de sintaxis: " + str);

        if (operatorMatcher.find()) {
            ini = operatorMatcher.start();
            fin = operatorMatcher.end();
            String strOperator = str.substring(ini, fin);
            
            if (strOperator.equals("="))				 operator = Operator.EQ;
	        else if (strOperator.equals("#MATCHES#"))	 operator = Operator.MATCHES;
            else if (strOperator.equals("#NOT_MATCHES#"))operator = Operator.NOT_MATCHES;
            else throw new AOException("Operador desconocido: " + str);
        }
        else throw new AOException("Error de sintaxis: " + str);

        if (valueMatcher.find()) {
            ini = valueMatcher.start();
            fin = valueMatcher.end();
            value = str.substring(ini + 2, fin - 2);
        }
        else throw new AOException("Error de sintaxis: " + str);
    }

    @Override
	public boolean eval(X509Certificate cert) throws AOException {
    	if (cert==null) return false;
        try {
            Object o1=null, o2;
            // Ver java.util.Pattern para lo de "?i"
            //if (aux == null && (operator == Operator.MATCHES || operator == Operator.NOT_MATCHES)) aux = "(?i)" + value; 

            if (field.equalsIgnoreCase("ISSUER.DN")) {
                aux = value;
                o1 = rewriteLdapName(cert.getIssuerDN().getName());
            }
            else if (field.equalsIgnoreCase("SUBJECT.SERIALNUMBER")) {
            	if (aux == null) aux = value;
            	String principal = cert.getSubjectX500Principal().getName();
            	List<Rdn> rdns = new LdapName(principal).getRdns();
        		if (rdns != null && (!rdns.isEmpty())) {
        			for (int j=0; j<rdns.size(); j++) {
        				if (rdns.get(j).toString().startsWith("serialnumber=") || rdns.get(j).toString().startsWith("SERIALNUMBER=")) {
        					o1 = asciiHexToString(rdns.get(j).toString().substring(13));
        				}
        				else if (rdns.get(j).toString().startsWith("2.5.4.5=")) {
        				    o1 = asciiHexToString(rdns.get(j).toString().substring(8));
        				}
        			}
        			if (o1==null) return false;
        		} 
        		else return false;
            }
            else if (field.equalsIgnoreCase("SUBJECT.DN")) {
                aux = value;
                o1 = rewriteLdapName(cert.getSubjectDN().getName());
            }
            else if (field.equalsIgnoreCase("SUBJECT.SN")) {
                if (aux == null) aux = new BigInteger(value).toString();
                o1 = cert.getSerialNumber().toString();
            }
            else if (field.toUpperCase().startsWith("SUBJECT.FP(")) {
                if (hashAlg == null) {
                    Matcher matcher = hashAlgPattern.matcher(field);
                    matcher.find();
                    hashAlg = field.substring(matcher.start(), matcher.end());
                }
                if (aux == null) aux = value;
                o1 = HEX_HELPER.toString(MessageDigest.getInstance(hashAlg).digest(cert.getEncoded()));
            }
            else throw new AOException("Campo desconocido:" + field);
            o2 = aux;
            
            Logger.getLogger("es.gob.afirma").info("\nValor extraido del certificado: " + o1+"\nValor patron del filtro: " + o2);
            
            return operator.eval(o1, o2);
        }
        catch (Throwable e) {
        	throw new AOException("Error evaluando expresion: " + e);
        }
    } 			

    private String asciiHexToString(String ah) {
        if(!ah.startsWith("#")) {
            return ah;
        }
        
        ah = ah.replaceAll("#", "");
        String tmpStr;
        StringBuilder outStr = new StringBuilder();
        for (int i=0;i<ah.length()/2;i++) {
            tmpStr = ah.substring(i*2, i*2+2);
            tmpStr = tmpStr.trim();
            
            if      (tmpStr.equalsIgnoreCase("30")) outStr.append("0");
            else if (tmpStr.equalsIgnoreCase("31")) outStr.append("1");
            else if (tmpStr.equalsIgnoreCase("32")) outStr.append("2");
            else if (tmpStr.equalsIgnoreCase("33")) outStr.append("3");
            else if (tmpStr.equalsIgnoreCase("34")) outStr.append("4");
            else if (tmpStr.equalsIgnoreCase("35")) outStr.append("5");
            else if (tmpStr.equalsIgnoreCase("36")) outStr.append("6");
            else if (tmpStr.equalsIgnoreCase("37")) outStr.append("7");
            else if (tmpStr.equalsIgnoreCase("38")) outStr.append("8");
            else if (tmpStr.equalsIgnoreCase("39")) outStr.append("9");
            else if (tmpStr.equalsIgnoreCase("41")) outStr.append("A");
            else if (tmpStr.equalsIgnoreCase("42")) outStr.append("B");
            else if (tmpStr.equalsIgnoreCase("43")) outStr.append("C");
            else if (tmpStr.equalsIgnoreCase("44")) outStr.append("D");
            else if (tmpStr.equalsIgnoreCase("45")) outStr.append("E");
            else if (tmpStr.equalsIgnoreCase("46")) outStr.append("F");
            else if (tmpStr.equalsIgnoreCase("47")) outStr.append("G");
            else if (tmpStr.equalsIgnoreCase("48")) outStr.append("H");
            else if (tmpStr.equalsIgnoreCase("49")) outStr.append("I");
            else if (tmpStr.equalsIgnoreCase("4a")) outStr.append("J");
            else if (tmpStr.equalsIgnoreCase("4b")) outStr.append("K");
            else if (tmpStr.equalsIgnoreCase("4c")) outStr.append("L");
            else if (tmpStr.equalsIgnoreCase("4d")) outStr.append("M");
            else if (tmpStr.equalsIgnoreCase("4e")) outStr.append("N");
            else if (tmpStr.equalsIgnoreCase("4f")) outStr.append("O");
            else if (tmpStr.equalsIgnoreCase("50")) outStr.append("P");
            else if (tmpStr.equalsIgnoreCase("51")) outStr.append("Q");
            else if (tmpStr.equalsIgnoreCase("52")) outStr.append("R");
            else if (tmpStr.equalsIgnoreCase("53")) outStr.append("S");
            else if (tmpStr.equalsIgnoreCase("54")) outStr.append("T");
            else if (tmpStr.equalsIgnoreCase("55")) outStr.append("U");
            else if (tmpStr.equalsIgnoreCase("56")) outStr.append("V");
            else if (tmpStr.equalsIgnoreCase("57")) outStr.append("W");
            else if (tmpStr.equalsIgnoreCase("58")) outStr.append("X");
            else if (tmpStr.equalsIgnoreCase("59")) outStr.append("Y");
            else if (tmpStr.equalsIgnoreCase("5a")) outStr.append("Z");
            else if (tmpStr.equalsIgnoreCase("61")) outStr.append("a");
            else if (tmpStr.equalsIgnoreCase("62")) outStr.append("b");
            else if (tmpStr.equalsIgnoreCase("63")) outStr.append("c");
            else if (tmpStr.equalsIgnoreCase("64")) outStr.append("d");
            else if (tmpStr.equalsIgnoreCase("65")) outStr.append("e");
            else if (tmpStr.equalsIgnoreCase("66")) outStr.append("f");
            else if (tmpStr.equalsIgnoreCase("67")) outStr.append("g");
            else if (tmpStr.equalsIgnoreCase("68")) outStr.append("h");
            else if (tmpStr.equalsIgnoreCase("69")) outStr.append("i");
            else if (tmpStr.equalsIgnoreCase("6a")) outStr.append("j");
            else if (tmpStr.equalsIgnoreCase("6b")) outStr.append("k");
            else if (tmpStr.equalsIgnoreCase("6c")) outStr.append("l");
            else if (tmpStr.equalsIgnoreCase("6d")) outStr.append("m");
            else if (tmpStr.equalsIgnoreCase("6e")) outStr.append("n");
            else if (tmpStr.equalsIgnoreCase("6f")) outStr.append("o");
            else if (tmpStr.equalsIgnoreCase("70")) outStr.append("p");
            else if (tmpStr.equalsIgnoreCase("71")) outStr.append("q");
            else if (tmpStr.equalsIgnoreCase("72")) outStr.append("r");
            else if (tmpStr.equalsIgnoreCase("73")) outStr.append("s");
            else if (tmpStr.equalsIgnoreCase("74")) outStr.append("t");
            else if (tmpStr.equalsIgnoreCase("75")) outStr.append("u");
            else if (tmpStr.equalsIgnoreCase("76")) outStr.append("v");
            else if (tmpStr.equalsIgnoreCase("77")) outStr.append("w");
            else if (tmpStr.equalsIgnoreCase("78")) outStr.append("x");
            else if (tmpStr.equalsIgnoreCase("79")) outStr.append("y");
            else if (tmpStr.equalsIgnoreCase("7a")) outStr.append("z");
            else if (tmpStr.equalsIgnoreCase("2d")) outStr.append("-");
            else if (tmpStr.equalsIgnoreCase("5f")) outStr.append("_");
            else if (tmpStr.equalsIgnoreCase("20")) outStr.append(" ");
        }
        
        return outStr.toString();
    }
    
    private String rewriteLdapName(String ln) {
        try {
            List<Rdn> rdns = new LdapName(ln).getRdns();
            StringBuilder out = new StringBuilder();
            if (rdns != null && (!rdns.isEmpty())) {
                for (int j=rdns.size()-1; j>=0; j--) {
                    String keyValue = rdns.get(j).toString();
                    if(keyValue.startsWith("2.5.4.5=") || keyValue.startsWith("serialnumber=") || keyValue.startsWith("SERIALNUMBER=")) {
                        out.append("SERIALNUMBER=");
                        out.append(asciiHexToString(keyValue.substring(keyValue.indexOf('=')+1)));
                        out.append(",");
                    } else {
                        out.append(keyValue);     
                        out.append(",");
                    }
                }
                String outStr = out.toString();
                if (outStr.endsWith(",")) outStr = outStr.substring(0,outStr.length()-1);
                return outStr;
            }
            return ln;
        }
        catch(Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha tratado el nombre X400 antes de aplicar un filtro: " + ln);
            Logger.getLogger("es.gob.afirma").warning("Excepcion: "+e);
            return ln;
        }
        
    }
    
}
