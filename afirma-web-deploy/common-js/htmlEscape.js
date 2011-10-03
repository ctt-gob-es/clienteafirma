/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

function htmlEscape(html)
{
	var escaped= "";
	
	html = "" + html;
	
	var i, pos=0;
	for(i=0; i<html.length; i++)
	{
		if( !isAlfaNum( html.charAt(i) ) )
		{
			escaped += html.substring(pos, i);
			escaped += "&#" + html.charCodeAt(i) + ";";
			pos = i+1;
		}
	}
	escaped += html.substring(pos, html.length);

	return escaped;
}

function isAlfaNum(c)
{
	return isLetter(c) || isNumber(c) || isOtherAlphaNum(c);
}

function isOtherAlphaNum(c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			return true;
	}
	return false;
}

function isLetter(c)
{
	return (c>='a' && c<='z') || (c>='A' && c<='Z');
}

function isNumber(c)
{
	return (c>='0' && c<='9');
}