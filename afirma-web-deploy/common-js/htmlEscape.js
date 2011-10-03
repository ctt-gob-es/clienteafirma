/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
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