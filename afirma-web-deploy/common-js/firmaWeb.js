/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

/*
 * - Soporta CSS en etiquetas LINK y STYLE. No soporta @import en CSS
 * - Elimina SCRIPTS y comentarios
 * - Escapa textos y valores de atributos -> Escapado XML / HTML
 */

ELEMENT 				= 1;
ATTRIBUTE				= 2;
TEXT					= 3;
CDATA					= 4;
ENTITY_REFERENCE		= 5;
PROCESSING_INSTRUCTION	= 6;
COMMENT					= 7;
DOCUMENT				= 8;
DOCUMENT_TYPE			= 9;
DOCUMENT_FRAGMENT		= 10;
NOTATION				= 11;

var alwaysClose   = new Array("script", "textarea", "title", "iframe");
var neverClose   = new Array("input", "br");
var specialAtts	  = new Array("value", "selected", "checked");
var forbiddenAtts = new Array(
	"contenteditable", "start", "loop", "maxLength", "disabled",
	"onreset", "onsubmit", "onclick", "onmousedown", "onmouseup", "onblur", "onchange", 
	"onfocus", "onkeydown", "onkeyup", "onkeypress", "onselect", "onafterupdate", "onbeforeupdate", 
	"onerrorupdate", "onabort", "onerror", "onscroll", "onbounce", "onfinish", "onstart", 
	"oncellchange", "ondataavailable", "ondatasetchanged", "ondatasetcomplete", "onrowenter", 
	"onrowexit", "onrowsdelete", "onrowsinserted", "onafterprint", "onbeforeprint", "onbeforeunload", 
	"onclose", "ondragdrop", "onhelp", "onmousemove", "onmouseout", "onmouseover", "onmove", 
	"onmovestart", "onmoveend", "ondblclick", "onactivate", "onbeforecopy", "onbeforecut", "onbeforedeactivate", 
	"onbeforeeditfocus", "onbeforepaste", "oncontextmenu", "oncontrolselect", "oncopy", "oncut",
	"ondrag", "ondrop", "ondragend", "ondragenter", "ondragleave", "ondragover", "ondragstart", 
	"onfilterchange", "onfocusin", "onfocusout", "onlosecapture", "onmouseenter", "onmouseleave",
	"onmousewheel", "onpaste", "onpropertychange", "onreadystatechange", "onresize", "onresizestart", 
	"onresizeend", "onselectstart", "onstop"
	);
var inputs = new Array("input", "textarea", "select");
var textualTags	= new Array("title", "textarea");

var fileNo=0;

function firmaWeb(element, doc)
{
	fileNo = 0;
	var html = toXMLPart(element, doc);
	html = clienteFirma.webSign(html);
	return html;
}

function toXMLPart(element, doc)
{
	fileNo = 0;
	var xml= "<html>\n";

	var styles= getStyles(doc);	

	xml+="<head>\n<style type=\"text/css\">\n"+styles+"\n</style>\n</head>\n<body disabled='disabled'"
	if(element.height)
	{
		xml += " height='"+element.height+"'";
	}
	if(element.width)
	{
		xml += " width='"+element.width+"'";
	}
	xml +=">\n";
	
	xml+= toXML(element);
	
	xml+="\n</body>\n</html>";

	return xml;
}

function toXML(element)
{
	var xml="";
	
	switch(element.nodeType)
	{
		case ELEMENT:
			if(element.nodeName.toLowerCase()!='script')
			{
				xml = toXMLElement(element);
			}
			break;
			
		case TEXT:
			xml = htmlEscape(element.nodeValue);
			break;
			
		case CDATA:
			xml = "<![CDATA["+element.nodeValue+"]]>";
			break;
			
		/* Eliminamos c?digo y comentarios */
		case PROCESSING_INSTRUCTION:
		case COMMENT:
			xml = "";
			break;
			
		default:
			xml = toXMLChildNodes(element);
			break;
	}
	
	return xml;
}

function toXMLElement(element)
{
	if(element.nodeName.charAt(0)=='/' || element.nodeName.toLowerCase()=="script")
	{
		return "";
	}
	
	var xml= "";
	if(element.nodeName.toLowerCase()=="input" && element.type.toLowerCase()=="file")
	{
		var n = fileNo++;
		xml+="<afirma type='file' href='"+escape(element.value)+"' id='afirma5file"+n+"'/>";
		//xml+="<afirma type='file' href='file://"+escape(element.value)+"' id='afirma5file"+n+"'><!--\n";
		//var fileContent = clienteFirma.getFileBase64Encoded(element.value, true);
		//if(fileContent)
		//{
		//	xml += fileContent;
		//}
		//xml+="\n--></afirma>";
		xml+="<a href='afirma:saveFile("+n+")'>"+element.value+"</a>";
		return xml;
	}
	
	xml= "<"+element.nodeName;
	
	if(!isBlank(element.height))
	{
		xml += " height='"+element.height+"'";
	}
	if(!isBlank(element.width))
	{
		xml += " width='"+element.width+"'";
	}
	if(!isBlank(element.size))
	{
		xml += " size='"+element.size+"'";
	}
	if(!isBlank(element.rows))
	{
		xml += " rows='"+element.rows+"'";
	}
	if(!isBlank(element.cols))
	{
		xml += " cols='"+element.cols+"'";
	}
	
	/* Si es textual (contiene texto) metemos el texto y cerramos el tag */
	if(isTextualTags(element.nodeName))
	{
		var text = element.text;
		if(isBlank(text))
		{
			text = htmlEscape(element.value);
		}
		xml+=">\n" + text + "</"+element.nodeName+">\n" ;
		
	}
	else
	{   /* Si tiene algun atributo especial, lo anadimos */
		var i;
		for(i=0; i<specialAtts.length; i++)
		{
			var att = eval("element."+specialAtts[i]);
			if(!isBlank(att))
			{
				xml += " "+specialAtts[i]+'="'+ htmlEscape(att) +'"';
			}
		}
		
		for(i=0; i<element.attributes.length; i++)
		{
			xml += toXMLAttribute(element.attributes[i]);
		}
		
		if(element.childNodes.length>0 && !isNeverClose(element.nodeName))
		{
			xml+=">\n";
			xml+= toXMLChildNodes(element);
			xml+="</"+element.nodeName+">\n";
		}
		else
		{
			/* Si hace falta un tag de cerrar, se siera con tag */
			if(isAlwaysClose(element.nodeName))
			{
				xml+=">\n</"+element.nodeName+">";
			}
			/* Se cierra en el propio tag de apertura */
			else if(!isNeverClose(element.nodeName))
			{
				xml+="/>\n";
			}
			else
			{
				xml+=">\n";
			}
		}
	}

	
	
	return xml;
}

function toXMLAttribute(attribute)
{
	if(!isBlank(attribute.nodeValue) && !isSpecial(attribute.nodeName) && !isForbiddenAtt(attribute.nodeName) && attribute.nodeName.charAt(0)!='{')
	{
		return " " + attribute.nodeName + '="' + htmlEscape(attribute.nodeValue) + '"';
	}
	
	return "";
}

function toXMLChildNodes(element)
{
	var xml= "";

	var i;
	for(i=0; i<element.childNodes.length; i++)
	{
		xml+= toXML(element.childNodes[i]);
	}
	
	return xml;
}

function isTextualTags(tagName)
{
	return containsElement(textualTags, tagName.toLowerCase());
}

function isAlwaysClose(tagName)
{
	return containsElement(alwaysClose,tagName.toLowerCase());
}

function isNeverClose(tagName)
{
	return containsElement(neverClose, tagName.toLowerCase());
}

function isForbiddenAtt(attName)
{
	return containsElement(forbiddenAtts, attName.toLowerCase());
}

function isSpecial(attName)
{
	return containsElement(specialAtts, attName.toLowerCase());
}

