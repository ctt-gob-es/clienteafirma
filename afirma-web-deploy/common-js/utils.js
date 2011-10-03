function isBlank(x)
{
	return (x==undefined || x=='' || x==null);
}

function isEmpty(x)
{
	return isBlank(x) || x.length<1;
}

function toHex(n, digits)
{
	if(!digits)
	{
		digits = 2;
	}
	var hex = new Number(n).toString(16);
	while(hex.length < digits)
	{
		hex = "0" + hex;
	}
	
	return hex;
}

function containsElement(array, elem)
{
	var containsElement = false;

	if(!isEmpty(array))
	{
		var i;
		for(i=0; i<array.length && !containsElement; i++)
		{
			containsElement= (array[i] == elem);
		}
	}
	
	return containsElement;
}

function escaparExpReg(expresion) 
{
	var escaped= "";
	expresion = "" + expresion;
	
	var i, pos=0;
	for(i=0; i<expresion.length; i++)
	{
		if( isSpecialCharacter(expresion.charAt(i)) )
		{
			escaped += expresion.substring(pos, i);
			escaped += "\\" + expresion.charAt(i);
			pos = i+1;
		}
	}
	escaped += expresion.substring(pos, expresion.length);

	return escaped;
}

function isSpecialCharacter(c)
{
	switch (c)
	{
		case '.':
		case '*':
		case '+':
		case '?':
		case '"':
		case '^':
		case '$':
		case '|':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		case '\\':
			return true;
	}
	return false;
}
