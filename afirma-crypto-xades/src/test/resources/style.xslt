<?xml version="1.0"?>
<xslt:stylesheet xmlns:xslt="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xslt:template match="/">
<html>
<body>
<xslt:apply-templates select="*"/>
</body>
</html>
</xslt:template>

<xslt:template match="*">
<div style="margin-left:10px">
<xslt:text>&lt;</xslt:text>

<span style="color:red">
<xslt:value-of select="name()"/>
</span>
<xslt:for-each select="@*">
	<xslt:text> </xslt:text>
	<span style="color:green">
		<xslt:value-of select="name()"/>
	</span>
	<xslt:text>="</xslt:text>
	<span style="color:blue">
		<xslt:value-of select="."/>
	</span>
	<xslt:text>"</xslt:text>
</xslt:for-each>
<xslt:text>&gt;</xslt:text>
<xslt:choose>
	<xslt:when test="name()='script' or name()='style'">
		<pre>
			<xslt:value-of select="."/>
		</pre>
	</xslt:when>
	<xslt:otherwise>
		<xslt:apply-templates select="*|text()"/>
	</xslt:otherwise>
</xslt:choose>

<xslt:text>&lt;/</xslt:text>
<span style="color:red">
<xslt:value-of select="name()"/>
</span>
<xslt:text>&gt;</xslt:text>
</div>
</xslt:template>


</xslt:stylesheet>
