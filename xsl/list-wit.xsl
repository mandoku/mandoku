<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl" version="2.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:cb="http://www.cbeta.org/ns/1.0"
    >
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Apr 3, 2010</xd:p>
            <xd:p><xd:b>Author:</xd:b> chris</xd:p>
            <xd:p>list the witness of the document</xd:p>
        </xd:desc>
    </xd:doc>
    <xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
    <xsl:template match="/">
        <xsl:for-each select="//tei:witness">
            <xsl:value-of select="@xml:id"/>
            <xsl:text>:</xsl:text>
            <xsl:value-of select="."/>
            <xsl:if test="position() != last()">
            <xsl:text> </xsl:text>    
            </xsl:if>    
        </xsl:for-each>    
    </xsl:template>
    
</xsl:stylesheet>
