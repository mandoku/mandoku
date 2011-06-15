<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="xs" version="2.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:cb="http://www.cbeta.org/ns/1.0"
    
    >
    <xsl:strip-space elements="tei:app"/>
    <xsl:output encoding="UTF-8" method="text" use-character-maps="out"/>
<!--
    <xsl:include href="cbeta-app.xsl"/>
-->
    <xsl:character-map name="out">
        <xsl:output-character character="$" string="&#xa;"/>
        <xsl:output-character character="&#xe003;" string="&gt;"/>
    </xsl:character-map>  
    <!-- set to no for no default newline after lb -->
    <xsl:param name="lb">yes</xsl:param>
    <!-- master should give the reading text -->
    <xsl:param name="curwit">master</xsl:param>
    <xsl:param name="curwitlabel">【CBETA】</xsl:param>
    <!-- need to give this as parameter    -->
    <xsl:param name="baseedition">T</xsl:param>
    
    <xsl:key name="anc" match="tei:anchor" use="@xml:id"/>
    <xsl:key name="app" match="tei:app|tei:choice" use="(@from|@cb:from)"/>
    
    <xd:doc xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Feb 1, 2010</xd:p>
            <xd:p><xd:b>Author:</xd:b> chris</xd:p>
            <xd:p></xd:p>
        </xd:desc>
    </xd:doc>
    <!--
    <xsl:key name="anc" match="anchor" use="@xml:id"/>-->
    <xsl:key name="ch" match="tei:char" use="@xml:id"/>
    <!--<xsl:variable name="wits" select="//tei:listWit/tei:witness"/>-->
    
<xsl:variable name="fileheader">
    <xsl:text>#-*- mode: mandoku-view; -*-</xsl:text> 
<xsl:text>
</xsl:text> 
    <xsl:text>#+DATE: </xsl:text> 
    <xsl:choose>
        <xsl:when test="not(/tei:TEI/tei:teiHeader[1]/tei:fileDesc[1]/tei:editionStmt[1]/tei:edition[1]/tei:date[1]='')">
            <xsl:value-of select="substring(substring-after(/tei:TEI/tei:teiHeader[1]/tei:fileDesc[1]/tei:editionStmt[1]/tei:edition[1]/tei:date[1], '$Date: '), 1, 11)"/> 
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="current-dateTime()"/>
        </xsl:otherwise>
    </xsl:choose>
    <xsl:text>
</xsl:text> 
    
    <xsl:text>#+TITLE: </xsl:text> 
    <xsl:choose>
        <xsl:when test="/tei:TEI/tei:text/tei:front/tei:docTitle">
            <xsl:value-of select="/tei:TEI/tei:text/tei:front/tei:docTitle"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
        </xsl:otherwise>
    </xsl:choose> 
    <xsl:text>
#+PROPERTY: ID </xsl:text> 
    <xsl:value-of select="/tei:TEI/@xml:id"/>
    <xsl:text></xsl:text> 
    <xsl:text>
#+PROPERTY: SOURCE </xsl:text> 
    <xsl:value-of select="normalize-space(/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:bibl)"/>
    <xsl:text>
</xsl:text> 
    <xsl:text>
#+PROPERTY: BASEEDITION </xsl:text>
    <xsl:value-of select="$baseedition"/>
    <xsl:text>
#+PROPERTY: WITNESS </xsl:text>
    <xsl:value-of select="$curwitlabel"/>
    <xsl:text>
</xsl:text>      
</xsl:variable>
    
<xsl:template match="/">
   <xsl:value-of select="$fileheader"/>
<!-- the base edition is used for the pb selection, the one used in the base edition is used -->
   <xsl:variable name="firstpb" select="(//tei:pb)[1]"/>
   <xsl:variable name="firstlb" select="(//tei:lb)[1]"/>
   <xsl:if test="($firstlb &lt;&lt; $firstpb) or (not($firstpb)) ">
       <xsl:variable name="pb" select="substring($firstlb/@n, 0, 6)"/>
       <xsl:variable name="l" select="xs:integer(substring($firstlb/@n, 6))" as="xs:integer"/>
       <xsl:text disable-output-escaping="yes">#+PROPERTY: LASTPB &lt;pb:</xsl:text>
       <xsl:value-of select="$baseedition"/>
       <xsl:text>_</xsl:text>
       <xsl:value-of select="/tei:TEI/@xml:id"/>
       <xsl:text>_</xsl:text>
       <xsl:value-of select="$pb"/>
       <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
       <xsl:for-each select="1 to $l">
           <xsl:text>¶</xsl:text>
       </xsl:for-each>
   </xsl:if> 
   <xsl:apply-templates/>
</xsl:template>    

<xsl:template match="tei:milestone[@unit='juan']|tei:fw[@type='open']">
<!--    <xsl:value-of select="$fileheader"/>-->
    <xsl:text>
#+PROPERTY: JUAN </xsl:text>
    <xsl:value-of select="@n"/>
    <xsl:text>
</xsl:text> 
<xsl:apply-templates/>
</xsl:template>

 <xsl:template match="tei:pb">
 <xsl:text>
</xsl:text>
     <xsl:text disable-output-escaping="yes">&lt;pb:</xsl:text>
     <xsl:choose>
         <xsl:when test="@ed">
             <xsl:value-of select="@ed"/>
         </xsl:when>
         <xsl:otherwise>
             <xsl:text>CB</xsl:text>
         </xsl:otherwise>
     </xsl:choose>
     <xsl:text>_</xsl:text>
     <xsl:value-of select="/tei:TEI/@xml:id"/>
     <xsl:text>_</xsl:text>
     <xsl:value-of select="@n"/>
     <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
     <xsl:text></xsl:text>   
 </xsl:template>   
<xsl:template match="tei:lb[@ed='X']"/>
<xsl:template match="tei:lb|lb">
    <xsl:variable name="from_anc" select="preceding-sibling::tei:anchor[1]"/>
    <xsl:variable name="ancid" select="$from_anc/@xml:id"/>
    <xsl:variable name="targetapp" select="key('app', concat('#', $ancid))"/>
    <xsl:variable name="endid" select="if ($targetapp/@to) then $targetapp/@to else $targetapp/@cb:to"/>
    <xsl:variable name="to_anc" select="key('anc', substring-after($endid, '#'))"/>
  <xsl:choose>
    <xsl:when test="$lb='no'">
      <xsl:text>¶</xsl:text>        
    </xsl:when>
    <!-- in these cases we do not want to start a new line after the lb -->
    <xsl:when test="count(ancestor::tei:list|ancestor::tei:lg) > 0">
      <xsl:text>¶</xsl:text>        
    </xsl:when>
    <!-- need to check if we are in master mode: output as is -->  
    <xsl:when test="$curwit='master'">
      <xsl:text>¶$</xsl:text>        
    </xsl:when>  
    <!-- need to check if we are in between relevant anchors: output only if not the lem has been selected -->  
    <xsl:when test="$from_anc &lt;&lt; . and . &lt;&lt; $to_anc ">
        <xsl:variable name="check">
            <xsl:call-template name="checkapp">
                <xsl:with-param name="app" select="$targetapp"/>
            </xsl:call-template>
        </xsl:variable>
<!--        <xsl:value-of select="$check"/>-->
        <xsl:if test="$check = 'rdg'">
          <xsl:text>¶$</xsl:text>        
        </xsl:if>
    </xsl:when>  
    <xsl:otherwise>
      <xsl:text>¶$</xsl:text>        
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
<xsl:template match="tei:p">
    <!--  might include the ID here, in a property drawer?  -->
    <xsl:text>
</xsl:text>
<xsl:apply-templates/>    
</xsl:template>  

<xsl:template match="tei:milestone[@unit='p']|tei:p[@rend='inline']">
    <!--  might include the ID here, in a property drawer?  -->
    <xsl:text>

</xsl:text>
<xsl:apply-templates/>    
</xsl:template>  

<xsl:template match="tei:lg">
    <!--  might include the ID here, in a property drawer?  -->
<xsl:text>
#+BEGIN_VERSE</xsl:text>
<xsl:apply-templates/>    
<xsl:text>
#+END_VERSE</xsl:text>
</xsl:template>  

<xsl:template match="tei:back"/>    
    <xsl:template match="tei:teiHeader"/>    
    
 <xsl:template match="tei:g">
     <xsl:variable name="ref" select="substring-after(@ref, '#')"/>
     <xsl:variable name="map" select="key('ch', $ref)//tei:mapping[@type='normalized']"/>
     <xsl:variable name="comp"
         select="key('ch', $ref)//tei:charProp/tei:localName[. =
         'composition']/following-sibling::tei:value"/>
     <xsl:choose>
         <xsl:when test="$map">
             <xsl:value-of select="$map"/>
         </xsl:when>
         <xsl:otherwise>
             <xsl:value-of select="$comp"/>
         </xsl:otherwise>
     </xsl:choose>
     
 </xsl:template>   

    <xsl:template match="tei:note">
        <xsl:text>(</xsl:text>
            <xsl:apply-templates/>
        <xsl:text>)</xsl:text>
    </xsl:template>

    <xsl:template match="tei:list">
        <xsl:text>
  </xsl:text>
        <xsl:apply-templates/>
    </xsl:template>
    <!--  FIXME do something sensible for nested lists. -->
    <xsl:template match="tei:item">
        <xsl:text>
  * </xsl:text>
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="tei:table">
            <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="tei:cell">
        <xsl:apply-templates/>
        <xsl:text> |</xsl:text>
    </xsl:template>
    <xsl:template match="tei:row">  
 <xsl:text> | </xsl:text>
            <xsl:apply-templates/>
    </xsl:template>

<xsl:template match="tei:list/tei:head">
  <xsl:text> </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>$</xsl:text>
</xsl:template>

<xsl:template match="tei:div">
<xsl:variable name="pnid" select="format-number(xs:integer(@n), '#000')"/>
<xsl:text>
</xsl:text>    
    <xsl:for-each select="ancestor::tei:div|ancestor::cb:div">
        <xsl:text>*</xsl:text>
    </xsl:for-each>
    <xsl:text> </xsl:text>
    <xsl:value-of select="$pnid"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="replace(normalize-space((.//tei:persName)[1]), ' ', '')"/>
<xsl:text>
:PROPERTIES:
:PERSID: </xsl:text> 
<xsl:value-of select="concat('ZTJ_', $pnid)"/>
<xsl:text>
:JUAN: </xsl:text>
<xsl:value-of select="(preceding::tei:fw[@type='open']/@n)[last()]"/>
<xsl:text>
:END:
</xsl:text> 

<xsl:text>
</xsl:text>    
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:head">
<!-- if the file contains cb:mulu elements, they will be used to construct the headings, leaving the normal headings as plain text -->
<xsl:choose>
<xsl:when test="//cb:mulu">
<xsl:text>
</xsl:text>
<xsl:text>:PROPERTIES:
:head: </xsl:text>
<xsl:apply-templates/>    
<xsl:text>
:END:
</xsl:text>
</xsl:when>
<xsl:otherwise>
<xsl:text>
</xsl:text>    
    <xsl:for-each select="ancestor::tei:div|ancestor::cb:div">
        <xsl:text>*</xsl:text>
    </xsl:for-each>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
    <xsl:if test="@xml:id">
<xsl:text>
:PROPERTIES:
 :PERSID:</xsl:text> 
 <xsl:value-of select="@xml:id"/>
<xsl:text>
:END:
</xsl:text> 
    </xsl:if>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="cb:mulu">
<xsl:choose>
<xsl:when test="not(@level = '')">
<xsl:text>
</xsl:text>
  <xsl:for-each select="1 to xs:integer(@level)">
    <xsl:text>*</xsl:text>
  </xsl:for-each>
  <xsl:text> </xsl:text>
</xsl:when>
<xsl:otherwise>
<!-- these are for juan, ignored -->
</xsl:otherwise>
</xsl:choose>
  <xsl:apply-templates/>
</xsl:template>


    <xsl:template match="tei:l">
        <xsl:text>
&#x3000;</xsl:text>
        <xsl:apply-templates/>
    </xsl:template>
<xsl:template match="tei:title">
  <xsl:text>《</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>》</xsl:text>
</xsl:template>
<xsl:template match="tei:name|tei:persName">
  <xsl:variable name="n">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>[[name:</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>][</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>]]</xsl:text>
</xsl:template>    

<xsl:template match="tei:placeName">
  <xsl:variable name="n">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>[[place:</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>][</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>]]</xsl:text>
</xsl:template>    

<xsl:template match="tei:rs">
  <xsl:variable name="n">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>[[rs:</xsl:text>
  <xsl:value-of select="@key"/>
  <xsl:text>][</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>]]</xsl:text>
</xsl:template>    

<xsl:template match="tei:date">
  <xsl:variable name="n">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>[[date:</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>][</xsl:text>
  <xsl:value-of select="replace(normalize-space($n), ' ', '')"/>
  <xsl:text>]]</xsl:text>
</xsl:template>    

<xsl:template match="tei:rdg"/>

<xsl:template match="tei:q">
  <xsl:text>「</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>」</xsl:text>
</xsl:template>
<xsl:template match="tei:quote">
  <xsl:text>『</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>』</xsl:text>
</xsl:template>

<xsl:template match="tei:figure">
     <xsl:text disable-output-escaping="yes">&lt;img:</xsl:text>
     <xsl:value-of select="@xml:id"/>
     <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
</xsl:template>
<!-- this is the stuff added in sanskrit etc. -->
<xsl:template match="tei:*[@place='foot']|cb:*[@place='foot']|tei:*[@cb:place='foot']|cb:*[@cb:place='foot']"/>
    

<!-- variant handling, requires curwit and the keys set up above -->
    <xsl:template match="text()" mode="app">
        <!-- within an app, we want to keep the newline, which points to a <lb/> -->
        <xsl:value-of select="replace(., '&#xa;', '¶\$')"/>
    </xsl:template>
    <xsl:template match="text()">
        <xsl:choose>
            <!-- for the master version, we just take the text as given.  This should be the same as the CBETA version-->
            <xsl:when test="$curwit='master'">
                <xsl:value-of select="translate(., ' &#xa;', '')"/>
            </xsl:when>
            <xsl:when test="preceding-sibling::tei:anchor">
                <xsl:variable name="from_anc" select="preceding-sibling::tei:anchor[1]"/>
                <xsl:variable name="ancid" select="$from_anc/@xml:id"/>
                <xsl:variable name="targetapp" select="key('app', concat('#', $ancid))"/>
                <xsl:variable name="endid" select="if ($targetapp/@to) then $targetapp/@to else $targetapp/@cb:to"/>
                <xsl:variable name="to_anc" select="key('anc', substring-after($endid, '#'))"/>
                <!-- see if this node is between the beginning and end, and if we have the first one! -->
                <xsl:choose>
                    <xsl:when test="$from_anc &lt;&lt; . and . &lt;&lt; $to_anc">
<!--                        <xsl:message>ancid: <xsl:value-of select="$ancid"/> 
                            endid: <xsl:value-of select="substring-after($endid, '#')"/>
                            to_anc: <xsl:value-of select="$to_anc/@xml:id"/>
                            </xsl:message>-->
                        <xsl:if test="./preceding-sibling::tei:*[1] is $from_anc">
                            <xsl:apply-templates select="$targetapp" mode="app"/>
                        </xsl:if>    
                    </xsl:when>    
                    <xsl:otherwise>
                        <xsl:value-of select="translate(., '&#xa;', '')"/>
                        </xsl:otherwise>
                </xsl:choose>    
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="translate(., '&#xa;', '')"/>
            </xsl:otherwise>
        </xsl:choose>     
    </xsl:template>
    <xsl:template match="tei:choice" mode="app">
        <xsl:choose>
            <!-- the corrected one is only chosen for the cbeta version! -->
            <xsl:when test="contains($curwitlabel, 'CBETA')">
                <xsl:apply-templates mode="app" select="./tei:corr"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="app" select="./tei:sic"/>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    <xsl:template match="tei:app" mode="app">
        <xsl:variable name="rdgwits" select="string-join(.//tei:rdg/@wit, ' ')"/>
        <xsl:choose>
            <!-- if it is not in a rdg, we will output the lem -->
            <xsl:when test="contains($rdgwits, $curwit)">
                <xsl:apply-templates mode="app" select="./tei:rdg"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="app" select="./tei:lem"/>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    <xsl:template match="tei:rdg" mode="app">
        <!-- there might be several rdg, we need to pick the right one -->
        <xsl:if test="contains(@wit, $curwit)">
<!--            <xsl:message>
                wit: <xsl:value-of select="@wit"/>
                curwit: <xsl:value-of select="$curwit"/>
            </xsl:message>-->
            <xsl:apply-templates mode="app"/>
        </xsl:if>
    </xsl:template>
    <xsl:template match="tei:lem|tei:corr|tei:sic" mode="app">
      <xsl:apply-templates mode="app"/>
    </xsl:template>
    <xsl:template match="tei:note[@type]" mode="#all">
        <xsl:choose>
            <xsl:when test="starts-with(@type, 'cf')"/>
            <xsl:otherwise>
                <xsl:apply-templates mode="#current"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <!-- check if we are outputting a lem or a rdg -->
    <xsl:template name="checkapp">
        <xsl:param name="app"/>
        <xsl:apply-templates select="$app" mode="check"/>
    </xsl:template>
    <xsl:template match="tei:app" mode="check">
        <xsl:variable name="rdgwits" select="string-join(.//tei:rdg/@wit, ' ')"/>
        <xsl:choose>
            <xsl:when test="contains($rdgwits, $curwit)">
                <xsl:text>rdg</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>lem</xsl:text>
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
</xsl:stylesheet>
