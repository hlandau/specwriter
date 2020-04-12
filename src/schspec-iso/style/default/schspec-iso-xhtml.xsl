<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:s="https://www.devever.net/ns/schspec-iso"
    xmlns:h="http://www.w3.org/1999/xhtml"
    xmlns:svg="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    version="1.0">

  <xsl:output method="xml" encoding="UTF-8" indent="no"/>

  <!-- Add XHTML superstructure. -->
  <xsl:template match="/s:top">
    <h:html lang="en" xml:lang="en" class="nojs">
      <h:head>
        <h:meta http-equiv="Content-Type" content="application/xhtml+xml; charset=utf-8"/>
        <h:meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <h:meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        <h:title><xsl:value-of select="/s:top/s:doc/s:docctl/s:docinfo/s:doctitle"/></h:title>
        <h:link rel="stylesheet" href="schspec-iso-xhtml.css"/>
      </h:head>
      <h:body>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </h:body>
    </h:html>
  </xsl:template>

  <!-- Map <doc>. -->
  <xsl:template match="s:doc">
    <xsl:copy>
      <xsl:apply-templates select="s:docctl"/>
      <h:div class="toc">
        <h:h2>Table of Contents</h:h2>
        <h:ul>
          <xsl:apply-templates mode="toc"/>
        </h:ul>
      </h:div>
      <h:main>
        <xsl:apply-templates select="s:docbody"/>
      </h:main>
    </xsl:copy>
  </xsl:template>

  <!-- Map <doctitle>. -->
  <xsl:template match="s:docinfo/s:doctitle">
    <h:h1><xsl:copy><xsl:apply-templates/></xsl:copy></h:h1>
  </xsl:template>

  <!-- Our input may have an <?xsl-stylesheet?>
    PI. Strip it off, since we instead
    reference CSS via h:link. -->
  <xsl:template match="processing-instruction('xml-stylesheet')"/>

  <!-- Make clauses referancable. -->
  <xsl:template match="s:clause | s:annex">
    <xsl:copy>
      <xsl:attribute name="id"><xsl:value-of select="@number"/></xsl:attribute>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <!-- Make term definitions referancable. -->
  <xsl:template match="s:termdef">
    <xsl:copy>
      <xsl:attribute name="id">t.<xsl:value-of select="@sym"/></xsl:attribute>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <!-- Clause references. -->
  <xsl:template match="s:iref">
    <xsl:copy>
      <xsl:variable name="x" select="@iref"/>
      <h:a><xsl:attribute name="href">#<xsl:value-of select="@iref"/></xsl:attribute>§ <xsl:value-of select="//s:clause[@id=$x]/s:hdr/s:number"/> (<xsl:value-of select="//s:clause[@id=$x]/s:hdr/s:title"/>)</h:a>
    </xsl:copy>
  </xsl:template>

  <!-- Make terms hyperlinks. -->
  <xsl:template match="s:term">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:a>
        <xsl:attribute name="href">#t.<xsl:value-of select="@sym"/></xsl:attribute>
        <xsl:apply-templates/>
      </h:a>
    </xsl:copy>
  </xsl:template>

  <!-- Map elements to XHTML equivalents: para → p -->
  <xsl:template match="s:p">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:p>
        <xsl:apply-templates/>
      </h:p>
    </xsl:copy>
  </xsl:template>

  <!-- Map elements to XHTML equivalents: ul → ul -->
  <xsl:template match="s:ul">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:ul>
        <xsl:apply-templates/>
      </h:ul>
    </xsl:copy>
  </xsl:template>

  <!-- Map elements to XHTML equivalents: ol → ol -->
  <xsl:template match="s:ol">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:ol>
        <xsl:apply-templates/>
      </h:ol>
    </xsl:copy>
  </xsl:template>

  <!-- Map elements to XHTML equivalents: li → li -->
  <xsl:template match="s:li">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:li>
        <xsl:apply-templates/>
      </h:li>
    </xsl:copy>
  </xsl:template>

  <!-- dict -->
  <!-- dice -->

  <!-- tabular -->
  <!-- tr -->
  <!-- td -->

  <!-- hexcode -->

  <!-- Make clause titles into XHTML headings. -->
  <xsl:template match="s:clause/s:hdr">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:h2><xsl:apply-templates/></h:h2>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:clause/s:clause/s:hdr">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:h3><xsl:apply-templates/></h:h3>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:clause/s:clause/s:clause/s:hdr">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:h4><xsl:apply-templates/></h:h4>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:clause/s:clause/s:clause/s:clause/s:hdr">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <h:h5><xsl:apply-templates/></h:h5>
    </xsl:copy>
  </xsl:template>

  <!-- Register blocks. -->
  <xsl:template match="s:regblock">
    <xsl:copy>
      <s:rb-titleblock>
        <h:h4>
          <xsl:apply-templates select="s:title" />
          <h:span class="lint"> (</h:span><xsl:apply-templates select="s:mnem" /><h:span class="lint">)</h:span>
        </h:h4>
      </s:rb-titleblock>
      <s:rb-body>
        <xsl:apply-templates select="s:reg" />
      </s:rb-body>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:reg">
    <xsl:copy>
      <s:rb-titleblock>
        <h:h5>
          <h:span class="lint">[</h:span>
          <xsl:apply-templates select="s:rb-offset" />
          <h:span class="lint">] </h:span>
          <xsl:apply-templates select="s:mnem" />
          <h:span class="lint"> — </h:span>
          <xsl:apply-templates select="s:title" />
        </h:h5>
      </s:rb-titleblock>
      <s:rb-diagram>
        <svg:svg width="100%" height="3em">
          <svg:defs>
            <svg:pattern id="rsvd" width="4" height="4" patternUnits="userSpaceOnUse">
              <svg:path d="M-1,1 l2,-2 M0,4 l4,-4 M3,5 l2,-2" style="stroke:#aaaaaa;stroke-width:1;"/>
            </svg:pattern>
          </svg:defs>
          <svg:rect fill="#b0b0b0" x="0" y="1em" width="100%" height="2em" style="fill:url(#rsvd);"/>
          <xsl:apply-templates select="s:field" mode="svg" />
        </svg:svg>
      </s:rb-diagram>
      <s:rb-body>
        <h:dl>
          <xsl:apply-templates select="s:desc" />
          <xsl:apply-templates select="s:field" />
        </h:dl>
      </s:rb-body>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:field">
    <xsl:copy>
      <xsl:attribute name="id">reg.<xsl:value-of select="ancestor::s:regblock/s:mnem"/>.<xsl:value-of select="../s:mnem"/>.<xsl:value-of select="s:mnem"/></xsl:attribute>
      <s:rb-titleblock>
        <h:dt>
          <xsl:apply-templates select="s:bitrange" />
          <h:span class="lint"> — </h:span>
          <xsl:apply-templates select="s:mnem" />
          <h:span class="lint">: </h:span>
          <xsl:apply-templates select="s:rb-title" />
        </h:dt>
      </s:rb-titleblock>
      <s:rb-body>
        <h:dd>
          <xsl:apply-templates select="s:rb-desc" />
        </h:dd>
      </s:rb-body>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:bitrange">
    <xsl:copy>
      <xsl:if test="s:br-single">
        <xsl:apply-templates select="s:br-single"/>
      </xsl:if>
      <xsl:if test="s:br-lo">
        <xsl:apply-templates select="s:br-lo"/><h:span class="brlint">:</h:span><xsl:apply-templates select="s:br-hi"/>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:field" mode="svg">
    <xsl:variable name="width" select="../s:rb-width"/>
    <svg:a>
      <xsl:attribute name="xlink:href">#reg.<xsl:value-of select="ancestor::s:regblock/s:mnem"/>.<xsl:value-of select="../s:mnem"/>.<xsl:value-of select="s:mnem"/></xsl:attribute>
    <svg:rect fill="#d0d0d0" stroke="#000000" y="1em" height="2em">
      <xsl:attribute name="x">
        <xsl:choose>
          <xsl:when test="s:bitrange/s:br-single">
            <xsl:value-of select="(number($width) - number(s:bitrange/s:br-single) - 1) div number($width)*100"/>%
          </xsl:when>
          <xsl:when test="s:bitrange/s:br-lo">
            <xsl:value-of select="(number($width) - number(s:bitrange/s:br-lo) - (number(s:bitrange/s:br-hi) - number(s:bitrange/s:br-lo) + 1)) div number($width)*100"/>%
          </xsl:when>
          <xsl:otherwise>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:attribute name="width">
        <xsl:choose>
          <xsl:when test="s:bitrange/s:br-single">
            <xsl:value-of select="1 div number($width)*100"/>%
          </xsl:when>
          <xsl:when test="s:bitrange/s:br-lo">
            <xsl:value-of select="(number(s:bitrange/s:br-hi) - number(s:bitrange/s:br-lo) + 1) div number($width)*100"/>%
          </xsl:when>
          <xsl:otherwise>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <svg:title><xsl:value-of select="s:mnem" /></svg:title>
    </svg:rect>
    </svg:a>
    <svg:text y="1em" style="font-size:0.7em;font-family:sans-serif;text-anchor:end;">
      <xsl:attribute name="x">
        <xsl:choose>
          <xsl:when test="s:bitrange/s:br-single">
            <xsl:value-of select="(number($width) - number(s:bitrange/s:br-single)) div number($width)*100"/>%
          </xsl:when>
          <xsl:when test="s:bitrange/s:br-lo">
            <xsl:value-of select="(number($width) - number(s:bitrange/s:br-lo)) div number($width)*100"/>%
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="s:bitrange/s:br-single">
          <xsl:value-of select="s:bitrange/s:br-single"/>
        </xsl:when>
        <xsl:when test="s:bitrange/s:br-lo">
          <xsl:value-of select="s:bitrange/s:br-lo"/>
        </xsl:when>
      </xsl:choose>
    </svg:text>
  </xsl:template>

  <!-- Figures. -->
  <xsl:template match="s:figure">
    <xsl:copy>
      <xsl:attribute name="id">figure.<xsl:value-of select="s:hdr/s:number" /></xsl:attribute>
      <xsl:apply-templates select="*[not(local-name()='hdr')]"/>
      <h:div>
        <xsl:apply-templates select="s:hdr"/>
      </h:div>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="s:fig-outputs">
    <xsl:if test="s:fig-output[1]">
      <h:object>
        <xsl:attribute name="data">
          <xsl:value-of select="s:fig-output[1]/@href"/>
        </xsl:attribute>
        <xsl:attribute name="type">
          <xsl:value-of select="s:fig-output[1]/@type"/>
        </xsl:attribute>

        <xsl:choose>
          <xsl:when test="s:fig-output[2]">
            <h:object>
              <xsl:attribute name="data">
                <xsl:value-of select="s:fig-output[2]/@href"/>
              </xsl:attribute>
              <xsl:attribute name="type">
                <xsl:value-of select="s:fig-output[2]/@type"/>
              </xsl:attribute>
              <xsl:value-of select="../s:fig-src"/>
            </h:object>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="../s:fig-src"/>
          </xsl:otherwise>
        </xsl:choose>
      </h:object>
    </xsl:if>
  </xsl:template>

  <xsl:template match="s:fig-builds"/>
  <xsl:template match="s:fig-src"/>

  <!-- Tables. -->
  <xsl:template match="s:table">
    <xsl:copy>
      <xsl:attribute name="id">table.<xsl:value-of select="s:hdr/s:number" /></xsl:attribute>
      <xsl:apply-templates select="*[not(local-name()='hdr')]"/>
      <h:div>
        <xsl:apply-templates select="s:hdr"/>
      </h:div>
    </xsl:copy>
  </xsl:template>

  <!-- Default passthrough. -->
  <xsl:template match="node() | @*">
    <xsl:copy>
      <xsl:apply-templates select="node() | @*"/>
    </xsl:copy>
  </xsl:template>

  <!-- TOC. -->
  <xsl:template match="s:clause | s:annex" mode="toc">
    <h:li>
      <xsl:attribute name="kind"><xsl:value-of select="@kind"/></xsl:attribute>
      <h:a>
        <xsl:attribute name="href">#<xsl:choose><xsl:when test="@id"><xsl:value-of select="@id"/></xsl:when><xsl:otherwise><xsl:value-of select="@number"/></xsl:otherwise></xsl:choose></xsl:attribute>
        <xsl:apply-templates select="s:hdr/*"/>
      </h:a>
      <h:ul>
        <xsl:apply-templates select="s:clause" mode="toc"/>
      </h:ul>
    </h:li>
  </xsl:template>

  <xsl:template match="s:docctl" mode="toc"/>

</xsl:stylesheet>
