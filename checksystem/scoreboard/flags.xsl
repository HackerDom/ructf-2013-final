﻿<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" encoding="utf-8"/>
  <xsl:decimal-format name="NaN2ZeroFormat" NaN="0" />

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <style>@import "scoreboard.css";</style>
        <script type="text/javascript" src="scripts.js"></script>
        <title>RuCTF 2013 - Захваченные флаги</title>
        <meta http-equiv="Refresh" content="30"/>
      </head>
      <body onload="parse_emails()">       
        <div id="mainWrapper">
          <div id="centeredImage">
          </div>
          <div id="in">
            <table id="inTable">
              <tr valign="top">
                <td id="inSpacer1">&#160;</td>
                <td id="inContent">
                  <div>
                    <a class="menulink" href="news.html">Новости</a>
                    <a class="menulink" href="scoreboard.xml">Скорборд</a>
                    <a class="selected" href="#">Флаги</a>
<!--                <a class="menulink" href="/advisories/">Адвайзори</a>
                    <a class="menulink" href="visualizer.html">Визуализация</a> -->
                  </div>

                  <div style="text-align:right">
                    <h5>
                      Раунд #<xsl:value-of select="/servicesFlagsStolen/@round"/> (начался в <xsl:value-of select="/servicesFlagsStolen/@roundStartTimeUTC"/> UTC)
                    </h5>
                  </div>

                  <h1 id="centeredTitle">
                    Захваченные флаги
                  </h1>
                  <xsl:apply-templates select="servicesFlagsStolen"/>
                  
                  <div style="text-align:center">
                    <h5>
                      Cгенерировано <xsl:value-of select="/servicesFlagsStolen/@genTimeUTC"/> UTC
                    </h5>
                  </div>
                </td>
                <td id="inSpacer3">&#160;</td>
              </tr>
            </table>
          </div>
          <div id="footWrap"></div>
        </div>

        <div id="footer">
          <div id="footer2">
            <div id="contacts">
              По всем вопросам можно обращаться по адресу <a href="mailto:info[at]ructf.org">
                <span class="email">info[at]ructf.org</span>
              </a>
              <xsl:element name="br"/>
              Официальная рассылка соревнований: <a href="http://groups.google.com/group/ructf/">http://groups.google.com/group/ructf/</a><xsl:element name="br"/>
            </div>
            <div id="copyright">&#xA9; 2013 HackerDom</div>
          </div>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="servicesFlagsStolen">
    <table width="900" class="scoreboard" cellspacing="0" cellpadding="10">
      
      <col width="50" /><!-- лого команды -->
      <col/><!-- имя команды -->     

      <tr>
        <th>Лого  </th>
        <th>Команда</th>
        <xsl:for-each select="services/service">
          <th width="50px">
            <xsl:value-of select="@name"/>
          </th>
        </xsl:for-each>
      </tr>

      <xsl:for-each select="teams/team">
        <xsl:sort select="@name" data-type="text" order="ascending"/>
        <xsl:variable name="teamName" select="@name"/>
        <tr>
          <td>
            <xsl:element name="img">
              <xsl:attribute name="src">img/<xsl:value-of select="@name"/>.png</xsl:attribute>
              <xsl:attribute name="alt"><xsl:value-of select="@name"/></xsl:attribute>
			  <xsl:attribute name="width">50</xsl:attribute>
			  <xsl:attribute name="height">50</xsl:attribute>
            </xsl:element>            
          </td>
          <td>
            <xsl:value-of select="$teamName"/>
          </td>
          <xsl:for-each select="/servicesFlagsStolen/services/service">
            <xsl:variable name="service" select="@name"/>
            <xsl:choose>
              <xsl:when test="/servicesFlagsStolen/teams/team[@name=$teamName]/flags[@service=$service]">
                <td>
                  <xsl:value-of select="/servicesFlagsStolen/teams/team[@name=$teamName]/flags[@service=$service]/@amount"/>
                </td>
              </xsl:when>
              <xsl:otherwise>
                <td>0</td>
              </xsl:otherwise>
              
            </xsl:choose>
          </xsl:for-each>
        </tr>        
      </xsl:for-each>

      <tr>
        <th>Лого  </th>
        <th>Команда</th>
        <xsl:for-each select="services/service">
          <th>
            <xsl:value-of select="@name"/>
          </th>
        </xsl:for-each>
      </tr>    
      
    </table>    
  </xsl:template>

</xsl:stylesheet>
