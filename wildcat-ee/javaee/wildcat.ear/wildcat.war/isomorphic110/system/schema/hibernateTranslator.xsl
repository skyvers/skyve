<?xml version="1.0"?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
    <xsl:output 
        method="xml" 
        indent="yes"
        doctype-public="-//Hibernate/Hibernate Mapping DTD 3.0//EN http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd"
    />

    <xsl:template match="/">
        <hibernate-mapping>
        <xsl:element name="class">
            <xsl:attribute name="entity-name"><xsl:value-of select="/DataSource/@ID"/></xsl:attribute>
            <xsl:if test="/DataSource/@tableName">
                <xsl:attribute name="table"><xsl:value-of select="/DataSource/@tableName"/></xsl:attribute>
            </xsl:if>
            <xsl:if test="/DataSource/@schema">
                <xsl:attribute name="schema"><xsl:value-of select="/DataSource/@schema"/></xsl:attribute>
            </xsl:if>
            <xsl:if test="/DataSource/fields/field[@type='imageFile']">
                <xsl:if test="not(/DataSource/fields/field[@name='image_filename'])">
                    <xsl:element name="property">
                        <xsl:attribute name="name">image_filename</xsl:attribute>
                        <xsl:attribute name="type">text</xsl:attribute>
                    </xsl:element>
                </xsl:if>
                <xsl:if test="not(/DataSource/fields/field[@name='image_filesize'])">
                    <xsl:element name="property">
                        <xsl:attribute name="name">image_filesize</xsl:attribute>
                        <xsl:attribute name="type">long</xsl:attribute>
                    </xsl:element>
                </xsl:if>
                <xsl:if test="not(/DataSource/fields/field[@name='image_date_created'])">
                    <xsl:element name="property">
                        <xsl:attribute name="name">image_date_created</xsl:attribute>
                        <xsl:attribute name="type">date</xsl:attribute>
                    </xsl:element>
                </xsl:if>
            </xsl:if>

            <xsl:for-each select="/DataSource/fields/field">
                <!-- Simply exclude fields marked as customSQL: true from the mapping -->
                <xsl:if test="not(@customSQL) or @customSQL = 'false'">
                    <xsl:variable name="tagName">
                        <xsl:choose>
                            <xsl:when test="@primaryKey = 'true'">id</xsl:when>
                            <xsl:otherwise>property</xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>
                    <xsl:element name="{$tagName}">
                        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
                    <!-- We default to quotedColumnNames, so if the property is either true or not present,
                         quote the column names.
                          May 2009 - quotedColumnNames is now NOT the default, since we added 
                          the ability to coerce case in SQLTransform.  Reversing this logic. -->
                        <xsl:if test="/DataSource/@quotedColumnNames">
                            <xsl:if test="/DataSource/@quotedColumnNames = 'true'">
                                <xsl:attribute name="column">`<xsl:value-of select="@name"/>`</xsl:attribute>
                            </xsl:if>
                        </xsl:if>
    
                        <xsl:variable name="type">
                            <xsl:choose>
                                <!-- allow an explicit nativeType, passed straight through.
                                     Necessary because we otherwise default unrecognized types to
                                     string, since you are allowed to set @type to values the
                                     server doesn't understand -->
                                <xsl:when test="@nativeType">
                                    <xsl:value-of select="@nativeType"/>
                                </xsl:when>
    
                                <!-- float to double: ISC creates a Java Double for all floating
                                     point types, plus original ISC SQL layer used to create
                                     doubles for some databases -->
                                <xsl:when test="@type = 'float'">double</xsl:when>
                                <xsl:when test="@type = 'double'">double</xsl:when>
                                <xsl:when test="@type = 'decimal'">double</xsl:when>
                                
                                <!-- Changed mapping of "number" to "long" - SmartClient passes Longs and
                                     Hibernate crashes when it tries to cast them as Integers -->
                                <xsl:when test="@type = 'long'">long</xsl:when>
                                <xsl:when test="@type = 'number'">long</xsl:when>
    
                                <!-- Changed mapping of "int" and "integer" to "long" - SmartClient 
                                     seems to pass Longs for all integer-type variables, and Hibernate
                                     can't cope -->
                                <xsl:when test="@type = 'int'">long</xsl:when>
                                <xsl:when test="@type = 'integer'">long</xsl:when>
                                <xsl:when test="@type = 'intEnum'">integer</xsl:when>
    
                                <xsl:when test="@type = 'date'">date</xsl:when>
                                <xsl:when test="@type = 'datetime'">timestamp</xsl:when>
                                <xsl:when test="@type = 'creatorTimestamp'">timestamp</xsl:when>
                                <xsl:when test="@type = 'modifierTimestamp'">timestamp</xsl:when>
                                <xsl:when test="@type = 'time'">time</xsl:when>
    
                                <xsl:when test="@type = 'binary'">binary</xsl:when>
                                <xsl:when test="@type = 'blob'">binary</xsl:when>
    
                                <xsl:when test="@type = 'boolean'">boolean</xsl:when>
    
                                <xsl:when test="@type = 'sequence'">long</xsl:when>
    
                                <xsl:when test="@type = 'enum'">string</xsl:when>
                                <xsl:when test="@type = 'string'">
                                    <xsl:choose>
                                        <xsl:when test="@length &gt; 1999">text</xsl:when>
                                        <xsl:otherwise>string</xsl:otherwise>
                                    </xsl:choose>
                                </xsl:when>
                                <xsl:when test="@type = 'text'">
                                    <xsl:choose>
                                        <xsl:when test="@length &gt; 1999">text</xsl:when>
                                        <xsl:otherwise>string</xsl:otherwise>
                                    </xsl:choose>
                                </xsl:when>
				<!-- imageType -->
				<xsl:when test="@type = 'imageFile'">com.isomorphic.hibernate.BlobUserType</xsl:when>
    
                                <!-- otherwise default to string -->
                                <xsl:otherwise>
                                    <xsl:choose>
                                        <xsl:when test="@length &gt; 1999">text</xsl:when>
                                        <xsl:otherwise>string</xsl:otherwise>
                                    </xsl:choose>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
        
                        <xsl:attribute name="type"><xsl:value-of select="$type"/></xsl:attribute>
    
                        <xsl:if test="@primaryKey = 'true'">
                            <xsl:choose>
                                <xsl:when test="@type = 'sequence'">
                                    <generator class="native">
                                    <xsl:choose>
                                        <xsl:when test="@sequenceName">
                                            <param name="sequence"><xsl:value-of select="@sequenceName"/></param>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <param name="sequence"><xsl:value-of select="/DataSource/@tableName"/>_<xsl:value-of select="@name"/></param>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                    </generator>    
                                </xsl:when>
                                <xsl:otherwise>
                                    <generator class="assigned"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:if>
                    </xsl:element>
                </xsl:if>
            </xsl:for-each>
        </xsl:element>
        </hibernate-mapping>
    </xsl:template>
</xsl:stylesheet>
