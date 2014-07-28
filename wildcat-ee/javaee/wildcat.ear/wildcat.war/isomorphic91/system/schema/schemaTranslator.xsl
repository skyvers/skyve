<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:isc="http://smartclient.com/XSExtensions" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:mime="http://schemas.xmlsoap.org/wsdl/mime/" version="1.0">

   

   <xsl:output method="xml" indent="yes"/>

   
   <xsl:template match="text()"/>
   <xsl:template match="text()" mode="schemaSet"/>

   <xsl:param name="startId" select="0"/>

   <xsl:param name="debug" select="0"/>

   

   <xsl:template match="/">
      
      <isomorphicXML>
          <xsl:apply-templates/>
      </isomorphicXML>
   </xsl:template>

   
   
   <xsl:template name="schemaSet" match="//xs:schema">
      <SchemaSet schemaNamespace="{@targetNamespace}" qualifyAll="{@elementFormDefault = 'qualified'}">
          <xsl:if test="//wsdl:definitions">
              <xsl:attribute name="serviceNamespace">
                  <xsl:value-of select="//wsdl:definitions/@targetNamespace"/>
              </xsl:attribute>
          </xsl:if>

          
          <schemaImports>
          <xsl:for-each select=".//xs:import[parent::xs:schema][@namespace]">
              <schemaImport namespace="{@targetNamespace | @namespace}" location="{@schemaLocation}"/>
          </xsl:for-each>
          </schemaImports>

          <schema>
              

              
              <xsl:for-each select=".//xs:element[@name and xs:complexType]/xs:complexType">
                  <xsl:call-template name="makeDataSource">
                     <xsl:with-param name="dsName" select="../@name"/>
                  </xsl:call-template>
              </xsl:for-each>

              
              <xsl:for-each select=".//xs:element[not(@name) and xs:complexType]/xs:complexType">
                  <xsl:call-template name="makeDataSource">
                     <xsl:with-param name="dsName" select="concat('autoDS',$startId + position())"/>
                  </xsl:call-template>
              </xsl:for-each>

              <xsl:apply-templates/>
          </schema>
      </SchemaSet>
   </xsl:template>

   
   

   <xsl:template name="webService" match="/wsdl:definitions">

        
        <xsl:apply-templates/>
    
        
            
        
        <WebService dataURL="{.//wsdl:service//soap:address/@location}" name="{.//wsdl:service[//soap:address]/@name}" serviceNamespace="{@targetNamespace}">

            <xsl:attribute name="soapStyle">
                <xsl:value-of select="//wsdl:binding/soap:binding/@style"/>
            </xsl:attribute>

            
            <wsdlImports>
            <xsl:for-each select="//wsdl:import[@namespace]">
                <wsdlImport namespace="{@namespace}" location="{@location}"/>
            </xsl:for-each>
            </wsdlImports>

            
            <schemaImports>
            <xsl:for-each select="//xs:schema[@targetNamespace] |                                   //xs:import[parent::xs:schema][@namespace]">
                <schemaImport namespace="{@targetNamespace | @namespace}" location="{@schemaLocation}"/>
            </xsl:for-each>
            </schemaImports>

            
            <bindings>
            <xsl:for-each select=".//wsdl:binding[soap:binding]">

                <xsl:variable name="portTypeName">
                   <xsl:call-template name="getLocalName">
                      <xsl:with-param name="fullName" select="@type"/>
                   </xsl:call-template>
                </xsl:variable>

                <binding name="{@name}" portTypeName="{$portTypeName}">
                    <xsl:for-each select="./wsdl:operation">
                        <operation name="{@name}" soapAction="{./soap:operation/@soapAction}">

                           <xsl:if test="wsdl:input/soap:body/@namespace">
                              <xsl:attribute name="inputNamespace">
                                  <xsl:value-of select="wsdl:input/soap:body/@namespace"/>
                              </xsl:attribute>
                           </xsl:if>

                           <xsl:if test="wsdl:output/soap:body/@namespace">
                               <xsl:attribute name="outputNamespace">
                                   <xsl:value-of select="wsdl:output/soap:body/@namespace"/>
                               </xsl:attribute>
                           </xsl:if>

                           
                           <xsl:attribute name="inputEncoding">
                                <xsl:value-of select="wsdl:input/soap:body/@use"/>
                           </xsl:attribute>
                           <xsl:attribute name="outputEncoding">
                               <xsl:value-of select="wsdl:output/soap:body/@use"/>
                           </xsl:attribute>

                           
                           <xsl:if test="soap:operation/@style">
                               <xsl:attribute name="soapStyle">
                                   <xsl:value-of select="soap:operation/@style"/>
                               </xsl:attribute>
                           </xsl:if>

                           
                           <xsl:attribute name="inputParts">
                               <xsl:value-of select="wsdl:input//soap:body/@parts"/>
                           </xsl:attribute>
                           <xsl:attribute name="outputParts">
                               <xsl:value-of select="wsdl:output//soap:body/@parts"/>
                           </xsl:attribute>

                           
                           <xsl:for-each select="wsdl:input/soap:header">
                              <inputHeaders encoding="{@use}" part="{@part}">
                                 <xsl:attribute name="message">
                                    <xsl:call-template name="getLocalName">
                                        <xsl:with-param name="fullName" select="@message"/>
                                    </xsl:call-template>
                                 </xsl:attribute>
                              </inputHeaders>
                           </xsl:for-each>
                           <xsl:for-each select="wsdl:output/soap:header">
                              <outputHeaders encoding="{@use}" part="{@part}">
                                 <xsl:attribute name="message">
                                    <xsl:call-template name="getLocalName">
                                        <xsl:with-param name="fullName" select="@message"/>
                                    </xsl:call-template>
                                 </xsl:attribute>
                              </outputHeaders>
                           </xsl:for-each>
                        </operation>
                    </xsl:for-each>
                </binding>
            </xsl:for-each>
            </bindings>

            <portTypes>
            <xsl:for-each select=".//wsdl:portType">
                 
                <portType portTypeName="{@name}">
                <xsl:for-each select="./wsdl:operation">
                    <operation name="{@name}">
                    <xsl:attribute name="inputMessage">
                       <xsl:call-template name="getLocalName">
                          <xsl:with-param name="fullName" select="wsdl:input/@message"/>
                       </xsl:call-template>
                    </xsl:attribute>
                    <xsl:attribute name="outputMessage">
                       <xsl:call-template name="getLocalName">
                          <xsl:with-param name="fullName" select="wsdl:output/@message"/>
                       </xsl:call-template>
                    </xsl:attribute>
                    </operation>
                </xsl:for-each>
                </portType>
            </xsl:for-each>
            </portTypes>

            <messages>
   
               <xsl:for-each select="//wsdl:message">
                   <xsl:variable name="messageName" select="@name"/>

                   <xsl:call-template name="makeDataSource">
                       <xsl:with-param name="dsName" select="concat('message:',@name)"/>
                   </xsl:call-template>
               </xsl:for-each>
            </messages>

        </WebService>
   </xsl:template>

   
   

   
   <xsl:template name="getAnonymousTypeId">
        <xsl:param name="typeElement" select="current()"/>
        <xsl:for-each select="//xs:element[not(@name) and xs:complexType]">
            <xsl:if test="$typeElement=node()">
                 <xsl:value-of select="concat('autoDS',$startId + position())"/>
            </xsl:if>
        </xsl:for-each>
   </xsl:template>

   
   <xsl:template match="//xs:complexType[@name]">
       <xsl:call-template name="makeDataSource">
           <xsl:with-param name="dsName" select="@name"/>
       </xsl:call-template>
   </xsl:template>

   
   <xsl:template match="//xs:element[@name and @type and parent::xs:schema]">
       
       <xsl:variable name="typeName">
           <xsl:call-template name="getLocalName">
               <xsl:with-param name="fullName" select="@type"/>
           </xsl:call-template>
       </xsl:variable>
 
       <xsl:choose>
         
         <xsl:when test="//xs:complexType[@name = $typeName]">
           
           <xsl:call-template name="makeDataSource">
              <xsl:with-param name="dsName" select="@name"/>
              <xsl:with-param name="inheritsFrom" select="$typeName"/>
              <xsl:with-param name="isGlobal" select="true()"/>
              <xsl:with-param name="xmlSource" select="'XSElement'"/>
           </xsl:call-template>
         </xsl:when>

         
         <xsl:when test="//xs:simpleType[@name = $typeName]">
             <SimpleType name="{@name}" xmlSource="XSElement" inheritsFrom="{$typeName}"/>
         </xsl:when>

         
         <xsl:otherwise> 
           <SimpleType name="{@name}" xmlSource="XSElement" inheritsFrom="{$typeName}" xsImportedType="true"/>
         </xsl:otherwise>
       </xsl:choose>
   </xsl:template>

   
   <xsl:template name="makeDataSource">
      <xsl:param name="dsName"/>
      
      <xsl:param name="inheritsFrom"/>
      <xsl:param name="isGlobal"/>
      <xsl:param name="xmlSource"/>

      <xsl:variable name="tagName">
          <xsl:choose>
             <xsl:when test="$xmlSource"><xsl:value-of select="$xmlSource"/></xsl:when>
             <xsl:when test="../wsdl:message">WSDLMessage</xsl:when>
             <xsl:when test="../../xs:element">XSElement</xsl:when>
             <xsl:otherwise>XSComplexType</xsl:otherwise>
          </xsl:choose>
      </xsl:variable>

      <xsl:element name="{$tagName}">
          <xsl:attribute name="ID"><xsl:value-of select="$dsName"/></xsl:attribute>
 
          <xsl:if test="$inheritsFrom">
              <xsl:attribute name="inheritsFrom">
                  <xsl:value-of select="$inheritsFrom"/>
              </xsl:attribute>
          </xsl:if>

          
          <xsl:variable name="qualifyAll" select="ancestor::xs:schema/@elementFormDefault='qualified'"/>

          
          <xsl:if test="$qualifyAll or                          $isGlobal or (../../xs:element and ../../../xs:schema)">       
             <xsl:attribute name="mustQualify">true</xsl:attribute>
          </xsl:if>

          
          <xsl:variable name="thisDefinition" select="."/>
          <xsl:variable name="immediateChildDefs" select="                 descendant::xs:element[ancestor::xs:complexType[1]=$thisDefinition]           "/>
          <xsl:variable name="immediateChildAttributes" select="                 descendant::xs:attribute[ancestor::xs:complexType[1]=$thisDefinition]           "/>

          
          <xsl:if test="@abtract">
             <xsl:attribute name="xsdAbstract">true</xsl:attribute>
          </xsl:if>

          
          <xsl:if test="descendant::xs:any[ancestor::xs:complexType[1]=$thisDefinition]">
             <xsl:attribute name="xsdAnyElement">true</xsl:attribute>
          </xsl:if>

          
          <xsl:if test="xs:complexContent/xs:extension[@base!='anyType'] |                         xs:complexContent/xs:restriction[@base!='anyType']">
                <xsl:attribute name="inheritsFrom">
                    <xsl:call-template name="getLocalName">
                        <xsl:with-param name="fullName" select="xs:complexContent/xs:extension/@base |                                                 xs:complexContent/xs:restriction/@base"/>
                    </xsl:call-template>
                </xsl:attribute>
                
                <xsl:if test="xs:complexContent/xs:restriction">
                    <xsl:attribute name="restrictToLocalFields">true</xsl:attribute>
                </xsl:if>
                
                <xsl:if test="xs:complexContent/xs:extension">
                    <xsl:attribute name="useParentFieldOrder">true</xsl:attribute>
                </xsl:if>
          </xsl:if>

          <xsl:if test="@abstract='true'">
              <xsl:attribute name="xmlAbstract">true</xsl:attribute>
          </xsl:if>

          
          <xsl:if test="xs:simpleContent">
              <xsl:attribute name="xsdSimpleContent">true</xsl:attribute>
              <xsl:attribute name="textContentProperty">xmlTextContent</xsl:attribute>
          </xsl:if>

          <fields>
                <xsl:for-each select="$immediateChildAttributes">
                    <xsl:call-template name="attributeToField"/>
                </xsl:for-each>

                <xsl:for-each select="$immediateChildDefs">
                     <xsl:call-template name="elementToField"/>
                </xsl:for-each>

                
                <xsl:for-each select="descendant::wsdl:part">
                     <xsl:call-template name="elementToField"/>
                </xsl:for-each>

                
                <xsl:if test="xs:simpleContent/xs:extension">
                    <xsl:call-template name="elementToField">
                        <xsl:with-param name="fieldName" select="'xmlTextContent'"/>
                        <xsl:with-param name="xmlRequired" select="'false'"/>
                    </xsl:call-template>
                </xsl:if>

          </fields>

      </xsl:element>

   </xsl:template>


   <xsl:template name="elementToField">
      

      <xsl:param name="fieldName" select="@name"/>
      
      
      <xsl:param name="xmlRequired" select="                     (count(@minOccurs)=0 or @minOccurs &gt; 0) and not(../../xs:choice)"/>

      
      <xsl:variable name="inapplicable" select="@maxOccurs=0"/>

      <xsl:variable name="nillable" select="@nillable='true'"/>

      
      <xsl:variable name="useMaxOccurs" select="@maxOccurs &gt; 1 or @maxOccurs = 'unbounded'"/>

      
      <xsl:variable name="useMinOccurs" select="@minOccurs != 1"/>

      <xsl:variable name="qualify" select="@form = 'qualified'"/>

      <xsl:choose>

         
         <xsl:when test="xs:complexType">
             <field name="{@name}" xmlRequired="{$xmlRequired}">

                <xsl:if test="$debug">
                    <xsl:attribute name="xmlSource">inlineComplexType</xsl:attribute>
                </xsl:if>

                <xsl:if test="$inapplicable">
                    <xsl:attribute name="inapplicable">true</xsl:attribute>
                </xsl:if>

                <xsl:if test="$nillable">
                    <xsl:attribute name="nillable">true</xsl:attribute>
                </xsl:if>

                <xsl:if test="$qualify">
                    <xsl:attribute name="mustQualify">true</xsl:attribute>
                </xsl:if>

                <xsl:if test="$useMaxOccurs">
                   <xsl:attribute name="xmlMaxOccurs">
                       <xsl:value-of select="@maxOccurs"/>
                   </xsl:attribute>
                </xsl:if>

                <xsl:if test="$useMinOccurs">
                   <xsl:attribute name="xmlMinOccurs">
                       <xsl:value-of select="@minOccurs"/>
                   </xsl:attribute>
                </xsl:if>

                <xsl:attribute name="type">
                   
                   <xsl:choose>
                      <xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when>
                      <xsl:otherwise>
                         <xsl:call-template name="getAnonymousTypeId">
                            <xsl:with-param name="typeElement" select="xs:complexType"/>
                         </xsl:call-template>
                      </xsl:otherwise>
                   </xsl:choose>
                </xsl:attribute>
              </field>
         </xsl:when>

         
         <xsl:when test="@type">
            
            <xsl:variable name="typeName">
                <xsl:call-template name="getLocalName">
                    <xsl:with-param name="fullName" select="@type"/>
                </xsl:call-template>
            </xsl:variable>

            


            
            <xsl:variable name="complexTypeDef" select="//xs:complexType[@name = $typeName]"/>
            <xsl:if test="count($complexTypeDef) = 0">
                <field name="{@name}" xmlRequired="{$xmlRequired}">

                    <xsl:if test="$debug">
                        <xsl:attribute name="xmlSource">typeAttrOfSimpleType</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$inapplicable">
                        <xsl:attribute name="inapplicable">true</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$nillable">
                        <xsl:attribute name="nillable">true</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$qualify">
                        <xsl:attribute name="mustQualify">true</xsl:attribute>
                    </xsl:if>

                    <xsl:if test="$useMaxOccurs">
                       <xsl:attribute name="xmlMaxOccurs">
                           <xsl:value-of select="@maxOccurs"/>
                       </xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$useMinOccurs">
                       <xsl:attribute name="xmlMinOccurs">
                           <xsl:value-of select="@minOccurs"/>
                       </xsl:attribute>
                    </xsl:if>

                    <xsl:if test="@isc:title">
                        <xsl:attribute name="title">
                            <xsl:value-of select="@isc:title"/>
                        </xsl:attribute>
                    </xsl:if>

                    
                    <xsl:attribute name="type">
                         <xsl:call-template name="getISCType">
                            <xsl:with-param name="type" select="@type"/>
                         </xsl:call-template>
                    </xsl:attribute>

                    
                    <xsl:if test="local-name()='part'">
                        <xsl:attribute name="xmlRequired">true</xsl:attribute>
                    </xsl:if>
                </field>
            </xsl:if>

            
            
            <xsl:if test="$complexTypeDef">
                <field name="{@name}" type="{$typeName}" xmlRequired="{$xmlRequired}">

                    <xsl:if test="$debug">
                        <xsl:attribute name="xmlSource">typeAttrOfComplexType</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$inapplicable">
                        <xsl:attribute name="inapplicable">true</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$nillable">
                        <xsl:attribute name="nillable">true</xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$qualify">
                        <xsl:attribute name="mustQualify">true</xsl:attribute>
                    </xsl:if>

                    <xsl:if test="$useMaxOccurs">
                       <xsl:attribute name="xmlMaxOccurs">
                           <xsl:value-of select="@maxOccurs"/>
                       </xsl:attribute>
                    </xsl:if>
                    <xsl:if test="$useMinOccurs">
                       <xsl:attribute name="xmlMinOccurs">
                           <xsl:value-of select="@minOccurs"/>
                       </xsl:attribute>
                    </xsl:if>

                    
                    <xsl:if test="                         contains($complexTypeDef/xs:complexContent/xs:restriction/@base,'Array')">
                       <xsl:attribute name="multiple">true</xsl:attribute>
                       <xsl:attribute name="type">
                          <xsl:call-template name="getISCType">
                             <xsl:with-param name="type" select="                                    substring-before(//@wsdl:arrayType,'[]')"/>
                          </xsl:call-template>
                       </xsl:attribute>
                    </xsl:if>

                </field>
            </xsl:if>
         </xsl:when>

         
         
         <xsl:when test="@ref or @element">
             <xsl:variable name="ref">
                <xsl:if test="local-name()='part'">
                   <xsl:call-template name="getLocalName">
                      <xsl:with-param name="fullName" select="@element"/>
                   </xsl:call-template>
                </xsl:if>
                <xsl:if test="local-name()='element'">
                   <xsl:value-of select="@ref"/>
                </xsl:if>
             </xsl:variable>

             
             <xsl:variable name="partName">
                <xsl:if test="local-name()='part'">
                   <xsl:value-of select="@name"/>
                </xsl:if>
             </xsl:variable>

             
             <xsl:variable name="refElement" select="//xs:element[@name = $ref and ../../xs:schema]"/>

             

             <xsl:choose>
        
                
                <xsl:when test="$refElement/xs:complexType">
                        <field name="{$refElement/@name}" type="{$refElement/@name}" xmlRequired="{$xmlRequired}" xsElementRef="true">
                            <xsl:if test="$debug">
                                <xsl:attribute name="xmlSource">refToAnonComplexType</xsl:attribute>
                            </xsl:if>

                            <xsl:if test="$inapplicable">
                                <xsl:attribute name="inapplicable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$nillable">
                                <xsl:attribute name="nillable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$qualify">
                                <xsl:attribute name="mustQualify">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMaxOccurs">
                               <xsl:attribute name="xmlMaxOccurs">
                                   <xsl:value-of select="@maxOccurs"/>
                               </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMinOccurs">
                               <xsl:attribute name="xmlMinOccurs">
                                   <xsl:value-of select="@minOccurs"/>
                               </xsl:attribute>
                            </xsl:if>

                            <xsl:if test="$partName">
                                <xsl:attribute name="partName">
                                    <xsl:value-of select="$partName"/>
                                </xsl:attribute>
                            </xsl:if>
                        </field>
                </xsl:when>

                <xsl:when test="$refElement/@type">
                    
                    <xsl:variable name="typeName">
                       <xsl:call-template name="getLocalName">
                          <xsl:with-param name="fullName" select="$refElement/@type"/>
                       </xsl:call-template>
                    </xsl:variable>

                    

                    
                    <xsl:if test="$typeName = //xs:complexType/@name">
                        <field name="{$refElement/@name}" type="{$refElement/@name}" xmlRequired="{$xmlRequired}" xsElementRef="true">
                            <xsl:if test="$debug">
                                <xsl:attribute name="xmlSource">refToPublicComplexType</xsl:attribute>
                            </xsl:if>

                            <xsl:if test="$inapplicable">
                                <xsl:attribute name="inapplicable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$nillable">
                                <xsl:attribute name="nillable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$qualify">
                                <xsl:attribute name="mustQualify">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMaxOccurs">
                               <xsl:attribute name="xmlMaxOccurs">
                                   <xsl:value-of select="@maxOccurs"/>
                               </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMinOccurs">
                               <xsl:attribute name="xmlMinOccurs">
                                   <xsl:value-of select="@minOccurs"/>
                               </xsl:attribute>
                            </xsl:if>

                            <xsl:if test="$partName">
                                <xsl:attribute name="partName">
                                    <xsl:value-of select="$partName"/>
                                </xsl:attribute>
                            </xsl:if>
                        </field>
                    </xsl:if>

                    
                    <xsl:if test="count(//xs:complexType[@name = $typeName]) = 0">
                        <field xmlRequired="{$xmlRequired}" xsElementRef="true">
                            <xsl:if test="$debug">
                                <xsl:attribute name="xmlSource">refToSimpleType</xsl:attribute>
                            </xsl:if>
                            <xsl:attribute name="name">
                                <xsl:if test="@ref"><xsl:value-of select="@ref"/></xsl:if>
                                <xsl:if test="@element">
                                    <xsl:value-of select="$refElement/@name"/>
                                </xsl:if>
                            </xsl:attribute>
                            <xsl:attribute name="type">
                                <xsl:call-template name="getISCType">
                                    <xsl:with-param name="type" select="$refElement/@type"/>
                                </xsl:call-template>
                            </xsl:attribute>

                            <xsl:if test="$inapplicable">
                                <xsl:attribute name="inapplicable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$nillable">
                                <xsl:attribute name="nillable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$qualify">
                                <xsl:attribute name="mustQualify">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMaxOccurs">
                               <xsl:attribute name="xmlMaxOccurs">
                                   <xsl:value-of select="@maxOccurs"/>
                               </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMinOccurs">
                               <xsl:attribute name="xmlMinOccurs">
                                   <xsl:value-of select="@minOccurs"/>
                               </xsl:attribute>
                            </xsl:if>

                            <xsl:if test="@isc:title">
                                <xsl:attribute name="title">
                                    <xsl:value-of select="@isc:title"/>
                                </xsl:attribute>
                            </xsl:if>
                            
                            <xsl:if test="local-name()='part'">
                                <xsl:attribute name="xmlRequired">true</xsl:attribute>
                                
                                <xsl:attribute name="partNamespace">
                                    <xsl:value-of select="                                             $refElement/../../xs:schema/@targetNamespace"/>
                                </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$partName">
                                <xsl:attribute name="partName">
                                    <xsl:value-of select="$partName"/>
                                </xsl:attribute>
                            </xsl:if>
                        </field>
                    </xsl:if>
                </xsl:when>

                <xsl:when test="$refElement/xs:simpleType">
                    

                    
                    <xsl:variable name="partNamespace">
                        <xsl:if test="local-name()='part'">
                              <xsl:value-of select="                                         $refElement/../../xs:schema/@targetNamespace"/>
                        </xsl:if>
                    </xsl:variable>
    
                    
                    <xsl:for-each select="$refElement/xs:simpleType">
                        
                        <field name="{$ref}" xmlRequired="{$xmlRequired}" xsElementRef="true">
                            <xsl:if test="$refElement/../../xs:schema">
                                <xsl:attribute name="mustQualify">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$partNamespace">
                                <xsl:attribute name="partNamespace">
                                    <xsl:value-of select="$partNamespace"/>
                                </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$partName">
                                <xsl:attribute name="partName">
                                    <xsl:value-of select="$partName"/>
                                </xsl:attribute>
                            </xsl:if>

                            <xsl:if test="$inapplicable">
                                <xsl:attribute name="inapplicable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$nillable">
                                <xsl:attribute name="nillable">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$qualify">
                                <xsl:attribute name="mustQualify">true</xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMaxOccurs">
                               <xsl:attribute name="xmlMaxOccurs">
                                   <xsl:value-of select="@maxOccurs"/>
                               </xsl:attribute>
                            </xsl:if>
                            <xsl:if test="$useMinOccurs">
                               <xsl:attribute name="xmlMinOccurs">
                                   <xsl:value-of select="@minOccurs"/>
                               </xsl:attribute>
                            </xsl:if>

                            <xsl:if test="@isc:title">
                                <xsl:attribute name="title">
                                    <xsl:value-of select="@isc:title"/>
                                </xsl:attribute>
                            </xsl:if>

                            
                            <xsl:call-template name="getISCTypeAttribute">
                               <xsl:with-param name="type" select="xs:restriction/@base | xs:list/@itemType"/>
                            </xsl:call-template>
                            <xsl:call-template name="simpleTypeContents"/>
                        </field>
                    </xsl:for-each>
                </xsl:when>

                <xsl:otherwise>
                    
                    <xsl:variable name="refLocal">
                        <xsl:call-template name="getISCType">
                            <xsl:with-param name="type" select="$ref"/>
                        </xsl:call-template>
                    </xsl:variable>
                    <field name="{$refLocal}" type="{$refLocal}" mustQualify="true" xmlRequired="{$xmlRequired}" xsElementRef="true">

                        <xsl:if test="$inapplicable">
                            <xsl:attribute name="inapplicable">true</xsl:attribute>
                        </xsl:if>
                        <xsl:if test="$nillable">
                            <xsl:attribute name="nillable">true</xsl:attribute>
                        </xsl:if>
                        <xsl:if test="$qualify">
                            <xsl:attribute name="mustQualify">true</xsl:attribute>
                        </xsl:if>
                        <xsl:if test="$useMaxOccurs">
                           <xsl:attribute name="xmlMaxOccurs">
                              <xsl:value-of select="@maxOccurs"/>
                           </xsl:attribute>
                        </xsl:if>
                        <xsl:if test="$useMinOccurs">
                           <xsl:attribute name="xmlMinOccurs">
                               <xsl:value-of select="@minOccurs"/>
                           </xsl:attribute>
                        </xsl:if>

                        <xsl:if test="$partName">
                            <xsl:attribute name="partName">
                                <xsl:value-of select="$partName"/>
                            </xsl:attribute>
                        </xsl:if>
                    </field>
                </xsl:otherwise>
             </xsl:choose>
         </xsl:when>

         <xsl:when test="xs:simpleType|xs:simpleContent">
             
             <field name="{$fieldName}" xmlRequired="{$xmlRequired}">
                 <xsl:variable name="type">
                    <xsl:choose>
                       <xsl:when test="                               not (xs:simpleType/xs:restriction/@base |                                    xs:simpleContent/xs:restriction/@base |                                    xs:simpleContent/xs:extension/@base)">string</xsl:when>
                       <xsl:otherwise>
                           <xsl:value-of select="xs:simpleType/xs:restriction/@base |                                                  xs:simpleContent/xs:restriction/@base |                                                  xs:simpleContent/xs:extension/@base"/>
                       </xsl:otherwise>
                    </xsl:choose>
                 </xsl:variable>
                 
                
                 <xsl:call-template name="getISCTypeAttribute">
                    <xsl:with-param name="type" select="$type"/>
                 </xsl:call-template>

                 <xsl:if test="$inapplicable">
                     <xsl:attribute name="inapplicable">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$nillable">
                     <xsl:attribute name="nillable">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$qualify">
                     <xsl:attribute name="mustQualify">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$useMaxOccurs">
                   <xsl:attribute name="xmlMaxOccurs">
                       <xsl:value-of select="@maxOccurs"/>
                   </xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$useMinOccurs">
                    <xsl:attribute name="xmlMinOccurs">
                        <xsl:value-of select="@minOccurs"/>
                    </xsl:attribute>
                 </xsl:if>

                 <xsl:for-each select="xs:simpleType | xs:simpleContent">
                     <xsl:call-template name="simpleTypeContents"/>
                 </xsl:for-each>
             </field>
         </xsl:when>

         
         <xsl:otherwise>
             <field name="{$fieldName}" xmlRequired="{$xmlRequired}" type="xsd:any">
                 <xsl:if test="$inapplicable">
                     <xsl:attribute name="inapplicable">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$nillable">
                     <xsl:attribute name="nillable">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$qualify">
                     <xsl:attribute name="mustQualify">true</xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$useMaxOccurs">
                   <xsl:attribute name="xmlMaxOccurs">
                       <xsl:value-of select="@maxOccurs"/>
                   </xsl:attribute>
                 </xsl:if>
                 <xsl:if test="$useMinOccurs">
                    <xsl:attribute name="xmlMinOccurs">
                        <xsl:value-of select="@minOccurs"/>
                    </xsl:attribute>
                 </xsl:if>
             </field>
         </xsl:otherwise>

      </xsl:choose>

   </xsl:template>

   <xsl:template name="attributeToField">
       <field name="{@name}" xmlAttribute="true">
           
           <xsl:if test="@use = 'prohibited'">
               <xsl:attribute name="inapplicable">true</xsl:attribute>
           </xsl:if>
           <xsl:if test="@type != ''">
               <xsl:call-template name="getISCTypeAttribute">
                   <xsl:with-param name="type" select="@type"/>
               </xsl:call-template>
           </xsl:if>

           <xsl:if test="@use='required'">
               <xsl:attribute name="xmlRequired">true</xsl:attribute>
           </xsl:if>
       </field>
   </xsl:template>

   <xsl:template name="getISCTypeAttribute">
       <xsl:param name="type"/>
       <xsl:attribute name="type">
           <xsl:call-template name="getISCType">
               <xsl:with-param name="type" select="$type"/>
           </xsl:call-template>
       </xsl:attribute>
   </xsl:template>

   
   <xsl:template name="getISCType">
       <xsl:param name="type"/>
       <xsl:choose>
           
           <xsl:when test="$type='xs:NCName' or $type='xsd:NCName'">identifier</xsl:when>
           <xsl:when test="contains($type, ':')">
               <xsl:value-of select="substring-after($type,':')"/>
           </xsl:when>
           <xsl:otherwise><xsl:value-of select="$type"/></xsl:otherwise>
       </xsl:choose>
   </xsl:template>

   <xsl:template name="getLocalName">
       <xsl:param name="fullName"/>
       <xsl:choose>
           <xsl:when test="contains($fullName, ':')">
               <xsl:value-of select="substring-after($fullName,':')"/>
           </xsl:when>
           <xsl:otherwise><xsl:value-of select="$fullName"/></xsl:otherwise>
       </xsl:choose>
   </xsl:template>

   
   

   
   <xsl:template match="//xs:simpleType[@name]">
       <xsl:call-template name="makeSimpleType">
           <xsl:with-param name="name" select="@name"/>
       </xsl:call-template>
   </xsl:template>

   
   <xsl:template match="//xs:schema/xs:element[@name]/xs:simpleType">
       <xsl:call-template name="makeSimpleType">
           <xsl:with-param name="name" select="../@name"/>
       </xsl:call-template>
   </xsl:template>

   

   <xsl:template name="makeSimpleType">
      <xsl:param name="name"/>
      <SimpleType name="{$name}" xmlSource="SimpleType">
          <xsl:if test="xs:restriction/@base | xs:list/@itemType">
             <xsl:attribute name="inheritsFrom">
               <xsl:call-template name="getISCType">
                   <xsl:with-param name="type" select="xs:restriction/@base | xs:list/@itemType"/>
               </xsl:call-template>
             </xsl:attribute>
          </xsl:if>
          <xsl:call-template name="simpleTypeContents"/> 
      </SimpleType>
   </xsl:template>


   
   <xsl:template name="simpleTypeContents">
       
       <xsl:if test="xs:restriction/xs:minLength/@value &gt; 0 or                      xs:restriction/xs:length/@value &gt; 0">
          <xsl:attribute name="xmlNonEmpty">true</xsl:attribute>
       </xsl:if>

       
       <xsl:if test="./xs:restriction/xs:enumeration">
           <valueMap>
               <xsl:for-each select="./xs:restriction/xs:enumeration">
                   <value>
                       <xsl:if test="@ID">
                           <xsl:attribute name="ID"><xsl:value-of select="@ID"/></xsl:attribute>
                       </xsl:if>
                       <xsl:value-of select="@value"/>
                   </value>
               </xsl:for-each>
           </valueMap>
       </xsl:if>

       <xsl:if test="./xs:list">
          <xsl:attribute name="xmlListType">true</xsl:attribute>
       </xsl:if>

       <xsl:if test="./xs:union/@memberTypes">
          <xsl:attribute name="memberTypes">
             <xsl:value-of select="./xs:union/@memberTypes"/>
          </xsl:attribute>
       </xsl:if>

       
       <xsl:for-each select="./xs:list/xs:simpleType | ./xs:union/xs:simpleType">
           <SimpleType>
               <xsl:if test="xs:restriction/@base | xs:list/@itemType">
                   <xsl:attribute name="inheritsFrom">
                      <xsl:call-template name="getISCType">
                         <xsl:with-param name="type" select="xs:restriction/@base | xs:list/@itemType"/>
                      </xsl:call-template>
                   </xsl:attribute>
               </xsl:if>
               <xsl:call-template name="simpleTypeContents"/> 
           </SimpleType> 
       </xsl:for-each>

       
       <xsl:if test="count(xs:restriction/*) &gt; 0 and                       count(xs:restriction/*) !=                             count(xs:restriction/xs:attribute |                                   xs:restriction/xs:attributeGroup)"> 

       <validators>
            <xsl:if test="./xs:restriction/xs:enumeration">
               <validator type="isOneOf"/>
            </xsl:if>

            
            <xsl:variable name="localBase">
                <xsl:call-template name="getISCType">
                    <xsl:with-param name="type" select="xs:restriction/@base"/>
                </xsl:call-template>
            </xsl:variable>

            
            <xsl:variable name="rangeValidator">
               <xsl:choose>
                  <xsl:when test="$localBase = 'decimal' or                                    $localBase = 'float' or                                   $localBase = 'double'">floatRange</xsl:when>
                  <xsl:when test="$localBase = 'date' or                                    $localBase = 'datetime' or                                   $localBase = 'dateTime' or                                    $localBase = 'gYear' or                                    $localBase = 'gYearMonth'">dateRange</xsl:when>
                  <xsl:otherwise>integerRange</xsl:otherwise>
               </xsl:choose>
            </xsl:variable>

            
            <xsl:for-each select=".//*[not(local-name()=enumeration)]">
                <xsl:choose>
                    
                    <xsl:when test="local-name()='pattern'">
                        <validator type="regexp" expression="{@value}"/>
                    </xsl:when>

                    <xsl:when test="local-name()='minInclusive'">
                        <validator type="{$rangeValidator}" min="{@value}"/>
                    </xsl:when>
                    <xsl:when test="local-name()='minExclusive'">
                        <validator type="{$rangeValidator}" min="{@value}" exclusive="true"/>
                    </xsl:when>
                    <xsl:when test="local-name()='maxInclusive'">
                        <validator type="{$rangeValidator}" max="{@value}"/>
                    </xsl:when>
                    <xsl:when test="local-name()='maxExclusive'">
                        <validator type="{$rangeValidator}" max="{@value}" exclusive="true"/>
                    </xsl:when>

                    <xsl:when test="local-name()='minLength'">
                        <validator type="lengthRange" min="{@value}"/>
                    </xsl:when>
                    <xsl:when test="local-name()='maxLength'">
                        <validator type="lengthRange" max="{@value}"/>
                    </xsl:when>
                    <xsl:when test="local-name()='length'">
                        <validator type="lengthRange" min="{@value}" max="{@value}"/>
                    </xsl:when>
    
                    <xsl:when test="local-name()='fractionDigits'">
                        <validator type="floatPrecision" precision="{@value}" roundToPrecision="true"/>
                    </xsl:when>
                </xsl:choose>
            </xsl:for-each>
        </validators>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>
