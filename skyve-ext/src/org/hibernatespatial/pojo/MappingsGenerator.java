/**
 * $Id: MappingsGenerator.java 236 2010-07-29 21:49:00Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the 
 * hibernate ORM solution for geographic data. 
 *  
 * Copyright © 2008 Geovise BVBA
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * For more information, visit: http://www.hibernatespatial.org/
 */
package org.hibernatespatial.pojo;

import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.XMLWriter;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;

/**
 * This class creates a Hibernate mapping file for a list of tables.
 * 
 * @author Karel Maesen, Geovise BVBA (http://www.geovise.com/)
 */
public class MappingsGenerator {

	private Document mappingDoc;

	private String packageName;

	public MappingsGenerator(String packageName) {
		this.packageName = packageName;
	}

	public void write(Writer writer) throws IOException {
		OutputFormat format = OutputFormat.createPrettyPrint();
		XMLWriter xmlWriter = new XMLWriter(writer, format);
		xmlWriter.write(this.mappingDoc);
		xmlWriter.close();
	}

	public Document getMappingsDoc() {
		return this.mappingDoc;
	}

	public void load(Collection<ClassInfo> mappedClasses, String schema)
			throws MissingIdentifierException {

		this.mappingDoc = DocumentHelper.createDocument();
		this.mappingDoc.addDocType("hibernate-mapping",
				"-//Hibernate/Hibernate Mapping DTD 3.0//EN",
				"http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd");
		Element root = this.mappingDoc.addElement("hibernate-mapping");
    	root.addAttribute("package", this.packageName);
        if (schema != null){
            root.addAttribute("schema", schema);
        }
		for (ClassInfo classInfo: mappedClasses) {
			addTableElement(root, classInfo);
		}
	}

	private void addTableElement(Element root, ClassInfo classInfo)
			throws MissingIdentifierException {
		Element tableEl = root.addElement("class");
		tableEl.addAttribute("name", classInfo.getClassName());
		tableEl.addAttribute("table", classInfo.getTableName());
		AttributeInfo idAttr = classInfo.getIdAttribute();
		addColElement(tableEl, idAttr);
		for (AttributeInfo ai : classInfo.getAttributes()) {
			if (!ai.isIdentifier()) {
				addColElement(tableEl, ai);
			}
		}

	}

	private void addColElement(Element tableEl, AttributeInfo ai) {
		Element colEl = null;
		if (ai.isIdentifier()) {
			colEl = tableEl.addElement("id");
		} else {
			colEl = tableEl.addElement("property");
		}
		colEl.addAttribute("name", ai.getFieldName());
		colEl.addAttribute("column", ai.getColumnName());
		colEl.addAttribute("type", ai.getHibernateType());
		return;
	}
}
