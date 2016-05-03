/**
 * $Id: ClassInfo.java 235 2010-07-29 19:36:00Z maesenka $
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

import java.util.ArrayList;
import java.util.List;

public class ClassInfo {

	private final String className;

	private final String tableName;

	private final List<AttributeInfo> attributes = new ArrayList<AttributeInfo>();

	public ClassInfo(String tableName, String className) {
		this.className = className;
		this.tableName = tableName;
	}

	public AttributeInfo getIdAttribute() throws MissingIdentifierException {
		for (AttributeInfo ai : getAttributes()) {
			if (ai.isIdentifier()) {
				return ai;
			}
		}
		throw new MissingIdentifierException();
	}

	public AttributeInfo getGeomAttribute() throws GeometryNotFoundException {
		for (AttributeInfo ai : getAttributes()) {
			if (ai.isGeometry()) {
				return ai;
			}
		}
		throw new GeometryNotFoundException();
	}

	public List<AttributeInfo> getAttributes() {
		return attributes;
	}

	public String getClassName() {
		return className;
	}

	public String getTableName() {
		return tableName;
	}

	public void addAttribute(AttributeInfo ai) {
		this.attributes.add(ai);
	}

	public void removeAttribute(AttributeInfo ai) {
		this.attributes.remove(ai);
	}

	public void clearAttributes() {
		this.attributes.clear();
	}

	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result
				+ ((attributes == null) ? 0 : attributes.hashCode());
		result = PRIME * result
				+ ((className == null) ? 0 : className.hashCode());
		result = PRIME * result
				+ ((tableName == null) ? 0 : tableName.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final ClassInfo other = (ClassInfo) obj;
		if (attributes == null) {
			if (other.attributes != null)
				return false;
		} else if (!attributes.equals(other.attributes))
			return false;
		if (className == null) {
			if (other.className != null)
				return false;
		} else if (!className.equals(other.className))
			return false;
		if (tableName == null) {
			if (other.tableName != null)
				return false;
		} else if (!tableName.equals(other.tableName))
			return false;
		return true;
	}

}
