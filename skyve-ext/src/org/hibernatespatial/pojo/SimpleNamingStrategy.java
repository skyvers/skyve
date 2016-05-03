/**
 * $Id: SimpleNamingStrategy.java 97 2008-06-21 12:00:01Z maesenka $
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

/**
 * This is the default implementation for a <code>NamingStrategy</code>.
 * 
 * @author Karel Maesen, Geovise BVBA (http://www.geovise.com/)
 */
public class SimpleNamingStrategy implements NamingStrategy {

	public String createClassName(String base) {
		String cleaned = toJavaName(base);
		cleaned = cleaned.toLowerCase();
		return capitalize(cleaned);
	}

	public String createGetterName(String fieldName) {
		return "get" + capitalize(fieldName);
	}

	public String createPropertyName(String base) {
		String cleaned = toJavaName(base);
		cleaned = cleaned.toLowerCase();
		return cleaned;
	}

	public String createSetterName(String fieldName) {
		return "set" + capitalize(fieldName);

	}

	/**
	 * 
	 * Turns the name into a valid, simplified Java Identifier.
	 * 
	 * @param name
	 * @return
	 */
	private String toJavaName(String name) {
		StringBuilder stb = new StringBuilder();
		char[] namechars = name.toCharArray();
		if (!Character.isJavaIdentifierStart(namechars[0])) {
			stb.append("__");
		} else {
			stb.append(namechars[0]);
		}
		for (int i = 1; i < namechars.length; i++) {
			if (!Character.isJavaIdentifierPart(namechars[i])) {
				stb.append("__");
			} else {
				stb.append(namechars[i]);
			}
		}

		return stb.toString();
	}

	private String capitalize(String s) {
		char[] ca = s.toCharArray();
		ca[0] = Character.toUpperCase(ca[0]);
		return new String(ca);
	}

	@SuppressWarnings("unused")
	private String uncapitalize(final String s) {
		final char[] ca = s.toCharArray();
		ca[0] = Character.toLowerCase(ca[0]);
		return new String(ca);
	}
}
