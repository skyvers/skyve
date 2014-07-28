/**
 * $Id: NamingStrategy.java 97 2008-06-21 12:00:01Z maesenka $
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
 * A <code>NamingStrategy</code> determines how to derive suitable class and
 * member names corresponding to database tables and columns.
 * 
 * @author Karel Maesen, Geovise BVBA (http://www.geovise.com/)
 */
public interface NamingStrategy {

	/**
	 * Create a valid name for a member variable based on the specified input
	 * name.
	 * 
	 * @param base
	 *            the input name.
	 * @return a valid java identifier for a member variable.
	 */
	public String createPropertyName(String base);

	/**
	 * Create a valid name for a setter for the property
	 * 
	 * @param propertyName
	 * @return valid java identifier for a property setter
	 */
	public String createSetterName(String propertyName);

	/**
	 * Create a valid name for a getter for the property
	 * 
	 * @param propertyName
	 * @return valid java identifier for a property getter
	 */
	public String createGetterName(String propertyName);

	/**
	 * Create a valid name for a Java class based on the specified input name.
	 * 
	 * @param base
	 *            the input name.
	 * @return a valid java identifier for a class.
	 */
	public String createClassName(String base);

}
