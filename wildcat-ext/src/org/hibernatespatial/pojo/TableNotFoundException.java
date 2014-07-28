/**
 * $Id: TableNotFoundException.java 97 2008-06-21 12:00:01Z maesenka $
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
 * This Exception is thrown when the POJO Utility cannot locate a primary key.
 * 
 * @author Karel Maesen, Geovise BVBA
 * 
 */
public class TableNotFoundException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static final String basemsg = "Table not found";

	public TableNotFoundException() {
		super(basemsg);
	}

	public TableNotFoundException(String msg) {
		super(basemsg + ": " + msg);
	}

	public TableNotFoundException(Throwable cause) {
		super(cause);
	}

	public TableNotFoundException(String msg, Throwable cause) {
		super(basemsg + ": " + msg, cause);
	}

}
