/**
 * $Id: AbstractDBGeometryType.java 138 2009-09-22 19:36:35Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the 
 * hibernate ORM solution for geographic data. 
 *  
 * Copyright © 2007 Geovise BVBA
 * Copyright © 2007 K.U. Leuven LRD, Spatial Applications Division, Belgium
 *
 * This work was partially supported by the European Commission, 
 * under the 6th Framework Programme, contract IST-2-004688-STP.
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
package org.hibernatespatial;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Properties;

import org.hibernate.HibernateException;
import org.hibernate.usertype.ParameterizedType;
import org.hibernate.usertype.UserType;
import org.hibernatespatial.cfg.GeometryFactoryHelper;
import org.hibernatespatial.mgeom.MGeometryFactory;

import com.vividsolutions.jts.geom.Geometry;

/**
 * This type is a abstract base type for implementing database specific user
 * types for geometry types.
 * 
 * @author Karel Maesen
 */
public abstract class AbstractDBGeometryType implements UserType,
		ParameterizedType, Serializable {
	
	private MGeometryFactory geomFactory = null;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#assemble(java.io.Serializable,
	 *      java.lang.Object)
	 */
	public Object assemble(Serializable cached, Object owner)
			throws HibernateException {
		return cached;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#deepCopy(java.lang.Object)
	 */
	public Object deepCopy(Object value) throws HibernateException {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#disassemble(java.lang.Object)
	 */
	public Serializable disassemble(Object value) throws HibernateException {
		return (Serializable) value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#equals(java.lang.Object,
	 *      java.lang.Object)
	 */
	public boolean equals(Object x, Object y) throws HibernateException {
		if (x == y)
			return true;
		if (x == null || y == null)
			return false;
		return ((Geometry) x).equalsExact((Geometry) y);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#hashCode(java.lang.Object)
	 */
	public int hashCode(Object x) throws HibernateException {
		return x.hashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#isMutable()
	 */
	public boolean isMutable() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#nullSafeGet(java.sql.ResultSet,
	 *      java.lang.String[], java.lang.Object)
	 */
	public Object nullSafeGet(ResultSet rs, String[] names, Object owner)
			throws HibernateException, SQLException {
		Object geomObj = rs.getObject(names[0]);
		return convert2JTS(geomObj);
	}

	/**
	 * Converts the native database geometry object to a JTS Geometry object.
	 * 
	 * Concrete subclasses should override this method.
	 * 
	 * @param geomObj
	 *            native database geometry object
	 * @return JTS Geometry
	 */
	public abstract Geometry convert2JTS(Object geomObj);

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#nullSafeSet(java.sql.PreparedStatement,
	 *      java.lang.Object, int)
	 */
	public void nullSafeSet(PreparedStatement st, Object value, int index)
			throws HibernateException, SQLException {
		if (value == null) {
			st.setNull(index, sqlTypes()[0]);
		} else {
			Geometry jtsGeom = (Geometry) value;
			Object dbGeom = conv2DBGeometry(jtsGeom, st.getConnection());
			st.setObject(index, dbGeom);
		}
	}

	/**
	 * Converts a JTS geometry object to a native database geometry object.
	 * 
	 * Concrete subclasses should override this method.
	 * 
	 * @param jtsGeom
	 *            JTS Geometry
	 * @return native database geometry object
	 */
	public abstract Object conv2DBGeometry(Geometry jtsGeom,
			Connection connection);

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#replace(java.lang.Object,
	 *      java.lang.Object, java.lang.Object)
	 */
	public Object replace(Object original, Object target, Object owner)
			throws HibernateException {
		return original;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#returnedClass()
	 */
	public Class returnedClass() {
		return Geometry.class;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.UserType#sqlTypes()
	 * 
	 * This should be overriden by concrete subclasses
	 */
	public abstract int[] sqlTypes();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.hibernate.usertype.ParameterizedType#setParameterValues(java.util.Properties)
	 */
	public void setParameterValues(Properties parameters) {
		if (parameters != null){
			this.geomFactory = GeometryFactoryHelper.createGeometryFactory(parameters);
		}
	}
	
	public MGeometryFactory getGeometryFactory(){
		return (this.geomFactory == null) ? HBSpatialExtension.getDefaultGeomFactory() : this.geomFactory;
	}

}
