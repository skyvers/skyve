/**
 * $Id: TypeMapper.java 225 2010-06-25 21:59:05Z maesenka $
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

import javassist.ClassClassPath;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.NotFoundException;
import org.hibernatespatial.GeometryUserType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Types;
import java.util.ArrayList;
import java.util.List;

/**
 * The <code>TypeMapper</code> maps a pair consisting of java.sql.Type, and a
 * database type name to a CtClass (a representation of a java type used by the
 * javassist class building tools) and to a Hibernate type (used when creating a
 * mapping file).
 * 
 * @author Karel Maesen, Geovise BVBA (http://www.geovise.com/)
 */
public class TypeMapper {

    protected final static Logger logger = LoggerFactory.getLogger(TypeMapper.class);

	private final static String GEOMETRY_USER_TYPE = GeometryUserType.class
			.getCanonicalName();

	private List<TMEntry> entries = new ArrayList<TMEntry>();

	private String dbGeomType = "";

	private CtClass ctGeom;

	// TODO -- create entires for all constants defined in java.sql.Types
	public TypeMapper(String dbGeomType) {

		// first set the type to use for the geometry
		this.dbGeomType = dbGeomType;

		ClassPool pool = ClassPool.getDefault();
		// ensure that we can load the JTS classes.
		pool.insertClassPath(new ClassClassPath(this.getClass()));

		CtClass ctString;
		CtClass ctDate;
        CtClass ctInteger;
        CtClass ctBoolean;
        CtClass ctFloat;
        CtClass ctDouble;
        CtClass ctLong;        
        CtClass ctShort;
        CtClass ctBigDecimal;
        CtClass ctByte;
        CtClass ctBinary;
		try {
			ctString = pool.get("java.lang.String");
			ctDate = pool.get("java.util.Date");
			ctGeom = pool.get("com.vividsolutions.jts.geom.Geometry");
            ctInteger = pool.get("java.lang.Integer");
            ctBoolean = pool.get("java.lang.Boolean");
            ctDouble = pool.get("java.lang.Double");
            ctLong = pool.get("java.lang.Long");
            ctShort = pool.get("java.lang.Short");
            ctFloat = pool.get("java.lang.Float");
            ctBigDecimal = pool.get("java.math.BigDecimal");
            ctByte = pool.get("java.lang.Byte");
            ctBinary = pool.get("byte[]");
		} catch (NotFoundException e) {
			throw new RuntimeException(e);
		}


		entries.add(new TMEntry(Types.BIGINT, "integer", ctInteger));
		entries.add(new TMEntry(Types.SMALLINT, "short", ctShort));
        entries.add(new TMEntry(Types.TINYINT, "byte", ctByte));
		entries.add(new TMEntry(Types.BOOLEAN, "boolean", ctBoolean));
		entries.add(new TMEntry(Types.BIT, "boolean", ctBoolean));
		entries.add(new TMEntry(Types.CHAR, "string", ctString));
		entries.add(new TMEntry(Types.DATE, "date", ctDate));
		entries.add(new TMEntry(Types.TIMESTAMP, "timestamp", ctDate));
		entries.add(new TMEntry(Types.TIME, "time", ctDate));
		entries.add(new TMEntry(Types.DECIMAL, "big_decimal", ctBigDecimal));
		entries.add(new TMEntry(Types.DOUBLE, "double", ctDouble));
		entries.add(new TMEntry(Types.NUMERIC, "big_decimal", ctBigDecimal));
		entries.add(new TMEntry(Types.FLOAT, "float", ctFloat));
		entries.add(new TMEntry(Types.INTEGER, "integer", ctInteger));
        entries.add(new TMEntry(Types.BIGINT, "long", ctLong));
		entries.add(new TMEntry(Types.VARCHAR, "string", ctString));
        entries.add(new TMEntry(Types.BINARY, "binary", ctBinary));
        entries.add(new TMEntry(Types.CLOB, "text", ctString));
	}

	public CtClass getCtClass(String dbType, int sqlType) {
		if (dbType.equalsIgnoreCase(this.dbGeomType)) {
			return this.ctGeom;
		}
		for (TMEntry entry : entries) {
			if (entry.javaType == sqlType) {
				return entry.ctClass;
			}
		}
		return null;
	}

	public String getHibernateType(String dbType, int sqlType) throws TypeNotFoundException {
		if (dbType.equalsIgnoreCase(this.dbGeomType)) {
			return GEOMETRY_USER_TYPE;
		}
		for (TMEntry entry : entries) {
			if (entry.javaType == sqlType) {
				return entry.hibernateTypeName;
			}
		}
		throw new TypeNotFoundException(dbType);

	}

	public int[] getMappedSQLTypes() {
		int l = this.entries.size();
		int[] types = new int[l];
		for (int i = 0; i < this.entries.size(); i++) {
			types[i] = this.entries.get(i).javaType;
		}
		return types;
	}

	public void addTypeMapping(int sqlType, String hibernateType,
			CtClass ctClass) {
		this.entries.add(new TMEntry(sqlType, hibernateType, ctClass));
	}

	public void removeTypeMapping(int sqlType) {
		TMEntry tm = null;
		for (TMEntry t : this.entries) {
			if (t.javaType == sqlType) {
				tm = t;
				break;
			}
		}
		if (tm != null) {
			this.entries.remove(tm);
		}
	}

	private static class TMEntry {
		protected int javaType = 0;

		protected String hibernateTypeName = "";

		protected CtClass ctClass;

		protected TMEntry(int jt, String ht, CtClass jc) {
			this.javaType = jt;
			this.hibernateTypeName = ht;
			this.ctClass = jc;
		}
	}

}
