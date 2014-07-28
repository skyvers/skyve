package org.skyve.wildcat.domain.types;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.dialect.Dialect;
import org.hibernate.type.LiteralType;
import org.hibernate.usertype.UserType;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal5;

public class Decimal5UserType implements UserType, Serializable, LiteralType {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4189877865378690596L;

	@Override
	public int[] sqlTypes() {
		return new int[] {Types.NUMERIC};
	}

	@Override
	public Class<?> returnedClass() {
		return Decimal5.class;
	}

	@Override
	public boolean equals(Object x, Object y) 
	throws HibernateException {
		return (x == y) || ((x != null) && (y != null) && ((Decimal5) x).equals(y));
	}

	@Override
	public int hashCode(Object o)
	throws HibernateException {
		return ((Decimal5) o).intValue();
	}

	@Override
	public Object nullSafeGet(ResultSet rs, String[] names, Object owner)
	throws HibernateException, SQLException {
		BigDecimal value = rs.getBigDecimal(names[0]);
		if (rs.wasNull()) {
			return null;
		}

		return new Decimal5(value);
	}

	@Override
	public void nullSafeSet(PreparedStatement ps, Object value, int index)
	throws HibernateException, SQLException {
		if (value == null) {
			ps.setNull(index, Types.NUMERIC);
		}
		else {
			ps.setBigDecimal(index, ((Decimal5) value).bigDecimalValue());
		}
	}

	@Override
	public Object deepCopy(Object value)
	throws HibernateException {
		return (value == null) ? null : new Decimal5((Decimal) value);
	}

	@Override
	public boolean isMutable() {
		return false;
	}

	@Override
	public Serializable disassemble(Object value)
	throws HibernateException {
		return (Serializable) value;
	}

	@Override
	public Object assemble(Serializable cached, Object owner)
	throws HibernateException {
		return cached;
	}

	@Override
	public Object replace(Object original, Object target, Object owner)
	throws HibernateException {
		return original;
	}

	@Override
	public String objectToSQLString(Object value, Dialect dialect)
	throws Exception {
		return (value == null) ? "NULL" : value.toString();
	}
}
